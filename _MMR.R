#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be given (Dataset, output, novelty alg).", call.=FALSE)
} else {
  ds = args[1] # values: ml100k, ml1m, ml10m, ml20m, LA, To
  if(!ds %in% c("ml100k", "ml1m", "ml10m", "ml20m", "LV", "To")){
    error_msg <- "Argument 1 can be one of: ml100k, ml1m, ml10m, ml20m, LV, To" 
    stop(error_msg)
  }
  oFile = args[2] # any output file
  nov = args[3] # values: cat or pop
  if(! nov %in% c("cat", "pop")){
    error_msg <- "Argument 3 can be one of: cat, pop" 
    stop(error_msg)
  }

}

## Note: many method are imported from rrecsys
#install.packages("rrecsys", repos='http://cran.us.r-project.org') # please install the library to acknowledge the authors!
#### Requirements
if (!require(Rcpp)) install.packages("Rcpp", repos='http://cran.us.r-project.org')
if (!require(tidyverse)) install.packages("tidyverse", repos='http://cran.us.r-project.org')
if (!require(dplyr)) install.packages("dplyr", repos='http://cran.us.r-project.org')
if (!require(readr)) install.packages("readr", repos='http://cran.us.r-project.org')
if (!require(proxy)) install.packages("proxy", repos='http://cran.us.r-project.org')

#######################
#######################
#######################
###### Edit Here ######
#######################
#######################
#######################

#### Parameters - Config
outputFile <- oFile

Neigh <- 10
Shrinkage <- 10 # damping on similarity computation.
learningRate <- 0.0001
regCoef <- 0.001
nrfeat <- 80 #nr latent features
steps <- 50# number of iterations
reg <- 3 # 1 MF, 2 L2 regulariztion, 3 L1 regularization
adjCos <- FALSE
topN <- 10

trade_off <- 0.5
x <- 80 # p-core


learningRate <- 0.0001
regCoef <- 0.001
regCoefNovelty <- c(0:10)/10
nrfeat <- 80 #nr latent features
steps <- 50# number of iterations
reg <- 3 # 1 MF, 2 L2 regulariztion, 3 L1 regularization
adjCos <- FALSE
topN <- 10

positiveThreshold <- 3 # when a ratign is considered a negative feedback

# Read Data
if(str_detect(ds, "ml")){
  source("src/readML_big.R")
  dataset <- getML(ds)
  categories <- dataset[[2]]
  dataset <- dataset[[1]]
}else{
  if(ds == "LV"){
    dataset <- read.csv("datasets/Yelp/LV/Las_Vegas.csv")
    categories <- read.csv("datasets/Yelp/LV/las_vegas_categories.csv")
  }else if (ds == "To"){
    dataset <- read.csv("datasets/Yelp/Toronto/Toronto.csv")
    categories <- read.csv("datasets/Yelp/Toronto/toronto_categories.csv")
  }
}


at_least_x_users <- dataset %>% 
  dplyr::group_by(item) %>%
  dplyr::summarise(nr_ratings = n()) %>%
  dplyr::filter(nr_ratings >= x)


dataset <- semi_join(dataset, at_least_x_users)

at_least_xr_ratings <- dataset %>% 
  dplyr::group_by(user) %>%
  dplyr::summarise(nr_ratings = n()) %>%
  dplyr::filter(nr_ratings >= x)

dataset <- semi_join(dataset, at_least_xr_ratings)

categories <- semi_join(categories, dataset)

source("src/evalSplit.R") # load the splitting function. Stratified splitting of the dataset in tran/test, given a splitting ratio.
d <- evalSplit(dataset, 0.25) # split train/test

#######################
#######################
#######################
##### DO NOT EDIT #####
#######################
#######################
#######################

#### Computing item's similarity
#normalization

#temp <- d$train
#if(adjCos){
#  temp <- temp %>% 
#    dplyr::group_by(user) %>% 
#    dplyr::summarise(offset = mean(score)) 
#  
#  temp <- dplyr::inner_join(temp, d$train) %>% 
#    dplyr::mutate(score = score - offset) %>% 
#    dplyr::select(-offset)
#}



# similarity compute
#sourceCpp("src/compute_similarity.cpp")
#source("src/ALG_similarity.R")

#knnUsr <- similarity(temp, shrinkage = Shrinkage, by = c("user", "item", "score"))
#knnUsr <- getKNN(knnUsr, Neigh)

#### Novelty
source("src/ALG_Novelty.R")
if(nov == "cat"){
  Nvl <- NoveltyCat(d$train, dataset, categories) # not so efficient.
}else{
  Nvl <- NoveltyPop(d$train, dataset)
}




#### Train
sourceCpp("src/NMFupdater.cpp")

train <- left_join(d$train, Nvl, by = c("user", "item"))

train$Novelty[is.na(train$Novelty)] <- 0;

source("src/ALG_MMR.R")

source("src/evalRec.R")

features <- NSVDupdater(
  as.matrix(train),
  learningRate, 
  regCoef,
  0, # because we are using the re-ranker
  nrfeat, # the total number of features.
  steps,
  reg # 1 MF, 2 L2 regulariztion, 3 L1 regularization
)

usrFeatures <- cbind(features$uID,features$U)
colnames(usrFeatures) <- c("user", paste0("f",1:nrfeat))
usrFeatures <- as.data.frame(usrFeatures)

itmFeatures <- cbind(features$iID, features$V)
colnames(itmFeatures) <- c("item", paste0("f",1:nrfeat))
itmFeatures <-as.data.frame(itmFeatures)

#### Recommend
source("src/ALG_similarity.R")

for (trd in trade_off) {
  
  #### Evaluate 
  
  if(nov == "cat"){
    distance <- dist(categories, method = "jaccard")
    distance <- apply(distance, 1, function(x) data.frame(item2 = categories[,1], distance = x))
    distance <- lapply(1:length(distance), function(x) cbind(rep(x, length(distance)), distance[[x]]  ))
    distance <- do.call(rbind, distance)
    colnames(distance) = c("item1", "item2", "dist")
    
    
  }else{
    distance <- similarity(dataset, shrinkage = Shrinkage, by = c("item", "user", "score"))
    distance <- getKNN(distance, length(unique(dataset$item)) - 1)
    distance$sim <- 1 - distance$sim
    colnames(distance) = c("item1", "item2", "dist")
  }
  
  
  rec <- RecommendMMR(train, usrFeatures, itmFeatures, topN, trd, distance)

  
  #### Evaluate 
  
  if(nov == "cat"){
    rec_nvl <- NoveltyCat(d$train, rec %>% 
                            rename(score = rank), categories)
  }else{
    rec_nvl <- NoveltyPop(d$train, rec %>% 
                            rename(score = rank))
  }
  
  
  rec <- left_join(rec, rec_nvl, by = c("user", "item"))
  
  rec$Novelty[is.na(rec$Novelty)] <- 0;
  
  
  if(exists('res')){
    r <- evalRec(rec, d$test, topN, positiveThreshold, max(dataset$score))
    res <- rbind(res,r)
  }else{
    res <- evalRec(rec, d$test, topN, positiveThreshold, max(dataset$score))
  }
  write.csv(res, outputFile)
  
  
}
res <- cbind(trade_off,res)

write.csv(res, outputFile)

