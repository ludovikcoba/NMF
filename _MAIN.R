#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("At least one argument must be given", call.=FALSE)
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
if (!require(stringr)) install.packages("stringr", repos='http://cran.us.r-project.org')

#######################
#######################
#######################
###### Edit Here ######
#######################
#######################
#######################

#### Parameters - Config
outputFile <- paste0("results/", oFile)

Neigh <- 10
Shrinkage <- 10 # damping on similarity computation.
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


at_least_40_users <- dataset %>% 
  dplyr::group_by(item) %>%
  dplyr::summarise(nr_ratings = n()) %>%
  dplyr::filter(nr_ratings >= 40)


dataset <- semi_join(dataset, at_least_40_users)

at_least_40_ratings <- dataset %>% 
  dplyr::group_by(user) %>%
  dplyr::summarise(nr_ratings = n()) %>%
  dplyr::filter(nr_ratings >= 40)

dataset <- semi_join(dataset, at_least_40_ratings)



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

source("src/ALG_Recommend.R")
source("src/evalRec.R")

for (rNVL in regCoefNovelty){
  
  
  features <- NSVDupdater(
    as.matrix(train),
    learningRate, 
    regCoef,
    rNVL,
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

  rec <- Recommend(train, usrFeatures, itmFeatures, topN)
  
  #### Evaluate 
  if(nov == "cat"){
    rec_nvl <- NoveltyCat(d$train, rec %>% 
                         dplyr::select(-rank) %>% 
                         rename(score = predScore), categories)
  }else{
    rec_nvl <- NoveltyPop(d$train, rec %>% 
                            dplyr::select(-rank) %>% 
                            rename(score = predScore))
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
res <- cbind(regCoefNovelty,res)

write.csv(res, outputFile)


