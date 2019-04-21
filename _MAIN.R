if (length(args)==0) {
  stop("At least one argument must be supplied.", call.=FALSE)
} else if (length(args)==1) {
  ds = args[1]
  oFile = args[2]
}


## Note: many method are imported from rrecsys
install.packages("rrecsys", repos='http://cran.us.r-project.org') # please install the library to acknowledge the authors!
#### Requirements
if (!require(Rcpp)) install.packages("Rcpp", repos='http://cran.us.r-project.org')
if (!require(tidyverse)) install.packages("tidyverse", repos='http://cran.us.r-project.org')
if (!require(readr)) install.packages("readr", repos='http://cran.us.r-project.org')

#######################
#######################
#######################
###### Edit Here ######
#######################
#######################
#######################

#### Parameters - Config
outputFile <- "results.txt"

Neigh <- 10
Shrinkage <- 10 # damping on similarity computation.
learningRate <- 0.001
regCoef <- 0.001
regCoefNovelty <- c(0:10)/10
nrfeat <- 80 #nr latent features
steps <- 100 # number of iterations
reg <- 3 # 1 MF, 2 L2 regulariztion, 3 L1 regularization
adjCos <- FALSE
topN <- 10

positiveThreshold <- 3 # when a ratign is considered a negative feedback

# Read Data
if(ds == "ml100k") source("src/readML100K.R") # will load ml100k and movie_categories in the environment.
if(ds == "ml1m") {
  source("src/readML1M.R")
  dataset <- getml1m()
  categories <- dataset[[2]]
  dataset <- dataset[[1]]
}


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

temp <- d$train
if(adjCos){
  temp <- temp %>% 
    group_by(user) %>% 
    summarise(offset = mean(score)) 
  
  temp <- inner_join(temp, d$train) %>% 
    mutate(score = score - offset) %>% select(-offset)
}



# similarity compute
sourceCpp("src/compute_similarity.cpp")
source("src/ALG_similarity.R")

knnUsr <- similarity(temp, shrinkage = Shrinkage, by = c("user", "item", "score"))
knnUsr <- getKNN(knnUsr, Neigh)

#### Novelty
source("src/ALG_Novelty.R")
Nvl <- Novelty(d$train, dataset, categories) # not so efficient.

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
  
  rec_nvl <- Novelty(d$train, rec %>% select(-rank) %>% rename(score = predScore), categories)
  
  rec <- left_join(rec, rec_nvl, by = c("user", "item"))
  
  rec$Novelty[is.na(rec$Novelty)] <- 0;
  

  if(exists('res')){
    r <- evalRec(rec, d$test, topN, positiveThreshold, max(dataset$score))
    res <- rbind(res,r)
  }else{
    res <- evalRec(rec, d$test, topN, positiveThreshold, max(dataset$score))
  }
  

  
}
res <- cbind(regCoefNovelty,res)

write.csv(res, paste0("results/",outputFile))


