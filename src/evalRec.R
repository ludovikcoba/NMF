evalRec <- function(rec, test, topN = 3, positiveThreshold = 3, maximum){

  test <- test %>% filter(score >= positiveThreshold)
  
  nrUsr <- length(unique(test$user))
  
  Hits <- semi_join(rec, test, by = c("user", "item")) 
  
  #### Precision
  
  
  TPcount <- Hits %>% 
    group_by(user) %>%
    summarise(TP = n())
  FPcount <- anti_join(rec, test, by = c("user", "item")) %>% 
    group_by(user) %>%
    summarise(FP = n())
  
  precUsr <- full_join(TPcount, FPcount)
  precUsr[is.na(precUsr)] <- 0
  
  precUsr <- precUsr %>% mutate(precision = TP/(TP + FP))
  
  # mean precision
  precision <- sum(precUsr$precision)/nrUsr

  #### nDCG

  nDCGusr <- Hits %>% 
    select(user,rank) %>%
    group_by(user) %>%
    nest() %>%
    mutate(val = map2(data, topN, eval_nDCG)) %>% 
    select(user, val) %>% unnest()
  
  # mean nDCG
  nDCG <- sum(nDCGusr$val) / nrUsr
  
  #### N_nDCG
  
  nUsr <- length(unique(rec$user))

  N_nDGCusr <- rec %>% 
    select(user, Novelty) %>%
    group_by(user) %>% 
    nest() %>%
    mutate(N_nDCG = map2(data, topN, eval_Novel_nDCG)) %>%
    select(user, N_nDCG) %>%
    unnest()
  
  # mean e_ndcg 
  N_nDCG <- mean(N_nDGCusr$N_nDCG)
  
  
  return(data.frame(precision, nDCG, N_nDCG))
  
}











eval_nDCG <- function(rank, topN){
  
  idcg <- getiDCG(topN)
  rank <- rank$rank
  if(1 %in% as.vector(rank)){
    dcg <- 1/log2(rank[-1])
    dcg <- 1 + sum(dcg)
  }else{
    dcg <- sum(1/log2(rank))
  }
  
  dcg/idcg
}

getiDCG <- function(n){
  
  idcg <- 1
  
  if(n > 1){
    idcg <- idcg + sum(1/log2(2:n))
  }
  
  idcg
}


eval_Novel_nDCG <- function(novelty, topN){

  novelty <- novelty$Novelty
  #generate ideal discounted comulative gain
  
  nidcg <- 1 + sum(1/log2(2:topN))

  novel_dcg <- 0
  
  if(length(novelty) > 1){
    novel_dcg <- novelty[-1]/log2(2:length(novelty))
  }
  
  novel_dcg <-  novelty[1] + sum(novel_dcg)
  
  novel_dcg/nidcg
}

