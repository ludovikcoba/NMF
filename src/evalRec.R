evalRec <- function(rec, test, topN = 3, positiveThreshold = 3, maximum){

  test <- test %>% 
    dplyr::filter(score >= positiveThreshold)
  
  nrUsr <- length(unique(test$user))
  
  Hits <- dplyr::semi_join(rec, test, by = c("user", "item")) 
  
  #### Precision
  
  
  TPcount <- Hits %>% 
    dplyr::group_by(user) %>%
    dplyr::summarise(TP = n())
  FPcount <- dplyr::anti_join(rec, test, by = c("user", "item")) %>% 
    dplyr::group_by(user) %>%
    dplyr::summarise(FP = n())
  
  precUsr <- dplyr::full_join(TPcount, FPcount)
  precUsr[is.na(precUsr)] <- 0
  
  precUsr <- precUsr %>% dplyr::mutate(precision = TP/(TP + FP))
  
  # mean precision
  precision <- sum(precUsr$precision)/nrUsr

  #### nDCG

  nDCGusr <- Hits %>% 
    dplyr::select(user,rank) %>%
    dplyr::group_by(user) %>%
    tidyr::nest() %>%
    dplyr::mutate(val = purrr::map2(data, topN, eval_nDCG)) %>% 
    dplyr::select(user, val) %>% 
    tidyr::unnest()
  
  # mean nDCG
  nDCG <- sum(nDCGusr$val) / nrUsr
  
  #### N_nDCG
  
  nUsr <- length(unique(rec$user))

  N_nDGCusr <- rec %>% 
    dplyr::select(user, Novelty) %>%
    dplyr::group_by(user) %>% 
    tidyr::nest() %>%
    dplyr::mutate(N_nDCG = purrr::map2(data, topN, eval_Novel_nDCG)) %>%
    dplyr::select(user, N_nDCG) %>%
    tidyr::unnest()
  
  # mean e_ndcg 
  N_nDCG <- mean(N_nDGCusr$N_nDCG)
  
  EPC <- Hits %>% 
    dplyr::mutate(disc = 1/log2(rank + 2), nov = Novelty * disc) %>%
    dplyr::group_by(user) %>%
    dplyr::summarise(nov = sum(nov), norm = sum(disc)) %>%
    dplyr::mutate(epc = nov/norm) %>%
    dplyr::mutate(epc = tidyr::replace_na(epc, 0))
  
  EPC <- mean(EPC$epc)
  
  return(data.frame(precision, nDCG, N_nDCG, EPC))
  
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

