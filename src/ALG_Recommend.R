Recommend <- function(dataset, usrFeatures, itmFeatures, topN){

  dataset <-  dataset %>% 
    dplyr::group_by(user) %>%
    tidyr::nest(.key = "Ratings")
  usrFeatures <- usrFeatures %>% 
    dplyr::group_by(user)%>%
    tidyr::nest(.key = "Features")
  
  dataset <- dplyr::inner_join(dataset, usrFeatures)
  
  rec2usr <- function(Rated, Features){
    temp <- dplyr::anti_join(itmFeatures, Rated, by = "item") #remove rated
    
    pred <- as.matrix(temp[,-1]) %*% t(Features)
    
    pred <- data.frame(item = temp[,1], predScore = pred)
    
    pred %>% dplyr::arrange(desc(predScore)) %>%
      dplyr::slice(1:topN) %>%
      dplyr::mutate(rank = 1:topN)
    
  }
  
  dataset %>% 
    dplyr::mutate(Rec = purrr::map2(Ratings, Features, rec2usr)) %>%
    dplyr::select(user, Rec) %>%
    tidyr::unnest()
  
}