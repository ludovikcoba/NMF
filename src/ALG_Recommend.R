Recommend <- function(dataset, usrFeatures, itmFeatures, topN){

  dataset <-  dataset %>% 
    group_by(user) %>%
    nest(.key = "Ratings")
  usrFeatures <- usrFeatures %>% 
    group_by(user)%>%
    nest(.key = "Features")
  
  dataset <- inner_join(dataset, usrFeatures)
  
  rec2usr <- function(Rated, Features){
    temp <- anti_join(itmFeatures, Rated, by = "item") #remove rated
    
    pred <- as.matrix(temp[,-1]) %*% t(Features)
    
    pred <- data.frame(item = temp[,1], predScore = pred)
    
    pred %>% arrange(desc(predScore)) %>%
      slice(1:topN) %>%
      mutate(rank = 1:topN)
    
  }
  
  dataset %>% 
    mutate(Rec = map2(Ratings, Features, rec2usr)) %>%
    select(user, Rec) %>%
    unnest()
  
}