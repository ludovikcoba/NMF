RecommendMMR <- function(dataset, usrFeatures, itmFeatures, topN, trade, dist){
  
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
    
    rec <- c(top_n(pred, 1, predScore)$item[1])
    nrm_val <- max(pred$predScore)
    
    pred <- pred %>% filter(item != rec[1])
    
    for (i in 2:topN) {
      if (nrow(pred) < 1) return(rec)
      sim2 <- distance %>% 
        dplyr::left_join(data.frame(rec), by =  c("item2" = "rec"))
      
      if(i>2){
        sim2 <- sim2 %>% 
          dplyr::group_by(item1) %>% 
          dplyr::summarise(dist = mean(dist))
      }
       
      mr <- data.frame(item = pred$item, norm_predScore = pred$predScore/nrm_val)
      
      mr <- dplyr::left_join(mr, sim2, by = c("item" = "item1"), n) 
      
      mr <- mr %>% dplyr::mutate(margrank = trade*norm_predScore + (1-trade)*dist) %>%
        dplyr::arrange(desc(margrank))
      
      rec <- c(rec, mr$item[1])
      
      pred <- pred %>% 
        dplyr::filter(item != rec[i])
      
    }
    data.frame(item = rec, rank = 1:length(rec))
    
  }
  
  
  dataset %>% 
    dplyr::mutate(Rec = purrr::map2(Ratings, Features, rec2usr)) %>%
    dplyr::select(user, Rec) %>%
    tidyr::unnest()
  
}
