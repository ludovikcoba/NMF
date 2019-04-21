similarity <- function(x, damp = 0, shrinkage = 0, by){


  x <- x %>% 
    arrange_at(vars(by)) %>%
    select_at(vars(by))
  x <- as.data.frame(x)

  sim <- compute_similarity(x[,1], x[,2], x[,3], length(unique(x[,1])), damp, as.integer(shrinkage))

}


getKNN <- function(sim, k){

  #repeat twice to save memory trading off efficiency
  left <- sim %>% group_by(A) %>%
    arrange(desc(sim)) %>%
    slice(1:k) %>%
    dplyr::select(observation = A, neighbour = B, sim)

  right <- sim %>% group_by(B) %>%
    arrange(desc(sim)) %>%
    slice(1:k)%>%
    dplyr::select(observation = B, neighbour = A, sim)

  rbind(left,right) %>%
    group_by(observation) %>%
    slice(1:k)


}

