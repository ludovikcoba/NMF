similarity <- function(x, damp = 0, shrinkage = 0, by){


  x <- x %>% 
    dplyr::arrange_at(dplyr::vars(by)) %>%
    dplyr::select_at(dplyr::vars(by))
  x <- as.data.frame(x)

  sim <- compute_similarity(x[,1], x[,2], x[,3], length(unique(x[,1])), damp, as.integer(shrinkage))

}


getKNN <- function(sim, k){

  #repeat twice to save memory trading off efficiency
  left <- sim %>% dplyr::group_by(A) %>%
    dplyr::arrange(desc(sim)) %>%
    dplyr::slice(1:k) %>%
    dplyr::select(observation = A, neighbour = B, sim)

  right <- sim %>% dplyr::group_by(B) %>%
    dplyr:: arrange(desc(sim)) %>%
    dplyr::slice(1:k)%>%
    dplyr::select(observation = B, neighbour = A, sim)

  rbind(left,right) %>%
    dplyr::group_by(observation) %>%
    dplyr::slice(1:k)


}

