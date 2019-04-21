Novelty <- function(train, dataset, categories){

  users<- dplyr::inner_join(train, categories, by = c("item") ) # find to which category training items belong
  
  # count how many times a user has seen a category only on the trainset
  nvl <- users %>%
    dplyr::select(-item, -score) %>%
    dplyr::group_by(user) %>%
    dplyr::summarise_all(dplyr::funs(sum))
  

  # group user items and categories
  nvl <- nvl %>% 
    dplyr::group_by(user) %>% 
    tidyr::nest(.key = "usrNov") 
  
  users<- dplyr::inner_join(dataset, categories, by = c("item") ) # find to which category all items belong
  users <- users %>% 
    dplyr::group_by(user) %>% 
    dplyr::select(-score) %>% 
    tidyr::nest(.key = "items")
  
  users <- dplyr::inner_join(users, nvl)
  
  nvlCmp <- function(items, usrNov){

    r <- as.matrix(items[,-1])
    usrNov <- 1/((r %*% t(usrNov)) + 1) # asuming that each item belong to a category we and based on how many times a user has seen a cetegory, we consider the usrNov to be the inverse of the weight of the category to the user.
    
    data.frame(item = items$item, Novelty = usrNov)
    
  }
  
  # compute and return novelty 
  users %>% 
    dplyr::mutate(Novelty = purrr::map2(items, usrNov, nvlCmp)) %>% 
    dplyr::select(user, Novelty) %>% 
    tidyr::unnest()
  
  
}