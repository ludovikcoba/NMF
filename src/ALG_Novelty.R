Novelty <- function(train, dataset, categories){

  users<- inner_join(train, categories, by = c("item") ) # find to which category training items belong
  
  # count how many times a user has seen a category only on the trainset
  nvl <- users %>%
    select(-item, -score) %>%
    group_by(user) %>%
    summarise_all(funs(sum))
  

  # group user items and categories
  nvl <- nvl %>% group_by(user) %>% 
    nest(.key = "usrNov") 
  
  users<- inner_join(dataset, categories, by = c("item") ) # find to which category all items belong
  users <- users %>% 
    group_by(user) %>% 
    select(-score) %>% 
    nest(.key = "items")
  
  users <- inner_join(users, nvl)
  
  nvlCmp <- function(items, usrNov){

    r <- as.matrix(items[,-1])
    usrNov <- 1/((r %*% t(usrNov)) + 1) # asuming that each item belong to a category we and based on how many times a user has seen a cetegory, we consider the usrNov to be the inverse of the weight of the category to the user.
    
    data.frame(item = items$item, Novelty = usrNov)
    
  }
  
  # compute and return novelty 
  users %>% 
    mutate(Novelty = map2(items, usrNov, nvlCmp)) %>% 
    select(user, Novelty) %>% 
    unnest()
  
  
}