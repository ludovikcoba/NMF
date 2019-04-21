getExplainability <- function(train, dataset, knn){
  
  explComp <- function(ratings, similar){
    
    t <- semi_join(train, similar, by = c("user" = "neighbour"))
    t <- semi_join(t, ratings, by = c("item"))
    t <- t %>% filter(score >= explThreshold) # clean items under the threshold
    t %>% group_by(item) %>% summarise(Explainability = sum(score))
    
  }
  
  temp <- dataset %>% 
    group_by(user) %>% 
    nest()
  
  Expl <- knn %>% 
    group_by(observation) %>% 
    nest(.key = "similar")
  
  Expl <- inner_join(temp, Expl, by = c("user"="observation")) 
  
  Expl <- Expl %>% 
    mutate(expl = map2(data, similar, explComp))
  
  Expl <- Expl %>% 
    select(-data,-similar) %>% unnest()
  
  Expl
  
}
