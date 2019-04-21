evalSplit <- function(data, test_ratio = .25){
  
  data <- as.data.frame(data)
  
  test_set <- data %>% sample_frac(test_ratio)
  data <- anti_join(data, test_set,  by = c("user", "item", "score"))
  
  list(train = data, test = test_set)
  
}