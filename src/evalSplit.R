evalSplit <- function(data, test_ratio = .25){
  
  data <- as.data.frame(data)
  
  test_set <- data %>% dplyr::sample_frac(test_ratio)
  data <- dplyr::anti_join(data, test_set,  by = c("user", "item", "score"))
  
  list(train = data, test = test_set)
  
}