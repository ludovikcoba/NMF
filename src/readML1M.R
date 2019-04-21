if (!require(stringr)) install.packages("stringr", repos='http://cran.us.r-project.org')


getml1m <- function(){
  download.file("http://files.grouplens.org/datasets/movielens/ml-1m.zip", destfile = "datasets/ml1m.zip")
  unzip("datasets/ml1m.zip", exdir = "datasets")
  
  
  dataset <- read_table2("datasets/ml-1m/ratings.dat", #<---dataset
                         col_names = FALSE)
  
  dataset <- dataset %>% separate(X1, into = c("user","item" ,"score"), sep = "::")
  
  dataset <- dataset %>% mutate(user = as.numeric(user), item = as.numeric(item), score = as.numeric(score))
  
  categories <- read_csv("datasets/ml-1m/movies.dat", 
                         col_names = FALSE, trim_ws = FALSE)
  
  
  
  categories <-   categories %>% 
    separate(X1, into = c("item","movie_title" ,"categories"), sep = "::")
  
  
  m_cat_list <- str_split(categories$categories, pattern = "\\|")
  
  unique_categories <- unique(unlist(m_cat_list))
  
  m_cat <- matrix(0, nrow = nrow(categories), ncol = length(unique_categories))
  
  for(i in 1:length(m_cat_list)){
    m_cat[i,which(unique_categories %in% m_cat_list[[i]])] <- 1
  }
  
  unique_categories[which(is.na(unique_categories))] <- "unknown"
  
  colnames(m_cat) = unique_categories
  
  categories <- categories %>%
    select(item) %>%
    mutate(item = as.numeric(item))
  
  categories <- bind_cols(categories,as.data.frame(m_cat))
  
  return(list(dataset, categories))
  
}
