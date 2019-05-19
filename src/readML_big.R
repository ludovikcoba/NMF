if (!require(stringr)) install.packages("stringr", repos='http://cran.us.r-project.org')


getML<- function(ds){

  if(ds == "ml100k"){
    
    dataset <- read_table2("datasets/MovieLens/ml-100k/u.data", #<---dataset
                           col_names = FALSE)
    
    dataset <- dataset[,-4]

    colnames(dataset) <-  c("user", "item", "score")
    
    categories <- read_delim("datasets/MovieLens/ml-100k/u.item", #<----categories' file
                             ";", escape_double = FALSE, col_names = FALSE,      trim_ws = TRUE)
    
    
    colnames(categories) <- c("item", "movie title", "release date", "video release date", "IMDb URL", "unknown", "Action", "Adventure", "Animation",              "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",              "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",        "Thriller", "War", "Western")
    
    
    categories <-categories %>%
      dplyr::mutate_at(.vars = 6:24, dplyr::funs(as.numeric(.))) %>%
      dplyr::select_at(.vars = c(1,6:24))
    
    
    categories <- categories %>%
      dplyr::arrange(item)
    
    return(list(dataset, categories))
    
  }else if(ds == "ml1m") {
    
    if(!file.exists("datasets/MovieLens/ml-1m/ratings.dat") || !file.exists("datasets/MovieLens/ml-1m/movies.dat")){
      download.file("http://files.grouplens.org/datasets/movielens/ml-1m.zip", destfile = "datasets/MovieLens/ml1m.zip")
      unzip("datasets/MovieLens/ml1m.zip", exdir = "datasets/MovieLens/")
    }
    
    pathRT <- "datasets/MovieLens/ml-1m/ratings.dat"
    pathCT <- "datasets/MovieLens/ml-1m/movies.dat"
    
    sep <- "::"
    
  } else if (ds == "ml10m"){
    
    if(!file.exists("datasets/MovieLens/ml-10M100K/ratings.dat") || !file.exists("datasets/MovieLens/ml-10M100K/movies.dat")){
      download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", destfile = "datasets/MovieLens/ml10m.zip")
      unzip("datasets/MovieLens/ml10m.zip", exdir = "datasets/MovieLens/")
    }
    
    pathRT <- "datasets/MovieLens/ml-10M100K/ratings.dat"
    pathCT <- "datasets/MovieLens/ml-10M100K/movies.dat"
    
    sep <- "::"
    
  }else if(ds == "ml20m"){
    
    if(!file.exists("datasets/MovieLens/ml-20m/ratings.csv") || !file.exists("datasets/MovieLens/ml-20m/movies.csv")){
      download.file("http://files.grouplens.org/datasets/movielens/ml-20m.zip", destfile = "datasets/MovieLens/ml20m.zip")
      unzip("datasets/MovieLens/ml20m.zip", exdir = "datasets/MovieLens/")
    }
    
    pathRT <- "datasets/MovieLens/ml-20m/ratings.csv"
    pathCT <- "datasets/MovieLens/ml-20m/movies.csv"
    
    sep <- ","
    
  }else{
    stop("Dataset not supported.")
  }
  
  

  
  

  
  if(sep == ','){
    
    dataset <- read_csv(pathRT, 
                           col_names = T, trim_ws = FALSE) 
    dataset <- dataset[,-4]
    colnames(dataset) <- c("user","item" ,"score")
    
    
    categories <- read_csv(pathCT, 
                           col_names = T, trim_ws = FALSE)
    colnames(categories) <- c("item","movie_title" ,"categories")
    
  }else{
    dataset <- read_table2(pathRT, #<---dataset
                           col_names = FALSE)
    dataset <- dataset %>% 
      tidyr::separate(X1, into = c("user","item" ,"score"), sep = sep)
    dataset <- dataset %>% 
      dplyr::mutate(user = as.numeric(user), item = as.numeric(item), score = as.numeric(score))
    
    categories <- read_csv(pathCT, 
                           col_names = FALSE, trim_ws = FALSE)
    categories <-   categories %>% 
      tidyr::separate(X1, into = c("item","movie_title" ,"categories"), sep = sep)
    
  }
  
  m_cat_list <- str_split(categories$categories, pattern = "\\|")
  
  unique_categories <- unique(unlist(m_cat_list))
  
  m_cat <- matrix(0, nrow = nrow(categories), ncol = length(unique_categories))
  
  for(i in 1:length(m_cat_list)){
    m_cat[i,which(unique_categories %in% m_cat_list[[i]])] <- 1
  }
  
  unique_categories[which(is.na(unique_categories))] <- "unknown"
  
  colnames(m_cat) = unique_categories
  
  categories <- categories %>%
    dplyr::select(item) %>%
    dplyr::mutate(item = as.numeric(item))
  
  categories <- bind_cols(categories,as.data.frame(m_cat))
  
  return(list(dataset, categories))
  
}
