
dataset <- read_table2("datasets/ml-100k/u.data", #<---dataset
                      col_names = FALSE)

dataset <- dataset[,-4]

colnames(dataset) <-  c("user", "item", "score")

categories <- read_delim("datasets/ml-100k/u.item", #<----categories' file
                               ";", escape_double = FALSE, col_names = FALSE,      trim_ws = TRUE)


colnames(categories) <- c("item", "movie title", "release date", "video release date", "IMDb URL", "unknown", "Action", "Adventure", "Animation",              "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",              "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi",        "Thriller", "War", "Western")


categories <-categories %>%
  mutate_at(.vars = 6:24, funs(as.numeric(.))) %>%
  select_at(.vars = c(1,6:24))
categories

categories <- categories %>%
  arrange(item)

