library(tidyverse)
library(recommenderlab)
library(cluster)

#read files
ratings<-read.csv("BX-Book-Ratings.csv", sep=";")
books<-read.csv("BX-Books.csv", sep=";")
users<-read.csv("BX-Users.csv", sep=";")

glimpse(books)
set.seed(168)

#to generate more realistic data, we will include a new variable called ‘Category’. This variable will indicate if the book belongs to any of the following categories:
categories = c("Action and Adventure","Classic","Detective and Mystery","Fantasy")
books$category = sample( categories, nrow(books), replace=TRUE, prob=c(0.25, 0.3, 0.25, 0.20))## This could be accomplished with some NLP and sentiment analysis of description or could be included in data sources in real life examples. 
books$category = as.factor(books$category)

rm(categories)

#transform data to be easily to read when larger tables are built
books$ISBN = paste0("Isbn.",books$ISBN)
users$User.ID = paste0("User.",users$User.ID)
ratings$ISBN = paste0("Isbn.",ratings$ISBN)
ratings$User.ID = paste0("User.",ratings$User.ID)


#EDA
#ratings distribution
ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10) 

ratings = ratings[ratings$Book.Rating!= 0, ]

#ratings distribution without 0 ratings items
ratings %>%
  group_by(Book.Rating) %>%
  summarize(cases = n()) %>%
  ggplot(aes(Book.Rating, cases)) + geom_col() +
  theme_minimal() + scale_x_continuous(breaks = 0:10)



ratings_sum = ratings %>%
  group_by(User.ID) %>%
  count() 

summary(ratings_sum$n)
#less than 75% of users have 3 or less recommendations

#reduce data to that range
user_index = ratings_sum$User.ID[ratings_sum$n>4]

users = users[users$User.ID %in% user_index, ]
ratings = ratings[ratings$User.ID %in% user_index, ]
books = books[books$ISBN %in% ratings$ISBN,]

rm(ratings_sum, user_index)


#content based recommendation

#issue with scaling and available resources in a local machine, sampling data to 10k books

book_feature = books[1:10000,c("Book.Author","Publisher","category")] 
book_feature$Publisher<-as.factor(book_feature$Publisher)
book_feature$Book.Author<-as.factor(book_feature$Book.Author)
str(book_feature)#data needed to be turn into factor to use as gower distance for dissimilarity calculations
dissimilarity = daisy(book_feature, metric = "gower", weights = c(2,0.5,1))
dissimilarity = as.matrix(dissimilarity)

row.names(dissimilarity)<-  books$ISBN[1:10000]
colnames(dissimilarity)<- books$ISBN[1:10000]

#choose a user has and rated books to recommend books based on content. We must consider that we only have a sub sample of data 
user_id = "User.1167"

user_books = ratings %>%
  filter(User.ID == user_id & ISBN %in% books$ISBN[1:10000]) %>%
  arrange(desc(Book.Rating))

#head(user_books,10)

books$ISBN = as.character(books$ISBN)
selected_books = user_books[ ,c("ISBN", "Book.Rating")]
#we recommend 5 books based on the content consume by a single user
recommend = function(selected_books, dissimilarity_matrix, 
                     books, n_recommendations = 5){
  
  selected_book_indexes = which(colnames(dissimilarity_matrix) %in% selected_books$ISBN)
  
  
  results = data.frame(dissimilarity_matrix[, selected_book_indexes], 
                       recommended_book = row.names(dissimilarity_matrix),
                       stringsAsFactors = FALSE) 
  
  
  recommendations = results %>%
    pivot_longer(cols = c(-"recommended_book") , names_to = "readed_book", 
                 values_to = "dissimilarity") %>%
    left_join(selected_books, by = c("recommended_book" = "ISBN"))%>%
    arrange(desc(dissimilarity)) %>%
    filter(recommended_book != readed_book) %>%
    filter(!is.na(Book.Rating) ) %>%
    mutate(
      similarity = 1 - dissimilarity,
      weighted_score = similarity * Book.Rating) %>%
    arrange(desc(weighted_score)) %>%
    filter(weighted_score>0) %>%
    group_by(recommended_book) %>% slice(1) %>%
    top_n(n_recommendations, weighted_score)  %>%
    left_join(books, by = c("recommended_book" = "ISBN"))
  
  return(recommendations)
}

recommendations = recommend(selected_books, dissimilarity, books)
recommendations

#item-based recommendation
#creating a user-product table
user_item = ratings %>%
  top_n(10000) %>%
  pivot_wider(names_from = ISBN,values_from = Book.Rating) %>%
  as.data.frame()

row.names(user_item) = user_item$User.ID
user_item$User.ID = NULL

user_item = as.matrix(user_item)

user_item[1:5,1:5]

#calculating degree of sparsity due to NA.
sum(is.na(user_item)) /  ( ncol(user_item) * nrow(user_item) )

cos_similarity = function(A,B){
  num = sum(A *B, na.rm = T)
  den = sqrt(sum(A^2, na.rm = T)) * sqrt(sum(B^2, na.rm = T)) 
  result = num/den
  
  return(result)
}

#item recommendation to selected items
item_recommendation = function(book_id, rating_matrix = user_item, n_recommendations = 5){
  
  book_index = which(colnames(rating_matrix) == book_id)
  
  similarity = apply(rating_matrix, 2, FUN = function(y) 
    cos_similarity(rating_matrix[,book_index], y))
  
  recommendations = tibble(ISBN = names(similarity), 
                           similarity = similarity) %>%
    filter(ISBN != book_id) %>% 
    top_n(n_recommendations, similarity) %>%
    arrange(desc(similarity)) 
  
  return(recommendations)
  
}
#item selected to test item recommendation
recom_cf_item = item_recommendation("Isbn.0446677450")

recom_cf_item = recom_cf_item %>%
  left_join(books, by = c("ISBN" = "ISBN")) 
recom_cf_item


#user-based recommendation collaboritve 

user_recommendation = function(user_id, user_item_matrix = user_item,
                               ratings_matrix = ratings,
                               n_recommendations = 5,
                               threshold = 1,
                               nearest_neighbors = 10){
  
  user_index = which(rownames(user_item_matrix) == user_id)
  
  similarity = apply(user_item_matrix, 1, FUN = function(y) 
    cos_similarity(user_item_matrix[user_index,], y))
  
  similar_users = tibble(User.ID = names(similarity), 
                         similarity = similarity) %>%
    filter(User.ID != user_id) %>% 
    arrange(desc(similarity)) %>%
    top_n(nearest_neighbors, similarity)
  
  
  readed_books_user = ratings_matrix$ISBN[ratings_matrix$User.ID == user_id]
  
  recommendations_user = ratings_matrix %>%
    filter(
      User.ID %in% similar_users$User.ID &
        !(ISBN %in% readed_books_user)) %>%
    group_by(ISBN) %>%
    summarise(
      count = n(),
      Book.Rating = mean(Book.Rating)
    ) %>%
    filter(count > threshold) %>%
    arrange(desc(Book.Rating), desc(count)) %>%
    head(n_recommendations)
  
  return(recommendations_user)
  
}

recom_cf_user = user_recommendation("User.99", n_recommendations = 20)
recom_cf_user = recom_cf_user %>%
  left_join(books, by = c("ISBN" = "ISBN"))
recom_cf_user

#for full scope solutions back to users the list of recommendations could be run against the imagine and presented in a canvas to user by different levels by checkout such as item recommedantions,  or when browsing  books is content based and/or user base collaborative recommendations. user base collaborative recommendations could be use as a after follow up to increase return orders or use with a/b testing for marketing campaigns to get more returned customers  
