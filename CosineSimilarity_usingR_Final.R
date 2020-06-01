install.packages("tm") # For text mining and data cleaning activities
install.packages ("SnowballC") # For stemming of text
install.packages("textstem") # For stemming of text
install.packages("readxl") # For reading excel files
install.packages("dplyr") # For filtering and selection of data
install.packages("text2vec") # For calculating cosine similarity
install.packages("superheat") # For plotting heat map
install.packages("magick") # For reading images

library(tm) # For text mining and data cleaning activities
library(textstem) # For stemming of text
library(SnowballC) # For stemming of text
library(readxl) # For reading excel files
library(dplyr) # For filtering and selection of data
library(text2vec) # For calculating cosine similarity
library(superheat) # For plotting heat map
library(magick) # For reading images

Sampledata<-data.frame("Text"=c("Amazon Echo Show - Greatest Gift EVER", "Best gift ever!","Gift for a relative they said they loved the gift.","we enjoy asking it questions about history & educational stuff for kids.","I LOVE MY ECHO SHOW, ALEXA IS SO HELPFULL SHE ANSWERS JUST ABOUT EVERY QUESTION I HAVE","It,Äôs awesome being able to control the lighting and lots of other controls with this!","The Amazon echo is excellent. Easy to set up, and we now use it to automate the entire house from our air-conditioning thermostat to every light in the house with the Phillips hue bulbs. We went and got one for everyone in our family."))

#Excel_example<-data.frame("Text"=c("Wonderful smartphone under 10,000", "Smartphone is very bad","Best smartphone of the year. Very good","Smartphone is very good"))

CorpusData<-Corpus(VectorSource(Sampledata$Text)) # Converts to a Corpus i.e. Collection of text

# Data Cleaning using tm package

CorpusData<-tm_map(CorpusData,tolower) # to convert the text to lowercase
CorpusData <-tm_map(CorpusData, removeNumbers) # to remove numbers
CorpusData <- tm_map(CorpusData, removePunctuation) # to remove punctuation
CorpusData <- tm_map(CorpusData, stripWhitespace) # to remove extra spaces
CorpusData <- tm_map(CorpusData, removeWords,stopwords("english")) # to remove stop words like a, and,...
stopwords("english") # to see list of stop words
CorpusData <- tm_map(CorpusData, removeWords,c('landline')) # To remove custom stop words

CorpusData<-tm_map(CorpusData,lemmatize_strings)

#Creating a term document matrix with tf-idf weight
tdm_tfidf <- DocumentTermMatrix(CorpusData,
                                control = list(weighting = weightTfIdf))

m_tfidf <- as.matrix(tdm_tfidf)

# Calculating Euclidean distance

Euclidean_Distance<-dist(m_tfidf, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

Minkowski_formula<-image_read("D:/OneDrive/SimplyAnalytics/Sessions/CosineSimilarity/Minkowski.svg")

print(Minkowski_formula)

# Calculating Cosine similarity 

library(text2vec)
Cosine_Similarity <- sim2(m_tfidf, y = NULL, method = c("cosine"), norm = c("l2"))

# Converting Euclidean_Distance values into a data frame

Euclidean_Distance_matrix<-as.matrix(Euclidean_Distance)
Euclidean_Distance_dataframe<-as.data.frame(Euclidean_Distance_matrix)


# To see similar documents for a particular document using Euclidean distance

get_similar_document <- function(Euclidean_Distance_dataframe, document_to_find_similarity, n_recommendations = 2){
  sort(Euclidean_Distance_dataframe[document_to_find_similarity, ], decreasing = FALSE)[1:( n_recommendations)]
}

diag(Euclidean_Distance_dataframe) <- NA

get_similar_document(Euclidean_Distance_dataframe,6)

# Converting Cosine_Similarity matrix into a data frame

Cosine_Similarity_dataframe<-as.data.frame(Cosine_Similarity)

# To see similar documents for a particular document using Cosine Similarity

get_similar_document <- function(Cosine_Similarity_dataframe, document_to_find_similarity, n_recommendations = 2){
  sort(Cosine_Similarity_dataframe[document_to_find_similarity, ], decreasing = TRUE)[1:( n_recommendations)]
}

diag(Cosine_Similarity_dataframe) <- NA

get_similar_document(Cosine_Similarity_dataframe, 6)

Sampledata$Text


library(superheat)
Heatmap_Euclidean<-superheat(Euclidean_Distance_dataframe, 
          
          # place dendrograms on columns and rows 
          #row.dendrogram = T, 
          #col.dendrogram = T,
         
          # make gridlines white for enhanced prettiness
          grid.hline.col = "white",
          grid.vline.col = "white",
          order.rows = 1:7,
          order.cols = 1:7,
          # rotate bottom label text
          bottom.label.text.angle = 0,
          title="Heat Map - using Euclidean")

Heatmap_Cosine<-superheat(Cosine_Similarity_dataframe, 
          
          yr=Cosine_Similarity_dataframe[,4],
          
          # place dendrograms on columns and rows 
          #row.dendrogram = F, 
          #col.dendrogram = F,
          
          # make gridlines white for enhanced prettiness
          grid.hline.col = "white",
          grid.vline.col = "white",
          order.rows = 1:7,
          order.cols = 1:7,
          # rotate bottom label text
          bottom.label.text.angle = 0,
          
          title="Heat Map - Cosine Similarity")



# Larger data set - Amazon Echo reviews

AmazonEcho<-read_excel("D:/SimplyAnalytics/Dataset/AmazonEcho_Dataset.xlsx")

CorpusData_AmazonEcho<-Corpus(VectorSource(AmazonEcho$reviews.text)) # Converts to a Corpus i.e. Collection of text

# Data Cleaning using tm package

CorpusData_AmazonEcho<-tm_map(CorpusData_AmazonEcho,tolower) # to convert the text to lowercase
CorpusData_AmazonEcho <-tm_map(CorpusData_AmazonEcho, removeNumbers) # to remove numbers
CorpusData_AmazonEcho <- tm_map(CorpusData_AmazonEcho, removePunctuation) # to remove punctuation
CorpusData_AmazonEcho <- tm_map(CorpusData_AmazonEcho, stripWhitespace) # to remove extra spaces
CorpusData_AmazonEcho <- tm_map(CorpusData_AmazonEcho, removeWords,stopwords("english")) # to remove stop words like a, and,...
stopwords("english") # to see list of stop words
CorpusData_AmazonEcho <- tm_map(CorpusData_AmazonEcho, removeWords,c('landline')) # To remove custom stop words

CorpusData_AmazonEcho<-tm_map(CorpusData_AmazonEcho,lemmatize_strings)

#Creating a term document matrix with tf-idf weight
tdm_tfidf_AmazonEcho <- DocumentTermMatrix(CorpusData_AmazonEcho,
                                             control = list(weighting = weightTfIdf))

m_tfidf_AmazonEcho <- as.matrix(tdm_tfidf_AmazonEcho)

# Calculating Euclidean distance

Euclidean_Distance_AmazonEcho<-dist(m_tfidf_AmazonEcho, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)

# Calculating Cosine similarity 


Cosine_Similarity_AmazonEcho <- sim2(m_tfidf_AmazonEcho, y = NULL, method = c("cosine"), norm = c("l2"))

# Converting Euclidean_Distance_AmazonEcho values into a data frame

Euclidean_Distance_AmazonEcho_matrix<-as.matrix(Euclidean_Distance_AmazonEcho)
Euclidean_Distance_AmazonEcho_dataframe<-as.data.frame(Euclidean_Distance_AmazonEcho_matrix)


# To see similar documents for a particular document using Euclidean distance

get_similar_document <- function(Euclidean_Distance_AmazonEcho_dataframe, document_to_find_similarity, n_recommendations = 5){
  sort(Euclidean_Distance_AmazonEcho_dataframe[document_to_find_similarity, ], decreasing = FALSE)[1:( n_recommendations)]
}

get_similar_document(Euclidean_Distance_AmazonEcho_dataframe, 1)

# Converting Cosine_Similarity_AmazonEcho matrix into a data frame

Cosine_Similarity_AmazonEcho_dataframe<-as.data.frame(Cosine_Similarity_AmazonEcho)

# To see similar documents for a particular document using Cosine Similarity

get_similar_document <- function(Cosine_Similarity_AmazonEcho_dataframe, document_to_find_similarity, n_recommendations = 5){
  sort(Cosine_Similarity_AmazonEcho_dataframe[document_to_find_similarity, ], decreasing = TRUE)[1:( n_recommendations)]
}

diag(Cosine_Similarity_AmazonEcho_dataframe) <- NA

get_similar_document(Cosine_Similarity_AmazonEcho_dataframe, 1)




Heatmap_Euclidean_AmazonEcho<-superheat(Euclidean_Distance_AmazonEcho_dataframe, 
                                          
                                          # place dendrograms on columns and rows 
                                          row.dendrogram = T, 
                                          col.dendrogram = T,
                                          
                                          # make gridlines white for enhanced prettiness
                                          grid.hline.col = "white",
                                          grid.vline.col = "white",
                                          order.rows = 1:845,
                                          order.cols = 1:845,
                                          # rotate bottom label text
                                          bottom.label.text.angle = 0,
                                          title="Heat Map and Dendogram - using Euclidean")

Heatmap_Cosine_AmazonEcho<-superheat(Cosine_Similarity_AmazonEcho_dataframe, 
                                       
                                       # place dendrograms on columns and rows 
                                       row.dendrogram = T, 
                                       col.dendrogram = T,
                                       
                                       # make gridlines white for enhanced prettiness
                                       grid.hline.col = "white",
                                       grid.vline.col = "white",
                                       order.rows = 1:845,
                                       order.cols = 1:845,
                                       # rotate bottom label text
                                       bottom.label.text.angle = 0,
                                       title="Heat Map and Dendogram - Cosine Similarity")




