
Netflix_data = read.csv("netflix_titles.csv", na.strings = c("","NA"), stringsAsFactors = FALSE)
print(class(Netflix_data))

#print(Netflix_data)

print(str(Netflix_data))

print(summary((Netflix_data)))
print(dim(Netflix_data))




library("plotly")

Netflix_data$rating<- as.factor(Netflix_data$rating)

#printing missing values by creating a new data frame.
a<-data.frame("Variable"=c(colnames(Netflix_data)), "Missing Values"=sapply(Netflix_data, function(x) sum(is.na(x))), row.names = NULL)

print(a)
mode<- function(v){
  uniqv<- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

Netflix_data$rating[is.na(Netflix_data$rating)] = mode(Netflix_data$rating)
b<-data.frame("Variable"=c(colnames(Netflix_data)), "Missing Values"=sapply(Netflix_data, function(x) sum(is.na(x))), row.names = NULL)
print(b)

library("dplyr")
Netflix_data= distinct(Netflix_data, title, country, type, release_year, .keep_all = TRUE)
View(Netflix_data)




#question 1
library("plotly")
library("dplyr")
content_by_type <- Netflix_data %>% group_by(type) %>% 
  summarise(count = n())

plot_ly(content_by_type, labels = ~type, values = ~count,
        type = 'pie', marker = list(colors = c("#bd3939", "#399ba3"))) %>% 
  layout(title = "Proportion of Content by Type",legend = list(x = 200, y = 1))


#question2
library("plotly")
library("dplyr")

df_by_date <- Netflix_data %>% group_by(date_added, type) %>% 
  summarise(added_today = n()) %>% 
  group_by(type)


plot_ly(full_data, x = ~date_added, y = ~total_number_of_content, 
        mode = 'lines', type = 'scatter',
        color = ~type, colors = c("#bd3939",  "#9addbd", "#399ba3")) %>% 
  layout(yaxis = list(title = 'Count'), 
         xaxis = list(title = 'Date'), 
         title = "Growth in Content over the Years", margin = list(t = 54),
         legend = list(x = 100, y = 0.5))


#question3
library("plotly")
library("dplyr")
df_by_rating <- Netflix_data %>% group_by(rating) %>% 
  summarise(count = n())

plot_ly(df_by_rating, type = 'pie',
        labels = ~rating, values = ~count) %>% 
  layout(title = "Distribution of Content by Rating",
         legend = list(x = 100, y = 0.5))


#question4
library("plotly")
library("dplyr")
s_genres <- strsplit(Netflix_data$listed_in, split = ", ")

df_by_listed_in <-  group_by(type, listed_in) %>% 
  summarise(count = n()) 

plot_ly(df_by_listed_in, x = ~listed_in, y = ~count,
        type = 'bar', color = ~type,
        colors = c("#bd3939", "#399ba3")) %>%
  layout(xaxis = list(title = 'Genre'), 
         yaxis = list(title = 'Count'), 
         title = "Top Genres (Movie vs. TV Show)",
         legend = list(x = 100, y = 0.5))


#question5
library("plotly")
library("dplyr")
duration_full_subset <- duration_full[duration_full$country %in% 
                                        c("United States", "India", "United Kingdom",
                                          "Canada", "France", "Japan", "Spain", "South Korea",
                                          "Mexico", "Australia", "China", "Taiwan"),]
plot_ly(duration_full_subset, y = ~duration, color = ~country, type = "box") %>% 
  layout(xaxis = list(title = "Country"), 
         yaxis = list(title = 'Duration (in min)'),
         title = "Movie Duration in Top 12 Countries",
         legend = list(x = 100, y = 0.5))


#Access the title of first 20 Movie/ TV Shows of data set.

head_movieshows<- head(Netflix_data$title,20)
print("Title of first 20 Movies and Tv shows: ")
print(head_movieshows)


#Find the total number of movies and TV shows in the data set.
a1<- aggregate(Netflix_data$show_id ~ Netflix_data$type, Netflix_data, length)
colnames(a1)= c("type","length")
print(a1)

#Display the show_id and the title of Movie/TV Show released in the year 2020.
b1<- Netflix_data[Netflix_data$release_year==2020,c("show_id","title")]
print(b1)


#Display the number of TV Shows Produced by each country.
e<- subset(Netflix_data, type=="TV Show")
f<- aggregate(e$show_id~e$country, e, length)
colnames(f)= c("Country","Tv_Shows_Produced")
print(f)

#Display the director name along with number of movies they have produced.
num_movies<- subset(Netflix_data,type="Movie")
agg_movies<- aggregate(num_movies$show_id~num_movies$director, num_movies, length)
colnames(agg_movies)= c("Director","No_Of_Movies")
print(agg_movies)

#Display title and date_added for TV Shows that are listed in Reality TV.

Tv_title<- subset(Netflix_data,Netflix_data$type=="TV Show"& Netflix_data$listed_in=="Reality TV")
print(Tv_title[,c(3,7)])

#Display description of the movie released after 2010.

des<- subset(Netflix_data, type="Movie"& release_year>2010)
print(des[,"description"])
