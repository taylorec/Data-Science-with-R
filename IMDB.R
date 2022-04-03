######## IMDB WEBSITE

library(rvest)

url="http://www.imdb.com/search/title?year=2017&title_type=feature&"

#Reading the HTML code from the website
webpage = read_html(url)

#CSS selectors to scrap the rankings section
rank_data_html = html_nodes(webpage,'.text-primary')

class(rank_data_html)

#Converting the ranking data to text
rank_data =html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)
class(rank_data)

##convert to numerical format
rank_data=as.numeric(rank_data)

head(rank_data)
class(rank_data)

#Using CSS selectors to scrap the title section
title_data_html = html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data = html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrap the Movie runtime section
runtime_data_html = html_nodes(webpage,' .runtime')

#Converting the runtime data to text
runtime_data = html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

runtime_data=gsub(" min","",runtime_data)
runtime_data=as.numeric(runtime_data)

#runtime data
head(runtime_data)


genre_data_html = html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data = html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

genre_data=gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data=gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data=gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data=as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)
############################################################
############ combine into a df


movies_df=data.frame(Rank = rank_data, Title = title_data,
                     Genre=genre_data)

head(movies_df)

barplot(table(movies_df$Genre))
