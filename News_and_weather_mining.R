library(rvest)
library(mailR)
library(rJava)
library(googlesheets)
library(gridExtra)
library(data.table)
library(gtools)

#I currently am running this script daily at 6 am on an Amazon EC2 AWS server with RStudio installed.
#I automated it with the package 'cronR'. This otherwise will have to be ran manually, but it's still
#a useful project if you're too lazy to check the weather and news on your own. Why not scrape it instead?



#url for 'Google News' that we will scrape for today's news stories
news_source<- read_html("https://news.google.com/")
Headlines<-html_nodes(news_source,".esc-lead-article-title .titletext") %>% html_text()
num_rows<- data.frame(c(1:(length(Headlines))))
Headlines_data<- data.frame(num_rows, Headlines)
Story<- html_nodes(news_source, ".esc-lead-snippet-wrapper") %>% html_text()
Story_data<- data.frame( num_rows,Story)

#Combine the headlines and stories into one dataframe
News_report<-merge(Headlines_data, Story_data)

#If you un-comment-out the below line, you can see what the News table looks like without having to email it to yourself. 
#View(News_report)




#Replace the below url with the url for 'weather.gov' for your zipcode/area. Current link is for Charleston, SC.
weather_html<- read_html("http://forecast.weather.gov/MapClick.php?lat=32.7921457414339&lon=-79.94311474025227#.WKUT1zvyvb0")
weather_report_days<- html_nodes(weather_html, ".forecast-label") %>% html_text() 
weather_report_forecasts<- html_nodes(weather_html, ".forecast-text") %>% html_text() 

#Making the weather report data frame
colclasses<- c("character", "character")
col.names<- c("Day", "Weather")
final_report<- data.frame(col1 = weather_report_days,col2= weather_report_forecasts)
colnames(final_report)<- col.names

#If you un-comment-out the below line, you can see what the weather table looks like without having to email it to yourself. 
#View(final_report)


#Saves today's news report as a .png picture file. Each day it will overwrite the previous day's 'news file'
png("News_Report.png", height = 22*nrow(News_report), width = 1050*ncol(News_report))
news_r<- tableGrob(News_report)
grid.arrange(news_r)
dev.off()

#Saves today's weather report as a .png picture file. Each day it will overwrite the previous day's 'weather file'
png("Weather_Report.png", height = 23*nrow(final_report), width = 380*ncol(News_report))
weather_r<- tableGrob(final_report)
grid.arrange(weather_r)
dev.off()


#Below function is to email the picture files to yourself. You have to replace the "username" and "password" with your gmail 
#account's username and password. Also set the "<From>" and "Recipient 1 <To>" to your own gmail address. You can change the
#subject and body of the email to whatever message you want. Then you need to put the full directory path for the two image
#files so the script can attach them to the email. example: c("C://Users//your_name//News_Report.png", "C://Users//your_name//Weather_Report.png"),
#This script otherwise works as intended.
send.mail(from = "<your.email@gmail.com>",
          to = c("Recipient 1 <your.email@gmail.com>"),
          subject = "Daily Report",
          body = "Here is today's report.",
          attach.files = c("C://Users//your_name//News_Report.png", "C://Users//your_name//Weather_Report.png"),
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "username", passwd = "password", ssl =T),
          authenticate = T,
          send = TRUE)
