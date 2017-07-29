#=======================================================================================
#
# File:        Whatsapp_parser.R
# Author:      Junaid Effendi
# Description: This code parses whatsap group chat files, you may need to customize it.
# Blog Link: http://www.tensorflowexamples.com/2017/07/analyzing-whatsapp-group-chat-using-r.html
#=======================================================================================

#========================================================
# Install and Load Packages
#========================================================
install.packages(c("stringr", "zoo", "ggplot2", "scales", "dplyr"))
library("stringr")
library("zoo")
library("ggplot2")
library("dplyr")
library("scales")

#========================================================
# Loading Data
#========================================================
all_data = readLines("whatsap_friends.txt")
head(all_data)

#========================================================
# Data Wrangling and Cleaning
#========================================================

#Removing the first message
all_data = all_data[-1]

#Extracting date and time  
date_time <- format(strptime(all_data, "%m/%d/%y, %I:%M %p"),"%m/%d/%y, %H:%M")
head(date_time)

#Extracting date
date = gsub(",.*$","",date_time) #Fetching all before ","

#Extracting time
time = gsub("^.*,","",date_time) #Fetching all after ","
time = str_trim(time) #Removing spaces from both ends
time 

sender <- 'sender' #Temorary Data
message <- all_data

#Creating Data Frame
clean_data = data.frame(date,time,sender,message)
head(clean_data)

#Extracting sender and message from the data frame
#Fetching only complete cases
sender_message = clean_data[complete.cases(clean_data),4] 
sender_message = gsub("^.*?-","",sender_message)
sender_message = str_trim(sender_message) 

#Extracting message
message = gsub("^.*?:","",sender_message) 
message = str_trim(message) #Removing spaces from both ends
head(message)
#Updating the data frame with new message data
clean_data$message <- as.character(clean_data$message)
clean_data[complete.cases(clean_data),4] <- message

#Extracting sender names
sender = gsub("?:.*$","",sender_message) 
# Removing prefixes and other names from sender "Customized"
sender = gsub("NED?.*$","",sender)
sender = str_trim(sender) #Removing spaces from both ends
head(sender) 
#Updating the data frame with new sender data
clean_data$sender <- as.character(clean_data$sender)
clean_data[complete.cases(clean_data),3] <- sender

#Replacing remaining "sender" values with NA
clean_data[clean_data=="sender"]<- NA


#Using transform function from Zoo Package 
#Filling NA with previous values
#Detailed explanation > http://www.tensorflowexamples.com/2017/07/analyzing-whatsapp-group-chat-using-r.html
clean_data <- transform(clean_data, date = na.locf(date), time = na.locf(time),
                        sender = na.locf(sender))


#This is a custom function, 
#Use only if you want to remove unknown contacts
clean_data = subset(clean_data, !grepl("92 334", sender))
nrow(clean_data)

#Refactorizing 
clean_data$sender <- as.factor(clean_data$sender)


#Exploring Data
summary(clean_data)
nrow(clean_data)

summary(clean_data$sender,maxsum = 25)

#========================================================
# Feature Engineering
#========================================================
#Setting media messages to empty space for length 0
clean_data[clean_data=="<Media omitted>"]<-""
#Finding length of each message
clean_data$message_length <- nchar(clean_data$message)
#Restting media messages 
clean_data[clean_data==""]<-"<Media omitted>"
summary(clean_data$message_length)

#========================================================
# Data Visualization
#========================================================

#===== Plot 1 =====#
ggplot(clean_data, aes(sender))+
  geom_bar()+
  ylab("number of messages")

#===== Plot 2 =====#
ggplot(clean_data, aes(sender,message_length))+
  geom_bar(stat="identity")+
  ylab("sum of message length")

#===== Plot 3 =====#
#Mean of the length of each message for each sender
grouped_mean_length <- aggregate(message_length ~ sender, clean_data, mean)

ggplot(grouped_mean_length, aes(sender,message_length))+
  geom_bar(stat="identity")+
  ylab("mean of message length")

#===== Plot 4 =====#
#No of records across date
date_count<-data.frame(table(clean_data$date))
colnames(date_count) <- c("date", "count")

#Scatter plot for the number of messages per given date
ggplot(date_count, aes(as.Date(date, "%m/%d/%y"),count))+
  geom_line()+
  scale_x_date(breaks = date_breaks("1 months"),labels = date_format("%m/%y"))+
  xlab("date")+
  ylab("number of messages")

#===== Plot 5 =====#
#No of records across date and sender
date_sender_count<-data.frame(table(clean_data$sender,clean_data$date))
colnames(date_sender_count) <- c("sender","date", "count")

#Scatter plot for the number of messages for each sender per given date
ggplot(date_sender_count, aes(as.Date(date, "%m/%d/%y"), count, color=sender))+
  geom_line()+
  scale_x_date(breaks = date_breaks("1 months"),labels = date_format("%m/%y"))+
  xlab("date")+
  ylab("number of messages")
