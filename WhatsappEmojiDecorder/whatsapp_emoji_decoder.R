#=======================================================================================
#
# File:        Whatsapp_emoji_decoder.R
# Author:      Junaid Effendi
# Description: This is a whatsapp emoji library, for extracting emojis and custom emojis, 
#              its count.
#
#=======================================================================================

#========================================================
# Install and Load Packages
#========================================================
install.packages("stringr")
library("stringr")
library("dplyr")

#========================================================
# Creating data frame without media
#========================================================
#Using same clean_data and removing all media messages (see whatsapp_parser.R)
clean_data_without_media <- clean_data
clean_data_without_media[clean_data_without_media=="<Media omitted>"] <- NA
clean_data_without_media <- clean_data_without_media[complete.cases(clean_data_without_media),]
head(clean_data_without_media)

#========================================================
# Emoji Loader Function
#========================================================

emoji_loader <- function(filename)
{
  emoji_data = read.csv(filename)
  head(emoji_data)
  
  #Adding emoji- to each name to extract patterns with accuracy
  emoji_data$Names <- paste0("emoji-", emoji_data$Names)
  emoji_data$Names <-  gsub(" ", "-", emoji_data$Names)
  
  
  #Adding \\ before a regex functional symbol
  sym <- c("\\$","\\.","\\^","\\*","\\[","\\]","\\?","\\(","\\)")
  sym_replace <- c("\\\\$","\\\\.","\\\\^","\\\\*","\\\\[","\\\\]","\\\\?","\\\\(","\\\\)")
  
  for(i in 1:length(sym)){
    emoji_data$Symbols <- gsub(sym[i],sym_replace[i],emoji_data$Symbols)
  }
  
  emoji_data$Symbols = str_trim(emoji_data$Symbols) #Removing spaces from both ends
  
  return(emoji_data)
}



emoji_all = emoji_loader("whatsapp_emoji_all.csv")
head(emoji_all)
emoji_all$Symbols

emoji_custom = emoji_loader("whatsapp_emoji_custom.csv")
head(emoji_custom)
emoji_custom$Symbols

emoji_human = emoji_loader("whatsapp_emoji_human.csv")
head(emoji_human)
emoji_human$Symbols

#========================================================
# Human Emoji Function
#========================================================

#Run this function only if you want to ignore the colors
#And want to consider all the six colors as same
emoji_human_color_ignore <- function(data)
{
  data$Names <- gsub(":.*$","",data$Names) 
  
  return(data)
}

emoji_human <- emoji_human_color_ignore(emoji_human)

#========================================================
# Emoji Replacer Function
#========================================================

#we have to replace each emoji with its name
emoji_replacer <- function(data)
{
  for(i in seq_len(nrow(data))){
    clean_data_without_media_temp = gsub(data[i,2],data[i,1],clean_data_without_media[,4])
    clean_data_without_media$message  = gsub(data[i,2],data[i,1],clean_data_without_media$message)
  }
  return(clean_data_without_media)
}

clean_data_without_media <- emoji_replacer(emoji_all)
clean_data_without_media <- emoji_replacer(emoji_custom)
clean_data_without_media <- emoji_replacer(emoji_human)

#Creating columns for each emoji with count
emoji_array <- as.character(emoji_all$Names)
clean_data_without_media_with_emoji <- clean_data_without_media

for(i in emoji_array){
  clean_data_without_media_with_emoji[,i] <- str_count(clean_data_without_media$message,i)
  
}

summary(clean_data_without_media_with_emoji)
unused_emoji_index = c(NA)
#Getting indexes of unused emojis
for(i in 6:length(clean_data_without_media_with_emoji)){
  sum = sum(clean_data_without_media_with_emoji[,i])
  cat(sprintf("%s = %i \n",names(clean_data_without_media_with_emoji)[i],sum))
  if(sum == 0)
  {
    unused_emoji_index <-  c(unused_emoji_index,i)
  }
}
unused_emoji_index <- unused_emoji_index[-1] 
unused_emoji_index
length(unused_emoji_index)

#Removing unused emojis
clean_data_without_media_with_emoji <- clean_data_without_media_with_emoji[,-unused_emoji_index]

#Removing emoji- from column names
names(clean_data_without_media_with_emoji) <- gsub("^.*?emoji-", "", names(clean_data_without_media_with_emoji))
