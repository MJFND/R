#=======================================================================================
#
# File:        utilizingWhatsappPackage.R
# Author:      Junaid Effendi
# Description: This code use the functions from the WhatsAppChatCleaner custom package,
#              to carry out emoji cleaning.
#
#=======================================================================================


# To load package from github we need devtools
install.packages("devtools")
library("devtools")

# Lets load our custom written package
# see more > https://github.com/MJFND/R/tree/master/WhatsappChatCleanerPackage/WhatsAppChatCleaner
install_github("MJFND/R/WhatsappChatCleanerPackage/WhatsAppChatCleaner")
library("WhatsAppChatCleaner")


# Loading required packages to use WhatsAppChatCleaner
install_load_packages()


# the first thing we need here is to clean the data
clean_data <- clean_whatsapp_chat("whatsapp_friends.txt")


# media from the clean_data
clean_media_nomedia <- media_remove(clean_data)

# now we need to load emojis from emoji datasets
# get from here > https://github.com/MJFND/R/tree/master/WhatsappChatCleanerPackage/EmojiDataSets
# I have downloaded and paste it into the same project folder 
# so will pass a file name only

whatsapp_emoji_all <- emoji_loader("whatsapp_emoji_all.csv")
head(whatsapp_emoji_all)

whatsapp_emoji_custom <- emoji_loader("whatsapp_emoji_custom.csv")
head(whatsapp_emoji_custom)

whatsapp_emoji_human <- emoji_loader("whatsapp_emoji_human.csv")
head(whatsapp_emoji_human)

# OPTIONAL! 
# If you want to ignore the color tone of human emojis 
whatsapp_emoji_human <- emoji_human_color_ignore(whatsapp_emoji_human)


# Now lets replace eomojis with their respective neames
clean_data_without_media_emoji_replaced <- emoji_replacer(whatsapp_emoji_all, clean_media_nomedia)

#Now passing the updated clean data so we can do the work on the remaining emojis
clean_data_without_media_emoji_replaced <- emoji_replacer(whatsapp_emoji_custom, clean_data_without_media_emoji_replaced)
clean_data_without_media_emoji_replaced <- emoji_replacer(whatsapp_emoji_human, clean_data_without_media_emoji_replaced)


# lets make new column for each emoji with count
# no need to pass emoji_custom as emoji_all overlaps 
clean_data_without_media_emoji_replaced <- make_emoji_count_column(whatsapp_emoji_all, clean_data_without_media_emoji_replaced)
clean_data_without_media_emoji_replaced <- make_emoji_count_column(whatsapp_emoji_human, clean_data_without_media_emoji_replaced)


# now removing unused emoji columns
clean_data_without_media_emoji_replaced <- remove_unused_emoji(clean_data_without_media_emoji_replaced)
