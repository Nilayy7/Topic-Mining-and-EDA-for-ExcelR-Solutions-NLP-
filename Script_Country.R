library(ggplot2)
library(dplyr)
library(tidyr)
#structured_data <- read.csv("D:/Data Science Project/Conversation Mining/structured_data.csv")
structured_data$Timestamp <- as.POSIXct(structured_data$Timestamp,format = "%Y-%m-%d%H:%M:%S")
structured_data$Unread <- as.factor(structured_data$Unread)
structured_data$Country_Code <- as.factor(structured_data$Country_Code)
structured_data$Country_Name <- as.factor(structured_data$Country_Name)
structured_data$Region <- as.factor(structured_data$Region)
structured_data$City <- as.factor(structured_data$City)
structured_data$Platform <- as.factor(structured_data$Platform)
structured_data$Browser <- as.factor(structured_data$Browser)
structured_data$Time<- format(as.POSIXct(structured_data$Timestamp,format="%Y:%m:%d%H:%M:%S"),"%H:%M:%S")
structured_data$Hour<- format(as.POSIXct(structured_data$Timestamp,format="%Y:%m:%d%H:%M:%S"),"%H")
structured_data$Date<- format(as.POSIXct(structured_data$Timestamp,format="%Y:%m:%d%H:%M:%S"),"%Y:%m:%d")
structured_data$Day <- weekdays(as.Date(structured_data$Timestamp,format ="%Y:%m:%d"))
structured_data$Month <- months(as.Date(structured_data$Timestamp,format ="%Y:%m:%d"))
structured_data$Date <- as.Date(structured_data$Date,format ="%Y:%m:%d")
structured_data$Day <- factor(structured_data$Day,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),ordered=TRUE)
structured_data$Month <- factor(structured_data$Month,levels=c("October","November","December","January"),ordered=TRUE)

Country<- structured_data[structured_data[, "Country_Name"] == input$CountryName,]

Region <- data.frame(table(Country$Region))
Region <-Region[order(-Region$Freq),]
Region <- Region[!(Region$Var1 == ""), ]
rownames(Region) <- NULL
Region <- Region[1:10,]
Region %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Region") +ylab("Count")+theme_bw() 

City <- data.frame(table(Country$City))
City <-City[order(-City$Freq),]
City <- City[!(City$Var1 == ""), ]
rownames(City) <- NULL
City <- City[1:10,]
City %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("City") +ylab("Count")+theme_bw() 

Platform <- data.frame(table(Country$Platform))
Platform <-Platform[order(-Platform$Freq),]
Platform <- Platform[!(Platform$Var1 == ""), ]
rownames(Platform) <- NULL
Platform <- drop_na(Platform)
Platform %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Platform") +ylab("Count")+theme_bw() 

Browser <- data.frame(table(Country$Browser))
Browser <-Browser[order(-Browser$Freq),]
Browser <- Browser[!(Browser$Var1 == ""), ]
rownames(Browser) <- NULL
Browser <- Browser[1:10,]
Browser <- drop_na(Browser)
Browser %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Browser") +ylab("Count")+theme_bw() 


ggplot(Country,aes(x=Hour,fill=Hour))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.5)
ggplot(Country,aes(x=Day,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.5)
ggplot(Country,aes(x=Month,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.5)
