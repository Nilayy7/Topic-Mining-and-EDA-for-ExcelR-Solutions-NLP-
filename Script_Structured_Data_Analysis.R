library(ggplot2)
library(dplyr)
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



Country_Name <- data.frame(table(structured_data$Country_Name))
Country_Name <-Country_Name[order(-Country_Name$Freq),]
Country_Name <- Country_Name[!(Country_Name$Var1 == ""), ]
rownames(Country_Name) <- NULL
Country_Name <- Country_Name[1:10,]
Country_Name %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Country_Name") +ylab("Count")+theme_bw() 

Region <- data.frame(table(structured_data$Region))
Region <-Region[order(-Region$Freq),]
Region <- Region[!(Region$Var1 == ""), ]
rownames(Region) <- NULL
Region <- Region[1:10,]
Region %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Region") +ylab("Count")+theme_bw() 

City <- data.frame(table(structured_data$City))
City <-City[order(-City$Freq),]
City <- City[!(City$Var1 == ""), ]
rownames(City) <- NULL
City <- City[1:10,]
City %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("City") +ylab("Count")+theme_bw() 

Platform <- data.frame(table(structured_data$Platform))
Platform <-Platform[order(-Platform$Freq),]
Platform <- Platform[!(Platform$Var1 == ""), ]
rownames(Platform) <- NULL
Platform %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Platform") +ylab("Count")+theme_bw() 

Browser <- data.frame(table(structured_data$Browser))
Browser <-Browser[order(-Browser$Freq),]
Browser <- Browser[!(Browser$Var1 == ""), ]
rownames(Browser) <- NULL
Browser <- Browser[1:10,]
Browser %>%  ggplot(aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Browser") +ylab("Count")+theme_bw() 


ggplot(structured_data,aes(x=Hour,fill=Hour))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar()
ggplot(structured_data,aes(x=Day,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar()
ggplot(structured_data,aes(x=Month,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar()

