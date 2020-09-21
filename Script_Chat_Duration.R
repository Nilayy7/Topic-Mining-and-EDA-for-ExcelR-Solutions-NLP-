library(tidyr)
library(ggplot2)
#chat_transcript <- read.csv("D:/Data Science Project/Conversation Mining/chat_transcript.csv", col.names = "Chat")
chat_Duration <- data.frame(V1 = grep(chat_transcript$Chat,pattern = "Patel:|Ananya:|Visitor|Service:|=====",value = T))
length(grep(chat_Duration$V1,pattern = "====="))


chat_Duration<- chat_Duration %>% separate(V1, c("A"),"\\)",fill = "left")
chat_Duration <- data.frame(V1 = gsub(pattern = "\\(",replacement = "",x = chat_Duration$A))

#df1 <- data.frame(V1 = chat_Duration[1,1])
#condition <- chat_Duration$V1 == "================================================================================"
#for (j in 1:(length(chat_Duration$V1)-1)){
#  if (condition[j]) {
#    A = chat_Duration$V1[j+1]
#    df1 <- rbind(df1,A)
#   }
#}

#df2 <- data.frame()
#condition <- chat_Duration$V1 == "================================================================================"
#for (j in 1:length(chat_Duration$V1)){
#  if (condition[j]) {
#    B = chat_Duration$V1[j-1]
#    df2 <- rbind(df2,B)
#  }
#}


duration <- data.frame(matrix(ncol = 2, nrow =length(chat_Duration$V1) ))
columnnames <- c("Start", "End")
colnames(duration) <- columnnames
duration[1,1] <- chat_Duration[1,1]
condition <- chat_Duration$V1 == "================================================================================"
for (j in 1:length(chat_Duration$V1)){
  if (condition[j]) {
    duration$Start[j] = chat_Duration$V1[j+1]
    duration$End[j] = chat_Duration$V1[j-1]
    }
}

start <- data.frame(duration[,1])
start <- drop_na(start)
end <- data.frame(duration[,2])
end <- drop_na(end)

Chat_Timings <- cbind(start,end)
colnames(Chat_Timings) <- columnnames

Chat_Timings$Start <- as.POSIXct(Chat_Timings$Start,format = "%Y-%m-%d %H:%M:%S")
Chat_Timings$End <- as.POSIXct(Chat_Timings$End,format = "%Y-%m-%d%H:%M:%S")
Chat_Timings["Duration"] <- as.numeric(Chat_Timings$End-Chat_Timings$Start)

Chat_Timings$Duration <- Chat_Timings$Duration/60
Non_Zero_Entries <- data.frame(Chat_Timings[Chat_Timings$Duration != 0, ])
Non_Zero_Entries <- Non_Zero_Entries[order(as.integer(Non_Zero_Entries$Duration),decreasing = TRUE), ]


#ggplot(Non_Zero_Entries, aes(x=Duration))+  
#  geom_bar(stat="bin", fill="steelblue",breaks = seq(0, 1, by = 0.1))+
#  theme_minimal()+scale_x_continuous("Less Than A Min Chat Duration", breaks=seq(0, 1, by = 0.1))

#ggplot(Non_Zero_Entries, aes(x=Duration))+  
#  geom_bar(stat="bin", fill="steelblue",breaks = seq(1, 30, by = 1))+
#  theme_minimal()+scale_x_continuous("1 Min to 30 Mins Chat Duration", breaks=seq(1, 30, by = 1))

#ggplot(Non_Zero_Entries, aes(x=Duration))+  
#  geom_bar(stat="bin", fill="steelblue",breaks = seq(30, 120, by = 5))+
#  theme_minimal()+scale_x_continuous("30 Mins to 120 Mins Chat Duration", breaks=seq(30, 120, by = 5))

#ggplot(Non_Zero_Entries, aes(x=Duration))+  
#  geom_bar(stat="bin", fill="steelblue",breaks = seq(120, 240, by = 24))+
#  theme_minimal()+scale_x_continuous("120 Mins to 240 Mins Chat Duration", breaks=seq(120, 240, by = 24))
