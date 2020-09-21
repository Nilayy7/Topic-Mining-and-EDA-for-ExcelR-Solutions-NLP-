library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(ggplot2)
library(tidytext)
library(topicmodels)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(RWeka)
library(textclean)
library(doSNOW)
library(RJSONIO)
library(LDAvis)

ui <- dashboardPage(
    skin = "black", 
    dashboardHeader(title = "Conversation Mining- ExcelR Solutions", titleWidth = 400,
                    
                    tags$li(a(href = 'https://www.excelr.com/', img(src = 'https://excelrcom.b-cdn.net/assets/media/general/excelr-logo.png', title = "Company Website", height = "30px"),
                              style = "padding-top:10px; padding-bottom:10px;"),class = "dropdown")),

    dashboardSidebar(width = 400,
                     sidebarMenu(   id = "tabs",
                                    h4(menuItem("Project Background", tabName = "Project_Background", icon = icon("home"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Input Data", tabName = "Input_Data", icon = icon("refresh"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Global Structured Data Analysis", tabName = "Structured_Data", icon = icon("th"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Global Visitors'Chat Data Analysis", tabName = "Chat_Data", icon = icon("list-alt"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Visitors' Chat Duration Analysis", tabName = "Chat_Duration", icon = icon("dashboard"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Global Topic Mining", tabName = "Topic_Mining", icon = icon("life-ring"))),
                                    tags$hr(),
                                    tags$hr(),
                                    h4(menuItem("Selected Country's Analysis", tabName = "Selected_Country", icon = icon("th"))),
                                    tags$hr(),
                                    tags$hr()
                                ),
                     h5(textOutput("tab_selected")),tags$head(tags$style("#tab_selected{color: yellow;font-size: 20px;font-style: italic;}"
                     )
                     )
                    ),
    
    dashboardBody(
        
        tabItems(
            
          tabItem(
                    tabName = "Project_Background",
                          fluidRow( 
                                    h2(tags$ul("Business Objective:",style = "color: blue")),
                                    tags$hr(),
                                    h3(tags$ul("Topic Mining & Exploratory Analysis for improving the Resource Allocation, Content Modification & Service Improvement.",style = "color: blue")),
                                    tags$hr(),
                                    tags$hr(),
                                    h3(tags$ul("Following are the actionables from this project.",style = "color: black")),
                                    tags$hr(),
                                    h4(
                                    tags$ul(tags$li(tags$span("By geographies identify visitors' inflow on ExcelR Solutions Chat Bot"))),tags$br(),
                                    tags$ul(tags$li(tags$span("Identify most topics and subtopics discussed by visitors"))),tags$br(),
                                    tags$ul(tags$li(tags$span("Identity day wise inflow of visitors"))),tags$br(),
                                    tags$ul(tags$li(tags$span("Identify geography wise inflow of visitors "))),tags$br(),
                                    tags$ul(tags$li(tags$span("Identify inflow of visitors by time of the day"))),tags$br(),
                                    tags$ul(tags$li(tags$span("Which are widely used browsers and platforms for conversation by visitors?"))),tags$br(),
                                         ),
                                    
                                    htmlOutput("chatbotpicture")
                                      
                                      )
                      ),
          
          
          tabItem(
                    tabName = "Input_Data",
                        
                        tags$head(tags$style(".irs-grid-text {font-size: 12pt !important;")),    
                        fluidRow(
                                    options(shiny.maxRequestSize=100*1024^2),
                                    h3(fileInput("structured_data", "Upload Structured Data File",multiple = F,accept = c(".csv"),width = 700)),

                                    options(shiny.maxRequestSize=100*1024^2),
                                    h3(fileInput("chat_transcript", "Upload Chat Data File",multiple = F,accept = c(".csv"),width = 700)), 
                                    tags$hr(),

                                    h3(sliderInput("NumberOfWords", "Select Number of Words for Unigram and Bigram Barplot:",min = 1, max = 25, value = 10, width = '700px')),
                                    tags$hr(),

                                    h3(sliderInput("NumberOfTopics", "Select Number of Topics for Topic Mining:",min = 1, max = 20, value = 6, width = '700px')),
                                    tags$hr(),
                                    
                                    tags$head(tags$style(HTML('#choice{background-color:orange}'))),
                                    actionButton("choice", "Click to Incorporate List of Countries Below From Input Structured Data File",width = 700, style= 'font-size:120%'),
                                    tags$hr(),
                                    h3(selectInput("CountryName", "Select Country for Deep-dive Analysis", choices = NULL,width = 700 )),
                                    tableOutput("table_display")
                                    )
                    ),
            
            tabItem(
                     tabName = "Structured_Data",
                       fluidRow(
                                        
                                    h3("Top 10 Countries By Visitors"),
                                      plotOutput("Country_Name",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top 10 Regions By Visitors"),
                                    plotOutput("Region",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top 10 Cities By Visitors"),
                                    plotOutput("City",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top Platforms By Visitors"),
                                    plotOutput("Platform",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top 10 Browser By Visitors"),
                                    plotOutput("Browser",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Month Wise Visitors' Bar Plot"),
                                    plotOutput("Month",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Day Wise Visitors' Bar Plot"),
                                    plotOutput("Day",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Hour (UCT) Wise Visitors' Bar Plot"),
                                    plotOutput("Hour",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    )
                    ),
            
            tabItem(    
                      tabName = "Chat_Data",
                        fluidRow(
                                    h3("TFIDF Unigram and Bar Plot - Visitors' Chat"),
                                    splitLayout(
                                    plotOutput("TFIDF_Unigram",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    plotOutput("TFIDF_Barplot",click = "plot_click")%>% withSpinner(color="#0dc5c1")),

                                    h3("TFIDF Positive Words Unigram and Bar Plot - Visitors' Chat"),
                                    splitLayout(
                                    plotOutput("TFIDF_Positive_Words_Unigram",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    plotOutput("TFIDF_Positive_Words_Barplot",click = "plot_click")%>% withSpinner(color="#0dc5c1")),

                                    h3("TFIDF Negative Words Unigram and Bar Plot - Visitors' Chat"),
                                    splitLayout(
                                    plotOutput("TFIDF_Negative_Words_Unigram",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    plotOutput("TFIDF_Negative_Words_Barplot",click = "plot_click")%>% withSpinner(color="#0dc5c1")),
                                      
                                    h3("TFIDF Bigram - Visitors' Chat"),
                                    splitLayout(
                                    plotOutput("TFIDF_Bigram_WC",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    plotOutput("TFIDF_Bigram_Barplot",click = "plot_click")%>% withSpinner(color="#0dc5c1")),
                                      
                                )
                    ),
          
          tabItem(    
                    tabName = "Chat_Duration",
                        fluidRow(
                                    h3("Less Than A Min Chat Duration Analysis"),
                                    plotOutput("less_than_one",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                            
                                    h3("1 Min to 30 Mins Chat Duration Analysis"),
                                    plotOutput("one_to_thirty",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
            
                                    h3("30 Mins to 120 Mins Chat Duration Analysis"),
                                    plotOutput("thirty_to_onetwenty",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                          
                                    h3("120 Mins to 240 Mins Chat Duration Analysis"),
                                    plotOutput("onetwenty_to_twtofourty",click = "plot_click")%>% withSpinner(color="#0dc5c1"),

                                )
                  ),
          
            tabItem(
                      tabName = "Topic_Mining",
                          fluidRow(
                                    h3("Optimum Number of Topics for Uploaded Visitors' Chat Data by Coherence Score"),
                                    plotOutput("NumberofTopics",click = "plot_click",height = "850px")%>% withSpinner(color="#0dc5c1"),
                                    h3("Topic Mining"),
                                    plotOutput("TopicMining",click = "plot_click",height = "850px")%>% withSpinner(color="#0dc5c1"),
                                    h3("JSON"),
                                    visOutput(outputId="jsonoutput")%>% withSpinner(color="#0dc5c1")
                                      
                                    )
                    ),
          tabItem(
                    tabName = "Selected_Country",
                      fluidRow(
                                    h2(textOutput("country_name")),tags$head(tags$style("#country_name{color: blue;font-size: 40px;font-style: bold;}")),
                                    h3("Top 10 Regions By Visitors"),  
                                    plotOutput("CountryRegion",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top 10 Cities By Visitors"),  
                                    plotOutput("CountryCity",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top Platforms By Visitors"), 
                                    plotOutput("CountryPlatform",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Top 10 Browser By Visitors"), 
                                    plotOutput("CountryBrowser",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Month Wise Visitors' Bar Plot"),
                                    plotOutput("CountryMonth",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Day Wise Visitors' Bar Plot"), 
                                    plotOutput("CountryDay",click = "plot_click")%>% withSpinner(color="#0dc5c1"),
                                    
                                    h3("Hour (UCT) Wise Visitors' Bar Plot"), 
                                    plotOutput("CountryHour",click = "plot_click")%>% withSpinner(color="#0dc5c1"),

            )
          )
          
                )
            )
        )




server <- function(input, output,session)   {
  
  #memory.limit(size=40000)

    output$tab_selected <- renderText({ paste("Currently You Are In", input$tabs, " Tab" ) })
    
    data <- eventReactive(input$chat_transcript,{    rnorm(1:100000)  })
  
    src = "https://s3-eu-west-1.amazonaws.com/userlike-cdn-blog/do-i-need-a-chatbot/header-chat-box.png"
    
    output$chatbotpicture<-renderText({c('<img src="',src,'">')})
    
    output$country_name <- renderText(input$CountryName)

    re <- reactive({
                    inFile <- input$chat_transcript
                    if (is.null(inFile))
                      return(NULL)
                    file.rename(inFile$datapath,paste(inFile$datapath, ".csv", sep=""))
                    chat_transcript <- read.csv(paste(inFile$datapath, ".csv", sep=""), col.names = "Chat")
                    chat_transcript
                  })
    
    output$TFIDF_Unigram <- renderPlot({
                                          chat_transcript <- re()
                                          if (is.null(chat_transcript))
                                            return(NULL)
                                          source("Script_Chat_Data_Analysis.R",local = TRUE) 
                                          makewordc(tfidf) %>% title(sub = "UNIGRAM - Wordcloud using TFIDF")
                                      })
    
    output$TFIDF_Barplot <- renderPlot({
                                          chat_transcript <- re()
                                          if (is.null(chat_transcript))
                                            return(NULL)
                                          source("Script_Chat_Data_Analysis.R",local = TRUE); 
                                          words_bar_plot(tfidf) 
                                      })
    
    output$TFIDF_Positive_Words_Unigram <- renderPlot({
                                                        chat_transcript <- re()
                                                        if (is.null(chat_transcript))
                                                          return(NULL)
                                                        source("Script_Chat_Data_Analysis.R",local = TRUE);makeposwordc(tfidf) %>% title(sub = "UNIGRAM - Positive Wordcloud using TFIDF")
                                                      })
    
    output$TFIDF_Positive_Words_Barplot <- renderPlot({
                                                        chat_transcript <- re()
                                                        if (is.null(chat_transcript))
                                                          return(NULL)
                                                        source("Script_Chat_Data_Analysis.R",local = TRUE);pos_words_bar_plot(dtm1) 
 
                                                       })
    
    output$TFIDF_Negative_Words_Unigram <- renderPlot({
                                                        chat_transcript <- re()
                                                        if (is.null(chat_transcript))
                                                          return(NULL)
                                                        source("Script_Chat_Data_Analysis.R",local = TRUE);makenegwordc(tfidf) %>% title(sub = "UNIGRAM - Negative Wordcloud using TFIDF")
                                                      })
    
    output$TFIDF_Negative_Words_Barplot <- renderPlot({
                                                        chat_transcript <- re()
                                                        if (is.null(chat_transcript))
                                                          return(NULL)
                                                        source("Script_Chat_Data_Analysis.R",local = TRUE);neg_words_bar_plot(dtm1) 
                                                      })
    
    
    output$TFIDF_Bigram_WC <- renderPlot({
                                            chat_transcript <- re()
                                            if (is.null(chat_transcript))
                                                return(NULL)
                                            source("Script_Chat_Data_Analysis.R",local = TRUE) 
                                            minfreq_bigram<-2
                                            token_delim <- " \\t\\r\\n.!?,;\"()"
                                            bitoken <- NGramTokenizer(corpus_clean, Weka_control(min=2,max=2, delimiters = token_delim))
                                            two_word <- data.frame(table(bitoken))
                                            sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
                                            wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(3,1),colors = brewer.pal(8,"Dark2"),max.words=20) 
                                          })
    
    output$TFIDF_Bigram_Barplot <- renderPlot({
                                                chat_transcript <- re()
                                                if (is.null(chat_transcript))
                                                  return(NULL)
                                                source("Script_Chat_Data_Analysis.R",local = TRUE) 
                                                library(RWeka)
                                                minfreq_bigram<-2
                                                token_delim <- " \\t\\r\\n.!?,;\"()"
                                                bitoken <- NGramTokenizer(corpus_clean, Weka_control(min=2,max=2, delimiters = token_delim))
                                                two_word <- data.frame(table(bitoken))
                                                sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
                                                sort_two %>% slice(1:input$NumberOfWords) %>% 
                                                ggplot() + geom_bar(aes(x= reorder(bitoken,Freq), y = Freq), stat = "identity", fill = "#de5833") +theme(axis.text = element_text(size = 13, color = "blue"))+coord_flip() +labs(x = NULL)
                                              })   
    
    
    
    output$less_than_one <- renderPlot({
                                                chat_transcript <- re()
                                                if (is.null(chat_transcript))
                                                  return(NULL)
                                                source("Script_Chat_Duration.R",local = TRUE)
                                                ggplot(Non_Zero_Entries, aes(x=Duration))+geom_bar(stat="bin", fill="steelblue",breaks = seq(0, 1, by = 0.1))+theme_minimal()+scale_x_continuous("Less Than A Min Chat Duration", breaks=seq(0, 1, by = 0.1))+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                    })
    
  
    output$one_to_thirty <- renderPlot({
                                                chat_transcript <- re()
                                                if (is.null(chat_transcript))
                                                  return(NULL)
                                                source("Script_Chat_Duration.R",local = TRUE)
                                                ggplot(Non_Zero_Entries, aes(x=Duration))+  geom_bar(stat="bin", fill="steelblue",breaks = seq(1, 30, by = 1))+ theme_minimal()+scale_x_continuous("1 Min to 30 Mins Chat Duration", breaks=seq(1, 30, by = 1))+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                      })
    
    output$thirty_to_onetwenty <- renderPlot({
                                                chat_transcript <- re()
                                                if (is.null(chat_transcript))
                                                  return(NULL)
                                                source("Script_Chat_Duration.R",local = TRUE)
                                                ggplot(Non_Zero_Entries, aes(x=Duration))+geom_bar(stat="bin", fill="steelblue",breaks = seq(30, 120, by = 5))+theme_minimal()+scale_x_continuous("30 Mins to 120 Mins Chat Duration", breaks=seq(30, 120, by = 5))+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                              })
    
    output$onetwenty_to_twtofourty <- renderPlot({
                                                  chat_transcript <- re()
                                                  if (is.null(chat_transcript))
                                                    return(NULL)
                                                  source("Script_Chat_Duration.R",local = TRUE)
                                                  ggplot(Non_Zero_Entries, aes(x=Duration))+geom_bar(stat="bin", fill="steelblue",breaks = seq(120, 240, by = 24))+theme_minimal()+scale_x_continuous("120 Mins to 240 Mins Chat Duration", breaks=seq(120, 240, by = 24))+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                                })
    
    
    
    output$TopicMining <- renderPlot({
                                        chat_transcript <- re()
                                        if (is.null(chat_transcript))
                                            return(NULL)                                              
                                        source("Script_Topic_Mining.R",local = TRUE)
                                        ap_top_terms %>% mutate(term = reorder(term, beta)) %>% ggplot(aes(term, beta,fill = factor(topic))) + geom_col(show.legend = FALSE) + facet_wrap(~topic, scale = "free") +coord_flip()+theme(axis.text = element_text(size = 15, color = "black"))
                                    }) 

    
      output$jsonoutput <- renderVis({
                                        chat_transcript <- re()
                                        if (is.null(chat_transcript))
                                          return(NULL)                                              
                                        source("Script_JSON_File.R",local = TRUE)                                
                                        json_lda
                                      })

    output$NumberofTopics <- renderPlot({
                                        chat_transcript <- re()
                                        if (is.null(chat_transcript))
                                          return(NULL)                                              
                                        source("Script_Visitor_Chat_Coherence_Plot.R",local = TRUE)
                                        ggplot(coherence_mat, aes(x = k, y = coherence)) +  geom_point() +  geom_line(group = 1)+  ggtitle("Best Topic by Coherence Score") + theme_minimal() +  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")
                                        
                                      }) 
    
    
    info <- eventReactive(input$choice, {
                                          inFile3 <- input$structured_data
                                          req(inFile3)
                                          file.rename(inFile3$datapath,paste(inFile3$datapath, ".csv", sep=""))
                                          structured_data <- read.csv(paste(inFile3$datapath, ".csv", sep=""))
                                          CountryName <- unique(structured_data$Country_Name)
                                          updateSelectInput(session, "CountryName","Select Country", choices = CountryName)
                                          CountryName 
                                        })
    
    output$table_display <- renderTable({
                                          CountryName <- info()
                                          colnames(CountryName)
                                        })
    
    re2 <- reactive({
                      inFile2 <- input$structured_data
                      if (is.null(inFile2))
                        return(NULL)
                      file.rename(inFile2$datapath,paste(inFile2$datapath, ".csv", sep=""))
                      structured_data <- read.csv(paste(inFile2$datapath, ".csv", sep=""))
                      structured_data
                    })    
    

    output$Country_Name <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                            return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(Country_Name,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Country_Name") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                      
                                        
                                      })
    output$Region <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(Region,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Region Name") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        })   
    output$City <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(City,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("City Name") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                        
                                      })    
    output$Platform <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(Platform,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Platform") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                        
                                      }) 
    
    output$Browser <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(Browser,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Browser") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                      
                                    }) 
    
    output$Month <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(structured_data,aes(x=Month,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.2)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                        
                                  }) 
    
    output$Day <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(structured_data,aes(x=Day,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.5)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                        
                                  }) 
    output$Hour <- renderPlot({
                                        structured_data <- re2()
                                        if (is.null(structured_data))
                                          return(NULL)
                                        source("Script_Structured_Data_Analysis.R",local = TRUE);
                                        ggplot(structured_data,aes(x=Hour,fill=Hour))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.7)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                        
                                  })   
    
    output$CountryRegion <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Region,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Region Name") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        })   
    output$CountryCity <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(City,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("City Name") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        })    
    output$CountryPlatform <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Platform,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Platform") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        }) 
    
    output$CountryBrowser <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Browser,aes(x=reorder(Var1,Freq), y=Freq)) +  geom_bar(stat="identity", fill="#3052DC", alpha=.6, width=.4) +  coord_flip() +  xlab("Browser") +ylab("Count")+theme_minimal() +theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        }) 
    
    output$CountryMonth <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Country,aes(x=Month,fill=Month))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.2)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        }) 
    
    output$CountryDay <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Country,aes(x=Day,fill=Day))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.5)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        }) 
    output$CountryHour <- renderPlot({
                                          structured_data <- re2()
                                          if (is.null(structured_data))
                                            return(NULL)
                                          source("Script_Country.R",local = TRUE);
                                          ggplot(Country,aes(x=Hour,fill=Hour))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+geom_bar(width = 0.7)+theme_minimal()+theme(axis.text = element_text(size = 13, color = "blue"),axis.title=element_text(size=14,face="bold"))+theme(plot.margin = unit(c(1, 6, 1, 6), "cm"))
                                          
                                        })
      
    }

shinyApp(ui, server)                

