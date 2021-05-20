library(shiny)
library(shinydashboard)
library(dplyr)
library(tm)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(syuzhet)
library(sentimentr)


server <- (function(input,output){
 
    dtset <- reactive({
      if(is.null(input$upload)){
        return(NULL)
      }
      read.csv(input$upload$datapath,header = TRUE,sep=",")
    })
    
    # GENDER BIAS DATASET 
    output$data1<-DT::renderDataTable({
      DT::datatable(dtset(),
                    options=list(scrollX=TRUE)
      )
    })
    
    output$summary<-renderPrint({
      summary(dtset())
    })
    
   
    output$plot <- renderPlot({
      
      gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
        #plot1
    
   
 plot1<- ggplot(gender_data,aes(age_group))+
   geom_bar(mapping = aes(fill=Gender),position='dodge')+
   geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
   scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
   labs(x='age group',y="Number of employees")+
   theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
         legend.text = element_text(size= 15),
         axis.title.x = element_text(size=15),
         axis.title.y = element_text(size=15),
         legend.title = element_text(size = 15))
 
plot1    
  })
  
  output$plot2 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot2
    m = nrow(gender_data)
    
plot2<-ggplot(gender_data,aes(x="",y=m,fill=Gender))+
  geom_bar(stat="identity")+
  coord_polar("y",start=0)+
  theme_void()+
  scale_fill_manual(values = c("aquamarine4", "dark sea green"))+
  theme(legend.title = element_text(size = 15), legend.text = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))
    
plot2   
    
  })
  
  output$plot3 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot3
    # SHOWS HOW MUCH EMPLOYEES ARE SACRIFICING THEIR PERSONAL HAPPINESS IN TERMS OF COUNT OF EMPLOYEES
    
    plot3<-ggplot(gender_data,aes(sac_ph))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='sacrificing personal happiness',y="Number of employees")+ 
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    plot3   
    
  })
  
  output$plot4 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot4
    # SHOWS HOW MUCH EMPLOYEES ARE SACRIFICING THEIR PERSONAL HAPPINESS IN TERMS OF RATIO OF EMPLOYEES
    
    
   
    m = nrow(gender_data)  
    
    plot4<- ggplot(gender_data,aes(fill=Gender,x=sac_ph,y=m))+
      geom_bar(position='fill',stat = "identity")+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='sacrificing personal happiness',y="Ratio of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    plot4   
    
  })
  
  output$plot5 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot5
    #  experience by gender
    
    plot5<-ggplot(gender_data,aes(exp))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='experience',y="Number of employees")+ 
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.text = element_text(size= 15),
            legend.title = element_text(size = 15))
    
    plot5  
    
  })
  
  output$plot6 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot6
    # EMPLOYEES VIEW ON WHETHER THEY HAD TO PUT AN EXTRA EFFORT
    
   
    plot6<-ggplot(gender_data,aes(extra_effort))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='extra effort',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    
    plot6  
    
  })
  
  output$plot7 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot7
    # EMPLOYEES VIEW ON WHETHER THEY HAD TO PUT AN EXTRA EFFORT :RATIO
    
    m = nrow(gender_data)  
    
    plot7<-ggplot(gender_data,aes(fill=Gender,x=extra_effort,y=m))+
      geom_bar(position='fill',stat = "identity")+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='extra effort',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    plot7  
    
  }) 
  
  output$plot8 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot8
    # job satisfaction by gender
    
    
    plot8<-ggplot(gender_data,aes(job_satisfaction))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='job satisfaction',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    plot8  
    
  }) 
  
  output$plot9 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot9
    # job satisfaction by age group
    
    
    
    plot9<-ggplot(gender_data,aes(job_satisfaction))+
      geom_bar(mapping = aes(fill=age_group),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green","yellow3","dark sea green4","blue4"))+
      labs(x='job satisfaction',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    
    plot9 
    
  }) 
  
  output$plot10 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot10
    # job satisfaction by age group :RATIO  
    m = nrow(gender_data)  
    
    plot10<-ggplot(gender_data,aes(fill=age_group,x=job_satisfaction,y=m))+
      geom_bar(position='fill',stat = "identity")+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green","yellow3","dark sea green4","blue4"))+
      labs(x='job satisfaction',y="ratio of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    
    plot10 
    
  }) 
  
  output$plot11 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot11
    # negotiating promotion by gender
    
    
    plot11<-ggplot(gender_data,aes(neg_pro))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='negotiating a promotion',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
      
    
    
    plot11 
    
  })  
 
  output$plot12 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot12
    # career development oppurtunities by gender
    
    
    plot12<- ggplot(gender_data,aes(career_dev))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='career development oppurtunities',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    
    plot12 
    
  })   
  
  output$plot13 <- renderPlot({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    
    #plot13
    #family dissatisfaction by gender
    
    
    plot13<- ggplot(gender_data,aes(fam_dis))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='family dissatisfaction',y="Number of employees")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    
    plot13 
    
  })   
  
  
  
  
  output$plot14 <- renderPlot({
     
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",")
    
    #PLOT 14
    #work to home by gender
    
    plot14 <- ggplot(gender_data,aes(work_to_home))+
      geom_bar(mapping = aes(fill=Gender),position='dodge')+
      geom_text(stat='count',aes(label=after_stat(count)),position = position_dodge(width = 0.8),,vjust=0)+
      scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      labs(x='work to home',y="Number of employees")+
      #ggtitle("Work to home by gender")+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            
            legend.text = element_text(size= 15),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            legend.title = element_text(size = 15))
    
    plot14
    
  })
  
 
  
  output$tb1<- renderTable({
    
    gender_data <- read.csv("final_dataset.csv", header = TRUE, sep = ",") 
    
    chisquaretable<- data.frame(  
      "factors" = c( "jobsatisfaction", "fam_discrimination" , "work adaptility" , "work to home", "treating low", "career development",
                     "Parent difficulty", "income", "experience", "negitiate promotion", "office stay", "unfair pay", "sacrificing happiness", 
                     "discrimination",  "extra effort", "work place adaptibility", " superior relationship"),  
      "Pvalue" = c( "0.427", "0.2304" , "0.8564", "0.04242", "0.538", "0.008073", "0.1471",
                    "0.062227", "0.06059", "0.1461", "0.2482", "0.3313", "0.9064", "1", "0.3725", "0.3145", "0.4305"), 
      "Independent with Gender" = c("yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes",
                                    "yes", "yes", "yes")
    )  
    
    head(chisquaretable,17) 
    
  } )
  
  
  #reading file
  text_df <- read.csv("sentiment.csv", stringsAsFactors = FALSE)
  
  #store the text in review vector in character format
  review <- as.character(text_df$Sentence)
  
  #obtain sentiment scores
  get_nrc_sentiment('happy')
  get_nrc_sentiment('excitement')
  
  #run it in entire dataset and store in s
  s <- get_nrc_sentiment(review)
  
  #combine with sentence column from original dataset
  review_sentiment <- cbind(text_df$Sentence,s) 
  
  output$emotion <- renderTable({

     head(review_sentiment, 30)
    
    
  })
  
 
  
  output$em1<- renderPlot({
    
  text_df <- read.csv("sentiment.csv", stringsAsFactors = FALSE) 
  
  emplot<- barplot(colSums(s),col=rainbow(10),ylab='Count', main ='Emotion Plot')
  
  emplot
  
  
  } )
  
  output$em2<- renderPlot({
    
    df <- read.csv("sentiment.csv", stringsAsFactors = FALSE)
    
    dt1 <- removePunctuation(df$Sentence)
    dt1
    
    s<- sentiment(dt1)
    
    df_senti <- cbind(df,s)
    
    #density graph
    df_senti %>% 
      ggplot() + geom_density(aes(sentiment))
    
    
  } )
  
  output$em3<- renderPlot({
    
    df <- read.csv("sentiment.csv", stringsAsFactors = FALSE)
    
    dt1 <- removePunctuation(df$Sentence)
    dt1
    
    s<- sentiment(dt1)
    
    df_senti <- cbind(df,s)
    
    df_senti %>%
      mutate(polarity_level = ifelse(sentiment>0,"Positive","Negative")) %>%
      count(Gender,polarity_level) %>%
      
      ggplot() + geom_col(aes(x=Gender,y = n, fill = polarity_level)) + 
      theme_minimal()+ scale_fill_manual(values = c( "aquamarine4", "dark sea green"))+
      theme(panel.border =element_rect(colour="black", fill=NA), axis.text = element_text(size= 15),
            legend.text = element_text(size= 12),
            legend.title = element_text(size = 12))
    
    
  } )
  

  
  
   
    
  
  
  
})

