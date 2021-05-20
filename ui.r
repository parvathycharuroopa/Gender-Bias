library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "Gender Bias At Workplace", titleWidth = 250),
  dashboardSidebar(width =250,sidebarMenu( 
    fileInput("upload","Upload the Dataset"),
    menuItem("Dataset",tabName = "dataset", icon = icon("book")),
    menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
    menuItem("Test Analysis", tabName = "cstest", icon = icon("table")),
    menuItem("Sentimental Analysis", tabName = "senti", icon = icon("file-medical-alt")),
    menuItem("Emotion Plot",tabName = "emplot", icon = icon("angle-double-right"))
    #menuItem("Conclusion",tabName = "Concl", icon = icon("angle-double-right"))
    
            # menuSubItem("Text emotions", tabName = "senti_score"),
             #menuSubItem("charts", tabName = "charts"))
  )),
  dashboardBody(
    tabItems(
      tabItem(
        h2(" Gender Bias Dataset"),
        tabName = "dataset",
        mainPanel(width = 20,
                  tabsetPanel(
                    type = "tabs",
                    tabPanel("Dataset",DT::dataTableOutput("data1")),
                    tabPanel("Summary",verbatimTextOutput("summary"))
                    
                  )
        )
        
      ),
      tabItem(
        h2("Plots"),
        tabName = "plots",
        
        mainPanel(
          width = 20,
          type="tabs",
          tabPanel("barplot 1",plotOutput("plot")),
          tabPanel("barplot 2",plotOutput("plot2")),
          tabPanel("barplot 3",plotOutput("plot3")),
          tabPanel("barplot 4",plotOutput("plot4")),
          tabPanel("barplot 5",plotOutput("plot5")),
          tabPanel("barplot 6",plotOutput("plot6")),
          tabPanel("barplot 7",plotOutput("plot7")),
          tabPanel("barplot 8",plotOutput("plot8")),
          tabPanel("barplot 9",plotOutput("plot9")),
          tabPanel("barplot 10",plotOutput("plot10")),
          tabPanel("barplot 11",plotOutput("plot11")),
          tabPanel("barplot 12",plotOutput("plot12")),
          tabPanel("barplot 13",plotOutput("plot13")),
          tabPanel("barplot 14",plotOutput("plot14"))
        )
      ),
      
      tabItem(
        h2("Chi-Square Table"),
        tabName = "cstest",
        
        mainPanel(
          width = 20,
          type= "tabs",
          tabItem(tabName="chi_squaretest", tableOutput("tb1"))
          )
   
       ),
      
       tabItem(
        h2("Emotion Table"),
        tabName = "senti",
        
        mainPanel(
          width = 20,
          type= "tabs",
          tabItem(tabName="senti_score",tableOutput("emotion"))
         
          
        )
        
      ),
      
      tabItem(
        #h2("Emotion Plot"),
        tabName = "emplot",
        
        mainPanel(
          width = 20,
          type= "tabs",
          tabItem(tabName="em_score1",plotOutput("em1")),
          tabItem(tabName="em_score2",plotOutput("em2")),
          tabItem(tabName="em_score3",plotOutput("em3"))
         
          
        )
        
      ),
     
      tabItem(
        h2("Conclusion"),
        #tabName = "emplot",
        
        mainPanel(
          width = 20,
          type= "tabs",
          tabPanel("conclusion",textOutput("con"))
         
          
          
        )
        
      )
      
       
    )
  )
)