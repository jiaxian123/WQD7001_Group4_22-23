#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(xgboost)
library(DALEX)
library(caret)
myData <- read.csv("./new_dataset-3.csv")
bpModel <- readRDS("./model_xgb1.rds")
final_model <- bpModel$finalModel
# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Body Performance Class Prediction",
                  titleWidth = "calc(100% - 44px)"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("home", lib = "font-awesome")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("gauge", lib = "font-awesome"), expandedName = "DASHBOARD",
             menuSubItem("Class Performance Analysis", tabName = "subitem1"),
             menuSubItem("Relationship Analysis", tabName = "subitem2")
    ),
    menuItem("Dataset Review", tabName = "dtbr", icon = icon("table", lib = "font-awesome")),
    menuItem("Prediction", tabName = "pre", icon = icon("robot", lib = "font-awesome")),
    menuItem("About Us", tabName = "abtus", icon = icon("building", lib = "font-awesome"))
  )
  ),
  dashboardBody(
    includeCSS("www/custom.css"),
    tabItems(
      tabItem(tabName = "dtbr",
              fluidRow(
                DT::dataTableOutput("mytable")
              )
              
      ),
      
      
      tabItem(tabName = "overview",
              fluidRow(
                img(src='IMG_4177.PNG',height="105%",width="100%")
                
              ),
              fluidRow(
                
              )
              
      ),
      
      tabItem(tabName = "abtus",
              
              fluidRow(
                tags$h2("About Our Project"),
                tags$div(class="landing-wrapper",
                         img(src="background.jpg", height="100%",width="100%"),
                         tags$div(class="landing-block foreground-content",
                                  tags$div(class="foreground-text",
                                           tags$h1("Welcome to our project"),
                                           tags$p("We are Master of Data Science students in University Malaya. This project is focus on many people lack knowledge, 
                                         skills, equipment, even time and resources to do body checks or related tests. Therefore, we are managing to develop 
                                         a model, which will save manpower, material, financial resources and make it easier for people to understand their body performances. "
                                           ),
                                           tags$p("Credit by: Yap Jia Xian(S2150857),	Li Junchi(S2163474), 	Sun Hanxiao(S2163311),	Wang Ruobing(S2163476), Wong Kian Wai(S2180506)")
                                  )
                         )
                )
              ),
      ),
      
      tabItem(tabName = "subitem1",
              
              fluidRow(
                
                box(width=12,
                    box(width = 4,
                        titlePanel("Class Performance Analysis"),
                        
                        selectInput("classInput", "Select the class", c("Class A","Class B","Class C","Class D")),
                        
                    ),
                    box(width = 4,
                        titlePanel("Slider Control for Age"),
                        sliderInput("bin1", 
                                    label = "Select number of histogram bins", 
                                    min=2, max=20, value= c(10)),
                        
                    ),
                    box(width = 4,
                        titlePanel("Slider Control for Body Fat in Percentage"),
                        sliderInput("bin2", 
                                    label = "Select number of histogram bins", 
                                    min=2, max=20, value= c(5)),
                        
                    )
                    
                )
              ),
              fluidRow(
                
                box(width=12,
                    box(title="Gender",width=4,plotOutput("plotGen", height = 250)),
                    box(title="Age",width=4,plotOutput("plotAge", height = 250)),
                    box(title="Body Fat in %",width=4,plotOutput("plotFat", height = 250))
                )
              ),
              
              fluidRow(
                
                box(width=12,
                    box(title="Height and Weight",width=3,plotOutput("plotHeiwei", height = 250)),
                    box(title="Diastolic and Systolic",width=3,plotOutput("plotSysDia", height = 250)),
                    box(
                      selectInput("sportaInput", "Select the first sport", c("situp","gripForce","sitbend","broadjump")),
                      selectInput("sportbInput", "Select the second sport", c("broadjump","sitbend","gripForce","situp")),
                      width=2
                    ),
                    box(title=textOutput("text"),width=4,plotOutput("plotSpr", height = 250)),
                )
              ),
              
              fluidRow(
                
                box(width=12,
                    box(title="Age Category",width=4,plotOutput("plotAgeCat", height = 250)),
                    box(title="Systolic Blood Pressure Category",width=4,plotOutput("plotSysCat", height = 250)),
                    box(title="Diastolic Blood Pressure Category",width=4,plotOutput("plotDisCat", height = 250)),
                )
              )
      ),
      tabItem(tabName = "pre",
              fluidRow(
                box(width=4,
                    numericInput(inputId='age', label='Age', value = 37,min = NA, max = 100, step = NA,width = NULL),
                    checkboxGroupInput(inputId='gender', label='Gender', c('male','female'), selected = NULL, inline = FALSE,width = NULL),
                    numericInput(inputId='height', label='Height', value = 168,min = NA, max = NA, step = NA,width = NULL),
                    numericInput(inputId='weight', label='Weight', value = 67, min = NA, max = NA, step = NA,width = NULL),
                    
                    numericInput(inputId='bodyfat_per', label='Body Fat in %', value = 22 ,min = 3.5, max = 45, step = NA,width = NULL),
                    numericInput(inputId='diastolic', label='Diastolic Blood Pressure', value = 79,min = 45, max = 110, step = NA,width = NULL),
                    numericInput(inputId='systolic', label='Systolic Blood Pressure', value = 130,min = 85, max = 190, step = NA,width = NULL),
                    
                    numericInput(inputId='gripforce', label='GripForce', value = 37,min = NA, max = NA, step = NA,width = NULL),
                    numericInput(inputId='sitbend', label='Sit Bend', value = 16,min = NA, max = NA, step = NA,width = NULL),
                    numericInput(inputId='situp', label='Sit up', value = 41,min = NA, max = NA, step = NA,width = NULL),
                    numericInput(inputId='broadjump', label='broardjump', value = 192,min = NA, max = NA, step = NA,width = NULL),
                    actionButton("button", "Apply Changes")
                ),
                box(width = 8,
                    fluidRow(
                      imageOutput("Pred")
                    )
                  ),
                
                
                
              )
              
      ),
      
      tabItem(tabName = "subitem2",
              fluidRow(
                h3("Correlation Matrix"),
                img(src="corr.png", height="450px",width="70%")
                ),
              
              fluidRow(
                h3("How much does increasing age affect the efficiency in exercises?"),
                img(src="q1_first.png", height="350px",width="33%"),
                img(src="q1_second.png", height="350px",width="33%"),
                img(src="q1_third.png", height="350px",width="33%")
                
              ),
              fluidRow(
                h3("How much does efficiency in exercises affect the class?"),
                img(src="q2_first.png", height="350px",width="45%"),
                img(src="q2_second.png", height="350px",width="45%")
              ),
              fluidRow(
                h3("How much does age affect to class?"),
                img(src="q3_first.png", height="350px",width="70%")
              ),
              fluidRow(
                h3("How much does blood pressure values affect to class? and diferrences between ages?"),
                img(src="q4_first.png", height="350px",width="45%"),
                img(src="q4_second.png", height="350px",width="45%"),
                img(src="q4_third.png", height="350px",width="45%"),
                img(src="q4_fourth.png", height="350px",width="45%")
              ),
              fluidRow(
                h3("How much does Fat% value affect to blood pressure? and diferrences between classes"),
                img(src="q5_first.png", height="350px",width="45%"),
                img(src="q5_second.png", height="350px",width="45%")
              ),
              fluidRow(
                h3("How much does Age category affect to Grip Force?"),
                img(src="q6_first.png", height="350px",width="70%")
              )
      )
      
      
      
    )
  )
)

server <- function(input, output) { 
  output$mytable = DT::renderDataTable({myData})
  
  
  output$plotGen <- renderPlot({
    req(input$classInput)
    myData$gender[myData$gender == 1] <- 'Male'
    myData$gender[myData$gender == 2] <- 'Female'
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      ggplot(myData, aes(x=gender,fill=gender)) + 
        geom_bar()
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      ggplot(myData, aes(x=gender,fill=gender)) + 
        geom_bar()
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      ggplot(myData, aes(x=gender,fill=gender)) + 
        geom_bar()
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      ggplot(myData, aes(x=gender,fill=gender)) + 
        geom_bar()
    }
    
  })
  
  output$plotAge <- renderPlot({
    req(input$classInput,input$bin1)
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      ggplot(myData, aes(x = age)) + 
        geom_histogram(colour = 4, fill = "#6accbc",bins = input$bin1)
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      ggplot(myData, aes(x = age)) + 
        geom_histogram(colour = 4, fill = "#6accbc",bins = input$bin1)
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      ggplot(myData, aes(x = age)) + 
        geom_histogram(colour = 4, fill = "#6accbc", bins = input$bin1)
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      ggplot(myData, aes(x = age)) + 
        geom_histogram(colour = 4, fill = "#6accbc",bins = input$bin1)
    }
  })
  
  output$plotFat <- renderPlot({
    req(input$classInput,input$bin2)
    
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      ggplot(myData, aes(x = bodyfat_per)) + 
        
        geom_histogram(colour = 4, fill = "#afb83b",bins = input$bin2)
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      ggplot(myData, aes(x = bodyfat_per)) + 
        geom_histogram(colour = 4, fill = "#afb83b",bins = input$bin2)
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      ggplot(myData, aes(x = bodyfat_per)) + 
        geom_histogram(colour = 4, fill = "#afb83b", bins = input$bin2)
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      ggplot(myData, aes(x = bodyfat_per)) + 
        geom_histogram(colour = 4, fill = "#afb83b",bins = input$bin2)
    }
  })
  
  output$plotHeiwei <- renderPlot({
    req(input$classInput)
    myData$gender[myData$gender == 1] <- 'Male'
    myData$gender[myData$gender == 2] <- 'Female'
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      ggplot(myData, aes(height_cm, weight_kg, fill= gender)) + 
        geom_point(shape=23,alpha = 0.7)+theme_classic()
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      ggplot(myData, aes(height_cm, weight_kg, fill= gender)) + 
        geom_point(shape=23,alpha = 0.7)+theme_classic()
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      ggplot(myData, aes(height_cm, weight_kg, fill= gender)) + 
        geom_point(shape=23,alpha = 0.7)+theme_classic()
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      ggplot(myData, aes(height_cm, weight_kg, fill= gender)) + 
        geom_point(shape=23,alpha = 0.7)+theme_classic()
    }
  })
  
  output$plotSysDia <- renderPlot({
    req(input$classInput)
    myData$gender[myData$gender == 1] <- 'Male'
    myData$gender[myData$gender == 2] <- 'Female'
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      ggplot(myData, aes(diastolic, systolic, alpha = bodyfat_per)) + 
        geom_point(shape=23,color=2)+theme_classic()
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      ggplot(myData, aes(diastolic, systolic, alpha = bodyfat_per)) + 
        geom_point(shape=23,color=2)+theme_classic()
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      ggplot(myData, aes(diastolic, systolic, alpha = bodyfat_per)) + 
        geom_point(shape=23,color=2)+theme_classic()
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      ggplot(myData, aes(diastolic, systolic, alpha = bodyfat_per)) + 
        geom_point(shape=23,color=2)+theme_classic()
    }
  })
  
  output$plotSpr <- renderPlot({
    req(input$sportaInput)
    req(input$sportbInput)
    req(input$classInput)
    
    
    
    myData$gender[myData$gender == 1] <- 'Male'
    myData$gender[myData$gender == 2] <- 'Female'
    
    
    
    if (input$classInput == 'Class A'){
      
      
      
      if(input$sportaInput == 'situp' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(situp, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(situp, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(situp, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(gripForce, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(gripForce, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(gripForce, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(sitbend, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(sitbend, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(sitbend, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(broadjump, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(broadjump, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 1)
        ggplot(myData, aes(broadjump, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }
      
      
    }else if (input$classInput == 'Class B'){
      if(input$sportaInput == 'situp' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(situp, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(situp, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(situp, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(gripForce, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(gripForce, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(gripForce, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(sitbend, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(sitbend, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(sitbend, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(broadjump, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(broadjump, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 2)
        ggplot(myData, aes(broadjump, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }
      
    }else if (input$classInput == 'Class C'){
      if(input$sportaInput == 'situp' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(situp, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(situp, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(situp, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(gripForce, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(gripForce, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(gripForce, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(sitbend, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(sitbend, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(sitbend, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(broadjump, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(broadjump, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 3)
        ggplot(myData, aes(broadjump, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }
    }else if (input$classInput == 'Class D'){
      if(input$sportaInput == 'situp' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(situp, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(situp, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'situp' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(situp, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(gripForce, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(gripForce, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'gripForce' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(gripForce, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(sitbend, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(sitbend, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'sitbend' & input$sportbInput == 'broadjump'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(sitbend, broadjump, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
        
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'situp'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(broadjump, situp, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'gripForce'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(broadjump, gripForce, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }else if(input$sportaInput == 'broadjump' & input$sportbInput == 'sitbend'){
        myData <- subset(myData, class == 4)
        ggplot(myData, aes(broadjump, sitbend, fill= bodyfat_per)) +
          geom_point(shape=23)+theme_classic()
      }
    }
    
  })
  
  output$text <- renderText({ 
    req(input$sportaInput)
    req(input$sportbInput)
    
    sprintf("%s and %s", input$sportaInput,input$sportbInput)
  })
  
  output$plotAgeCat <- renderPlot({
    req(input$classInput)
    
    myData$gender[myData$gender == 1] <- 'Male'
    myData$gender[myData$gender == 2] <- 'Female'
    
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      myData <- myData %>% 
        group_by(age_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$age_cat[myData$age_cat == 1] <- 'Younger'
      myData$age_cat[myData$age_cat == 2] <- 'Older'
      cols <- c("Younger" = "#D55E00", "Older" = "#CC79A7")
      ggplot(myData, aes(x = "", y = labels, fill = age_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
      
      
      
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      myData <- myData %>% 
        group_by(age_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$age_cat[myData$age_cat == 1] <- 'Younger'
      myData$age_cat[myData$age_cat == 2] <- 'Older'
      cols <- c("Younger" = "#D55E00", "Older" = "#CC79A7")
      ggplot(myData, aes(x = "", y = labels, fill = age_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      myData <- myData %>% 
        group_by(age_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$age_cat[myData$age_cat == 1] <- 'Younger'
      myData$age_cat[myData$age_cat == 2] <- 'Older'
      cols <- c("Younger" = "#D55E00", "Older" = "#CC79A7")
      ggplot(myData, aes(x = "", y = labels, fill = age_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      myData <- myData %>% 
        group_by(age_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$age_cat[myData$age_cat == 1] <- 'Younger'
      myData$age_cat[myData$age_cat == 2] <- 'Older'
      cols <- c("Younger" = "#D55E00", "Older" = "#CC79A7")
      ggplot(myData, aes(x = "", y = labels, fill = age_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
    }
  })
  
  output$plotSysCat <- renderPlot({
    req(input$classInput)
    
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      myData <- myData %>% 
        group_by(sys_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$sys_cat[myData$sys_cat == 1] <- 'Category 1'
      myData$sys_cat[myData$sys_cat == 2] <- 'Category 2'
      myData$sys_cat[myData$sys_cat == 3] <- 'Category 3'
      myData$sys_cat[myData$sys_cat == 4] <- 'Category 4'
      cols <- c("Category 1" = "#E69F00", "Category 2" = "#56B4E9", "Category 3" = "#009E73", "Category 4" = "#F0E442")
      ggplot(myData, aes(x = "", y = labels, fill = sys_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
      
      
      
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      myData <- myData %>% 
        group_by(sys_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$sys_cat[myData$sys_cat == 1] <- 'Category 1'
      myData$sys_cat[myData$sys_cat == 2] <- 'Category 2'
      myData$sys_cat[myData$sys_cat == 3] <- 'Category 3'
      myData$sys_cat[myData$sys_cat == 4] <- 'Category 4'
      cols <- c("Category 1" = "#E69F00", "Category 2" = "#56B4E9", "Category 3" = "#009E73", "Category 4" = "#F0E442")
      ggplot(myData, aes(x = "", y = labels, fill = sys_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      myData <- myData %>% 
        group_by(sys_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$sys_cat[myData$sys_cat == 1] <- 'Category 1'
      myData$sys_cat[myData$sys_cat == 2] <- 'Category 2'
      myData$sys_cat[myData$sys_cat == 3] <- 'Category 3'
      myData$sys_cat[myData$sys_cat == 4] <- 'Category 4'
      cols <- c("Category 1" = "#E69F00", "Category 2" = "#56B4E9", "Category 3" = "#009E73", "Category 4" = "#F0E442")
      ggplot(myData, aes(x = "", y = labels, fill = sys_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      myData <- myData %>% 
        group_by(sys_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$sys_cat[myData$sys_cat == 1] <- 'Category 1'
      myData$sys_cat[myData$sys_cat == 2] <- 'Category 2'
      myData$sys_cat[myData$sys_cat == 3] <- 'Category 3'
      myData$sys_cat[myData$sys_cat == 4] <- 'Category 4'
      cols <- c("Category 1" = "#E69F00", "Category 2" = "#56B4E9", "Category 3" = "#009E73", "Category 4" = "#F0E442")
      ggplot(myData, aes(x = "", y = labels, fill = sys_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
    }
  })
  
  output$plotDisCat <- renderPlot({
    req(input$classInput)
    
    if (input$classInput == 'Class A'){
      myData <- subset(myData, class == 1)
      myData <- myData %>% 
        group_by(dis_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$dis_cat[myData$dis_cat == 1] <- 'Category 1'
      myData$dis_cat[myData$dis_cat == 2] <- 'Category 2'
      myData$dis_cat[myData$dis_cat == 3] <- 'Category 3'
      cols <- c("Category 1" = "#f1b6da", "Category 2" = "#b2abd2", "Category 3" = "#f4a582")
      ggplot(myData, aes(x = "", y = labels, fill = dis_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
      
      
      
    }else if (input$classInput == 'Class B'){
      myData <- subset(myData, class == 2)
      myData <- myData %>% 
        group_by(dis_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$dis_cat[myData$dis_cat == 1] <- 'Category 1'
      myData$dis_cat[myData$dis_cat == 2] <- 'Category 2'
      myData$dis_cat[myData$dis_cat == 3] <- 'Category 3'
      cols <- c("Category 1" = "#f1b6da", "Category 2" = "#b2abd2", "Category 3" = "#f4a582")
      ggplot(myData, aes(x = "", y = labels, fill = dis_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
    }else if (input$classInput == 'Class C'){
      myData <- subset(myData, class == 3)
      myData <- myData %>% 
        group_by(dis_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$dis_cat[myData$dis_cat == 1] <- 'Category 1'
      myData$dis_cat[myData$dis_cat == 2] <- 'Category 2'
      myData$dis_cat[myData$dis_cat == 3] <- 'Category 3'
      cols <- c("Category 1" = "#f1b6da", "Category 2" = "#b2abd2", "Category 3" = "#f4a582")
      ggplot(myData, aes(x = "", y = labels, fill = dis_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
      
    }else if (input$classInput == 'Class D'){
      myData <- subset(myData, class == 4)
      myData <- myData %>% 
        group_by(dis_cat) %>% # Variable to be transformed
        count() %>% 
        ungroup() %>% 
        mutate(perc = `n` / sum(`n`)) %>% 
        arrange(perc) %>%
        mutate(labels = scales::percent(perc))
      
      myData$dis_cat[myData$dis_cat == 1] <- 'Category 1'
      myData$dis_cat[myData$dis_cat == 2] <- 'Category 2'
      myData$dis_cat[myData$dis_cat == 3] <- 'Category 3'
      cols <- c("Category 1" = "#f1b6da", "Category 2" = "#b2abd2", "Category 3" = "#f4a582")
      ggplot(myData, aes(x = "", y = labels, fill = dis_cat)) +
        geom_col(color = "black") +
        geom_label(aes(label = labels),
                   position = position_stack(vjust = 0.5),
                   show.legend = FALSE) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = cols)
    }
  })
  
  # data <- reactive({
  #   req(input$gender)
  #   if (input$gender == 'male'){
  #     data.frame(age=input$age,
  #                gender=1,
  #                height_cm=input$height,
  #                weight_kg=input$weight,
  #                bodyfat_per=input$bodyfat_per,
  #                diastolic=input$diastolic,
  #                systolic=input$systolic,
  #                gripForce=input$gripforce,
  #                sitbend=input$sitbend,
  #                broadjump=input$broadjump,
  #                situp=input$situp)
  #   } else if(input$gender == 'female'){
  #     data.frame(age=input$age,
  #                gender=2,
  #                height_cm=input$height,
  #                weight_kg=input$weight,
  #                bodyfat_per=input$bodyfat_per,
  #                diastolic=input$diastolic,
  #                systolic=input$systolic,
  #                gripForce=input$gripforce,
  #                sitbend=input$sitbend,
  #                broadjump=input$broadjump,
  #                situp=input$situp)
  #   }
  #   
  # })
  
  pred <- function(){
    
    predict(bpModel,data)
  }
  output$Pred <- renderImage({
    input$button 
    req(isolate(input$gender))
    
    if (isolate(input$gender) == 'male'){
      data<- data.frame(age=isolate(input$age),
                        gender=1,
                        height_cm=isolate(input$height),
                        weight_kg=isolate(input$weight),
                        bodyfat_per=isolate(input$bodyfat_per),
                        diastolic=isolate(input$diastolic),
                        systolic=isolate(input$systolic),
                        gripForce=isolate(input$gripforce),
                        sitbend=isolate(input$sitbend),
                        broadjump=isolate(input$broadjump),
                        situp=isolate(input$situp))
      
    } else if(isolate(input$gender) == 'female'){
        data<- data.frame(age=isolate(input$age),
                          gender=2,
                          height_cm=isolate(input$height),
                          weight_kg=isolate(input$weight),
                          bodyfat_per=isolate(input$bodyfat_per),
                          diastolic=isolate(input$diastolic),
                          systolic=isolate(input$systolic),
                          gripForce=isolate(input$gripforce),
                          sitbend=isolate(input$sitbend),
                          broadjump=isolate(input$broadjump),
                          situp=isolate(input$situp))
    }
    
    pred <- predict(final_model,newdata= data)
    pred <- which.max(pred)
    print(pred)
    if (pred == "1"){
      # Generate the PNG
      list(src = './predA.PNG',width = "100%", height = "900px")
    }else if (pred == "2"){
      list(src = './predB.PNG',width = "100%", height = "900px")
    }else if (pred == "3"){
      list(src = './predC.PNG',width = "100%", height = "900px")
    }else if (pred == "4"){
      list(src = './predD.PNG',width = "100%", height = "900px")
    }
      
  }, deleteFile = FALSE)
  #observeEvent(input$button,{output$Pred <- renderPrint(pred())})
  
}





# Run the application 
shinyApp(ui = ui, server = server)
