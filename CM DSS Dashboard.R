library(shiny)
library(shinydashboard)
library(ggplot2)
library(readr)
library(tidyverse)
library(DT)
library(rsconnect)

cm <- read_csv("../DemoCM.csv")

## Comes from SHINY DAshboard
ui <- dashboardPage(
  dashboardHeader(title="Construction Management"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Reactive Datatable", tabName = "cmtable", icon=icon("table")),
      menuItem("Demographics", tabName="demographics", icon=icon("bar-chart-o"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "cmtable", 
              h2("Reactive Datatable"),
              fluidRow(
                column(4, selectInput("grad_year", "Anticipated Graduation:", 
                                      c("Select","Winter 2018", "Spring 2018", "Fall 2018", "Winter 2019", "Spring 2019", "Fall 2019", "Winter 2020", "Spring 2020", "Fall 2020", "Winter 2021", "Spring 2021", "Fall 2021", "Winter 2022", "Spring 2022", "Fall 2022", "Winter 2023", "Spring 2023", "Fall 2023", "Winter 2024", "Spring 2024", "Fall 2024", "Winter 2025", "Spring 2025", "Fall 2025"))),
                column(4, selectInput("nextIntern", "Semester of Next Internship:",
                                      c("Select", "Fall 2017", "Spring 2017", "Winter 2018", "Spring 2018", "Fall 2018", "Winter 2019", "Spring 2019", "Fall 2019", "Winter 2020", "Spring 2020", "Fall 2020", "Winter 2021", "Spring 2021", "Fall 2021", "Winter 2022", "Spring 2022", "Fall 2022"))),
                column(4, selectInput("internNum", "Number of Internships Completed:", c("Select", 0, 1, 2))),
                column(4, textInput("stateInterest", "Anticipated State for Next Internship or Job")),
                column(4, textInput("passion", "Student's passion")),
                column(4, textInput("experience", "Student's experience"))
                
                
              )
              ,
              DT::dataTableOutput("table"), style = "overflow-x: scroll;"
      ),
      tabItem(tabName = "demographics",
              h2("Student Demographics/Summaries"),
              fluidRow(
                box(height=463,
                    plotOutput("plot1")
                ),
                
                tabBox(
                  tabPanel("First Internship",
                           plotOutput("plot2")
                  ),
                  tabPanel("Second Internship",
                           plotOutput("plot3")
                  ), 
                  tabPanel("Post Grad",
                           plotOutput("plot4")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) { 
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- cm
    if (input$grad_year != "Select") {
      data <- data[data$`Anticipated Graduation Semester Year` == input$grad_year,]
    } 
    if(input$passion!=""){
      data <- data %>% 
        mutate(checkPassion = str_detect(data$Passions, input$passion)) %>%
        filter(checkPassion==TRUE) %>% 
        select(-checkPassion)
    }
    if(input$internNum!="Select"){
      data <- data[data$`Number of Internships` == input$internNum,]
    }
    if (input$nextIntern != "Select") {
      data <- data[data$`Next Anticipated Internship Semester Year` == input$nextIntern,]
    }
    if(input$stateInterest!=""){
      data <- data %>% 
        mutate(check1 = str_detect(data$`Anti FI State`, input$stateInterest),
               check2 = str_detect(data$`Anti SI State`, input$stateInterest),
               check3 = str_detect(data$`Post Grad State`, input$stateInterest)) %>%
        filter(check1==TRUE| check2==TRUE |check3==TRUE) %>% 
        select(-c(check1, check2, check3))
    }
    if(input$experience!=""){
      data <- data %>% 
        mutate(checkExperience = str_detect(data$Experience, input$experience)) %>%
        filter(checkExperience==TRUE) %>% 
        select(-checkExperience)
    }
    data
  })
  )
  
  
  
  output$plot1 <- renderPlot ({
    cm %>% 
      ggplot(aes(x=`Number of Internships`))+
      geom_bar(fill="steelblue")+
      theme_minimal()+
      labs(title="Number of Students That Have Completed None, 1, and 2 Internships", 
           y="Number of Students", x="Internships Completed")
  })
  
  output$plot2 <- renderPlot ({
    FIrate <- cm %>% filter(!is.na(`FI Pay Rate`))
    avgFIrate <- mean(FIrate$`FI Pay Rate`)
    cm %>% ggplot(aes(x=`FI Pay Rate`))+
      geom_histogram(fill="steelblue", binwidth = 1)+
      geom_vline(xintercept = avgFIrate, size=2, color="grey30")+
      theme_minimal()+
      labs(title="Hourly Rate Distribution for First Internship", y="Count", x="Dollars Per Hour",
           subtitle=paste("Average hourly rate is", " $", round(avgFIrate, 0), sep=""))
  })
  
  output$plot3 <- renderPlot ({
    SIrate <- cm %>% filter(!is.na(`SI Pay Rate`))
    avgSIrate <- mean(SIrate$`SI Pay Rate`)
    cm %>% ggplot(aes(x=`SI Pay Rate`))+
      geom_histogram(fill="steelblue", binwidth = 1)+
      geom_vline(xintercept = avgSIrate, size=2, color="grey30")+
      theme_minimal()+
      labs(title="Hourly Rate Distribution for Second Internship", y="Count", x="Dollars Per Hour",
           subtitle=paste("Average hourly rate is", " $", round(avgSIrate, 0), sep=""))
    
  })
  
  output$plot4 <- renderPlot ({
    PGrate <- cm %>% filter(!is.na(`Post Grad Pay Rate`))
    avgPGrate <- mean(PGrate$`Post Grad Pay Rate`)
    cm %>% ggplot(aes(x=`Post Grad Pay Rate`))+
      geom_histogram(fill="steelblue")+
      geom_vline(xintercept = avgPGrate, size=2, color="grey30")+
      theme_minimal()+
      scale_x_continuous(labels=scales::comma)+
      labs(title="Salary Distribution for Post Graduation Job", y="Count", x="Salary",
           subtitle=paste("Average salary is", " $", round(avgPGrate, 0), sep=""))
    
  }) 
  
  
}
## END WITH THIS
shinyApp(ui, server)

