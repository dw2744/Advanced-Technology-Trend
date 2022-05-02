library(rvest)
library(stringr)
library(shiny)
library(shinydashboard)
library(graphics)
library(ggplot2)
library(lubridate)
library(rdrop2)

#outputDir <-""
token<-readRDS("token.rds")
#token$refresh()

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Tech Trend Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "Overview", icon = icon("th")),
      menuItem("Scanning", tabName = "Scanning", icon = icon("fa-solid fa-hand-sparkles")),
      menuItem("Trend", tabName = "Trend", icon = icon("bar-chart-o"))
      
    )
  ), 
  
  dashboardBody(
    tags$style(HTML("
 .logo {background-color: #8900e1 !important;
        }
 .navbar {
background-color: #8900e1 !important;
 }
 .skin-blue .main-sidebar {
  color:#fff;
  background:#57068c
 }
.skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  color:#fff;
  background:#CDB5CD
                          }
.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#8900e1
                    }
.box.box-solid.box-primary{
border-bottom-color:#8900e1;
border-left-color:#8900e1;
border-right-color:#8900e1;
border-top-color:#8900e1;
} 
.box.box-primary>.box-header {
  color:#fff;
  background:#8900e1
                    }
.box.box-primary{
border-bottom-color:#8900e1;
border-left-color:#8900e1;
border-right-color:#8900e1;
border-top-color:#8900e1;
} 
.box.box-solid.box-warning>.box-header {
  color:#fff;
  background:#8900e1
                    }
.box.box-solid.box-warning{
border-bottom-color:#8900e1;
border-left-color:#8900e1;
border-right-color:#8900e1;
border-top-color:#8900e1;
} 
button.btn.btn-primary {
  color:#fff;
  background:#8900e1
} 
button.btn.btn-solid.btn-primary{
border-bottom-color:#8900e1;
border-left-color:#8900e1;
border-right-color:#8900e1;
border-top-color:#8900e1;
}                    
                    ")),
    
    tabItems(
      tabItem(tabName = "Overview",
              fluidRow(
                box(h2(strong("Advanced technology Trend Analyzer"), align = "center"),
                    br(),
                    h4("A real-time tool, Tech Trend Tracker, which automates the generation of bar charts
                    and graphs by using R language to crawl the Indeed.com and perform 
                    keywords frequency analysis of different categories of technologies.", align = "left"),
                    br(),
                    h2(strong("User Guide"), align = "center"),
                    h4("Step 1: Select category to scan", align = "left"),
                    p("1.1  Select None: Check the data without scanning, you don't need extra time to wait. 
                      Skip the Scanning page and click the Trend page to view the charts.
                      ", align = "left"),
                    p("1.2  Multiple Choice: Select one or many categories for scanning, 
                      and only the scanned data can generate the latest data.
                      Select the categories needed to scan, click the scanning button, 
                      the font color in the box will turn gray.
                      After the 'Scanning is finished' appears, click the Trend page to view the result.
                      ", align = "left"),
                    p("Tips: Due to the anti-crawl settings of the Indeed.com , it takes a few minutes to scan each category
                      (which also depends on the internet speed). Please make sure you have enough time to scan. 
                      The scanning may also be terminated due to the poor network conditions, so please make sure the stable internet network.
                      ", align = "left"),
                    br(),
                    h4("Step 2: Check the charts on the Trend page.", align = "left"),
                    p("Tips: If you need the latest data, make sure the Scanning page is scanning successfully.", align = "left"),
                    width = 8
                ),
                box(
                  imageOutput("plot_cover"), width = 4,status = "primary", align = "center")
                
                
              )
              
      ),
      tabItem(tabName = "Scanning",
              fluidRow(
                #radioButtons(
                box( title = "Select the category for Scanning", status = "primary",solidHeader = TRUE,
                     checkboxGroupInput(
                       "variable_job", 
                       "",
                       choices =  list("Programming" = "Programming", 
                                       "Methodologies" = "Methodologies",
                                       "Machine learning"= "Machine learning",
                                       "Data"= "Data",
                                       "Software Web"= "Software Web",
                                       "Software Mobile"= "Software Mobile",
                                       "Infrastructure"= "Infrastructure",
                                       "Cloud"=  "Cloud"
                       )#,selected = "none"
                       
                       
                     ),
                     submitButton("Scanning"),
                     width = 6,
                     
                ),
                box(h3(strong("Step 1"), align = "left"),
                    
                    h4("Select category to scan", align = "left"),
                    p("1.1  Select None: Check the data without scanning, you don't need extra time to wait. 
                      Skip the Scanning page and click the Trend page to view the charts.
                      ", align = "left"),
                    p("1.2  Multiple Choice: Select one or many categories for scanning, 
                      and only the scanned data can generate the latest data.
                      Select the categories needed to scan, click the scanning button, 
                      the font color in the box will turn gray.
                      After the 'Scanning is finished' appears, click the Trend page to view the result.
                      ", align = "left"),
                    p("Tips: Due to the anti-crawl settings of the Indeed.com, it takes a few minutes to scan each category
                      (which also depends on the internet speed). Please make sure you have enough time to scan. 
                      The scanning may also be terminated due to the poor network conditions, so please make sure the stable internet network.
                      ", align = "left"),
                    width = 6
                )
              ),
              fluidRow(box(title = "Scanning Status",solidHeader = TRUE, status = "warning",width = 6,
                           textOutput("tx")   
              ))
      ),
      tabItem(tabName = "Trend",
              tabsetPanel(
                tabPanel("CATEGORIES",
                         fluidRow(
                           box(status = "primary",solidHeader = TRUE,plotOutput("plot_1"), width = 8),
                           box(#selectInput("variable_job_1", 
                             # "Select Job",
                             # choices=c("Programming","Methodologies","Machine learning","Data",
                             #"Software Web","Software Mobile","Infrastructure","Cloud")),
                             radioButtons(
                               "variable_job_1", 
                               "Select the category",
                               choices =  list("Programming" = "Programming", 
                                               "Methodologies" = "Methodologies",
                                               "Machine learning"= "Machine learning",
                                               "Data"= "Data",
                                               "Software Web"= "Software Web",
                                               "Software Mobile"= "Software Mobile",
                                               "Infrastructure"= "Infrastructure",
                                               "Cloud"=  "Cloud"
                               ),selected = "Programming"
                               
                             ),
                             submitButton("Track"),title = "Select the category",solidHeader = TRUE, status = "warning",width = 4
                           )
                           
                         )
                         
                         
                ),
                tabPanel("TREND",
                         fluidRow(
                           box(plotOutput("plot_2"), width = 8, status = "primary",solidHeader = TRUE),
                           box(#selectInput("variable_job_2", 
                             #"Select a category",
                             #choices=c("Programming","Methodologies","Machine learning","Data",
                             #"Software Web","Software Mobile","Infrastructure","Cloud")),
                             radioButtons(
                               "variable_job_2", 
                               "",
                               
                               choices =  list("Programming" = "Programming", 
                                               "Methodologies" = "Methodologies",
                                               "Machine learning"= "Machine learning",
                                               "Data"= "Data",
                                               "Software Web"= "Software Web",
                                               "Software Mobile"= "Software Mobile",
                                               "Infrastructure"= "Infrastructure",
                                               "Cloud"=  "Cloud"
                               ),selected = "Programming"
                               
                             ),
                             submitButton("Track"),title = "Select the Category",solidHeader = TRUE, status = "warning",width = 4
                           )
                           
                         )
                )
              ))
    )
  )
)

server <-function(input, output) {

  fileselect <-function(job){
    files <- drop_dir()
    files <- files$name
    csvfiles <- grep(job,files, value = TRUE)
    csvfiles
  }
  dateselect <-function(file,job){
    num <- c(1:length(file))
    for(i in 1:length(file)){
      num[i] <- gsub(paste0(job,"-"),"",file[i])
      num[i] <- gsub(".csv","",num[i])
    }
    num
  }
  countKeyword <- function(text_to_view,keyword){
    text_to_view <- tolower(text_to_view)
    keyword <- tolower(keyword)
    ct = 0 
    ct = ct + str_count(text_to_view,fixed(keyword))
    ct
  }
  searchtech_fre <- function(job,tech){
    urls <- c()
    for(n in 0:9){
      Sys.sleep(0.005)
      print(paste0("https://www.indeed.com/jobs?q=", job, "&start=", n*10))
      page1 <- read_html(paste0("https://www.indeed.com/jobs?q=", job, "&start=", n*10))
      jobcards <- html_node(page1, "#mosaic-provider-jobcards")
      job_links <- html_nodes(jobcards, 'a[id^="job"]')
      url <- paste0("https://www.indeed.com", html_attr(job_links, "href"))
      urls <- c(urls,url)
    }
    
    tech_num <- matrix(data = "0",nrow=length(urls), ncol=length(tech))
    for(j in 1:length(urls)){
      page <- read_html(urls[j])
      Sys.sleep(0.005)
      job_desc <- html_text(html_node(page,"div#jobDescriptionText"))
      for(k in 1:length(tech)){
        if(is.na(countKeyword(job_desc,tech[k]))){
          tech_num[j,k] <- 0
        }else{
          tech_num[j,k] <- countKeyword(job_desc,tech[k])
        }
      }
    }
    
    result <- as.data.frame(tech_num)
    for (j in 1:length(tech)){
      names(result)[j] <- tech[j]
    }
    run_date <- Sys.Date()
    filename <- paste0(job,"-",run_date,".csv") 
    filename <- gsub("%20"," ",filename )
    write.table(result,file=filename,row.names = FALSE ,sep=",")
    drop_upload(filename, mode="overwrite")
  }
  plot_histogram <-function(file,job,tech){
    job_message <- drop_read_csv(file,header = TRUE)
    plotdate <- dateselect(file,job)
    statistical_num <- c()
    
    for (j in 1:length(tech)){
      statistical_num <- c(statistical_num,as.numeric(sum(job_message[1:nrow(job_message),tech[j]])))
    }
    data_plot <- matrix(nrow=(length(tech)), ncol=2)
    data_plot <- as.data.frame(data_plot)
    names(data_plot)[1] <- "tech"
    names(data_plot)[2] <- "num"
    data_plot$tech <- tech
    data_plot$num <- statistical_num
    data_plot <- data_plot[order(data_plot[,"num"],decreasing = T),]
    
    ggplot(data_plot,aes(x=reorder(tech,-num),y=num)) + geom_bar(stat = "identity",fill=c("purple"))+ 
      theme(axis.text.x=element_text(angle=60,face="bold",size=14, hjust=1),axis.text.y=element_text(size=14),
            axis.title.x=element_text(size=15,face="bold"),axis.title.y=element_text(size=15,face="bold"),
            plot.title = element_text(hjust=0.5,size=16,face="bold"))+ 
      labs(title=paste0("Mentioned times of technologies in job descriptions","(",plotdate,")") , 
           x="Technologies", y = "Frequency")
  } 
  plot_line_chart_day <-function(file,date,tech){
    statistical_num <- matrix(nrow=length(file), ncol=length(tech)+1)
    for (i in 1:length(file)){
      job_message <- drop_read_csv(file[i],header = TRUE)
      for (j in 1:length(tech)){
        statistical_num[i,j+1] <- as.numeric(sum(job_message[1:nrow(job_message),tech[j]]))
      }
      statistical_num[i,1] <- date[i]
    }
    
    statistical_num <- as.data.frame(statistical_num)
    names(statistical_num)[1] <- "date"
    for (j in 1:length(tech)){
      names(statistical_num)[j+1] <- tech[j]
    }
    data_plot <- matrix(nrow=(length(date)*length(tech)), ncol=3)
    data_plot <- as.data.frame(data_plot)
    Technologies <- c()
    for (i in 1:length(tech)){
      temp <- c(rep(tech[i], length(date)))
      Technologies <- c(Technologies,temp)
    }
    date <- c(rep(date, length(tech)))
    num <- c()
    for (i in 2:ncol(statistical_num)) {
      num <- c(num,statistical_num[,i])
    }
    names(data_plot)[1] <- "Technologies"
    names(data_plot)[2] <- "date"
    names(data_plot)[3] <- "num"
    data_plot$Technologies <- Technologies
    data_plot$date <- date
    data_plot$num <- as.numeric(num)
    Technologies <- as.factor(data_plot$Technologies)
    ggplot(data = data_plot, aes(x = date, y = num,  color = Technologies, shape = Technologies)) + 
      geom_point(size = 3) + 
      geom_line(aes(group=Technologies),size = 2)+ 
      theme(axis.text.x=element_text(angle=60, hjust=1,size=14),axis.text.y=element_text(size=14),
            axis.title.x=element_text(size=15,face="bold"),axis.title.y=element_text(size=15,face="bold"),
            plot.title = element_text(hjust=0.5,size=16,face="bold"))+ 
      labs(title="Mentioned times of technologies in job descriptions",
           x="Date(Daily)", y = "Frequency")
  }
  jobText <- reactive({
    paste0(input$variable_job)
  })
  output$tx <- renderText({
    tx <- "Waiting Scanning"
    for (job in jobText()){
      object <- gsub(" ","",job)
      tech <- switch (object,
                      Programming =c("JAVA","Go","Python","Ruby","JavaScript"," C ","C#","C++"," R ","Swift","PHP"),
                      Methodologies = c("Lean","Agile","Scrum","Kanban","Waterfall"),
                      Machinelearning =c("AutoML","Neural networks","TensorFlow","Modeling","Data Processing","Deployment","Scikit learn","Tuning","Math"),
                      Data = c("SQL","NoSQL","Analytics","Visualization","Pipelines","Cassandra","Data warehouses","Python"," R "),
                      SoftwareWeb = c("JavaScript","CSS","HTML"),
                      SoftwareMobile = c("Toolkits","Caching","IOS","Android","Monitoring/alerting"),
                      Infrastructure = c("Servers","VMs","SRE","DevOps","AIOps","Docker","Kubernetes"),
                      Cloud = c("IBM Cloud","Oracle Cloud Infrastruture","Alibaba","Google Cloud Platform","Microsoft Azure","Amazon Web Service")
      )
      jobtemp <- gsub(" ","%20",job)
      searchtech_fre(jobtemp,tech)
      tx <- "Scanning is finished"
    }
    #tx <- "IT WILL BE TAKE SEVERAL MINUTES FOR CRAWING ALL THE JOB"
    
    tx
  })
  
  jobText_1 <- reactive({
    paste0(input$variable_job_1)
  })
  output$plot_cover <- renderImage({
    imagefile <- "Rlogo.jpg"
    deleteFile=FALSE
    list(src = imagefile,width="60%")
  })
  output$plot_1 <- renderPlot({
    filename <- fileselect(paste0(jobText_1()))
    filedate <- dateselect(filename,paste0(jobText_1()))
    object <- gsub(" ","",paste0(jobText_1()))
    tech <- switch (object,
                    Programming =c("JAVA","Go","Python","Ruby","JavaScript"," C ","C#","C++"," R ","Swift","PHP"),
                    Methodologies = c("Lean","Agile","Scrum","Kanban","Waterfall"),
                    Machinelearning =c("AutoML","Neural networks","TensorFlow","Modeling","Data Processing","Deployment","Scikit learn","Tuning","Math"),
                    Data = c("SQL","NoSQL","Analytics","Visualization","Pipelines","Cassandra","Data warehouses","Python"," R "),
                    SoftwareWeb = c("JavaScript","CSS","HTML"),
                    SoftwareMobile = c("Toolkits","Caching","IOS","Android","Monitoring/alerting"),
                    Infrastructure = c("Servers","VMs","SRE","DevOps","AIOps","Docker","Kubernetes"),
                    Cloud = c("IBM Cloud","Oracle Cloud Infrastruture","Alibaba","Google Cloud Platform","Microsoft Azure","Amazon Web Service")
    )
    plot_histogram(paste0(jobText_1(),"-",filedate[length(filedate)],".csv"),paste0(jobText_1()),tech)
  })
  jobText_2 <- reactive({
    paste0(input$variable_job_2)
  })
  output$plot_2 <- renderPlot({
    filename <- fileselect(paste0(jobText_2()))
    filedate <- dateselect(filename,paste0(jobText_2()))
    object <- gsub(" ","",paste0(jobText_2()))
    tech <- switch (object,
                    Programming =c("JAVA","Go","Python","Ruby","JavaScript"," C ","C#","C++"," R ","Swift","PHP"),
                    Methodologies = c("Lean","Agile","Scrum","Kanban","Waterfall"),
                    Machinelearning =c("AutoML","Neural networks","TensorFlow","Modeling","Data Processing","Deployment","Scikit learn","Tuning","Math"),
                    Data = c("SQL","NoSQL","Analytics","Visualization","Pipelines","Cassandra","Data warehouses","Python"," R "),
                    SoftwareWeb = c("JavaScript","CSS","HTML"),
                    SoftwareMobile = c("Toolkits","Caching","IOS","Android","Monitoring/alerting"),
                    Infrastructure = c("Servers","VMs","SRE","DevOps","AIOps","Docker","Kubernetes"),
                    Cloud = c("IBM Cloud","Oracle Cloud Infrastruture","Alibaba","Google Cloud Platform","Microsoft Azure","Amazon Web Service")
    )
    plot_line_chart_day(filename,filedate,tech)
  })
  
  
  #output$downloadData <- downloadHandler(
  #filename = function() { paste(input$dataset, '.csv', sep='') },
  # content = function(file) {write.csv(datasetInput(), file)}
  # )
  
}

shinyApp(ui, server)
