library(rvest)
library(stringr)
library(ggplot2)
library(reshape)
library(shiny)
library(shinydashboard)
library(graphics)
library(ggplot2)
library(lubridate)
countKeyword <- function(text_to_view,keyword){
  text_to_view <- tolower(text_to_view)
  keyword <- tolower(keyword)
  ct = 0 
  ct = ct + str_count(text_to_view,fixed(keyword))
  ct
}
searchJob_urls <- function(job, n){
  run_date <- Sys.Date()
  print(paste0("https://www.indeed.com/jobs?q=", job, "&start=", n*10))
  page = read_html(paste0("https://www.indeed.com/jobs?q=", URLencode(job), "&start=", n*10))
  jobcards <- html_node(page, "#mosaic-provider-jobcards")
  job_links <- html_nodes(jobcards, 'a[id^="job"]')
  for(k in 1:length(job_links)){
    link <- job_links[k]
    urls <- paste0("https://www.indeed.com", html_attr(link, "href"))
    write.table(urls,file=paste0(job,"-urls-",run_date,"_temp.csv"),append = TRUE,row.names = FALSE,sep=",")
  }
  print(paste0("n=",n))
  print(job_links)
}


searchJob <- function(id,urls,tech){
  tech_num <- matrix(nrow=1, ncol=length(tech)+1)
  page <- read_html(urls)
  print(page)
  dates <- html_text(html_nodes(page,"span.jobsearch-HiringInsights-entry--text"))
  print(dates)
  #job_titles <- html_text(html_nodes(page,"h1.icl-u-xs-mb--xs icl-u-xs-mt--none jobsearch-JobInfoHeader-title"))
  job_titles <- html_text(html_nodes(page,"h1.jobsearch-JobInfoHeader-title"))
  print(job_titles)
  # < -html_text(html_node(page,"#icl-u-xs-mr--xs"))
  
  job_companies <- html_text(html_nodes(page,"div[class^='icl-u-lg-mr--sm icl-u-xs-mr--xs']"))
  job_companies <- job_companies[length(job_companies)]
  job_desc <- html_text(html_node(page,"div#jobDescriptionText"))
  dates <- html_text(html_nodes(page,"span.jobsearch-HiringInsights-entry--text"))
  dates_split <- strsplit(dates[length(dates)]," ")
  print(dates_split)
  print(lengths(dates_split))
  print("length")
  run_date <- Sys.time() 
  if (lengths(dates_split) < 4){
    job_dates <- format(run_date,format = "%Y-%m-%d")
  } else {
    dates_split <- unlist(dates_split)
    if(str_count(dates_split[2],fixed("30+")) == 1){
      job_dates <- dates
    }else{
      days <- as.numeric(dates_split[2])
      job_dates <- format(as.POSIXlt(run_date-24*60*60*days),format = "%Y-%m-%d")
    }
    
  }
  
  for (j in 1:length(tech)){
    tech_num[1,1] <- id
    tech_num[1,j+1] <- countKeyword(job_desc,tech[j])
  }
  
  tech_num <- as.data.frame(tech_num)
  for (j in 1:length(tech)){
    names(tech_num)[1] <- "id"
    names(tech_num)[j+1] <- tech[j]
  }
  job_info=data.frame(id=id,job_titles=job_titles,job_companies=job_companies,job_dates=job_dates)
  
  output <- merge(job_info,tech_num,by="id")
  output
}

run_date <- Sys.Date()
strat_time <- Sys.time()
n_start <-0
job <- "Software MOBILE"
n_end <- 9
for(m in n_start:n_end){
  searchJob_urls(job,m)
}
file <-paste0(job,"-urls-",run_date,"_temp.csv") 
print(file)
urls_temp <- read.csv(file,header = FALSE)
urls_temp <- as.vector(urls_temp[,1])
print(length(urls_temp))
urls <- matrix(nrow=length(urls_temp)/2, ncol=1)
id <- c(1:length(urls))
for(i in 1:length(id)){
  urls[i] <- urls_temp[2*i]
}
job_urls=data.frame(id=id,job_urls=urls)
write.table(job_urls,file=paste0(job,"-urls-",run_date,".csv"),row.names = FALSE,sep=",")
#----------------------

#tech <- c("JAVA","Go","Python","Ruby","JavaScript"," C ","C#","C++"," R ","Swift","PHP")
#tech <- c("Lean","Agile","Scrum","Kanban","Waterfall")
tech <- c("AutoML","Neural networks","TensorFlow","Modeling","Data Processing","Deployment","Scikit learn","Tuning","Math")
#tech <- c("SQL","NoSQL","Analytics","Visualization","Pipelines","Cassandra","Data warehouses","Python"," R ")
#tech <- c("JavaScript","CSS","HTML")
#tech <- c("Toolkits","Caching","IOS","Android","Monitoring/alerting")
#tech <- c("Servers","VMs","SRE","DevOps","AIOps","Docker","Kubernetes")
#tech <- c("IBM Cloud","Oracle Cloud Infrastruture","Alibaba","Google Cloud Platform","Microsoft Azure","Amazon Web Service")
run_date <- "2022-04-14"
tech <- c("Toolkits","Caching","IOS","Android","Monitoring","alerting")
file <-paste0(job,"-urls-",run_date,".csv")
print(file)
job_urls <- read.csv(file,header = TRUE)
id <- job_urls$id
job_urls <- job_urls$job_urls
print(length(job_urls))


#length(job_urls)
for (n in 220:229){
  result <- searchJob(id[n],job_urls[n],tech)
  write.table(result,file=paste0(job,"-statistical-result-",run_date,"_temp.csv"),append = TRUE,row.names = FALSE,sep=",")
}
end_time <- Sys.time()
print(strat_time)
print(end_time) 


file=paste0(job,"-statistical-result-",run_date,"_temp.csv")
print(file)
result_temp <- read.csv(file)
print(length(result_temp$id))
del <- seq(2, length(result_temp$id), by = 2)
print(del)
result <- result_temp[-del, ]
print(result$id)
write.table(result,file=paste0(job,"-statistical-result-",run_date,".csv"),row.names = FALSE,sep=",")

file=paste0(job,"-statistical-result-",run_date,".csv")
job_message <- read.csv(file,header = TRUE)
#print(job_message$id)
file="data-statistical-result-2022-04-10.csv"
tech <- c("SQL","NoSQL","Analytics","Visualization","Pipelines","Cassandra","Data warehouses")
job_message <- read.csv(file,header = TRUE)
statistical_num <- matrix(nrow=length(unique(job_message$job_dates)), ncol=length(tech)+1)
print(length(unique(job_message$job_dates)))
job_dates_uniq <- sort(unique(job_message$job_dates))
date <- c(job_dates_uniq[length(job_dates_uniq)],job_dates_uniq)
print(date)
date <- date[-length(date)]
print(date)
for (i in 1:length(date)){
  for (j in 1:length(tech)){
    statistical_num[i,j+1] <- as.numeric(sum(job_message[job_message$job_dates==date[i],tech[j]]))
  }
  statistical_num[i,1] <- date[i]
}
print(statistical_num)
class(statistical_num[2,2])
statistical_num <- as.data.frame(statistical_num)
print(statistical_num)
names(statistical_num)[1] <- "date"
print(statistical_num)
for (j in 1:length(tech)){
  names(statistical_num)[j+1] <- tech[j]
}
print(statistical_num)
statistical_num[,2:length(tech)+1] <- lapply(statistical_num[,2:length(tech)+1],as.numeric)
print(statistical_num$date)
class(statistical_num[2,2])
ggplot(statistical_num,aes(x=date,y=tech[2])) + geom_bar(stat = "identity")+ 
  theme(axis.text.x=element_text(angle=60, hjust=1))+ 
  labs(title="Mentioned times of technologies in job descriptions", 
       x="Date", y = "SQL")
#geom_bar(stat = "identity")geom_histogram(binwidth = 15)
#wer <- c("14","78","1","2","3","4","5")
wer <- c(14,78,1,2,3,4,5)
print(wer)
class(wer[3])
qwe <- data.frame(x=wer,tech=tech)
print(qwe)
ggplot(qwe,aes(x=tech,y=wer)) + 
  geom_bar(stat = "identity")
gc()
result_temp <- read.csv("programming-statistical-result-2022-04-12.csv")
names(result_temp)[11] <- "C#"
names(result_temp)[12] <- "C++"
write.table(result,file="programming-statistical-result-2022-04-12.csv",row.names = TRUE ,sep=",")
result_temp <- read.csv("programming-statistical-result-2022-04-12.csv")
