############################################################New version 2018/3/9
library(tidyverse)
library(stringr)
library(zoo)

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

get_url <- function(i){
  urls <- str_c(url1, i, url2, sep = "")
  urls
}

get_filenames <- function(i){
  filenames <-  str_c("mr", i, sep = "")
  filenames
}

get_MR <- function(a,b) {
  N <- length(a)
  for (i in 1:N){
    suppressMessages(
      assign(b[i], read_table(a[i],col_names = T))
    )
    
    file <- get(b[i])
    
    colnames(file)[1] <- "YYYY"
    file$YYYY <- as.numeric(file$YYYY) 

    if(i == 1){
      MR <- file
    }else{
      MR <- rbind(MR, file)
    }
  }
  MR  
}

clean_atmp <- function(i){
  a <- i %>% filter(hh==12) %>% select(YYYY,MM,DD,ATMP)
  return(a)
  
}

clean_wtmp <- function(i){
  b <- i %>% filter(hh==12) %>% select(YYYY,MM,DD,WTMP)
  return(b)
  
}

trans4 <- function(i){
  i[i>50] <- NA
  i <- na.approx(i)
}

get_atmp <- function(i){
  a <- get_MR(get_url(i),get_filenames(i))
  b <- clean_atmp(a)%>% select(ATMP)
  c <- trans4(b)
  c
}

get_wtmp <- function(i){
  a <- get_MR(get_url(i),get_filenames(i))
  b <- clean_wtmp(a) %>% select(WTMP)
  c <- trans4(b)
  c
}

graphtest <- function(a){
  a <- ts(a, start = c(1987,1,1), frequency=365)
  b <- data.frame(Time=c(time(a)),Atmp=c(a))
  p <- ggplot(b,aes(x=Time,y=Atmp))
  p + geom_line(colour = 'Blue') + xlab('The Time Series of Date') + ylab('ATMP')
}

get_atmp_mess <- function(i){
  a <- get_MR(get_url(i),get_filenames(i))
  b <- clean_atmp(a)%>% select(ATMP)
  b[b>50] <- NA
  b
}

#data in 2010,2011,2012 is missing a lot, data in 2013 is totally missing...

#1987-2009
for (i in c(1987:2009)){
  if(i == 1987){
    data1987_2009 <- get_atmp(i)
  }else{
    data1987_2009 <- rbind(a, get_atmp(i))
  }
}

View(a)






#2010,2011,2012

data2010 <- get_atmp_mess(2010)

data2011 <- get_atmp_mess(2011)

data2012 <- get_atmp_mess(2012)

data2013 <- data2012

data2010_2013 <- rbind(data2010,data2011,data2012,data2013)

#2014-2017

for (i in c(2014:2017)){
  if(i == 2014){
    data2014_2017 <- get_atmp(i)
  }else{
    data2014_2017 <- rbind(data2014_2017, get_atmp(i))
  }
}

View(data2014_2017)



#total data

data_total <- rbind(data1987_2009,data2010_2013,data2014_2017)

#final graph
a <- ts(data_total, start = c(1987,1,1), frequency=365)
b <- data.frame(Time=c(time(a)),Atmp=c(a))
p <- ggplot(b,aes(x=Time,y=Atmp))
p + geom_line(colour = 'Blue') + xlab('The Time Series of Date') + ylab('ATMP')

