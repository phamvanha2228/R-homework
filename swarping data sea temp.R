# install.packages("janitor")

library(rvest)
library(tidyverse)
library(xml2)
library(stringr)
# library(janitor)

##To get data directly from web

## 1.create a data frame to save the links 

year <- c(2000:2017)
output <- data.frame(year,link= NA)
i <- 1
while (i < length(year)+1)
{ z <- paste("https://www.idescat.cat/pub/?id=aec&n=218&t=", year[[i]],sep='')
output$link[i] <- z
i= i+1
}

## 2. create a dataframe to store all the data for looping run

url <- output$link[17] # "https://www.idescat.cat/pub/?id=aec&n=218&t=2002"

temp <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="Contingut"]/main/div[2]/div[1]/table') %>%
  html_table(fill = TRUE)

tempo <- temp[[1]]
tempo

tran <- t(na.omit(t(tempo)))

sea <- as.data.frame(tran) ## here is the dataframe to store all the data scraping from web. 

sea$year_filter <- NA

sea <- sea[-c(1:length(sea$year_filter)),] ## erase all the temporary data to start the looping run and store new data. 


##3. looping run to scrap data from web.  
i <- 1
while (i < length(output$link)+1 ) {
     scrappedurl <- output$link[i]
     
    temp1 <- scrappedurl %>%
            html() %>%
            html_nodes(xpath='//*[@id="Contingut"]/main/div[2]/div[1]/table') %>%
            html_table(fill = TRUE)
    
    temp2 <- temp1[[1]]
    tran <- as.data.frame(t(na.omit(t(temp2))))
    tran$year <- i+1999
    sea <- rbind(sea, tran, stringsAsFactors=FALSE)
    i = i+1
}

####this step will take time to get data. check in the evironement pannel to see that whether the dataframe gets data or not?


##4. clean the data. 

sea
test <- sea[-which(sea[2] == ""), ]

test <- test[-which(test[1] == "Font: Departament de Medi Ambient i Habitatge. Servei Meteorològic de Catalunya."), ]

test <- test[-which(test[1] == "Font: Departament de Medi Ambient. Servei Meteorològic de Catalunya."),]
test <- test[-which(test[1] == "Font: Departament de Territori i Sostenibilitat. Servei Meteorològic de Catalunya."),]

##5. Name the columns
names(test)[2] <- "Deep_0_m"
names(test)[3] <- "Deep_minus20_m"
names(test)[4] <- "Deep_minus50_m"
names(test)[5] <- "Deep_minus80_m"
names(test)[1] <- "year_month"

##6. clean one more time
test <- test[-which(test[1] == ""), ]
test <- test[-c(grep("Any", test$year_month)),]

####change data into numeric
test$Deep_minus80_m <- sub(",", ".", test$Deep_minus80_m)

test$Deep_minus50_m <- sub(",", ".", test$Deep_minus50_m)
test$Deep_minus20_m <- sub(",", ".", test$Deep_minus20_m)
test$Deep_0_m <- sub(",", ".", test$Deep_0_m)


test$Deep_minus80_m <- as.numeric(test$Deep_minus80_m)
test$Deep_minus50_m <- as.numeric(test$Deep_minus50_m)
test$Deep_minus20_m <- as.numeric(test$Deep_minus20_m)
test$Deep_0_m <- as.numeric(test$Deep_0_m)



##7. final table 

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))}



final_table <- test

final_table$Period_comp <- NA

# for checking 
# library(plyr)
# count(as.factor(final_table$year))


e <- 0
r <- 0
while (e < 18){
  while (r <14)
    {final_table$Period_comp[14+27*e+r] <- substrRight(as.vector(final_table$year_month[14+ 27*e]), 9)
    r=r+1
  }
  e= e + 1
  r <- 0
}


final_table$month <-  ifelse(final_table$year_month =="gener"|final_table$year_month =="Gener", 1, 
                           ifelse( final_table$year_month =="febrer"|final_table$year_month =="Febrer", 2,
                                   ifelse( final_table$year_month =="març"|final_table$year_month =="Març", 3,
                                           ifelse( final_table$year_month =="abril"| final_table$year_month =="Abril", 4,
                                                   ifelse( final_table$year_month =="maig"|final_table$year_month =="Maig", 5,
                                                           ifelse( final_table$year_month =="juny"|final_table$year_month =="Juny", 6,
                                                                   ifelse( final_table$year_month =="juliol"| final_table$year_month =="Juliol", 7,
                                                                           ifelse( final_table$year_month =="agost"| final_table$year_month =="Agost", 8,
                                                                                   ifelse( final_table$year_month =="setembre"|final_table$year_month =="Setembre", 9,
                                                                                           ifelse( final_table$year_month =="octubre"|final_table$year_month =="Octubre", 10,
                                                                                                   ifelse( final_table$year_month =="novembre"|final_table$year_month =="Novembre", 11,
                                                                                                           ifelse( final_table$year_month =="desembre"|final_table$year_month =="Desembre", 12, "annual"))))))))))))







final_table$type <- ifelse( is.na(final_table$Period_comp)== TRUE, "starting year","compared period")

final_table <- final_table[-c(grep("Període", final_table$year_month)),]

# library(openxlsx)
# write.xlsx(final_table, 'sea_temperature_fixed.xlsx')



library(reshape)
sea.deep <- melt(final_table, id=c("type","year","year_month", "Period_comp", "month"))

sea.deep$depth <- ifelse(sea.deep$variable =="Deep_0_m", 0,
                         ifelse(sea.deep$variable =="Deep_minus20_m", -20,
                                ifelse(sea.deep$variable =="Deep_minus50_m", -50, -80)))
names(sea.deep)[7] <- "temperature"

sea.deep$depth <- as.factor(sea.deep$depth)

summary(sea.deep)

# write.xlsx(sea.table, 'sea_temp.xlsx')




     
library(gplots)

     plt2 <- subset(sea.deep, type =="starting year"& year=="2017")     

     
     boxplot2(temperature~depth, plt2,frame = FALSE,
              notch = TRUE)   
     
     
     plotmeans(plt1$temperature~plt1$year)
  
     
library(plotrix)         
     plt3 <- subset(sea.deep,  month=="annual" )
     
     brkdn.plot(temperature~ type+depth, data=plt3, main="Test of the breakdown plot",
                mct="median",md="mad",xlab="Depth", ylab="Temperature",pch=1:4,lty=1:4,col=1:4 )
     
