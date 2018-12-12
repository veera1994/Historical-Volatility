#Term Project - Fall 2017
#Historical Volatiltiy of Stock
#Authors: Abhishek Kishore and Veeraraghavan Veerasubramanian


library(quantmod)
library(anytime)
library(RSQLite)
library(dplyr)
library(readr)
setwd("G:/Engineering Management/CourseWork/CSR/Project")
Nasdaq_company_list <- read_csv("G:/Engineering Management/CourseWork/CSR/Project/Nasdaq company list.csv")

#Function to get the daily stock prices for a given company as an XTS object
get_price_history <- function(Ticker){
  
  Ticker <- getSymbols(Symbols = Ticker,
                       from="2007-01-01",
                       to="2017-12-01",
                       env=parent.frame(),
                       reload.Symbols = FALSE,
                       verbose = FALSE,
                       warnings = TRUE,
                       src = "google",
                       symbol.lookup = TRUE,
                       auto.assign = getOption('getSymbols.auto.assign',TRUE))
  
  
}

#Creates a new database stockdata in sqlite environment
stockdata<-dbConnect(SQLite(),dbname="x.sqlite")


#Creates a new table Nasdaq_Company_List in stockdata database
dbSendQuery(conn=stockdata,"create table Nasdaq_Company_List(
            Symbol text PRIMARY KEY ,
            Name text,
            MarketCap text,
            IPOYear text,
            Sector text,
            Industry text,
            SummaryQuote text)
            without rowid")

#Uploads the csv containg data regarding 3500 companies under Nasdaq Index
dbWriteTable(conn=stockdata,name="Nasdaq_Company_List",value =Nasdaq_company_list ,row.names=F,header=T,overwrite=T)

#Creates a new table HistoricalVolatility to store the calculated Historical Volatility of a company's stock
#for a period of 2007-2017
dbSendQuery(conn=stockdata,"create table HistoricalVolatility(
            Symbol text,
            HV_2007 num,
            HV_2008 num,
            HV_2009 num,
            HV_2010 num,
            HV_2011 num,
            HV_2012 num,
            HV_2013 num,
            HV_2014 num,
            HV_2015 num,
            HV_2016 num,
            HV_2017 num,
            foreign key(Symbol) references Nasdaq_Company_List(Symbol))")

#Function to convert the XTS object into a csv file and load the csv file as a data frame into R environment
get_dataframe <- function(name)
{
  df.name <- deparse(substitute(name))
  write.zoo(name,paste(df.name,".csv",sep = ""),sep = ",",index.name = "Date")
  
  temp = list.files(pattern="*.csv")
  list2env(
    lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
           read.csv), envir = .GlobalEnv)
  
}

#Function to upload the dataframe into stockdata database as an Table
upload_db <- function(name){
  
  df.name <- deparse(substitute(name))
  dbWriteTable(conn=stockdata,name=df.name,value = name,row.names=F,header=T,overwrite=T)
  
}

#Function to retrieve the data from database and to calculate the Historical Volatility for every year 
#from 2007 to 2017.This function also uploads the calculated Volatility into HistoricalVolatility table.
HV_stock <- function(name){
  
  sqlStatement <- paste("select * from",name)
  a <- dbGetQuery(stockdata, sqlStatement)
  
  a[[1]] <- anytime(a[[1]])
  a[[1]] <- as.Date(a[[1]])
  
  symbol <- deparse(substitute(name))
  
  b1 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2007)
  c1 <- sd((b1[[5]] /lag(b1[[5]], default=first(b1[[5]])))-1)*sqrt(252)
  HV_2007 <- round(c1,4)
  print(paste0("The volatility for Year 2007:",HV_2007))
  
  b2 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2008)
  c2 <- sd((b2[[5]] /lag(b2[[5]], default=first(b2[[5]])))-1)*sqrt(252)
  HV_2008 <- round(c2,4)
  print(paste0("The volatility for Year 2008:",HV_2008))
  
  b3 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2009)
  c3 <- sd((b3[[5]] /lag(b3[[5]], default=first(b3[[5]])))-1)*sqrt(252)
  HV_2009 <- round(c3,4)
  print(paste0("The volatility for Year 2009:",HV_2009))
  
  b4 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2010)
  c4 <- sd((b4[[5]] /lag(b4[[5]], default=first(b4[[5]])))-1)*sqrt(252)
  HV_2010 <- round(c4,4)
  print(paste0("The volatility for Year 2010:",HV_2010))
  
  b5 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2011)
  c5 <- sd((b5[[5]] /lag(b5[[5]], default=first(b5[[5]])))-1)*sqrt(252)
  HV_2011 <- round(c5,4)
  print(paste0("The volatility for Year 2011:",HV_2011))
  
  b6 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2012)
  c6 <- sd((b6[[5]] /lag(b6[[5]], default=first(b6[[5]])))-1)*sqrt(252)
  HV_2012 <- round(c6,4)
  print(paste0("The volatility for Year 2012:",HV_2012))
  
  b7 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2013)
  c7 <- sd((b7[[5]] /lag(b7[[5]], default=first(b7[[5]])))-1)*sqrt(252)
  HV_2013 <- round(c7,4)
  print(paste0("The volatility for Year 2013:",HV_2013))
  
  b8 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2014)
  c8 <- sd((b8[[5]] /lag(b8[[5]], default=first(b8[[5]])))-1)*sqrt(252)
  HV_2014 <- round(c8,4)
  print(paste0("The volatility for Year 2014:",HV_2014))
  
  b9 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2015)
  c9 <- sd((b9[[5]] /lag(b9[[5]], default=first(b9[[5]])))-1)*sqrt(252)
  HV_2015 <- round(c9,4)
  print(paste0("The volatility for Year 2015:",HV_2015))
  
  b10 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2016)
  c10 <- sd((b10[[5]] /lag(b10[[5]], default=first(b10[[5]])))-1)*sqrt(252)
  HV_2016 <- round(c10,4)
  print(paste0("The volatility for Year 2016:",HV_2016))
  
  b11 <- subset(a, format(as.Date(a[[1]]),"%Y")== 2017)
  c11 <- sd((b11[[5]] /lag(b11[[5]], default=first(b11[[5]])))-1)*sqrt(252)
  HV_2017 <- round(c11,4)
  print(paste0("The volatility for Year 2017:",HV_2017))

  df=data.frame(symbol,HV_2007,HV_2008,HV_2009,HV_2010,HV_2011,HV_2012,HV_2013,HV_2014,HV_2015,HV_2016,HV_2017)
  dbWriteTable(conn=stockdata,name="HistoricalVolatility",value = df,row.names=F,header=T,overwrite=F,append=T)
  
}

#Final 4 functions:
#Gets the daily prices for past decade as an XTS object
get_price_history("GOOGL")
#Converts XTS to csv file and imports csv into R as data frame
get_dataframe(GOOGL)
#Uploads the csv file into database as a table
upload_db(GOOGL)
#Calculated the Historical Volatility for the stock
HV_stock("GOOGL")
#To cross verifu our Funtion
dbListTables(stockdata)
View(dbReadTable(stockdata,"Historicalvolatility"))

#Our project works for over 3000 company tickers. Try using "AMZN","FB","AMD","MSFT"




























