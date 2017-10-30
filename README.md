# TDI
#attempting to display some plots that I have created
---------------
output: rmarkdown::github_document
-----------------
Excursion<-function()
{
  #Get File from yahoo finance, with dates 1993-01-29 to present
  getSymbols("SPY", src = 'yahoo', from = as.Date("1993-01-29"), to = as.Date("2017-10-27"))
  TICKER1<-data.frame(SPY)
  row.names(TICKER1)->Dates
  Dates<-data.frame(Dates)
  TICKER1<-cbind(TICKER1,Dates)
  TICKER1$Dates<-as.Date(TICKER1$Dates)
  TICKER1<-TICKER1[c(7,1,2,3,5,6,4)]
  
  #Calulate Percent Change 1 Day Backward so today's close has moved x percent from yesterdays close
  i<-c(1:nrow(TICKER1)-1) #so subtract 1 from each integer of sequence, it will now start at 0
  "B.1.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0) #if x = 0 or less, that means backwards percent change not possible
    {
      c(NA)->B.1.Day.Percent[x+1,1] #print NA in that spot of the column
    }
    else #take x (which is really X-1) close and subtract x+1 (which is really x) close, etc and place at x+1 (which is really x) spot
    {
      (((TICKER1[x+1,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.1.Day.Percent[x+1,1] 
    }
  }
  #Calulate Percent Change 5 Day Backward todays close has moved x percent from 5 days ago
  i<-c(1:nrow(TICKER1)-5)
  "B.5.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.5.Day.Percent[x+5,1] 
    }
    else
    {
      (((TICKER1[x+5,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.5.Day.Percent[x+5,1]
    }
  }
  #Calulate Percent Change 10 Day Backward
  i<-c(1:nrow(TICKER1)-10)
  "B.10.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.10.Day.Percent[x+10,1] 
    }
    else
    {
      (((TICKER1[x+10,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.10.Day.Percent[x+10,1]
    }
  }
  #Calulate Percent Change 20 Day Backward
  i<-c(1:nrow(TICKER1)-20) 
  "B.20.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0) 
    {
      c(NA)->B.20.Day.Percent[x+20,1] 
    }
    else
    {
      (((TICKER1[x+20,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.20.Day.Percent[x+20,1]
    }
  }
  #Calulate Percent Change 30 Day Backward
  i<-c(1:nrow(TICKER1)-30)
  "B.30.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.30.Day.Percent[x+30,1] 
    }
    else
    {
      (((TICKER1[x+30,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.30.Day.Percent[x+30,1]
    }
  }
  #Calulate Percent Change 45 Day Backward
  i<-c(1:nrow(TICKER1)-45)
  "B.45.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.45.Day.Percent[x+30,1] 
    }
    else
    {
      (((TICKER1[x+45,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.45.Day.Percent[x+45,1]
    }
  }
  
  #Calulate Percent Change 60 Day Backward
  i<-c(1:nrow(TICKER1)-60)
  "B.60.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.60.Day.Percent[x+60,1] 
    }
    else
    {
      (((TICKER1[x+60,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.60.Day.Percent[x+60,1]
    }
  }
  
  
  #Calulate Percent Change 120 Day Backward
  i<-c(1:nrow(TICKER1)-120)
  "B.120.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.120.Day.Percent[x+120,1] 
    }
    else
    {
      (((TICKER1[x+120,7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.120.Day.Percent[x+120,1]
    }
  }
  
  #calculate Back 252 trading days
  i<-c(1:nrow(TICKER1)-252)
  "B.252.Day.Percent"<-data.frame(1)
  for (x in i)
  {
    if (x <= 0)
    {
      c(NA)->B.252.Day.Percent[x+252,1] 
    }
    else
    {
      (((TICKER1[(x+252),7]-TICKER1[x,7])/TICKER1[x,7])*100)->B.252.Day.Percent[x+252,1]
    }
  }
  
  #Bind Everything
  library(gdata)
  cbindX(TICKER1, B.1.Day.Percent, B.5.Day.Percent, B.10.Day.Percent, B.20.Day.Percent, B.30.Day.Percent, B.45.Day.Percent, B.60.Day.Percent, B.120.Day.Percent, B.252.Day.Percent)->TICKER1
  colnames(TICKER1)<-c("Dates", "Open", "High", "Low", "Volume", "Adjusted", "Close", "B.1.Day.Percent", "B.5.Day.Percent", "B.10.Day.Percent", "B.20.Day.Percent", "B.30.Day.Percent", "B.45.Day.Percent", "B.60.Day.Percent", "B.120.Day.Percent", "Percent252")
  return(TICKER1)
}

####The previous part created a df that contained percent movement
#now identify -10 or greater movement, get the date and stock price at that time,
max(TICKER1$B.1.Day.Percent, na.rm = TRUE)
min(TICKER1$B.1.Day.Percent, na.rm = TRUE)

#skip B.1. because nothing happens min is less than 10%
#start with B.5

B.5.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,9])==FALSE) && (TICKER1[x,9]<=(-10)))
    {
      StockPrice<-TICKER1[x,7]
      EventDate<-TICKER1[x,1]
      #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
      ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
      ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
      TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
      temp<-c(as.Date(as.character(EventDate)), StockPrice, as.Date(as.character(ReturnDate)), ReturnStockPrice, TimeToReturn)
      rbind(B.5.Movement,temp)->B.5.Movement
    }
}
names(B.5.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.5.Movement[,1]<-as.Date(B.5.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.5.Movement[,3]<-as.Date(B.5.Movement[,3], "%Y-%m-%d", origin="1970-01-01")
#start with B.10

B.10.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,10])==FALSE) && (TICKER1[x,10]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate))) #Trading Days
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.10.Movement,temp)->B.10.Movement
  }
}
names(B.10.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.10.Movement[,1]<-as.Date(B.10.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.10.Movement[,3]<-as.Date(B.10.Movement[,3], "%Y-%m-%d", origin="1970-01-01")
#start with B.20

B.20.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,11])==FALSE) && (TICKER1[x,11]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.20.Movement,temp)->B.20.Movement
  }
}
names(B.20.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.20.Movement[,1]<-as.Date(B.20.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.20.Movement[,3]<-as.Date(B.20.Movement[,3], "%Y-%m-%d", origin="1970-01-01")
#start with B.30

B.30.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,12])==FALSE) && (TICKER1[x,12]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.30.Movement,temp)->B.30.Movement
  }
}
names(B.30.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.30.Movement[,1]<-as.Date(B.30.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.30.Movement[,3]<-as.Date(B.30.Movement[,3], "%Y-%m-%d", origin="1970-01-01")


#start with B.45

B.45.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,13])==FALSE) && (TICKER1[x,13]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.45.Movement,temp)->B.45.Movement
  }
}
names(B.45.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.45.Movement[,1]<-as.Date(B.45.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.45.Movement[,3]<-as.Date(B.45.Movement[,3], "%Y-%m-%d", origin="1970-01-01")

#start with B.60

B.60.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,14])==FALSE) && (TICKER1[x,14]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.60.Movement,temp)->B.60.Movement
  }
}
names(B.60.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.60.Movement[,1]<-as.Date(B.60.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.60.Movement[,3]<-as.Date(B.60.Movement[,3], "%Y-%m-%d", origin="1970-01-01")

#start with B.120

B.120.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,15])==FALSE) && (TICKER1[x,15]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.120.Movement,temp)->B.120.Movement
  }
}
names(B.120.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.120.Movement[,1]<-as.Date(B.120.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.120.Movement[,3]<-as.Date(B.120.Movement[,3], "%Y-%m-%d", origin="1970-01-01")

#start with B.252

B.252.Movement<-data.frame()
i<-(1:nrow(TICKER1))
for (x in i)
{
  if ((is.na(TICKER1[x,16])==FALSE) && (TICKER1[x,16]<=(-10)))
  {
    StockPrice<-TICKER1[x,7]
    EventDate<-TICKER1[x,1]
    #Find First date it returns to StockPriceX1.15  after Event Date->ReturnDate
    ReturnDate<-TICKER1$Dates[TICKER1$Close >= (1.15 * StockPrice) & TICKER1$Dates > EventDate] [1]
    ReturnStockPrice<-TICKER1$Close[TICKER1$Dates == ReturnDate]
    TimeToReturn<-((which(TICKER1$Dates == ReturnDate))-(which(TICKER1$Dates == EventDate)))
    temp<-c(as.Date(EventDate), StockPrice, as.Date(ReturnDate), ReturnStockPrice, TimeToReturn)
    rbind(B.252.Movement,temp)->B.252.Movement
  }
}
names(B.252.Movement)<-c("EventDate", "StockPrice", "ReturnDate", "ReturnStockPrice", "TradingDaysToReturn")
B.252.Movement[,1]<-as.Date(B.252.Movement[,1], "%Y-%m-%d", origin="1970-01-01")
B.252.Movement[,3]<-as.Date(B.252.Movement[,3], "%Y-%m-%d", origin="1970-01-01")

hist(B.45.Movement$TradingDaysToReturn) #right skewed
 hist(B.252.Movement$TradingDaysToReturn) #bimodal and skewed to right 
