## installing required packages
library(Quandl)
library(tidyverse)
library(tidyquant)
library(timetk)
library(forecast)
library(gridExtra)

## Quandl API

Quandl.api_key('uuqx7-4PqVjvykTe-Gmx')

## Loading the dataset

Canara=Quandl("NSE/CANBK",collapse="daily",start_date="2016-09-01",type="raw")
BOB=Quandl("NSE/BANKBARODA",collapse="daily",start_date="2016-09-01",type="raw")
SBI=Quandl("NSE/SBIN",collapse="daily",start_date="2016-09-01",type="raw")
ICICI = Quandl("NSE/ICICIBANK",collapse="daily",start_date="2016-09-01",type="raw")
PNB= Quandl("NSE/PNB",collapse="daily",start_date="2016-09-01",type="raw")
Axis=Quandl("NSE/AXISBANK",collapse="daily",start_date="2016-09-01",type="raw")

ICICI<-cbind(ICICI,Stock="")
PNB<-cbind(PNB,Stock="")
Axis<-cbind(Axis,Stock="")
SBI<-cbind(SBI,Stock="")
Canara<-cbind(Canara,Stock="")
BOB<-cbind(BOB,Stock="")

ICICI$Stock<-paste(ICICI$Stock,"ICICI",sep="")
PNB$Stock<-paste(PNB$Stock,"PNB",sep="")
Axis$Stock<-paste(Axis$Stock,"Axis",sep="")
SBI$Stock<-paste(SBI$Stock,"SBI",sep="")
Canara$Stock<-paste(Canara$Stock,"Canara",sep="")
BOB$Stock<-paste(BOB$Stock,"BOB",sep="")

Master_Data<-rbind(ICICI,PNB,Axis,SBI,Canara,BOB)  

## Coerce the Date variable into as.Date from character

Master_Data$Date<-as.Date(Master_Data$Date)

end<-ymd("2017-09-01")
start<-ymd("2016-09-01")

Master_Data<-Master_Data%>%
  tibble::as_tibble() %>%
  group_by(Stock)

## Visualisation of BBand in ggplot2 

Master_Data%>%filter(Stock=="ICICI"|Stock=="PNB")%>%ggplot(aes(x=Date,y=Close))+
  geom_line(size=1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  facet_wrap(~ Stock, scales = "free_y")+
  labs(title = "Bollinger Band", x = "Date",y="Price") +
  theme(text = element_text(family = 'Gill Sans', color = "#444444",hjust=0.5)
        ,panel.background = element_rect(fill = 'lightyellow')
        ,panel.grid.minor = element_blank(),
        ,panel.grid.major = element_blank()
        ,plot.title = element_text(size = 20,hjust=0.5,colour="orangered4")
        ,axis.title = element_text(size = 18, color = '#555555')
        ,axis.title.y = element_text(hjust=0.5,size=15)
        ,axis.title.x = element_text(hjust = 0.5,size=15)
  ) +
  theme(legend.position="none")

