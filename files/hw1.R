## ----setup, include=FALSE-----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- warning=FALSE, message=FALSE, error=FALSE, results='hide'---------------------------------------------------------
library(EVDS)
library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)
Sys.setlocale("LC_TIME", "English")


## ---- include=FALSE, warning=FALSE--------------------------------------------------------------------------------------
set_evds_key("mKHU7opk2l")


## -----------------------------------------------------------------------------------------------------------------------
df <- get_series(series = c("TP.TG2.Y17", "TP.UR.S08","TP.KTF11", "TP.DK.USD.A.YTL"),start_date = "01-01-2012", end_date = "01-02-2021")

df1 <- df$items %>%
  mutate(Date=as.yearmon(Tarih, "%Y-%m"),prob_of_buying_car=as.numeric(TP_TG2_Y17),total_automobile_production=as.numeric(TP_UR_S08), interest_rate_for_vehicle_loan=as.numeric(TP_KTF11), USD_TRY=as.numeric(TP_DK_USD_A_YTL) )%>%
  select(Date:USD_TRY)



## -----------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/Asus/Desktop/Spring 2021/IE 360/HW1")
gt <- read.csv("arabafiyat.csv", row.names = NULL)
gt <- as.data.frame(gt)
gt <- gt[-1, ]
gt1 <- gt %>%
  mutate(Date=as.yearmon(row.names), interest_over_time=as.numeric(Kategori..TÃ.m.kategoriler)) %>%
  select(Date:interest_over_time)

arackredi <- read.csv("arackredi.csv", row.names = NULL)
arackredi <- as.data.frame(arackredi)
arackredi <- arackredi[-1, ]

gt2 <- arackredi %>%
  mutate(Date=as.yearmon(row.names), interest_over_time=as.numeric(Kategori..TÃ.m.kategoriler)) %>%
  select(Date:interest_over_time)

turk_uretimi_araba <- read.csv("turk_uretimi_araba.csv", row.names = NULL)
turk_uretimi_araba <- as.data.frame(turk_uretimi_araba)
turk_uretimi_araba <- turk_uretimi_araba[-1, ]

gt3 <- turk_uretimi_araba %>%
  mutate(Date=as.yearmon(row.names), interest_over_time=as.numeric(Kategori..TÃ.m.kategoriler)) %>%
  select(Date:interest_over_time)

USD_TRY <- read.csv("USD_TRY.csv", row.names = NULL)
USD_TRY <- as.data.frame(USD_TRY)
USD_TRY <- USD_TRY[-1, ]

gt4 <- USD_TRY %>%
  mutate(Date=as.yearmon(row.names), interest_over_time=as.numeric(Kategori..TÃ.m.kategoriler)) %>%
  select(Date:interest_over_time)



## -----------------------------------------------------------------------------------------------------------------------
merged <- df1 %>%
  inner_join(gt1, by='Date') %>%
  inner_join(gt2, by='Date', suffix=c("","_arac_kredi")) %>%
  inner_join(gt3, by='Date', suffix=c("","_turk_uretimi_araba")) %>%
  inner_join(gt4, by='Date', suffix=c("","_USD_TRY")) 

names(merged)[6]="interest_over_time_araba_fiyat"


## -----------------------------------------------------------------------------------------------------------------------
ggplot(df1, aes(x=Date,y=prob_of_buying_car))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Probability of buying a car (%)", title = "EVDS - The probability of buying a car (over the next 12 months)")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,20,by=1))+
  geom_point(color="cornflowerblue")


## -----------------------------------------------------------------------------------------------------------------------

ggplot(gt1, aes(x=Date,y=interest_over_time))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Interest", title = "Google Trends - 'Araba Fiyat' Search Interest Over Time")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  geom_point(color="coral")


## -----------------------------------------------------------------------------------------------------------------------
ggplot(merged)+
  geom_point(aes(x=prob_of_buying_car,y=interest_over_time_araba_fiyat, color=as.Date(Date)))+
  theme_minimal()+
  labs(x="Probability of buying a car (%)", y="'Araba Fiyat' Search interest over time", color="Date")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(df1, aes(x=Date,y=total_automobile_production))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Total automobile production", title="EVDS - Total Automobile Production")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks = seq(0, 120000, by = 10000))+
  geom_point(color="cornflowerblue")


## -----------------------------------------------------------------------------------------------------------------------

ggplot(gt3, aes(x=Date,y=interest_over_time))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Interest", title = "Google Trends - 'Turk Uretimi Araba' Search Interest Over Time")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  geom_point(color="coral")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(merged)+
  geom_point(aes(x=total_automobile_production,y=interest_over_time_turk_uretimi_araba, color=as.Date(Date)))+
  theme_minimal()+
  labs(x="Total automobile procution", y="'Turk Uretimi Araba' Search interest over time", color="Date")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(df1, aes(x=Date,y=interest_rate_for_vehicle_loan))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Interest rate for vehicle loans (%)", title="EVDS - Interest Rate for Vehicle Loans")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,40,by=2))+
  geom_point(color="cornflowerblue")


## -----------------------------------------------------------------------------------------------------------------------

ggplot(gt2, aes(x=Date,y=interest_over_time))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Interest", title = "Google Trends - 'Arac Kredi' Search Interest Over Time")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  geom_point(color="coral")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(merged)+
  geom_point(aes(x=interest_rate_for_vehicle_loan,y=interest_over_time_arac_kredi, color=as.Date(Date)))+
  theme_minimal()+
  labs(x="Interest rate for vehicle loans (%)", y="'Arac Kredi' Search interest over time", color="Date")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(df1, aes(x=Date,y=USD_TRY))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="USD TRY exchange rate", title = "EVDS - US Dollar Exchange Rate")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,10,by=1))+
  geom_point(color="cornflowerblue")



## -----------------------------------------------------------------------------------------------------------------------

ggplot(gt4, aes(x=Date,y=interest_over_time))+
  geom_line()+
  theme_minimal()+
  labs(x="Date", y="Interest", title = "Google Trends - 'USD TRY' Search Interest Over Time")+
  theme(axis.text.x = element_text(angle=45,size=7))+
  scale_x_yearmon(n=18)+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  geom_point(color="coral")



## -----------------------------------------------------------------------------------------------------------------------
ggplot(merged)+
  geom_point(aes(x=USD_TRY,y=interest_over_time_USD_TRY, color=as.Date(Date)))+
  theme_minimal()+
  labs(x="USD TRY rate", y="'USD TRY' Search interest over time", color="Date")



## -----------------------------------------------------------------------------------------------------------------------

M <- cor(df1[2:5])
corrplot::corrplot(M)


