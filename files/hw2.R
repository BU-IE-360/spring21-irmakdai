## ---- warning=FALSE, message=FALSE, error=FALSE, results='hide'----------------------------------------------------------------------------
library(EVDS)
library(zoo)
library(lubridate)
library(dplyr)
library(ggplot2)
library(forecast)
library(GGally)
library(data.table)
Sys.setlocale("LC_TIME", "English")


## ---- include=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------
set_evds_key("mKHU7opk2l")


## ------------------------------------------------------------------------------------------------------------------------------------------
data <- get_series(series = c("TP.FG.J02","TP.TG2.Y04","TP.TG2.Y11","TP.DK.USD.A.YTL","TP.ITHALATBEC.121","TP.ITHALATBEC.111"), start_date = "01-01-2013", end_date = "01-03-2021")
df <- data$items[,-8]
df1 <- df %>%
  mutate(Date=as.yearmon(Tarih, "%Y-%m"),
         cpi_alcohol_tobacco=as.numeric(TP_FG_J02),
         general_economic_situation=as.numeric(TP_TG2_Y04), 
         USD_TRY=as.numeric(TP_DK_USD_A_YTL), 
         import_processed_food_beverage=as.numeric(TP_ITHALATBEC_121), 
         import_unprocessed_food_beverage=as.numeric(TP_ITHALATBEC_111)) %>%
  select(Date:import_unprocessed_food_beverage)


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
ggplot(df1, aes(x=Date,y=cpi_alcohol_tobacco))+
  geom_line()+
  theme_minimal()+
  labs(y="CPI for alcoholic beverages and tobacco", title = "Monthly CPI for Alcoholic Beverages and Tobacco")+
  scale_x_yearmon(n=10)+
  geom_smooth()


## ----warning=FALSE, message=FALSE----------------------------------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(log_target=log(cpi_alcohol_tobacco))

ggplot(df1, aes(x=Date,y=log_target))+
  geom_line()+
  theme_minimal()+
  labs(y="log(CPI) for alcoholic beverages and tobacco", title = "Monthly log(CPI) for Alcoholic Beverages and Tobacco")+
  scale_x_yearmon(n=10)+
  geom_smooth()



## ------------------------------------------------------------------------------------------------------------------------------------------
ggpairs(df1, columns = c(7,3:6))


## ------------------------------------------------------------------------------------------------------------------------------------------
ts <- ts(df1[,c(7,3:6)], start=c(2013,1), frequency=12)
plot(ts, cex.lab=0.5, main="Time Series")



## ------------------------------------------------------------------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(trend=c(1:nrow(df1)))

model1 <- lm(log_target ~ trend, df1)
summary(model1)
checkresiduals(model1)


## ------------------------------------------------------------------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate( month=month(Date))
model2 <- lm(log_target~+trend+as.factor(month), df1)
summary(model2)
checkresiduals(model2)
ggplot(df1)+
  geom_line(aes(x=Date, y=log_target))+
  geom_line(aes(x=Date, y=predict(model2, df1)),color="red")+
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------------------------------
acf( df1$log_target)


## ------------------------------------------------------------------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(lag1=lag(log_target,1),
         lag3=lag(log_target,3),
         lag6=lag(log_target,6),
         lag12=lag(log_target,12))

model3 <- lm(log_target~trend+as.factor(month)+lag1+lag3+lag6+lag12, df1)
summary(model3)
checkresiduals(model3)



## ------------------------------------------------------------------------------------------------------------------------------------------
df1 <- subset(df1, select=-c(lag3,lag6,lag12))
model4 <- lm(log_target~.-Date-cpi_alcohol_tobacco-month+as.factor(month), df1)
summary(model4)
checkresiduals(model4)


## ---- warning=FALSE, message=FALSE---------------------------------------------------------------------------------------------------------
ggplot(df1)+
  geom_line(aes(x=Date, y=log(cpi_alcohol_tobacco)))+
  geom_line(aes(x=Date, y=c(NA,fitted(model4))),color="red")+
  theme_minimal()+
  scale_x_yearmon(n=10)



## ---- message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------
df1 <- df1 %>%
  mutate(fitted=c(NA, fitted(model4)),
          residual=c(NA,residuals(model4)))

ggplot(df1, aes(x=fitted, y=residual))+
  geom_point()+
  geom_smooth()


## ------------------------------------------------------------------------------------------------------------------------------------------


data <- get_series(series = c("TP.FG.J02","TP.TG2.Y04","TP.TG2.Y11","TP.DK.USD.A.YTL","TP.ITHALATBEC.121","TP.ITHALATBEC.111"), start_date = "01-04-2021", end_date = "01-04-2021")
df <- data$items[,-8]
df2 <- df %>%
  mutate(Date=as.yearmon(Tarih, "%Y-%m"),
         cpi_alcohol_tobacco=as.numeric(TP_FG_J02),
         general_economic_situation=as.numeric(TP_TG2_Y04), 
         USD_TRY=as.numeric(TP_DK_USD_A_YTL), 
         import_processed_food_beverage=as.numeric(TP_ITHALATBEC_121), 
         import_unprocessed_food_beverage=as.numeric(TP_ITHALATBEC_111)) %>%
  select(Date:import_unprocessed_food_beverage)

fc1 <-forecast(df1$import_processed_food_beverage)
fc2 <-forecast(df1$import_unprocessed_food_beverage)
df2 <- df2%>%
  mutate(
    log_target=NA,
    trend=100,
    month=4,
    lag1=6.779547,
    import_processed_food_beverage=fc1$mean[1],
    import_unprocessed_food_beverage=fc2$mean[1]
  )

exp(predict(model4, newdata = df2))

