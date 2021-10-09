library(tidyverse)
library(Metrics)
library(leaps)
library(randomForest)

list.files('Data',recursive = T,full.names = T)



bike_rentals_df <- read_csv("Data/Bike-Sharing-Dataset/hour.csv")

sample_n(bike_rentals_df, 10) %>% View()

summary(bike_rentals_df)
glimpse(bike_rentals_df)

# 1
datenum_parser <- function(x){
  dates = rep(NA, length(x))
  datenum = 1
  for(i in unique(x)){
    dates[x==i] = datenum
    datenum = datenum + 1
  }
  
  return(dates)
}


bike_rentals_df$datenum <- datenum_parser(bike_rentals_df$dteday)

bike_clean_df <- bike_rentals_df %>% 
  mutate(season = as.factor(season),
         weekday = as.factor(weekday),
         weathersit = as.factor(weathersit)) %>% 
  select(-c(instant, casual, registered,dteday, workingday)) 


bike_clean_df %>% glimpse()



# 2
quancut <- quantile(bike_clean_df$datenum,.8)

bike_train <- bike_clean_df %>% filter(datenum <= quancut)

bike_test <- bike_clean_df %>% filter(datenum > quancut)


# 3

bss <- regsubsets(cnt~., bike_train , method = 'exhaustive', nvmax = 21)

summary(bss) 
summary(bss)$adjr2 %>% which.max()
summary(bss)$adjr2[summary(bss)$adjr2 %>% which.max()]
summary(bss)$bic %>% which.min()
summary(bss)$bic[summary(bss)$bic %>% which.min()]


fwd <- regsubsets(cnt~., bike_train , method = 'forward', nvmax = 21)

summary(fwd) 
summary(fwd)$adjr2 %>% which.max()
summary(fwd)$adjr2[summary(fwd)$adjr2 %>% which.max()]
summary(fwd)$bic %>% which.min()
summary(fwd)$bic[summary(fwd)$bic %>% which.min()]

bwd <- regsubsets(cnt~., bike_train , method = 'forward', nvmax = 21)

summary(bwd) 
summary(bwd)$adjr2 %>% which.max()
summary(bwd)$adjr2[summary(bwd)$adjr2 %>% which.max()]
summary(bwd)$bic %>% which.min()
summary(bwd)$bic[summary(bwd)$bic %>% which.min()]

seqrep <- regsubsets(cnt~., bike_train , method = 'seqrep', nvmax = 21)

summary(seqrep) 
summary(seqrep)$adjr2 %>% which.max()
summary(seqrep)$adjr2[summary(seqrep)$adjr2 %>% which.max()]
summary(seqrep)$bic %>% which.min()
summary(seqrep)$bic[summary(seqrep)$bic %>% which.min()]


# 4
summary(lm(cnt~.,data = bike_rentals_df %>% select(-c(casual, registered,dteday))))

# 5

lmodel <- lm(cnt ~. -datenum, data = bike_train)
summary(lmodel)

preds <- predict(lmodel, bike_test)

rmse(bike_test$cnt, preds)


data_frame(y = bike_test$cnt,
           yhat = preds) %>% 
  ggplot()+
  geom_point(aes(x = y, y = yhat), alpha = .1)+
  theme_bw()


# 6
set.seed(0)
rf_model <- randomForest(cnt ~., data = bike_train, ntree = 100)
preds_rf <- predict(rf_model, bike_test)

rmse(bike_test$cnt, preds_rf)

data_frame(y = bike_test$cnt,
           yhat = preds_rf) %>% 
  ggplot()+
  geom_point(aes(x = y, y = yhat), alpha = .1)+
  theme_bw()
