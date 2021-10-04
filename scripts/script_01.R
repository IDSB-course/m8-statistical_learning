library(tidyverse)
library(moderndive)
library(janitor)
library(Metrics)
library(leaps)


#Regresión Lineal Simple
set.seed(0)

epsilon <- rnorm(n = 100, mean = 0,sd = 15)
x <- runif(100,-50,50)
y <- 0.5 + 2*x + epsilon

cor(x,y)

reglin_df <- data.frame(x=x, y=y)

g <-  reglin_df %>% 
  ggplot(aes(x=x,y=y))+
  geom_point()

g

g+
  geom_smooth(method = 'lm',  se = FALSE)

reglin_model <- lm(y~x, data = reglin_df)

summary(reglin_model)

  
#Regresión Lineal Simple 10000 obs

epsilon <- rnorm(n = 10000, mean = 0,sd = 15)
x <- runif(10000,-50,50)
y <- 0.5 + 2*x + epsilon

cor(x,y)

reglin_df <- data.frame(x=x, y=y)

g <-  reglin_df %>% 
  ggplot(aes(x=x,y=y))+
  geom_point(alpha=.1)

g

g+
  geom_smooth(method = 'lm',  se = FALSE, color = 'red')

reglin_model <- lm(y~x, data = reglin_df)

summary(reglin_model)


machines_df <- read_csv('data/machine.data',
                        col_names = c('vendor', 'model','myct',
                                           'mmin', 'mmax', 'cache',
                                           'chmin','chmax','prp', 'erp'))


#usamos cache para estimar


reglin_model <- lm(prp~cache, machines_df)

reglin_model %>% summary()

ggplot(machines_df, aes(x = cache, y=prp))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)

#vemos mejor regresión lineal simple

formulas <- list(
  as.formula(prp~myct),
  as.formula(prp~mmin),
  as.formula(prp~mmax),
  as.formula(prp~cache),
  as.formula(prp~chmin),
  as.formula(prp~chmax)
)

models <- list()

for(i in 1:length(formulas)){
  model <- lm(formulas[[i]],data = machines_df)
  models[[i]] <- model
  model_summary <- summary(model)
  print(formulas[[i]])
  print(model_summary$r.squared)
  
}



ggplot(machines_df, aes(x = mmax, y=prp))+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE)


models[[3]] %>% summary()



#Variables Binarias

ibm_model <- machines_df %>% 
  mutate(ibm = vendor == 'ibm') %>% 
  lm(prp~ibm, .) 

ibm_model %>% summary()  




machines_df %>% 
  mutate(ibm = vendor == 'ibm') %>% 
  group_by(ibm) %>% 
  summarise(prp = mean(prp)) 
  




#regresión múltiple

# 1er intento

reglin_m_model <- lm(prp ~ ., data = machines_df)
summary(reglin_m_model)

# 2do intento

reglin_m_model <- lm(prp ~ ., data = machines_df %>% select(-model))
summary(reglin_m_model)


# 3er intento

reglin_m_model <- lm(prp ~ ., data = machines_df %>% select(-model, -erp))
summary(reglin_m_model)


#sumemos variables sintéticas

sinth_df <- map_dfc(1:100,~rnorm(209))

reglin_m_model <- lm(prp ~ ., data = machines_df %>%
                       select(-model, -erp) %>% 
                       cbind(sinth_df))
summary(reglin_m_model)

broom::augment(reglin_m_model) %>% 
  ggplot(aes(x = mmax, y=prp))+
  geom_point()+
  geom_line(aes(y = .fitted))
 


#train and validation approach

read_fwf('data/auto-mpg.data')

cars_df <- read_fwf('data/auto-mpg.data',na = '?') %>% 
  mutate(origin = str_sub(X8,1,1),
         car_name = str_sub(X8,4,-2)) %>% 
  select(-X8) %>% 
  set_names(c('mpg', 'cylinders', 'displacement', 'hp', 'weight', 'acceleration',
              'year', 'origin', 'car_name')) 




#train test split

selected_rows <- sample(1:nrow(cars_df), floor(.75*nrow(cars_df)))

cars_train <- cars_df[selected_rows,] %>% filter(complete.cases(.))
cars_test <- cars_df[-selected_rows,] %>% filter(complete.cases(.))


#train model

model_0 <- lm(mpg~hp, cars_train)
model_1 <- lm(mpg~hp+weight, cars_train)
model_2 <- lm(mpg~. , cars_train %>% select(-car_name))


summary(model_0)
summary(model_1)
summary(model_2)

# Predict test

predict(model_0, cars_test)

cars_test$p0 <- predict(model_0, cars_test)
cars_test$p1 <- predict(model_1, cars_test)
cars_test$p2 <- predict(model_2, cars_test)

# RMSE manual way
sqrt(mean((cars_test$p0 - cars_test$mpg) ^ 2, na.rm = T))
sqrt(mean((cars_test$p1 - cars_test$mpg) ^ 2, na.rm = T))
sqrt(mean((cars_test$p2 - cars_test$mpg) ^ 2, na.rm = T))

# RMSE using Metrics Lib

rmse(actual = cars_test$mpg, predicted = cars_test$p0)
rmse(cars_test$mpg,cars_test$p1)
rmse(cars_test$mpg,cars_test$p2)


#Subset Selection

#BSS

bss_selection <- regsubsets(mpg~., cars_train %>% select(-car_name),
                            method = 'exhaustive')

summary(bss_selection)

summary(bss_selection)$adjr2
summary(bss_selection)$adjr2 %>% which.max()
summary(bss_selection)$bic %>% which.min()


#FSS

fss_selection <- regsubsets(mpg~., cars_train %>% select(-car_name),
                            method = 'forward')

summary(fss_selection)

summary(fss_selection)$adjr2
summary(fss_selection)$adjr2 %>% which.max()
summary(fss_selection)$bic %>% which.min()

