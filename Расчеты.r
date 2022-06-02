install.packages("car")
install.packages("foreign")
install.packages("readstata13")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stargazer")
install.packages("lmtest")
install.packages("pROC")
install.packages("rms")
install.packages("ResourceSelection")
install.packages("openxlsx")
install.packages("haven")
install.packages("pscl") 
install.packages("MASS") 
install.packages("InformationValue") 
install.packages("caret") 
install.packages("MuMIn") 
install.packages("aod")
install.packages("ggplot2")
install.packages("skedastic")
install.packages("ggcorrplot")
install.packages("memisc")

library(skedastic)
library(PRROC) 
library(ROCR) 
library(aod)
library(ggplot2)
library(MuMIn) 
library(MASS) 
library(caret) 
library(pscl) 
library(InformationValue) 
library(car)
library(foreign)
library(readstata13)
library(ggplot2)
library(dplyr)
перlibrary(stargazer)
library(lmtest)
library(pROC)
library(rms)
library(ResourceSelection)
library(openxlsx)
library(sandwich)
library(lmtest)
library(haven)
library(ggcorrplot)
library(memisc)

D <- read.xlsx('DATA.xlsx')
data<-na.omit(D)
glimpse(data) 


#Фиктивные переменные сезонов
data$d_2017 <- ifelse(data$Season == 2017, 1, 0)
data$d_2018 <- ifelse(data$Season == 2018, 1, 0)
data$d_2019 <- ifelse(data$Season == 2019, 1, 0)
data$d_2020 <- ifelse(data$Season == 2020, 1, 0)

data <- subset(data, select=-Season)


#Пересчитываем PTS, FGM, FGA, 3PM, 3PA, FTM, FTA, OFF, DEF, TRB, AST, STL, BLK, TOV, PF в среднем на 40 минут
stats_to_be_adjusted = c("PTS", "FGM", "FGA", "3PM", "3PA", "FTM", "FTA", 
                         "OFF", "DEF", "TRB", "AST", "STL", "BLK", "TOV", "PF")

data <- data %>%
  mutate(
    across(stats_to_be_adjusted,
           .fns = ~./MIN*40))

#убираем неполные наблюдения
data<-na.omit(data) 

#убираем из выборки игроков, у которых неизвестен возраст
data <- filter(data, Age != 0)
View(data)

#Корреляционная матрица
cor(data[,4:36])

#Корреляционная матрица (более наглядная)
corr <- round(cor(data[,4:36]), 2) 
p.mat <- round(cor_pmat(data[,4:36]), 2) 

ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Корреляция характеристик", 
           ggtheme=theme_bw, 
           p.mat = p.mat)



#Описательные статистики
stargazer(data, type="text", median=TRUE,
          digits=2, title="Data")









#Логит-модель
model1 <- glm(Drafted ~ . - Name - Team - Center - OFF - DEF,
              data=data, family = binomial)
summary(model1)

#удаление лишних переменных по критерию Акаике
mod1 <- stepAIC(model1)
summary(mod1)

#псевдо - R^2
pr2_1 <- pR2(mod1)
pr2_1

#робастные ошибки
sandwich1 <- function(mod1) sandwich(mod1) * nobs(mod1) / (nobs(mod1) - 1)
coeftest(mod1, vcov = sandwich1)

#исследуем мультиколлинеарность в данных
vif(mod1) 


#LR-тест
mod1_red_pos <- update(mod1, .~. - Guard - Forward)
lrtest(mod1, mod1_red_pos)

mod1_red_season <- update(mod1, .~. - d_2017 - d_2018 - d_2019 - d_2020)
lrtest(mod1, mod1_red_season)


# Верно и неверно классифицированные наблюдения 
hitmiss(mod1, k=0.025) #0.025 - пороговый уровень вероятности, с которого классифицируем прогноз как единицу

#построение ROC-кривой
pr1 <- predict(mod1, data, type="response")
pred1 <- prediction(pr1, data$Drafted)
perf1 <- performance(pred1,"tpr","fpr")
plot(perf1,colorize=TRUE)


#второй тип графика, используем, чтобы получить значение AUC 
PRROC_obj1 <- roc.curve(scores.class0 = pr1, weights.class0=data$Drafted, curve=TRUE)
plot(PRROC_obj1)






#Пробит-модель
model2 <- glm(Drafted ~ . - Name - Team - Center - OFF - DEF,
              data=data, family = binomial(link = "probit"))

#удаление лишних переменных по критерию Акаике
mod2 <- stepAIC(model2)
summary(mod2)

#псевдо - R^2
pr2_2 <- pR2(mod2)
pr2_2

#робастные ошибки
sandwich2 <- function(mod2) sandwich(mod2) * nobs(mod2) / (nobs(mod2) - 1)
coeftest(mod2, vcov = sandwich2)

#исследуем мультиколлинеарность в данных
vif(mod2) 

#LR-тест
mod2_red_pos <- update(mod2, .~. - Guard - Forward)
lrtest(mod2, mod2_red_pos)

mod2_red_season <- update(mod2, .~. - d_2017 - d_2018 - d_2019 - d_2020)
lrtest(mod2, mod2_red_season)

# Верно и неверно классифицированные наблюдения 
hitmiss(mod2, k=0.0275) #0.0275 - пороговый уровень вероятности, с которого классифицируем прогноз как единицу

#построение ROC-кривой
pr2 <- predict(mod2, data, type="response")
pred2 <- prediction(pr2, data$Drafted)
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2,colorize=TRUE)


#второй тип графика, используем, чтобы получить значение AUC 
PRROC_obj2 <- roc.curve(scores.class0 = pr2, weights.class0=data$Drafted, curve=TRUE)
plot(PRROC_obj2)








#Линейно-вероятностная модель
model3 <- lm(Drafted ~ . - Name - Team - Center - OFF - DEF,
             data=data)
summary(model3)

#Уберём незначимые переменные
model3 <- lm(Drafted ~ . - Name - Team - Center - OFF - DEF-Weight-GP-PTS-FGM-FTM-TOV-d_2017-d_2018-d_2019-d_2020-Forward-`FG%`-`3PM`-`3PA`-`3P%`-`FT%`-STL,
             data=data)
summary(model3)

#уберем лишние переменные по критерию Акаике
mod3 <- stepAIC(model3)
summary(mod3)

#Тест Уайта
white_lm(model3, interactions = FALSE, statonly = FALSE)

#Робастные ошибки
coeftest(model3, vcov. = vcovHC, type = "HC1")

#Проверим мультиколлинеарность
vif(mod3)

#Тест Рамсея
resettest(model3, power = 2:3, type = c("fitted", "regressor",
                                         "princomp"), data = data, vcov = NULL)




#Сводная таблица
mtable(mod1,mod2, mod3)
stargazer(mod1, mod2, mod3, title="Три модели драфта", type="text", 
          column.labels=c("Логит", "Пробит","Линвер"), 
          df=FALSE, digits=3)
stargazer(mod1, mod2, mod3, type = "html", out="~/Desktop/Mod1r.html")








