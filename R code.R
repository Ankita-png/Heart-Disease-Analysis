# Load required package
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(randomForest)
library(tidyverse)

# Load data
heart <- read.csv(".../heart.csv") %>% rename(.,age = ï..age)
head(heart)

# Checking structure of the data
str(heart)

# Checking missing values
colSums(is.na(heart))

# Data Processing
data <- heart %>% 
  mutate(sex = factor(ifelse(sex == 0,"Female","Male")),
         cp = factor(cp),
         fbs = factor(fbs),
         restecg = factor(restecg),
         exang = factor(exang),
         slope = factor(slope),
         ca = factor(ca),
         thal = factor(thal),
         target = factor(target))

# Checking structure of the data
str(data)

######--------- Analysis 1 ---------#######
ggcorr(heart,label = TRUE,label_size = 3,size = 3,hjust = 0.75,angle = -45,name = "rho") + 
  labs(title = "Correlation Matrix Plot")

# Checking relation of categorical features with target
data %>% select(sex,cp,fbs,restecg,exang,slope,ca,thal,target) %>%
  rename(Sex = sex,Chest_Pain_Type = cp,Fasting_Blood_Sugar = fbs,Resting_ECG = restecg,Exercise_Induced_Angina = exang,Peak_Exercise_ST_segment = slope,Major_vessels = ca,Thalassemia = thal) %>%
  mutate(Chest_Pain_Type = recode_factor(Chest_Pain_Type, `0` = "typical",`1` = "atypical",`2` = "non-angina",`3` = "asymptomatic"),
         Fasting_Blood_Sugar = recode_factor(Fasting_Blood_Sugar, `0` = "<= 120 mg/dl",`1` = "> 120 mg/dl"),
         Resting_ECG = recode_factor(Resting_ECG, `0` = "normal",
                                     `1` = "ST-T abnormality",`2` = "LV hypertrophy"),
         Exercise_Induced_Angina = recode_factor(Exercise_Induced_Angina,`0` = "no",`1` = "yes"),
         Peak_Exercise_ST_segment = recode_factor(Peak_Exercise_ST_segment,`0` = "up-sloping",`1` = "flat",`2` = "down-sloping"),
         Thalassemia = recode_factor(Thalassemia,`1` = "normal",
                                     `2` = "fixed defect",`3` = "reversible defect")) %>%
  gather(key = "key",value = "value",-target) %>%
  ggplot(aes(value)) +
  geom_bar(aes(x = value, 
               fill = target),position = "dodge",width = 0.8) +
  facet_wrap(~ key, scales = "free", nrow = 4) +
  scale_fill_manual(values = c("#fde725ff", "#20a486ff"),
                    name   = "Heart\nDisease",
                    labels = c("No", "Yes")) +
  labs(title = "Effect of Categorical Variables")

######--------- Analysis 2 ---------#######
#*****# Data Partition #*****#
set.seed(13,sample.kind = "Rounding")
index <- createDataPartition(data$target,times = 1,p = 0.2,list = FALSE)
train_set <- data[-index,]
test_set <- data[index,]

#*****# Logistic Regression #*****#
set.seed(12,sample.kind = "Rounding")
model1 <- train(target~.,method = "glm",data = train_set)
pred1 <- predict(model1,test_set)
m1 <- mean(pred1 == test_set$target)*100
# Confusion Matrix
cm1 <- confusionMatrix(factor(pred1),test_set$target)
as.data.frame(cm1$table) %>% ggplot(aes(x = Reference,y = Prediction)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_color_viridis_d(option = "D")

#*****# k-Nearest Neighbors #*****#
set.seed(12,sample.kind = "Rounding")
model2 <- train(target~.,method = "knn",data = train_set)
pred2 <- predict(model2,test_set)
m2 <- mean(pred2 == test_set$target)*100
# Confusion Matrix
cm2 <- confusionMatrix(factor(pred2),test_set$target)
as.data.frame(cm2$table) %>% ggplot(aes(x = Reference,y = Prediction)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_color_viridis_d(option = "D")
  
#*****# Random Forest #*****#
set.seed(12,sample.kind = "Rounding")
model3 <- randomForest(target~.,data = train_set)
pred3 <- predict(model3,test_set)
m3 <- mean(pred3 == test_set$target)*100
# Confusion Matrix
cm3 <- confusionMatrix(factor(pred3),test_set$target)
as.data.frame(cm3$table) %>% ggplot(aes(x = Reference,y = Prediction)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq), vjust = 1) +
  scale_color_viridis_d(option = "D")

val <- c(m1,m2,m3,cm1$byClass['F1'],cm2$byClass['F1'],cm3$byClass['F1'])
matrix(val,ncol = 2,nrow = 3,byrow = FALSE,
       dimnames = list(c("Logistic Regression","k-NN","Random Forest"),
                       c("Accuracy","F1 Score")))
