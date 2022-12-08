library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)

View(kc_house_data)

house <- kc_house_data
house

is.null(house)

str(house)

summary(house)

#Exploratory Data Analysis

colnames(house)

head(house)

corr <- cor(select(house,-long,-lat,-date))
corr.plot <- ggcorrplot(corr, type = "lower", outline.col = "white" ,lab = TRUE,lab_size = 1)+
  labs(title = "Data Correlation")
ggplotly(corr.plot,label=style)

gghistogram(house$price,fill ="skyblue",bins =60,title =" Price Distrbuation")+
  scale_x_continuous(labels = label_number_si())

gghistogram(houses$price,fill="skyblue",bins =150,title =" Price Distrbuation < 1M",interactive=TRUE)+
  scale_x_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::dollar)+
  coord_cartesian(x=c(0,1000000))

house$bedrooms<-factor(houses$bedrooms)
house %>% filter(!(bedrooms=="30"|bedrooms=="33")) %>%
  group_by(bedrooms) %>% summarise(mean=mean(price)) %>% 
  ggbarplot(x="bedrooms",y="mean",fill = "bedrooms",palette = "Set3",size = 1.5)+
  scale_y_continuous(labels = scales::dollar,n.breaks = 15)+
  theme(legend.position = "none")+
  labs(title = " Price Average By Bedrooms")

summarise() ungrouping output (override with '.groups' argument)

#Grades

house$grade<-factor(houses$grade)
house %>% group_by(grade) %>% summarise(mean=mean(price)) %>% 
  ggbarplot(x="grade",y="mean",fill="grade",size=1.5)+
  scale_y_continuous(labels = scales::dollar,n.breaks = 15)+
  theme(legend.position = "none")+
  labs(title = "Price Average by grade")

summarise() ungrouping output (override with .groups argument)

#View

house$view<-factor(houses$view)
house %>% group_by(view) %>% summarise(mean=mean(price)) %>% 
  ggbarplot(x="view",y="mean",fill="view",palette = wes_palette("Darjeeling1"),size = 1.5)+
  theme(legend.position = "none")+scale_y_continuous(labels = dollar,n.breaks = 15)+
  labs(title = " Price Average By View")

#Randomforest

house = dplyr::select(house,-id,-date,-yr_renovated,-zipcode)

#Now split our data into train and test data

sample=sample.split(house$price,SplitRatio=0.75)
train=subset(house,sample==T)
test=subset(house,sample==F)

#create the model

model=randomForest(price~.,train)
model

predict=predict(model,test[,-1])
postResample(test$price,predict)

#Improving The Model
#We did some changes in our data so that we improve the model,for example we converted the year built from numeric to factors and convert basement space to (true,false) factor,and some rounding . and resampling.

house$yearb<-cut(houses$yr_built,c(1900,1950,2000,2020))
house$yearb<-factor(house$yearb,levels = c("(1.9e+03,1.95e+03]","(1.95e+03,2e+03]","(2e+03,2.02e+03]"),labels = c("1900-1950","1950-2000","2000-2020"))
house=house %>% dplyr::select(-yr_built) 
house$bathrooms=round(house$bathrooms)
house$floors<-round(house$floors)
house<-house %>% filter(!is.na(yearb))
house$basement<-ifelse(house$sqft_basement>0,1,0)
house<-house %>% dplyr::select(-sqft_basement)
sample=sample.split(house$price,SplitRatio=0.75)
train=subset(house,sample==T)
test=subset(house,sample==F)
train=rep_sample_n(train,size =16884,reps = 2,replace = TRUE)
model=randomForest(price~.,train[,-1])
model


