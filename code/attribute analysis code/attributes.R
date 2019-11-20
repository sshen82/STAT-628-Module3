setwd("C:/Users/billh/Documents/lesson/2019fall/628/module3")
require('leaps')
data=read.csv("business_attributes.csv")
summary(data)
# clean data
data_new=data
data_new$X=NULL
data_new$business_id=NULL
data_new$GoodForMeal=NULL
data_new$Ambience=NULL
data_new$BusinessParking=NULL
#linear model
model1=lm(stars~.,data_new)
anova(model1)
summary(model1)

#select significant variables
model2=lm(stars~BusinessAcceptsCreditCards+WiFi+NoiseLevel+RestaurantsAttire+Alcohol+RestaurantsGoodForGroups+GoodForKids,data_new)
anova(model2)
#use R squared to select 
leaps<-regsubsets(stars ~ ., data=data_new, nbest=4)
model3=lm(stars~BusinessAcceptsCreditCards+WiFi+NoiseLevel+RestaurantsAttire+Alcohol+RestaurantsGoodForGroups+GoodForKids,data_new)
anova(model3)
summary(model3)






