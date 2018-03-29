#Kobe's shots

library(dplyr)
library(e1071)
data <- read.csv("./data.csv", stringsAsFactors = FALSE)
predictormodel <- naiveBayes()
train=data[!is.na(data$shot_made_flag),]
test=data[is.na(data$shot_made_flag),]
a=dim(train)[1]
b=dim(test)[1]
#gaining all the id values
shot_id=test[c(1:b),c(25)]
#training for only 2/3 of the training sample to save computation
a=round(a*2/3)
trainchuchuuu=train[c(1:a),c(1,2,5:10,13:18)]
train$shot_made_flag<-as.factor(train$shot_made_flag)
names(train)

#attributes to be utilized should be-action_type,combined_shot_type
#,lat,loc_x,loc_y,lon,minutes_remaining,period,seconds_remaining,shot_distance,shot_made_flag
#,shot_type,shot_zone_area,shot_zone_basic
model<-naiveBayes(shot_made_flag~ action_type+combined_shot_type+lat + loc_x + loc_y + lon + minutes_remaining + period + seconds_remaining + shot_distance + shot_made_flag + shot_type + shot_zone_area + shot_zone_basic,train)
shot_made_flag<-predict(model,test)
GG=cbind(shot_id,shot_made_flag)
head(GG)
#setting values down by one
for (i in 1:dim(GG)[1])
{GG[i,2]=GG[i,2]-1}
write.csv(GG,file="./result.csv")
