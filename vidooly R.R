# Clearing the environment
rm(list=ls(all=T))
# Setting working directory
setwd("E:/vidooly")

# Loading libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart","mlr", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','fastDummies')

lapply(x, require, character.only = TRUE)
rm(x)
d1 = read.csv('ad_org_train.csv')

d1$views=as.numeric(d1$views)
d1$likes=as.numeric(d1$likes)
d1$dislikes=as.numeric(d1$dislikes)
d1$comment=as.numeric(d1$comment)
d=data.frame(d1$published)
d1$published=as.Date(d1$published,format="%Y-%m-%d")
d1$month=format(as.Date(d1$published,format="%Y-%m-%d"), "%m")
d1$day=format(as.Date(d1$published,format="%Y-%m-%d"), "%d")
d1$year=format(as.Date(d1$published,format="%Y-%m-%d"), "%Y")
d1$year=as.factor(d1$year)
d1$month=as.factor(d1$month)
d1$day=as.factor(d1$day)
d1$duration=as.numeric(d1$duration)
data=d1
d3=data.frame(data$adview)
data=subset(data,select=-c(adview,vidid,published))
data=cbind(data,d3)
names(data)[10]="adview"
str(data)

###Missing Values Analysis###############################################

# 1. checking for missing value
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))

##############Outlier Analysis##########

# 1.BoxPlots - Distribution and Outlier Check
numeric_index = sapply(data,is.numeric) #selecting only numeric

numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "adview"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="adviews")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,ncol=3)
gridExtra::grid.arrange(gn3,gn4,gn5,ncol=2)

################## Correlation Plot #############
corrgram(data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


################################
cnames = c("views","comment","likes","dislikes","duration")

for(i in cnames){
  print(i)
  data[,i] = (data[,i] - min(data[,i]))/
    (max(data[,i] - min(data[,i])))
}

################################Model development######################
train_index = sample(1:nrow(data), 0.8 * nrow(data))
train = data[train_index,]
test = data[-train_index,]

RF_model = randomForest(adview ~ ., train, importance = TRUE, ntree = 20)
predictions_RF = predict(RF_model,test[,-10])




########################################################################





