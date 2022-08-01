lead=read.csv(file.choose(),header = TRUE)
head(lead)
summary(lead)
lead[lead=='Select']=NA
colSums(is.na(lead))

r=round(100*colSums(is.na(lead))/length(rownames(lead)),2)
r
#we will drop all the columns greater than 50% NA values
lead=subset(lead,select = -c(How_did_you_hear_about_X_Education))

#Now checking column one by one
# Removing NAs
library(ggplot2)
library(tidyverse)
library(dplyr)
ggplot(data = lead,aes(x=Lead.Quality))+geom_bar()

sum(is.na(lead$Asymmetrique_Activity_Score))
sum(is.na(lead$Asymmetrique_Profile_Score))
#let's drop these two columns
lead=subset(lead,select = -c(Asymmetrique_Activity_Score,Asymmetrique_Profile_Score))
colnames(lead)

summary(lead$City)
sum(is.na(lead$City))
mode(lead$City)
ggplot(data = lead,aes(x=City))+geom_bar()
# Mumbai is the mode, so filling the missing with Mumbai
lead$City=as.factor(lead$City)
lead$City[is.na(lead$City)]='Mumbai'

sum(is.na(lead$Specialization))
summary(lead$Specialization)
ggplot(data = lead,aes(x=Specialization))+geom_bar(width = 0.4)
#  may not have any specialization or is a student.
# Hence we can make a category "Others" for missing values.
lead$Specialization[is.na(lead$Specialization)]='others'
sum(is.na(lead$Specialization))

colSums(is.na(lead))
mean_visit=mean(lead$Total_Visits,na.rm = TRUE)
lead$Total_Visits[is.na(lead$Total_Visits)]=mean_visit
sum(is.na(lead$Total_Visits))

# Checking outliers
desc(lead$Page_Views_Per_Visit)
boxplot(lead$Page_Views_Per_Visit, main="Page Views", boxwex=0.1)
#will cap the outliers to 95% value for analysis.
fun = function(x){
  quantiles <- quantile( x, c(.05, .95 ),na.rm=TRUE )
  x[ x < quantiles[1] ] = quantiles[1]
  x[ x > quantiles[2] ] = quantiles[2]
  x
}
lead$Page_Views_Per_Visit=fun( lead$Page_Views_Per_Visit )
boxplot(lead$Page_Views_Per_Visit, main="Page Views", boxwex=0.1)
summary(lead$Page_Views_Per_Visit)
sum(is.na(lead$Page_Views_Per_Visit))
lead$Page_Views_Per_Visit[is.na(lead$Page_Views_Per_Visit)]=mean(lead$Page_Views_Per_Visit,na.rm = TRUE)
sum(is.na(lead$Page_Views_Per_Visit))
colSums(is.na(lead))

summary(lead$Lead.Profile)
tail(lead$Lead.Profile,20)
ggplot(data = lead,aes(x=Lead.Profile))+geom_bar()
#let's fill na with Other Leads
lead$Lead.Profile[is.na(lead$Lead.Profile)]='Other Leads'
summary(lead$Lead.Profile)

colSums(is.na(lead))


# missing values and outliers are treated 
#lets find the features affecting the conversions of a lead.
colnames(lead)
library(caret)
install.packages("Boruta")
library(Boruta)
#By observing, we can drop first two columns.
lead=select(lead,-c('Prospect_ID','Lead_Number'))

#Lead_Origin
head(lead$Lead_Origin)
# Let's use Boruta package to see importance of features w.r.t Converted.
set.seed(111)
boruta=Boruta(Converted~.,data = lead,doTrace=2)
print(boruta)
attStats(boruta)

#making training set based on boruta
df_1=select(lead,-c('Search','Magazine','Newspaper.Article','X.Education.Forums','Newspaper','Digital.Advertisement','Through.Recommendations','Receive.More.Updates.About.Our.Courses','Update.me.on.Supply.Chain.Content','Get.updates.on.DM.Content','I.agree.to.pay.the.amount.through.cheque','Do_Not_Call'))
head(df_1)
view(df_1)
colnames(df_1)
#let's Check the classes of each variable
df_1$Lead_Origin=as.factor(df_1$Lead_Origin)
class(df_1$Lead_Origin)

df_1$Lead_Source=as.factor(df_1$Lead_Source)
df_1$Country=as.factor(df_1$Country)
df_1$Specialization=as.factor(df_1$Specialization)
df_1$What.is.your.current.occupation=as.factor(df_1$What.is.your.current.occupation)
co_names=c('What.matters.most.to.you.in.choosing.a.course','Tags','Lead.Quality','Lead.Profile','City','Asymmetrique_Activity_Index','Asymmetrique_Profile_Index','A_free_copy_of_Mastering_The_Interview','Last_Activity','Last_Notable_Activity','Do_Not_Email','Converted')
features=c(co_names,'What.is.your.current.occupation','Specialization','Country','Lead_Origin')
df_1[,co_names]=lapply(df_1[,co_names], factor)
str(df_1)
view(df_1)



# lets get dummies for all factors
install.packages("fastDummies")
library(fastDummies)
df_1=dummy_columns(df_1,select_columns =c('What.matters.most.to.you.in.choosing.a.course','Tags','Lead.Quality','Lead.Profile','City','Asymmetrique_Activity_Index','Asymmetrique_Profile_Index','A_free_copy_of_Mastering_The_Interview','Last_Activity','Last_Notable_Activity','Do_Not_Email','What.is.your.current.occupation','Specialization','Country','Lead_Origin','Lead_Source'))
view(df_1)
df_1=select(df_1,-c('Lead_Source'))
df1_1=select(df_1,-c('What.matters.most.to.you.in.choosing.a.course','Tags','Lead.Quality','Lead.Profile','City','Asymmetrique_Activity_Index','Asymmetrique_Profile_Index','A_free_copy_of_Mastering_The_Interview','Last_Activity','Last_Notable_Activity','Do_Not_Email','What.is.your.current.occupation','Specialization','Country','Lead_Origin'))
df_1=select(df_1,-c(1:18))
view(df_1)

#train test split for first model
library(caTools)
set.seed(1111)
ind=sample(2,nrow(df_1),replace = T,prob = c(0.7,0.3))
train_1=df_1[ind==1,]
test_1=df_1[ind==2,]

# lets start with Logistic Regression

log_1=glm(Converted ~.,data = train_1,family = 'binomial' )
summary(log_1)
p1=predict(log_1,train_1,type = 'response')
head(p1)
head(train_1$Converted)
pred1=ifelse(p1>0.5,1,0)

#Confusion Matrix
tab1=table(Predicted=pred1,Actual=train_1$Converted)
tab1


#lets predict for test data:
p2=predict(log_1,test_1,type = 'response')
head(test_1)
pred2=ifelse(p2>0.5,1,0)
tab2=table(Predicted=pred2,Actual=test_1$Converted)
tab2
Accur=(1611+996)/(1611+138+90+996)
Accur
# Accuracy of 91.9% is achieved using Logistic Regression.

# Lets apply Random Forest now
library(randomForest)
set.seed(1234)

#making column names understandable for R (Legal names)
names(train_1)=make.names(names(train_1))
names(test_1)=make.names(names(test_1))

#Applying random forest:
rf=randomForest(Converted~.,data = train_1)
print(rf)
# lets predict on train data first
p_train=predict(rf,train_1)
head(p_train)
head(train_1$Converted)
confusionMatrix(p_train,train_1$Converted)
# Got accuracy of 96.1% in train dataset!

# lets predict for test dataset now:
p_test=predict(rf,test_1)
head(p_test)
head(test_1$Converted)
confusionMatrix(p_test,test_1$Converted)
# Got an accuracy of 93.3% in test dataset !
view(test_1)
