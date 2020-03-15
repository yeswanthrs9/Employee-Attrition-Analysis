cat('\014')

library(DMwR)
library(ggplot2)
library(dplyr)
library(arules)
library(arulesViz)

df<-read.csv('employee data.csv')

View(df)
str(df)

df$Gender[df$Gender==""]<- NA
df$OverTime[df$OverTime==""]<- NA

df$Gender<-as.character(df$Gender)
df$OverTime<-as.character(df$OverTime)

str(df)
summary(df)

df$Gender<-as.factor(df$Gender)
df$OverTime<-as.factor(df$OverTime)
df$Education<-as.factor(df$Education)
df$EnvironmentSatisfaction<-as.factor(df$EnvironmentSatisfaction)
df$JobInvolvement<-as.factor(df$JobInvolvement)
df$JobLevel<-as.factor(df$JobLevel)
df$JobSatisfaction<-as.factor(df$JobSatisfaction)
df$PerformanceRating<-as.factor(df$PerformanceRating)
df$RelationshipSatisfaction<-as.factor(df$RelationshipSatisfaction)
df$StockOptionLevel<-as.factor(df$StockOptionLevel)
df$WorkLifeBalance<-as.factor(df$WorkLifeBalance)
df$TrainingTimesLastYear<-as.factor(df$TrainingTimesLastYear)

df<-df[,-c(9,10,22,27)]

str(df)

sum(!complete.cases(df))

sapply(df,function(x)sum(is.na(x)))

df$DistanceFromHome[is.na(df$DistanceFromHome)]<-mean(df$DistanceFromHome,na.rm = TRUE)
df$PercentSalaryHike[is.na(df$PercentSalaryHike)]<-mean(df$PercentSalaryHike,na.rm = TRUE)
df$TotalWorkingYears[is.na(df$TotalWorkingYears)]<-mean(df$TotalWorkingYears,na.rm = TRUE)
df$YearsSinceLastPromotion[is.na(df$YearsSinceLastPromotion)]<-mean(df$YearsSinceLastPromotion,na.rm = TRUE)

df <- knnImputation(df)

sum(!complete.cases(df))

sapply(df,function(x)sum(is.na(x)))

prop.table(table(df$Attrition))*100

sum(duplicated(df))

# VISUALIZATION

c<-data.frame(df[,c(1,4,6,11,17,18,19,21,25,28,29,30,31)])
View(c)
corrplot(cor(c),method = 'circle', type='upper',tl.cex = 0.5)


# Age
ggplot(df, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot()

# Business Travel
ggplot(df, aes(x=BusinessTravel,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Department
ggplot(df, aes(x=Department,fill=Attrition)) + geom_bar(position = position_dodge()) + 
  labs(y="Proportion")

# Distance From Home
ggplot(df,aes(x=Attrition, y=DistanceFromHome, fill=Attrition))+geom_boxplot()

# Education
ggplot(df, aes(x=Education,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Education Field
ggplot(df, aes(x=EducationField,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Environment Satisfaction
ggplot(df, aes(x=EnvironmentSatisfaction,fill=Attrition)) + 
  geom_bar(position = position_fill()) + labs(y="Proportion")

# Gender
ggplot(df, aes(x=Gender,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Job Involvement
ggplot(df, aes(x=JobInvolvement,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Job Level
ggplot(df, aes(x=JobLevel,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Job Role
ggplot(df, aes(x=JobRole,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Job Satisfaction
ggplot(df, aes(x=JobSatisfaction,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Marital Status
ggplot(df, aes(x=MaritalStatus,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Monthly Income
ggplot(df,aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+geom_boxplot()

# Number of Companies Worked
ggplot(df,aes(x=Attrition,y=NumCompaniesWorked,fill=Attrition))+geom_boxplot()

# OverTime
ggplot(df, aes(x=OverTime,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Percent Salary Hike
ggplot(df,aes(x=Attrition,y=PercentSalaryHike,fill=Attrition))+geom_boxplot()

# Performance Rating
ggplot(df, aes(x=PerformanceRating,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Relationship Satisfaction
ggplot(df, aes(x=RelationshipSatisfaction,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# StockOption Level
ggplot(df, aes(x=StockOptionLevel,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Total Working Years
ggplot(df,aes(x=Attrition,y=TotalWorkingYears,fill=Attrition))+geom_boxplot()

# Training Times LastYear  
ggplot(df,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar(position = position_fill())

# WorkLife Balance
ggplot(df, aes(x=WorkLifeBalance,fill=Attrition)) + geom_bar(position = position_fill()) + 
  labs(y="Proportion")

# Years At Company
ggplot(df,aes(x=Attrition,y=YearsAtCompany,fill=Attrition))+geom_boxplot()

# Years In Current Role
ggplot(df,aes(x=Attrition,y=YearsInCurrentRole,fill=Attrition))+geom_boxplot()

#Years Since Last Promotion 
ggplot(df,aes(x=Attrition,y=YearsSinceLastPromotion,fill=Attrition))+geom_boxplot()

#Years With Current Manager
ggplot(df,aes(x=Attrition,y=YearsWithCurrManager,fill=Attrition))+geom_boxplot()


# discretization

data<-df

breaks<-function(variable)
{
  discretize(variable, method = "frequency",breaks = 3, labels = c("low","medium", "high"))
}

for (i in c(1,4,6,11,17,18,19,21,25,28,29))
{
  data[,i]<-breaks(data[,i])
}
  
str(data)

breaks2<-function(input)
{
  v<-input
  b<-quantile(input,c(0.25,0.75))
  v[input<=b[1]]<-"low"
  v[input>b[1] & input<b[2]]<-"medium"
  v[input>=b[2]]<-"high"
  v<-factor(v)
  return(v)
}

data$YearsSinceLastPromotion<-breaks2(data$YearsSinceLastPromotion)
data$YearsWithCurrManager<-breaks2(data$YearsWithCurrManager)

str(data)

ar<-as(data,'transactions')

rules<-apriori(ar,parameter = list(support=0.025,confidence=0.65,minlen=3,maxlen=15),appearance = list(default="lhs",rhs="Attrition=Yes"))
toprules <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspectDT(head(toprules,10))

rules1<-apriori(ar,parameter = list(support =0.02,confidence=0.7,minlen=3,maxlen=15),appearance = list(default="lhs",rhs="Attrition=No"))
toprules1 <- sort(rules1, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(toprules1,10))









