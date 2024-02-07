read.csv("C:\\Users\\ASALES\\Desktop\\datasets\\Credit.csv")
mydata <- read.csv("C:\\Users\\ASALES\\Desktop\\datasets\\Credit.csv")
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(plotly)

View(mydata)
attach(mydata)

###Check the type of data.
typeof(mydata)

###Check the format of the data.
is.data.frame(mydata)
as.data.frame(mydata)

###Check for missing values in the dataset.
sum(is.na(mydata))

###Describe the data.
nrow(mydata)
ncol(mydata)
names(mydata)

###Check the type of the variables of the data.
str(mydata)

###Describe statistics of the data.
install.packages("psych")
install.packages("ggpubr")
library(psych)
library(ggpubr)
describe(mydata)
summary(mydata)

###Relation between the variables of the data
plot(mydata)
plot(Income)
plot(Rating)
boxplot(Rating)
boxplot(Limit)
boxplot(Income)
barplot(Balance)
barplot(Cards)
with(data=mydata,hist(Income))

###Use of dplyr package
filter(mydata,Age>60)
select(mydata,Student,Education)
arrange(mydata,Age)
arrange(mydata,Cards)
recode(mydata$Student,"Yes"="1","No"="0")
recode(mydata$Married,"Yes"="1","No"="0")
summarize(mydata,avg_income=mean(Income))
summarize(mydata,var_income=var(Income))

###Use of ggplot2 package
#Scatter plot between Income and Cards
ggplot(data=mydata)+
  geom_point(mapping=aes(x=Income,y=Cards))+
  labs(title="Scatter plot between Income and cards",x="Income",y="Cards")

#Barchart for the Cards variable with respect to Education
ggplot(data=mydata)+
  geom_bar(mapping=aes(x=Cards,fill=Education))+
  labs(title="Bar plot between Education w.r.t. Cards",x="Cards",y="Education")

#Line chart  to visualize the relationship between Limit and Balance
ggplot(data=mydata)+
  geom_line(mapping=aes(x=Limit,y=Balance))+
  labs(title="Line between Limit and Balance",x="Limit",y="Balance")

#Histogram of Age variable
ggplot(data=mydata)+
  geom_histogram(mapping=aes(x=Age),binwidth=1)+
  labs(title="Histogram for Age variable",x="Age")

#Boxplot for the Income variable
ggplot(data=mydata)+
  geom_boxplot(mapping = aes(x=Income))+
  labs(title="Boxplot for Income variable",x="Income")


###Check the data is Normally distributed or not.
ggdensity(Income,main="Income of the Person",xlab="Income")
ggqqplot(Income)
shapiro.test(Income)

###Analysis of Variances(ANOVA)
aov(Income~Student,data=mydata)
aov(Income~Student+Married,data=mydata)

###Test for equality of variances
var.test(Income,Balance)

###Bernoulli distribution
#Probability of having a Student is 0.5 in Credit dataset
p=0.5
U<-runif(Student,min=0,max=1)
x<-(U<p)
x


###Creating a linear model of the dataset
mydataModel1 <- lm(Income~Rating+Cards+Age+Education+Balance,data=mydata)
summary(mydataModel1)
##Interpretation- This is interpreted  to mean that 81.52% of the 
#variance in Income is explained by the predictors in the model.
#Coefficient plot
install.packages("coefplot")
library(coefplot)
coefplot(mydataModel1)
#Evaluating the appropriateness of the model.
par(mfrow=c(2,2))
plot(mydataModel1)






