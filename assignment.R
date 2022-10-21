# NAME

#TP123456

#Steps in Data Preprocessing
#Step 1: Importing the Dataset
#Before we start preparing our data, first we need to dowload it and load it in RStudio
#Here is how to achieve this:
df <-read.csv("/Users/ayoub/Downloads/Placement_Data_Full_Class.csv",header=TRUE)     #This code imports our data stored in CSV format.
View(df)              #We can have a look at our data using the ‘view()’ function
summary(df)

#Step 2: Cleaning of data
sum(is.na(df))        #this function show the number of missing values in the data 
#Clean column names
install.packages("janitor")
clean<-janitor::clean_names(df)
colnames(clean)
sum(is.na(df))       #the number of missing values in the data :
# replace missing values with mean :
m <- c()
for(i in colnames(df)){
  # compute mean for all columns
  mean_value <- mean(df[,i],na.rm = TRUE)
  m <- append(m,mean_value)
}
# adding column names to matrix
a <- matrix(m,nrow=1)
colnames(a) <- colnames(df)
a
for(i in colnames(df))
  df[,i][is.na(df[,i])] <- a[,i]
df


#Step 3: pre-processing the dataset :
#View data frame properties and summary statistics :
#The function “table()”  represent frequency of each column in the data :
table(df$gender)    
table(df$age)     
table(df$address) 
table(df$Mjob)
table(df$Fjob)
table(df$famsup)
table(df$paid)
table(df$activities)
table(df$internet)
table(df$ssc_b)
table(df$hsc_b)
table(df$hsc_s)
table(df$degree_t)
table(df$workex)
table(df$specialisation)
table(df$status)

#We can also represent the frequency with barplot function  : 
# 0 plot gender
FM0<-table(df$gender)
show<-barplot(FM0,main="Female and Male Respondents",xlab = "Gender",ylab = "Number of respondents",col = c(7,10))
text(show,0,FM0,cex = 1,pos = 3)
# 1 plot age
FM1<-table(df$age)
show<-barplot(FM1,main="age Respondents",xlab = "age",ylab = "Number of respondents",col = c(6,10,5,8,4,3))
text(show,0,FM1,cex = 1,pos = 3)
# 2 plot address
FM2<-table(df$address)
show<-barplot(FM2,main="address Respondents",xlab = "address",ylab = "Number of respondents",col = c(4,3))
text(show,0,FM2,cex = 1,pos = 3)
# 3 plot Mjob
FM3<-table(df$Mjob)
show<-barplot(FM3,main="Mjob Respondents",xlab = "Mjob",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM3,cex = 1,pos = 3)
# 4 plot Fjob
FM4<-table(df$Fjob)
show<-barplot(FM4,main="Fjob Respondents",xlab = "Fjob",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM4,cex = 1,pos = 3)
# 5 plot famsup
FM5<-table(df$famsup)
show<-barplot(FM5,main="famsup Respondents",xlab = "famsup",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM5,cex = 1,pos = 3)
# 6 plot paid
FM6<-table(df$paid)
show<-barplot(FM6,main="paid Respondents",xlab = "paid",ylab = "Number of respondents",col = c(2,5))
text(show,0,FM6,cex = 1,pos = 3)
# 7 plot activities
FM7<-table(df$activities)
show<-barplot(FM7,main="activities Respondents",xlab = "activities",ylab = "Number of respondents",col = c(2,5))
text(show,0,FM7,cex = 1,pos = 3)
# 8 plot hsc_s
FM8<-table(df$hsc_s)
show<-barplot(FM8,main="hsc_s Respondents",xlab = "hsc_s",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM8,cex = 1,pos = 3)
#9 plot degree_t
FM9<-table(df$degree_t)
show<-barplot(FM9,main="degree_t Respondents",xlab = "degree_t",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM9,cex = 1,pos = 3)
#10 plot specialisation
FM10<-table(df$specialisation)
show<-barplot(FM10,main="specialisation Respondents",xlab = "specialisation",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM10,cex = 1,pos = 3)
#11 plot status
FM11<-table(df$status)
show<-barplot(FM11,main="status Respondents",xlab = "status",ylab = "Number of respondents",col = c(2,5,6,7,8,4,3))
text(show,0,FM11,cex = 1,pos = 3)

#We can also represent the numerical column with box plot function:
#0 box plot salary
ggplot(df, aes(y=salary, x=1)) + geom_boxplot()
#1 box plot mba_p
ggplot(df, aes(y=mba_p, x=1)) + geom_boxplot()
#2 box plot etest_p
ggplot(df, aes(y=etest_p, x=1)) + geom_boxplot()
#3 box plot degree_p
ggplot(df, aes(y=degree_p, x=1)) + geom_boxplot()
#4 box plot hsc_p
ggplot(df, aes(y=hsc_p, x=1)) + geom_boxplot()
#5 box plot ssc_p
ggplot(df, aes(y=ssc_p, x=1)) + geom_boxplot()

#Step 4: transformation of data  :
#0 Converting gender column of df to 0/1 format
df$gender<-ifelse(df$gender=="M",1,0)
#1 Converting address column of df to 0/1 format
df$address<-ifelse(df$address=="U",1,0)
#2 Converting famsup column of df to 0/1 format
df$famsup<-ifelse(df$famsup=="yes",1,0)
#3 Converting paid column of df to 0/1 format
df$paid<-ifelse(df$paid=="yes",1,0)
#4 Converting activities column of df to 0/1 format
df$activities<-ifelse(df$activities=="yes",1,0)
#5 Converting internet column of df to 0/1 format
df$internet<-ifelse(df$internet=="yes",1,0)
#6 Converting workex column of df to 0/1 format
df$workex<-ifelse(df$workex=="Yes",1,0)
#7 Converting status column of df to 0/1 format
df$status<-ifelse(df$status=="Placed",1,0)
#8 Converting specialisation column of df to 0/1 format
df$specialisation<-ifelse(df$specialisation=="Mkt&HR",1,0)
View(df)

#Using this function to select the binary columns 0/1 :
data <- select(df,gender,address,famsup,paid,activities,internet,workex,status,specialisation)
View(data)


#Step 5:The extra feature:
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")
# Alternatively, install just dplyr:
install.packages("dplyr")
# Or the development version from GitHub:
install.packages("devtools")
devtools::install_github("tidyverse/dplyr")
install.packages()
library(dplyr)
#Question 1: There is a relation between famsup: (family educational support ) and Fedu, Medu ( mother’s and father’s educational Level ) ?
#Analysis 1-1: Find the relationship between famsup and Medu :
df3<-subset(df, famsup ==1, select = c(famsup, Medu))
table(df3$Medu)
table(df$Medu,df$famsup)

#Analysis 1-2: Find the relationship between famsup and Fedu :
df3<-subset(df, famsup ==1, select = c(famsup, Fedu))
table(df3$Fedu)
table(df$Fedu,df$famsup)

#Question 2: There is a relation or impact between a student's home address type ( binary ‘U’ OR Rural ‘R’), and Internet access at home (binary: yes or no) ?
#Analysis 2-1: Find the relationship between address and Internet :
df3<-subset(df, address ==1, select = c(address, internet))
table(df3$internet)
table(df$internet,df$address)

#Question 3: There is a relation between famsup: (family educational support ) and Paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no) ?
#Analysis 3-1:Find the relationship between famsup and paid :
df3<-subset(df, famsup ==1, select = c(famsup, paid))
table(df3$paid)
table(df$paid,df$famsup)

#Question 4: There is a relation between Salary offered by corporate to candidates and extra paid classes within the course subject (Math or Portuguese) ?
#Analysis 4-1:Find the relationship between Salary and paid :
df5<-subset(df, paid ==1 , select = c(paid, salary))
table(df5$salary)
table(df$salary,df$paid)

#END 