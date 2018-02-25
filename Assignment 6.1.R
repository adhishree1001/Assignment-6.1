#1. Import the Titanic Dataset from the link Titanic Data Set.
TitanicData <- read.csv("C:/Users/Mymaster/Desktop/Adhishree/Data Analytics/Assignments_Questions/TitanicData.txt", header=FALSE)
View(TitanicData)#Perform the following:
Titanic <- TitanicData
dim(Titanic)
install.packages("reshape")
View(TitanicData)
library(reshape)
Titanic = rename (Titanic, c(V1 = "PassengerId",
                             V2 = "Survived",
                             V3 = "Pclass",
                             V4 = "Name",
                             V5 = "Sex",
                             V6 = "Age",
                             V7 = "SinSP",
                             V8 = "Parch",
                             V9 = "Ticket",
                             V10 = "Fare",
                             V11 = "Cabin",
                             V12 = "Embarked"))
View(Titanic)
str(Titanic)
Titanic$Pclass <- as.factor(Titanic$Pclass)
Titanic$Name <- as.character(Titanic$Name)
str(Titanic)
#Perform the following: 
#a. Preprocess the passenger names to come up with a list of titles that represent families and represent using appropriate visualization graph. 
Titanic$Subtitle <- gsub("\\..*" , "", Titanic$Name)
Titanic$Title <- gsub(".*\\ " , "" , Titanic$Subtitle)
counts <- table(Titanic$Title)
Title <- barplot(counts, main = "No. Of Passengers by Title" ,  xlab = "Title" , ylab = "Number Of Passengers",  col = heat.colors(17))
#b. Represent the proportion of people survived from the family size using a graph. 

Titanic$Title <- as.factor(Titanic$Title)
Titanic$Survivors <- as.numeric(Titanic$Survived)
str(Titanic)
counts1<-table(Titanic$Survivors, Titanic$Title )
counts1
barplot(counts1, main = "Survivors by family size", xlab = "Families" , ylab = "Survivors", col = c("Blue", "Red"), legend = c("Survived","Not Survived"))
#c. Impute the missing values in Age variable using Mice Library, create two different graphs showing Age distribution before and after imputation. 
sum(is.na(Titanic$Age))
install.packages("mice")
library(mice)
md.pattern(Titanic)
mice_imputes <-  mice(Titanic, m=5, maxit = 40)
Imputed_data <- complete(mice_imputes,5)
hist(Titanic$Age, freq=F, main='Age distribution before', col='darkgreen')
hist(Imputed_data$Age, freq=F, main='Age distribution after', col='lightgreen')

