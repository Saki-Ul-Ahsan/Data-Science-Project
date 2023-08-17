install.packages("dplyr")
install.packages("matrixStats")
install.packages("tibble")
library(tibble)
library(dplyr)
library(matrixStats)

mydata <- read.csv("C:/Users/USER/Desktop/Data Science/Mid project/Dataset_midterm_Section(B).csv", header = TRUE, sep = ",")
mydata

names(mydata)
summary(mydata)
colSums(is.na(mydata))
str(mydata)
sum(is.na(mydata))

gender_counts <- table(mydata$Gender)
barplot(gender_counts, main = "Gender Distribution", xlab = "Gender", ylab = "Count")

hist(mydata$age, main = "Histogram of Age with Outliers", xlab = "Age", ylab = "Frequency")

data_filtered <- mydata %>% filter(age < 100)

hist(data_filtered$age, main = "Histogram of Age without Outliers", xlab = "Age", ylab = "Frequency")

meanvalueage <- mean(mydata$age, na.rm = TRUE)
meanvalueage
mydata$age[is.na(mydata$age)] <- meanvalueage

modevaluegender <- names(gender_counts)[which.max(gender_counts)]
modevaluegender
mydata$Gender[is.na(mydata$Gender)] <- modevaluegender

mydata

cleaned_class <- na.omit(mydata$class)
cleaned_class <- cleaned_class[cleaned_class %in% c("First", "Second", "Third")]
class_counts <- table(cleaned_class)
barplot(class_counts, xlab = "Class", ylab = "Frequency", main = "Bar Chart of Class of passengers")

alone_counts <- table(mydata$alone)
barplot(alone_counts, xlab = "Alone", ylab = "Frequency", main = "Bar chart of surviving alone")

survive_counts <- table(mydata$survived)
barplot(survive_counts, xlab = "Survived", ylab = "Frequency", main = "Bar chart of survival")
