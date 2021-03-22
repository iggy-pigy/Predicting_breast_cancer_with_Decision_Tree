mammo_data <- read.csv("mammographic.csv", header= TRUE)

#Inspect the data in R
dim(mammo_data)
str(mammo_data)
names(mammo_data)
head(mammo_data)
tail(mammo_data)
summary(mammo_data)
dim(mammo_data)

#Check N/A values
apply(mammo_data, 2, function(x) any(is.na(x)))

#Check "?" values
apply(mammo_data, 2, function(x) any(x=="?"))

#Remove data with "?" marks
cleaned_mammo<-mammo_data[!(mammo_data$Score=="?" | mammo_data$Age=="?" 
                            | mammo_data$Shape=="?" | mammo_data$Margin=="?"
                            |mammo_data$Density=="?"),]
#Inspect cleaned_mammo data
dim(cleaned_mammo)
summary(cleaned_mammo)

#Convert from a character type to a numeric type 
cleaned_mammo <- data.frame(apply(cleaned_mammo, 2, function(x) as.numeric(x)))
str(cleaned_mammo)

# Fixing a misprint value from 55 to 5
cleaned_mammo$Score[cleaned_mammo$Score==55] <- 5

# Adding a new value to our dataset called MalignantF as factor:
cleaned_mammo$MalignantF <- as.factor(cleaned_mammo$Malignant)
str(cleaned_mammo)

#Building a ML model

#Split our dataset into the training and test (validation) sets
set.seed(1234)
pd <- sample(2, nrow(cleaned_mammo),replace=TRUE, prob=c(0.8,0.2))
pd
str(cleaned_mammo)
train <- cleaned_mammo[pd==1,]
validate <- cleaned_mammo[pd==2,]
validate
dim(train) # Retrieve the dimension of the train data set
dim(validate) # Retrieve the dimension of the validate data set

#Install package "party"
library(party)

#Building ctree
tumour_tree <- ctree(MalignantF ~ Age + Shape + Margin + Density + Score ,data = train)
print(tumour_tree) 
 
# Plot the tree
plot(tumour_tree, type="simple")

#Make precition
predict(tumour_tree)

#Calculate the classification accuracy and error on the train dataset
tab <- table(predict(tumour_tree), train$MalignantF)
print(tab)
sum(diag(tab))/sum(tab)  #This result shows that classification is 84.65% is accurate. 
1-sum(diag(tab))/sum(tab) #This result shows that classification error is 15.35%

#Validate our model on the validate(test) dataset
validate_predict <- table(predict(tumour_tree, newdata= validate), validate$MalignantF)
print(validate_predict)

sum(diag(validate_predict))/sum((validate_predict))  #This result shows that classification is 84.88% is accurate.
1-sum(diag(validate_predict))/sum(validate_predict)  #This result shows that classification error is 15.12%
