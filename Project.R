# install.packages("data.table")
# install.packages("dummies")
# install.packages("Boruta", repos = "https://cran.r-project.org")
# install.packages("ROCR")
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("randomForest")


##########################################
#1. Business Understanding               #
##########################################

#
# See Report
#

##########################################
#2. Data Understanding                   #
##########################################

#Loading the data
data <- read.csv("Loan payments data.csv",
                 strip.white = T,
                 na.strings = c("NA",
                                "NaN",
                                "",
                                "?"),
                 stringsAsFactors = FALSE)

summary(data)
str(data)

# Seeing percentage of categories for loan_staut column
(table(data$loan_status) / length(data$loan_status)) * 100
# COLLECTION COLLECTION_PAIDOFF            PAIDOFF 
# 20                 20                 60 


# Creating histogram for Principal
# Set up the plot area
par(mfrow = c(1,1))
# Create the histogram bars
hist(data$Principal,
     breaks = 30,
     xlim = c(200, 1000),
     col = "blue",
     border = "black",
     ylim = c(0, 300),
     xlab = "Principal",
     ylab = "Counts",
     main = "Histogram of Principal variable")
# Make a box around the plot
box(which = "plot",
    lty = "solid",
    col="black")


# Creating histogram for terms
# Set up the plot area
par(mfrow = c(1,1))
# Create the histogram bars
hist(data$terms,
     breaks = 30,
     xlim = c(0, 1),
     col = "blue",
     border = "black",
     ylim = c(0, 100),
     xlab = "Terms",
     ylab = "Counts",
     main = "Histogram of Terms variable")
# Make a box around the plot
box(which = "plot",
    lty = "solid",
    col="black")

# Creating histogram for age
# Set up the plot area
par(mfrow = c(1,1))
# Create the histogram bars
hist(data$age,
     breaks = 30,
     xlim = c(15, 55),
     col = "blue",
     border = "black",
     ylim = c(0, 60),
     xlab = "Age",
     ylab = "Counts",
     main = "Histogram of Age variable")
# Make a box around the plot
box(which = "plot",
    lty = "solid",
    col="black")


# Creating scatterplot of Principal v/s Age
plot(data$age,data$Principal,
     xlim = c(15, 60),
     ylim = c(0, 1000),
     xlab = "Age",
     ylab = "Principal",
     main = "Scatterplot of Principal V/s Age",
     type = "p",
     pch = 16,
     col = "green")


# normalizing Principal
mmnorm.Principal <- (data$Principal-min(data$Principal))/(max(data$Principal)-min(data$Principal))

# Set up the plot area
par(mfrow = c(1,1))
# Create the histogram bars
hist(mmnorm.Principal,
     breaks = 10,
     xlim = c(0, 1),
     col = "blue",
     border = "black",
     ylim = c(0,8),
     xlab = "Principal",
     ylab = "Counts",
     main = "Histogram of Principal variable")
# Make a box around the plot
box(which = "plot",
    lty = "solid",
    col="black")


# normalizing Terms
mmnorm.terms <- (data$terms-min(data$terms))/(max(data$terms)-min(data$terms))
# Set up the plot area
par(mfrow = c(1,1))
# Create the histogram bars
hist(mmnorm.terms,
     breaks = 10,
     xlim = c(0, 1),
     col = "blue",
     border = "black",
     ylim = c(0, 100),
     xlab = "Terms",
     ylab = "Counts",
     main = "Histogram of Terms variable")
# Make a box around the plot
box(which = "plot",
    lty = "solid",
    col="black")

#Normalizing age
mmnorm.age <- (data$age-min(data$age))/(max(data$age)-min(data$age))

plot(mmnorm.age,mmnorm.Principal,
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Age",
     ylab = "Principal",
     main = "Scatterplot of Principal V/s Age",
     type = "p",
     pch = 16,
     col = "green")

##########################################
#3. Data Preparation Activity            #
##########################################

#3.1. Removing columns
data$Loan_ID <- NULL
data$due_date <- NULL
data$paid_off_time <- NULL
data$past_due_days <- NULL
data$effective_date <- NULL


#3.2. Revaluing factors
data$loan_status[data$loan_status == "PAIDOFF"] <- 0
data$loan_status[data$loan_status == "COLLECTION"] <- 1
data$loan_status[data$loan_status == "COLLECTION_PAIDOFF"] <- 1


#3.3. Read categorical data as factors:
data$loan_status <- as.factor(data$loan_status)
data$education <- as.factor(data$education)
data$Gender <- as.factor(data$Gender)
data$terms <- as.factor(data$terms)


#3.4. Classifying ages in groups
data$age_bin[data$age <= 20] <- 'YoungAdult'
data$age_bin[data$age > 20 & data$age <= 30] <- 'EarlyAdulthood'
data$age_bin[data$age > 30 & data$age <= 40] <- 'Adult'
data$age_bin[data$age > 40] <- 'Senior'

# Reading age as a fator
data$age_bin <- as.factor(data$age_bin)

# Removing age variable
data$age <- NULL


#3.5. Standardizing Principal
data$Principal <- (data$Principal - min(data$Principal)) / (max(data$Principal) -min(data$Principal))


summary(data)
str(data)

View(data)


#3.6. Association Rules Mining on subset
library(arules)
header <- c("education", "Gender", "loan_status")
subset<- data[,header]
str(subset)
rules <- apriori(subset)
inspect(rules)

# change loan_status as rhs
rules <- apriori(subset,
                 control = list(verbose=F),
                 parameter = list(minlen=2,
                                  supp=0.005,
                                  conf=0.3),
                 appearance = list(rhs=c("loan_status=1",
                                         "loan_status=0"),
                                   default="lhs"))

# order rules by lift
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

# install.packages("arulesViz")
library(arulesViz)
plot(rules)

##########################################
#4. Modeling Activity                    #
##########################################

#4.1: create dummies for factors:
library("dummies")

dummy_data <- dummy.data.frame(data,
                               names = c("education",
                                         "age_bin",
                                         "Gender"))

dummy_data
str(dummy_data)


#4.2. Renaming columns as to remove spaces
names(dummy_data)
names(dummy_data)[names(dummy_data) == 'educationHigh School or Below'] <- 'educationHighSchoolorBelow'
names(dummy_data)[names(dummy_data) == 'educationMaster or Above'] <- 'educationMasterorAbove'


#recasting dummy variables as a factor of values
dummy_data$educationBechalor <- as.factor(dummy_data$educationBechalor)
dummy_data$educationcollege <- as.factor(dummy_data$educationcollege)
dummy_data$educationHighSchoolorBelow <- as.factor(dummy_data$educationHighSchoolorBelow)
dummy_data$educationMasterorAbove <- as.factor(dummy_data$educationMasterorAbove)

dummy_data$Genderfemale <- as.factor(dummy_data$Genderfemale)
dummy_data$Gendermale <- as.factor(dummy_data$Gendermale)

dummy_data$age_binYoungAdult <- as.factor(dummy_data$age_binYoungAdult)
dummy_data$age_binAdult <- as.factor(dummy_data$age_binAdult)
dummy_data$age_binSenior <- as.factor(dummy_data$age_binSenior)
dummy_data$age_binEarlyAdulthood<- as.factor(dummy_data$age_binEarlyAdulthood)

str(dummy_data)

#4.3. Splitting data into train and test data sets
n = nrow(dummy_data)

# We create an index for 70% of obs. by random
set.seed(2966)
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE)

# We use the index to create training data
train = dummy_data[trainIndex,]

# We take the remaining 30% as the testing data
test= dummy_data[-trainIndex,]

##########################################
#4. MODELING                             #
#       &                                #
#5. EVALUATION                           #
##########################################
library(ROCR)
library(ggplot2)

##########################################
#4.4. Model - CART                       #
##########################################
library("rpart")
library("rpart.plot")

cartfit <- rpart(loan_status ~ .,
                 data = train, method= "class")
print(cartfit)
rpart.plot(cartfit)


##########################################
#5.1. Evaluation - CART                  #
##########################################

predicted_values <- predict(cartfit, test,type= "prob")[,2]

predicted_values <- predict(cartfit, test,type= "prob")[,2] 
pred <- prediction(predicted_values, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc_dt <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("CART ROC Curve w/ AUC=", auc_dt))



##########################################
#4.5. Model - Logistic Regression        #
##########################################

logit <- glm(loan_status ~.,
             family=binomial(link='logit'),
             data=train)
summary(logit)

##########################################
#5.2. Evaluation - Logistic Regression   #
##########################################

predicted_values <- predict(logit, test,type= "link")

pred <- prediction(predicted_values,
                   test$loan_status)
perf <- performance(pred,
                    measure = "tpr",
                    x.measure = "fpr")
auc <- performance(pred,
                   measure = "auc")
auc_lr <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")


ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Logistic Regression ROC Curve w/ AUC=", auc_lr))


##########################################
#4.6. Model - Random Forest              #
##########################################

set.seed(1006)
rf <- randomForest(loan_status ~.,
                   data = train,
                   ntree=3)
print(rf)


##########################################
#5.3. Evaluating - Random Forest         #
##########################################

predicted_values <- predict(rf, test,type= "prob")[,2]
pred <- prediction(predicted_values, test$loan_status)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc_rf <- auc@y.values[[1]]

summary(test$loan_status)
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="rf")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("Random Forest ROC Curve w/ AUC=", auc_rf))

#END