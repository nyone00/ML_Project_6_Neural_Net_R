# get the data
bank <- read.csv('bank_note_data.csv')
head(bank)
summary(bank)
str(bank)
library(ggplot2)

# EDA using visualization
ggplot(bank,aes(Image.Skew)) + geom_histogram(bins=30)

# Train Test Split
library(caTools)
set.seed(101)
sample <- sample.split(bank$Class, SplitRatio = 0.7)

train <- subset(bank, sample == TRUE)
test <- subset(bank, sample == FALSE)

# Building the Neural Net
library(neuralnet)

n <- names(train)
f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))

# Browse through the documentation of neuralnet
nn <- neuralnet(f, data=train, hidden = 10, linear.output = FALSE) # FALSE b/c this is a classification
plot(nn)

# Predictions
# Use compute() to grab predictions using your nn model on the test set. 
predicted.nn.values <- compute(nn,test[1:4])

predicted.nn.values$net.result <- round(predicted.nn.values$net.result)
# or predicted.nn.values$net.reusult <- sapply(predicted.nn.values$net.result,round)

table(predicted.nn.values$net.result,test$Class)


# COMPARING MODELS
library(randomForest)

bank$Class <- factor(bank$Class)

samply <- sample.split(bank$Class, SplitRatio = 0.7)
train <- subset(bank, sample == T)
test <- subset(bank, sample == F)

# Create a randomForest model with the new adjusted training data
rf.model <- randomForest(Class ~., data = train, importance = T)
predicted.randomF <- predict(rf.model, test)

# use table() to create the confusion matrix
table(predicted.randomF, test$Class)

# rf.pred    0    1
#       0   227   1
#       1    2   182
# 99% accuracy


