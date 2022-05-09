# Import our required libraries
library(rpart)
library(rpart.plot)

mushrooms <- read.csv("C:\\Users\\kinja\\OneDrive\\Documents\\R\\mushrooms.csv", header = TRUE)
summary(mushrooms)
head(mushrooms)

# Define the factor names for "class"
levels(mushrooms$class) <- c("Edible","Poisonous")

# Define the factor names for "odor"
levels(mushrooms$odor) <- c("Almonds","Anise","Creosote","Fishy","Foul","Musty","None","Pungent","Spicy")

# Define the factor names for "spore.print.color"
levels(mushrooms$spore.print.color) <- c("Black","Brown","Buff","Chocolate","Green","Orange","Purple","White","Yellow")
head(mushrooms)

# Create a classification decision tree using "Class" as the variable we want to predict and everything else as its predictors.
myDecisionTree <- rpart(class ~ ., data = mushrooms, method = "class")

# Print out a summary of our created model.
print(myDecisionTree)

rpart.plot(myDecisionTree, type = 5, extra = 2, under = TRUE, faclen=5, cex = .75)

newCase  <- mushrooms[10,-1]
newCase

predict(myDecisionTree, newCase, type = "class")

train_ind <- sample(c(1:nrow(mushrooms)), size = 10)

## 75% of the sample size
n <- nrow(mushrooms)
smp_size <- floor(0.75 * n)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(c(1:n), size = smp_size)

mushrooms_train <- mushrooms[train_ind, ]
mushrooms_test <- mushrooms[-train_ind, ]

newDT <- rpart(class ~ ., data = mushrooms_train, method = "class")

result <- predict(newDT, mushrooms_test[,-1], type = "class")

head(result)

head(mushrooms_test$class)

table(mushrooms_test$class, result)

