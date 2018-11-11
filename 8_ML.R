library(tidyverse)
library(caret)
library(dslabs)

data(heights)

y <- heights$sex
x <- heights$height

set.seed(2)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE) %>%
    factor(levels=levels(test_set$sex))

y_hat[1]
test_set$sex[1]
y_hat[1] == test_set$sex[1]
y_hat == test_set$sex
### Overall accuracy
mean(y_hat == test_set$sex)

heights %>% group_by(sex) %>%
    summarize(mean(height), sd(height))

y_hat <- ifelse(x > 62, "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
mean(y == y_hat)
y_hat <- ifelse(test_set$height > 62, "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
mean(y_hat == test_set$sex)

cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels=levels(train_set$sex))
    mean(y_hat == train_set$sex)
})
data.frame(cutoff=cutoff, accuracy=accuracy) %>%
    ggplot(aes(cutoff,accuracy)) +
    geom_line() +
    geom_point() +
    theme_bw()

max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
mean(y_hat == test_set$sex)

### Confusion matrix
table(predicted = y_hat, actual = test_set$sex)

test_set %>%
    mutate(y_hat=y_hat) %>%
    group_by(sex) %>%
    summarize(accuracy = mean(y_hat == sex))

sum(y == "Male")/sum(y %in% c("Male","Female"))

y_hat <- ifelse(test_set$height > 64, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
table(predicted=y_hat, actual=test_set$sex)

caret::confusionMatrix(data=y_hat, reference=test_set$sex)

### maximize F-score
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(test_set$height > x, "Male", "Female") %>%
        factor(levels=levels(test_set$sex))
    F_meas(data=y_hat,reference=test_set$sex)
})
data.frame(cutoff=cutoff, F_1=F_1) %>%
    ggplot(aes(cutoff,F_1)) +
    geom_line() +
    geom_point() +
    theme_bw()

y_hat <-
    ifelse(test_set$height > cutoff[which.max(F_1)], "Male", "Female") %>%
    factor(levels=levels(test_set$sex))
caret::confusionMatrix(data=y_hat, reference=test_set$sex)

p <- 0.9
y_hat <- sample(c("Male","Female"),
                length(test_index),
                replace=TRUE,
                prob=c(p,1-p)) %>%
    factor(levels=levels(test_set$sex))
mean(y_hat == test_set$sex)
caret::confusionMatrix(data=y_hat, reference=test_set$sex)

probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
    y_hat <- 
        sample(c("Male", "Female"), length(test_index),
               replace = TRUE, prob=c(p, 1-p)) %>% 
        factor(levels = c("Female", "Male"))
    list(method = "Guessing",
         FPR = 1 - specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>%
    qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")
