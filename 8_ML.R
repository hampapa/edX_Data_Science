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

### algorithm: guessing
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

### finding the parameter for cutoff that maximizes accuracy
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

### check the prevalence of category "Male"
sum(y == "Male")/sum(y %in% c("Male","Female"))

y_hat <- ifelse(test_set$height > 64, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
table(predicted=y_hat, actual=test_set$sex)

caret::confusionMatrix(data=y_hat, reference=test_set$sex, positive="Male")

### maximize F-score
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
        factor(levels=levels(train_set$sex))
    F_meas(data=y_hat,reference=factor(train_set$sex))
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

### guessing with higher probabilities
y_hat <- sample(c("Male","Female"), length(test_index), replace=TRUE) %>%
    factor(levels=levels(test_set$sex))
caret::confusionMatrix(data=y_hat, reference=test_set$sex)

p <- 0.0
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
         TPR = sensitivity(y_hat, test_set$sex),
         idx = p)
})

guessing %>%
    ggplot(aes(FPR, TPR, label=sprintf("%.2f",idx))) +
    geom_abline(color="grey") +
    geom_point() +
    geom_text(nudge_x = 0.08) +
    theme_bw()


cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
        factor(levels = c("Female", "Male"))
    list(method = "Height cutoff",
         FPR = 1-specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
    ggplot(aes(FPR, TPR, color = method)) +
    geom_line() +
    geom_point() +
    xlab("1 - Specificity") +
    ylab("Sensitivity")

### ROC curves have one weakness and it is that neither of
### the measures plotted depend on prevalence.
guessing <- map_df(probs, function(p){
    y_hat <- sample(c("Male", "Female"), length(test_index), 
                    replace = TRUE, prob=c(p, 1-p)) %>% 
        factor(levels = c("Female", "Male"))
    list(method = "Guess",
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
        factor(levels = c("Female", "Male"))
    list(method = "Height cutoff",
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex))
})
bind_rows(guessing, height_cutoff) %>%
    ggplot(aes(recall, precision, color = method)) +
    geom_line() +
    geom_point()

### If we change positives to mean Male instead of Female, the
### ROC curve remains the same, but the precision recall plot changes
guessing <- map_df(probs, function(p){
    y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                    prob=c(p, 1-p)) %>% 
        factor(levels = c("Male", "Female"))
    list(method = "Guess",
         recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
         precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
    y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
        factor(levels = c("Male", "Female"))
    list(method = "Height cutoff",
         recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
         precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
    ggplot(aes(recall, precision, color = method)) +
    geom_line() +
    geom_point()



###
### Logistic Regression
###

library(tidyverse)
library(HistData)
library(caret)

gh <- GaltonFamilies %>%
    filter(childNum == 1 & gender == "male") %>%
    select(father, childHeight) %>%
    rename(son = childHeight)

### predict son's height Y using fathers height X
y <- gh$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)

train_set <- gh %>% slice(-test_index)
test_set <- gh %>% slice(test_index)

train_set %>% ggplot(aes(x=father, y=son)) +
    geom_point() +
    theme_bw()

Y_hat <- mean(train_set$son)
### Mean Squared Error => using the test set to evaluate
MSE <- mean((Y_hat - test_set$son)^2)

fit <- lm(son ~ father, data=train_set)
Y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
MSE <- mean((Y_hat - test_set$son)^2)
MSE

Y_hat <- predict(fit, test_set)
MSE <- mean((Y_hat - test_set$son)^2)
MSE

### Exercises

Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
dat %>% ggplot(aes(x,y)) +
    geom_point() +
    theme_bw()

###
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
RMSE <- replicate(500, {
    dat <- MASS::mvrnorm(n = 1000, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE) 
    test_set <- dat %>% slice(test_index)
    train_set <- dat %>% slice(-test_index)
    fit <- lm(y ~ x, data=train_set)
    Y_hat <- predict(fit, test_set)
    sqrt(mean((Y_hat - test_set$y)^2))
}) %>% data.frame() %>% setNames("RMSE")
RMSE %>% summarize(mean=mean(RMSE), sd=sd(RMSE))

RMSE %>% ggplot(aes(x=RMSE)) +
    geom_histogram(binwidth=0.005)

###
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
B <- 100
n_size <- c(100, 500, 1000, 5000, 10000)
mean_sd <- map_df(n_size, function(n){
    replicate(B, {
        dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
            data.frame() %>% setNames(c("x", "y"))
        test_index <- createDataPartition(dat$x, times=1, p=0.5, list=FALSE) 
        test_set <- dat %>% slice(test_index)
        train_set <- dat %>% slice(-test_index)
        fit <- lm(y ~ x, data=train_set)
        Y_hat <- predict(fit, test_set)
        sqrt(mean((Y_hat - test_set$y)^2))  # Loss function
    }) %>% data.frame() %>% setNames("RMSE") %>%
        summarize(n=n, mean=mean(RMSE), sd=sd(RMSE))
})
mean_sd

###
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
RMSE <- replicate(500, {
    dat <- MASS::mvrnorm(n = 1000, c(69, 69), Sigma) %>%
        data.frame() %>% setNames(c("x", "y"))
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE) 
    test_set <- dat %>% slice(test_index)
    train_set <- dat %>% slice(-test_index)
    fit <- lm(y ~ x, data=train_set)
    Y_hat <- predict(fit, test_set)
    sqrt(mean((Y_hat - test_set$y)^2))
}) %>% data.frame() %>% setNames("RMSE")
RMSE %>% summarize(mean=mean(RMSE), sd=sd(RMSE))

RMSE %>% ggplot(aes(x=RMSE)) +
    geom_histogram(binwidth=0.005)

### 6.
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0, 0.75, 0, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = n, c(0, 0, 0), Sigma) %>%
    data.frame() %>% setNames(c("y", "x_1", "x_2"))


