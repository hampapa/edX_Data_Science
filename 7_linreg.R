library(tidyverse)
library(Lahman)
data(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR_per_game=HR/G, R_per_game=R/G) %>%
    ggplot(aes(x=HR_per_game, y=R_per_game)) +
    geom_point(alpha=0.5) +
    theme_bw()

Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(SB_per_game=SB/G, R_per_game=R/G) %>%
    ggplot(aes(x=SB_per_game, y=R_per_game)) +
    geom_point(alpha=0.5) +
    theme_bw()

Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(BB_per_game=BB/G, R_per_game=R/G) %>%
    ggplot(aes(x=BB_per_game, y=R_per_game)) +
    geom_point(alpha=0.5) +
    theme_bw()



### Correlation
fx <- function(x){
    return (3+2*x)
}

x <- c(2,3,5,7,12)
y <- fx(x)
cor(x,y)
y <- y*-1
cor(x,y)


library(HistData)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
    filter(childNum == 1 & gender == "male") %>%
    select(father, childHeight) %>%
    rename(son=childHeight)
galton_heights %>%
    summarize(mean(father), sd(father), mean(son), sd(son))
galton_heights %>% summarize(cor(father, son), cor(son, father))

galton_heights %>%
    ggplot(aes(x=father,y=son)) +
    geom_point(alpha=0.5) +
    theme_bw()

### Correlation with different parameter values
library(tidyverse)

c_df <- function(y_sl=1, y_sd=1){   ## create data frame of random data
    x <- rnorm(n=2000,mean=0,sd=2)
    y_base <- y_sl*x
    error <- rnorm(n=length(y_base), mean=0, sd=y_sd)
    y <- y_base + error
    return(data.frame(x=x, y=y))
}

### manually calculating mean, std.dev, covariance, correlation
y_sd <- 1.5
df1 <- c_df(y_sd=y_sd)
n <- length(df1$x)
x <- df1$x
y <- df1$y
mean_x <- sum(x)/n
mean_y <- sum(y)/n
sd_x <- sqrt((1/(n-1))*sum((x-mean_x)^2))     ## sample std. dev.
sd_y <- sqrt((1/(n-1))*sum((y-mean_y)^2))
cov_xy <- 1/(n-1)*sum((x-mean_x)*(y-mean_y))  ## sample cov
cor_xy <- cov_xy/(sd_x*sd_y)
cor_xy
cor(x,y)

rho_label <- round(cor(df1$x,df1$y), digits=2)
df1 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    ggtitle(label=rho_label) +
    theme_bw()
cor(df1$x,df1$y)
