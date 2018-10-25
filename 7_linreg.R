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

### Correlation
library(tidyverse)

c_df <- function(y_sl=1, y_sd=1){   ## create data frame of random data
    x <- rnorm(n=500,mean=0,sd=2)
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

### comparing scatter plots with different parameters
y_sl <- 1
y_sd <- 1.5
df1 <- c_df(y_sl=y_sl, y_sd=y_sd)
rho <- cor(df1$x,df1$y)
p1 <- df1 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    ggtitle(label=sprintf("Cor: %.2f, y_sl: %.1f, y_sd: %.1f",
                          rho, y_sl, y_sd)) +
    coord_fixed(xlim=c(-10,10),ylim=c(-10,10)) +
    theme_bw()

y_sl <- 0.5
y_sd <- 1.5
df2 <- c_df(y_sl=y_sl, y_sd=y_sd)
rho <- cor(df2$x,df2$y)
p2 <- df2 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    ggtitle(label=sprintf("Cor: %.2f, y_sl: %.1f, y_sd: %.1f",
                          rho, y_sl, y_sd)) +
    coord_fixed(xlim=c(-10,10),ylim=c(-10,10)) +
    theme_bw()

y_sl <- 0.01
y_sd <- 1.5
df3 <- c_df(y_sl=y_sl, y_sd=y_sd)
rho <- cor(df3$x,df3$y)
p3 <- df3 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    ggtitle(label=sprintf("Cor: %.2f, y_sl: %.1f, y_sd: %.1f",
                          rho, y_sl, y_sd)) +
    coord_fixed(xlim=c(-10,10),ylim=c(-10,10)) +
    theme_bw()

y_sl <- 0.01
y_sd <- 0.05
df4 <- c_df(y_sl=y_sl, y_sd=y_sd)
rho <- cor(df4$x,df4$y)
p4 <- df4 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    ggtitle(label=sprintf("Cor: %.2f, y_sl: %.1f, y_sd: %.1f",
                          rho, y_sl, y_sd)) +
    coord_fixed(xlim=c(-10,10),ylim=c(-10,10)) +
    theme_bw()

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)



### Sample Correlation is a Random Variable
set.seed(0)
R <- sample_n(galton_heights, 25, replace=TRUE) %>%
    summarize(cor(father, son))
### run Monte Carlo Simultion to see distribution of this variabel
B <- 10000
N <- 25
R <- replicate(B, {
    sample_n(galton_heights, N, replace=TRUE) %>%
        summarize(r=cor(father, son)) %>% .$r
})

# data.frame(R) %>%
#    ggplot(aes(R)) + geom_histogram(binwidth=0.05, color="black")
mean(R)
sd(R)

B <- 10000
N <- 50
R <- replicate(B, {
    sample_n(galton_heights, N, replace=TRUE) %>%
        summarize(r=cor(father, son)) %>% .$r
})

# data.frame(R) %>%
#    ggplot(aes(R)) + geom_histogram(binwidth=0.05, color="black")
mean(R)
sd(R)



### Anscombe's Quartet - Stratification
galton_heights %>% summarize(mean(son), sd(son))
galton_heights %>% summarize(mean(father), sd(father))
(72 - mean(galton_heights$father))/sd(galton_heights$father)

galton_heights %>% filter(father==72)

conditional_average <- galton_heights %>% filter(round(father) == 72) %>%
    summarize(avg=mean(son)) %>% .$avg
conditional_average
(conditional_average - mean(galton_heights$son))/sd(galton_heights$son)

galton_heights %>% mutate(father_strata=factor(round(father))) %>%
    ggplot(aes(father_strata, son)) +
    geom_boxplot() +
    geom_point()

### slope on this chart appears to be 0.5, which appears to be the
### correlation between father and son
galton_heights %>% mutate(father=round(father)) %>%
    group_by(father) %>%
    summarize(son_conditional_avg = mean(son)) %>%
    ggplot(aes(father,son_conditional_avg)) +
    geom_point()

### the following gives the regression line
r <- galton_heights %>% summarize(r=cor(father,son)) %>% .$r
galton_heights %>%
    mutate(father=round(father)) %>%
    group_by(father) %>%
    summarize(son=mean(son)) %>%
    mutate(z_father=scale(father), z_son=scale(son)) %>%
    ggplot(aes(z_father, z_son)) +
    geom_point() +
    geom_abline(intercept=0, slope=r)

### regression line for E(Y|X=x)
gh <- galton_heights
mu_x <- mean(gh$father)
mu_y <- mean(gh$son)
s_x <- sd(gh$father)
s_y <- sd(gh$father)
r <- cor(gh$father, gh$son)
m <- r*(s_y/s_x)
b <- mu_y - m*mu_x

gh %>% ggplot(aes(father,son)) +
    geom_point(alpha=0.5) +
    geom_abline(intercept=b, slope=m)

gh %>% ggplot(aes(scale(father),scale(son))) +
    geom_point(alpha=0.5) +
    geom_abline(intercept=0, slope=r)



### Bivariate Normal Distribution
gh %>%
    mutate(z_father = round((father-mean(father))/sd(father))) %>%
    filter(z_father %in% -2:2) %>%
    ggplot() +
    stat_qq(aes(sample=son)) +
    facet_wrap(~z_father)
