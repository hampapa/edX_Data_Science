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



### Linear Models, Least Squares Estimates (LSE)
gh <- galton_heights
rss <- function(beta0, beta1, data){
    resid <- gh$son - (beta0 + beta1*gh$father)
    return(sum(resid^2))
}
### would be 3D plot, but keep beta0 fixed at 25 -> 2D plot: RSS as
### function of beta1
beta1 <- seq(0,1,len=nrow(gh))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0=36))
results %>% ggplot(aes(x=beta1,y=rss)) +
    geom_line() +
    geom_line(aes(x=beta1,y=rss), col=2)

### lm() function
fit <- lm(son ~ father, data=gh)
summary(fit)

bb_dat <-  Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(BB=BB/G, R=R/G, HR=HR/G)
baseball <- lm(R ~ BB + HR, data=bb_dat)



### LSE are random variables
B <- 1000
N <- 50
lse <- replicate(B, {
    sample_n(gh, N, replace=TRUE) %>%
        lm(son ~ father, data=.) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth=5, color="black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth=0.1, color="black")
gridExtra::grid.arrange(p1,p2,ncol=2)

sample_n(gh, N, replace=TRUE) %>%
    lm(son ~ father, data=.) %>% summary
lse %>% summarize(se_0=sd(beta_0), se_1=sd(beta_1))

lse %>% summarize(cor(beta_0, beta_1))
### correlation if predictor gets centralized
lse <- replicate(B, {
    sample_n(gh, N, replace=TRUE) %>%
        mutate(father = father - mean(father)) %>%
        lm(son ~ father, data=.) %>% .$coef
})
cor(lse[1,], lse[2,])

gh %>% ggplot(aes(x=father, y=son)) +
    geom_point() +
    geom_smooth(method="lm")

gh %>%
    mutate(Y_hat = predict(lm(son~father, data=.))) %>%
    ggplot(aes(x=father, y=Y_hat)) +
    geom_line()

fit <- gh %>% lm(son ~ father, data=.)
Y_hat <- predict(fit, se.fit=TRUE)
names(Y_hat)



### Advanced dplyr: Tibbles
library(tidyverse)
library(Lahman)
data(Teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR=round(HR/G, 1),
           BB=BB/G,
           R=R/G) %>%
    select(HR, BB, R) %>%
    filter(HR >= 0.4 & HR <= 1.2)

dat %>% group_by(HR) %>%
    summarize(slope=cor(R,BB)*sd(R)/sd(BB))
### lm() ignores group_by()
dat %>% group_by(HR) %>%
    lm(R ~ BB, data=.) %>%
    .$coef

### Tibbles
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

tibble(id=c(1,2,3), func = c(mean, median, sd))

dat %>% group_by(HR) %>%
    do(fit = lm(R ~ BB, data=.))

get_slope <- function(data){
    fit <- lm(R ~ BB, data=data)
    data.frame(slope=fit$coefficients[2],
               se=summary(fit)$coefficient[2,2])
}
dat %>% group_by(HR) %>%
    do(get_slope(.))

dat %>% group_by(HR) %>%
    do(slope = get_slope(.)) ## get a data frame

get_lse <- function(data){
    fit <- lm(R ~ BB, data=data)
    data.frame(term=names(fit$coefficients),
               slope=fit$coefficients,
               se=summary(fit)$coefficient[,2])
}
dat %>% group_by(HR) %>%
    do(get_lse(.))

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)

  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))



### broom
library(broom)
library(tidyverse)
library(Lahman)
data(Teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR=round(HR/G, 1),
           BB=BB/G,
           R=R/G) %>%
    select(HR, BB, R) %>%
    filter(HR >= 0.4 & HR <= 1.2)

fit <- lm(R ~ BB, data=dat)
tidy(fit, conf.int=TRUE)
names(tidy(fit))
class(tidy(fit))
tidy(fit)$estimate
### because the outcome is a data frame we can immediately use it with do
dat %>%
    group_by(HR) %>%
    do(tidy(lm(R~BB,data=.), conf.int=TRUE))

dat %>%
    group_by(HR) %>%
    do(tidy(lm(R~BB,data=.), conf.int=TRUE)) %>%
    filter(term == "BB") %>%
    select(HR, estimate, conf.low, conf.high) %>%
    ggplot(aes(HR, y=estimate, ymin=conf.low, ymax=conf.high)) +
    geom_errorbar() +
    geom_point()

glance(fit)

### Question
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(HR = HR/G,
           R = R/G) %>%
    select(lgID, HR, BB, R)

dat %>% 
    group_by(lgID) %>% 
    do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
    filter(term == "HR")


### Building a better offensive model for baseball
fit <- Teams %>%
    filter(yearID %in% 1961:2001) %>%
    mutate(HR=HR/G,
           BB=BB/G,
           R=R/G) %>%
    lm(R ~ BB + HR, data=.)
tidy(fit, conf.int=TRUE)

fit <- Teams %>%
    filter(yearID %in% 1961:2001) %>%
    mutate(HR=HR/G,
           singles=(H-X2B-X3B-HR)/G,
           doubles=X2B/G,
           triples=X3B/G,
           BB=BB/G,
           R=R/G) %>%
    lm(R ~ BB + singles + doubles + triples + HR, data=.)
coefs <- tidy(fit, conf.int=TRUE)

Teams %>%
    filter(yearID %in% 2002) %>%
    mutate(HR=HR/G,
           singles=(H-X2B-X3B-HR)/G,
           doubles=X2B/G,
           triples=X3B/G,
           BB=BB/G,
           R=R/G) %>%
    mutate(R_hat=predict(fit, newdata=.)) %>%
    ggplot(aes(R_hat, R, label = teamID)) + 
    geom_point() +
    geom_text(nudge_x=0.05, cex = 2) + 
    geom_abline()

pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
    group_by(teamID) %>%
    summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
    .$pa_per_game %>% 
    mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
    group_by(playerID) %>%
    mutate(PA = BB + AB) %>%
    summarize(G = sum(PA)/pa_per_game,
              BB = sum(BB)/G,
              singles = sum(H-X2B-X3B-HR)/G,
              doubles = sum(X2B)/G, 
              triples = sum(X3B)/G, 
              HR = sum(HR)/G,
              AVG = sum(H)/sum(AB),
              PA = sum(PA)) %>%
    filter(PA >= 300) %>%
    select(-G) %>%
    mutate(R_hat = predict(fit, newdata = .))

players %>% ggplot(aes(R_hat)) + 
    geom_histogram(binwidth = 0.5, color = "black")

players <- Salaries %>% 
    filter(yearID == 2002) %>%
    select(playerID, salary) %>%
    right_join(players, by="playerID")

players <- Fielding %>% filter(yearID == 2002) %>%
    filter(!POS %in% c("OF","P")) %>%
    group_by(playerID) %>%
    top_n(1, G) %>%
    filter(row_number(G) == 1) %>%
    ungroup() %>%
    select(playerID, POS) %>%
    right_join(players, by="playerID") %>%
    filter(!is.na(POS)  & !is.na(salary))

players <- Master %>%
    select(playerID, nameFirst, nameLast, debut) %>%
    right_join(players, by="playerID")

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
    arrange(desc(R_hat)) %>% 
    top_n(10) 

players %>% ggplot(aes(salary, R_hat, color = POS)) + 
    geom_point() +
    scale_x_log10()

players %>% filter(debut < 1998) %>%
    ggplot(aes(salary, R_hat, color = POS)) + 
    geom_point() +
    scale_x_log10()


### Regression Fallacy
library(Lahman)
playerInfo <- Fielding %>% 
    group_by(playerID) %>% 
    arrange(desc(G)) %>%
    slice(1) %>%
    ungroup %>% 
    left_join(Master, by="playerID") %>% 
    select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>% 
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>% 
  mutate(AVG = H/AB) %>% 
  filter(POS != "P")

ROY <- ROY %>%  
    filter(yearID == rookie_year | yearID == rookie_year+1) %>% 
    group_by(playerID) %>% 
    mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
    filter(n() == 2) %>% 
    ungroup %>%
    select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG) 

ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

two_years <- Batting %>% 
    filter(yearID %in% 2013:2014) %>% 
    group_by(playerID, yearID) %>%  
    filter(sum(AB) >= 130) %>% 
    summarize(AVG = sum(H)/sum(AB)) %>% 
    ungroup %>% 
    spread(yearID, AVG) %>% 
    filter(!is.na(`2013`) & !is.na(`2014`)) %>%
    left_join(playerInfo, by="playerID") %>% 
    filter(POS!="P") %>% 
    select(-POS) %>%
    arrange(desc(`2013`)) %>% 
    select(-playerID)

two_years %>% 
    ggplot(aes(`2013`, `2014`)) + 
    geom_point()


### Measurement Error Models
library(dslabs)

falling_object <- rfalling_object()
falling_object %>% 
    ggplot(aes(time, observed_distance)) + 
    geom_point() +
    ylab("Distance in meters") + 
    xlab("Time in seconds")

fit <- falling_object %>% 
    mutate(time_sq = time^2) %>% 
    lm(observed_distance~time+time_sq, data=.)
tidy(fit)

augment(fit) %>% 
    ggplot() +
    geom_point(aes(time, observed_distance)) + 
    geom_line(aes(time, .fitted), col = "blue")



### Confounding

