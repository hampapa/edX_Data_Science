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

c_df <- function(y_sl=1, y_sd=1){
    x <- rnorm(n=1000,mean=0,sd=2)
    y_base <- y_sl*x
    error <- rnorm(n=length(y_base), mean=0, sd=y_sd)
    y <- y_base + error
    return(data.frame(x=x, y=y))
}

df1 <- c_df()
df1 %>% ggplot(aes(x=x,y=y)) +
    geom_point(alpha=0.5) +
    theme_bw()
df1 %>% summarize(cor(x,y))
