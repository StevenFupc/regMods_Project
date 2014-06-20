library(ggplot2)
data(mtcars)

plot(mtcars)

# Model 1: mpg-am
mpg_am <- lm(mpg ~ am, data=mtcars)
summary(mpg_am)

ggplot(mtcars, aes(x=am, y=mpg)) +
    geom_point() +
    geom_abline(intercept=17, slope=7.2)

# We'll add two more predictors, which hopefully will explain the most remaining variance. We're stopping at a 3-form model to maintain some amount of interpretability, although the interaction terms between our predictors will make this difficult.

variablesLeft <- setdiff(names(mtcars), c("mpg", "am"))

models1 <- lapply(variablesLeft, FUN=function(x) {
    form <- paste0("mpg ~ am + ", x)
    lm(form, data=mtcars)
})

cbind(variablesLeft, adj.r.squared=sapply(models1, FUN=function(x) summary(x)$adj.r.squared))

summary(models1[[3]])

ggplot(mtcars, aes(x=hp, y=mpg, color=as.factor(am))) +
    geom_point()

# Here, we find that HP is the best independent covariate. Let's find one more:
variablesLeft <- setdiff(variablesLeft, "hp")
models2 <- lapply(variablesLeft, FUN=function(x) {
    form <- paste0("mpg ~ am + hp + ", x)
    lm(form, data=mtcars)
})

cbind(variablesLeft, adj.r.squared=sapply(models2, FUN=function(x) summary(x)$adj.r.squared))

# And our winner is weight; let's take a look at the fit now.

summary(models2[[4]])

ggplot(mtcars, aes(x=hp, y=mpg, color=as.factor(am))) +
    geom_point(aes(size=wt))

plot(models2[[4]])
