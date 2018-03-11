
rm(list = ls())


library(data.table)
library(magrittr)
library(robustbase)
library(caret)
library(ggplot2)
library(gapminder)



fitModel <- function(df){
  tryCatch({
    df <- na.omit(df)
    df <- df[lifeExp > 0 & gdpPercap > 0]
    fitControl <- trainControl(method="repeatedcv", number=20, repeats=10)
    rlmfit <- train(lifeExp ~ gdpPercap, data = df, 
                    method = "rlm", 
                    trControl = fitControl,
                    ## This last option is actually one
                    ## for gbm() that passes through
                    metric = "RMSE")
    rlmfit
  }, error = function(e){
    NA
  })
}

getCoef <- function(model){
  model$finalModel$coefficients
}

getGoodnessOfFit <- function(model){
  finalModel <- model$finalModel
  pred_ <- predict(model, finalModel$model)
  postResample(pred = pred_, obs = finalModel$model$.outcome)
}

getPvalue <- function(model){
  summary_ <- summary(model)
  tvalue <- summary_$coefficients[2,3]
  degreeOfFreedom <- summary_$df[2]
  pvalue <- 2 * pt(abs(tvalue), degreeOfFreedom, lower.tail = FALSE)
  pvalue
}

testSignificance <- function(Pvalue, conf.level = 0.95){
  test <- Pvalue < 1-conf.level
  test
}

df <- gapminder %>% data.table()

df %>%
  ggplot(aes(x=gdpPercap, y=lifeExp)) + 
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

modelMethod <- function(df, method="loglog"){
  switch(method,
         loglog = df[ ,`:=` (gdpPercap = log(gdpPercap), lifeExp = log(lifeExp))],
         loglinear = df[ ,`:=` (gdpPercap = log(gdpPercap))],
         linearlog = df[ ,`:=` (lifeExp = log(lifeExp))]
  )
}

modelMethod(df, "loglog")

df_grouped <- df[, .(data = .(.SD)), by=continent]
df_grouped[, model      := lapply(data, fitModel)]
df_grouped[, slope      := lapply(model, function(x) getCoef(x)[2])]
df_grouped[, intercept  := lapply(model, function(x) getCoef(x)[1])]
df_grouped[, RMSE       := lapply(model, function(x) getGoodnessOfFit(x)[1])]
df_grouped[, R2         := lapply(model, function(x) getGoodnessOfFit(x)[2])]
df_grouped[, PValue     := lapply(model, getPvalue)]
df_grouped[, PVAlueTest := testSignificance(PValue, 0.95)]



df_grouped



