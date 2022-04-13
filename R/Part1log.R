#Preparations
library(ggplot2)
plasma <- read.delim("Data/plasma.txt")

# LOG-LIN MODEL 
#### Make calculations ####
#Removes the problematic observation of betaplasma = 0, that can't be log:ed.
#the function removes the entire row 0, but keeps all other data
newplasma <- plasma[plasma$betaplasma > 0, ]

## simple view of the data when using log-y-scale
ggplot(data = newplasma, aes(x = age, y = log(betaplasma))) + geom_point()

#log-lin model of betaplasma against age
bp.loglinmodel <- lm(log(betaplasma) ~ age, data = newplasma)

#get estimate Y-hat - Y-hat is now a function handle for the whole linear model!
newplasma$yhat <- predict(bp.loglinmodel)

#Get confidence intervals and prediction intervals as lines.
#cbind is a function to combind different data sets.
#"confidence" and "prediction" are parameters to the inteligent function predict.
bp.loglinpred <- cbind(newplasma, 
                       fit = predict(bp.loglinmodel),
                       conf = predict(bp.loglinmodel, interval = "confidence"),
                       pred = predict(bp.loglinmodel, interval = "prediction")
)
# We don't need the average of the confidence or the prediction intervals, as that is precisely the model itself. So we remove them
bp.loglinpred$conf.fit <- bp.loglinpred$pred.fit <- NULL


#### Complete plot with data, fitted line, confidence, and prediction intervals ####
(
  plot.linpred <- 
    ggplot(data = bp.loglinpred, aes(x = age, y = log(betaplasma))) 
  + geom_point() 
  + geom_line(aes(y = fit), color = "blue", size = 1) 
  + geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2)
  + geom_line(data = bp.loglinpred, aes(y = pred.lwr), color = "red", linetype = "dashed", size = 1)
  + geom_line(data = bp.loglinpred, aes(y = pred.upr), color = "red", linetype = "dashed", size = 1)
  + xlab("Length (cm)")
  + ylab("Weight (g)")
  + labs(title = "Betaplasma against age, log-linear model")
  + labs(caption = "fitted log-linear model, 95% conf. and pred. intervals")
  + theme(text = element_text(size = 18))
)


#### Basic residual analysis ####
# Add the residuals to the predicted data, name them e.log (a vector of residuals).
# Used in plotting as PRED is the dataset for that, not MODEL
bp.loglinpred$e.log <- bp.loglinmodel$residuals

# Save the max-value in order to make the y-axis symmetrical 
# in the plots. c is a function that combine two values in a vector.
(bp.residualspan <- c(-max(abs(bp.loglinmodel$residuals)), max(abs(bp.loglinmodel$residuals))))

# Plot residuals against x, add a horizontal line at y=0,
# add a moving average, geom_smooth(), to see trends,
# and expand the y-axis to include +/- max residual.

# Residuals vs x-values
ggplot(data = bp.loglinpred, aes(x = age, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = bp.residualspan) +
  xlab("Age") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

# Residuals vs predicted values Y-hat
ggplot(data = bp.loglinpred, aes(x = fit, y = e.log)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = bp.residualspan) +
  xlab("Y-hat") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Normal Q-Q-plot of the residuals
ggplot(data = bp.loglinpred, aes(sample = e.log)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# Histogram of residuals
ggplot(data = bp.loglinpred, aes(x = e.log)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))

#### Prediction Intervals for young vs old person####
plasma.x0 <- data.frame(age = c(25, 26, 75, 76))
(plasma.y0.pred = cbind(plasma.x0,
                        fit = predict(bp.loglinmodel, plasma.x0),
                        conf = predict(bp.loglinmodel, plasma.x0, interval = "confidence"),
                        pred = predict(bp.loglinmodel, plasma.x0, interval = "prediction")))

plasma.y0.pred$conf.fit <- plasma.y0.pred$pred.fit <- NULL

diff25 <- cbind(
  fit = exp(plasma.y0.pred$fit[2]) - exp(plasma.y0.pred$fit[1]),
  pred.lwr = exp(plasma.y0.pred$pred.lwr[2]) - exp(plasma.y0.pred$pred.lwr[1]),
  pred.upr = exp(plasma.y0.pred$pred.upr[2]) - exp(plasma.y0.pred$pred.upr[1])
)

diff75 <- cbind(
  fit = exp(plasma.y0.pred$fit[4]) - exp(plasma.y0.pred$fit[3]),
  pred.lwr = exp(plasma.y0.pred$pred.lwr[4]) - exp(plasma.y0.pred$pred.lwr[3]),
  pred.upr = exp(plasma.y0.pred$pred.upr[4]) - exp(plasma.y0.pred$pred.upr[3])
)

differences <- rbind(
  diff25,
  diff75
)
(differences)

exp(plasma.y0.pred)

exp(confint(bp.loglinmodel))

#### 

