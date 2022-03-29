#Our first script
library(ggplot2)
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)

#simple plot of betaplasma against age
ggplot(plasma, aes(x = age, y = betaplasma)) + geom_point()

### LINEAR MODEL ###
#### Make calculations ####
#linear model of betaplasma against age
bp.linmodel <- lm(betaplasma ~ age, data = plasma)
#extract coefficients of linear model
bp.linmodel.coefficients <- bp.linmodel$coefficients
#show the coefficients
(bp.linmodel.coefficients)
#simplest way to get confidence intervals for the model
confint(bp.linmodel)

#get estimate Y-hat - yhat is now a function handle for the whole linear model!
plasma$yhat <- predict(bp.linmodel)

#Get confidence intervals and prediction intervals as lines.
#cbind is a function to combind different data sets.
#"confidence" and "prediction" are parameters to the inteligent function predict.
bp.linpred <- cbind(plasma, 
                    fit = predict(bp.linmodel),
                    conf = predict(bp.linmodel, interval = "confidence"),
                    pred = predict(bp.linmodel, interval = "prediction"))
head(bp.linpred)
#We don't need the average of the confidence or the prediction intervals, as that is precisely the model itself. So we remove them
bp.linpred$conf.fit <- bp.linpred$pred.fit <- NULL

#### Simple plot against data ####

#PLOT - plot the linear model in the same graph as the data points.
#Do a ggplot and save in plot.data
#add labels to previous plot
#Add line to previous plot
(plot.data <- ggplot(data = plasma, aes(x = age, y = betaplasma)) + geom_point())
(plot.labels <- plot.data +
    xlab("Age") +
    ylab("betaplasma") +
    labs(title = "betaplasma against age") +
    theme(text = element_text(size = 18))
)
(plot.line <- plot.labels + 
    geom_line(aes(x = age, y = yhat), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)

#### complete plot with data, fitted line, confidence, and prediction intervals ####
(
  plot.linpred <- 
    ggplot(data = bp.linpred, 
           aes(x = age, y = betaplasma)) + 
    geom_point() +
    geom_line(aes(y = fit), color = "blue", size = 1) +
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    geom_line(data = bp.linpred, aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(data = bp.linpred, aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    xlab("Length (cm)") +
    ylab("Weight (g)") +
    labs(title = "Betaplasma against age, linear model") +
    labs(caption = 
           "fitted linear model, 95% conf. and pred. intervals") +
    theme(text = element_text(size = 18))
)
#### Basic residual analysis ####

### Add the residuals to the predicted data, name them e.lin (a vector of residuals)
bp.linpred$e.lin <- bp.linmodel$residuals
head(bp.linpred)

# Save the max-value in order to make the y-axis symmetrical 
# in the plots. c is a function that combine two values in a vector.
(bp.elin <- max(abs(bp.linpred$e.lin)))
(bp.lim.elin <- c(-max.elin, max.elin))


# Plot residuals against x, add a horizontal line at y=0,
# add a moving average, geom_smooth(), to see trends,
# and expand the y-axis to include +/- max residual.

# Residuals vs x-values
ggplot(data = bp.linpred, aes(x = age, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = bp.lim.elin) +
  xlab("Length (cm)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs x-values") +
  theme(text = element_text(size = 18))

# Residuals vs predicted values Y-hat
ggplot(data = bp.linpred, aes(x = fit, y = e.lin)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth() +
  expand_limits(y = bp.lim.elin) +
  xlab("Predicted weight (g)") +
  ylab("Residual") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18))

# Normal Q-Q-plot of the residuals
ggplot(data = bp.linpred, aes(sample = e.lin)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

# Histogram of residuals
ggplot(data = bp.linpred, aes(x = e.lin)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))




### LOGARITHMC MODEL ###

#### Make calculations ####
#Removes the problematic observation of betaplasma = 0, that can't be logged.
#the function removes the entire row 0, but keeps all other data
newplasma <- plasma[plasma$betaplasma > 0, ]

## simple view of the data when using log-y-scale
ggplot(data = newplasma, aes(x = age, y = log(betaplasma))) + geom_point()

#log-lin model of betaplasma against age
bp.loglinmodel <- lm(log(betaplasma) ~ age, data = newplasma)
#extract coefficients of linear model
bp.loglinmodel.coefficients <- bp.loglinmodel$coefficients
#show the coefficients
(bp.linmodel.coefficients)
#simplest way to get confidence intervals for the model
confint(bp.linmodel)

#get estimate Y-hat - yhat is now a function handle for the whole linear model!
newplasma$yhat <- predict(bp.loglinmodel)

#Get confidence intervals and prediction intervals as lines.
#cbind is a function to combind different data sets.
#"confidence" and "prediction" are parameters to the inteligent function predict.
bp.loglinpred <- cbind(newplasma, 
                    fit = predict(bp.loglinmodel),
                    conf = predict(bp.loglinmodel, interval = "confidence"),
                    pred = predict(bp.loglinmodel, interval = "prediction")
)
head(bp.loglinpred)
# We don't need the average of the confidence or the prediction intervals, as that is precisely the model itself. So we remove them
bp.loglinpred$conf.fit <- bp.loglinpred$pred.fit <- NULL

#### Simple plot against data ####

#PLOT - plot the linear model in the same graph as the data points.
#Do a ggplot and save in plot.data
#add labels to previous plot
#Add line to previous plot
(plot.data <- ggplot(data = newplasma, aes(x = age, y = log(betaplasma)) + geom_point())
(plot.labels <- plot.data +
    xlab("Age") +
    ylab("betaplasma") +
    labs(title = "betaplasma against age") +
    theme(text = element_text(size = 18))
)
(plot.line <- plot.labels + 
    geom_line(aes(x = age, y = yhat), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)