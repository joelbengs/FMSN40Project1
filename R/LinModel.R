#### Linear Model ####
#Read data
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)
library(ggplot2)

#Plot data
ggplot(data = plasma, aes(x = age, y = betaplasma)) + geom_point()

#Create linear model
linmodel <- lm(betaplasma ~ age, data = plasma)
linmodel.coeff <-linmodel$coefficients
linmodel.coeff
confint(linmodel)

#Plot data
plasma.pred <- 
  cbind(plasma, 
        fit = predict(linmodel),
        conf = predict(linmodel, interval = "confidence"))

plasma.pred$conf.fit <- NULL
head(plasma.pred)

(
  plot.data <- 
    ggplot(data = plasma.pred, aes(x = age, y = betaplasma)) + 
    geom_point(size = 1) +
    xlab("Age") +
    ylab("Concentration of beta carotene (ng/ml)") +
    labs(title = "How the concentration of beta carotene plasma varies with age") +
    theme(text = element_text(size = 14))
)

(
  plot.line <- plot.data + 
    geom_line(aes(y = fit), color = "blue", size = 1) +
    labs(caption = "data and fitted line")
)

(
  plot.conf <- plot.line + 
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2)
)

#Calculate residuals
plasma.pred$e <- linmodel$residuals
head(plasma.pred$e)

#Plot residuals
ggplot(data = plasma.pred, aes(x = fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  xlab("Predicted concentration plasma (ng/ml)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) + 
  geom_smooth()

ggplot(data = plasma.pred, aes(x = age, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  xlab("Predicted concentration plasma (ng/ml)") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs age") +
  theme(text = element_text(size = 18)) + 
  geom_smooth()

#Q-Q-Plot
ggplot(data = plasma.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() + 
  labs(tag = "C") + 
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))

#Histogram
ggplot(data = plasma.pred, aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 18))


