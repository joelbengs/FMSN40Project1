#### Log-Lin Model ####

plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)
library(ggplot2)

plasmanew <- plasma[plasma$betaplasma > 0, ]

ggplot(data = plasmanew, aes(x = age, y = log(betaplasma))) + geom_point()

#Log-lin model
logmodel <- lm(log(betaplasma) ~ age, data = plasmanew)
logmodel$coefficients
confint(logmodel)

#Plot data
plasma.pred <- 
  cbind(plasmanew, 
        fit = predict(logmodel),
        conf = predict(logmodel, interval = "confidence"))

plasma.pred$conf.fit <- NULL
head(plasma.pred)

(
  plot.data <- 
    ggplot(data = plasma.pred, aes(x = age, y = log(betaplasma))) + 
    geom_point(size = 1) +
    xlab("Age (years)") +
    ylab("Concentration of beta carotene (log(ng/ml)") +
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
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    labs(caption = "data, fitted line and confidence interval")
)

#Calculate residuals
plasma.pred$e <- logmodel$residuals
head(plasma.pred$e)

#Plot residuals
ggplot(data = plasma.pred, aes(x = fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  xlab("Predicted concentration plasma (log(ng/ml)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 18)) + 
  geom_smooth()

ggplot(data = plasma.pred, aes(x = age, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  xlab("Age (years)") +
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

#Plot data, fitted line, confidence, prediction
plasma.pred <- 
  cbind(plasmanew, 
        fit = predict(logmodel),
        conf = predict(logmodel, interval = "confidence"),
        pred = predict(logmodel, interval = "prediction"))
plasma.pred$conf.fit <- plasma.pred$pred.fit <- NULL
head(plasma.pred)


(
  plot.data <- 
    ggplot(data = plasma.pred, aes(x = age, y = log(betaplasma))) + 
    geom_point(size = 1) +
    xlab("Age (years)") +
    ylab("Concentration of beta carotene (log(ng/ml)") +
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
    geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
    labs(caption = "data, fitted line and confidence interval")
)

(
  plot.pred <- plot.conf + 
    geom_line(aes(y = pred.lwr),
              color = "red", linetype = "dashed", size = 1) +
    geom_line(aes(y = pred.upr),
              color = "red", linetype = "dashed", size = 1) +
    labs(caption = "data, fitted line, 95% confidence and prediction intervals")
)





#### Prediction Intervals ####
plasma.x0 <- data.frame(age = c(25, 26, 75, 76))
plasma.x0

(plasma.y0.pred = cbind(plasma.x0,
                   fit = predict(logmodel, plasma.x0),
                   conf = predict(logmodel, plasma.x0, interval = "confidence"),
                   pred = predict(logmodel, plasma.x0, interval = "prediction")))

plasma.y0.pred$conf.fit <- plasma.y0.pred$pred.fit <- NULL
plasma.y0.pred
diff25 <- exp(plasma.y0.pred$fit[2]) - exp(plasma.y0.pred$fit[1])
diff75 <- exp(plasma.y0.pred$fit[4]) - exp(plasma.y0.pred$fit[3])
diff25
diff75

exp(plasma.y0.pred)


#### 

#### Categorical Variables ####
summary(plasmanew)
head(plasmanew)

#Introduce categorical variables
plasmanew$sex <- factor(plasmanew$sex,
                          levels = c(1, 2),
                          labels = c("Male", "Female"))

plasmanew$smokstat <- factor(plasmanew$smokstat,
                          levels = c(1, 2, 3),
                          labels = c("Never", "Former", "Current Smoker"))

plasmanew$bmicat <- factor(plasmanew$bmicat,
                           levels = c(1, 2, 3, 4),
                           labels = c("Underweight", "Normal", "Overweight", 
                                      "Obese"))
#Frequency tables
table(plasmanew$sex)
table(plasmanew$smokstat)
table(plasmanew$bmicat)

#BMI Model
bmi.model <- lm(log(betaplasma) ~ bmicat, data = plasmanew)
bmi.model$coefficients

summary(bmi.model)

#Change reference to "Normal"
plasmanew$bmicat <- relevel(plasmanew$bmicat, ref = "Normal")
bmi.model <- lm(log(betaplasma) ~ bmicat, data = plasmanew)
bmi.model$coefficients
summary(bmi.model)

#Sex Model
plasmanew$sex <- relevel(plasmanew$sex, ref = "Female")
sex.model <- lm(log(betaplasma) ~ sex, data = plasmanew)
sex.model$coefficients
summary(sex.model)

#SmokStat Model
smokstat.model <- lm(log(betaplasma) ~ smokstat, data = plasmanew)
smokstat.model$coefficients
summary(smokstat.model)

#Model with age, sex, smokstat, bmicat
fullmodel <- lm(log(betaplasma) ~ age + sex + smokstat + bmicat, data = plasmanew)
fullmodel$coefficients
confint(fullmodel)
exp(confint(fullmodel))

##Test 1: Is this model better than the null model?
sum.full <- summary(fullmodel)
sum.full
sum.full$coefficients
# If P(F(4, 309) > 13.85) = 3.786e-09 < 0.05 => reject H0 
# => the full model is better than the null model

# Test 2: Is this model better than only using age?
logmodel <- lm(log(betaplasma) ~ age, data = plasmanew)

# Compare the models:
(plasma.anova <- anova(logmodel, fullmodel))

# Compare the F-value with upper F-quantile:
Fvalue <- plasma.anova$F[2]
qf(1 - 0.05, 6, 306)

# Calculate P-value:
pf(Fvalue, 6, 306, lower.tail = FALSE)
plasma.anova$`Pr(>F)`[2]
#P-value = 1.76346e-08 < 0.05 => reject H0 => full model is better than reduced model


# Test 3: Are the variables significant?
sum.full$coefficients[, "Pr(>|t|)"]


# Test 4: Is underweight significantly different from normalweight?
bmi.model <- lm(log(betaplasma) ~ bmicat, data = plasmanew)
sum.bmi <- summary(bmi.model)
sum.bmi
sum.bmi$coefficients[, "Pr(>|t|)"]








#### Plot ####
plasma.pred <- cbind(plasmanew,
                     fit = )