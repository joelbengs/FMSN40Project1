
library(ggplot2)
data <- read.delim("Data/plasma.txt")
plasma <- data[data$betaplasma > 0, ]
rm(data)

#### part2a ####

#categorical variables
plasma$sex <- factor(plasma$sex,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))
plasma$smokstat <- factor(plasma$smokstat,
                          levels = c(1,2,3),
                          labels = c("Never", "Former", "Current Smoker"))
plasma$bmicat <- factor(plasma$bmicat,
                        levels = c(1,2,3,4),
                        labels = c("Underweight", "Normal", "Overweight", "Obese"))

#Frequency Tables for categorical variables
table(plasma$sex)
table(plasma$smokstat)
table(plasma$bmicat)

#### Part2b ####
#Log-lin model of betaplasma as a function of BMI. More weight -> less Bp.
#The first label is default reference category
(bp.loglin.bmi <- lm(log(betaplasma) ~ bmicat, data = plasma))

#Change the reference category and do new model
plasma$bmicat <- relevel(plasma$bmicat, "Normal")
(bp.loglin.bmirelevel <- lm(log(betaplasma) ~ bmicat, data = plasma))

#Relevel sek. Smokestat does not need relevel.
plasma$sex <- relevel(plasma$sex, "Female")

#### Part2c ####

#fit a model of the continuous variable age and the categorical variables above, releveld if needed.
#The coefficients of this model say how betaplasma changes whit different categories.
#Betaplasma is a positive thing for ones health
(bp.loglinmodel <- lm(log(betaplasma) ~ age + sex + smokstat + bmicat, data = plasma))

#table of estimates of the model
(
part2c <- cbind(
  beta = bp.loglinmodel$coefficients,
  confint(bp.loglinmodel),
  ebeta = exp(bp.loglinmodel$coefficients),
  exp(confint(bp.loglinmodel)))
)

#### Test1 ####
## Global F-test. Tells if this model is better than nothing. That is, at least one Beta is significant
#F-test. when storing as a variable, the P-value is not included,
#only F-value and the necessary data for calculation of P-value.
#However, when we (summary), p-value is presented in the console at the last row.
(bp.loglinmodel.summary <- summary(bp.loglinmodel))

#### Test 2 ####
Partial F-test for soil
# Fit the reduced model without soil:
model.reduced <- lm(headwt ~ fertilize, data = cabbage)

# Compare the models:
(cabbage.anova <- anova(model.reduced, model.full))
#### Test 3 ####
#T-TEST. Get the T-values for each beta, extracted from summary.
bp.loglinmodel.summary <- summary(bp.loglinmodel)
beta.tvalues <- bp.loglinmodel.summary$coefficients[ ,3]
degreesfreedom <- 306 #n - (p+1) degrees of freedom
a <- qt(1 - 0.05/2, 306)
sigvector <- c(a,a,a,a,a,a,a,a)
result <- (sigvector - beta.tvalues) < 0
part2c <- cbind(part2c, significant = result)

#### Test 4 ####
# Test 4: Is underweight significantly different from normalweight?
bmi.model <- lm(log(betaplasma) ~ bmicat, data = plasma)
sum.bmi <- summary(bmi.model)
(sum.bmi$coefficients[, "Pr(>|t|)"])
#### Part 2d - plotting ####

plasma <- cbind(
  plasma,
  fit = predict(bp.loglinmodel),
  conf = predict(bp.loglinmodel, interval = "confidence"),
  pred = predict(bp.loglinmodel, interval = "prediction")
)

plasma$conf.fit <- plasma$pred.fit <- NULL

ggplot(data = plasma, aes(x = age, y = log(betaplasma), color = sex)) +
  geom_point() + 
  facet_grid(smokstat ~ relevel(bmicat, "Underweight")) +
  geom_line( aes(y = fit)) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) + 
  geom_line(aes(y = pred.lwr), linetype = "dashed") + 
  geom_line(aes(y = pred.upr), linetype = "dashed")
