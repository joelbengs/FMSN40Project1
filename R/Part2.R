
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

#### Test 1 ####
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

plasma.x0 <- data.frame(age = c(mean(plasma$age)), bmicat = "Underweight", smokstat = "Former", sex = "Male")

plasma.pred <- cbind(
  plasma.x0,
  fit = predict(bp.loglinmodel, plasma.x0),
  conf = predict(bp.loglinmodel, plasma.x0, interval = "confidence"),
  pred = predict(bp.loglinmodel, plasma.x0, interval = "prediction")
)
plasma.pred$conf.fit <- plasma.pred$pred.fit <- NULL
(plasma.pred)
(summary(plasma))
(summary(bp.loglinmodel))

#### Part 2e - continous bmi (quetelet) ####
#Build model and show beta, expbeta and confidence of exbeta as table.
Qmodel <- lm(log(betaplasma) ~ age + sex + smokstat + quetelet, data = plasma)
part2E.table <- cbind(
  beta = Qmodel$coefficients,
  expbeta = exp(Qmodel$coefficients),
  expconf = exp(confint(Qmodel))
)
(part2E.table.round <- round(part2E.table, digits = 2))

# Estimate the future for tw humans with two different models.
Eman <- data.frame(sex = "Male", age = c(40), smokstat = "Former", quetelet = c(22))
Ewoman <- data.frame(sex = "Female", age = c(40), smokstat = "Former", quetelet = c(22))
Emancat <- data.frame(sex = "Male", age = c(40), smokstat = "Former", bmicat = "Normal")
Ewomancat <- data.frame(sex = "Female", age = c(40), smokstat = "Former", bmicat = "Normal")

#Qmodel, with continous quetelet isntead of BMI
Eman.Qmodel.pred <- cbind(
  Eman,
  fit = predict(Qmodel, Eman),
  conf = predict(Qmodel, Eman, interval = "confidence"),
  pred = predict(Qmodel, Eman, interval = "prediction")
)
Eman.Qmodel.pred$conf.fit <- Eman.Qmodel.pred$pred.fit <- NULL

Ewoman.Qmodel.pred <- cbind(
  Ewoman,
  fit = predict(Qmodel, Ewoman),
  conf = predict(Qmodel, Ewoman, interval = "confidence"),
  pred = predict(Qmodel, Ewoman, interval = "prediction")
)
Ewoman.Qmodel.pred$conf.fit <- Ewoman.Qmodel.pred$pred.fit <- NULL

#Old model, with gategrical BMI
Eman.cat.pred <- cbind(
  Emancat,
  fit = predict(bp.loglinmodel, Emancat),
  conf = predict(bp.loglinmodel, Emancat, interval = "confidence"),
  pred = predict(bp.loglinmodel, Emancat, interval = "prediction")
)
Eman.cat.pred$conf.fit <- Eman.cat.pred$pred.fit <- NULL

Ewoman.cat.pred <- cbind(
  Ewomancat,
  fit = predict(bp.loglinmodel, Ewomancat),
  conf = predict(bp.loglinmodel, Ewomancat, interval = "confidence"),
  pred = predict(bp.loglinmodel, Ewomancat, interval = "prediction")
)
Ewoman.cat.pred$conf.fit <- Ewoman.cat.pred$pred.fit <- NULL

#merge into one data structure. BMI and quetelet has to be removed for merge to work
Eman.Qmodel.pred$quetelet <- Eman.cat.pred$bmicat <- Ewoman.Qmodel.pred$quetelet <- Ewoman.cat.pred$bmicat <- NULL
Modelcomp <- rbind(
  Eman.Qmodel.pred,
  Eman.cat.pred,
  Ewoman.Qmodel.pred,
  Ewoman.cat.pred
)

## Change BMI = 22 = Normal to BMI = 33 = obese
# For categorical model (bp.loglinmodel) the addative change in fatness wll give
# a relative change in betaplasma level. Ynew = Yold * e^beta(obese)
# Same for woman and men as the model is the same.
#REMINDER: ADD THE CONFint
catChangeFactor <- cbind(
  relativeExpChange = exp(bp.loglinmodel$coefficients["bmicatObese"]),
  conf.lwr = exp(confint(bp.loglinmodel)["bmicatObese", "2.5 %"]),
  conf.upr = exp(confint(bp.loglinmodel)["bmicatObese", "97.5 %"])
)
(catChangeFactor)
# For continous model (Qmodel), we have to multiply by step length (33-22 = 11)
contChangeFactor <- cbind(
  relativeExpChange = 11*exp(Qmodel$coefficients["quetelet"]),
  conf.lwr = 11*exp(confint(Qmodel)["quetelet", "2.5 %"]),
  conf.upr = 11*exp(confint(Qmodel)["quetelet", "97.5 %"])
)
(contChangeFactor)

#### part 2f ####
#double model, both bmicat and quetelet
Dmodel <- lm(log(betaplasma) ~ age + sex + smokstat + bmicat + quetelet, data = plasma )
(confint(Dmodel)) # No, quetelet and BMIcat are both non-significant as the confidence interval includes zero.

