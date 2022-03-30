
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

#F-test. when storing as a variable, the P-value is not included,
#only F-value and the necessary data for calculation of P-value.
#However, when we (summary), p-value is presented in the console at the last row.
(bp.loglinmodel.summary <- summary(bp.loglinmodel))

