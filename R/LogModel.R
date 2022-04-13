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

exp(confint(logmodel))

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
(Fvalue <- plasma.anova$F[2])
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
                     fit = predict(fullmodel),
                     conf = predict(fullmodel, interval = "confidence"),
                     pred = predict(fullmodel, interval = "prediction"))

plasma.pred$conf.fit <- plasma.pred$pred.fit <- NULL

ggplot(data = plasma.pred, aes(x = age, y = log(betaplasma), color = sex)) + geom_point() +
  geom_line(aes(y = fit)) + 
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) + 
  geom_line(aes(y = pred.lwr),linetype = "dashed", size = 1) +
  geom_line(aes(y = pred.upr), linetype = "dashed", size = 1) + 
  facet_grid(smokstat ~ relevel(bmicat, "Underweight"))

#### Confidence and prediction intervals for a new data point ####
avg.age <- mean(plasmanew$age)
plasma.x0 <- data.frame(age = c(avg.age), bmicat = "Underweight", smokstat = "Former",
                        sex = "Male")
(plasma.y0 <- cbind(plasma.x0,
                   fit = predict(fullmodel, plasma.x0),
                   conf = predict(fullmodel, plasma.x0, interval = "confidence"),
                   pred = predict(fullmodel, plasma.x0, interval = "prediction")))

plasma.y0$conf.fit <- plasma.y0$pred.fit <- NULL

#### Continuous BMI ####
contbmi.model <- lm(log(betaplasma) ~ age + sex + smokstat + quetelet, data = plasmanew)
contbmi.model$coefficients
confint(contbmi.model)
exp(contbmi.model$coefficients)
exp(confint(contbmi.model))

#### Confidence and prediction intervals for a man and a woman ####
plasma.xwoman <- data.frame(age = c(40), sex = "Female", smokstat = "Former",
                         bmicat = "Normal", quetelet = c(22))
plasma.xman <- data.frame(age = c(40), sex = "Male", smokstat = "Former",
                             bmicat = "Normal", quetelet = c(22))

#Full model
plasma.ywoman <- cbind(plasma.xwoman,
                      fit = predict(fullmodel, plasma.xwoman),
                      conf = predict(fullmodel, plasma.xwoman, interval = "confidence"))
plasma.ywoman
(plasma.yman <- cbind(plasma.xman,
                      fit = predict(fullmodel, plasma.xman),
                      conf = predict(fullmodel, plasma.xman, interval = "confidence"))
)

#Continuous BMI
(plasma.ywoman <- cbind(plasma.xwoman,
                       fit = predict(contbmi.model, plasma.xwoman),
                       conf = predict(contbmi.model, plasma.xwoman, interval = "confidence"))
)

(plasma.yman <- cbind(plasma.xman,
                     fit = predict(contbmi.model, plasma.xman),
                     conf = predict(contbmi.model, plasma.xman, interval = "confidence"))
)

#The difference between BMI 22(normal) and 33(obese)
fullmodel$coefficients
confint(fullmodel)

beta.q <- contbmi.model$coefficients["quetelet"]
11*beta.q
11*confint(contbmi.model)


#### Model with bmicat and quetelet ####
bothbmi.model <- lm(log(betaplasma) ~ age + sex + smokstat + bmicat + quetelet, 
                    data = plasmanew)
confint(bothbmi.model)

#### Ranking the models ####
model.0 <- fullmodel
(sum.0 <- summary(model.0))

model.1 <- contbmi.model
(sum.1 <- summary(model.1))
anova(model.0, model.1)

(collect.R2s <- data.frame(
  nr = seq(1, 2),
  model = c("0.full model", "1.model with quetelet"),
  R2 = c(sum.0$r.squared,
         sum.1$r.squared),
  R2.adj = c(sum.0$adj.r.squared,
             sum.1$adj.r.squared)))

(collect.AIC <- data.frame(
  nr = seq(1, 2),
  model = c("0.year", "1.year+region"),
  AIC(model.0, model.1),
  BIC(model.0, model.1)))

# The model with quetelet is the better model => background model

#### 
#### Checking correlation between x-variables ####
contx <- plasmanew[, c("age", "quetelet", "calories", "fat", "fiber",
                    "alcohol", "cholesterol", "betadiet")]
library(ggplot2)
corr <- cor(contx)
pairs(contx)

#Correlation of 0.7 or more:
# calories/fat, fat/cholesterol
pairs(plasmanew[, c("fat", "cholesterol")])
pairs(plasmanew[, c("fat", "calories")])

table(plasmanew$vituse)
plasmanew$vituse <- factor(plasmanew$vituse,
                           levels = c(1, 2, 3),
                           labels = c("Yes, fairly often", "Yes, not often",
                                      "No"))
plasmanew$vituse <- relevel(plasmanew$vituse, ref = "Yes, fairly often")

# it a model using all the dietary variables, 
# vituse, calories, fat, fiber, alcohol, cholesterol, and betadiet
dietmodel <- lm(log(betaplasma) ~ vituse + calories + calories + fat + fiber +
                  alcohol + cholesterol + betadiet, data = plasmanew)
dietmodel$coefficients

#### Advanced Residual Analysis ####
# calculate leverage and studentized residuals
betaplasma.diet <- cbind(
  plasmanew,
  fit = predict(dietmodel),
  r = rstudent(dietmodel),
  v = influence(dietmodel)$hat
)

# plot age vs leverage to identify potentially influential points
ggplot(betaplasma.diet, aes(x = fit, y = v)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 1/nrow(plasmanew)) +
  geom_hline(yintercept = 2*length(dietmodel$coefficients)/nrow(plasmanew), 
             color = "red") +
  labs(title = "Betaplasma: leverage vs log length") +
  labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
  theme(text = element_text(size = 18))
  
v.strange <- which(betaplasma.diet$v >= 0.1)

table(betaplasma.diet$alcohol)

alcohol.strange <- which(betaplasma.diet$alcohol == 203)

# plotting age vs betaplasma highlighting high leverage points and high alcohol consumption
ggplot(betaplasma.diet, aes(age, log(betaplasma))) + 
  geom_point() +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 3, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 3, shape = 24) + 
  labs(title = "Age vs Betaplasma",
       caption = "") +
  theme(text = element_text(size = 18))

# plotting age vs studentized residuals 
ggplot(betaplasma.diet, aes(x = fit, y = r)) +
  geom_point(size = 1) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_smooth() +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ], 
             color = "blue", size = 4, shape = 24) +
  labs(title = "Betaplasma: residuals vs fitted values") +
  xlab("fitted values (log betaplasma)") +
  ylab("studentized residuals") +
  theme(text = element_text(size = 18))

ggplot(betaplasma.diet, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 1) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_smooth() +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  labs(title = "Betaplasma: constant variance?") +
  xlab("fitted values (log betaplasma)") +
  ylab("sqrt(|r*|)") +
  theme(text = element_text(size = 18))

# Cook's distance
betaplasma.diet$D <- cooks.distance(dietmodel)
head(betaplasma.diet)

r.large <- max(betaplasma.diet$r)

(f1 <- length(dietmodel$coefficients))
(f2 <- dietmodel$df.residual)
(cook.limit <- qf(0.5, f1, f2))
ggplot(betaplasma.diet, aes(fit, D)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  #geom_hline(yintercept = cook.limit, color = "red") +
  geom_hline(yintercept = 4/nrow(betaplasma.diet), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Beta plasma: Cook's D") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 14))

d.large <- max(betaplasma.diet$D)

# Calculate dfbetas
dietmodel$coefficients
betaplasma.diet$df0 <- dfbetas(dietmodel)[, "(Intercept)"]
betaplasma.diet$df1 <- dfbetas(dietmodel)[, "vituseYes, not often"]
betaplasma.diet$df2 <- dfbetas(dietmodel)[, "vituseNo"]
betaplasma.diet$df3 <- dfbetas(dietmodel)[, "calories"]
betaplasma.diet$df4 <- dfbetas(dietmodel)[, "fat"]
betaplasma.diet$df5 <- dfbetas(dietmodel)[, "fiber"]
betaplasma.diet$df6 <- dfbetas(dietmodel)[, "alcohol"]
betaplasma.diet$df7 <- dfbetas(dietmodel)[, "cholesterol"]
betaplasma.diet$df8 <- dfbetas(dietmodel)[, "betadiet"]
head(betaplasma.diet)

# Plotting dfbetas against the responding x-variable
ggplot(betaplasma.diet, aes(fit, df0)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas0") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df1)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: Dfbetas1") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df2)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas2") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df3)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas3") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df4)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas4") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df5)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas5") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df6)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas6") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df7)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas7") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))

ggplot(betaplasma.diet, aes(fit, df8)) + 
  geom_point(size = 1) +
  geom_point(data = betaplasma.diet[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = betaplasma.diet[alcohol.strange, ],
             color = "blue", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[r.large, ],
             color = "green", size = 4, shape = 24) + 
  geom_point(data = betaplasma.diet[d.large, ],
             color = "pink", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(9)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Beta plasma: CDfbetas8") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 14))


#### Adjusting the model ####
# Calculating AIC
dietmodel$coefficients
model.0 <- lm(log(betaplasma) ~ 1, data = plasmanew)
(sum.0 <- summary(model.0))

model.1 <- lm(log(betaplasma) ~ vituse, data = plasmanew)
(sum.1 <- summary(model.1))
anova(model.0, model.1)

model.2 <- lm(log(betaplasma) ~ vituse + calories, data = plasmanew)
(sum.2 <- summary(model.2))

model.3 <- lm(log(betaplasma) ~ vituse + calories + fat, data = plasmanew)
(sum.3 <- summary(model.3))

model.4 <- lm(log(betaplasma) ~ vituse + calories + fat + fiber, data = plasmanew)
(sum.4 <- summary(model.4))

model.5 <- lm(log(betaplasma) ~ vituse + calories + fat + fiber + alcohol, 
              data = plasmanew)
(sum.5 <- summary(model.5))

model.6 <- lm(log(betaplasma) ~ vituse + calories + fat + fiber + alcohol + 
                cholesterol, data = plasmanew)
(sum.6 <- summary(model.6))

model.7 <- lm(log(betaplasma) ~ vituse + calories + fat + fiber + alcohol + 
                cholesterol + betadiet , data = plasmanew)
(sum.7 <- summary(model.7))

(collect.AIC <- data.frame(
  nr = seq(1, 8),
  model = c("0.(Intercept)", "1.vituse", "2.calories", 
            "3.fat", "4.fiber", "5.alcohol", "6.cholesterol", "7.betadiet"),
  AIC(model.0, model.1, model.2, model.3, model.4, model.5, model.6, model.7)))

# Backward elimination
model.7
dietarymodel <- step(model.7)

# Beta and exp(beta)
dietarymodel$coefficients
confint(dietarymodel)

exp(dietarymodel$coefficients)
exp(confint(dietarymodel))

# Combine background and dietary using stepwise elimination
backmodel <- contbmi.model


