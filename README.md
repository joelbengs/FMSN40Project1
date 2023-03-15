# PROJECT 1: Linear regression - Determinants of plasma beta carotene levels
Project 1 in the course FMSN40 Linear and Logisitc regression with data collection, spring of 2022. Lund School of Engineering

Code can be found in the folder "R". Files starting with "part" was written by Joel Bengs. Other files were weitten by Sofia Larsson and Sofie Thulin

Data can be found in the folder "Data"

# Abstract (from report)
This report investigates which factors affect the plasma beta-carotene levels in the body. After
constructing two models, one lin-lin model and one log-lin model, it was found that the log-lin model was
the better model after analyzing the residuals. After that, a background model and a dietary model was
constructed and later combined into one model. The variables included in the final model were vitamin
use, calories, fiber, betadiet, quetelet, smokstat, sex and age. Vitamin use, calories, quetelet, former
smoker, current smoker and male were variables that decreased the plasma beta-carotene levels and fiber,
betadiet and age increased the plasma beta-carotene levels. 24.4 % of the variability in the used data set
can be explained by the final model.

# Conclusion (from report) 
When only using age as a parameter for the fitted model, a residual analysis showed that a log-lin model
described the plasma beta-carotene level in a patient better than a lin-lin model. Furthermore, when
adding additional categorical variables such as sex, smoking status, and BMI-categories, it was concluded
that the accuracy of the log-lin model depended a lot on the choice of reference category.

Since the collected data was not spread equally over all categories, predictions for some undercategories
were unreliable due to a small number of observations. For instance, when trying to predict the outcome
for a category that is not included in the data. A model using categorical variables should be used with
great caution when predicting the level of plasma beta-carotene in the categories with none or few
observations. To get a more reliable result for these people, a more diverse data set with more
observations is needed.

Furthermore, the impact of using the categorical variable bmicat, the continuous variable quetelet or both
variables at the same time was investigated. Since they are strongly correlated, the quality of the model
was worsened by including them both at the same time. Instead, a better model was achieved by replacing
bmicat with quetelet. This problem of having at least two variables with strong correlation presented itself
once again when introducing the dietary variables. When using a stepwise procedure to find the best
model, it was therefore not surprising that these pairs were not included in the seemingly best model. For
that reason, they were also not included in the final model.

The odd observations in the data were investigated and analyzed. There were some outliers in the data set,
for instance, one individual who consumed a lot more alcohol than the rest of the patients. By plotting
leverage, studentized residuals and Cook's distance, the outliers were shown to have a large influence on
the used model. Reasonably, the model would benefit from excluding the outliers from the model.

Finally, a model that could explain almost a quarter of the variability in the plasma beta-carotene levels
was constructed. That is far more than the simple models where age was the only variable, yet there might
be other factors that affect plasma beta-carotene levels that have not yet been investigated
