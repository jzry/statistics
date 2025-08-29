################################################
#### IMPORTING DATA:
################################################

library(ISLR)
library(olsrr)
data <- read.csv("data.csv")
d <- data.frame(data)
summary(d)
str(d)

train_data <- read.csv("train_set.csv")
train_d <- data.frame(train_data)

test_data <- read.csv("test_set.csv")
test_d <- data.frame(test_data)

################################################
#### BASIC DATA ANALYSIS:
################################################

# Checking for obvious outliers.
boxplot(d$Sleep.duration) # Outliers at 5 and 10 hours.
hist(d$Sleep.duration)
boxplot(d$Sleep.efficiency)
boxplot(d$REM.sleep.percentage)
boxplot(d$Awakenings)
boxplot(d$Caffeine.consumption) # 3 outliers at 200mg
table(d$Caffeine.consumption) # Check frequency of caffeine
boxplot(d$Alcohol.consumption)

# Correlation Matrix: Any |cor| > 0.8, potential collinearity issue.
# If there's variables less than 1 but greater than 0.8 correlation ratio, there is collinearity.
tmp <- cor(d)
rc <- which(tmp < 1 & tmp > 0.8, arr.ind = TRUE)
out <- data.frame(rn = row.names(tmp)[rc[,1]], cn = colnames(tmp)[rc[,2]])
rm(tmp)
out # Nothing above 0.8 in collinearity, so no variables are colinear.

# There are 3 caffeine consumption outliers with 200mg.
# We have to remove points 42, 41, 29 which are the caffeine outliers.
caff_row <- subset(d, d$Caffeine.consumption %in% 200)

# Make a vector without the 3 outliers.
d <- d[-c(229, 240, 273), ]

####################################################
#### SCATTERPLOTS + PARTIAL REGRESSION PLOT
####################################################

# Higher deep sleep percentage predicts higher levels of sleep efficiency.
scatter.smooth(d$Deep.sleep.percentage, d$Sleep.efficiency)

# REM sleep is not a good predictor of sleep efficiency.
scatter.smooth(d$REM.sleep.percentage * d$Deep.sleep.percentage, d$Sleep.efficiency)

# The more awakenings, the less predicted sleep efficiency.
scatter.smooth(d$Awakenings, d$Sleep.efficiency)
hist(d$Awakenings)
table(d$Awakenings)

# Caffeine is not a good predictor of sleep efficiency, sleep duration, or awakenings.
scatter.smooth(d$Caffeine.consumption, d$Sleep.duration)
scatter.smooth(d$Caffeine.consumption, d$Sleep.efficiency)
scatter.smooth(d$Awakenings, d$Caffeine.consumption)

# Smoking status is likely to have a slightly lower deep sleep percentage than non smokers.
scatter.smooth(d$Smoking.status, d$Deep.sleep.percentage)
scatter.smooth(d$Smoking.status, d$Sleep.efficiency)
hist(d$Smoking.status)
table(d$Deep.sleep.percentage)
boxplot(d$Deep.sleep.percentage)

# The higher exercise activity level there is, the higher an individual's sleep efficiency.
scatter.smooth(d$Exercise.frequency, d$Sleep.efficiency)
hist(d$Exercise.frequency)
table(d$Exercise.frequency)
boxplot(d$Exercise.frequency)

# Higher alcohol consumption is correlated with lower sleep efficiency.
scatter.smooth(d$Alcohol.consumption, d$Sleep.efficiency)

# There doesn't seem to be a definitive trend for age and sleep efficiency.
scatter.smooth(d$Age, d$Sleep.efficiency)

# There doesn't seem to be a difference in sleep efficiency between genders.
scatter.smooth(d$Gender, d$Sleep.efficiency)

# REM and DEEP sleep are correlated obviously. The less deep sleep, the less REM.
scatter.smooth(REM.sleep.percentage, Deep.sleep.percentage)

# The more awakenings, the less deep sleep percentage.
scatter.smooth(d$Awakenings, d$Deep.sleep.percentage)
scatter.smooth(d$Awakenings * d$Deep.sleep.percentage, d$Sleep.efficiency)

###########################################################
#### OPTIONAL: SELECTING ONLY ONE GENDER OR SMOKER STATUS:
###########################################################

# 0 males, 1 females.
d <- subset(d, Gender == 1 & Smoking.status == 1)

################################################
#### MAKING THE TRAINING SET AND PREDICTION SET:
################################################

# Random select observation numbers for training set and test set.
# This is a very small dataset, so the majority of observation should
# be in the training set.
train=sample(1:nrow(d),nrow(d)*0.8) #Put 80% into training set.
test=(-train) # Remaining observations are put into testing set.

# Split dataset into 2 sets: train_set and test_set.
train_set <- d[train,]
test_set <- d[test,]

print(train_set)
print(test_set)

write.csv(train_set, "train_set.csv")
write.csv(test_set, "test_set.csv")

attach(train_d)

####################################
# BUILDING THE FULL MODEL
####################################

full_model <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Smoking.status +
                 Deep.sleep.percentage : Awakenings, data=train_d)

summary(full_model)

###############################################
# MULTIPLE PARTIAL F TEST FOR INTERACTION TERMS
###############################################

mod2 <- lm(Sleep.efficiency ~ ., data=train_d)

# One or more interaction terms in our full model are significant, so we test for both.
anova(mod2, full_model)

mod3 <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Smoking.status, data=train_d)
mod4 <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Awakenings, data=train_d)

# Deep sleep * awakenings is significant.
anova(mod3, full_model)

# Deep sleep * smoking status is significant.
anova(mod4, full_model)

# Therefore, we keep both interaction terms in the model.

##################################################################
# SELECTION ALGORITHMS TO BUILD FORWARD, BACKWARD, STEPWISE MODELS
##################################################################

forward <- ols_step_forward_p(full_model, p_enter = 0.1)
backward <- ols_step_backward_p(full_model, p_enter = 0.1)
stepwise <- ols_step_both_p(full_model, p_enter = 0.1, p_remove = 0.3, details=T)

# Used to check which variables to include in our models.
forward
backward
stepwise

#############################################
# BUILDING FORWARD, BACKWARD, STEPWISE MODELS
#############################################

forward_model <- lm(Sleep.efficiency ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age + Sleep.duration +
                    Gender + Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Awakenings +
                    Deep.sleep.percentage : Awakenings + Deep.sleep.percentage : Smoking.status, data=train_d)

backward_model <- lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Awakenings + Caffeine.consumption +
                     Alcohol.consumption + Smoking.status + Exercise.frequency + Deep.sleep.percentage : Smoking.status + 
                     Deep.sleep.percentage : Awakenings, data=train_d)

stepwise_model <- lm(Sleep.efficiency ~ Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Awakenings +
                     Exercise.frequency + Deep.sleep.percentage : Smoking.status, data=train_d)

summary(forward_model)
summary(backward_model)
summary(stepwise_model)

# THESE ARE THE MODELS WITH ONLY THE SIGNIFICANT VALUES KEPT IN. WE REMOVED THE INSIGNIFICANT VALUES.


forward_model_sig_only <- lm(Sleep.efficiency ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age +
                             Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Awakenings +
                             Deep.sleep.percentage : Awakenings + Deep.sleep.percentage : Smoking.status, data=train_d)

backward_model_sig_only <- lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Awakenings + Caffeine.consumption +
                              Alcohol.consumption + Smoking.status + Exercise.frequency + Deep.sleep.percentage : Smoking.status + 
                              Deep.sleep.percentage : Awakenings, data=train_d)

stepwise_model_sig_only <- lm(Sleep.efficiency ~ Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Awakenings +
                              Exercise.frequency + Deep.sleep.percentage : Smoking.status, data=train_d)

summary(forward_model_sig_only)
summary(backward_model_sig_only)
summary(stepwise_model_sig_only)

###############################################
# MULTIPLE PARTIAL F TEST FOR FINAL TERMS
###############################################

# The extra terms in the forward model are not significant, so they may be removed.
anova(forward_model, forward_model_sig_only)

# Same as original model. All terms were significant.
anova(backward_model, backward_model_sig_only)
anova(stepwise_model, stepwise_model_sig_only)

# Forward and backward models are identical, so we will discard backward and use forward.
anova(forward_model_sig_only, backward_model_sig_only)

###################################################
# CHECKING FOR OUTLIERS (COOK, LEVERAGE, JACKKNIFE)
###################################################

# Library for outlier functions.
library(MASS)

# Change to test different model types.
model = forward_model_sig_only

# Jackknife residual.
t <- qt(.025, (nrow(train_d)-ncol(train_d)-2), lower.tail = FALSE) # t distribution with n-k-2 df.
subset(sort(studres(model)), sort(studres(model)) > t) # Select outliers only

# Leverage: Compare values to 2(k+1)/n.
leverage_value = ((2 * (ncol(train_d) + 1)) / nrow(train_d))
subset(sort(hatvalues(model)), sort(hatvalues(model)) > leverage_value)

# Cook's Distance
cook_dist_val = 2 * sqrt(ncol(train_d) / nrow(train_d))
subset(sort(cooks.distance(model)), sort(hatvalues(model)) > cook_dist_val)

##########################################
# BOX COX TO FIX NORMALITY
##########################################

# Normalizing our data set.
hist(train_d$Sleep.efficiency)
hist(train_d$Sleep.efficiency^3.7)

# Checking for normality.
shapiro.test(forward_model_sig_only$residuals)
shapiro.test(backward_model_sig_only$residuals)
shapiro.test(stepwise_model_sig_only$residuals)

# Checking for normality, linearity, and homoscedasticity.
plot(forward_model_sig_only)
plot(backward_model_sig_only)
plot(stepwise_model_sig_only)

# Y^2 is the best transformation.
boxcox(forward_model_sig_only)

forward_model_sig_only <- lm(I(Sleep.efficiency^3.7) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age +
                               Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Awakenings +
                               Deep.sleep.percentage : Awakenings + Deep.sleep.percentage : Smoking.status, data=train_d)

boxCox(stepwise_model_sig_only)

stepwise_model_sig_only <- lm(I(Sleep.efficiency^3.7) ~ Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Awakenings +
                                Exercise.frequency + Deep.sleep.percentage : Smoking.status, data=train_d)


################################################
#### CHECKING OUTLIERS AND ASSUMPTIONS:
################################################

forward_model_sig_only <- lm(I(Sleep.efficiency^3.7) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age +
                               Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Awakenings +
                               Deep.sleep.percentage : Awakenings, data=train_d)

stepwise_model_sig_only <- lm(I(Sleep.efficiency^3.7) ~ Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Awakenings +
                                Exercise.frequency + Deep.sleep.percentage : Smoking.status, data=train_d)


# Checking for normality.
shapiro.test(forward_model_sig_only$residuals)
shapiro.test(backward_model_sig_only$residuals)
shapiro.test(stepwise_model_sig_only$residuals)

# Checking for normality, linearity, and homoscedasticity.
plot(forward_model_sig_only)
plot(backward_model_sig_only)
plot(stepwise_model_sig_only)

# Change to test different model types.
model = stepwise_model_sig_only

# If our line = 0, and our plot appears random, constant variance holds.
# If there appears to be a trend in our plot then there could be departure
# From linearity.
# model$fitted.values to get predicted Y.
plot(model$fitted.values, studres(model))
abline(h = 0)

########################################
# TESTING FOR MODEL OVERFITTING
########################################
##### TESTING FOR MSE / AIC
########################################

model = stepwise_model_sig_only

library(Metrics)
pred_train <- predict(model, train_d)
mse(train_d$Sleep.efficiency, pred_train)

pred_test <- predict(model, test_d)
mse(test_d$Sleep.efficiency, pred_test)
#Since MSE training is about equal to MSE testing,
#the model is NOT overfit

BIC(forward_model_sig_only)
BIC(stepwise_model_sig_only)

AIC(forward_model_sig_only)
AIC(stepwise_model_sig_only)

##########################################
# MAE
##########################################

#Calculate y-hats for training model.
pred_train <- predict(model, train_set)
mae(train_set$Sleep.efficiency, pred_train)

pred_test <- predict(model, test_d)
mae(test_d$Sleep.efficiency, pred_test)

#Since MAE training is about equal to MAE testing,
#the model is NOT overfit

##########################################
# SHRINKAGE
##########################################

# Get predicted values from test set from the model on training data.
Yhat2 <- predict(model, test_d)

# Get R^2 between predicted values of Y and actual values of Y in test set.
R2_2 <- (cor(Yhat2, test_d$Sleep.efficiency))^2
print(R2_2)

# Calculate Shrinkage
R1_2 <- cor(model$fitted.values, train_d$Sleep.efficiency)^2
shrink = R1_2 - R2_2
print(shrink)

##########################################
# FINAL MODELS
##########################################

library(modelsummary)

?modelsummary

# Decided to go with forward model because it has the lowest AIC score compared to stepwise.
# > AIC(forward_model_sig_only)
# [1] -808.5632
# > AIC(stepwise_model_sig_only)
# [1] -879.2802
# Backward model doesn't work because it violates every assumption. We would have to transform it.
summary(forward_model_sig_only)
summary(backward_model_sig_only)
summary(stepwise_model_sig_only)

models <- list(
  "Forward"     = forward_model_sig_only,
)

modelsummary(forward_model_sig_only)
summary(forward_model_sig_only)

