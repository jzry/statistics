################################################
#### IMPORTING DATA:
################################################

library(ISLR)
library(olsrr)
d <- data.frame(stats3Group18Data)
summary(d)
str(d)


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


####################################
# BUILDING THE FULL MODEL
####################################

full_model <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Smoking.status +
                   Deep.sleep.percentage : Awakenings, data=d)

summary(full_model)

###############################################
# MULTIPLE PARTIAL F TEST FOR INTERACTION TERMS
###############################################

mod2 <- lm(Sleep.efficiency ~ ., data=d)

# One or more interaction terms in our full model are significant, so we test for both.
anova(mod2, full_model)

mod3 <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Smoking.status, data=d)
mod4 <- lm(Sleep.efficiency ~ . + Deep.sleep.percentage : Awakenings, data=d)

# Deep sleep * awakenings is significant.
anova(mod3, full_model)

# Deep sleep * smoking status is significant.
anova(mod4, full_model)

# Therefore, we keep both interaction terms in the model.

###################################################
# CHECKING FOR OUTLIERS (COOK, LEVERAGE, JACKKNIFE)
###################################################

# Library for outlier functions.
library(MASS)

# Change to test different model types.
model = full_model

# Jackknife residual.
t <- qt(.025, (nrow(d)-ncol(d)-2), lower.tail = FALSE) # t distribution with n-k-2 df.
subset(sort(studres(model)), sort(studres(model)) > t) # Select outliers only

# Leverage: Compare values to 2(k+1)/n.
leverage_value = ((2 * (ncol(d) + 1)) / nrow(d))
subset(sort(hatvalues(model)), sort(hatvalues(model)) > leverage_value)

# Cook's Distance
cook_dist_val = 2 * sqrt(ncol(d) / nrow(d))
subset(sort(cooks.distance(model)), sort(hatvalues(model)) > cook_dist_val)

# There are 3 caffeine consumption outliers with 200mg.
# We have to remove points 42, 41, 29 which are the caffeine outliers.
caff_row <- subset(d, d$Caffeine.consumption %in% 200)

# Make a vector without the 3 outliers.
d <- d[-c(229, 240, 273), ]

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
                      Deep.sleep.percentage : Awakenings + Deep.sleep.percentage : Smoking.status, data=d)

backward_model <- lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Awakenings + Caffeine.consumption +
                       Alcohol.consumption + Smoking.status + Exercise.frequency + Deep.sleep.percentage : Smoking.status + 
                       Deep.sleep.percentage : Awakenings, data=d)

stepwise_model <- lm(Sleep.efficiency ~ Deep.sleep.percentage + Caffeine.consumption + Alcohol.consumption + Awakenings +
                       Exercise.frequency + Deep.sleep.percentage : Smoking.status, data=d)

summary(forward_model)
summary(backward_model)
summary(stepwise_model)
plot(forward_model)
plot(backward_model)
plot(stepwise_model)
print(forward)
print(backward)
print(stepwise)

# THESE ARE THE MODELS WITH ONLY THE SIGNIFICANT VALUES KEPT IN. WE REMOVED THE INSIGNIFICANT VALUES.
backward_model_sig_only <- lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Caffeine.consumption +
                                Alcohol.consumption + Smoking.status + Exercise.frequency + Deep.sleep.percentage : Smoking.status + 
                                Deep.sleep.percentage : Awakenings, data=d)

summary(backward_model_sig_only)

##########################################
# BOX COX TO FIX NORMALITY
##########################################

# Checking for normality.
shapiro.test(backward_model_sig_only$residuals)

# Checking for normality, linearity, and homoscedasticity.
plot(backward_model_sig_only)

# Normalizing our data set.
hist(d$Sleep.efficiency)
hist(d$Sleep.efficiency)

# Y^2 is the best transformation.
boxCox(backward_model_sig_only)

################################################
#### CHECKING OUTLIERS AND ASSUMPTIONS:
################################################

backward_model_sig_only <- lm(I(Sleep.efficiency^0.9) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age +
                               Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Deep.sleep.percentage : Smoking.status
                               Deep.sleep.percentage : Awakenings, data=d)

# Checking for normality.
shapiro.test(backward_model_sig_only$residuals)

#Normality is still violated, try again with 3
backward_model_sig_only <- lm(I(Sleep.efficiency^3) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age +
                                Caffeine.consumption + Alcohol.consumption + Exercise.frequency + Deep.sleep.percentage : Smoking.status
                                Deep.sleep.percentage : Awakenings, data=d)

# Checking for normality.
shapiro.test(backward_model_sig_only$residuals)


#Conducted one more time, without Caffine since it is not signifigant
backward_model_sig_only <- lm(I(Sleep.efficiency^3) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age 
                                 + Alcohol.consumption + Exercise.frequency + Deep.sleep.percentage : Smoking.status
                                Deep.sleep.percentage : Awakenings, data=d)

# Checking for normality.
shapiro.test(backward_model_sig_only$residuals)

#Normality is still violated, try again with 3
backward_model_sig_only <- lm(I(Sleep.efficiency^3) ~ Deep.sleep.percentage + Smoking.status + REM.sleep.percentage + Age 
                                 + Alcohol.consumption + Exercise.frequency +
                                Deep.sleep.percentage : Awakenings, data=d)

# Checking for normality.
shapiro.test(backward_model_sig_only$residuals)

# Checking for normality, linearity, and homoscedasticity.
plot(backward_model_sig_only)

# Change to test different model types.
model = backward_model_sig_only

# Checking for normality, linearity, and homoscedasticity.
plot(backward_model_sig_only)

# Change to test different model types.
model = backward_model_sig_only

# If our line = 0, and our plot appears random, constant variance holds.
# If there appears to be a trend in our plot then there could be departure
# From linearity.
# model$fitted.values to get predicted Y.
plot(model$fitted.values, studres(model))
abline(h = 0)

##########################################
# FINAL MODELS
##########################################

# Decided to go with backwards model because it has the lowest AIC score 
# compared to stepwise and forward.

summary(backward_model_sig_only)
modelsummary(backward_model_sig_only)
