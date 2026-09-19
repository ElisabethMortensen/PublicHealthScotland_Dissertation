# A&E attendance rate fit models

library(readr)
library(MASS)
library(lme4)

#load and prep population attendance data
model_data <- read_csv("ae_attendance_rates_postcovid.csv", show_col_types = FALSE)
model_data$Age <- factor(model_data$Age, levels = c("Under 18", "18-24", "25-39", "40-64", "65-74", "75 plus"))
model_data$Deprivation <- factor(model_data$Deprivation, levels = 1:5)
model_data$Sex <- factor(model_data$Sex)
model_data$HBT <- factor(model_data$HBT)


# Model A, intercept only (baseline), including offset
model_a <- glm.nb(NumberOfAttendances ~ offset(log_offset), data = model_data)
AIC(model_a)

# Model B, add fixed effects for all demographics (saturated fixed effects model)
model_b <- glm.nb(NumberOfAttendances ~ Age + Sex + Deprivation + HBT + offset(log_offset), data = model_data)
# model_b_step <- step(model_b, direction = "both")
# summary(model_b_step)
AIC(model_b)

# Model C, full fixed effects model with all interactions (saturated model)
model_c <- glm.nb(NumberOfAttendances ~ (Age + Sex + Deprivation + HBT)^2 + offset(log_offset), data = model_data)
# model_c_step <- step(model_c, direction = "both")
# summary(model_c_step)
AIC(model_c)
summary(model_c)

#diagnostic plot for final chosen model
# exp(coef(model_c))
par(mfrow=c(2,2))
plot(model_c, col = phs_colours("phs-purple"), pch = 1, cex = 0.8, col.smooth = phs_colours("phs-green"), lwd = 2)
par(mfrow=c(1,1))

# Model comparison 
comparison_table <- data.frame(Model = c("intercept only", "Saturated fixed effects", "Saturated interactions"), 
                                 AIC = c(AIC(model_a), AIC(model_b), AIC(model_c)), 
                                 BIC=c(BIC(model_a), BIC(model_b), BIC(model_c)), 
                                 Residual_df = c(model_a$df.residual, model_b$df.residual, model_c$df.residual))
comparison_table

# Likelihood ratio tests
print(anova(model_a, model_b))
print(anova(model_b, model_c))