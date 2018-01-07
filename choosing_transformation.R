blood_pressure.df = read.table('/Users/hithyshikrishnamurthy/Documents/blood_pressure.txt',header=T)
attach(blood_pressure.df)
names(blood_pressure.df) = list('PatientID','Age','Waist','Systolic','Cholesterol','BMI')
summary(blood_pressure.df)
variances = vector()
par(mfrow=c(2,2))

#Type-1
scatter.smooth(Waist,Systolic)
blood_pressure.lm = lm(Systolic ~ Waist, data = blood_pressure.df)
cor(resid(blood_pressure.lm), y = NULL)
plot(fitted(blood_pressure.lm), resid(blood_pressure.lm), main = 'Plot of Residual V Fitted Values')
abline(h = 0, lty = 2)
hist(resid(blood_pressure.lm), main = 'Histogram of Residuals')
qqnorm(resid(blood_pressure.lm), main = 'Normal probability plot of variables')
qqline(resid(blood_pressure.lm))

#Type - 2
scatter.smooth(Waist, log(Systolic))
blood_pressure_log.lm = lm(log(Systolic) ~ Waist, data = blood_pressure.df)
plot(fitted(blood_pressure_log.lm), resid(blood_pressure_log.lm, main = 'Plot of Residual V Fitted Values'))
abline(h = 0, lty = 2)
hist(resid(blood_pressure_log.lm), main = 'Histogram of Residuals')
qqnorm(resid(blood_pressure_log.lm), main = 'Normal probability plot of variables')
qqline(resid(blood_pressure_log.lm))

#Type-3
scatter.smooth(Waist, 1/Systolic)
blood_pressure_inv.lm = lm(log(Systolic) ~ Waist, data = blood_pressure.df)
plot(fitted(blood_pressure_inv.lm), resid(blood_pressure_inv.lm, main = 'Plot of Residual V Fitted Values'))
abline(h = 0, lty = 2)
hist(resid(blood_pressure_inv.lm), main = 'Histogram of Residuals')
qqnorm(resid(blood_pressure_inv.lm), main = 'Normal probability plot of variables')
qqline(resid(blood_pressure_inv.lm))
