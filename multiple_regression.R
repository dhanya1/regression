#Assignment
blood_pressure.df=read.table('/Users/hithyshikrishnamurthy/Documents/blood_pressure',header=T)
attach(blood_pressure.df)
names(blood_pressure.df) = list('PatientID','Age','Waist','Systolic','Cholesterol','BMI')
summary(blood_pressure.df)
variances = vector()
for ( i in 1:6 )
{
  variances[i] = var(blood_pressure.df[,i])
}
names(blood_pressure.df) = list('PatientID','Age','Waist','Systolic','Cholesterol','BMI')
names(variances) = names(blood_pressure.df)
standevs = variances^0.5
pairs(blood_pressure.df,pch=16)

#Co-variance matrix
cor(blood_pressure.df) 

#Estimate beta-1
blood_pressure.lm = lm(Systolic~Waist+Age+BMI+Cholesterol,blood_pressure.df)
summary(blood_pressure.lm)

#90% confidence Interval for beta-1
(0.557391)+qt(0.95,70)*0.216498
(0.557391)-qt(0.95,70)*0.216498

#Prediction value
predict(blood_pressure.lm,data.frame(Waist = 80, Age=20, BMI = 24, Cholesterol = 300), se.fit = T, interval = "prediction", level =0.95)

#Checking null hypothesis
blood_pressure1.lm = lm(Systolic~Waist,blood_pressure.df)
anova(blood_pressure1.lm,blood_pressure.lm)

#R-squared
b1 = lm(Systolic~Cholesterol,blood_pressure.df)
summary(b1)