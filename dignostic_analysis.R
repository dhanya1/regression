#Assignment
blood_pressure.df=read.table('/Users/hithyshikrishnamurthy/Documents/blood_pressure',header=T)
attach(blood_pressure.df)
names(blood_pressure.df) = list('PatientID','Age','Waist','Systolic','Cholesterol','BMI')
summary(blood_pressure.df)

#Linear Modelling
blood_pressure.lm = lm(Systolic~Waist+Age+BMI+Cholesterol,blood_pressure.df)
summary(blood_pressure.lm)

#Calculate the leverage
h = lm.influence(blood_pressure.lm)$hat
plot(h , type='h', main = "Plot of Leverage vs. observation number", ylab = 'Leverage')

#Getting residuals
e = resid(blood_pressure.lm)
#Sigma stands for common standard deviation
s = summary(blood_pressure.lm)$sigma
#Studentized residual 
r = e / (s*(1-h)^0.5)

#plot of studentized residuals vs observation number
plot(r, type='h', main='Plot of SR vs ON', ylab = 'Studentized residuals', ylim=c(-4,4))
abline(h=0, lty = 2)
#Anything over this line is a residual
abline(h=c(-2,2), lty = 2)

#Calculate the Cook's distance
p = length(coef(blood_pressure.lm))
d = (1/p)*(h/(1-h))*r^2

#Plot the cook's distance vs observation number
plot(d , type='h', main = "Plot of Cook's distance vs. observation number", ylab= "Cook's distance")
coeffs.changes = lm.influence(blood_pressure.lm)$coefficients

