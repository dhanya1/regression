#Question 5 - 2012
car_means.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/11-12/Q5 Car Price Means.txt',header=T)
summary(car_means.df)
w_car_means.lm = lm(MeanPrice ~ car_means.df$Age, data = car_means.df , weights = N)
summary(w_car_means.lm)
cars.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/11-12/Q1 Car Prices.txt',header=T)
car.lm = lm(cars.df$Price ~ cars.df$Age, data = car_means.df)
summary(car.lm)
anova(w_car_means.lm)
anova(car.lm)
cars.aov = aov(cars.df$Price ~ factor(cars.df$Age), data = cars.df)
anova(car.lm, cars.aov)

#Question 5 - 2013
w.q5.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/12-13/Q5 Claims.txt',header=T)
summary(w.q5.df)
w.q5.lm = lm(w.q5.df$TotalClaim ~ w.q5.df$Age, weights = 1/w.q5.df$n)
summary(w.q5.lm)

asparin.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/12-13/Q5 Aspirin Means.txt',header=T)
summary(asparin.df)
w.asparin.lm = lm(asparin.df$CmaxMean ~ asparin.df$Dose, data = asparin.df, weights = asparin.df$n)
asp.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/12-13/Q5 Aspirin.txt',header=T)
summary(asp.df)

#Question 2015
quality.df = read.table('/Users/hithyshikrishnamurthy/Documents/exam papers/ST2053/exam/Datasets/14-15/Q5 Quality Means.txt',header=T)

