library(moments)

flights <- read.csv("C:/Users/Diana/datasets/flights.csv")
View(flights)

#Empirical density function
plot(ecdf(flights$price), xlab = "Price", ylab = "Fn(Price)")
plot(ecdf(flights$time), xlab = "Time", ylab = "Fn(Time)")
plot(ecdf(flights$time), xlab = "Distance", ylab = "Fn(Distance)")

#Boxplot
boxplot(flights$price, col = "yellow", ylab = "Pressure")
boxplot(flights$time, col = "red", ylab = "Pressure")
boxplot(flights$distance, col = "green", ylab = "Pressure")

#Minimum
min(flights$price)
min(flights$time)
min(flights$distance)

#Maximum
max(flights$price)
max(flights$time)
max(flights$distance)

#Median
median(flights$price)
median(flights$time)
median(flights$distance)

#Quartile
quantile(flights$price)
quantile(flights$time)
quantile(flights$distance)

#Decile
quantile(flights$price, p = seq(0, 1, 0.1))
quantile(flights$time, p = seq(0, 1, 0.1))
quantile(flights$distance, p = seq(0, 1, 0.1))

#Mathematical expectation
mean(flights$price)
mean(flights$time)
mean(flights$distance)

#Geometric mean
exp(mean(log(flights$price)))
exp(mean(log(flights$time)))
exp(mean(log(flights$distance)))

#Harmonic mean
1/mean(1/flights$price)
1/mean(1/flights$time)
1/mean(1/flights$distance)

#Mode
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}

getmode(flights$price)
getmode(flights$time)
getmode(flights$distance)



#Variance
var(flights$price)
var(flights$time)
var(flights$distance)

#Standart deviation
sd(flights$price)
sd(flights$time)
sd(flights$distance)

#Coefficient of variation
sd(flights$price) / mean(flights$price)
sd(flights$time) / mean(flights$time)
sd(flights$distance) / mean(flights$distance)

#Stochastic dispersal
(quantile(flights$price, 0.75) - quantile(flights$price, 0.25))/2
(quantile(flights$time, 0.75) - quantile(flights$time, 0.25))/2
(quantile(flights$distance, 0.75) - quantile(flights$distance, 0.25))/2

#Sample size
max(flights$price) - min(flights$price)
max(flights$time) - min(flights$time)
max(flights$distance) - min(flights$distance)

#Interquartile range
IQR(flights$price)
IQR(flights$time)
IQR(flights$distance)

#Pearson Correlation Coefficient, analysis of the strength of a linear statistical relationship between paired data
cor.test(flights$price, flights$time, method = 'pearson')
cor.test(flights$price, flights$distance, method = 'pearson')
cor.test(flights$distance, flights$time, method = 'pearson')
cov(flights$price,flights$time)/sqrt(var(flights$price)*var(flights$time))
cov(flights$price,flights$distance)/sqrt(var(flights$price)*var(flights$distance))
cov(flights$distance,flights$time)/sqrt(var(flights$distance)*var(flights$time))

#The maximum level of significance, where the corresponding pairwise statistical relationship is not significant
cor.test(flights$price,flights$time)$p.value
cor.test(flights$price,flights$distance)$p.value
cor.test(flights$distance,flights$time)$p.value

#Multiple correlation coefficient, analysis of the strength of multiple statistical relationships
#a <- flights$price
#b <- flights$time
#c <- flights$distance

a.model <- lm(a~b+c)
cor(a.model$model$a, a.model$fitted.values)

b.model <- lm(b~a+c)
cor(b.model$model$b, b.model$fitted.values)

c.model <- lm(c~b+a)
cor(c.model$model$c, c.model$fitted.values)

cor.test(flights$price,flights$time,flights$distance)$p.value


#LAB 2

newflights <- flights[,c("price","distance","time")]
View(newflights)

model <- lm(flights$time~flights$distance+flights$price,data=flights)
summary(model)

pairs(newflights,col=3)                                    #plot matrix, consisting of scatter plots corresponding to each data frame

a <- flights$time
plot(model$fitted.values,newflights$a)
abline(c(0,1),col="red")

plot(model$fitted.values,model$residuals)
abline(0,0,col="red")

ppnorm(model$residuals)
ppline(model$residuals,col="red")

lobrary(lmtest)
bptest(model)
