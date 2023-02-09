library(moments)

flights <- read.csv("C:/Users/Diana/datasets/flights.csv")
View(flights)

#Емпірична функція щільності
plot(ecdf(flights$price), xlab = "Price", ylab = "Fn(Price)")
plot(ecdf(flights$time), xlab = "Time", ylab = "Fn(Time)")
plot(ecdf(flights$time), xlab = "Distance", ylab = "Fn(Distance)")

#Скринька з вусами
boxplot(flights$price, col = "yellow", ylab = "Pressure")
boxplot(flights$time, col = "red", ylab = "Pressure")
boxplot(flights$distance, col = "green", ylab = "Pressure")

#Мінімальне спостереження вибірки
min(flights$price)
min(flights$time)
min(flights$distance)

#Максимальне спостереження вибірки
max(flights$price)
max(flights$time)
max(flights$distance)

#Медіана
median(flights$price)
median(flights$time)
median(flights$distance)

#Квартилі
quantile(flights$price)
quantile(flights$time)
quantile(flights$distance)

#Децилі
quantile(flights$price, p = seq(0, 1, 0.1))
quantile(flights$time, p = seq(0, 1, 0.1))
quantile(flights$distance, p = seq(0, 1, 0.1))

#Матсподівання
mean(flights$price)
mean(flights$time)
mean(flights$distance)

#Геометричне середнє
exp(mean(log(flights$price)))
exp(mean(log(flights$time)))
exp(mean(log(flights$distance)))

#Середнє гармонічне
1/mean(1/flights$price)
1/mean(1/flights$time)
1/mean(1/flights$distance)

#Мода
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x,uniqx)))]
}

getmode(flights$price)
getmode(flights$time)
getmode(flights$distance)



#Дисперсія
var(flights$price)
var(flights$time)
var(flights$distance)

#Стандартне відхилення
sd(flights$price)
sd(flights$time)
sd(flights$distance)

#Коефіцієнт варіацій
sd(flights$price) / mean(flights$price)
sd(flights$time) / mean(flights$time)
sd(flights$distance) / mean(flights$distance)

#Стохастичне розсіювання
(quantile(flights$price, 0.75) - quantile(flights$price, 0.25))/2
(quantile(flights$time, 0.75) - quantile(flights$time, 0.25))/2
(quantile(flights$distance, 0.75) - quantile(flights$distance, 0.25))/2

#Розмах вибірки
max(flights$price) - min(flights$price)
max(flights$time) - min(flights$time)
max(flights$distance) - min(flights$distance)

#Інтарвал концентрації
IQR(flights$price)
IQR(flights$time)
IQR(flights$distance)

#Коефіцієнт кореляції Пірсона, аналіз істотності парних статистичних зв'язків
cor.test(flights$price, flights$time, method = 'spearman')
cor.test(flights$price, flights$distance, method = 'spearman')
cor.test(flights$distance, flights$time, method = 'spearman')
cov(flights$price,flights$time)/sqrt(var(flights$price)*var(flights$time))
cov(flights$price,flights$distance)/sqrt(var(flights$price)*var(flights$distance))
cov(flights$distance,flights$time)/sqrt(var(flights$distance)*var(flights$time))

#Максимальний рівень значущості при якому відповідний парний статистичний зв'язок не є значимим
cor.test(flights$price,flights$time)$p.value
cor.test(flights$price,flights$distance)$p.value
cor.test(flights$distance,flights$time)$p.value

#Коефіцієнт множинної кореляції, аналіз істотності множинних статистичних зв'язків
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

pairs(newflights,col=3)                                    #матриця розсіювання

a <- flights$time
plot(model$fitted.values,newflights$a)
abline(c(0,1),col="red")

plot(model$fitted.values,model$residuals)
abline(0,0,col="red")

ppnorm(model$residuals)
ppline(model$residuals,col="red")

lobrary(lmtest)
bptest(model)
