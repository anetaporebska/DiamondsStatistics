
################################################################################
#################### PRZYGOTOWANIE DANYCH DO ANALIZY ###########################

library(e1071)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(GGally)
library(nortest)
library(MASS)


setwd("/home/aneta/Pulpit/Studia/Semestr_3/Prawdopodobieństwo i statystyka/projekt")
data <- read.csv("diamonds.csv")
data_set <- as.data.frame(cbind(data$carat, data$depth, data$table, data$price, data$x, data$y, data$z))
colnames(data_set)[1]<-"carat"
colnames(data_set)[2]<-"depth"
colnames(data_set)[3]<-"table_var"
colnames(data_set)[4]<-"price"
colnames(data_set)[5]<-"x_corr"
colnames(data_set)[6]<-"y_corr"
colnames(data_set)[7]<-"z_corr"
attach(data_set)

# zapisanie danych do pliku
write.csv(data_set,"/home/aneta/Pulpit/Studia/Semestr_3/Prawdopodobieństwo i statystyka/projekt/data_set.csv", row.names = TRUE)


###############################################################################
################## PODSTAWOWA ANALIZA DANYCH ##################################
summary(data_set)

# ZMIENNA PRICE

hist(price, breaks = 20, main="Histogram for variable 'price' ", col="darkmagenta")

range(price)
median(price)
mean(price)
quantile(price, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(price) 
kurtosis(price)
skewness(price)

boxplot(price, 
        main="Boxplot for variable 'price' ",
        col = "#009999",
        border = "#0b6774",
        notch=TRUE)


# ZMIENNA CARAT

hist(carat, breaks = 20, main="Histogram for variable 'carat' ", col="darkmagenta")

range(carat)
median(carat)
mean(carat)
quantile(carat, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(carat) 
kurtosis(carat)
skewness(carat)

boxplot(carat, 
        main="Boxplot for variable 'carat' ",
        col = "#009999",
        border = "#0b6774",
        notch=TRUE)


# ZMIENNA TABLE

hist(table_var, breaks = 20, main="Histogram for variable 'table_var' ", col="darkmagenta")

range(table_var)
median(table_var)
mean(table_var)
quantile(table_var, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(table_var) 
kurtosis(table_var)
skewness(table_var)

boxplot(table_var, 
        main="Boxplot for variable 'table_var' ",
        col = "#009999",
        border = "#0b6774",
        notch=TRUE)

# ZMIENNA DEPTH

hist(depth, breaks = 20, main="Histogram for variable 'depth' ", col="darkmagenta")

range(depth)
median(depth)
mean(depth)
quantile(depth, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(depth) 
kurtosis(depth)
skewness(depth)

boxplot(depth, 
        main="Boxplot for variable 'depth' ",
        col = "#009999",
        border = "#0b6774",
        notch=TRUE)



# ZMIENNA X

hist(x_corr, breaks = 20, main="Histogram for variable 'x_corr' ", col="darkmagenta")

range(x_corr)
median(x_corr)
mean(x_corr)
quantile(x_corr, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(x_corr) 
kurtosis(x_corr)
skewness(x_corr)


# ZMIENNA Y

hist(y_corr, breaks = 20, main="Histogram for variable 'y_corr' ", col="darkmagenta")

range(y_corr)
median(y_corr)
mean(y_corr)
quantile(y_corr, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(y_corr) 
kurtosis(y_corr)
skewness(y_corr)

# ZMIENNA Z

hist(z_corr, breaks = 20, main="Histogram for variable 'z_corr' ", col="darkmagenta")

range(z_corr)
median(z_corr)
mean(z_corr)
quantile(z_corr, c(0.1, 0.25, 0.5, 0.75, 0.9)) 
sd(z_corr) 
kurtosis(z_corr)
skewness(z_corr)


# boxplot dla zmiennych x, y, z
boxplot(x_corr, y_corr, z_corr, main = "Boxplot for dimensions x, y, z",
        at = c(1,2,3),
        names = c("x","y","z"),
        col = c("#2e4dbf", "#09217b", "#1169ae"),
        notch = TRUE
        )


###############################################################################
########## Analiza rodzaju rozkładu badanych cech #############################

##### Testowanie zgodności z rozkładem normalnym

# anderson-darling normality test
ad.test(price)
ad.test(carat)
ad.test(depth)

# porównanie kwantyli rozkładu empirycznego z teoretycznymi kwantylami rozkładu normalnego - jeżeli obserwacje pochodzą z rozkładu normalnego, to punkty na wykresie
# układają się wzdłuż linii prostej
qqnorm(price) 
qqnorm(carat)
qqnorm(depth)



#### TESTOWANIE ZGODNOŚCI Z ROZKŁADEM JEDNOSTAJNYM

# Chi-squared test
chisq.test(price)
chisq.test(carat)
chisq.test(depth)



###############################################################################
################### ZALEŻNOŚCI POMIEDZY ZMIENNYMI #############################


# macierz kowariancji dla wszystkich zmiennych
cov(data_set[,c(1,2,3,4,5,6,7)])

# macierz korelacji dla wszystkich zmiennych
cor(data_set[,c(1,2,3,4,5,6,7)])
ggcorr(data_set[,c(1,2,3,4,5,6,7)])





# Zależności pomiędzy price a carat

data <- data.frame(
  carat = carat, 
  price = price 
)

p1 <- ggplot(data, aes(x=carat, y=price), xlab="Carat", ylab="Price") + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
p1

# Regresja prosta
lm.price = lm(price~carat)
summary(lm.price) # spełnia założenia


# współczynniki
beta0 <- coef(lm.price)[1]
beta1 <- coef(lm.price)[2]
plot(carat, price, pch = 18, cex =1, col ="#69b3a2" )
lines (carat, beta0 + beta1*carat,  col="#427e09")



# przedziały ufności dla wybranych wartości x
newdata = data.frame(carat=1)
p1 <- predict(lm.price, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]


newdata = data.frame(carat=2)
p1 <- predict(lm.price, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]

newdata = data.frame(carat=3)
p1 <- predict(lm.price, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]

newdata = data.frame(carat=4)
p1 <- predict(lm.price, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]


# wykres przedstawiający różnice pomiędzy wartościami przewidzianymi a rzeczywistymi (linia prosta to wartości przewidziane)
res.price <- resid(lm.price)

data <-data.frame(
  carat=carat,
  price=res.price
)

p1 <- ggplot(data, mapping=aes(x=carat, y=price), xlab="Carat", ylab="Price") + 
  geom_point( color="#69b3a2") + geom_segment(data, mapping = aes(x=carat, y=0, xend = 4, yend = 0), col="#427e09") +
  theme_ipsum()
p1




# zależności pomiędzy price a depth

data <- data.frame(
  depth = depth, 
  price = price 
)

p1 <- ggplot(data, aes(x=depth, y=price), xlab="Depth", ylab="Price") + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
p1

# brak liniowej zależności



# zależność pomiędzy depth a table
data <- data.frame(
  depth = depth, 
  table_var = table_var 
)

p1 <- ggplot(data, aes(x=table_var, y=depth), xlab="Depth", ylab="Table") + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
p1

# brak liniowej zależności


# zależności pomiędzy x i y

data <- data.frame(
  x = x_corr,
  y = y_corr
)

p1 <- ggplot(data, aes(x=x, y=y), xlab="x", ylab="y") + 
  geom_point( color="#69b3a2") +
  theme_ipsum()
p1


# Regresja prosta
lm.y = lm(y_corr~x_corr)
summary(lm.y) # spełnia założenia

# współczynniki
beta0 <- coef(lm.y)[1]
beta1 <- coef(lm.y)[2]

plot(x_corr, y_corr, pch = 18, cex =1, col ="#69b3a2", ylim=c(0,11) )
lines (x_corr, beta0 + beta1*x_corr,  col="#427e09")


newdata = data.frame(x_corr=1)
p1 <- predict(lm.y, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]


newdata = data.frame(x_corr=3)
p1 <- predict(lm.y, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]

newdata = data.frame(x_corr=5)
p1 <- predict(lm.y, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]

newdata = data.frame(x_corr=7)
p1 <- predict(lm.y, newdata, interval = "confidence", level = 0.95 )
p1
p1[3]-p1[2]


res.y <- resid(lm.y)
plot(x_corr, res.y, pch = 18, cex =1, col ="#69b3a2" , ylim=c(-1,1))
lines(x_corr, y=rep(0, length(x_corr)) ,col="#427e09")






### Badanie zgodności dwóch rozkładów

# jeśli obserwacje z obu wektorów pochodzą z tego samego rozkładu, to punkty powinny ułożyć się blisko prostej y=x
qqplot(x_corr, y_corr,  pch = 18, cex =2, col ="#69b3a2", ylim=c(0,11), xlim=c(0,11))
lines(x_corr, x_corr, type="l", col="#427e09")



#### ROZKŁAD ZMIENNEJ DEPTH 
# jest najbliższy rozkładowi normalnemu, na nim przeprowadzam tetowanie hipotez

hist(depth, breaks=100) 
plot(density(depth))

m <- mean(depth)
s <- sd(depth)
sd_depth <- ((depth-m)/s) # standaryzacja
hist(sd_depth, breaks=100)
plot(density(sd_depth))

ad.test(depth)
ad.test(sd_depth)


# sprawdzam czy ma średnią = 0 i odchylenie standardowe = 1
mean(sd_depth)
sd(sd_depth)


#### TESTOWANIE HIPOTEZ NA ROZKŁADZIE NORMALNYM sd_depth

t.test(sd_depth, mu = 2, alternative = "less")

# p wartość jest mała, zatem przyjmujemy hipotezę alternatywną: prawdziwa średnia jest mniejsza niż 2.

t.test(sd_depth, mu = 1, alternative = "less" )

# p wartość jest mała, zatem przyjmujemy hipotezę alternatywną: prawdziwa średnia jest mniejsza niż 1.

t.test(sd_depth, mu = 0 )

# wniosek : p-wartość jest wysoka, zatem nie ma podstaw by twierdzidzić, żę średnia jest różna od 0 








