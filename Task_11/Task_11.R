setwd('~/Desktop/Evolution/Tasks/Task_11')
x <- rnorm(100, mean=5, sd =2)
x
y <- (x *5) + 2 + (rnorm(100, 0:0.1 ))
y
#?rnorm()
plot(x, y)
abline(lm(y~x), col='red')
coef(lm(y~x))
#the slope will be around 5, with the y-intercept being around 2. Adding the random numbers from 0-0.1 could sway that a little bit. The above line of code returned 5.02 for my slope and 1.79 for my intercept. The reason this is the slope and intercept is because y is a function of x in this case. And we multiplied x by 5 and added 2, then added a random number between 0and 0.1 to each number in the set, swaying it a little bit. Had we not added those random numbers, we'd have a perfect 5x + 2 graph. 
z <- c()
x <- rnorm(100, mean=5, sd=2)
for (i in 1:100) {
	z[i] <- runif(1)
	y <- (x * z[i]) + 2 + (rnorm(100, 0:0.1))
	l <- coef(lm(z[1:100]~y))
}
l
plot(z[1:100], y)
abline(lm(y~z[1:100]))

plot(c(z, -0.029)) 
#I don't know what this reveals meaning I probably didn't do it right. 

install.packages('meme')
library('meme')
u <- 'https://i.imgflip.com/3read8.jpg'
my_meme <- meme(u, lower= "I am once again asking 
for help making a for() loop", color="white", size="1.5")

