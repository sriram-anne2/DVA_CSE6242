data("faithful")
names(faithful)
plot(faithful$eruptions, xlab = "sample number", ylab = "eruption times(min)", main = "Old Faithful Erution Times")

library(ggplot2)
qplot(x = waiting, data = faithful, binwidth = 3, main = "waiting time to next eruption (min)")

ggplot(faithful,aes(x = waiting)) + geom_histogram((binwidth = 1))

ggplot(faithful, aes(waiting)) + geom_histogram(binwidth = 1)

data("mtcars")
names(mtcars)
x = seq(-2,2,length.out = 30)
y = x^2
qplot(x,y,geom = "line")
qplot(x,y,geom = c("point","line"))
dataframe = data.frame(x,y)
ggplot(dataframe, aes(x,y)) + geom_line() + geom_point()

S = sort.int(mpg$cty, index.return = T)
plot(S$x, type = "l", lty = 2, xlab = "sample number (sorted by city mpg)", ylab = "mpg")
lines(mpg$hwy[S$ix], lty = 1)
legend("topleft", c("highway mpg", "city mpg"), lty = c(1,2))  



data("faithful")

plot(faithful$waiting, faithful$eruptions, 
     pch = 17, col = 2, cex = 1.2, 
     xlab = "waiting times", ylab = "eruption times")


install.packages('GGally')
library(GGally)

data("mtcars")
data(mpg)
names(mtcars)
plot(mtcars$mpg,
     mtcars$hp,
     pch = mtcars$am,
     xlab = "horsepower",
     cex = 1.2,
     ylab = "mpg",
     main = "mpg vs hp using transmission")
legend("topright", c("automatic", "manual"), pch = c(0,1))


names(mpg)

qplot(x = wt,
      y = mpg,
      data = mtcars,
      size = cyl,
      main = "MPG vs Weight using cylinder")

data("midwest")
plot(midwest$state, 
     midwest$percprof,
     pch = 17, col = 2, cex = 1.2,
     xlab = "states",
     ylab = "Professional Education"
)

qplot(x = state,
      y = county,
      data = midwest,
      size = percprof,
      main = "State Vs PercProf")

ggplot(midwest, aes(x = state, y = percprof)) + geom_line() + geom_point() 
#geom_histogram(binwidth = 2)

ggplot(midwest, aes(y = percprof, x = state)) + geom_violin(scale = "area")
#geom_bar(stat = "identity")



qplot(x = state,
      y = perchsd,
      data = midwest,
      main = "Percentage with HS Diploma VS. State")
ggplot(midwest, aes(x = state, y = perchsd)) + geom_violin(scale = "area")

ggplot(midwest, aes(x = state, y = perchsd)) + geom_boxplot()










