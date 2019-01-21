log_factorial <- function (n) {
  # Return the log of factorial(n) for any integer n > 0
  if (n <= 1)
    return (0)
  return (log(n) + log_factorial(n - 1))
}

sum_log_factorial <- function (n) {
  # Return the sum of log_factorial(i) for i in 1..n
  sum <- 0
  for(i in seq(1, n, 1)) {
    sum <- sum + log_factorial(i)
  }
  return (sum)
}

#starttime <- Sys.time()
fibonacci <- function(n) {
  # Return nth Fibonacci number
  if (n <= 1)
    return (n)
  return (fibonacci(n - 1) + fibonacci(n - 2))
}

#endtime <- Sys.time()
#timetaken <- endtime - starttime
#timetaken
install.packages("ggplot2", dependencies = TRUE)

library(ggplot2)

a = 1:100
for (i in a)  y = fibonacci(i)
return(y)

  y = fibonacci(a)

qplot(system.time(fibonacci(a)),a)

y1 = NULL
y2 = NULL

times1 = NULL
times2 = NULL

y3 = NULL
times3 = NULL
for (i in 1:500) {
  times1 = system.time(log_factorial(i))['elapsed']
  y1 <- append(y1,times1)
  times2 = system.time(sum_log_factorial(i))['elapsed']
  y2 <- append(y2,times2)
}

for (j in 1:35) {
  times3 = system.time(fibonacci(j))['elapsed']
  y3 <- append(y3,times3)
}
length(y3)
y2

plot(y1, type = "o", col = "red", axes = FALSE, ann = FALSE)
box()
lines(y2, type="o", pch=22, lty=2, col="blue")
title(main = "log_factorial/sum_log_factorial run time", col.main = "black", font.main = 1)
title(xlab = "Value of n")
title(ylab = "System Run Time")
legend(1,y2,c("log_factorial", "sum_log_factorial"), cex = 0.8, col = c("red","blue"), pch = 21:22, lty = 1:2)

plot(y3, type = "o", col = "forestgreen"); title(main = "FIBONACCI RUN TIME", col.main = "black", font.main = 1)
