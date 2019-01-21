#PART2
########PART2- LOG GAMMA LOOP ########

log_gamma_loop <- function(n) {
  val = 0
  if (n == 1)
    return(0)
  for(i in 2:n) {
    val = val + log(i-1)
  }
  return(val)
}

#PART3
########PART3- LOG GAMMA RECURSIVE ########

log_gamma_recursive <- function(n) {
  if (n==1)
    return(0)
  return(log(n-1) + log_gamma_recursive(n-1))
}

#PART4
########PART4- SUM OF LOG GAMMA LOOP AND SUM OF LOG GAMMA RECURSIVE ########

sum_log_gamma_loop <- function(n) {
  sumlg = 0
  if (n==1)
    return(0)
  for (i in 2:n) {
    sumlg = sumlg + log_gamma_loop(i)
  }
  return(sumlg)
}

sum_log_gamma_recursive <- function(n) {
  sumlgr = 0
  if (n==1)
    return(0)
  for (j in 2:n) {
    sumlgr = sumlgr + log_gamma_recursive(j)
  }
  return(sumlgr)
}

#PART5
########PART5- COMPARE RESULTS TO BUILT IN R FUNCTION ########

sum_lgamma <- function(n) {
  sumlg = 0
  if (n==1)
    return(0)
  for (k in 2:n) {
    sumlg = sumlg + lgamma(k)
  }
  return(sumlg)
}

for(i in 1:3000) {
  times_loop = system.time(sum_log_gamma_loop(i))['elapsed']
  y_loop <- append(y_loop,times_loop)
}


for (j in 1:3000) {
  times = system.time(sum_lgamma(j))['elapsed']
  y <- append(y,times)
}


for (k in 1:1750) {
  times_recursive = system.time(sum_log_gamma_recursive(k))['elapsed']
  y_recursive <- append(y_recursive, times_recursive)
}


plot(y_loop, type = "o", col = "red", xlab = "Value of n", ylab = "System Execution Time", main = "SUM_LOG_GAMMA_LOOP")
plot(y, type = "o", col = "blue", xlab = "Value of n", ylab = "System Execution Time", main = "SUM_LGAMMA")
plot(y_recursive, type = "o", col = "forestgreen", xlab = "Value of n", ylab = "System Execution Time", main = "SUM_LOG_GAMMA_RECURSIVE")