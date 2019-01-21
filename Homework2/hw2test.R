library(ggplot2)
data("midwest")
names(midwest)
summary(midwest$state == 'IL')


ggplot(midwest, aes(x = state, y = percprof)) + 
  geom_boxplot() + 
  ggtitle("Professional Education By State") + 
  xlab("State") + 
  ylab("Percentage of Professional Education")

midwest$nmf[midwest$inmetro == 0] = 'Non Metro County'
midwest$nmf[midwest$inmetro == 1] = 'Metro County'

qplot(x = state,
      y = percprof,
      facets = .~nmf,
      size = poptotal,
      data = midwest,
      main = "Professional Education By State")

ggplot(midwest, aes(x = state, y = percprof)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Professional Education By State") + 
  xlab("State") + 
  ylab("Percentage of Professional Education")


#perchsd vs percollege
qplot(x = perchsd,
      y = percollege,
      data = midwest,
      main = "Percentage of HS Diplomas vs Percentage of College Degree")

#state vs perchsd
ggplot(midwest, aes(x = state, y = perchsd)) + 
  geom_boxplot() +
  ggtitle("Percentage of High School Diplomas By State") + 
  xlab("State") +
  ylab("Percentage of HS Diplomas")

#state vs percollege
ggplot(midwest, aes(x = state, y = percollege)) + 
  geom_boxplot() +
  ggtitle("Percentage of College Degrees By State") + 
  xlab("State") +
  ylab("Percentage of College Degrees")
mean(popblack~inmetro, data=midwest)
?mean()

il_subset <- midwest[midwest$state == "IL",]
in_subset <- midwest[midwest$state == "IN",]
mi_subset <- midwest[midwest$state == "MI",]
oh_subset <- midwest[midwest$state == "OH",]
wi_subset <- midwest[midwest$state == "WI",]

max(il_subset$percprof)
max(in_subset$percprof)
max(mi_subset$percprof)
max(oh_subset$percprof)
max(wi_subset$percprof)

summary(il_subset)

data("mtcars")


ggplot(mtcars, aes(x = cyl, y = mpg)) +
  ggtitle("Car Mileage based on Number of Cylinders") +
  geom_boxplot() +
  xlab("Number of Cylinders") + 
  ylab("MPG")

ggplot(mtcars, aes(mpg)) +
  ggtitle("MPG of Car") +
  geom_histogram(binwidth = 3)

plot(runif(10))
plot(runif(10000))

sizes <- c(10,100,1000,10000,100000)

postscript('n100000.ps')
plot(runif(100000))
dev.off()


n = c(2, 3, 5)
s = c("aa", "bb", "cc") 
b = c(TRUE, FALSE, TRUE)

N_Val = c(10,100,1000,10000,100000)
jpeg_size = c(11,18,58,90,17)
pdf_size = c(5,6,12,69,564)
png_size = c(3,4,11,22,4)
ps_size = c(7,9,31,251,2448)

Random_DF = data.frame(N_Val, jpeg_size, pdf_size, png_size, ps_size)
sized <- log10(Random_DF)

x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)

plot(x,y1,type="l",col="red")
lines(x,y2,col="green")

p = seq(-2,2,length.out = 30)
q = p^2

qplot(p,q,geom = c("point","line"))

plot(sized$N_Val, sized$jpeg_size, type = "b", col = "red", xlab = "LOG10(N Value)", ylab = "LOG10(File Size(kb))")
lines(sized$N_Val, sized$pdf_size, type = "b", col = "blue")
lines(sized$N_Val, sized$png_size, type = "b", col = "green")
lines(sized$N_Val, sized$ps_size, type = "b", col = "orange")
legend("topleft", c("jpeg","pdf","png","ps"), col = c("red","blue","green","orange"), lty = c(1,1,1,1))





data("diamonds")
help("diamonds")

ggplot(diamonds, aes(price)) + geom_histogram()

ggplot(diamonds, aes(x = color, y = carat)) +
  ggtitle("Carat vs Color") + 
  geom_boxplot() +
  xlab("Color") +
  ylab("Carat")

ggplot(diamonds, aes(x = color, y = price)) +
  ggtitle("Price vs Color") + 
  geom_boxplot() +
  xlab("Color") +
  ylab("Price")

ggplot(diamonds, aes(x = carat, y = price)) +
  ggtitle("Carat vs Price") + 
  geom_point() +
  xlab("Carat") +
  ylab("Price")




il_subset <- midwest[midwest$state == "IL",]
in_subset <- midwest[midwest$state == "IN",]
mi_subset <- midwest[midwest$state == "MI",]
oh_subset <- midwest[midwest$state == "OH",]
wi_subset <- midwest[midwest$state == "WI",]

il_subset$tot_prof_ed = (il_subset$poptotal * il_subset$percprof ) / 100
in_subset$tot_prof_ed = (in_subset$poptotal * in_subset$percprof ) / 100
mi_subset$tot_prof_ed = (mi_subset$poptotal * mi_subset$percprof ) / 100
oh_subset$tot_prof_ed = (oh_subset$poptotal * oh_subset$percprof ) / 100
wi_subset$tot_prof_ed = (wi_subset$poptotal * wi_subset$percprof ) / 100

il_prof_ed_rate = sum(il_subset$tot_prof_ed)/sum(il_subset$poptotal)
in_prof_ed_rate = sum(in_subset$tot_prof_ed)/sum(in_subset$poptotal)
mi_prof_ed_rate = sum(mi_subset$tot_prof_ed)/sum(mi_subset$poptotal)
oh_prof_ed_rate = sum(oh_subset$tot_prof_ed)/sum(oh_subset$poptotal)
wi_prof_ed_rate = sum(wi_subset$tot_prof_ed)/sum(wi_subset$poptotal)

state <- c("IL","IN","MI","OH","WI")
edurate <- c(il_prof_ed_rate, in_prof_ed_rate, mi_prof_ed_rate, oh_prof_ed_rate, wi_prof_ed_rate)
high_low <- data.frame(state, edurate)
high_low$edurate <- 100 * high_low$edurate
high_low
