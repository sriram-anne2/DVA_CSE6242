#*****************************************************
###GT USERNAME: sanne31@gatech.edu################
#*****************************************************

library(ggplot2)
data("midwest")
data("diamonds")
#####QUESTION 1) PROFESSIONAL EDUCATION BY STATE############
names(midwest)
summary(midwest)

#Using boxplot to visualize percprof grouped by state
ggplot(midwest, aes(x = state, y = percprof)) + 
  geom_boxplot() + 
  ggtitle("Professional Education By State") + 
  xlab("State") + 
  ylab("Percentage of Professional Education")

#implemented facets to include visualization based on county
#classified based on metro or non metro county
#found by inmetro column in midwest dataset
midwest$nmf[midwest$inmetro == 0] = 'Non Metro County'
midwest$nmf[midwest$inmetro == 1] = 'Metro County'
#created formula above by adding a column to the dataset to be used below for facet division
qplot(x = state,
      y = percprof,
      facets = .~nmf,
      data = midwest,
      main = "Professional Education By State")

####Finding highest and lowest prof eduction by population######
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

#####QUESTION 2) SCHOOL AND COLLEGE EDUCATION BY STATE#######

#using scatter plots/point plots/line plots for 
#variables having character columns will not be of any use as no observation 
#can be made, hence i am using boxplots

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

#perchsd vs percollege 
#has 2 continuous variables
qplot(x = perchsd,
      y = percollege,
      data = midwest,
      main = "Percentage of HS Diplomas vs Percentage of College Degree")

#####QUESTION 4) RANDOM SCATTERPLOTS#######

#generating a random scatterplot for N = 10 using the runif() function
plot(runif(10))
#generating a random scatterplot for N = 100 using the runif() function
plot(runif(100))
#generating a random scatterplot for N = 1000 using the runif() function
plot(runif(1000))
#generating a random scatterplot for N = 10000 using the runif() function
plot(runif(10000))


######plotting and saving files to working directory#################
  jpeg('n10.jpg')
  plot(runif(10))
  dev.off()
  jpeg('n100.jpg')
  plot(runif(100))
  dev.off()
  jpeg('n1000.jpg')
  plot(runif(1000))
  dev.off()
  jpeg('n10000.jpg')
  plot(runif(10000))
  dev.off()
  jpeg('n100000.jpg')
  plot(runif(100000))
  dev.off()
          
  png('n100000.png')
  plot(runif(100000))
  dev.off()
  png('n10000.png')
  plot(runif(10000))
  dev.off()
  png('n1000.png')
  plot(runif(1000))
  dev.off()
  png('n100.png')
  plot(runif(100))
  dev.off()
  png('n10.png')
  plot(runif(10))
  dev.off()
          
  pdf('n10.pdf')
  plot(runif(10))
  dev.off()
  pdf('n100.pdf')
  plot(runif(100))
  dev.off()
  pdf('n1000.pdf')
  plot(runif(1000))
  dev.off()
  pdf('n10000.pdf')
  plot(runif(10000))
  dev.off()
  pdf('n100000.pdf')
  plot(runif(100000))
  dev.off()
          
  postscript('n10.ps')
  plot(runif(10))
  dev.off()
  postscript('n100.ps')
  plot(runif(100))
  dev.off()
  postscript('n1000.ps')
  plot(runif(1000))
  dev.off()
  postscript('n10000.ps')
  plot(runif(10000))
  dev.off()
  postscript('n100000.ps')
  plot(runif(100000))
  dev.off()
#######Creating Data Frame with N values and file sizes for each type#######
N_Val = c(10,100,1000,10000,100000)
jpeg_size = c(11,18,58,90,17)
pdf_size = c(5,6,12,69,564)
png_size = c(3,4,11,22,4)
ps_size = c(7,9,31,251,2448)

Random_DF = data.frame(N_Val, jpeg_size, pdf_size, png_size, ps_size)
#####Changing values to log base 10 values#########
sized <- log10(Random_DF)

#####Plotting file sizes with N values############
plot(sized$N_Val, sized$jpeg_size, type = "b", col = "red", xlab = "LOG10(N Value)", ylab = "LOG10(File Size(kb))")
lines(sized$N_Val, sized$pdf_size, type = "b", col = "blue")
lines(sized$N_Val, sized$png_size, type = "b", col = "green")
lines(sized$N_Val, sized$ps_size, type = "b", col = "orange")
legend("topleft", c("jpeg","pdf","png","ps"), col = c("red","blue","green","orange"), lty = c(1,1,1,1))
          
############QUESTION 5) DIAMONDS#######################

#discrete variable color - using bar graph
ggplot(diamonds, aes(color)) + geom_bar()
#price and carat are continuous variables - using histograms
ggplot(diamonds, aes(price)) + geom_histogram()
ggplot(diamonds, aes(carat)) + geom_histogram()


##plotting color vs price##
ggplot(diamonds, aes(x = color, y = price)) +
  ggtitle("Price vs Color") + 
  geom_boxplot() +
  xlab("Color") +
  ylab("Price")

##plotting color vs carat##
ggplot(diamonds, aes(x = color, y = carat)) +
  ggtitle("Carat vs Color") + 
  geom_boxplot() +
  xlab("Color") +
  ylab("Carat")

##plotting carat vs price##
ggplot(diamonds, aes(x = carat, y = price)) +
  ggtitle("Carat vs Price") + 
  geom_point() +
  xlab("Carat") +
  ylab("Price")
