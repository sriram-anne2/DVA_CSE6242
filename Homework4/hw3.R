#GT Username: sanne31@gatech.edu
getwd()
setwd("F:/GA-Tech/Summer 2018/CSE6242/Homework3/mnist")
#Reading csv files separately into their own dataframes
if((file.exists("mnist_test.csv")) && (file.exists("mnist_train.csv"))) {
  tr_set <- read.csv(file = "mnist_train.csv", header = FALSE)
  te_set <- read.csv(file = "mnist_test.csv", header = FALSE)
}
#transposing training and testing datasets
tr_set <- as.data.frame(t(tr_set))
te_set <- as.data.frame(t(te_set))

names(tr_set)[785] <- "Label"
names(te_set)[785] <- "Label"

#partitioning training set based on given classification
train_0_1 <- tr_set[(tr_set$Label == 0) | (tr_set$Label == 1),]
train_3_5 <- tr_set[(tr_set$Label == 3) | (tr_set$Label == 5),]

#partitioning testing set based on given classification
test_0_1 <- te_set[(te_set$Label == 0) | (te_set$Label == 1),]
test_3_5 <- te_set[(te_set$Label == 3) | (te_set$Label == 5),]

dim(train_0_1) #12,665 samples
dim(train_3_5) #11,552 samples

dim(test_0_1) #2,115 samples
dim(test_3_5) #1,902 samples

#separating true class label from all partitions
true_train01 <- train_0_1$Label
train_0_1$Label <- NULL
true_train35 <- train_3_5$Label
train_3_5$Label <- NULL
true_test01 <- test_0_1$Label
test_0_1$Label <- NULL
true_test35 <- test_3_5$Label
test_3_5$Label <- NULL

#plotting images for each class in training set
#https://www.r-bloggers.com/creating-an-image-of-a-matrix-in-r-using-image/
#https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/gray.colors.html

#images for training set
tr0 <- tr_set[tr_set$Label == 0,]
tr1 <- tr_set[tr_set$Label == 1,]
tr3 <- tr_set[tr_set$Label == 3,]
tr5 <- tr_set[tr_set$Label == 5,]
tr0_mat <- matrix(unlist(tr0[1,1:784]), ncol = 28, byrow = TRUE)
tr1_mat <- matrix(unlist(tr1[1,1:784]), ncol = 28, byrow = TRUE)
tr3_mat <- matrix(unlist(tr3[1,1:784]), ncol = 28, byrow = TRUE)
tr5_mat <- matrix(unlist(tr5[1,1:784]), ncol = 28, byrow = TRUE)
image(tr0_mat, col = gray.colors(256))
image(tr1_mat, col = gray.colors(256))
image(tr3_mat, col = gray.colors(256))
image(tr5_mat, col = gray.colors(256))

#images for testing set
te0 <- te_set[te_set$Label == 0,]
te1 <- te_set[te_set$Label == 1,]
te3 <- te_set[te_set$Label == 3,]
te5 <- te_set[te_set$Label == 5,]
te0_mat <- matrix(unlist(te0[1,1:784]), ncol = 28, byrow = TRUE)
te1_mat <- matrix(unlist(te1[1,1:784]), ncol = 28, byrow = TRUE)
te3_mat <- matrix(unlist(te3[1,1:784]), ncol = 28, byrow = TRUE)
te5_mat <- matrix(unlist(te5[1,1:784]), ncol = 28, byrow = TRUE)
image(te0_mat, col = gray.colors(256))
image(te1_mat, col = gray.colors(256))
image(te3_mat, col = gray.colors(256))
image(te5_mat, col = gray.colors(256))

