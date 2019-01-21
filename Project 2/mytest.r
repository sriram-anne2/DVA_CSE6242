cat = 1:5
num = 6:24
new_feat <- data.frame(matrix(0, nrow = dim(base_model4)[1], ncol= 0))
new_feat_colnames<-c()
for (i in num){
  for (j in cat){
     tmp <- paste("relate", colnames(base_model4)[i], colnames(base_model4)[j], sep = "_")
    new_feat_colnames <- c(new_feat_colnames, tmp)
    cols = base_model4[i] * base_model4[j]
    new_feat <- cbind(new_feat, cols)
  }
}
colnames(new_feat) <- new_feat_colnames
new_df <- cbind(base_model4, new_feat)
fitted_model5 <- lm(Gross~., data = new_df)
base_model5 <- new_df[, (summary(fitted_model5)$coefficients[, 4] < 0.05)]

model5 = trainer(base_model5,tmp_train_vals) 
