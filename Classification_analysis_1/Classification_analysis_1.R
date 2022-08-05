ravan<-read.csv("~/ravanshenasi.csv",header = T)
ravan$group<-as.factor(ravan$group)


#Classification with Linear Discriminant Analysis
library(MASS)
ravan.lda= lda(group ~ .,data=ravan)
pred = predict(ravan.lda,ravan)
table.lda<-table(ravan$group,pred$class,dnn = c('Actual Group','Predicted Group'))
(Accuracy.lda<-sum(diag(table.lda))/sum(table.lda))
(error.rate.lda<-1-Accuracy.lda)


# Cross-Validation of Classification with Linear Discriminant Analysis
ravan.lda.cv= lda(group ~ .,data=ravan,CV=T)
table.lda.cv<-table(ravan$group,ravan.lda.cv$class,dnn = c('Actual Group','Predicted Group'))
(Accuracy.lda.cv<-sum(diag(table.lda.cv))/sum(table.lda.cv))
(error.rate.lda.cv<-1-Accuracy.lda.cv)