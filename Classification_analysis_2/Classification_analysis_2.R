
Appel<-read.table("~/APPEL.txt",header = T)
Appel$group<-as.factor(Appel$group)
attach(Appel)
colMeans(Appel[,-1])


#Classification with Linear Discriminant Analysis
#library(MASS)
Appel.lda= lda(group ~ .,data=Appel)
pred = predict(Appel.lda,Appel)
table.lda<-table(Appel$group,pred$class,dnn = c('Actual Group','Predicted Group'))
table.lda
(Accuracy.lda<-sum(diag(table.lda))/sum(table.lda))
(error.rate.lda<-1-Accuracy.lda)



# Cross-Validation of Classification with Linear Discriminant Analysis
#library(MASS)
Appel.lda.cv= lda(group ~ .,data=Appel,CV=T)
table.lda.cv<-table(Appel$group,Appel.lda.cv$class,dnn = c('Actual Group','Predicted Group'))
table.lda.cv
(Accuracy.lda.cv<-sum(diag(table.lda.cv))/sum(table.lda.cv))
(error.rate.lda.cv<-1-Accuracy.lda.cv)




# Cross-Validation of Quadratic Discriminant Analysis Classifications
#library(MASS)
Appel.qda.cv= qda(group ~ .,data=Appel,CV=T)
table.qda.cv<-table(Appel$group,Appel.qda.cv$class,dnn = c('Actual Group','Predicted Group'))
table.qda.cv
(Accuracy.qda.cv<-sum(diag(table.qda.cv))/sum(table.qda.cv))
(error.rate.qda.cv<-1-Accuracy.qda.cv)


# Quadratic Discriminant Analysis in R
Appel.qda = qda(group ~ .,data=Appel)
pred.q = predict(Appel.qda,Appel)
table.qda<-table(Appel$group,pred.q$class,dnn = c('Actual Group','Predicted Group'))
table.qda
(Accuracy.qda<-sum(diag(table.qda))/sum(table.qda))
(error.rate.qda<-1-Accuracy.qda)
