rm(list=ls())
setwd("/Users/liaoyantai/Documents/job/data incubator")

data.crime<-read.delim("communities.data.txt",sep=",",header=F,na.string="?")
data.name<-read.table("attribute state numeric.doc",sep=" ",header=F)
colnames(data.crime)<-data.name[,2]
data.crime.2<-data.crime[,c(6:128)]

nrow(data.crime.2[!complete.cases(data.crime.2),]) # 1675
na.value<-c()
for(i in 1:1994){
  na.value<-c(na.value,length(which(is.na(data.crime.2[i,]))))
}
max(na.value) # 23

library(cluster)
dist<-as.matrix(daisy(data.crime.2,stand=F))

for(n in c(20,10)){
  for(r in c(which(!complete.cases(data.crime.2)))){
    data.crime.2[r,which(is.na(data.crime.2[r,]))]<-apply(data.frame(data.crime.2[c(as.integer(names(sort(dist[r,])[2:n]))),
                                                                        which(is.na(data.crime.2[r,]))]),2,median,na.rm=T)
  }
}

nrow(data.crime.2[!complete.cases(data.crime.2),]) # 0

for(i in 1:1994){
  if(data.crime.2$LemasGangUnitDeploy[i]<0.25){
    data.crime.2$LemasGangUnitDeploy[i]<-0
}else if(data.crime.2$LemasGangUnitDeploy[i]<0.75){
  data.crime.2$LemasGangUnitDeploy[i]<-0.5
}else{
  data.crime.2$LemasGangUnitDeploy[i]<-1
}
}

set.seed(1)
training<-sample(1:nrow(data.crime.2),0.8*nrow(data.crime.2))
crime.2.train<-data.crime.2[training,]
test<-data.crime.2[-training,]
y.test.true<-test$ViolentCrimesPerPop
crime.2.test<-test[,-123]


# OLS
train.fit.ols<-lm(ViolentCrimesPerPop~.,data=crime.2.train)
test.predict.ols<-predict(train.fit.ols,crime.2.test)
error.ols.test<-mean((test.predict.ols-y.test.true)^2)
error.ols.test # 0.01919984
summary(train.fit.ols) 

# Forwards
train.fit.fwd<-step(lm(ViolentCrimesPerPop~1,data=crime.2.train),direction="forward",
                    scope=formula(lm(ViolentCrimesPerPop~.,data=crime.2.train)))
test.predict.fwd<-predict(train.fit.fwd,crime.2.test)
error.fwd.test<-mean((test.predict.fwd-y.test.true)^2)
error.fwd.test  # 0.01944217
summary(train.fit.fwd)

# backwards
train.fit.bwd<-step(lm(ViolentCrimesPerPop~.,data=crime.2.train),direction="backward",
                    scope=formula(lm(ViolentCrimesPerPop~.,data=crime.2.train)))
test.predict.bwd<-predict(train.fit.bwd,crime.2.test)
error.bwd.test<-mean((test.predict.bwd-y.test.true)^2)
error.bwd.test # 0.01922707
summary(train.fit.bwd) 

# Lasso
library(glmnet)
x<-as.matrix(crime.2.train[,1:122])
y<-crime.2.train[,123]
z<-as.matrix(crime.2.test)
train.fit.lasso<-glmnet(x,y,alpha=1)
cv.out<-cv.glmnet(x,y,alpha=1)
bestlambda.lasso<-cv.out$lambda.min
bestlambda.lasso # 0.001144234
test.predict.lasso<-predict(train.fit.lasso,s=bestlambda.lasso,newx=z,type="response")
error.lasso.test<-mean((test.predict.lasso-y.test.true)^2)
error.lasso.test # 0.01803607
coef.lasso<-predict(train.fit.lasso,s=bestlambda.lasso,type="coefficients")


# Ridge

train.fit.ridge<-glmnet(x,y,alpha=0)
cv.out<-cv.glmnet(x,y,alpha=0)
bestlambda.ridge<-cv.out$lambda.min
bestlambda.ridge # 0.01739134
test.predict.ridge<-predict(train.fit.ridge,s=bestlambda.ridge,newx=z,type="response")
error.ridge.test<-mean((test.predict.ridge-y.test.true)^2)
error.ridge.test # 0.01813027
coef.ridge<-predict(train.fit.ridge,s=bestlambda.ridge,type="coefficients")
coef.ridge

# pcr
library(pls)
train.fit.pcr<-pcr(ViolentCrimesPerPop~.,data=crime.2.train,scale=TRUE,validation="CV")
summary(train.fit.pcr)
quartz()
validationplot(train.fit.pcr,val.type="MSEP")
pcr_test_error_store<-c()
for (i in 1:122){
  test.predict.pcr<-predict(train.fit.pcr,crime.2.test,ncomp = i)
  error.pcr.test<-mean((test.predict.pcr-y.test.true)^2)
  pcr_test_error_store<-c(pcr_test_error_store, error.pcr.test)
}

i<-which.min(pcr_test_error_store)
i # 94
test.predict.pcr<-predict(train.fit.pcr,crime.2.test,ncomp=i)
error.pcr.test<-mean((test.predict.pcr-y.test.true)^2)
error.pcr.test # 0.01827995

#pls

train.fit.pls<-plsr(ViolentCrimesPerPop~.,data=crime.2.train,scale=TRUE,validation="CV")
summary(train.fit.pls)
quartz()
validationplot(train.fit.pls,val.type="MSEP")
pls_test_error_store<-c()
for (i in 1:122){
  test.predict.pls<-predict(train.fit.pls,crime.2.test,ncomp = i)
  error.pls.test<-mean((test.predict.pls-y.test.true)^2)
  pls_test_error_store<-c(pls_test_error_store, error.pls.test)
}

i<-which.min(pls_test_error_store)
i # 5
test.predict.pls<-predict(train.fit.pls,crime.2.test,ncomp=i)
error.pls.test<-mean((test.predict.pls-y.test.true)^2)
error.pls.test # 0.01805598

error.test<-c("ols","fwd","bwd","lasso","ridge","pcr","pls")
error.value<-c(0.01919984,0.01944217,0.01922707,0.01803607,0.01813027,0.01827995,0.01805598)

quartz()
barplot(error.value, width = 1, names.arg = error.test, beside = T,
        col = c(1:7),  ylab = "test error ", ylim = c(0,0.03),main="The range of the test error for different algorithm")


r<-names(data.crime.2)[summary(coef.lasso)$i-1]
c<-summary(coef.lasso)$x[-1]
lasso.model<-data.frame(r,c)
lasso.model

n<-c(2:57)

quartz()
barplot(c, width = 1, names.arg =n , beside = T,
        col = c(1:57),  ylab = "value ", ylim = c(-0.3,0.3),main="lasso model coefficient")




