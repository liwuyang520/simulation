library(devtools)
library(BayesRecipe)
library(bayeslm)
library(MASS)

shuju.std <- data.frame(cbind(scale(x[,1:20]),Y))    #数字对应维度，需要时更改，此项为标准化
names(shuju.std)[21] <- 'Y'                          #将Y于x放到同一矩阵中
library(caret)
train <-createDataPartition(y=shuju.std$X1,p=0.70,list=FALSE)    #定义测试组与实验组
data.train <- shuju.std[train,]
data.test <- shuju.std[-train,]
y.train   = data.train$Y - mean(data.train$Y)
y.test <- data.test$Y - mean(data.test$Y)
x.train = scale(as.matrix(data.train[,1:20]))           
x.test = scale(as.matrix(data.test[,1:20]))

fit_BayesRLasso1<-BayesRLasso(x.train, y.train,lambda.estimate = 'AP')    #先验方法求λ
y.pred.BayesRLasso1<-x.test%*%fit_BayesRLasso1$beta

fit_BayesRLasso2<-BayesRLasso(x.train, y.train, method ='SMN' ,lambda.estimate = 'EB')  #经验分布方法求
y.pred.BayesRLasso2<-x.test%*%fit_BayesRLasso2$beta

fit_BayesRLasso3<-BayesRLasso(x.train, y.train, method ='SMN' ,lambda.estimate = 'MCMC')   #MCMC方法求
y.pred.BayesRLasso3<-x.test%*%fit_BayesRLasso3$beta

set.seed(1234)       #lasso方法
library(glmnet)
lasso.cv=cv.glmnet(x.train,y.train,alpha = 1)  
lambda.cv.lasso=lasso.cv$lambda.min          
lasso.sol=glmnet(x.train,y.train,alpha = 1)    
lasso.coeff=predict(lasso.sol,type="coef",s=lambda.cv.lasso)
y.pred.lasso=x.test%*%lasso.coeff[-1]

fit_rLASSO = rrLASSO.S5(x.train,y.train)   #rlasso方法
rlasso.coeff<-fit_rLASSO$beta
y.pred.rlasso<-x.test%*%rlasso.coeff[-1]

#求MSE，比较优劣
mean((y.pred.lasso-y.test)^2)
mean((y.pred.rlasso - y.test)^2)
mean((y.pred.BayesRLasso1 - y.test)^2)
mean((y.pred.BayesRLasso2 - y.test)^2)
mean((y.pred.BayesRLasso3 - y.test)^2)