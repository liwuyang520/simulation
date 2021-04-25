library(devtools)
library(BayesRecipe)
library(bayeslm)
library(MASS)
library(caret)
i<-1            #定义循环初始值，要求循环100次
mselasso<-c()
mserlasso<-c()
mseBayesRLasso1<-c()
mseBayesRLasso2<-c()
mseBayesRLasso3<-c()    #定义mse矩阵，最终求均值
repeat{
  #set.seed(1234)
  p = 20   #维度
  n = 50    #样本数
  var=5       #定义方差
  a<-diag(n)       
  e = rnorm(n,0,var*a)    #随机生成扰动项
  
  sigma<-diag(p)    #情况一生成x
  
  mean<-rep(0,p)
  x<- mvrnorm(n,mean,sigma)
  b = rnorm(p)     #随机生成系数
  Y = x %*% b + e    #根据生成的x、系数与扰动项，生成y
  
  shuju.std <- data.frame(cbind(scale(x[,1:20]),Y))    #数字对应维度，需要时更改，此项为标准化
  names(shuju.std)[21] <- 'Y'                          #将Y于x放到同一矩阵中
  
  train <-createDataPartition(y=shuju.std$X1,p=0.70,list=FALSE)    #定义测试组与实验组
  data.train <- shuju.std[train,]
  data.test <- shuju.std[-train,]      #x标准化
  y.train   = data.train$Y - mean(data.train$Y)
  y.test <- data.test$Y - mean(data.test$Y)     #y标准化
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
  msel<-mean((y.pred.lasso-y.test)^2)
  mser<-mean((y.pred.rlasso - y.test)^2)
  mseba<-mean((y.pred.BayesRLasso1 - y.test)^2)
  msebb<-mean((y.pred.BayesRLasso2 - y.test)^2)
  msebc<-mean((y.pred.BayesRLasso3 - y.test)^2)
  
  
  #将生成的mse值放入矩阵
  mselasso<-c(mselasso,msel)
  mserlasso<-c(mserlasso,mser)
  mseBayesRLasso1<-c(mseBayesRLasso1,mseba)
  mseBayesRLasso2<-c(mseBayesRLasso2,msebb)
  mseBayesRLasso3<-c(mseBayesRLasso3,msebc)
  
  i=i+1
  if(i>100){
    break
  }
}
#求均值
mean(mselasso)
mean(mserlasso)
mean(mseBayesRLasso1)
mean(mseBayesRLasso2)
mean(mseBayesRLasso3)