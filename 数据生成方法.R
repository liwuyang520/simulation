set.seed(1234)
p = 20   #维度
n = 50    #样本数
var=5
a<-diag(n)
e = rnorm(n,0,var*a)

#分不同情况
#生成情况一：
sigma<-diag(p)

#生成情况二：
persymmetrix<-function(n){
  a<-matrix(0,n,n)
  for(i in 1:n){
    a[i,i]<-1
  }
  rho<-0.3
  b<-matrix(rho,n,n)
  b[!upper.tri(b, diag = FALSE)] <- 0
  return(b+t(b)+a)
}
sigma<-persymmetrix(p)

#生成情况三：
persymmetrix<-function(n){
  v<-c()
  for(i in 1:n){
    for(j in 1:n){
      rho<-0.3
      m<-rho^abs(i-j)
      v<-c(v,m)
    }
  }
  b<-matrix(v,n,n,byrow = T)
}
sigma<-persymmetrix(p)

#根据定义的协方差矩阵生成变量x
mean<-rep(0,p)
x<- mvrnorm(n,mean,sigma)
b = rnorm(p)
Y = x %*% b + e