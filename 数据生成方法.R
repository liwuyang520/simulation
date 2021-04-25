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
  a<-matrix(0,n,n)      #生成n维单位阵
  for(i in 1:n){
    a[i,i]<-1        #对角元素定位1
  }
  rho<-0.3       #定义rho值
  b<-matrix(rho,n,n)    #定义n维矩阵，其中值为rho
  b[!upper.tri(b, diag = FALSE)] <- 0    #取b矩阵的上三角，不包括对角元素
  return(b+t(b)+a)     #生成对称阵
}
sigma<-persymmetrix(p)

#生成情况三：
persymmetrix<-function(n){
  v<-c()      #定义向量，存储对称矩阵的值
  for(i in 1:n){
    for(j in 1:n){
      rho<-0.3     #定义rho
      m<-rho^abs(i-j)    #生成矩阵内数值
      v<-c(v,m)   #将生成的值储存在向量v中
    }
  }
  b<-matrix(v,n,n,byrow = T)   #生成对称阵b
}
sigma<-persymmetrix(p)

#根据定义的协方差矩阵生成变量x
mean<-rep(0,p)
x<- mvrnorm(n,mean,sigma)
b = rnorm(p)
Y = x %*% b + e