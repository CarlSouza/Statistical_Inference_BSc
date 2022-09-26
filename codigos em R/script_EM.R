n=100
m1=100
m2=120
s1=4
s2=6
pr=0.5
w=rbinom(n,1,pr)
x=rnorm(n,m1,s1)*(w==0)+rnorm(n,m2,s2)*(w==1) 
hist(x)
#funcao para calcular densidade
fc=function(x,m1,m2,s1,s2,prob){
  fx=(1-prob)*dnorm(x,m1,s1)+prob*dnorm(x,m2,s2)
  return(fx)
}
grad=seq(60,200,by=0.1)
fx=fc(grad,m1,m2,s1,s2,pr)
#plot(grad,fx,type="l")
hist(x,ylim = range(fx),prob=T)
lines(grad,fx)

#param[1] Media 1
#param[2] Media 2
#param[3] variancia 1
#param[4] variancia 2
#param[5] epsilon (peso da componente 2)

y=c(195,166,188,195,179,198,161,179,200,191)
param=c(180,100,15,20,0.5) #chute inicial
e=0.0001
erro=0.5; it=0
while(erro>e){
  param0=param
  #Passo E
  part1 = (1-param[5])*dnorm(x,param[1],param[3])
  part2 = param[5]*dnorm(x,param[2],param[4])
  gam = part2/(part1+part2)
  #passo M
  aux= c(sum( (1-gam)*x)/sum(1-gam),sum(gam*x)/sum(gam))
  param[1] = min(aux)
  param[2] = max(aux)
  param[3] = sqrt(sum( ((1-gam)*(x-param[1])^2) )/sum(1-gam))
  param[4] = sqrt(sum( (gam*(x-param[2])^2) )/sum(gam))
  param[5] = mean(gam)
  #erro = sum((param0-param)^2)/(sum(param))^2
  erro = max(abs(param0-param)/(abs(param0)+0.001))  
  it=it+1
}
round(param,3); it
