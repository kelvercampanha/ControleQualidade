rm(list=ls(all=T))#Limpando a memória
setwd("C:/Users/user/Desktop/Controle estatístico de Qualidade")#Diretório de trabalho

#Dados do exemplo
ex1=matrix(c(1004.6, 997.3,1003.0,1005.9,995.8,
              1001.6,1008.6,997.9,1001.3,999.1,
              999.1,992.6,1001.1,1001.6,1002.9,
              1007.9,997.5,991.3,997.8,1000.8,
              999.5,995.6,1004.3,995.6,991.4,
              1003.3,996.8,997.2,993.6,1000.1,
              999.7,1012.1,995.2,1001.8,1002.2,
              1000.1,995.3,990.0,997.5,1003.2,
              1004.3,1001.4,1001.6,999.1,996.4,
              999.0,995.8,989.9,995.1,1002.8,
              1003.2,1004.4,993.5,994.6,997.6,
              996.2,1017.3,993.6,996.5,1003.7,
              1014.0,1008.9,1004.1,1007.9,1000.7,
              1002.2,996.6,1002.7,1004.2,1001.8,
              998.3,997.5,1006.1,996.5,998.1,
              995.8,1000.8,999.1,1002.5,1001.0,
              1004.1,1003.0,1004.8,997.9,999.9,
              1000.1,994.9,1000.1,1004.9,997.3,
              1000.2,996.1,998.0,1006.1,999.4,
              1002.3,999.0,1000.8,1000.7,998.0,
              998.3,998.1,1004.2,1002.1,991.3,
              997.1,1000.7,999.8,1000.6,1001.7,
              1003.6,996.1,1001.4,998.0,991.8,
              999.9,1006.4,1005.1,999.8,1003.0,
              1007.3,999.8,992.5,996.2,998.2),nrow=25,ncol=5,byrow=T)

#Gráfico de controle para amplitude
par(mfrow=c(2,2))

Ri=apply(ex1,1,max,na.rm=TRUE)-apply(ex1,1,min,na.rm=TRUE)
Rbar=mean(Ri)
d2=2.326
d3=0.864
LSCr=ifelse((d2+3*d3)*Rbar/d2<0,0,(d2+3*d3)*Rbar/d2)
LICr=ifelse((d2-3*d3)*Rbar/d2<0,0,(d2-3*d3)*Rbar/d2)

plot(Ri,type="l",ylim=c(LICr-1,LSCr+1),xlab="Amostra",ylab="Amplitude",main="Gráfico de Controle para Amplitude")
points(Ri,col=c(ifelse(Ri>LSCr|Ri<LICr,"red","blue")),pch=19)
abline(h=c(0,LSCr),col="red",lty=2)
abline(h=c(0,LICr),col="red",lty=2)
abline(h=c(0,Rbar),col="blue",lty=2)

#Removendo a observação fora de controle
Ri_=ifelse(Ri>LSCr|Ri<LICr,NA,Ri)
Rbar_=mean(Ri_,na.rm = T)
d2=2.326
d3=0.864
LSCr=ifelse((d2+3*d3)*Rbar_/d2<0,0,(d2+3*d3)*Rbar_/d2)
LICr=ifelse((d2-3*d3)*Rbar_/d2<0,0,(d2-3*d3)*Rbar_/d2)

plot(Ri_,type="l",ylim=c(LICr-1,LSCr+1),xlab="Amostra",ylab="Amplitude",main="Gráfico de Controle para Amplitude")
points(Ri_,col=c(ifelse(Ri_>LSCr|Ri_<LICr,"red","blue")),pch=19)
abline(h=c(0,LSCr),col="red",lty=2)
abline(h=c(0,LICr),col="red",lty=2)
abline(h=c(0,Rbar_),col="blue",lty=2)

#Gréfico de controle para a média
id_remove=which(Ri_ %in% NA)
Xb=apply(ex1,1,mean,na.rm=TRUE);Xb[id_remove]=NA
Xbb=mean(Xb,na.rm = T)
LSCx=ifelse(Xbb+3*(Rbar_/d2)/sqrt(ncol(ex1))<0,0,Xbb+3*(Rbar_/d2)/sqrt(ncol(ex1)))
LICx=ifelse(Xbb-3*(Rbar_/d2)/sqrt(ncol(ex1))<0,0,Xbb-3*(Rbar_/d2)/sqrt(ncol(ex1)))

plot(Xb,type="l",ylim=c(LICx-1,LSCx+1),xlab="Amostra",ylab="Média",main="Gráfico de Controle para Média")
points(Xb,col=c(ifelse(Xb>LSCx|Xb<LICx,"red","blue")),pch=19)
abline(h=c(0,LSCx),col="red",lty=2)
abline(h=c(0,LICx),col="red",lty=2)
abline(h=c(0,Xbb),col="blue",lty=2)

#Removendo a observação fora de controle
Xb_=ifelse(Xb>LSCx|Xb<LICx,NA,Xb)
Xbb_=mean(Xb_,na.rm = T)
LSCx=ifelse(Xbb_+3*(Rbar_/d2)/sqrt(ncol(ex1))<0,0,Xbb_+3*(Rbar_/d2)/sqrt(ncol(ex1)))
LICx=ifelse(Xbb_-3*(Rbar_/d2)/sqrt(ncol(ex1))<0,0,Xbb_-3*(Rbar_/d2)/sqrt(ncol(ex1)))

plot(Xb_,type="l",ylim=c(LICx-1,LSCx+1),xlab="Amostra",ylab="Média",main="Gráfico de Controle para Média")
points(Xb_,col=c(ifelse(Xb_>LSCx|Xb_<LICx,"red","blue")),pch=19)
abline(h=c(0,LSCx),col="red",lty=2)
abline(h=c(0,LICx),col="red",lty=2)
abline(h=c(0,Xbb_),col="blue",lty=2)

#Gráfico de poder gráfico de Xbarra
png(file="plot.png",width=1200,height=600, pointsize=24)
par(mfrow=c(1,3))
#png(file="plot.png",width=800,height=600)
delta=c(0,0.5,1,1.5,2,2.5,3)
Pd_n3=pnorm(-3+delta*sqrt(3))+pnorm(-3-delta*sqrt(3))
Pd_n5=pnorm(-3+delta*sqrt(5))+pnorm(-3-delta*sqrt(5))
Pd_n8=pnorm(-3+delta*sqrt(8))+pnorm(-3-delta*sqrt(8))
plot(delta,Pd_n3,col="royalblue4",xlab = "Variação da média",ylab ="Poder",type="l",lty=2)
lines(delta,Pd_n5,col="lightgreen",lty=2)
lines(delta,Pd_n8,col="red",lty=2)
points(delta,Pd_n3,col="royalblue4",pch=19)
points(delta,Pd_n5,col="lightgreen",pch=19)
points(delta,Pd_n8,col="red",pch=19)
legend("topleft", legend = c("n = 3","n = 5", "n = 8"), 
       col = c("royalblue4","lightgreen","red"),
       lty = 2, pch=19,bty="n")

#Gráfico rapidez detecção de descontroles
delta=c(0.5,0.75,1,1.25,1.5,1.75,2)
nma_n3=1/(pnorm(-3+delta*sqrt(3))+pnorm(-3-delta*sqrt(3)))
nma_n5=1/(pnorm(-3+delta*sqrt(5))+pnorm(-3-delta*sqrt(5)))
nma_n8=1/(pnorm(-3+delta*sqrt(8))+pnorm(-3-delta*sqrt(8)))
plot(delta,nma_n3,col="royalblue4",xlab = "Variação da média",ylab ="NMA",type="l",lty=2)
lines(delta,nma_n5,col="lightgreen",lty=2)
lines(delta,nma_n8,col="red",lty=2)
points(delta,nma_n3,col="royalblue4",pch=19)
points(delta,nma_n5,col="lightgreen",pch=19)
points(delta,nma_n8,col="red",pch=19)
legend("topright", legend = c("n = 3","n = 5", "n = 8"), 
       col = c("royalblue4","lightgreen","red"),
       lty = 2, pch=19,bty="n")

#Curvas de probabilidade de não detecção
pd_n3=Pd_n3=pnorm(-3+1*sqrt(3))+pnorm(-3-1*sqrt(3))
pd_n5=Pd_n3=pnorm(-3+1*sqrt(5))+pnorm(-3-1*sqrt(5))
pd_n8=Pd_n3=pnorm(-3+1*sqrt(8))+pnorm(-3-1*sqrt(8))

amostra=c(1,2,3,4,5,6,7,8,9,10)
Prob_n3=pd_n3*(1-pd_n3)^(amostra-1)
ProbCs_n3=cumsum(Prob_n3)

plot(amostra,1-ProbCs_n3,col="royalblue4",ylim=c(0,1),xlab = "Index da amostra",ylab ="Probabilidade",type="l",lty=2)
points(amostra,1-ProbCs_n3,col="royalblue4",pch=19)

Prob_n5=pd_n5*(1-pd_n5)^(amostra-1)
ProbCs_n5=cumsum(Prob_n5)
lines(amostra,1-ProbCs_n5,col="lightgreen",lty=2)
points(amostra,1-ProbCs_n5,col="lightgreen",pch=19)

Prob_n8=pd_n8*(1-pd_n8)^(amostra-1)
ProbCs_n8=cumsum(Prob_n8)
lines(amostra,1-ProbCs_n8,col="red",lty=2)
points(amostra,1-ProbCs_n8,col="red",pch=19)

legend("topright", legend = c("n = 3","n = 5", "n = 8"), 
       col = c("royalblue4","lightgreen","red"),
       lty = 2, pch=19,bty="n")
dev.off()

#Capacidade do processo
LIE=985
LSE=1015
d=(LIE+LSE)/2

mu=rep(c(1000,1002),2)
sa=c(rep(2,2),rep(4,2))
Cp=(LSE-LIE)/(6*sa)
Cpk=pmin((LSE-mu)/(3*sa),(mu-LIE)/(3*sa))
Cpm=(LSE-LIE)/(6*sqrt(sa^2+(d-mu)^2))
ppm=(pnorm((LSE-mu)/sa,lower.tail=F)+pnorm((LIE-mu)/sa,lower.tail=T))*1000000
df=round(as.data.frame(cbind(mu,sa,Cp,Cpk,Cpm,ppm)),2)

#Inspeção da Qualidade
perm = function(n, x) {
        factorial(n) / factorial(n-x)
}

comb = function(n, x) {
        factorial(n) / factorial(n-x) / factorial(x)
}

calc_prop_rejeicao=function(n,d0,p)
{
        (comb(n,d0))*(p^d0)*((1-p)^(n-d0)) 
}
calc_prop_rejeicao(20,2,0.1)
calc_prop_rejeicao(20,1,0.1)
calc_prop_rejeicao(20,0,0.1)
1-(calc_prop_rejeicao(20,2,0.1)+calc_prop_rejeicao(20,1,0.1)+calc_prop_rejeicao(20,0,0.1))

   