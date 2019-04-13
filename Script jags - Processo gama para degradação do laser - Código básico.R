#Limpando a memória do R e carregando pacotes necessários
rm(list=ls(all=TRUE))
if(!require(coda)){install.packages("coda"); require(coda)}
if(!require(rootSolve)){install.packages("rootSolve"); require(rootSolve)}
if(!require(rjmcmc)){install.packages("rjmcmc"); require(rjmcmc)}
if(!require(R2jags)){install.packages("R2jags"); require(R2jags)}

#Fixando sementes para replicar resultados
set.seed(123)

#carregando lista com os dados
modelo.data=list(n1=240, n2=15, L=10,
y=c(
0.4741, 0.9255, 2.1147, 2.7168, 3.511, 4.3415, 4.9076, 5.4782,
     5.9925, 6.7224, 7.1303, 8.0006, 8.9193, 9.494, 9.8675, 10.9446,
0.7078, 1.2175, 1.8955, 2.2954, 2.865, 3.7454, 4.4192, 4.9894,
     5.5148, 6.0668, 6.6385, 7.1615, 7.7778, 8.4242, 8.9092, 9.2834,
0.7074, 1.1651, 1.7253, 1.9888, 2.5325, 2.9695, 3.2977, 3.9354,
     4.1613, 4.4459, 4.889, 5.2696, 5.6913, 6.0216, 6.4485, 6.8849,
0.3634, 0.6161, 1.3597, 1.9525, 2.2995, 2.9468, 3.3871, 3.7863,
     4.1116, 4.5013, 4.7209, 4.9766, 5.2812, 5.615, 5.9503, 6.1438,
0.2707, 0.6144, 1.1093, 1.773, 2.0625, 2.5835, 2.9863, 3.3765,
     4.0491, 4.626, 5.237, 5.6191, 6.0449, 6.3167, 7.0952, 7.5941,
0.3584, 1.3922, 1.9541, 2.8617, 3.4552, 3.8117, 4.533, 5.3541,
     5.9217, 6.7078, 7.7027, 8.6053, 9.1477, 9.951, 10.4857, 11.0096,
0.3637, 0.9237, 1.2098, 1.4591, 1.9323, 2.3862, 2.6799, 2.9397,
     3.4183, 4.0857, 4.5843, 4.8402, 5.1145, 5.5691, 6.114, 7.1723,
0.458, 1.0678, 1.4163, 1.7659, 2.1061, 2.3969, 2.78, 3.0187,
     3.289, 3.7542, 4.1595, 4.7575, 5.1597, 5.4591, 5.8085, 6.245,
0.5098, 0.9308, 1.5713, 1.9576, 2.5871, 3.2892, 3.6075, 4.1148,
     4.5964, 4.913, 5.3408, 5.8418, 6.3961, 6.8441, 7.2037, 7.8848,
0.4136, 1.488, 2.381, 2.995, 3.835, 4.501, 5.251, 6.256,
     7.051, 7.803, 8.321, 8.93, 9.554, 10.45, 11.28, 12.21,
0.4435, 0.9978, 1.5738, 1.9584, 2.5093, 2.8436, 3.4682, 4.007,
     4.5124, 4.8018, 5.2042, 5.6556, 6.1951, 6.5405, 6.9626, 7.4238,
0.3864, 0.7988, 1.3521, 1.741, 2.979, 3.5871, 4.0315, 4.4393,
     4.7906, 5.2205, 5.4848, 5.9639, 6.2276, 6.9877, 7.3671, 7.8841,
0.3034, 0.7448, 1.5225, 1.8548, 2.3897, 2.9473, 3.5104, 3.9207,
     5.0316, 5.4708, 5.8362, 6.4962, 6.9372, 7.3883, 7.8472, 8.087,
0.438, 0.6979, 1.0515, 1.3526, 1.7998, 2.5514, 2.8274, 3.3908,
     3.7159, 4.0906, 4.8296, 5.4145, 5.759, 6.1419, 6.5086, 6.8823,
0.5056, 0.8349, 1.2864, 1.5191, 1.9148, 2.2697, 2.7763, 3.4153,
     3.7814, 4.1053, 4.3754, 4.6295, 5.3831, 5.8391, 6.1618, 6.6247),

hours=
c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 250,
     500, 750, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000),

units=
c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
  9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
  10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12,
  12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15,
  15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15),

tpred=c(3000,3100,3200,3300,3400,3500,3600,3700,3800,3900,
        4000,4100,4200,4300,4400,4500,4600,4700,4800,4900,
        5000,5100,5200,5300,5400,5500,5600,5700,5800,5900,
        6000,6100,6200,6300,6400,6500,6600,6700,6800,6900,7000),
npred=41
)

#Parametrização e Monte Carlo para Inferência Bayesiana
troutmodel = function(){
for( i in 1:n1) 
{
     	y[i]~dgamma(alpha[i],mu)
     	alpha[i]<-igam*hours[i] 
}
	igam~dgamma(0.01,0.01) #(shape,scale)
	mu~dgamma(0.01,0.01)
}

params=c("mu","igam")
inits=function()
{
	list(mu=1,igam=0.1)
	list(mu=5,igam=0.1)
}
y=modelo.data$y
n1=modelo.data$n1
hours=modelo.data$hours
jagsfit1=R2jags::jags(data=c("y","hours","n1"), inits=inits,params,n.chains=2,n.iter=110000,n.burnin=10000,n.thin=100,model.file=troutmodel)

getsampler(modelfit=jagsfit1, sampler.name="posterior1")
set.seed(1279)
posterior1()

#Coletando resultados de interesse
output=jagsfit1$BUGSoutput[[9]][,2:3]
RR=output[1:1000,]
RR=mcmc(RR, start = 1, end = 1000, thin = 1)
RR2=output[1001:2000,]
RRmatrix=as.matrix(RR)#Coluna 1 é o igam, Coluna 2 é o mu
RR2matrix=as.matrix(RR2)
npar=ncol(RRmatrix)

#Comparando a convergência para duas cadeias distintas de simulações
m.erg1=as.matrix(cbind(cumsum(RRmatrix[,1])/(1:nrow(RRmatrix)),cumsum(RRmatrix[,2])/(1:nrow(RRmatrix))))
m.erg2=as.matrix(cbind(cumsum(RR2matrix[,1])/(1:nrow(RR2matrix)),cumsum(RR2matrix[,2])/(1:nrow(RR2matrix))))

#Gráficos para avaliação dos resultados a posteriori
ACFplot=0
RRm=as.matrix(RR)
nomes=c("Lambda","Beta")
for(i in 1:npar)
{
	if((i+3)%%4==0)
	{
		win.graph()
		par(mfrow=c(2,(4+ACFplot)))
	}
	plot(RRm[,i],type="l",ylab="",xlab="Iterations",main=paste("Trace of",nomes[i]),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
	acf(RRm[,i], main=paste("Autocorrelation of", nomes[i]),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
	densplot(RR[,i],main=paste("Density of",nomes[i]),cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
	plot.ts(m.erg1[,i],main="Ergodic Mean", xlab="Iterations", ylab=nomes[i],col="blue",ylim=c(min(m.erg1[,i],m.erg2[,i]),max(m.erg1[,i],m.erg2[,i])))
	lines(m.erg2[,i],col="red")
	#legend("topright",c("C1","C2"),lty=c(2,2),col=c("blue","red"),bty="n",cex=0.8)
}

# Inferências: ==========================

mu=RRm[,2]
igam=RRm[,1]

Iter=length(mu)
MTTF=numeric(Iter)
Median=numeric(Iter)
Perc0025=numeric(Iter)
Perc0975=numeric(Iter)
perc010=numeric(Iter)
med=numeric(Iter)
L=10
nt=8000 #nº de pontos varrendo T
Ft=matrix(0,nt,Iter)
tseq=seq(0,nt) #sequencia de pontos varrendo T

for(jj in 1:Iter)
{    
	mttfaux=0
	for(tt in 1:nt)
	{  
		mttfaux2=pgamma(L,igam[jj]*tseq[tt],mu[jj])
		mttfaux=mttfaux+mttfaux2
		Ft[tt,jj]=(1-pgamma(L,igam[jj]*tseq[tt],mu[jj])) # Ft uma unidade qualquer 
		#Ft[tt,jj]=(1-pgamma(L,igam[jj]*tseq[tt],mu[jj]))^15 # Ft todas as unidades conjuntamente
	}
	qqmed<-function(tt){pgamma(L,igam[jj]*tt,mu[jj])-1+0.5}    # mediana iteracao jj
	med[jj]=uniroot.all(qqmed,c(min(tseq),max(tseq)),maxiter = 100,n=100)
	qq010<-function(tt){pgamma(L,igam[jj]*tt,mu[jj])-1+0.1}      # percentil 10% iteracao jj
	perc010[jj]=uniroot.all(qq010,c(min(tseq),max(tseq)),maxiter = 100,n=100)
	MTTF[jj]=mttfaux     # Tempo médio até a falha (MTTF) iteracao jj
} 

meFt=apply(Ft, 1, mean)
Median=apply(Ft, 1, quantile, probs = c(0.5))
Perc0025=apply(Ft, 1, quantile, probs = c(0.025))
Perc0975=apply(Ft, 1, quantile, probs = c(0.975))

# Summaries: ==========================
summary(Median) # Mediana Ft
quantile(Median,c(0.025,0.975))

summary(meFt)  # Media Ft
quantile(meFt,c(0.025,0.975))

summary(MTTF)   # Media
quantile(MTTF,c(0.025,0.975))

summary(med)   # Perc 50%
quantile(med,c(0.025,0.975))

summary(perc010) # Perc 10%
quantile(perc010,c(0.025,0.975))

#R(t=4500): ===========================
# Funcao de confiabilidade:
#sobreviver > 4500 hours:
ttt=4500
tseq[ttt+1]
1-meFt[ttt+1]
1-Perc0025[ttt+1]
1-Median[ttt+1]
1-Perc0975[ttt+1]

# Graficos: ===========================
par(mfrow=c(1,3))
dadmc1med=mcmc(med, start = 1, end = Iter, thin = 1)
densplot(dadmc1med,main="",xlab="Median")
title("Median")
dadmc1perc01=mcmc(perc010, start = 1, end = Iter, thin = 1)
densplot(dadmc1perc01,main="",xlab="Percentile 0.10")
title("Percentile 0.10")
dadmc1MTTF=mcmc(MTTF, start = 1, end = Iter, thin = 1)
densplot(dadmc1MTTF,main="",xlab="MTTF")
title("MTTF")


#Descritivas:==========================
R4500=1-Ft[4501,]
Maux=as.data.frame(cbind(as.matrix(RR),MTTF,med,R4500))

HPD=NULL
for (i in 1:ncol(Maux))
{
	HPDaux=HPDinterval(mcmc(Maux[,i]),prob=0.90)
	HPDaux=t(sapply(HPDaux,'['))
	HPD=rbind(HPD,HPDaux)
}
colnames(HPD)=c("lowerHPD","upperHPD")

qlamb=round(quantile(Maux[,1],c(0.05,0.5,0.95)),digits=3)
qbeta=round(quantile(Maux[,2],c(0.05,0.5,0.95)),digits=3)
qmttf=round(quantile(Maux[,3],c(0.05,0.5,0.95)),digits=3)
q50=round(quantile(Maux[,4],c(0.05,0.5,0.95)),digits=3)
qR4500=round(quantile(Maux[,5],c(0.05,0.5,0.95)),digits=3)
media=round(sapply(Maux,mean),digits=3)
desviop=round(sapply(Maux,sd),digits=3)
Desc=cbind(media,desviop,rbind(qlamb,qbeta,qmttf,q50,qR4500),HPD)
Desc
par(mfrow=c(1,3))
hist(MTTF,xlab="Tempo (h)",main="MTTF",col="lightblue")
hist(med,xlab="Tempo (h)",main="Tempo mediano",col="lightblue")
hist(R4500,xlab="Probabilidade",main="Sobreviver a 4500h",col="lightblue")

###Dist. de Probabilidade e int. HPD##############
int.hpd=matrix(0,nrow(Ft),2)
for(i in 1:nrow(Ft))
{
	int.hpd[i,]=HPDinterval(mcmc(Ft[i,]),prob = c(0.95))
}

par(mfrow=c(2,1))
#F_T(t):===============================
plot(tseq[1:nt],meFt,type='l',xlab="Tempo (h)",ylab="F(t)",xlim=c(2000,8000),main="Função de Distribuição Acumulada")
par(new=T)
plot(tseq[1:nt],int.hpd[,1],type='l',xlab="Tempo (h)",ylab="F(t)",lty=c(2),xlim=c(2000,8000),col="orangered")
par(new=T)
plot(tseq[1:nt],int.hpd[,2],type='l',xlab="Tempo (h)",ylab="F(t)",lty=c(2),xlim=c(2000,8000),col="orangered")
#legend("bottomright",c("FDA","HPD"),lty=c(1,2),col=c("black","orangered"),bty="n",cex=0.8)

#R_T(t):===============================
plot(tseq[1:nt],1-meFt,type='l',xlab="Tempo (h)",ylab="R(t)",xlim=c(2000,8000),main="Função de Confiabilidade")
par(new=T)
plot(tseq[1:nt],1-int.hpd[,1],type='l',xlab="Tempo (h)",ylab="R(t)",lty=c(3),xlim=c(2000,8000),col="orangered")
par(new=T)
plot(tseq[1:nt],1-int.hpd[,2],type='l',xlab="Tempo (h)",ylab="R(t)",lty=c(3),xlim=c(2000,8000),col="orangered")
#legend("topright",c("FDP","HPD"),lty=c(1,2),col=c("black","orangered"),bty="n",cex=0.8)

