

n.loci<-1000
epsilons<-rbeta(n.loci,1,1)

freq.1<-rnorm(n.loci,mean=epsilons,sd=sqrt(0.01*epsilons*(1-epsilons)))
freq.2<-rnorm(n.loci,mean=epsilons,sd=sqrt(0.01*epsilons*(1-epsilons)))

effect.sizes<-rnorm(n.loci,sd=0.01)

beta<-10

freq.2.sel<-freq.2 + beta*effect.sizes*freq.2*(1-freq.2)
freq.1.sel<-freq.1 + beta*effect.sizes*freq.1*(1-freq.1)
plot(freq.1.sel-epsilons,freq.2.sel-epsilons,col=ifelse(effect.sizes>0,"red","blue"))
cor.test(freq.1.sel-epsilons,freq.2.sel-epsilons)
signed.change.1<-sign(effect.sizes)*(freq.1.sel-epsilons)
signed.change.2<-sign(effect.sizes)*(freq.2.sel-epsilons)

num.up.1<-sum(signed.change.1>0)
num.up.2<-sum(signed.change.2>0)

chisq.test(x=c(num.up.1,n.loci-num.up.1),p=c(0.5,0.5))
chisq.test(x=c(num.up.2,n.loci-num.up.2),p=c(0.5,0.5))

up.up<-sum(signed.change.1 > 0 & signed.change.2 >0)
up.down<-sum(signed.change.1 > 0 & signed.change.2 <=0)
down.up<-sum(signed.change.1 <= 0 & signed.change.2 >0)
down.down<-sum(signed.change.1 <= 0 & signed.change.2 <=0)

sign.matrix<-matrix(c(c(up.up,up.down),c(down.up,down.down)),byrow=TRUE,nrow=2)
chisq.test(sign.matrix)

 chisq.test(matrix(c(c(num.up.1,n.loci-num.up.1),c(num.up.2,n.loci-num.up.2)),byrow=TRUE,nrow=2))

mantelhaen.test()