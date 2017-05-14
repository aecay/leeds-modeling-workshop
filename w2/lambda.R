lambda <- seq(600,0,by=-5)


BIC_vec<-rep(Inf,length(lambda))

# specify starting values for the very first fit; pay attention that Delta.start has suitable length!
Delta.start<-as.matrix(t(rep(0,583)))
Q.start<-diag(0.1, 2)

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))

  glm3 <- glmmLasso(log(rt) ~
                    scale(subtlex.frequency) +
                    scale(celex.frequency) +
                    scale(celex.frequency.lemma) +
                    scale(bnc.frequency),
                    rnd = list(participant = ~1, spelling = ~1),
                    data = subset(dat, !is.na(rt)),
                    lambda=lambda[j],
                    switch.NR=FALSE,
                    final.re=TRUE,
                    control = glmmLassoControl(start=Delta.start[j,],q_start=Q.start))

  print(colnames(glm3$Deltamatrix)[2:5][glm3$Deltamatrix[glm3$conv.step,2:5]!=0])
  BIC_vec[j]<-glm3$bic
  Delta.start<-rbind(Delta.start,glm3$Deltamatrix[glm3$conv.step,])
  Q.start<-glm3$Q_long[[glm3$conv.step+1]]
}

## opt3<-which.min(BIC_vec)
