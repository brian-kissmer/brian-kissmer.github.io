rm(list=ls())

#Replace $PATH with the path that this file downloaded to, similar to if you were using read.table()
source("$PATH/working_PP2_functions2.R")

#Get function names
fsn<-objects()
N<-length(fsn)
fsL<-vector("list",N)

#Read functions into list object
for(i in 1:N){
        fsL[[i]]<-eval(parse(text=fsn[i]))
}

#Create all of the combos
Nc<-(N * (N-1))/2
Cmat<-matrix(NA,nrow=Nc,ncol=2)
k<-1
for(i in 1:(N-1)){
    for(j in (i+1):N){
        Cmat[k,]<-c(i,j)
        k<-k+1
    }    
}    

playGame<-function(f1=NA,f2=NA){
    V<-1;C<--4;S<--5;R<-2 #Assign fitness values
    ng<-sample(15:40,1,replace=FALSE)
    m1<-NULL
    m2<-NULL
    sc1<-rep(NA,ng)
    sc2<-rep(NA,ng)
    for(i in 1:ng){
        m1[i]<-f1(i,m1,m2)
        m2[i]<-f2(i,m2,m1)
        if((m1[i] == 1) & (m2[i] == 1)){ #Both cooperate
            sc1[i]<-V
            sc2[i]<-V
        } else if ((m1[i] == 0) & (m2[i] == 0)){ #Both defect
            sc1[i]<-C
            sc2[i]<-C
        } else if ((m1[i] == 1) & (m2[i] == 0)){ #1 cooperate 2 defect
            sc1[i]<-S
            sc2[i]<-R
        } else { #2 coop, 1 defect
            sc1[i]<-R
            sc2[i]<-S
        }    
    }
    #Return means, controls for number of games
    return(c(mean(sc1),mean(sc2)))
    
}    

Scores<-matrix(NA,nrow=N,ncol=N)

for(i in 1:Nc){
    a<-Cmat[i,1];b<-Cmat[i,2]
    sc<-playGame(fsL[[a]],fsL[[b]])
    Scores[a,b]<-sc[1]
    Scores[b,a]<-sc[2]    
  }  
    ## 1, 15

par(mar=c(7,4,1,1))    
mnsc<-apply(Scores,1,mean,na.rm=TRUE)
relsc<-mnsc-min(mnsc)
relsc<-relsc/max(relsc)
plot(relsc,pch=19,axes=FALSE,xlab="",ylab="Mean fitness value")
axis(1,at=c(1:N),fsn,las=2) 
axis(2)
box()   

#Sort highest to lowest, 5 points for top 3, 3 points for next 3
rev(fsn[order(apply(Scores,1,mean,na.rm=TRUE))])
