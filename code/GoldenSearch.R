goldenSearch<-function(a=0,b=1,y=NA,N=NA,tol=0.001){

    #Define golden ratio
    phi<-(1+sqrt(5))/2
    err<-b-a
    #Compute values of of x1 and x2
    x1<-b-(b-a)/phi
    x2<-a+(b-a)/phi   
    #Test of error > tolerance if so
    #Enter while loop
    while(err > tol){
        #Compute -log likelihoods
        lx1<- -log(dbinom(y,size=N,x1))
        lx2<- -log(dbinom(y,size=N,x2))
        if(lx1 > lx2){ ## min to right of x1
            a<-x1
            x1<-b-(b-a)/phi
            x2<-a+(b-a)/phi              
        } else{ #Min to left
            b<-x2
            x1<-b-(b-a)/phi
            x2<-a+(b-a)/phi 
        }
        #New error
        err<-b-a
    }    
    #Return lower and upper bounds and mean
    return(c(a,b,(a+b)/2))    
}    
#Read data table
#One row per data set, count of Y successes out of N
#True prob. of success in last column
cdat<-read.table("countData.txt",header=TRUE)
head(cdat)

#Now let's estimate p from Y and N, use the golden section search
K<-dim(cdat)[1]
Pest<-matrix(NA,nrow=K,ncol=3)
for(i in 1:K){
    Pest[i,]<-goldenSearch(a=0,b=1,y=cdat$Y[i],N=cdat$N[i],tol=0.001)
    }

head(Pest)    
plot(Pest[,3],cdat[,3],pch=19,xlab="Estimate of P",ylab="True value of P")
abline(a=0,b=1) ## 1:1 line

#Correlation between estimate and true value
cor(Pest[,3],cdat[,3])
#Correlation between estimate from golden search and analytical est.
cor(Pest[,3],cdat[,1]/cdat[,2])
