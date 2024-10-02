#Function for seed dispersal example
SeedDispersal<-function(N0,xsize=c(-100,100),ysize=c(-100,100),time=100,lambda=10,p=0.15,sd=1){
    
    #Set initial x and y location for N0 plants
    x<-runif(N0,xsize[1],xsize[2])
    y<-runif(N0,ysize[1],ysize[2])
    
    #Plot initial population
    plot(x,y,pch=19,xlim=xsize,ylim=ysize,xlab="Spatial X",ylab="Spatial Y")
    
    #Loop over time
    for(i in 2:time){
        Nplants<-length(x)
        
        if (Nplants == 0) { 
            #Stop if no plants are left 
            cat("All plants are extinct at time step", i, "\n") 
            break 
        }
        
        xnew<-NULL
        ynew<-NULL
        #Loop over plants for random survival
        if(Nplants >= 1){
            for(j in 1:Nplants){
                if(runif(1,0,1) < p){ #Plant reproduces
                    Nseeds<-rpois(1,lambda) #Generate number of seeds per offspring
                    if(Nseeds > 0){
                        #Sample x and y coordinates
                        xtemp<-rnorm(Nseeds,mean=x[j],sd=sd)
                        ytemp<-rnorm(Nseeds,mean=y[j],sd=sd)
                        #Make sure they didn't disperse off the space
                        keep<-(xtemp > xsize[1]) & (xtemp < xsize[2]) & (ytemp > ysize[1]) & (ytemp < ysize[2])
                        xnew<-c(xnew,xtemp[keep])
                        ynew<-c(ynew,ytemp[keep])
                    }
                }
            }
            #Set x and y for new population
            x<-xnew
            y<-ynew
            plot(x,y,pch=19,xlim=xsize,ylim=ysize,xlab="Spatial X",ylab="Spatial Y",col = "red")
        }
    }
    out<-list(N=length(x),x=x,y=y)
    return(out)
} 



