zConfInterval <- function (xmean,n,sd,type,alpha ){
    if (type=="upper"){ 
        xa<-xmean+qnorm(1-alpha)*sd/sqrt(n)
        cat(sprintf("  Upper one-sided (%d)%% confidence interval of μ is (-oo,%.3f]",100*(1-alpha),xa))              
    } else {
        if (type=="lower") {
           xa<-xmean+qnorm(alpha)*sd/sqrt(n)
           cat(sprintf("  Lower one-sided (%d)%% confidence interval of μ is [%.3f,oo)",100*(1-alpha),xa))
        } else {
             xaR<-xmean+qnorm(1-alpha/2)*sd/sqrt(n)
             xaL<-xmean+qnorm(alpha/2)*sd/sqrt(n)
             cat(sprintf("  Two-sided (%d)%% confidence interval of μ is [%.3f,%.3f]",100*(1-alpha),xaL,xaR))             
        }
    }   
  }