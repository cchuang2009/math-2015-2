tConfInterval <- function (xmean,df,sd,type,alpha ){
    n<-df+1
    if (type=="upper"){ 
        xa<-xmean+qt(1-alpha,df=df)*sd/sqrt(n)
        cat(sprintf("  Upper one-sided (%d)%% confidence interval of μ is (-oo,%.3f]",100*(1-alpha),xa))              
    } else {
        if (type=="lower") {
           xa<-xmean+qt(alpha,df=df)*sd/sqrt(n)
           cat(sprintf("  Lower one-sided (%d)%% confidence interval of μ is [%.3f,oo)",100*(1-alpha),xa))
        } else {
             xaR<-xmean+qt(1-alpha/2,df=df)*sd/sqrt(n)
             xaL<-xmean+qt(alpha/2,df=df)*sd/sqrt(n)
             cat(sprintf("  Two-sided (%d)%% confidence interval of μ is [%.3f,%.3f]",100*(1-alpha),xaL,xaR))             
        }
    }   
  }