twoConfInterval <- function (xmean,n1,n2,sd1,sd2,type,alpha ){
    sd<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
    df<-n1+n2-2
    if (type=="upper"){ 
        xa<-xmean+qt(1-alpha,df=df)*sd*sqrt(1/n1+1/n2)
        cat(sprintf("  Upper one-sided (%d)%% confidence interval of μ is (-oo,%.3f]",100*(1-alpha),xa))              
    } else {
        if (type=="lower") {
           xa<-xmean+qt(alpha,df=df)*sd*sqrt(1/n1+1/n2)
           cat(sprintf("  Lower one-sided (%d)%% confidence interval of μ is [%.3f,oo)",100*(1-alpha),xa))
        } else {
             xaR<-xmean+qt(1-alpha/2,df=df)*sd*sqrt(1/n1+1/n2)
             xaL<-xmean+qt(alpha/2,df=df)*sd*sqrt(1/n1+1/n2)
             cat(sprintf("  Two-sided (%d)%% confidence interval of μ is [%.3f,%.3f]",100*(1-alpha),xaL,xaR))             
        }
    }   
  }