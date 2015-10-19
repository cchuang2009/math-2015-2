ttest <- function (x,df,mean,sd,type,alpha ){
    n<-df+1
    ta<-qt(alpha,df=df)
    tx<-(x-mean)/(sd/sqrt(n))
    cat(sprintf("        Testing Hypothesis\n"))
    cat(sprintf("---------------------------------------\n"))
    if (type=="greater"){ 
        xa<-mean+qt(alpha,df=df)*sd/sqrt(n)
        pval<-pt(x,mean=mean,sd=sd,df=df)
        cat(sprintf("  H0: μ ≥ %.3f v.s. Ha: μ < %.3f\n", mean,mean))
        cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
        cat(sprintf("  t_(%d,α): %.2f, μ+t_(%d,α)*s/√n : %.2f\n ",df,qt(alpha,df=df),df,xa))
        if (x<xa){            
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----|------[-------o-------------------\n"))  
                cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:  %.2f   %.2f\n",x,xa))
                cat(sprintf("T: %.2f  %.2f\n",tx,ta))
        }else{
                cat(sprintf("\n       do not Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----[------|-------o-------------------\n"))  
                cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:  %.2f  %.2f\n",xa,x))
                cat(sprintf("T: %.2f  %.2f\n",ta,tx))
         }   
    } else {
        if (type=="less") {
             xa<-mean+qt(1-alpha,df=df)*sd/sqrt(n)
             pval<-pt(tx,df=df,sd=sd,lower.tail=F)
             cat(sprintf("  H0: μ ≤ %.3f v.s. Ha: μ > %.3f\n", mean,mean))
             cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
             cat(sprintf("  t_(%d,1-α): %.2f, μ+t_(%d,1-α)*s/√n : %.2f\n ",df,qt(1-alpha,df=df),df,xa))
             if (x>xa){
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("------------------o------]--------|---\n"))  
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:                     %.2f     %.2f\n",xa,x))
                cat(sprintf("T:                     %.2f      %.2f\n",-ta,tx)) 
             }else{
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                cat(sprintf("------------------o------|--------]---\n"))  
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:                     %.2f     %.2f\n",x,xa))
                cat(sprintf("T:                     %.2f      %.2f\n",tx,-ta))
             } 
        } else {
             xaR<-mean+qt(1-alpha/2,df=df)*sd/sqrt(n)
             xaL<-mean+qt(alpha/2,df=df)*sd/sqrt(n)
             ta<-qt(alpha/2,df=df)
             pval<-2*pt(tx,df=df,lower.tail=F)
             cat(sprintf("  H0: μ = %.3f v.s. Ha: μ ‡ %.3f\n", mean,mean))
             cat(sprintf("  α level: %.3f, p-value: %.4f\n", alpha,pval)   )
             cat(sprintf("  t_(%d,α/2): %.3f, t_(%d,1-α/2): %.3f,\n", df,
                            qt(alpha/2,df=df),df,-qt(alpha/2,df=df)))
             cat(sprintf("  μ+t_(%d,α/2)*s/√n : %.3f, ",df,xaL))
             cat(sprintf("  μ+t_(%d,1-α/2)*s/√n : %.3f\n ",df,xaR))
             if (x>xaL && x<xaR){
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                if(x>mean){
                    cat(sprintf("---------[---------o------|--------]---\n")) 
                    cat(sprintf("X:      %.2f             %.2f    %.2f\n",xaL,x,xaR))
                    cat(sprintf("T:     %.2f             %.2f     %.2f\n",ta,tx,-ta))
                } else{
                    cat(sprintf("---------[------|---o-------------]---\n"))
                    cat(sprintf("X:      %.2f   %.2f             %.2f\n",xaL,x,xaR))
                    cat(sprintf("T:     %.2f  %.2f             %.2f\n",ta,tx,-ta))
                }
             }else{
                if(x<xaL){ 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   cat(sprintf("                  μ0 = %.2f\n",mean)) 
                   cat(sprintf("-------|----[--------o--------]------\n"))
                   cat(sprintf("X:  %.2f   %.2f             %.2f\n",x,xaL,xaR))
                   cat(sprintf("T: %.2f  %.2f             %.2f\n",tx,ta,-ta))                   
                }else{
                   #cat(sprintf(" %.3f <%.3f: reject H0",xaR,x)) 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   cat(sprintf("              μ0 = %.2f\n",mean))
                   cat(sprintf("-------[--------o--------]-----|---\n"))
                   cat(sprintf("X:   %.2f              %.2f  %.2f\n",xaL,xaR,x))
                   cat(sprintf("T:  %.2f               %.2f  %.2f\n",ta,-ta,tx))
                }    
             } 
        }
    }   
  }