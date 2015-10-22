twoSampletest <- function (x,n1,n2,sd1,sd2,type,alpha ){
    df<-n1+n2-2
    n<-df+1
    mean<-0
    sd<-((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
    ta<-qt(alpha,df=df)
    tx<-(x-mean)/(sd*sqrt(1/n1+1/n2))
    cat(sprintf("        Testing Hypothesis\n"))
    cat(sprintf("---------------------------------------\n"))
    if (type=="greater"){ 
        xa<-mean+qt(alpha,df=df)*sd*sqrt(1/n1+1/n2)
        pval<-pt(x,df=df)
        cat(sprintf("  H0: μ2 ≥ μ1 v.s. Ha: μ2 < μ1\n"))
        cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
        cat(sprintf("  t_(%d,α): %.2f, t_(%d,α)*s/√(1/n1+1/n2) : %.2f\n ",df,qt(alpha,df=df),df,xa))
        if (x<xa){            
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----|------[-------o-------------------\n"))  
                #cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:  %.2f   %.2f\n",x,xa))
                cat(sprintf("T:  %.2f  %.2f\n",tx,ta))
        }else{
                cat(sprintf("\n       do not Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----[------|-------o-------------------\n"))  
                #cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:  %.2f  %.2f\n",xa,x))
                cat(sprintf("T:  %.2f  %.2f\n",ta,tx))
         }
         cat(sprintf("\n(1-%0.3f) - confident interval is (-oo,%.3f+(%.3f)]=(-oo,%.3f]\n",alpha,x,xa,x+xa))
    } else {
        if (type=="less") {
             xa<-qt(1-alpha,df=df)*sd*sqrt(1/n1+1/n2)
             pval<-pt(tx,df=df,lower.tail=F)
             cat(sprintf("  H0: μ2 ≤ μ1 v.s. Ha: μ2 > μ1\n"))
             cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
             cat(sprintf("  t_(%d,1-α): %.2f, t_(%d,1-α)*s/√(1/n1+1/n2): %.2f\n ",df,qt(1-alpha,df=df),df,xa))
             if (x>xa){
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("------------------o------]--------|---\n"))  
                #cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:                     %.2f     %.2f\n",xa,x))
                cat(sprintf("T:                     %.2f     %.2f\n",-ta,tx)) 
             }else{
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                cat(sprintf("------------------o------|--------]---\n"))  
                #cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("X:                     %.2f     %.2f\n",x,xa))
                cat(sprintf("T:                     %.2f     %.2f\n",tx,-ta))
             } 
             cat(sprintf("\n(1-%0.3f) - confidence interval is [%.3f+(%.3f),oo)=[%.3f,oo)\n",alpha,x,xa,x+xa))
        } else {
             xaR<-qt(1-alpha/2,df=df)*sd*sqrt(1/n1+1/n2)
             xaL<-qt(alpha/2,df=df)*sd*sqrt(1/n1+1/n2)
             ta<-qt(alpha/2,df=df)
             pval<-2*pt(abs(tx),df=df,lower.tail=F)
             cat(sprintf("  H0: μ1 = μ2 v.s. Ha: μ1 ‡ μ2\n"))
             cat(sprintf("  α level: %.3f, p-value: %.4f\n", alpha,pval)   )
             cat(sprintf("  t_(%d,α/2): %.3f, t_(%d,1-α/2): %.3f,\n", df,
                            qt(alpha/2,df=df),df,-qt(alpha/2,df=df)))
             cat(sprintf("  t_(%d,α/2)*s*√(1/n1+1/n2) : %.3f, ",df,xaL))
             cat(sprintf("  t_(%d,1-α/2)*s*√(1/n1+1/n2) : %.3f\n ",df,xaR))
             if (x>xaL && x<xaR){
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                #cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                if(x>mean){
                    cat(sprintf("---------[---------o------|--------]---\n")) 
                    cat(sprintf("X:      %.2f             %.2f    %.2f\n",xaL,x,xaR))
                    cat(sprintf("T:      %.2f             %.2f    %.2f\n",ta,tx,-ta))
                } else{
                    cat(sprintf("---------[------|---o-------------]---\n"))
                    cat(sprintf("X:      %.2f   %.2f             %.2f\n",xaL,x,xaR))
                    cat(sprintf("T:      %.2f   %.2f             %.2f\n",ta,tx,-ta))
                }
                cat(sprintf("\n##(1-%0.3f) - confidence interval is##\n
                   [%.3f+(%.3f),%.3f+(%.3f)] = [%.3f,%.3f]\n",alpha,x,xaL,x,xaR,x+xaL,x+xaR))
             }else{
                if(x<xaL){ 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   #cat(sprintf("                  μ0 = %.2f\n",mean)) 
                   cat(sprintf("-------|----[--------o--------]------\n"))
                   cat(sprintf("X:  %.2f   %.2f             %.2f\n",x,xaL,xaR))
                   cat(sprintf("T:  %.2f  %.2f              %.2f\n",tx,ta,-ta)) 
                }else{
                   #cat(sprintf(" %.3f <%.3f: reject H0",xaR,x)) 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   cat(sprintf("              μ1 = μ2\n"))
                   cat(sprintf("-------[--------o--------]-----|---\n"))
                   cat(sprintf("X:   %.2f              %.2f  %.2f\n",xaL,xaR,x))
                   cat(sprintf("T:   %.2f              %.2f  %.2f\n",ta,-ta,tx))
                }
                cat(sprintf("\n##(1-%0.3f) - confidence interval is##\n
                   [%.3f+(%.3f),%.3f+(%.3f)] = [%.3f,%.3f]\n",alpha,x,xaL,x,xaR,x+xaL,x+xaR))
             } 
        }
    }   
  }