ztest <- function (x,n,mean,sd,type,alpha ){
    za<-qnorm(alpha)
    zx<-(x-mean)/(sd/sqrt(n))
    cat(sprintf("        Testing Hypothesis\n"))
    cat(sprintf("---------------------------------------\n"))
    if (type=="greater"){ 
        xa<-mean+qnorm(alpha)*sd/sqrt(n)
        pval<-pnorm(x,mean=mean,sd=sd/sqrt(n))
        cat(sprintf("  H0: μ ≥ %.3f v.s. Ha: μ < %.3f\n", mean,mean))
        cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
        cat(sprintf("  z_α: %.2f, μ+z_α*σ/√n : %.2f\n ",qnorm(alpha),xa))
        if (x<xa){            
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----|------[-------o-------------------\n"))  
                cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("x:  %.2f   %.2f\n",x,xa))
                cat(sprintf("Z: %.2f  %.2f\n",zx,za))
        }else{
                cat(sprintf("\n       do not Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("-----[------|-------o-------------------\n"))  
                cat(sprintf("                 μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("x:  %.2f  %.2f\n",xa,x))
                cat(sprintf("Z: %.2f  %.2f\n",za,zx))
         }   
    } else {
        if (type=="less") {
             xa<-mean+qnorm(1-alpha)*sd/sqrt(n)
             pval<-pnorm(x,mean=mean,sd=sd/sqrt(n),lower.tail=F)
             cat(sprintf("  H0: μ ≤ %.3f v.s. Ha: μ > %.3f\n", mean,mean))
             cat(sprintf("  α level: %.2f, p-value: %.2f\n", alpha,pval)   )
             cat(sprintf("  z_(1-α): %.2f, μ+z_(1-α)*σ/√n : %.2f\n ",qnorm(1-alpha),xa))
             if (x>xa){
                cat(sprintf("\n              Reject H0\n"))
                cat(sprintf("                       α = %.3f p = %.3f\n",alpha,pval))
                cat(sprintf("------------------o------]--------|---\n"))  
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("x:                     %.2f     %.2f\n",xa,x))
                cat(sprintf("Z:                     %.2f      %.2f\n",-za,zx)) 
             }else{
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                cat(sprintf("------------------o------|--------]---\n"))  
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                cat(sprintf("x:                     %.2f     %.2f\n",x,xa))
                cat(sprintf("Z:                     %.2f      %.2f\n",zx,-za))
             } 
        } else {
             xaR<-mean+qnorm(1-alpha/2)*sd/sqrt(n)
             xaL<-mean+qnorm(alpha/2)*sd/sqrt(n)
             za<-qnorm(alpha/2)
             pval<-2*pnorm(x,mean=mean,sd=sd/sqrt(n),lower.tail=F)
             cat(sprintf("  H0: μ = %.3f v.s. Ha: μ ‡ %.3f\n", mean,mean))
             cat(sprintf("  α level: %.3f, p-value: %.4f\n", alpha,pval)   )
             cat(sprintf("  z_(α/2): %.3f, z_(1-α/2): %.3f,\n", qnorm(alpha/2),-qnorm(alpha/2)))
             cat(sprintf("  μ+z_(α/2)*σ/√n : %.3f, ",xaL))
             cat(sprintf("  μ+z_(1-α/2)*σ/√n : %.3f\n ",xaR))
             if (x>xaL && x<xaR){
                cat(sprintf("\n              Do not Reject H0\n"))
                cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                cat(sprintf("               μ0 = %.2f\n",mean))
                cat(sprintf("\n"))
                if(x>mean){
                    cat(sprintf("---------[---------o------|--------]---\n")) 
                    cat(sprintf("x:      %.2f             %.2f    %.2f\n",xaL,x,xaR))
                    cat(sprintf("Z:     %.2f             %.2f     %.2f\n",za,zx,-za))
                } else{
                    cat(sprintf("---------[------|---o-------------]---\n"))
                    cat(sprintf("x:      %.2f   %.2f             %.2f\n",xaL,x,xaR))
                    cat(sprintf("Z:     %.2f  %.2f             %.2f\n",za,zx,-za))
                }
             }else{
                if(x<xaL){ 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   cat(sprintf("                  μ0 = %.2f\n",mean)) 
                   cat(sprintf("-------|----[--------o--------]------\n"))
                   cat(sprintf("x:  %.2f   %.2f             %.2f\n",x,xaL,xaR))
                   cat(sprintf("Z: %.2f  %.2f             %.2f\n",zx,za,-za))                   
                }else{
                   #cat(sprintf(" %.3f <%.3f: reject H0",xaR,x)) 
                   cat(sprintf("\n              Reject H0\n"))
                   cat(sprintf("                       p = %.3f α = %.3f\n",pval,alpha))
                   cat(sprintf("              μ0 = %.2f\n",mean))
                   cat(sprintf("-------[--------o--------]-----|---\n"))
                   cat(sprintf("x:   %.2f              %.2f  %.2f\n",xaL,xaR,x))
                   cat(sprintf("Z:  %.2f               %.2f  %.2f\n",za,-za,zx))
                }    
             } 
        }
    }   
  }