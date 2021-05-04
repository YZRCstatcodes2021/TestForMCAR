library(R.utils); library(kSamples)

source("/Storey_2004.R")

MCAR.test = function(data, alpha=0.05, threshold.cat=12, warning=T){

    if(!is.data.frame(data)){
        mcar.data = data.frame(data)
    } else{mcar.data = data}
    if(all(complete.cases(test.data))){
        stop("No missing data exists\n")
    }
    rownames(mcar.data) = 1:nrow(mcar.data)
    n.var = ncol(mcar.data)
    pvalue.list = pvalue.list2 = numeric(n.var)
    var.type = numeric(n.var)
      ind.mis <- 1 * is.na(mcar.data)
      mdp <- (ind.mis %*% (2^((1:n.var - 1)))) + 1
      for (p in 1:n.var){
        if( length(unique(mcar.data[,p]))>threshold.cat ){
        index <- (ind.mis[,p]!=1)
        y.l <- mcar.data[index,p]
        mdp.l <- mdp[index,]
        if( sum(unique(mdp.l)) == 1){stop("Not enough missing data patterns for using the tests\n")}
        pvalue.list[p]  = anova(aov(y.l~factor(mdp.l)))$"Pr(>F)"[1]
        pvalue.list2[p] = ad.test(y.l~factor(mdp.l),method="exact",Nsim=8000)$ad[1,4]
        var.type[p] = 1
        }
        if( length(unique(mcar.data[,p]))<=threshold.cat ){
        R.table = NULL
        index <- (ind.mis[,p]!=1)
        y.l <- mcar.data[index,p]
        mdp.l <- mdp[index,]
        tab.pat = table(mdp.l)
      for( pp in 1 : length(tab.pat) ){
        Object = which( mdp.l == names(tab.pat)[pp] )
        numb.in.data = y.l[Object]
        R.small.table  = table(numb.in.data)
        # Check if the table has all categories, otherwise error when combining
        if( length(R.small.table) != length(unique(na.omit(mcar.data[,p]))) ){
        add.0 = which(!sort( unique(na.omit(mcar.data[,p]))) %in% as.numeric(names(R.small.table)) )
        for(a in 1:length(add.0)) { R.small.table  = insert( as.numeric(R.small.table), ats=add.0[a],value=0 )}
      }
        if(!all(R.small.table==0)){
        R.table = rbind(R.table,R.small.table)}
    }

      ifelse(warning==T,withCallingHandlers(chisq.test(R.table),warning = function(w){warn<<-TRUE;invokeRestart("muffleWarning") }),warn<-F)
         if(warn==T){
         pvalue.list[p] = chisq.test(R.table,simulate.p.value=T,B=1000)$p.value
         } else{
         pvalue.list[p] = chisq.test(R.table)$p.value
         }
         pvalue.list2[p] = pvalue.list[p]
         var.type[p] = 2
      } #end of "if"
     }

     if( any(is.na(pvalue.list)) ){
     pvalue.missing.var = which(is.na(pvalue.list))
     pvalue.list = na.exclude(pvalue.list)
     pvalue.list2 = na.exclude(pvalue.list2)
    }

    pvalue.anova.BH = p.adjust(pvalue.list,method = "BH")
    pvalue.AD.BH = p.adjust(pvalue.list2,method = "BH")
    anova.Storey_alpha = adaptive.Storey.2004(pvalue.list,lambda=alpha,automatic.lambda=FALSE)
    AD.Storey_alpha = adaptive.Storey.2004(pvalue.list2,lambda=alpha,automatic.lambda=FALSE)
    anova.Storey_bootstrap = adaptive.Storey.2004(pvalue.list,automatic.lambda=TRUE)
    AD.Storey_bootstrap = adaptive.Storey.2004(pvalue.list2,automatic.lambda=TRUE)
    pvalue.anova.Storey_alpha = anova.Storey_alpha$adaptive.pvalues
    pvalue.AD.Storey_alpha = AD.Storey_alpha$adaptive.pvalues
    pvalue.anova.Storey_bootstrap = anova.Storey_bootstrap$adaptive.pvalues
    pvalue.AD.Storey_bootstrap = AD.Storey_bootstrap$adaptive.pvalues


    result.anova.BH = ifelse( any( pvalue.anova.BH < alpha), "Reject Null","Do not reject null")
    result.anova.Storey_alpha = ifelse( sum(anova.Storey_alpha$pvalues.reject) != 0 , "Reject null", "Do not reject null")
    result.anova.Storey_bootstrap = ifelse( sum(anova.Storey_bootstrap$pvalues.reject) != 0 , "Reject null", "Do not reject null")
    result.AD.BH = ifelse( any( pvalue.AD.BH < alpha), "Reject Null","Do not reject null")
    result.AD.Storey_alpha = ifelse( sum(AD.Storey_alpha$pvalues.reject) != 0 , "Reject null", "Do not reject null")
    result.AD.Storey_bootstrap = ifelse( sum(AD.Storey_bootstrap$pvalues.reject) != 0 , "Reject null", "Do not reject null")
    return(result=list(Var.type=var.type, anova.BH=result.anova.BH, anova.Storey_alpha=result.anova.Storey_alpha, anova.Storey_bootstrap=result.anova.Storey_bootstrap,
                       AD.BH=result.AD.BH, AD.Storey_alpha = result.AD.Storey_alpha, AD.Storey_bootstrap = result.AD.Storey_bootstrap,
                       anova_unadjusted.pvalue=pvalue.list, AD_unadjusted.pvalue=pvalue.list2,
                       anova.BH.pvalue= pvalue.anova.BH, anova.Storey_alpha.pvalue=pvalue.anova.Storey_alpha,anova.Storey_bootstrap.pvalue=pvalue.anova.Storey_bootstrap,
                       AD.BH.pvalue= pvalue.AD.BH, AD.Storey_alpha.pvalue= pvalue.AD.Storey_alpha, AD.Storey_bootstrap.pvalue= pvalue.AD.Storey_bootstrap))
}


test.data = iris
test.data[sample(1:150,10),1] = NA
test.data[sample(1:150,10),3] = NA
test.data[sample(1:150,5),5] = NA

MCAR.test(test.data)

test.data2 = mtcars
test.data2[sample(1:32,10),1] = NA
test.data2[sample(1:32,10),3] = NA
test.data2[sample(1:32,10),5] = NA
test.data2[sample(1:32,10),8] = NA
test.data2[sample(1:32,10),10] = NA

MCAR.test(test.data2)

###############################################################################
###############################################################################
###############################################################################



