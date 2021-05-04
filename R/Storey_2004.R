m0.est = function(pvalues,lambda)
{
    R.lambda = sapply( lambda, function(i) sum(pvalues <= i) )
    m0.est = (length(pvalues) - R.lambda + 1) / (1-lambda)
    return(m0.est)
}

adaptive.Storey.2004 = function(pvalues, alpha=0.05, lambda=0.5, automatic.lambda=TRUE, B=1000)
{

    if(!is.numeric(pvalues)){
        stop("P.values need to be numeric\n")
    }
    if(!is.numeric(alpha)){
        stop("alpha needs to be numeric\n")
    }
    if(!is.numeric(lambda)){
        stop("Lambda needs to be numeric\n")
    }
    if( B%%1!=0 | B<0){
        stop("B needs to be an nonnegative integer\n")
    }
    if( any(is.na(pvalues)) | is.null(pvalues) ){ 
        stop("NA exists in p-values\n")  
    }
    if( alpha>=1 | alpha <=0 ){
        stop("Type one error alpha should between 0 and 1")
    }
    if( lambda>1 | lambda <0 ){
        stop("Tunning parameter lambda should between 0 and 1")
    }

    m = length(pvalues) 
 
    # Bootstrap lambda
    if(automatic.lambda == TRUE){
       lambda = seq(0,0.95,by=0.05)
       # m0 true value estimates 
       m0.true.est = min(m0.est(pvalues,lambda))
       # simulate Bootstrap samples
       pvalue.boot = matrix( sample(pvalues,size=B*m,replace=T), nrow=B, ncol=m)
       # for each lambda, compute m0.est using bootstrap samples 
       m0.boot = apply(pvalue.boot, 1, function(i) m0.est(i,lambda) )
       # compute MSE for 
       lambda.mse = apply(m0.boot, 1, function(i) sum((i-m0.true.est)^2)/ (m*B)  ) 
       # lambda = arg min MSE(lambda)
       lambda = lambda[lambda.mse==min(lambda.mse)]
    }

    # Fixed lambda
    m0.hat = m0.est(pvalues,lambda)
    adaptive.pvalues = p.adjust(pvalues,method="BH") * min( m0.hat/m , 1)
    pvalues.reject = (adaptive.pvalues <= alpha & adaptive.pvalues<=lambda)
    return( list(adaptive.pvalues = adaptive.pvalues, pvalues.reject = pvalues.reject,
                alpha=alpha, lambda=lambda, m0.hat=m0.hat)
    )
}



