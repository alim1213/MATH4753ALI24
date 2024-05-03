#' Make Barplots for random samples
#'
#' Make random samples of size n between 1:10 with replacement, and make factor based on frequency of each number in the sample. Then, make barplot based on this factor.
#' @param n Sample size
#' @param iter Number of iterations, which is 5 by default
#'
#' @return Display barplots of created factor based on sample
#' @export
#'
#' @examples
#' randsample(20, iter = 3)
#' randsample(10)
randsample=function(n, iter=5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

  }
}
