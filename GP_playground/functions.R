mycols <- c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")
lightblue <- "#729FCF4D"
darkblue <- "#204A87FF"
darkbluetr <- "#204A874D"

get_kernel <- function(name_str){
  if(name_str=="kExp")   kernel <- kExp
  if(name_str=="kMat32") kernel <- kMat32
  if(name_str=="kMat52") kernel <- kMat52
  if(name_str=="kGauss") kernel <- kGauss
  if(name_str=="kBrown") kernel <- kBrown  
  return(kernel)
}

nearPoints <- function(X,Y,x,y){
  n <- length(X)
  dist2 <- (X-rep(x,n))^2 + (Y-rep(y,n))^2
  m <- min(dist2)
  if(m < 0.008){
    return(which(dist2 == m))
  }else{
    return(c())
  }
}

dist <- function(x,y,theta){
  dist2 <- matrix(0,dim(x)[1],dim(y)[1])
  for(i in 1:dim(x)[2]){
    dist2 <- dist2 + (outer(x[,i],y[,i],"-")/theta[i])^2
  }
  return(sqrt(dist2))
}

kBrown <- function(x,y,param=NULL){
  if(is.null(param)) param <-1
  param[1]*outer(c(x),c(y),"pmin")
}

kExp <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  param[1]*exp(-dist(x,y,param[-1]))
}

kGauss <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  param[1]*exp(-.5*dist(x,y,param[-1])^2)
}

kMat32 <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  d <- sqrt(3)*dist(x,y,param[-1])
  return(param[1]*(1 + d)*exp(-d))
}

kMat52 <- function(x,y,param=NULL){
  if(is.null(param)) param <- c(1,rep(.2,ncol(x)))
  d <- sqrt(5)*dist(x,y,param[-1])
  return(param[1]*(1 + d +1/3*d^2)*exp(-d))
}

kWhite <- function(x,y,param=NULL){
  if(is.null(param)) param <- 1
  d <- dist(x,y,rep(1,dim(x)[2]))
  return(param*(d==0))
}

plotGPR <- function(x,mean,var,xlab="",ylab="",ylim=NULL){
  sdDiag <- sqrt(pmax(0,var))
  upp95 <- mean + 1.96*sdDiag
  low95 <- mean - 1.96*sdDiag
  if(is.null(ylim)) ylim <- range(low95-0.5,upp95+0.5)
  plot(x, mean, type="n", xlab=xlab,ylab=ylab, ylim=ylim, cex.axis=1.5,cex.lab=2)
  polygon(c(x,rev(x)),c(upp95,rev(low95)),border=NA,col=lightblue)
  lines(x,mean,col=darkblue,lwd=3)
  lines(x,low95,col=darkbluetr)  
  lines(x,upp95,col=darkbluetr)  
}

