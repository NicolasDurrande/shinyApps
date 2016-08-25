computeCatapult <- function(Fact,theta){
  Fact <- Fact[c(3,1,4,2)]
  XspringBras <- springBras[1,1] + Fact[1] * (springBras[2,1] - springBras[1,1]) 
  XRotBras <- rotBras[1,1] + Fact[2] * (rotBras[2,1] - rotBras[1,1]) 
  XspringMat <- springMat[1,2] + Fact[3] * (springMat[2,2] - springMat[1,2]) 
  XblockBody <- blockBody[1,] + Fact[4] * (blockBody[2,] - blockBody[1,])
  # define body
  body <- matrix(c(seq(-15,-widthMat,0.1),-widthMat,widthMat,widthMat,1.3*sqrt(seq(-15,-widthMat,0.1)+15)-2*widthBras,lengthMat,lengthMat,-2*widthBras),ncol=2)
  # define arm
  bras <- matrix(c(widthBras,widthBras,-lengthBras,-lengthBras-0.5,-lengthBras+2*widthBall+0.25,-lengthBras+2*widthBall,widthBras,-widthBras,-widthBras,widthBras+widthBall,widthBras+widthBall,widthBras),ncol=2)
  # rotate and translate arm
  rotatedBras <- bras %*% matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2)
  rotatedBras[,1] <- rotatedBras[,1] + XRotBras
  # define points
  ball <- c(-lengthBall,widthBras+widthBall)
  rotatedSpringBras <- rbind(ball,c(XspringBras,0),springBras) %*% matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2) 
  rotatedSpringBras[,1] <- rotatedSpringBras[,1] + XRotBras
  factPoints <- rbind(rotatedSpringBras,rotBras,springMat,blockBody)
  factBullets <- rbind(rotatedSpringBras[2,],c(XRotBras,0),c(0,XspringMat),XblockBody)
  #return
  catapult <- list(body,rotatedBras,factPoints,factBullets)
  return(catapult)
}

makepolygon <- function(X){
  circle <- 0.2*cbind(cos(seq(0,2*pi,0.1)),sin(seq(0,2*pi,0.1)))
  bool <- (circle[,1]*(X[1,1]-X[2,1])+circle[,2]*(X[1,2]-X[2,2]))>0
  circle[bool,1] <- circle[bool,1] + X[1,1]
  circle[bool,2] <- circle[bool,2] + X[1,2]
  circle[!bool,1] <- circle[!bool,1] + X[2,1]
  circle[!bool,2] <- circle[!bool,2] + X[2,2]
  return(circle)
}

plotCatapult <- function(catapult,colBody,colBall,plotBall=TRUE){
  if(plotBall) symbols(catapult[[3]][1,1],catapult[[3]][1,2], circles=widthBall,bg=colBall, inches=F, add=T)
  polygon(catapult[[1]],col=colBody)
  polygon(makepolygon(blockBody),col="white")
  polygon(makepolygon(rotBras),col="white")
  polygon(makepolygon(springMat),col="white")
  polygon(catapult[[2]],col=colBody)
  polygon(makepolygon(catapult[[3]][3:4,]),col="white")
  symbols(catapult[[4]][,1],catapult[[4]][,2], circles = rep(.3,nrow(catapult[[4]])),inches=F,add=T,bg='black')
  # add spring
  lines(c(catapult[[4]][1,1],catapult[[4]][3,1]),c(catapult[[4]][1,2],catapult[[4]][3,2]),lwd=2)
}

maxAngle <- function(Fact){
  Fact <- Fact[c(3,1,4,2)]
  XrotBras <- rotBras[1,] + Fact[2] * (rotBras[2,] - rotBras[1,]) 
  XblockBody <- blockBody[1,] + Fact[4] * (blockBody[2,] - blockBody[1,])
  x <- XblockBody - XrotBras
  theta <- pi - acos(x[1]/sqrt(sum(x^2))) - asin((0.3+widthBras)/sqrt(sum(x^2)))
  return(theta)
}

departureLocation<- function(Fact,theta){
  Fact <- Fact[c(3,1,4,2)]
  XrotBras <- c(rotBras[1,1] + Fact[2] * (rotBras[2,1] - rotBras[1,1]),0) 
  x <- c(-lengthBall,widthBras+widthBall) %*% matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2) 
  x <- x + XrotBras
  return(x)
}

departureSpeed <- function(Fact,theta){
  Fact <- Fact[c(3,1,4,2)]
  XspringBras <- c(springBras[1,1] + Fact[1] * (springBras[2,1] - springBras[1,1]),0)
  XrotBras <- c(rotBras[1,1] + Fact[2] * (rotBras[2,1] - rotBras[1,1]),0) 
  XspringMat <- c(0,springMat[1,2] + Fact[3] * (springMat[2,2] - springMat[1,2]))
  rotatedSpringBras <- XspringBras %*% matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2)
  lengthBeg <- sqrt(XspringMat[2]^2 + (XspringBras[1]+XrotBras[1])^2)
  lengthEnd <- sqrt(sum((rotatedSpringBras - XspringMat)^2))
  energy <- stiffnessSpring*(lengthBeg - lengthEnd)^2
  speed <- sqrt(energy/inertiaBras)*lengthBall*c(cos(pi/2-theta),sin(pi/2-theta))
  return(speed)
}

elementaryMove <- function(x,v,dt){
  x <- x + v*dt
  #   if(x[2]<=0){       # Should be uncommented for bouncing
  #     x[2] <- -x[2]
  #     v[2] <- -v[2]
  #   }
  v <- v + c(0,-9.81)*dt - ballFriction * v * dt
  return(c(x,v))
}

trajectory <- function(Fact){
  #Fact <- Fact[c(3,1,4,2)]
  # Flight initial conditions
  thetaMax <- maxAngle(Fact)
  X <- x <- departureLocation(Fact,thetaMax)
  v <- departureSpeed(Fact,thetaMax)
  # sum elementary moves
  while(x[2]>0){
    em <- elementaryMove(x,v,dt)
    x <- em[1:2]
    v <- em[3:4]
    X <- rbind(X,x)
  }
  return(X)
}

runExperiment <- function(Fact,plot='new',xlim=c(-20,200),ylim=c(-5,80),colBody=lightBlue,colBall=darkBlue,plotOutputValues=FALSE){
  #Fact <- Fact[c(3,1,4,2)]
  X <- trajectory(Fact)
  output <- apply(X, 2, max) 
  savpar <- par(no.readonly = TRUE)
  if(plot=='new'){ # create new plot window
    par(mar=c(0,0,0,0))
    plot(xlim,rep(-widthBall,2),xlim=xlim+c(-15,5),ylim=ylim,type='l',asp=1,ylab='',xlab="",axes=FALSE)
    lines(c(0,0),ylim)
    #title(paste('Fact = [ ',Fact[1],paste(', ',Fact[-1],collapse=''),' ]',sep=''),line=-2,col.main=colBody) #,collapse=' '))
  }
  # Plot catapult
  thetaMax <- maxAngle(Fact)
  catapult <- computeCatapult(Fact,thetaMax)
  plotCatapult(catapult,colBody,colBall)
  # Plot ball
  symbols(X[-1,1],X[-1,2], circles=rep(widthBall,nrow(X)-1), inches=F, add=T,bg=colBall)
  # plot Output Values
  lines(xlim,rep(output[2],2),lty=2)
  text(xlim[1],output[2],paste(round(output[2],1)),cex=1.2,pos=2,col=colBody)
  lines(rep(output[1],2),ylim,lty=2)
  text(output[1],ylim[1],paste(round(output[1],1)),cex=1.2,pos=1,col=colBody)
  par(savpar)
  return(output)
}


