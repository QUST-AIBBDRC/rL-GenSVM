##Take MLL or DLBCL dataset as an example
y <- read.csv("MLL.csv",header=FALSE,sep=",")
y0 <- c(rep(1,24),rep(2,20),rep(3,28))#The class lable of MLL
#y0 <- c(rep(0,19),rep(1,58))#The class lable of DLBCL
###########
y2 <- scale(y)
ys <- as.numeric(scale(y0))
##############
refram <- list();relen <- c();
library(relaxo)
Relas <- relaxo(y2,ys,phi=1/3,max.steps = min(2*length(ys), 2 * ncol(y2)))
#############
for(i in 1:length(Relas$lambda))
{
  relen[i] <- length(which(Relas$beta[i,]!=0))
  for(j in 2:30)
    if(relen[i] == j&&Relas$phi[i]==1/3)
    { 
      coln <- colnames(y[,which(Relas$beta[i,]!=0)])
      refram[[j]] <- coln
    }
}