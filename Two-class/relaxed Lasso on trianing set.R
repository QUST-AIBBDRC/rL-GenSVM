#Take DLBCL dataset as an example
y <- read.csv("DLBCL.csv",header=FALSE,sep=",")
#DLBCLtraclass,DLBCLtesclass,DLBCLtraidx,DLBCLtesidx is from "82Split.R"
classtrain <- as.character(DLBCLtraclass);classtest <- as.character(DLBCLtesclass)
ytrainrow <- y[DLBCLtraidx,];ytrain2 <- scale(ytrainrow)
ytestrow <- y[DLBCLtesidx,];ytest2 <- scale(ytestrow)
ys <- as.numeric(scale(DLBCLtraclass))
library(relaxo)
refram <- list();relen <- c();
Relas <- relaxo(ytrain2,ys,phi=1/3,max.steps = min(2*length(ys), 2*ncol(ytrain2)))
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
refram[[9]]