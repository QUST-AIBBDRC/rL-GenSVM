###Take the DLBCL dataset as an example
rLfeage <- refram[[20]];###refram is feature gene sets
q <- ytrain2[,rLfeage]
##########For two-class datasets, set epsilon=10; For Multi-class, set epsilon=1e-3
##########parameter of lambda and gamma of GenSVM
gr1 <- expand.grid(kernel="rbf",p=1,weights="group",epsilon=10,kappa=0,
                  lambda=10^seq(from=-10,to=-1),gamma=10^seq(from=-10,to=-1))
genr1 <- gensvm.grid(q,classtrain,param.grid=gr1,cv=10);genr1
paraacc <- genr1$cv.results$mean.test.score
la <- seq(from=-10,to=-1);ga <- seq(from=-10,to=-1)
pascoreM <- matrix(paraacc,nrow=10,ncol=10,dimnames=list(la,ga));
pascoreM*100
##########parameter of Kappa of GenSVM
gr2 <- expand.grid(kernel="rbf",p=1,weights="group",epsilon=10,kappa=seq(from=0,to=2.5,by=0.1),
                  lambda=1e-7,gamma=1e-7)
genr2 <- gensvm.grid(q,classtrain,param.grid=gr2,cv=10);genr2
katest <- cbind(kappa=seq(from=0,to=2.5,by=0.1),Acc=genr2$cv.results$mean.test.score*100);katest