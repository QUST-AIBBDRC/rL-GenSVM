class <- c(rep("a",24),rep("b",20),rep("c",28))
rLfeage <- refram[[20]];###20 feature genes
q <- y2[,rLfeage];
###################parametre of lambda and gamma of GenSVM
gr1 <- expand.grid(kernel="rbf",p=1,weights="group",epsilon=1e-3,kappa=0,
                  lambda=10^seq(from=-10,to=-1),gamma=10^seq(from=-10,to=-1))
genr1 <- gensvm.grid(q,class,param.grid=gr1,cv=10);genr1
paraacc <- genr1$cv.results$mean.test.score
la <- seq(from=-10,to=-1);ga <- seq(from=-10,to=-1)
pascoreM <- matrix(paraacc,nrow=10,ncol=10,dimnames=list(la,ga))
###################parameter of kappa of GenSVM
class <- c(rep("a",24),rep("b",20),rep("c",28))
gr2 <- expand.grid(kernel="rbf",p=1,weights="group",epsilon=1e-3,kappa=seq(from=0,to=2.5,by=0.1),
                  lambda=1e-9,gamma=1e-8)
genr2 <- gensvm.grid(q,class,param.grid=gr2,cv=10);genr2
katest <- cbind(kappa=seq(from=0,to=2.5,by=0.1),
                   Acc=genr2$cv.results$mean.test.score*100)