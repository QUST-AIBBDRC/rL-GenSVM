#Take MLL dataset as an example
classtrain <- as.character(MLLtraclass);classtest <- as.character(MLLtesclass)
    rLfeage <- refram[[14]];
    q1 <- ytrain2[,rLfeage];q2 <- ytest2[,rLfeage];
#####preoptimal gensvm on training set
    gsvm <- gensvm(q1,classtrain,p=1,kernel='rbf',epsilon=1e-3,weights="group")
    psvm <- predict(gsvm,q2)
    acctest <- gensvm.accuracy(psvm,classtest)
    fap <- factor(psvm,level=c("1","2","3"))
    kaptest<-kappa2(cbind(as.numeric(fap),classtest))$value
acctest*100;kaptest

classtrain <- as.character(MLLtraclass);classtest <- as.character(MLLtesclass)
    rLfeage <- refram[[14]];
    q1 <- ytrain2[,rLfeage];q2 <- ytest2[,rLfeage];
#####optimal gensvm on training set
    gsvm <- gensvm(q1,classtrain,p=1,kernel='rbf',epsilon=1e-3,
                      lambda=1e-7,gamma=1e-8,kappa=2,weights="group")
    psvm <- predict(gsvm,q2)
    acctest <- gensvm.accuracy(psvm,classtest)
    fap <- factor(psvm,level=c("1","2","3"))
    kaptest <- kappa2(cbind(as.numeric(fap),classtest))$value
acctest*100;kaptest;