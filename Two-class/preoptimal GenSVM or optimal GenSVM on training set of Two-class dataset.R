#Take DLBCL dataset as an example
classtrain <- as.character(DLBCLtraclass);classtest <- as.character(DLBCLtesclass)
    rLfeage <- refram[[9]];
    q1 <- ytrain2[,rLfeage];q2 <- ytest2[,rLfeage];
#####preoptimal gensvm on training set
    gsvm <- gensvm(q1,classtrain,p=1,kernel='rbf',epsilon=10,weights="group")
    psvm <- predict(gsvm,q2)
    acctest <- gensvm.accuracy(psvm,classtest)
    fap <- factor(psvm,level=c("0","1"))
    autest <- roc(classtest,as.numeric(fap),levels=c("0","1"),auc=TRUE)$auc
acctest*100;auctest


classtrain <- as.character(DLBCLtraclass);classtest <- as.character(DLBCLtesclass)
    rLfeage <- refram[[9]];
    q1 <- ytrain2[,rLfeage];q2 <- ytest2[,rLfeage];
#####optimal gensvm on training set
    gsvm <- gensvm(q1,classtrain,p=1,kernel='rbf',epsilon=10,
                      lambda=1e-7,gamma=1e-7,kappa=2,weights="group")
    psvm <- predict(gsvm,q2)
    acctest <- gensvm.accuracy(psvm,classtest)
    fap <- factor(psvm,level=c("0","1"))
    autest <- roc(classtest,as.numeric(fap),levels=c("0","1"),auc=TRUE)$auc
acctest*100;auctest