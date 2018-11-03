#######Take DLBCL dataset as an example
ACC <- c();AUC <- c();acc <- c();auc <- c();
for(j in 2:30)
{
   rLfeage <- refram[[j]];q <- ytrain2[,rLfeage];acc <- c();au <- c();
   ##refram is feature gene sets
   classtrain <- as.character(DLBCLtraclass)##DLBCLtraclass is from "82Split.R"
     folda <- createFolds(classtrain,k=10);i <- 1;
     repeat
      {if(i>10) break
       else 
      {
        test<-q[folda[[i]],];train<-q[-folda[[i]],];
        classtrain<-as.data.frame(classtrain)
        ytest<-classtrain[folda[[i]],];ytrain<-classtrain[-folda[[i]],];
        trainM <- as.matrix(train);testM <- as.matrix(test)
##########################################
        gsvm <- gensvm(trainM,ytrain,p=1,kernel='rbf',epsilon=10,
                       lambda=1e-7,gamma=1e-7,kappa=2,weights="group")
        psvm <- predict(gsvm,testM)
        acc[i] <- gensvm.accuracy(psvm,ytest)
        fap <- factor(psvm,level=c("0","1"))
        auc[i] <- roc(ytest,as.numeric(fap),levels=c("0","1"),auc=TRUE)$auc
        i <- i+1}};
ACC[j] <- mean(acc);AUC[j] <- mean(auc)};
ACC*100;AUC
