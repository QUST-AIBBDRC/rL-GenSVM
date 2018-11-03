     library(gensvm);
     library(pROC)
     acc <- c();auc <- c();
     rLfeage <- refram[[8]];q <- y2[,rLfeage];##8 feature genes
     class <- c(rep("a",19),rep("b",58))
     folda <- createFolds(class,k=10);i <- 1;
     repeat
      {if(i>10) break
       else 
      {
        test<-q[folda[[i]],];train<-q[-folda[[i]],];
        class<-as.data.frame(class)
        ytest<-class[folda[[i]],];ytrain<-class[-folda[[i]],];
        trainM <- as.matrix(train);testM <- as.matrix(test)
        ##########################################
        gsvm <- gensvm(trainM,ytrain,p=1,kernel='rbf',epsilon=1e1,
                       lambda=1e-7,gamma=1e-7,kappa=1.2,weights="group")
        psvm <- predict(gsvm,testM)
        acc[i] <- gensvm.accuracy(psvm,ytest)
        auc[i] <- roc(ytest,as.numeric(fap),levels=c("a","b"),auc=TRUE)$auc
        i <- i+1
       }};
     ACC <- mean(acc);AUC <- mean(auc)

