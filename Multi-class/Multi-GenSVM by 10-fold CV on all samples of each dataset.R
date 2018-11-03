     library(gensvm)
     library(irr)
     acc <- c();ka <- c();
     rLfeage <- refram[[13]];q <- y2[,rLfeage];
     class <- c(rep("a",24),rep("b",20),rep("c",28))
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
        gsvm <- gensvm(trainM,ytrain,p=1,kernel='rbf',epsilon=1e-3,
                       lambda=1e-9,gamma=1e-8,kappa=2,weights="group")
        psvm <- predict(gsvm,testM)
        acc[i] <- gensvm.accuracy(psvm,ytest)
        fap <- factor(psvm,level=c("a","b","c"))
        ka[i]<-kappa2(cbind(as.numeric(fap),ytest))$value
        i <- i+1
       }};
        ACC <- mean(acc);Kappa <- mean(ka)