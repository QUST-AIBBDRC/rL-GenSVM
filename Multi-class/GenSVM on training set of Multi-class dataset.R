#########Take MLL dataset as an example
ACC <- c();Kappa <- c();acc <- c();ka <- c();
   rLfeage <- refram[[8]];q <- ytrain2[,rLfeage];acc <- c();ka <- c();
  ########refram is feature gene sets
   classtrain <- as.character(Lytraclass)####Lytraclass is from "82Split.R"
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
        gsvm <- gensvm(trainM,ytrain,p=1,kernel='rbf',epsilon=1e-3,
                       lambda=1e-7,gamma=1e-7,kappa=2.5,weights="group")
        psvm <- predict(gsvm,testM)
        acc[i] <- gensvm.accuracy(psvm,ytest)
        fap <- factor(psvm,level=c("1","2","3"))
        if(length(table(as.numeric(ytest)))>=3)
        {
           if(length(table(as.numeric(psvm)))==1)
           {ka[i]<-gensvm.accuracy(psvm,ytest)}
           else
           {ka[i]<-kappa2(cbind(as.numeric(fap),ytest))$value}
         }
        if(length(table(as.numeric(ytest)))==1)
        {ka[i]<-gensvm.accuracy(psvm,ytest)}
        i <- i+1}};
ACC <- mean(acc);Kappa <- mean(ka)};