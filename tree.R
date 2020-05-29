library(randomForest)
library(party)

options(max.print = 3000)
cmc <-
  read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data",
             header=FALSE, sep=",",dec=".")
table(cmc$V10)

train<-sample(1:nrow(cmc),1250)
cmc.train<-cmc[train,]
cmc.test<-cmc[-train,]

rf <- randomForest(V10 ~ . , data = cmc.train)
rf <- randomForest(V10 ~ . , data = cmc.train)
cm <- table(cmc.test$V10, predict(rf, cmc.test))
cm

cm2 <- cm
err <- rep(0, nrow(cm2))
for (a in 1:nrow(cm2))
{
  for (b in 1:ncol(cm2))
  {
    if (a != b)
    {
      err[a] <- err[a] + cm2[a,b]
    }
    cm2[a,b] <- cm2[a,b] / sum(cm[a,])
  }
  err[a] <- err[a] / sum(cm[a,])
}
round(cbind(cm2, err),2)
#recognition rate
rr<- 0
for (a in 1:nrow(cm))
{
  rr <- rr + cm[a,a]
}
round(rr / sum(cm),6)

summ <- 0
for (a in 1:nrow(cm))
{
  summ<- summ + cm[a,a]
}
summ/sum(cm)

importance(rf)
print(rf)
