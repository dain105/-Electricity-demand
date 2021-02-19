#일별 시간별 전력수급량 자료 
elec.gen<-read.csv("S:/Users/Dain/R/energy/data/trade_raw.csv",header=T)


wds<-c("월요일","화요일","수요일","목요일","금요일","토요일","일요일")

#날짜변수 생성
elec.gen<-elec.gen[complete.cases(elec.gen),]
elec.gen[,1]<-as.Date(elec.gen[,1],"%Y-%m-%d")
elec.gen$weekdays<-weekdays(elec.gen[,1])
elec.gen$mon<-format(elec.gen[,1],"%m")

#요일변수 생성
elec.gen$wds<-NA
elec.gen$wds[which(elec.gen$weekdays=="월요일")]=1
elec.gen$wds[which(elec.gen$weekdays=="화요일")]=2
elec.gen$wds[which(elec.gen$weekdays=="수요일")]=3
elec.gen$wds[which(elec.gen$weekdays=="목요일")]=4
elec.gen$wds[which(elec.gen$weekdays=="금요일")]=5
elec.gen$wds[which(elec.gen$weekdays=="토요일")]=6
elec.gen$wds[which(elec.gen$weekdays=="일요일")]=7

#Set the period for clustering 
#colume 1 : Date 
#colume 2-25 : Hourly electricity demand 
cluster.data<-(elec.gen[which(format(elec.gen$LoadDay,"%Y")>=2008 & format(elec.gen$LoadDay,"%Y")<2013),1:25])
cluster.data[,2:25]<-sqrt(cluster.data[,2:25])


x<-seq(1:dim(cluster.data)[1])

#removing time trend using linear model given time zone (4hr~5hr)
fit.lm<-lm(cluster.data[,4]~x)

temp.data<-cluster.data

for (coln in 2:25)
  temp.data[,coln]<-cluster.data[,coln]-fit.lm$fitted.values

#temp.data<-electricity demand data without time trend

#plot time series without time trend given time zone
for ( trendi in 2:25)
{plot(temp.data[,trendi],type="l")
 abline(lm(temp.data[,trendi]~x),col=2)}


###########################################################
#Principal components analysis and extracting two main factors 
library(psych)

elec.pca<-principal(temp.data[,2:25], rotate="varimax", nfactors=2)

plot(elec.pca$loadings[,1], type="l", col=2, ylim=c(0,1), ylab="Loadings")
lines(elec.pca$loadings[,2], col=4)

plot(elec.pca) #Checking the characteristic of the preincipal components analysis
##########################################################

#k means clustering 
#function : kmeans 
#kmeans(data, number of cluster)

fit.kmeans8 <- kmeans(elec.pca$scores, 8)
fit.kmeans9 <- kmeans(elec.pca$scores, 9)
fit.kmeans10 <- kmeans(elec.pca$scores, 10)
fit.kmeans11 <- kmeans(elec.pca$scores, 11)


#example of result plot cluster(9, 10)

plot(elec.pca$scores[which(fit.kmeans9$cluster==1),], pch=16, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=9)")
points(elec.pca$scores[which(fit.kmeans9$cluster==2),], pch=17, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==3),], pch=18, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==4),], pch=19, col=4, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==5),], pch=20, col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==6),], pch=21, col=6, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==7),], pch=22, col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==8),], pch=23, col=8, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==9),], pch=24, col=9, xlim=c(-2,2), ylim=c(-2,2))

plot(elec.pca$scores[which(fit.kmeans10$cluster==1),], pch=16, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=10)")
points(elec.pca$scores[which(fit.kmeans10$cluster==2),], pch=17, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==3),], pch=18, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==4),], pch=19, col=4, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==5),], pch=20, col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==6),], pch=21, col=6, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==7),], pch=22, col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==8),], pch=23, col=8, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==9),], pch=24, col=9, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==10),], pch=25, col=10, xlim=c(-2,2), ylim=c(-2,2))


#######################################################################
#Model based clustering
library(mclust)
#fit.mclust <- Mclust(temp.data[,2:25])
#plot(fit.mclust)
#summary(fit.mclust)

#finding appropriate cluster number and result (based on mixture normal model)
Mclust(elec.pca$scores)

fit.mclust8<-Mclust(elec.pca$scores, G=8)
summary(fit.mclust8)

fit.mclust9 <- Mclust(elec.pca$scores, G=9)
summary(fit.mclust9)

fit.mclust10 <- Mclust(elec.pca$scores, G=10)
summary(fit.mclust10)

fit.mclust11 <- Mclust(elec.pca$scores, G=11)
summary(fit.mclust11)

plot(fit.mclust9)
plot(fit.mclust10)
#######################################################################


#######################################################################
#functional clustering
library('MFDA')

# names "BIC", "nclust", "clust", "clust.center
set.seed(124)
#r.cluster9<-MFclust(temp.data[,2:25],minG=9, maxG=9, nchain=4, iter.max=8)
#r.cluster10<-MFclust(temp.data[,2:25],minG=10, maxG=10, nchain=4, iter.max=8)

r.cluster9<-MFclust(elec.pca$scores,minG=9, maxG=9, nchain=4, iter.max=8)
r.cluster10<-MFclust(elec.pca$scores,minG=10, maxG=10, nchain=4, iter.max=8)


########################################
#plot k-cluster and its 95% confidence interval
for (t.clust in c(1,3,2,8,4,7,9,10,5,6)){
  plot(r.cluster10$clust.center[t.clust,],type="l",ylim=c(-40,60), ylab="Electricity demand", xlab="Time-point",
       main=paste("cluster=", cnt), xaxt="n")
  axis(1,at=c(1,4,7,12,13,14,18,22,24), c(1,4,7,12,13,14,18,22,24))
  con.int<-matrix(rep(0,24*2),ncol=2)
  
  for (coli in 2:25)
    con.int[coli-1,]<-quantile(temp.data[which(r.cluster10$clust==t.clust),coli],prob=c(0.025,0.975))
  
  polygon(c(1:24,24:1),c(con.int[,1],rev(con.int[,2])),col="grey75")
  
  abline(v=4,col="blue", lyt=2)
  #abline(v=6,col="blue", lyt=2)
  abline(v=12,col="blue", lyt=2)
  abline(v=13,col="blue", lyt=2)
  abline(v=14,col="blue", lyt=2)
  abline(v=18,col="blue", lyt=2)
  abline(v=22,col="blue", lyt=2)
  
  lines(1:24,con.int[,1], col="red", lty=2)
  lines(1:24,con.int[,2], col="red", lty=2)
  lines(1:24,r.cluster10$clust.center[t.clust,], lwd=2)
  cnt=cnt+1
}
#END k-cluster plot
