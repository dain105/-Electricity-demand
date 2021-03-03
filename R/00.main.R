a<-read.csv("S:/Users/Dain/R/energy/data/trade_raw1.csv",header=T)

b <-log(a[,2:25])

x<-seq(1:dim(a)[1])

fit.lm<-lm(b[,4]~x)

temp.data<-a 

#write.table(temp.data,"E:/electricity.dat",sep=",")


for (coln in 2:25)
  temp.data[,coln]<-b[,(coln-1)]-fit.lm$fitted.values

for ( trendi in 2:25)
{plot(temp.data[,trendi],type="l")
  abline(lm(temp.data[,trendi]~x),col=2)}

###########################################################
#Principal components analysis and extracting two main factors 

library(psych)

elec.pca<-principal(temp.data[,2:25], rotate="varimax", nfactors=2)

plot(elec.pca$loadings[,1], type="l", col=2, ylim=c(0,1), ylab="Loadings")
lines(elec.pca$loadings[,2], col=4)

plot(elec.pca)

elec.pca$loadings


elec.pca$scores<-elec.pca$scores[complete.cases(elec.pca$scores),]
e1<-elec.pca$scores
##########################################################

#k means clustering
fit.kmeans5 <- kmeans(elec.pca$scores, 5)
fit.kmeans6 <- kmeans(elec.pca$scores, 6)
fit.kmeans7 <- kmeans(elec.pca$scores, 7)
fit.kmeans8 <- kmeans(elec.pca$scores, 8)
fit.kmeans9 <- kmeans(elec.pca$scores, 9)
fit.kmeans10 <- kmeans(elec.pca$scores, 10)
fit.kmeans11 <- kmeans(elec.pca$scores, 11)

temp.data$km8<-fit.kmeans8$cluster
temp.data$km9<-fit.kmeans9$cluster
temp.data$km10<-fit.kmeans10$cluster
temp.data$km11<-fit.kmeans11$cluster

plot(elec.pca$scores[which(fit.kmeans6$cluster==1),], pch=1, cex=17, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=6)")
points(elec.pca$scores[which(fit.kmeans6$cluster==2),], pch=7, cex=1, col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans6$cluster==3),], pch=4, cex=1, col=4, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans6$cluster==4),], pch=2, cex=1, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans6$cluster==5),], pch=3, cex=1, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans6$cluster==6),], pch=5, cex=1, col=5, xlim=c(-2,2), ylim=c(-2,2))

plot(elec.pca$scores[which(fit.kmeans7$cluster==1),], pch=16, cex=1, col=4, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=7)")
points(elec.pca$scores[which(fit.kmeans7$cluster==2),], pch=17, cex=17,  col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==3),], pch=18, cex=1,  col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==3),], pch=1, cex=1,  col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==4),], pch=19, cex=1,  col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==5),], pch=13, cex=1,  col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==6),], pch=14, cex=1,  col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans7$cluster==7),], pch=15, cex=1,  col=6, xlim=c(-2,2), ylim=c(-2,2))


plot(elec.pca$scores[which(fit.kmeans8$cluster==1),], pch=16, cex=1,  col=8, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=8)")
points(elec.pca$scores[which(fit.kmeans8$cluster==2),], pch=19, cex=1,col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==3),], pch=13, cex=1, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==4),], pch=17, cex=1, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==5),], pch=18, cex=1,col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==5),], pch=1,  cex=1,col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==6),], pch=21, cex=1, col=4, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==7),], pch=15, cex=1, col=6, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans8$cluster==8),], pch=20, cex=17, col=1, xlim=c(-2,2), ylim=c(-2,2))

plot(elec.pca$scores[which(fit.kmeans9$cluster==1),], pch=20, cex=1, col=5, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=9)")
points(elec.pca$scores[which(fit.kmeans9$cluster==2),], pch=16, cex=1, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==3),], pch=17, cex=1, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==4),], pch=1, cex=1, col=8, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==5),], pch=21, cex=1, col=4, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==6),], pch=15, cex=17, col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==7),], pch=19, cex=1, col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==7),], pch=1, cex=1, col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==8),], pch=20, cex=1,col=8, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans9$cluster==9),], pch=13, cex=1, col=6, xlim=c(-2,2), ylim=c(-2,2))

plot(elec.pca$scores[which(fit.kmeans10$cluster==1),], pch=18, cex=1,  col=4, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=10)")
points(elec.pca$scores[which(fit.kmeans10$cluster==2),], pch=15, cex=1,  col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==3),], pch=19, cex=1,  col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==4),], pch=17, cex=1,  col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==5),], pch=13, cex=1,  col=6, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==6),], pc=21, cex=1, col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==7),], pch=16, cex=1,  col=8, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==8),], pch=16, cex=1,  col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==9),], pch=20, cex=1,  col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==9),], pch=1, cex=1,  col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans10$cluster==10),], pch=9, cex=17,  col=1, xlim=c(-2,2), ylim=c(-2,2))

plot(elec.pca$scores[which(fit.kmeans11$cluster==1),], pch=4, cex=1, col=4, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="K-means clustering (k=11)")
points(elec.pca$scores[which(fit.kmeans11$cluster==2),], pch=9, cex=1, col="#8b4513", xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==3),], pch=10, cex=1, col="#ff7e24", xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==4),], pch=6, cex=1, col=6, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==5),], pch=5, cex=1, col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==6),], pch=5, cex=1, col=5, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==7),], pch=7, cex=1, col=7, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==8),], pch=1, cex=1, col=1, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==9),], pch=2, cex=1, col=2, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==10),], pch=3, cex=1, col=3, xlim=c(-2,2), ylim=c(-2,2))
points(elec.pca$scores[which(fit.kmeans11$cluster==11),], pch=8, cex=1, col=8, xlim=c(-2,2), ylim=c(-2,2))


#######################################################################
#Model based clustering
library(mclust)

#fit.mclust <- Mclust(temp.data[,2:25])
#plot(fit.mclust)
#summary(fit.mclust)
Mclust(elec.pca$scores)
fit.mclust6<-Mclust(elec.pca$scores, G=6)
summary(fit.mclust6)

fit.mclust7<-Mclust(elec.pca$scores, G=7)
summary(fit.mclust7)

fit.mclust8<-Mclust(elec.pca$scores, G=8)
summary(fit.mclust8)

fit.mclust9 <- Mclust(elec.pca$scores, G=9)
summary(fit.mclust9)

fit.mclust10 <- Mclust(elec.pca$scores, G=10)
summary(fit.mclust10)

fit.mclust11 <- Mclust(elec.pca$scores, G=11)
summary(fit.mclust11)

temp.data$mb8<-fit.mclust8$classification
temp.data$mb9<-fit.mclust9$classification
temp.data$mb10<-fit.mclust10$classification
temp.data$mb11<-fit.mclust11$classification


plot(fit.mclust6)
plot(fit.mclust7)
plot(fit.mclust8)
plot(fit.mclust9)
plot(fit.mclust10)
plot(fit.mclust11)


########################################################################



#functional clustering
library(mvtnorm)
library('MFDA')
library(gss)
dev.off()
# names "BIC", "nclust", "clust", "clust.center
set.seed(124)
# r.cluster9<-MFclust(temp.data[,2:25],minG=9, maxG=9, nchain=4, iter.max=8)
# r.cluster10<-MFclust(temp.data[,2:25],minG=10, maxG=10, nchain=4, iter.max=8)
r.cluster6<-MFclust(elec.pca$scores,minG=6, maxG=6, nchain=4, iter.max=8)
r.cluster7<-MFclust(elec.pca$scores,minG=7, maxG=7, nchain=4, iter.max=8)
r.cluster8<-MFclust(elec.pca$scores,minG=8, maxG=8, nchain=4, iter.max=8)
r.cluster9<-MFclust(elec.pca$scores,minG=9, maxG=9, nchain=4, iter.max=8)
r.cluster10<-MFclust(elec.pca$scores,minG=10, maxG=10, nchain=4, iter.max=8)
r.cluster11<-MFclust(elec.pca$scores,minG=11, maxG=11, nchain=4, iter.max=8)

#correlation between climatic information and classfication result
clu.dat<-read.csv("E:/WISE/2015/R-work/energy/data/cluster.csv", header=T)
rpart<-read.csv("E:/WISE/2015/R-work/energy/data/cli_anova.csv")


plot(elec.pca$scores[which(r.cluster6$clust==1),], pch=5, cex=1, col=5, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=6)")
points(elec.pca$scores[which(r.cluster6$clust==2),], pch=4, cex=1, col=4)
points(elec.pca$scores[which(r.cluster6$clust==3),], pch=3, cex=1, col=3)
points(elec.pca$scores[which(r.cluster6$clust==4),], pch=1, cex=1, col=1)
points(elec.pca$scores[which(r.cluster6$clust==5),], pch=2,  cex=1,col=2)
points(elec.pca$scores[which(r.cluster6$clust==6),], pch=7, cex=1, col=7)



plot(elec.pca$scores[which(r.cluster7$clust==1),], pch=15,  cex=1, col=8, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=7)")
points(elec.pca$scores[which(r.cluster7$clust==2),], pch=13, cex=1, col=2)
points(elec.pca$scores[which(r.cluster7$clust==3),], pch=19, cex=1, col=3)
points(elec.pca$scores[which(r.cluster7$clust==4),], pch=16, cex=1, col=5)
points(elec.pca$scores[which(r.cluster7$clust==5),], pch=17, cex=1, col=1)
points(elec.pca$scores[which(r.cluster7$clust==6),], pch=18, cex=1, col=7)
points(elec.pca$scores[which(r.cluster7$clust==6),], pch=1, cex=1, col=1)
points(elec.pca$scores[which(r.cluster7$clust==6),], pch=1, cex=1, col=1)
points(elec.pca$scores[which(r.cluster7$clust==7),], pch=21, cex=1, col=4)

plot(elec.pca$scores[which(r.cluster8$clust==1),], pch=20, cex=1, col=8, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=8)")
points(elec.pca$scores[which(r.cluster8$clust==2),], pch=17, cex=1, col=1)
points(elec.pca$scores[which(r.cluster8$clust==3),], pch=21, cex=1, col=4)
points(elec.pca$scores[which(r.cluster8$clust==4),], pch=20, cex=1, col=6)
points(elec.pca$scores[which(r.cluster8$clust==5),], pch=16, cex=1, col=5)
points(elec.pca$scores[which(r.cluster8$clust==6),], pch=13, cex=1, col=2)
points(elec.pca$scores[which(r.cluster8$clust==7),], pch=18, cex=1, col=7)
points(elec.pca$scores[which(r.cluster8$clust==7),], pch=1, cex=1, col=1)
points(elec.pca$scores[which(r.cluster8$clust==8),], pch=19, cex=1, col=3)


plot(elec.pca$scores[which(r.cluster9$clust==1),], pch=1, cex=1, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=9)")
points(elec.pca$scores[which(r.cluster9$clust==2),], pch=2,  cex=1, col=2)
points(elec.pca$scores[which(r.cluster9$clust==3),], pch=3,  cex=1, col=3)
points(elec.pca$scores[which(r.cluster9$clust==4),], pch=4,  cex=1, col=4)
points(elec.pca$scores[which(r.cluster9$clust==5),], pch=5,  cex=1, col=5)
points(elec.pca$scores[which(r.cluster9$clust==6),], pch=6,  cex=1, col=6)
points(elec.pca$scores[which(r.cluster9$clust==7),], pch=7,  cex=1, col=7)
points(elec.pca$scores[which(r.cluster9$clust==8),], pch=8,  cex=1, col=8)
points(elec.pca$scores[which(r.cluster9$clust==9),], pch=8,  cex=1, col=8)

table(r.cluster9$clust)


plot(elec.pca$scores[which(r.cluster10$clust==1),], pch=1, cex=1, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=10)")
points(elec.pca$scores[which(r.cluster10$clust==2),], pch=2, cex=1, col=2)
points(elec.pca$scores[which(r.cluster10$clust==3),], pch=3, cex=1, col=3)
points(elec.pca$scores[which(r.cluster10$clust==4),], pch=4, cex=1, col=4)
points(elec.pca$scores[which(r.cluster10$clust==5),], pch=5, cex=1, col=5)
points(elec.pca$scores[which(r.cluster10$clust==6),], pch=6, cex=1, col=6)
points(elec.pca$scores[which(r.cluster10$clust==7),], pch=7, cex=1, col=7)
points(elec.pca$scores[which(r.cluster10$clust==8),], pch=3, cex=1, col=8)
points(elec.pca$scores[which(r.cluster10$clust==9),], pch=9, cex=1, col=9)
points(elec.pca$scores[which(r.cluster10$clust==10),], pch=10, cex=1, col=10)


table(r.cluster10$clust)


c.rpart<-cbind(rpart,clu.dat,elec.pca$scores)

che.cor<-c.rpart[,c("clust10","PC1","PC2","Tmax","Tmin")]

cor(che.cor)

plot(che.cor[which(che.cor$clust10==1),2:5])
plot(che.cor[which(che.cor$clust10==2),2:5])
plot(che.cor[which(che.cor$clust10==3),2:5])
plot(che.cor[which(che.cor$clust10==4),2:5])
plot(che.cor[which(che.cor$clust10==5),2:5])
plot(che.cor[which(che.cor$clust10==6),2:5])
plot(che.cor[which(che.cor$clust10==7),2:5])
plot(che.cor[which(che.cor$clust10==8),2:5])
plot(che.cor[which(che.cor$clust10==9),2:5])
plot(che.cor[which(che.cor$clust10==10),2:5])

plot(che.cor[,2:5])


###################################################################################
# Read climate data
d.cli<-read.csv("E:/wise/2015/r-work/energy/data/climate.csv")
d.cluster<-read.csv("E:/wise/2015/r-work/energy/data/cluster.csv")

###################################################################################
# saving plots 
# projected 2 dimesional factors

png("E:/wise/2015/paper/cluster classifier/plots/FC10_PC.png", width=600, height=600)

s1<-c(9,3,10,7,1,2,5,8,4,6)
temp<-d.cluster[which(d.cluster$FC10==s1[1]),c("PC1","PC2")]
plot(temp, pch=16, col=11, xlim=c(-4,2.3),ylim=c(-2.8,2.7)
     ,main="Functional clustering (clusters : 10)")

for (i in 2:10){
  temp<-d.cluster[which(d.cluster$FC10==s1[i]),c("PC1","PC2")]
  points(temp, pch=(16-i), col=i)}
dev.off()

png("E:/wise/2015/paper/cluster classifier/plots/KM10_PC.png", width=600, height=600)



temp<-d.cluster[which(d.cluster$KM10==1),c("PC1","PC2")]
plot(temp, pch=16, col=11, xlim=c(-4,2.3),ylim=c(-2.8,2.7)
     ,main="K-means clustering (clusters : 10)")

for (i in 2:10){
  temp<-d.cluster[which(d.cluster$KM10==i),c("PC1","PC2")]
  points(temp, pch=(16-i), col=i)}
dev.off()

png("E:/wise/2015/paper/cluster classifier/plots/MC10_PC.png", width=600, height=600)
s2<-c(5,2,7,8,10,9,4,1,6,3)
temp<-d.cluster[which(d.cluster$MC10==s2[1]),c("PC1","PC2")]
plot(temp, pch=16, col=11, xlim=c(-4,2.3),ylim=c(-2.8,2.7)
     ,main="Model based clustering (clusters : 10)")

for (i in 2:10){
  temp<-d.cluster[which(d.cluster$MC10==s2[i]),c("PC1","PC2")]
  points(temp, pch=(16-i), col=i)}
dev.off()


###################################################################################
#
#
png("E:/wise/2015/paper/cluster classifier/plots/FC10_clu.png", width=600, height=600)
#par(mfrow=c())
plot(x=1:24,y=rep(NA,24),ylim=c(-40, 60), main="Functional clustering", 
     xlab="Hour", ylab="Electricity demand") 

for (i in 1:10){
  temp<-d.cluster[which(d.cluster$FC10==s1[i]),3:26]
  m.temp<-colMeans(temp)
  #l.temp<-h.temp<-rep(0,24)
  #  for (j in 1:24)
  #    {
  #      l.temp[j]<-quantile(temp[,j])[2]
  #      h.temp[j]<-quantile(temp[,j])[4]
  #    }
  
  #polygon(c(1:24,24:1),c(l.temp,rev(h.temp)),col=i)
  lines(m.temp, type="l", col=i, ylim=c(-40, 60),lwd=2) 
}
dev.off()

png("E:/wise/2015/paper/cluster classifier/plots/KM10_clu.png", width=600, height=600)
par(mfrow=c(2,2))
for (i in 1:10){
  temp<-d.cluster[which(d.cluster$KM10==i),3:26]
  m.temp<-colMeans(temp)
  l.temp<-h.temp<-rep(0,24)
  for (j in 1:24)
  {
    l.temp[j]<-quantile(temp[,j])[2]
    h.temp[j]<-quantile(temp[,j])[4]
  }
  plot(m.temp, type="l",ylim=c(-40, 60), main=paste("Cluster",i,"(K-Means clustering)"),
       ylab="Electricity demand") 
  polygon(c(1:24,24:1),c(l.temp,rev(h.temp)),col="grey75")
  lines(m.temp, type="l", col=1, ylim=c(-40, 60),lwd=2) 
}
dev.off()

png("E:/wise/2015/paper/cluster classifier/plots/MC10_clu.png", width=600, height=600)
par(mfrow=c(2,2))
for (i in 1:10){
  temp<-d.cluster[which(d.cluster$MC10==i),3:26]
  m.temp<-colMeans(temp)
  l.temp<-h.temp<-rep(0,24)
  for (j in 1:24)
  {
    l.temp[j]<-quantile(temp[,j])[2]
    h.temp[j]<-quantile(temp[,j])[4]
  }
  plot(m.temp, type="l",ylim=c(-40, 60), main=paste("Cluster",i,"(Model based clustering)"),
       ylab="Electricity demand") 
  polygon(c(1:24,24:1),c(l.temp,rev(h.temp)),col="grey75")
  lines(m.temp, type="l", col=1, ylim=c(-40, 60),lwd=2) 
}
dev.off()



############################################################################################################
# 

temp.data[,1]<-as.Date(temp.data[,1],"%Y-%m-%d")
temp.data$weekdays<-weekdays(temp.data[,1])

temp.data$wds[which(temp.data$weekdays=="월요일")]=1
temp.data$wds[which(temp.data$weekdays=="화요일")]=2
temp.data$wds[which(temp.data$weekdays=="수요일")]=3
temp.data$wds[which(temp.data$weekdays=="목요일")]=4
temp.data$wds[which(temp.data$weekdays=="금요일")]=5
temp.data$wds[which(temp.data$weekdays=="토요일")]=6
temp.data$wds[which(temp.data$weekdays=="일요일")]=7
#??????????
a<-read.csv("S:/Users/Dain/R/energy/data/data.1.csv",header=T)



#????ġ ???Ѱ?
a$average.temperature <- a$average.temp*a$weight
a$minimum.temperature <- a$lowtemp*a$weight 	
a$maximum.temperature<-a$high.temp*a$weight
a$precipitation<-a$dayrain*a$weight
a$average.wind.speed<-a$average.wind*a$weight
a$minimum.relative.humidity<-a$low.humidity*a$weight
a$average.relative.humidity<-a$average.humidity*a$weight
a$sunlight.hours<-a$sumdaylighthours*a$weight
a$holiday<- 1*a$holiday
#??��??¥???? ???ճ???/ ?ϳ??ǵ???ó?? ??????
b<-aggregate(a[,14:22], list(day=a$day),mean)


b$weekday<-temp.data$wds

x<-b[,1:11]
x[is.na(x)] <- 0
sum(is.na(x))
#k=means ???? ?????? ?ֱ?
x$k6<-fit.kmeans6$cluster
x$k7<-fit.kmeans7$cluster
x$k8<-fit.kmeans8$cluster
x$k9<-fit.kmeans9$cluster
x$k10<-fit.kmeans10$cluster

#mclust
x$m6<- fit.mclust6$classification
x$m7<- fit.mclust7$classification
x$m8<- fit.mclust8$classification
x$m9<- fit.mclust9$classification
x$m10<- fit.mclust10$classification
head(x)
#Functional 

x$f6<-r.cluster6$clust
x$f7<-r.cluster7$clust
x$f8<-r.cluster8$clust
x$f9<-r.cluster9$clust
x$f10<-r.cluster10$clust

# ?????͸? trainData?? testData?? ???ø??Ѵ?.
trainData<- x[1:1461,]
testData<- x[1462:1827,]



## rflibrary(randomForest)
library(randomForest)
library(caret)
#K-MEANS

head(trainData)

rf6 <- randomForest(as.factor(k6) ~ .  -day -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10 , data=trainData, var.importance=TRUE)
rf6.pred <- predict(rf6, testData)
Tabs <- table(rf6.pred, testData$k6)
r.rf6<-confusionMatrix(Tabs)
round(r.rf6$overall,4)


rf7 <- randomForest(as.factor(k7) ~ .  -day -k6 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10   , data=trainData, var.importance=TRUE)
rf7.pred <- predict(rf7, testData)
Tabs <- table(rf7.pred, testData$k7)
r.rf7<-confusionMatrix(Tabs)
round(r.rf7$overall,4)


rf8 <- randomForest(as.factor(k8) ~ .  -day -k6 -k7   -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rf8.pred <- predict(rf8, testData)
Tabs <- table(rf8.pred, testData$k8)
r.rf8<-confusionMatrix(Tabs)
round(r.rf8$overall,4)

rf9 <- randomForest(as.factor(k9) ~ .   -day -k6 -k7 -k8  -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10 , data=trainData, var.importance=TRUE)
rf9.pred <- predict(rf9, testData)
Tabs <- table(rf9.pred, testData$k9)
r.rf9<-confusionMatrix(Tabs)
round(r.rf9$overall,4)

rf10 <- randomForest(as.factor(k10) ~ .   -day -k6 -k7 -k8  -k9  -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rf10.pred <- predict(rf10, testData)
Tabs <- table(rf10.pred, testData$k10)
r.rf10<-confusionMatrix(Tabs)
round(r.rf10$overall,4)


#m clust 
head(trainData)
rfm6 <- randomForest(as.factor(m6) ~ .  -day -k6 -k7 -k8  -k9 -k10 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10 , data=trainData, var.importance=TRUE)
rfm6.pred <- predict(rfm6, testData)
Tabs.m6 <- table(rfm6.pred, testData$m6)
r.rfm6<-confusionMatrix(Tabs.m6)
round(r.rfm6$overall,4)

rfm7 <- randomForest(as.factor(m7) ~ .   -day -k6 -k7 -k8  -k9 -k10 -m6 -m8 -m9 -m10  -f6 -f7 -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rfm7.pred <- predict(rfm7, testData)
Tabs.m7 <- table(rfm7.pred, testData$m7)
r.rfm7<-confusionMatrix(Tabs.m7)
round(r.rfm7$overall,4)

rfm8 <- randomForest(as.factor(m8) ~ .   -day  , data=trainData, var.importance=TRUE)
rfm8.pred <- predict(rfm8, testData)
Tabs.m8 <- table(rfm8.pred, testData$m8)
r.rfm8<-confusionMatrix(Tabs.m8)
round(r.rfm8$overall,4)

getTree(rfm8, k=1, labelVar=FALSE)


rfm9 <- randomForest(as.factor(m9) ~ .   -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m10  -f6 -f7 -f8 -f9 -f10 , data=trainData, var.importance=TRUE)
rfm9.pred <- predict(rfm9, testData)
Tabs.m9 <- table(rfm9.pred, testData$m9)
r.rfm9<-confusionMatrix(Tabs.m9)
round(r.rfm9$overall,4)

rfm10 <- randomForest(as.factor(m10) ~ .   -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -f6 -f7 -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rfm10.pred <- predict(rfm10, testData)
Tabs.m10 <- table(rfm10.pred, testData$m10)
r.rfm10<-confusionMatrix(Tabs.m10)
round(r.rfm10$overall,4)
#FunCTIONAL 
rf.f6 <- randomForest(as.factor(f6) ~ . -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f7 -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rf.pred.f6 <- predict(rf.f6, testData)
Tabs.f6 <- table(rf.pred.f6, testData$f6)
r.rf.f6<-confusionMatrix(Tabs.f6)
round(r.rf.f6$overall,4)

rf.f7 <- randomForest(as.factor(f7) ~ .  -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6  -f8 -f9 -f10  , data=trainData, var.importance=TRUE)
rf.pred.f7 <- predict(rf.f7, testData)
Tabs.f7 <- table(rf.pred.f7, testData$f7)
r.rf.f7<-confusionMatrix(Tabs.f7)
round(r.rf.f7$overall,4)

rf.f8 <- randomForest(as.factor(f8) ~ . -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f9 -f10 , data=trainData, var.importance=TRUE)
rf.pred.f8 <- predict(rf.f8, testData)
Tabs.f8 <- table(rf.pred.f8, testData$f8)
r.rf.f8<-confusionMatrix(Tabs.f8)
round(r.rf.f8$overall,4)

rf.f9 <- randomForest(as.factor(f9) ~ .  -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8  -f10 , data=trainData, var.importance=TRUE)
rf.pred.f9 <- predict(rf.f9, testData)
Tabs.f9 <- table(rf.pred.f9, testData$f9)
r.rf.f9<-confusionMatrix(Tabs.f9)
round(r.rf.f9$overall,4)

rf.f10 <- randomForest(as.factor(f10) ~ .  -day -k6 -k7 -k8  -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f6 -f7 -f8 -f9   , data=trainData, var.importance=TRUE)
rf.pred.f10 <- predict(rf.f10, testData)
Tabs.f10 <- table(rf.pred.f10, testData$f10)
r.rf.f10<-confusionMatrix(Tabs.f10)
round(r.rf.f10$overall,4)



varImpPlot(rf6)
varImpPlot(rf7)
varImpPlot(rfm8)
varImpPlot(rf9)
varImpPlot(rf10)
varImpPlot(rf)


## Decision Tree
library(rpart)
tree.k6 <- rpart(as.factor(k6) ~ . -day -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.k6)
text(tree.k6, use.n=TRUE)
tree.k6.pred <- predict(tree.k6, testData,type='class')
Tabs.k6 <- table(tree.k6.pred, testData$k6)
dt.k6 <- confusionMatrix(Tabs.k6)
round(dt.k6$overall,4)

tree.k7 <- rpart(as.factor(k7) ~ .-day -k6  -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.k7)
text(tree.k7, use.n=TRUE).
tree.k7.pred <- predict(tree.k7, testData,type='class')
Tabs.k7 <- table(tree.k7.pred, testData$k7)
dt.k7 <- confusionMatrix(Tabs.k7)
round(dt.k7$overall,4)

tree.k8 <- rpart(as.factor(k8) ~ . -day -k6 -k7  -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10 , data=trainData, method="class")
plot(tree.k8)
text(tree.k8, use.n=TRUE)
tree.k8.pred <- predict(tree.k8, testData,type='class')
Tabs.k8 <- table(tree.k8.pred, testData$k8)
dt.k8 <- confusionMatrix(Tabs.k8)
round(dt.k8$overall,4)

tree.k9 <- rpart(as.factor(k9) ~ . -day -k6 -k7 -k8  -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.k9)
text(tree.k9, use.n=TRUE)
tree.k9.pred <- predict(tree.k9, testData,type='class')
Tabs.k9 <- table(tree.k9.pred, testData$k9)
dt.k9 <- confusionMatrix(Tabs.k9)
round(dt.k9$overall,4)

tree.k10 <- rpart(as.factor(k10) ~ . -day -k6 -k7 -k8 -k9  -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10 , data=trainData, method="class")
plot(tree.k10)
text(tree.k10, use.n=TRUE)
tree.k10.pred <- predict(tree.k10, testData,type='class')
Tabs.k10 <- table(tree.k10.pred, testData$k10)
dt.k10 <- confusionMatrix(Tabs.k10)
round(dt.k10$overall,4)

#dt.mclust.classification
tree.m6 <- rpart(as.factor(m6) ~ . -day -k6 -k7 -k8 -k9 -k10 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.m6)
text(tree.m6, use.n=TRUE)
tree.m6.pred <- predict(tree.m6, testData,type='class')
Tabs.m6 <- table(tree.m6.pred, testData$m6)
dt.m6 <- confusionMatrix(Tabs.m6)
round(dt.m6 $overall,4)

tree.m7 <- rpart(as.factor(m7) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.m7)
text(tree.m7, use.n=TRUE)
tree.m7.pred <- predict(tree.m7, testData,type='class')
Tabs.m7 <- table(tree.m7.pred, testData$m7)
dt.m7 <- confusionMatrix(Tabs.m7)
round(dt.m7 $overall,4)

tree.m8 <- rpart(as.factor(m8) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7  -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.m8)
text(tree.m8, use.n=TRUE)
tree.m8.pred <- predict(tree.m8, testData,type='class')
Tabs.m8 <- table(tree.m8.pred, testData$m8)
dt.m8 <- confusionMatrix(Tabs.m8)
round(dt.m8 $overall,4)

tree.m9 <- rpart(as.factor(m9) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.m9)
text(tree.m9, use.n=TRUE)
tree.m9.pred <- predict(tree.m9, testData,type='class')
Tabs.m9 <- table(tree.m9.pred, testData$m9)
dt.m9 <- confusionMatrix(Tabs.m9)
round(dt.m9 $overall,4)

tree.m10 <- rpart(as.factor(m10) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9  -f6 -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.m10)
text(tree.m10, use.n=TRUE)
tree.m10.pred <- predict(tree.m10, testData,type='class')
Tabs.m10 <- table(tree.m10.pred, testData$m10)
dt.m10 <- confusionMatrix(Tabs.m10)
round(dt.m10 $overall,4)

#dt.fclust.classification
tree.f6 <- rpart(as.factor(f6) ~ .-day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10  -f7 -f8 -f9 -f10, data=trainData, method="class")
plot(tree.f6)
text(tree.f6, use.n=TRUE)
tree.pred.f6 <- predict(tree.f6, testData,type='class')
Tabs.f6 <- table(tree.pred.f6, testData$f6)
dt.f6 <- confusionMatrix(Tabs.f6)
round(dt.f6 $overall,4)

tree.f7 <- rpart(as.factor(f7) ~ .-day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6  -f8 -f9 -f10, data=trainData, method="class")
plot(tree.f7)
text(tree.f7, use.n=TRUE)
tree.pred.f7 <- predict(tree.f7, testData,type='class')
Tabs.f7 <- table(tree.pred.f7, testData$f7)
dt.f7 <- confusionMatrix(Tabs.f7)
round(dt.f7 $overall,4)

tree.f8 <- rpart(as.factor(f8) ~ .-day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7  -f9 -f10, data=trainData, method="class")
plot(tree.f8)
text(tree.f8, use.n=TRUE)
tree.pred.f8 <- predict(tree.f8, testData,type='class')
Tabs.f8 <- table(tree.pred.f8, testData$f8)
dt.f8 <- confusionMatrix(Tabs.f8)
round(dt.f8 $overall,4)

tree.f9 <- rpart(as.factor(f9) ~ .-day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8  -f10, data=trainData, method="class")
plot(tree.f9)
text(tree.f9, use.n=TRUE)
tree.pred.f9 <- predict(tree.f9, testData,type='class')
Tabs.f9 <- table(tree.pred.f9, testData$f9)
dt.f9 <- confusionMatrix(Tabs.f9)
round(dt.f9 $overall,4)

tree.f10 <- rpart(as.factor(f10) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9, data=trainData, method="class")
plot(tree.f10)
text(tree.f10, use.n=TRUE)
tree.pred.f10 <- predict(tree.f10, testData,type='class')
Tabs.f10 <- table(tree.pred.f10, testData$f10)
dt.f10 <- confusionMatrix(Tabs.f10)
round(dt.f10 $overall,4)


## SVM
library(e1071)
head(trainData)
svm.model.6<-svm(as.factor(k6) ~ .  -day  -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.6 <- predict(svm.model.6, testData)
Tabs.6 <- table(svm.pred.6, testData$k6)
svm.k6<-confusionMatrix(Tabs.6)
round(svm.k6 $overall,4)


svm.model.7<-svm(as.factor(k7) ~ . -day -k6  -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.7 <- predict(svm.model.7, testData)
Tabs.7 <- table(svm.pred.7, testData$k7)
svm.k7<-confusionMatrix(Tabs.7)
round(svm.k7 $overall,4)


svm.model.8<-svm(as.factor(k8) ~ . -day -k6 -k7  -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.8 <- predict(svm.model.8, testData)
Tabs.8 <- table(svm.pred.8, testData$k8)
svm.k8<-confusionMatrix(Tabs.8)
round(svm.k8 $overall,4)

svm.model.9<-svm(as.factor(k9) ~ .-day -k6 -k7 -k8 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.9 <- predict(svm.model.9, testData)
Tabs.9 <- table(svm.pred.9, testData$k9)
svm.k9<-confusionMatrix(Tabs.9)
round(svm.k9 $overall,4)

svm.model.10<-svm(as.factor(k10) ~ .-day -k6 -k7 -k8 -k9 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.10 <- predict(svm.model.10, testData)
Tabs.10 <- table(svm.pred.10, testData$k10)
svm.k10<-confusionMatrix(Tabs.10)
round(svm.k10 $overall,4)
#가우시안
svm.model.m6<-svm(as.factor(m6) ~ . -day -k6 -k7 -k8 -k9 -k10 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.m6 <- predict(svm.model.m6, testData)
Tabs.m6 <- table(svm.pred.m6, testData$m6)
svm.m6<-confusionMatrix(Tabs.m6)
round(svm.m6 $overall,4)

svm.model.m7<-svm(as.factor(m7) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m8 -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.m7 <- predict(svm.model.m7, testData)
Tabs.m7 <- table(svm.pred.m7, testData$m7)
svm.m7<-confusionMatrix(Tabs.m7)
round(svm.m7 $overall,4)

svm.model.m8<-svm(as.factor(m8) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7  -m9 -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.m8 <- predict(svm.model.m8, testData)
Tabs.m8 <- table(svm.pred.m8, testData$m8)
svm.m8<-confusionMatrix(Tabs.m8)
round(svm.m8 $overall,4)

svm.model.m9<-svm(as.factor(m9) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8  -m10 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.m9 <- predict(svm.model.m9, testData)
Tabs.m9 <- table(svm.pred.m9, testData$m9)
svm.m9<-confusionMatrix(Tabs.m9)
round(svm.m9 $overall,4)

svm.model.m10<-svm(as.factor(m10) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -f6 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.m10 <- predict(svm.model.m10, testData)
Tabs.m10 <- table(svm.pred.m10, testData$m10)
svm.m10<-confusionMatrix(Tabs.m10)
round(svm.m10 $overall,4)


#functional

svm.model.f6<-svm(as.factor(f6) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f7 -f8 -f9 -f10, data=trainData)
svm.pred.f6 <- predict(svm.model.f6, testData)
Tabs.f6 <- table(svm.pred.f6, testData$f6)
svm.f6<-confusionMatrix(Tabs.f6)
round(svm.f6 $overall,4)


svm.model.f7<-svm(as.factor(f7) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f8 -f9 -f10, data=trainData)
svm.pred.f7 <- predict(svm.model.f7, testData)
Tabs.f7 <- table(svm.pred.f7, testData$f7)
svm.f7<-confusionMatrix(Tabs.f7)
round(svm.f7 $overall,4)

svm.model.f8<-svm(as.factor(f8) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f9 -f10, data=trainData)
svm.pred.f8 <- predict(svm.model.f8, testData)
Tabs.f8 <- table(svm.pred.f8, testData$f8)
svm.f8<-confusionMatrix(Tabs.f8)
round(svm.f8 $overall,4)

svm.model.f9<-svm(as.factor(f9) ~ .-day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f10, data=trainData)
svm.pred.f9 <- predict(svm.model.f9, testData)
Tabs.f9 <- table(svm.pred.f9, testData$f9)
svm.f9<-confusionMatrix(Tabs.f9)
round(svm.f9 $overall,4)

svm.model.f10<-svm(as.factor(f10) ~ . -day -k6 -k7 -k8 -k9 -k10 -m6 -m7 -m8 -m9 -m10 -f6 -f7 -f8 -f9, data=trainData)
svm.pred.f10 <- predict(svm.model.f10, testData)
Tabs.f10 <- table(svm.pred.f10, testData$f10)
svm.f10 <-confusionMatrix(Tabs.f10)
round(svm.f10 $overall,4)

##naive bayes
library(e1071)
head(trainData)
nBayes.k6 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,12])))
pred_bayes.6<-predict(nBayes.k6, testData[,2:11])
testData[,12]
nb6<-table(pred_bayes.6, testData[,12])
nb.k6<-confusionMatrix(nb6)
round(nb.k6$overall,4)



nBayes.7 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,13])))
pred_bayes.7<-predict(nBayes.7, testData[,2:11])
testData[,13]
nb7<-table(pred_bayes.7, testData[,13])
nb.k7<-confusionMatrix(nb7)
round(nb.k7$overall,4)

nBayes.8 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,14])))
pred_bayes.8<-predict(nBayes.8, testData[,2:11])
testData[,14]
nb8<-table(pred_bayes.8, testData[,14])
nb.k8<-confusionMatrix(nb8)
round(nb.k8$overall,4)

nBayes.9 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,15])))
pred_bayes.9<-predict(nBayes.9, testData[,2:11])
testData[,15]
nb9<-table(pred_bayes.9, testData[,15])
nb.k9<-nb.k9<-confusionMatrix(nb9)
round(nb.k9$overall,4)

nBayes.10 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,16])))
pred_bayes.10<-predict(nBayes.10, testData[,2:11])
testData[,16]
table(pred_bayes.10, testData[,16])
nb10<-table(pred_bayes.10, testData[,16])
nb.k10<-confusionMatrix(nb10)
round(nb.k10$overall,4)

#mclust
head(x)
nBayes.m6 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,17])))
pred_bayes.m6<-predict(nBayes.m6, testData[,2:11])
testData[,17]
nb.m6<-table(pred_bayes.m6, testData[,17])
nb.m6<-confusionMatrix(nb.m6)
round(nb.m6$overall,4)

nBayes.m7 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,18])))
pred_bayes.m7<-predict(nBayes.m7, testData[,2:11])
testData[,18]
nb.m7<-table(pred_bayes.m7, testData[,18])
nb.m7<-confusionMatrix(nb.m7)
round(nb.m7$overall,4)

nBayes.m8 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,19])))
pred_bayes.m8<-predict(nBayes.m8, testData[,2:11])
testData[,19]
nb.m8<-table(pred_bayes.m8, testData[,19])
nb.m8<-confusionMatrix(nb.m8)
round(nb.m8$overall,4)

nBayes.m9 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,20])))
pred_bayes.m9<-predict(nBayes.m9, testData[,2:11])
testData[,20]
nb.m9<-table(pred_bayes.m9, testData[,20])
nb.m9<-confusionMatrix(nb.m9)
round(nb.m9$overall,4)

nBayes.m10 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,21])))
pred_bayes.m10<-predict(nBayes.m10, testData[,2:11])
testData[,21]
nb.m10<-table(pred_bayes.m10, testData[,21])
nb.m10<-confusionMatrix(nb.m10)
round(nb.m10$overall,4)

#functional

nBayes.f6 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,22])))
pred_bayes.f6<-predict(nBayes.f6, testData[,2:11])
testData[,22]
nb.f6<-table(pred_bayes.f6, testData[,22])
nb.f6<-confusionMatrix(nb.f6)
round(nb.f6$overall,4)


nBayes.f7 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,23])))
pred_bayes.f7<-predict(nBayes.f7, testData[,2:11])
testData[,23]
nb.f7<-table(pred_bayes.f7, testData[,23])
nb.f7<-confusionMatrix(nb.f7)
round(nb.f7$overall,4)

nBayes.f8 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,24])))
pred_bayes.f8<-predict(nBayes.f8, testData[,2:11])
testData[,24]
nb.f8<-table(pred_bayes.f8, testData[,24])
nb.f8<-confusionMatrix(nb.f8)
round(nb.f8$overall,4)

nBayes.f9 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,25])))
pred_bayes.f9<-predict(nBayes.f9, testData[,2:11])
testData[,25]
nb.f9<-table(pred_bayes.f9, testData[,25])
nb.f9<-confusionMatrix(nb.f9)
round(nb.f9$overall,4)

nBayes.f10 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,26])))
pred_bayes.f10<-predict(nBayes.f10, testData[,2:11])
testData[,26]
nb.f10<-table(pred_bayes.f10, testData[,26])
nb.f10<-confusionMatrix(nb.f10)
round(nb.f10$overall,4)

nBayes.f11 <-naiveBayes(trainData[,2:11],(as.factor(trainData[,23])))
pred_bayes.f11<-predict(nBayes.f11, testData[,2:11])
testData[,23]
nb.f11<-table(pred_bayes.f11, testData[,23])
nb.f11<-confusionMatrix(nb.f11)
round(nb.f11$overall,4)

boxplot(temp.data[which(fit.mclust8$classification==1),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==2),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==3),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==4),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==5),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==6),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==7),], ylim=c(-0.5,0.5))
boxplot(temp.data[which(fit.mclust8$classification==8),], ylim=c(-0.5,0.5))

temp.data[which(fit.mclust8$classification==1),1]
temp.data[which(fit.mclust8$classification==2),1]
temp.data[which(fit.mclust8$classification==3),1]


#군집별 그래프
par(mfrow=c(4,2))
for (i in 1:8){
  temp<-temp.data[which(fit.mclust8$classification==i),2:25]
  m.temp<-colMeans(temp)
  l.temp<-h.temp<-rep(0,24)
  for (j in 1:24)
  {
    l.temp[j]<-quantile(temp[,j])[2]
    h.temp[j]<-quantile(temp[,j])[4]
  }
  
  plot(m.temp,xaxt="n", type="l",ylim=c(-0.2, 0.4), main=paste("Cluster:",i),
       ylab="Electricity demand", xlab="Hours of day") 
  polygon(c(1:24,24:1),c(l.temp,rev(h.temp)),col="grey75")
  abline(v=1,lty=2, col="darkgrey") 
  abline(v=4,lty=2, col="darkgrey")
  abline(v=7,lty=2, col="darkgrey")
  abline(v=12,lty=2, col="darkgrey")
  abline(v=14,lty=2, col="darkgrey")
  abline(v=18,lty=2, col="darkgrey") 
  abline(v=22,lty=2, col="darkgrey") 
  abline(v=24,lty=2, col="darkgrey")
  axis(1, at=1,lty=2, col="darkgrey")
  axis(1, at=4,lty=2, col="darkgrey")
  axis(1, at=7,lty=2, col="darkgrey")
  axis(1, at=12,lty=2, col="darkgrey")
  axis(1, at=14,lty=2, col="darkgrey")
  axis(1, at=18,lty=2, col="darkgrey")
  axis(1, at=22,lty=2, col="darkgrey")
  axis(1, at=24,lty=2, col="darkgrey")
  lines(m.temp, type="l", col=1, ylim=c(-40, 60),lwd=2) 
}



temp.data$holiday<-x$holiday1
temp.data$weekdays<-x$weekday
temp.data$lowtemp<-x$lowtemp1
temp.data$average.temp<-x$average.temp1
temp.data$hight.temp<-x$high.temp1

temp.data$LoadDay  [which(fit.mclust8$classification==1)]
temp.data$LoadDay  [which(fit.mclust8$classification==2)]
temp.data$LoadDay  [which(fit.mclust8$classification==3)]
temp.data$LoadDay  [which(fit.mclust8$classification==4)]
temp.data$LoadDay [which(fit.mclust8$classification==5)]
temp.data$LoadDay  [which(fit.mclust8$classification==6)]
temp.data$LoadDay [which(fit.mclust8$classification==7)]
temp.data$LoadDay [which(fit.mclust8$classification==8)]

mean(temp.data$average.temp  [which(fit.mclust8$classification==4)])
