x<-seq(1:dim(cluster.data)[1])

fit.lm<-lm(cluster.data[,3]~x)

temp.data<-cluster.data

#write.table(temp.data,"E:/electricity.dat",sep=",")


for (coln in 2:25)
  temp.data[,coln]<-cluster.data[,coln]-fit.lm$fitted.values

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
##########################################################

#k means clustering
fit.kmeans8 <- kmeans(elec.pca$scores, 8)
fit.kmeans9 <- kmeans(elec.pca$scores, 9)
fit.kmeans10 <- kmeans(elec.pca$scores, 10)
fit.kmeans11 <- kmeans(elec.pca$scores, 11)




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
########################################################################



#functional clustering

library('MFDA')

# names "BIC", "nclust", "clust", "clust.center
set.seed(124)
r.cluster9<-MFclust(temp.data[,2:25],minG=9, maxG=9, nchain=4, iter.max=8)
r.cluster10<-MFclust(temp.data[,2:25],minG=10, maxG=10, nchain=4, iter.max=8)

r.cluster9<-MFclust(elec.pca$scores,minG=9, maxG=9, nchain=4, iter.max=8)
r.cluster10<-MFclust(elec.pca$scores,minG=10, maxG=10, nchain=4, iter.max=8)

#correlation between climatic information and classfication result
clu.dat<-read.csv("E:/WISE/2015/R-work/energy/data/cluster.csv", header=T)
rpart<-read.csv("E:/WISE/2015/R-work/energy/data/cli_anova.csv")

plot(elec.pca$scores[which(r.cluster9$clust==1),], pch=16, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=9)")
points(elec.pca$scores[which(r.cluster9$clust==2),], pch=17, col=2)
points(elec.pca$scores[which(r.cluster9$clust==3),], pch=18, col=3)
points(elec.pca$scores[which(r.cluster9$clust==4),], pch=19, col=4)
points(elec.pca$scores[which(r.cluster9$clust==5),], pch=20, col=5)
points(elec.pca$scores[which(r.cluster9$clust==6),], pch=21, col=6)
points(elec.pca$scores[which(r.cluster9$clust==7),], pch=22, col=7)
points(elec.pca$scores[which(r.cluster9$clust==8),], pch=23, col=8)
points(elec.pca$scores[which(r.cluster9$clust==9),], pch=24, col=9)

table(r.cluster9$clust)


plot(elec.pca$scores[which(r.cluster10$clust==1),], pch=16, col=1, xlim=c(-4,2.6), ylim=c(-2.5,2.5),
     main="Functional clustering (k=10)")
points(elec.pca$scores[which(r.cluster10$clust==2),], pch=17, col=2)
points(elec.pca$scores[which(r.cluster10$clust==3),], pch=18, col=3)
points(elec.pca$scores[which(r.cluster10$clust==4),], pch=19, col=4)
points(elec.pca$scores[which(r.cluster10$clust==5),], pch=20, col=5)
points(elec.pca$scores[which(r.cluster10$clust==6),], pch=21, col=6)
points(elec.pca$scores[which(r.cluster10$clust==7),], pch=22, col=7)
points(elec.pca$scores[which(r.cluster10$clust==8),], pch=23, col=8)
points(elec.pca$scores[which(r.cluster10$clust==9),], pch=24, col=9)
points(elec.pca$scores[which(r.cluster10$clust==10),], pch=25, col=10)


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



