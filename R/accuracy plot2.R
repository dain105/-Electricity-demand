plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="Random forest",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))
#gmm 
points(9,0.792,pch=17, col=8,type="p", cex=3)
points(7,0.754,pch=17, col=8,type="p", cex=3)
points(8,0.866,pch=17, col=8,type="p", cex=3)
points(10,0.765,pch=17, col=8,type="p", cex=3)

# k-means
points(7,0.817,pch=20,type="p", cex=3)
points(8,0.730,pch=20,type="p", cex=3)
points(9,0.790,pch=20,type="p", cex=3)
points(10,0.746,pch=20,type="p", cex=3)

#f 
points(7,0.779,pch=8,type="p", cex=3)
points(8,0.760,pch=8,type="p", cex=3)
points(9,0.768,pch=8,type="p", cex=3)
points(10,0.738,pch=8,type="p", cex=3)






plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="Decision tree",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))
#gmm
points(7,0.762,pch=17, col=8,type="p", cex=3)
points(8,0.822,pch=17, col=8,type="p", cex=3)
points(9,0.773,pch=17, col=8,type="p", cex=3)
points(10,0.768,pch=17, col=8,type="p", cex=3)
#k
points(7,0.795,pch=20,type="p", cex=3)
points(8,0.686,pch=20,type="p", cex=3)
points(9,0.754,pch=20,type="p", cex=3)
points(10,0.680,pch=20,type="p", cex=3)

#f
points(7,0.795,pch=8,type="p", cex=3)
points(8,0.746,pch=8,type="p", cex=3)
points(9,0.730,pch=8,type="p", cex=3)
points(10,0.732,pch=8,type="p", cex=3)




plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="SVM",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))

#gmm
points(7,0.683,pch=17, col=8,type="p", cex=3)
points(8,0.724,pch=17, col=8,type="p", cex=3)
points(9,0.661,pch=17, col=8,type="p", cex=3)
points(10,0.689,pch=17, col=8,type="p", cex=3)
#k
points(7,0.751,pch=20,type="p", cex=3)
points(8,0.645,pch=20,type="p", cex=3)
points(9,0.710,pch=20,type="p", cex=3)
points(10,0.615,pch=20,type="p", cex=3)
#f
points(7,0.700,pch=8,type="p", cex=3)
points(8,0.637,pch=8,type="p", cex=3)
points(9,0.672,pch=8,type="p", cex=3)
points(10,0.656,pch=8,type="p", cex=3)


plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="Naive bayes",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))
#gmm
points(7,0.593,pch=17, col=8,type="p", cex=3)
points(8,0.678,pch=17, col=8,type="p", cex=3)
points(9,0.601,pch=17, col=8,type="p", cex=3)
points(10,0.628,pch=17, col=8,type="p", cex=3)

#k
points(7,0.626,pch=20,type="p", cex=3)
points(8,0.585,pch=20,type="p", cex=3)
points(9,0.571,pch=20,type="p", cex=3)
points(10,0.511,pch=20,type="p", cex=3)



#f
points(7,0.628,pch=8,type="p", cex=3)
points(8,0.568,pch=8,type="p", cex=3)
points(9,0.533,pch=8,type="p", cex=3)
points(10,0.527,pch=8,type="p", cex=3)































plot(0,type="n",ylim=c(0.4,1),xlim=c(6.5,10.5),main="Naive bayes",xlab="cluster", ylab="Accyracy")

#k
points(7,0.626,pch=20,type="p", cex=3)

#gmm
points(8,0.678,pch=17, col=8,type="p", cex=3)


#f
points(9,0.533,pch=8,type="p", cex=3)
