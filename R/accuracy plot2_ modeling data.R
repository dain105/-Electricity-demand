plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="Random forest",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))
#gmm 
points(9,1.0,pch=17, col=8,type="p", cex=3)
points(7,1.0,pch=17, col=8,type="p", cex=3)
points(8,1.0,pch=17, col=8,type="p", cex=3)
points(10,1.0,pch=17, col=8,type="p", cex=3)

# k-means
points(7,1.0,pch=20,type="p", cex=3)
points(8,1.0,pch=20,type="p", cex=3)
points(9,1.0,pch=20,type="p", cex=3)
points(10,1.0,pch=20,type="p", cex=3)

#f 
points(7,1.0,pch=8,type="p", cex=3)
points(8,1.0,pch=8,type="p", cex=3)
points(9,1.0,pch=8,type="p", cex=3)
points(10,1.0,pch=8,type="p", cex=3)






plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="Decision tree",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))
#gmm
points(7,0.828,pch=17, col=8,type="p", cex=3)
points(8,0.834,pch=17, col=8,type="p", cex=3)
points(9,0.813,pch=17, col=8,type="p", cex=3)
points(10,0.795,pch=17, col=8,type="p", cex=3)

#k
points(7,0.811,pch=20,type="p", cex=3)
points(8,0.806,pch=20,type="p", cex=3)
points(9,0.761,pch=20,type="p", cex=3)
points(10,0.769,pch=20,type="p", cex=3)

#f
points(7,0.819,pch=8,type="p", cex=3)
points(8,0.788,pch=8,type="p", cex=3)
points(9,0.774,pch=8,type="p", cex=3)
points(10,0.785,pch=8,type="p", cex=3)




plot(0,type="n",ylim=c(0.0,1),xlim=c(6.5,10.5),main="SVM",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))

#gmm
points(7,0.8,pch=17, col=8,type="p", cex=3)
points(8,0.840,pch=17, col=8,type="p", cex=3)
points(9,0.800,pch=17, col=8,type="p", cex=3)
points(10,0.798,pch=17, col=8,type="p", cex=3)

#k
points(7,0.821,pch=20,type="p", cex=3)
points(8,0.808,pch=20,type="p", cex=3)
points(9,0.771,pch=20,type="p", cex=3)
points(10,0.787,pch=20,type="p", cex=3)


#f
points(7,0.807,pch=8,type="p", cex=3)
points(8,0.810,pch=8,type="p", cex=3)
points(9,0.792,pch=8,type="p", cex=3)
points(10,0.799,pch=8,type="p", cex=3)


plot(0,type="n",ylim=c(0.,1),xlim=c(6.5,10.5),main="Naive bayes",xlab="The number of cluster", ylab="Accuracy")
grid()
legend(x=6.4, y=0.3,cex=1.1, pt.cex =2,
       c("Gaussian mixture model", "k-means clustering", "Functional clustering"),
       col=c(8,1,1),
       pch=c(17,20,8))

#k
points(7,0.680,pch=17, col=8,type="p", cex=3)
points(8,0.651,pch=17, col=8,type="p", cex=3)
points(9,0.517,pch=17, col=8,type="p", cex=3)
points(10,0.405,pch=17, col=8,type="p", cex=3)

#gmm
points(7,0.666,pch=20,type="p", cex=3)
points(8,0.667,pch=20,type="p", cex=3)
points(9,0.637,pch=20,type="p", cex=3)
points(10,0.668,pch=20,type="p", cex=3)



#f
points(7,0.625,pch=8,type="p", cex=3)
points(8,0.556,pch=8,type="p", cex=3)
points(9,0.507,pch=8,type="p", cex=3)
points(10,0.576,pch=8,type="p", cex=3)





















 plot(0,type="n",ylim=c(0.4,1),xlim=c(6.5,10.5),main="Naive bayes",xlab="cluster", ylab="Accuracy")

#k
points(7,0.626,pch=20,type="p", cex=3)

#gmm
points(8,0.678,pch=17, col=8,type="p", cex=3)


#f
points(9,0.533,pch=8,type="p", cex=3)
