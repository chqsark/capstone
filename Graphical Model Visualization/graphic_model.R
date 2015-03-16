# Rui Wang
# Below is the code for construct and plot the graphic in R
# please install and loading the necessary library before running this code
# this relevatnt packages include gRbase, Rgraphviz, RBGL

test<-as.matrix(closingPriceSmoothResult[-1,-1])

test[cbind(row(test)[which(!test==0)],col(test)[which(!test==0)])]<-1

number<-seq(1, 476, by=1)

rownames(test)<-colnames(test)<-number

dg<-as(test,"graphNEL")

plot(dg)
