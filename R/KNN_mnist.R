###Load the training and test datasets
# setwd('~/Documents/courses/R4beginners/')
# train <- read.csv("train.csv")
# train=train[sample(1:nrow(train), nrow(train)),]
# train=train[1:3000,]
# write.csv(train, 'train_p1.csv',row.names =FALSE)

train <- read.csv("train_p1.csv")
labels=train$label
train$label=NULL

test=train[1:300,]
train=train[1000:1400,]
labels=labels[1000:1400]
train=as.matrix(train)
test=as.matrix(test)

pic=matrix(train[1,], nrow=28, byrow=TRUE)
pheatmap(pic, cluster_rows = F, cluster_cols = F)
###print 100 2s in the training set
table(labels)
train2s=train[labels==2,][1:12,]
par(mfrow=c(5,5),mar=c(0.1,0.1,0.1,0.1))
for (i in 1:100){
    pic=matrix(train2s[i,], nrow=28, byrow = TRUE)
    image(pic, axes=FALSE)
}
dev.off()

###One nearest neighbour predictions
one_nearest_neighbour=function(test, train){
    test_dists=rep(NA, nrow(train))
    for (i in 1:nrow(train)){
        test_dists[i]=dist(rbind(test, train[i,]))
    }
    image(matrix(test, nrow=28, byrow=TRUE))
    labels[which.min(test_dists)]
}

one_nearest_neighbour(test[11,], train)

###K nearest neighbour predictions
knn=function(test, train, k, showvotes=FALSE){
    test_dists=rep(NA, nrow(train))
    for (i in 1:nrow(train)){
        test_dists[i]=dist(rbind(test, train[i,]))
    }
    image(matrix(test, nrow=28, byrow=TRUE))
    votes=labels[order(test_dists)][1:k]
    if (showvotes==TRUE) print(votes)
    
    majority=sort(table(votes),decreasing=TRUE)[1]
    names(majority)
}

knn(test[19,], train, 10, showvotes = TRUE)
