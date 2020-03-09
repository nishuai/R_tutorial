library(ggplot2)

##########################random points
aa=rnorm(100, 0.5, 0.08)
bb=rnorm(100, 0.5, 0.08)
plot(aa, bb, ylim=c(0,1), xlim=c(0,1))

cc=rnorm(100, 0.2, 0.03)
dd=rnorm(100, 0.8, 0.09)
     
points(cc, dd)

ee=rnorm(100, 0.2, 0.1)
ff=rnorm(100, 0.2, 0.1)

points(ee, ff)

df=data.frame(xx=c(aa, cc, ee), yy=c(bb, dd, ff))
df$class=rep(c('A','B','C'), each=100)

p=ggplot(df, aes(xx, yy))+geom_point(aes(color=class))
p

################## K means 
### Init: defining 4 points
mean_points=data.frame(matrix(rnorm(8, 0.4, 0.2), 4, 2))
mean_points$label=c('A','B','C','D')
p+geom_text(data =mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='black', size=5)


##########assining points to the nearest mean
distance=matrix(NA, 300, 4)
distance[,1]=((df$xx-mean_points[1,1])^2+ (df$yy-mean_points[1,2])^2)^0.5
distance[,2]=((df$xx-mean_points[2,1])^2+ (df$yy-mean_points[2,2])^2)^0.5
distance[,3]=((df$xx-mean_points[3,1])^2+ (df$yy-mean_points[3,2])^2)^0.5
distance[,4]=((df$xx-mean_points[4,1])^2+ (df$yy-mean_points[4,2])^2)^0.5

###########assining points to its nearest means
assignment=apply(distance, 1, function(x) which.min(x))
df$class=as.factor(assignment)

##########calculate means

new_mean_points=mean_points
new_mean_points[,1]=aggregate(df[,1], by=list(df$class), FUN=mean)$x
new_mean_points[,2]=aggregate(df[,2], by=list(df$class), FUN=mean)$x

p+geom_text(data =mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='black', size=9)+
    geom_text(data =new_mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='brown1', size=7)


######################################################################################
## Update mean_points
mean_points=new_mean_points

#########assining points to the nearest mean
distance=matrix(NA, 300, 4)
distance[,1]=((df$xx-mean_points[1,1])^2+ (df$yy-mean_points[1,2])^2)^0.5
distance[,2]=((df$xx-mean_points[2,1])^2+ (df$yy-mean_points[2,2])^2)^0.5
distance[,3]=((df$xx-mean_points[3,1])^2+ (df$yy-mean_points[3,2])^2)^0.5
distance[,4]=((df$xx-mean_points[4,1])^2+ (df$yy-mean_points[4,2])^2)^0.5

###########
assignment=apply(distance, 1, function(x) which.min(x))
df$class=as.factor(assignment)

####compare new means to previous means
new_mean_points=mean_points
new_mean_points[,1]=aggregate(df[,1], by=list(df$class), FUN=mean)$x
new_mean_points[,2]=aggregate(df[,2], by=list(df$class), FUN=mean)$x


p+geom_text(data =mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='black', size=9)+
    geom_text(data =new_mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='brown1', size=7)

#####################################################################################

Update_means=function(){
    #########assining points to the nearest mean
    distance[,1]=((df$xx-mean_points[1,1])^2+ (df$yy-mean_points[1,2])^2)^0.5
    distance[,2]=((df$xx-mean_points[2,1])^2+ (df$yy-mean_points[2,2])^2)^0.5
    distance[,3]=((df$xx-mean_points[3,1])^2+ (df$yy-mean_points[3,2])^2)^0.5
    distance[,4]=((df$xx-mean_points[4,1])^2+ (df$yy-mean_points[4,2])^2)^0.5
    
    ###########
    assignment=apply(distance, 1, function(x) which.min(x))
    df$class=as.factor(assignment)
    
    ####compare new means to previous means
    new_mean_points[,1]=aggregate(df[,1], by=list(df$class), FUN=mean)$x
    new_mean_points[,2]=aggregate(df[,2], by=list(df$class), FUN=mean)$x
    
    p=ggplot(df, aes(xx, yy))+geom_point(aes(color=class))   
    print(p+geom_text(data =mean_points, mapping = 
                      aes(x =X1, y = X2, label=label), color='black', size=9)+
        geom_text(data =new_mean_points, mapping = 
                      aes(x =X1, y = X2, label=label), color='brown1', size=7))
    
     mean_points <<- new_mean_points
}


####################try it out
mean_points=data.frame(matrix(rnorm(6, 0.4, 0.2), 3, 2))
mean_points$label=c('A','B', 'C')
new_mean_points=mean_points
p+geom_text(data =mean_points, mapping = 
                  aes(x =X1, y = X2, label=label), color='black', size=8)

Update_mean()
