library(datasets)
library(ggplot2)
require(gridExtra)
data(iris)
summary(iris)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

iris[,1:4] <- normalize(iris[,1:4])

plot1 <- ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
plot1
plot2 <- ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
plot2
grid.arrange(plot1, plot2, ncol=2)


cluster <- kmeans(iris[,1:4],2, nstart = 100)
cluster$cluster

par(mar = rep(0.2,4))
plot(iris$Sepal.Length, iris$Sepal.Width, col = cluster$cluster, pch = 1, cex = 2)
points(cluster$centers, col = 1:3, pch =3, cex = 3, lwd =3)


plot1 <- ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= cluster$cluster)) + geom_point() 
plot1

set.seed(200)
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(iris[,1:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

cluster <- kmeans(iris[,1:4],3,nstart = 100)
table(cluster$cluster,iris$Species)

plot1 <- ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= cluster$cluster)) + geom_point() 
plot1

plot2 <- ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= cluster$cluster)) + geom_point() 
plot2

grid.arrange(plot1, plot2, ncol=2)



