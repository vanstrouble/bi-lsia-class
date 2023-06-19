library(cluster.datasets)
data(life.expectancy.1971)
set.seed(13579)

km <- kmeans(life.expectancy.1971[,c(4,8)],4)
km
plot(life.expectancy.1971$m50, life.expectancy.1971$f50, asp=1, pch=km$cluster)

plot(life.expectancy.1971$m25, life.expectancy.1971$f25, asp=1, pch=km$cluster, main="clusters", xlab="m25", ylab="f25")
points(km$centers[,2], km$centers[,1], pch=23, col="maroon", bg="lightblue", cex=3)
text(km$centers[,2], km$centers[,1], cex=1.1, col="black", attributes(km$centers)$dimnames[[1]])
