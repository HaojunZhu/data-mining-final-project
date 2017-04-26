setwd('//studentfiles/storage$/Desktop/data mining final project')
input <- read.csv('denver.csv')

input_std <- data.frame(scale(input[,-c(1,2)]))

x2 <- input[,-c(1:3)]


library(apcluster)
x2 <- input_std[,-1]
s1 <- negDistMat(input_std[,-1], r=2)
a1 <- apclusterK(s1, as.matrix(x2), K = 5)
plot(a1, input_std[,-1])


a2 <- apclusterK(negDistMat(r=2), x2, K=5, details = T)
plot(a2, x2[,c(3:4)])

points(x2[,3], x2[,4], col = mem_actual, pch = 19)
legend('topleft', col = c(1:5), legend = c(1:5), pch = 19)


com



fea <- names(input)[-c(1:3)]
tp <- combn(fea, 2)


pdf("plot.pdf", width = 5000, height = 5000)
par(mfrow=c(20,20))
for (i in 1:dim(tp)[2]) {
  #pdf(paste('plot', i, '.pdf', sep=''))
  plot(a2, x2[tp[,i]], xlab = tp[1,i], ylab = tp[2,i])
  points(x2[tp[,i]], col = mem_actual, pch = 19)
  #dev.off()
}
par(mfrow=c(1,1))
dev.off()


# sum up diagonal elements with offsets 
diag_sum <- function(M) {
  s <- sum(diag(M))
  for (i in 1:dim(M)[1]) {
    if (i == 1) {
      s <- s + M[i,i+1]
    } else if (i == dim(M)[1]) {
      s <- s + M[i,i-1]
    } else {
      s <- s + M[i,i+1] + M[i,i-1]
    }
  }
  return(s)
}

# calculate n groups
groups <- function(x, n) {
  bin.size <- diff(range(x)) / n
  cutoffs <- min(x) + c(0:n)*bin.size
  membership <- character()
  for (i in 1:length(x)) {
    for (j in 2:length(cutoffs)) {
      if (x[i] >= cutoffs[j-1] & x[i] <= cutoffs[j]) {
        membership[i] <- as.character(j-1)
      } 
    }
  }
  return(as.factor(membership))
}









fea <- names(input_std)[-1]
tp <- combn(fea, 8)



dim(tp)[2]

combn(fea,5) -> aa


mem_actual <- groups(input$Total.Risk.Factor, 5)
rpt <- 10 #run k-means clustering 10 times
accu <- matrix(nrow = dim(tp)[2], ncol = rpt)

for (i in 1:dim(tp)[2]) {
  for (j in 1:rpt) {
    
    temp_cluster <- kmeans(input_std[,tp[,i]], 5)
    tt <- table(mem_actual, temp_cluster$cluster)
    accu[i,j] <- sum(diag(tt)) / 100
  }
}






mem


### K means 
#set.seed(45)
denver_cluster <- kmeans(input[,-c(1:3)], 5, nstart = 20)

mem_cluster <- denver_cluster$cluster
mem_cluster <- as.factor(rev(mem_cluster))
mem_actual <- groups(input$Total.Risk.Factor, 5)

# confusion matrix 
tt <- table(mem_actual, mem_cluster)
tt


diag_sum(tt)








### affinity propagation 
library(apcluster)




### Ward clustering
dist_matrix <- dist(input_std[,-1])
denver_ward <- hclust(dist_matrix)



## spectral clustering
library(kknn)
library(kernlab)
