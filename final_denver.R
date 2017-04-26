setwd('C:/Users/Fiona/Desktop/data-mining-final-project')
input <- read.csv('denver.csv')

input_std <- data.frame(scale(input[,-c(1,2)]))


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


### K means 
#set.seed(45)
# denver_cluster <- kmeans(input[,-c(1:3)], 5, nstart = 20)

library(cluster)
library(apcluster)


#denver_cluster <- hclust(dist(input_std[,-1]), method = 'average')

#denver_cluster <- cluster::agnes(input_std[,-1], method = "ward")

denver_cluster <- apcluster::apcluster(apcluster::negDistMat(r=2), input_std[,-1])
plot(denver_cluster, input_std[,-1])
  
mem_cluster <- cutree(denver_cluster, 5)

#mem_cluster <- denver_cluster$cluster
mem_cluster <- as.factor(rev(mem_cluster))
mem_actual <- groups(input$Total.Risk.Factor, 5)

# confusion matrix 
tt <- table(mem_actual, mem_cluster)
tt

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

diag_sum(tt)








### affinity propagation 
library(apcluster)




### Ward clustering
dist_matrix <- dist(input_std[,-1])
denver_ward <- hclust(dist_matrix)



## spectral clustering
library(kknn)
library(kernlab)
