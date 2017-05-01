# Setting the working directory
setwd('C:/Users/Fiona/Desktop/data-mining-final-project')

# Reading the Workbook of region-wise Safeway data 
library(XLConnect)
wb.region <- loadWorkbook("C:/Users/Fiona/Desktop/DataMining1/University of Wyoming Risk Reports_Transformed Data.xlsx")
lst = readWorksheet(wb.region, sheet = getSheets(wb.region))

# Getting the names of all regions
sheet_names <-  getSheets(wb.region)
names(sheet_names) <- sheet_names

# Getting the data from each region
sheet_list <- lapply(sheet_names, function(.sheet) {
  readWorksheet(object = wb.region, .sheet)
})

# Function to group rows into clusters
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

# Function to group rows into clusters
# transformed
groups_transformed <- function(y, n, lambda) {
  x <- (y^lambda - 1) / lambda
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

# get membership from cluster
membership <- function(x) {
  mem <- numeric(0)
  n <- length(x)
  for (i in 1:n) {
    mem[x[[i]]] <- i
  }
  return(as.factor(mem))
}

# For each region in the workbook
i=1
# Dataframe to store results
result <- data.frame("KMeans" = integer(), "Hierarchical" = integer(), "Ward" = integer(), "Affinity" = integer())
for (sheet in sheet_list) {
  # Loading the current spreadsheet
  input <- sheet
  input_std <- data.frame(scale(input[,-c(1,2)]))
  
  # Actual Cluster
  mem_actual <- groups(input$Total.Risk.Factor, 5)
  
  # K-Means Clustering
  set.seed(45)
  kmeans_cluster <- kmeans(input_std[,-1], 5, nstart = 20)
  mem_cluster <- kmeans_cluster$cluster
  mem_cluster <- as.factor(rev(mem_cluster))
  # confusion matrix 
  tt <- table(mem_actual, mem_cluster)
  kresult <- diag_sum(tt)
  
  # Hierarchical Clustering 
  hierarchical_cluster <- hclust(dist(input_std[,-1]), method = 'average')
  mem_cluster <- cutree(hierarchical_cluster, 5)
  mem_cluster <- as.factor(rev(mem_cluster))
  # confusion matrix 
  tt <- table(mem_actual, mem_cluster)
  hresult <- diag_sum(tt)
  
  # Ward Clustering
  library(cluster)
  ward_cluster <- cluster::agnes(input_std[,-1], method = "ward")
  mem_cluster <- cutree(ward_cluster, 5)
  mem_cluster <- as.factor(rev(mem_cluster))
  # confusion matrix 
  tt <- table(mem_actual, mem_cluster)
  wresult <- diag_sum(tt)
  
  # affinity propagation
  library(apcluster)
  ap28 <- apclusterK(negDistMat(r=2), input[,-c(1:3)], K=5)
  # get membership from clustering
  mem_cluster <- membership(ap28@clusters)
  ##### affinity propagation based on transformed response variable ####
  # total risk factor is transformed based on a Box Cox transformation
  # lambda is chosen to be -4 based on the Box Cox plot
  library(MASS)
  boxcox(input[,3]~1, lambda = seq(-10,4,.1))
  lambda <- -4 # based on Box Cox transformation
  mem_actual <- groups_transformed(input[,3], 5, lambda=lambda)
  # confusion matrix 
  tt <- table(mem_actual, mem_cluster)
  aresult <- diag_sum(tt)
  
  # Appending the current row of accuracy scores into the result dataframe
  newrow <- data.frame("KMeans" = kresult, "Hierarchical" = hresult, "Ward" = wresult, "Affinity" = aresult)
  row.names(newrow) <- sheet_names[i]
  result <- rbind(result, newrow)
  
  i = i+1
}

# The resultant dataframe with all the accuracy scores
result
write.csv(result, file = "result.csv")







