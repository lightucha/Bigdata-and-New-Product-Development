#####################################################################################
### Project : Cluster Analysis
### Script : Clustering.R
### Description : Demonstrate how to estimate the number of clusters
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("ecodist", "factoextra", "vegan", "mclust", "apcluster", "cluster", "NbClust")
sapply(pkgs, require, character.only = T)

# Generate random data
n <- 100
g <- 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))

# Visualize the random data
plot(d, main = "How many cluster do you see?")


#####################################################################################
### Hierarchical Clustering
#####################################################################################

# With choice of distance/agglomerative measures
m.dist <- "euclidean"   # euclidean / manhattan / mahalanobis / ...
m.aggr <- "complete"      # single / complete / average / median / centroid / ward.D 
fit.h  <- hclust(if(m.dist == "mahalanobis") distance(d) else dist(d, method = m.dist), 
                 method = m.aggr)
plot(fit.h)


#####################################################################################
### Estimate the number of clusters
#####################################################################################

# Method 1. Within Sum of Squared Error (W-SSE)
fviz_nbclust(d, kmeans, method = "wss")
plot(d, col = kmeans(d, 4)$cluster, main = "4 Clusters") # plot 4 groups

# Method 2. Silhouette (-1 < within + between < 1)
fviz_nbclust(d, kmeans, method = "silhouette")

# Method 3. Calinski-Harabasz index (pseudo F-stat)
fit <- cascadeKM(scale(d, center = T,  scale = T), 1, 10, iter = 1000)
plot(fit, sortg = T, grpmts.plot = T)
plot(d, col = fit$partition[, 5], main = "5 Clusters") # plot 5 groups

# Method 4. BIC (Bayesian Information Criterion)
plot(Mclust(as.matrix(d), G = 1:20))

# Method 5. Afinity Propagation (AP)
d.apclus <- apcluster(negDistMat(r = 2), d)
plot(d.apclus, d, main = "Afinity Propagation: 4 Clusters")

# Method 6. Gap analysis
clusGap(d, kmeans, 10, B = 100)

# All in One
NbClust(d, method = "kmeans", index = "alllong")
