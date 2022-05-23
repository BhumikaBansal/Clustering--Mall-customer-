# Mall customer dataset
# Segmentation / Grouping / Clustering problem

dataset <- read.csv(choose.files())
head(dataset)
str(dataset)
summary(dataset)
dim(dataset)
colSums(is.na(dataset))
dataset1 <- dataset[,4:5]
head(dataset1)

colSums(is.na(dataset1))
# we have to use elbow method to find the best k value

set.seed(123)
wcss = vector()
for (i in 1:15) wcss[i] = sum(kmeans(dataset1, i)$withinss)
wcss
plot(1:15, 
     wcss, 
     type='b', 
     main = paste("The Elbow Method"),
     xlab = "Number of Clusters", ylab = 'wcss')

# k = 5

set.seed(123)
kmeans = kmeans(x = dataset1, centers = 5)
y_kmeans = kmeans$cluster
y_kmeans


install.packages("cluster")# has clusplot and silhoutte
library(cluster)

clusplot(dataset1,y_kmeans, lines = 0,
         shade = T, color = T, 
         main = paste("Cluster of Customer basis Income and score"),
         xlab="Annual Income",
         ylab = "Spending Score of Credit Card")


# adding the cluster data into original dataset

getwd()
dataset <- cbind(dataset, y_kmeans)
write.csv(dataset, "best_customer_details.csv")


##########################################
# Hierarchical Cluster

dendrogram = hclust(d= dist(dataset1, method = 'euclidean'),
                    method = 'ward.D')

plot(dendrogram, main = paste("Dendrogram for HCluster"),
     xlab ="Customer Details",
     ylab = "Euclidean Distance")


hc = hclust(d = dist(dataset1, method = 'euclidean'), 
            method = 'ward.D')
y_hc = cutree(hc, 5)
y_hc

clusplot(dataset1,y_hc, lines = 0,
         shade = T, color = T, 
         main = paste("Cluster of Customer basis Income and score"),
         xlab="Annual Income",
         ylab = "Spending Score of Credit Card")

hc_cbind <- cbind(dataset, y_hc)
write.csv(hc_cbind, "best_customer_details1.csv")


  







