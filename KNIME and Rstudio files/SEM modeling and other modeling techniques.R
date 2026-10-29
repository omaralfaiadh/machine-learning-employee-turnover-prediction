library(lavaan)
library(readxl)
library(ggplot2)
#calling dta
data <- read.csv("D:/Capastone Project/submission/cleaned capstone data.csv")
head(data)
row.names(data)
#resource: https://turingbotsoftware.com/blog/symbolic-regression-r-example/
data$idx <- as.numeric(row.names(data))
# Define the equation function
eq <- function(row) {
  2966.96 + (2.98602 * (-55.4604 + row) * cos(0.0397268 * (row + 8.34129 * cos(-0.0819996 * row)) - 1.16301 * cos(-0.0358919 * row)))
  }
# Create the ggplot object
p <- ggplot(data, aes(x = idx, y = Close)) +
  geom_point() +
  stat_function(fun = eq, color = 'blue')
# Display the plot
print(p)
# Save the plot as a PNG file
# png("test.png") # print(p)
# dev.off()


#SEM modeling
colnames(data)
sem_model <- '
  has_left_company ~ satisfaction_level + last_performance_rating +
                     number_of_projects + avg_monthly_hours + 
                     years_at_company + had_work_accident + 
                     promoted_in_last_5_years + Department + salary_level
'


# Fitting the SEM model
fit <- sem(sem_model, data = data)

# Summarizing results
summary(fit, fit.measures = TRUE, standardized = TRUE)

#Plotting
library(OpenMx)
library(semPlot)
semPaths(fit, "std", layout = "tree", edge.label.cex = 1.2, curvePivot = TRUE)




#CFA
#Source of codes: https://www.geeksforgeeks.org/confirmatory-factor-analysis-in-r/


# Specify the CFA model
model <- '
    work_engagement =~ satisfaction_level + last_performance_rating + number_of_projects
    workload_commitment =~ avg_monthly_hours + years_at_company + had_work_accident
    career_growth_rewards =~ promoted_in_last_5_years + salary_level
'

#Run CFA
cfa_result <- cfa(model, data = data)

# Summary of the model
summary(cfa_result)

# Plotting CFA
semPaths(cfa_result, what="std", layout="tree", edge.label.cex=1.2, fade=FALSE)









#k-means
#source: https://www.geeksforgeeks.org/k-means-clustering-in-r-programming/

#installing pakages and calling libraries
install.packages("ClusterR")
install.packages("cluster")
library(ClusterR)
library(cluster)

hr_data <- data[, c("satisfaction_level", "last_performance_rating", 
                    "number_of_projects", "avg_monthly_hours", 
                    "years_at_company", "had_work_accident", 
                    "promoted_in_last_5_years")]

hr_data_scaled <- scale(hr_data)


wss <- sapply(1:10, function(k){
  kmeans(hr_data_scaled, centers = k, nstart = 20, iter.max = 300)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (K)", ylab = "Total Within Sum of Squares")


set.seed(123) 
kmeans_result <- kmeans(hr_data_scaled, centers = 3, nstart = 20)

data$cluster <- as.factor(kmeans_result$cluster)

library(ggplot2)

# Perform PCA
pca <- prcomp(hr_data_scaled, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca$x[, 1:2], Cluster = data$cluster)

# Plot Clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(title = "K-Means Clustering (PCA Projection)",
       x = "Principal Component 1", 
       y = "Principal Component 2") +
  theme_minimal()

