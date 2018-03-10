#####################################################################################
### Project : Lifecycle Trend Analysis
### Script  : Clustering_lifecycle_trend.R
### Description : Cluster Similar customers based on their revenue over time
#####################################################################################

# Load data
rev_data           <- read.csv(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLIJHabBRuQO.csv"))
rev_data$end_month <- as.Date(paste0(rev_data$end_month, "-01")) # Convert to date format

# Load library  
pkgs <- c("dplyr", "tidyr", "ggplot2", "lubridate", "factoextra")
sapply(pkgs, require, character.only = T)


#####################################################################################
### Clustering by calculating summary metrics
#####################################################################################

# Step 1: Quantify the shape of the time series by calculating descriptive metrics
#   1. growing_months - number of months when revenue increased
#   2. peak_months - number of months with > 70% revenue
#   3. mean_rev - mean revenue across all time points
#   4. mean_growth - mean monthly growth
#   5. last_12m_growth - growth over the last 12 months
#   6. current_rev - revenue for the most recent month
rev_data_met <- rev_data %>% group_by(primary_account_id) %>% arrange(end_month) %>%
                mutate(up_months = ifelse(rev > lag(rev, default = 0), 1, 0),
                       peak_months = ifelse(rev > 0.7, 1, 0),
                       growth_1m = rev - lag(rev, 1, default = 0),
                       growth_12m = rev - lag(rev, 12, default = 0)) %>%
                summarise(current_rev = last(rev),
                          mean_rev = mean(rev),
                          mean_growth = mean(growth_1m),
                          peak_months = sum(peak_months),
                          growing_months = sum(up_months),
                          last_12m_growth = last(growth_12m)) %>% 
                ungroup()

# Scale data for clustering
rev_data_met <- rev_data_met %>% dplyr::mutate_at(vars(-primary_account_id), scale)

# Step 2: Determine the number of clusters to select using WSSE
fviz_nbclust(rev_data_met[, -1], kmeans, method = "wss")
k <- 9 # Let's use 9 clusters for the sake of demonstration

# Step 3: Find clusters using K-Means Clustering
set.seed(1234)
cluster_fit  <- kmeans(rev_data_met[, -1], k)
rev_data_met <- rev_data_met %>% dplyr::mutate(cluster = as.factor(cluster_fit$cluster)) %>%
                dplyr::select(primary_account_id, cluster)

# Step 4: Join cluster numbers to orginal dataframe
rev_data <- rev_data %>% inner_join(rev_data_met, by = "primary_account_id")

# Step5: Plot clusters
ggplot(rev_data, aes(end_month, rev, color = cluster, group = primary_account_id)) +
  geom_line(alpha = 0.5) + facet_wrap(~ cluster) +
  theme_bw() + theme(legend.position = "none") +
  geom_smooth(aes(group = cluster), method = "loess", se = F, size = 2, span = 0.2)
