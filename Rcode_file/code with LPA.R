library(mclust)
library(tidyLPA)
library(tidyverse)
library(haven)

# 载入数据
data <- read_sav("C:/Users/14946/Downloads/Midus1_Squad_Data_USJAPANwellbeing.sav")%>%
  dplyr::select("Zcommunion","ZMyMidus1Autonomy","QCL_1")%>%
  na.omit()

k <- 4
kmeans_result <- kmeans(data, centers = k, nstart = 25)
WCSS <- sum(kmeans_result$withinss)
n <- nrow(data)
d <- ncol(data)
num_parameters <- k * d  # 每个中心d个参数

# 计算BIC，正确的公式
BIC_value <- log(n) * num_parameters + WCSS
print(BIC_value)

# 使用tidyLPA拟合和选择最佳的潜在剖面模型
# 这里我们尝试1到5个潜在类别，使用mclust包的1到3号模型
model <- data %>%
  dplyr::select("Zcommunion","ZMyMidus1Autonomy")%>%
  estimate_profiles(n_profiles = 4, models = 1:3, package = "mclust")
print(model)
summary(model)

# 可视化聚类结果
clustering<-get_data(model)%>%
  mutate(Old_Class=data$QCL_1)%>%
  mutate(New_class=new_class)

ari <- adjustedRandIndex(clustering$Old_Class, clustering$New_class)
print(paste("Adjusted Rand Index:", ari))

# 使用bootstrap方法估算ARI的95%置信区间
set.seed(123)  # 为了结果可重复
bootstrap_ari <- function(data, n_bootstrap = 1000) {
  ari_samples <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_indices <- sample(nrow(data), replace = TRUE)
    ari_samples[i] <- adjustedRandIndex(data$cluster_kmeans[sample_indices], 
                                        data$cluster_lpa[sample_indices])
  }
  return(quantile(ari_samples, c(0.025, 0.975)))
}

ci_ari <- bootstrap_ari(data)
print(ci_ari)

# 计算总体密度
overall_density <- density(clustering$ZMyMidus1Autonomy)
max_density <- max(overall_density$y)

# 绘制图表
ggplot(clustering, aes(x =ZMyMidus1Autonomy ,y = after_stat(density))) +
  # 添加每个类别的密度曲线
  geom_density(aes(fill = as.factor(Old_Class)), alpha = 0.5) +
  # 添加总体密度曲线
  geom_line(data = data.frame(x = overall_density$x, y = overall_density$y),
            aes(x = x, y = y), color = "black", size = 0.5) +
  # 设置图例标题
  # 设置图表标题和坐标轴
  labs(title = "Normalized Density Plot of MyMidus1Autonomy by Class with Overall Distribution",
       x = "Zcommunion", y = "Normalized Density", fill = "Class Old") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
