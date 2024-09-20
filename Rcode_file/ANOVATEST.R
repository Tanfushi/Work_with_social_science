library(mclust)
library(tidyLPA)
library(tidyverse)
library(haven)

# 载入数据
data_us <- Midus1_Squad_Data_USJAPANwellbeing%>%
  dplyr::select("Zcommunion","ZMyMidus1Autonomy","1SIADL")%>%
  na.omit()

data_jp <- MIDJA1_Squad_Data_USJAPANwellbeing_1_%>%
  dplyr::select("Zcommunion","ZMyMIDJA1_Autonomy","MIDJA_IDS")%>%
  na.omit()

model_us <- data_us %>%
  dplyr::select("Zcommunion","ZMyMidus1Autonomy")%>%
  estimate_profiles(n_profiles = 4, models = 2, package = "mclust")
clustering_us<-get_data(model_us)

model_jp <- data_jp %>%
  dplyr::select("Zcommunion","ZMyMIDJA1_Autonomy")%>%
  estimate_profiles(n_profiles = 4, models = 2, package = "mclust")
clustering_jp<-get_data(model_jp)

plot_density(model_jp)
plot_density(model_us)
data_us$class<-clustering_us$Class
data <- left_join(data_us, Midus1_Squad_Data_USJAPANwellbeing, by = "M2ID")

data$class <- as.factor(data$class)

anova_1SIADL <- aov(1SIADL ~ class, data = data)
summary(anova_1SIADL)

anova_A1SCHRON <- aov(A1SCHRON ~ class, data = data)
summary(anova_A1SCHRON)

anova_A1PRAGE_2019 <- aov(A1PRAGE_2019 ~ class, data = data)
summary(anova_A1PRAGE_2019)

anova_A1SAGREE <- aov(A1SAGREE ~ class, data = data)
summary(anova_A1SAGREE)

anova_A1SEXTRA <- aov(A1SEXTRA ~ class, data = data)
summary(anova_A1SEXTRA)

anova_A1SNEURO <- aov(A1SNEURO ~ class, data = data)
summary(anova_A1SNEURO)

anova_A1SCONS <- aov(A1SCONS ~ class, data = data)
summary(anova_A1SCONS)

anova_A1SOPEN <- aov(A1SOPEN ~ class, data = data)
summary(anova_A1SOPEN)

tukey_test <- TukeyHSD(anova_A1SOPEN)
print(tukey_test)
plot(tukey_test, las = 1)