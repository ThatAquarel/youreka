library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggstatsplot)
library(ggpubr)

data <- read.csv("/home/dataset/tianyi/src/youreka/dataset/all_subsetted_group.csv")

# filter mental health and indigenous status
filtered <- data %>%
    filter(GEN_015 <= 5) %>%
    filter(SDC_015 <= 2)

# normalize NDVI into a range of [0, 1]
ndvi <- filtered$NDVI
filtered$NDVI <- (ndvi - min(ndvi)) / (max(ndvi) - min(ndvi))

# categorize NDVI into four intervals
filtered$NDVI <- cut(
  filtered$NDVI, 
  breaks = c(-Inf, 0.33, 0.67, Inf),
  #breaks = c(-Inf, 0.25, 0.5, 0.75, Inf),
  #labels = c(1, 2, 3, 4)
  labels = c("Low", "Medium", "High")
)

# change SDC_015, NDVI and GEN_015 to factor
filtered$NDVI <- as.factor(filtered$NDVI)

filtered$SDC_015 <- as.factor(filtered$SDC_015)
levels(filtered$SDC_015) <- c("Indigenous (n = 3,814; 5.22%)", "Non-Indigenous (n = 69,199; 94.78%)")

filtered$GEN_015 <- as.factor(filtered$GEN_015)
levels(filtered$GEN_015) <- c("Excellent", "Very good", "Good", "Fair", "Poor")
filtered$GEN_015 = ordered(filtered$GEN_015, levels = c("Poor", "Fair", "Good", "Very good", "Excellent"))
# sample data
# sampled <- filtered %>% slice_sample(n = 1000, replace=FALSE)
sampled <- filtered

# rename
analysis_data <- sampled

# dataset visual
grouped_ggbarstats(
  data = analysis_data,
  x = GEN_015,
  legend.title = "Self-perceived mental health",
  xlab = "Normalized difference vegetation index (NDVI)",
  y = NDVI,
  grouping.var = SDC_015,
  results.subtitle = FALSE,
  package = "ggthemes",
  palette = "excel_Grayscale"
)
ggsave("/home/tianyi/src/youreka/src/data_vis.png")
