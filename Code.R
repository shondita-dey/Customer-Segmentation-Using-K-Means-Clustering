# Load necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(arules)
library(arulesViz)

# Load data
data <- read.csv("D:\\M.Sc. Data Analytics\\FY\\Semester 2\\Big Data Analytics\\CA2\\marketing_campaign.csv")

# Spending variable creation
data$Age <- 2014 - data$Year_Birth
data$Spending <- data$MntWines + data$MntFruits + data$MntMeatProducts + data$MntFishProducts + data$MntSweetProducts + data$MntGoldProds

# Seniority variable creation
last_date <- as.Date("2014-10-04")
data$Seniority <- as.numeric(difftime(last_date, as.Date(data$Dt_Customer, format = "%d-%m-%Y"), units = "days")) / 30

# Data cleaning and preparation
data <- data %>%rename(Web = NumWebPurchases, Catalog = NumCatalogPurchases, Store = NumStorePurchases)%>%mutate(Marital_Status = case_when(Marital_Status %in% c("Divorced", "Single", "Absurd", "Widow", "YOLO") ~ "Alone",Marital_Status %in% c("Married", "Together") ~ "In couple",TRUE ~ Marital_Status),Education = case_when(Education %in% c("Basic", "2n Cycle") ~ "Undergraduate",Education %in% c("Graduation", "Master", "PhD") ~ "Postgraduate",TRUE ~ Education),Children = Kidhome + Teenhome,Has_child = ifelse(Children > 0, "Has child", "No child"),Children = case_when(Children == 3 ~ "3 children",Children == 2 ~ "2 children",Children == 1 ~ "1 child",TRUE ~ "No child"))%>%rename(Wines = MntWines,Fruits = MntFruits,Meat = MntMeatProducts,Fish = MntFishProducts,Sweets = MntSweetProducts,Gold = MntGoldProds)

# Subset the relevant columns
data <- data%>%select(Age, Education, Marital_Status, Income, Spending, Seniority, Has_child, Children, Wines, Fruits, Meat, Fish, Sweets, Gold)

# Remove outliers and missing values
data <- data %>%filter(!is.na(Income) & Income < 600000)

# Standardize and cluster the data using K-means
dataset_temp <- data %>% select(Income, Seniority, Spending)
dataset_temp_scaled <- scale(dataset_temp)

# Perform K-means clustering
set.seed(123)
kmeans_result <- kmeans(dataset_temp_scaled, centers = 4, nstart = 25)

# Assign clusters to the dataset
data$Cluster <- factor(kmeans_result$cluster, levels = 1:4, labels = c('Stars', 'Need attention', 'High potential', 'Leaky bucket'))

# Visualize clusters using 3D scatter plot
fviz_cluster(kmeans_result, data = dataset_temp_scaled, geom = "point", stand = FALSE, ellipse = TRUE)

# Create summary statistics
summary <- data %>%group_by(Cluster) %>%summarise(Income_mean = mean(Income, na.rm = TRUE),Spending_mean = mean(Spending, na.rm = TRUE),Seniority_mean = mean(Seniority, na.rm = TRUE))
print(summary)

# Apriori Algorithm
# Create Age, Income, Seniority segments
data <- data %>%
  mutate(Age_group = cut(Age, breaks = c(0, 30, 45, 65, 120), labels = c("Young", "Adult", "Mature", "Senior")),
         Income_group = ntile(Income, 4),
         Seniority_group = ntile(Seniority, 4))

# Binarize product consumption data
data <- data %>%
  mutate(Wines_segment = cut(Wines, breaks = quantile(Wines[Wines > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')),
         Fruits_segment = cut(Fruits, breaks = quantile(Fruits[Fruits > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')),
         Meat_segment = cut(Meat, breaks = quantile(Meat[Meat > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')),
         Fish_segment = cut(Fish, breaks = quantile(Fish[Fish > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')),
         Sweets_segment = cut(Sweets, breaks = quantile(Sweets[Sweets > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')),
         Gold_segment = cut(Gold, breaks = quantile(Gold[Gold > 0], probs = c(0, .25, .75, 1)), include.lowest = TRUE, labels = c('Low consumer', 'Frequent consumer', 'Biggest consumer')))

# Replace missing values for consumers who didn't buy any product
for(col in names(data)) {
  if (is.factor(data[[col]])) {
    # Add "Non consumer" as a new level to factor columns
    levels(data[[col]]) <- c(levels(data[[col]]), "Non consumer")}}
data[is.na(data)] <- 'Non consumer'

# Apply Apriori Algorithm
transaction_data <- as(data, "transactions")
rules <- apriori(transaction_data, parameter = list(supp = 0.08, conf = 0.5, maxlen = 100))

# Filter rules related to 'Biggest consumer' of 'Wines'
target <- 'Wines_segment=Biggest consumer'
rules_target <- subset(rules, rhs %pin% target)

# Visualize the top rules
inspect(sort(rules_target, by = 'confidence')[1:5])
plot(rules_target, method = "graph", engine = "htmlwidget")