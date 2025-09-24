library(dplyr)
library(tidyr)

file.choose()

df <- read.csv("C:\\Users\\Priyanka\\Desktop\\Winter 25\\Data Vizualization\\Project\\metaverse_transactions_dataset.csv\\metaverse_transactions_dataset.csv")

head(df)

# View the structure of the dataset
str(df)

# Summary statistics of numerical variables
summary(df)

# Frequency distribution of categorical variables
table(df$transaction_type)
table(df$location_region)
table(df$anomaly)

# Load necessary library for visualization
library(ggplot2)

# Histogram of transaction amounts
ggplot(df, aes(x = amount)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of Transaction Amounts", x = "Amount", y = "Frequency")

# Bar plot of transaction types
ggplot(df, aes(x = transaction_type)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Transaction Type Distribution", x = "Transaction Type", y = "Count")

# Scatter plot: Amount vs. Risk Score
ggplot(df, aes(x = amount, y = risk_score, color = transaction_type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Amount vs. Risk Score", x = "Amount", y = "Risk Score") +
  theme_minimal()

ggplot(df, aes(x = "", y = count, fill = location_region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Proportion of Transactions by Location Region") +
  theme_minimal()


#boxplot for comparing various age groups which gets targeted for scams and phishing
# Bar Plot: Scam and Phishing by Age Group
ggplot(df %>% filter(transaction_type %in% c("scam", "phishing")), 
       aes(x = age_group, fill = transaction_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Scam and Phishing Transactions by Age Group",
       x = "Age Group", y = "Count of Transactions") +
  theme_minimal()

# Check distribution of scam and phishing by age group
table(df$age_group, df$transaction_type)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
df <- read.csv("metaverse_transactions_dataset.csv")

# Filter relevant transaction types
df_filtered <- df %>% 
  filter(transaction_type %in% c("purchase", "transfer", "sale", "scam", "phishing"))

# Violin Plot: Session Duration by Age Group and Transaction Type
# Grouped Violin Plot
ggplot(df_filtered, aes(x = age_group, y = session_duration, fill = transaction_type)) +
  geom_violin(trim = FALSE, position = position_dodge(0.8)) +
  labs(title = "Session Duration by Age Group and Transaction Type",
       x = "Age Group", y = "Session Duration (Minutes)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ transaction_type)


#revised code to appear all the age group for trasactiont type
# Ensure all transaction types and age groups appear
df_filtered <- df %>% 
  filter(transaction_type %in% c("purchase", "transfer", "sale", "scam", "phishing")) %>%
  complete(transaction_type, age_group, fill = list(session_duration = 0))

# Create Violin Plot
ggplot(df_filtered, aes(x = age_group, y = session_duration, fill = transaction_type)) +
  geom_violin(trim = FALSE, position = position_dodge(1), scale = "width") +
  labs(title = "Session Duration by Age Group and Transaction Type",
       x = "Age Group", y = "Session Duration (Minutes)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ transaction_type, scales = "free_y")  # Allows better scaling
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
df <- read.csv("metaverse_transactions_dataset.csv")

ggplot(df, aes(x = age_group, y = transaction_type, fill = session_duration)) +
  geom_tile(color = "white", width = 1, height = 1) +  # Keeps matrix structure
  geom_text(aes(label = round(session_duration, 1)), 
            color = "black", size = 4.5, fontface = "bold", family = "Arial",
            vjust = 0.5, hjust = 0.5) +  # Ensures text is properly centered
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Session Duration (min)") +
  labs(title = "Bertin Matrix: Average Session Duration by Transaction Type & Age Group",
       x = "Age Group", y = "Transaction Type") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Bigger title
    axis.text.x = element_text(size = 12, angle = 0, vjust = 0.5),  # Keep x-axis readable
    axis.text.y = element_text(size = 12),  # Keep y-axis readable
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  coord_fixed() 








# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example Data (Ensure you replace this with your actual dataset)
df <- data.frame(
  purchase_pattern = rep(c("focused", "random", "high_value"), each = 3),
  age_group = rep(c("established", "new", "veteran"), times = 3),
  anomaly = c("high_risk", "moderate_risk", "low_risk"),
  risk_score = c(0.75, 0.32, 1, 0.5, 0.8, 0, 0.95, 0.6, 0.3)
)

# Ensure all combinations exist by filling missing values with 0
df_complete <- df %>%
  complete(purchase_pattern, age_group, anomaly, fill = list(risk_score = 0))

# Create the Bertin Matrix
ggplot(df_complete, aes(x = age_group, y = purchase_pattern, fill = anomaly)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(risk_score, 2)), 
            color = "black", size = 4,family = "sans",
            vjust = 0.5, hjust = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Risk Score") +
  labs(title = "Bertin Matrix: Risk Score by Purchase Pattern & Age Group",
       x = "Age Group", y = "Purchase Pattern") +
  facet_wrap(~ anomaly) +  # Separating by Anomaly
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )


#spine plot
ggplot(data = df) + 
  geom_mosaic(aes(x = product(anomaly), fill = age_group), na.rm = TRUE) +
  labs(title = "Spine Plot: Age Group vs. Anomaly",
       x = "Anomaly",
       y = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right")


library(ggplot2)
library(ggplot2)
library(ggmosaic)

ggplot(df) + 
  geom_mosaic(aes(x = product(anomaly), fill = age_group), na.rm = TRUE) +
  
  # Improve labeling
  labs(title = "Spine Plot: Age Group vs. Anomaly",
       x = "Risk Score",
       y = NULL) +  # Removes y-axis label
  
  theme_minimal() +
  
  # Theme Adjustments
  theme(
    axis.text.y = element_blank(),    # Removes y-axis labels
    axis.ticks.y = element_blank(),   # Removes y-axis ticks
    axis.title.y = element_blank(),   # Removes y-axis title
    panel.grid.major.y = element_blank(),  # Removes major y-axis grid lines
    panel.grid.minor.y = element_blank(),  # Removes minor y-axis grid lines
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12)  # Makes x-axis readable
  )



install.packages("ggmosaic")

library(ggmosaic)




library(ggplot2)
library(ggmosaic)

# Load required libraries
library(ggplot2)
library(ggmosaic)

# Create the Spine Plot
ggplot(d) + 
  geom_mosaic(aes(x = product(factor(anomaly)), fill = age_group), na.rm = TRUE) +
  
  # Add text labels for counts
  geom_mosaic_text(aes(x = product(factor(anomaly)), label = after_stat(count)), 
                   stat = "mosaic", position = position_stack(vjust = 0.5), 
                   color = "black", size = 5) +  
  
  # Labels and Theme
  labs(title = "Spine Plot: Anomaly vs. Risk Score",
       x = "Risk Score",
       y = "Proportion of Anomaly") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right")


# Load required libraries
library(ggplot2)
library(ggmosaic)

# Create Spine Plot
ggplot(df) + 
  geom_mosaic(aes(x = product(factor(anomaly, transaction_type)), fill = age_group), na.rm = TRUE) +
  
  # Add count labels inside the bars
  geom_mosaic_text(aes(x = product(factor(anomaly, transaction_type)), label = after_stat(count)), 
                   stat = "mosaic", position = position_stack(vjust = 0.5), 
                   color = "black", size = 4) +  
  
  # Improve axis readability
  labs(title = "Spine Plot: Age Group vs. Risk Score",
       x = "Risk Score",
       y = "Proportion of Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right")



library(ggplot2)
library(ggmosaic)

ggplot(df) + 
  geom_mosaic(aes(x = product(anomaly, purchase_pattern), fill = age_group), na.rm = TRUE) +
  
  # Add count labels inside the bars correctly
  geom_text(stat = "mosaic", aes(label = after_stat(count)), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) +  
  
  # Improve axis readability
  labs(title = "Spine Plot: Age Group vs. Risk Score",
       x = "Risk Score",
       y = "Proportion of Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "right")

theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))



# Ensure the Anomaly and Age Group categories are in the correct order
# Load necessary library
library(ggplot2)

# Create the boxplot
ggplot(df, aes(x = purchase_pattern, y = risk_score, fill = age_group)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  
  labs(title = "Boxplot of Anomaly Scores by Purchase Pattern",
       x = "Purchase Pattern",
       y = "Anomaly Score",
       fill = "age_score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
        plot.title = element_text(size = 14, face = "bold"))



library(ggplot2)
library(dplyr)

# Modify the purchase_pattern values
df <- df %>%
  mutate(purchase_pattern = recode(purchase_pattern,
                                   "random" = "Random",
                                   "focused" = "Focused",
                                   "high_value" = "High Value"))

# Create the boxplot with modified labels
library(ggplot2)

# Define the color scheme: Light Red, Light Violet, and Light Blue
color_palette <- c("established" = "#FF9999",  # Light Red
                   "new" = "#DDA0DD",          # Light Violet
                   "veteran" = "#ADD8E6")      # Light Blue

# Create the Boxplot-----
ggplot(df, aes(x = purchase_pattern, y = risk_score, fill = age_group)) +
  geom_boxplot(outlier.shape = NA) +  # Removes outliers
  scale_fill_manual(values = color_palette) +  # Apply custom colors
  labs(title = "Distribution of Anomaly Scores Across Purchase Patterns by Age Group",
       x = "Purchase Pattern",
       y = "Risk Score",
       fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10),
        plot.title = element_text(size = 14, face = "bold"))


