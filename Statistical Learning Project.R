############################################# Supervised Learning ############################################# 

#--------------------------------------------Data Extraction and Cleaning --------------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(rworldmap)
library(GGally)
library(caret)
library(rlang)
library(car)
library(lmtest)
library(gridExtra)
library(ggpubr)
library(glmnet)
library(gam)
library(tree)
library(rpart)
library(rpart.plot)
library(party)
library(partykit)
library(randomForest)
library(factoextra)
library(plotly)
library(cluster)
library(dendextend)


# Importing the dataset
dataset = read.csv('STAT_LEARNING_DATASET_SUPERVISIONED.csv', header = TRUE, sep = ';', fileEncoding = 'UTF-8-BOM', stringsAsFactors = FALSE)

# Removing rows with at least one NA, blank, or space value
dataset <- dataset[complete.cases(dataset) & apply(dataset, 1, function(row) all(row != "" & row != " ")), ]

# Replace commas with periods if commas are used as decimal separators
dataset$Country_Name <- dataset$Country_Name
dataset$Country_Code <- dataset$Country_Code
dataset$DEM_FL <- dataset$DEM_FL
dataset$DEM_IND <- gsub(",", ".", dataset$DEM_INDEX)
dataset$GDP_PC <- gsub(",", ".", dataset$GDP_PC)
dataset$EXP <- gsub(",", ".", dataset$EXPORT)
dataset$POL <- gsub(",", ".", dataset$POLITICAL_STABILITY)
dataset$COR <- gsub(",", ".", dataset$CORRUPTION_CONTROL)
dataset$ELE <- gsub(",", ".", dataset$ELECTRICITY_CONSUMPTION)
dataset$INT <- gsub(",", ".", dataset$IND_USING_INTERNET)
dataset$FDI <- gsub(",", ".", dataset$FDI)
dataset$LAB <- gsub(",", ".", dataset$LABOR_PART)
dataset$SFE <- gsub(",", ".", dataset$SELF_EMP)
dataset$UNE <- gsub(",", ".", dataset$UNEMPLOYEMENT)
dataset$URB <- gsub(",", ".", dataset$URBAN_POP)

# Remove original column names after renaming (optional)
dataset <- dataset[, c("Country_Name","Country_Code","DEM_FL","DEM_IND", "GDP_PC", "EXP", "POL", "COR", "ELE", "INT", "FDI", "LAB", "SFE", "UNE", "URB")]

# Convert to numeric again
dataset$DEM_IND <- as.numeric(dataset$DEM_IND)
dataset$GDP_PC <- as.numeric(dataset$GDP_PC)
dataset$EXP <- as.numeric(dataset$EXP)
dataset$POL <- as.numeric(dataset$POL)
dataset$COR <- as.numeric(dataset$COR)
dataset$ELE <- as.numeric(dataset$ELE)
dataset$INT <- as.numeric(dataset$INT)
dataset$FDI <- as.numeric(dataset$FDI)
dataset$LAB <- as.numeric(dataset$LAB)
dataset$SFE <- as.numeric(dataset$SFE)
dataset$UNE <- as.numeric(dataset$UNE)
dataset$URB <- as.numeric(dataset$URB)

# Check the structure of the dataset to confirm conversion
str(dataset)

#-------------------------------------------- World Map ----------------------------------------------------------------------------------------------------------------

# Join dataset and world map data (adjusting region names)
world_map <- map_data("world")
world_data <- left_join(world_map, dataset, by = c("region" = "Country_Name"))

# Step 1: Create GDP_Category based on your fixed thresholds
world_data$GDP_Category <- case_when(
  world_data$GDP_PC < 500 ~ "Very-Low (<500)",
  world_data$GDP_PC >= 500 & world_data$GDP_PC < 2500 ~ "Low (500 - 2.500)",
  world_data$GDP_PC >= 2500 & world_data$GDP_PC < 10000 ~ "Medium (2.500 - 10.000)",
  world_data$GDP_PC >= 10000 & world_data$GDP_PC < 25000 ~ "Medium-High (10.000 - 25.000)",
  world_data$GDP_PC >= 25000 ~ "High (>25.000)",
  TRUE ~ "NA"  # For NA values
)

# Step 2: Set factor levels for GDP_Category
world_data$GDP_Category <- factor(world_data$GDP_Category, 
                                  levels = c("Very-Low (<500)", 
                                             "Low (500 - 2.500)", 
                                             "Medium (2.500 - 10.000)", 
                                             "Medium-High (10.000 - 25.000)", 
                                             "High (>25.000)", 
                                             "NA"))

# Step 3: Create the plot with ordered legend
map_GDP_PC <- ggplot(world_data, aes(x = long, y = lat, group = group, fill = GDP_Category)) +
  geom_polygon(color = "black") +
  scale_fill_manual(
    values = c("Very-Low (<500)" = rgb(227/255, 138/255, 129/255),          # Red for Very-Low
               "Low (500 - 2.500)" = rgb(252/255, 197/255, 80/255),       # Yellow for Low
               "Medium (2.500 - 10.000)" = rgb(252/255, 237/255, 167/255),  # Light Yellow for Medium
               "Medium-High (10.000 - 25.000)" = rgb(190/255, 232/255, 196/255),  # Light Green for Medium-High
               "High (>25.000)" = rgb(132/255, 184/255, 137/255),         # Green for High
               "NA" = rgb(230/255, 230/255, 230/255)),    # Light Gray for NA
    name = "GDP Category"
  ) +
  theme_void() +
  labs(title = "World Map by GDP Per Capita") +
  theme(legend.position = "right")

# Display the plot
print(map_GDP_PC)

ggsave("map_GDP_PC.png", plot = map_GDP_PC, width = 12, height = 6, dpi = 300)

#----------------------------------------------------------------------------------------------------------------

#Set Country_Name as dataset index
rownames(dataset) <- dataset$Country_Name
dataset <- dataset[, -which(names(dataset) == "Country_Name")]

# Check the structure of the dataset to confirm conversion
str(dataset)

#dataset <- dataset %>% select( -Country_Code)

#-------------------------------------------- Check Linearity (Original Data) --------------------------------------------------------

#In this section the liner model is compared to the quadratic one for each variable
#in order to understand which polinomial to introduce
m1 <- lm(GDP_PC ~ COR, data = dataset)
m2 <- lm(GDP_PC ~ COR + I(COR^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ EXP, data = dataset)
m2 <- lm(GDP_PC ~ EXP + I(EXP^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ ELE, data = dataset)
m2 <- lm(GDP_PC ~ ELE + I(ELE^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ INT, data = dataset)
m2 <- lm(GDP_PC ~ INT + I(INT^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ FDI, data = dataset)
m2 <- lm(GDP_PC ~ FDI + I(FDI^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ LAB, data = dataset)
m2 <- lm(GDP_PC ~ LAB + I(LAB^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ SFE, data = dataset)
m2 <- lm(GDP_PC ~ SFE + I(SFE^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ UNE, data = dataset)
m2 <- lm(GDP_PC ~ UNE + I(UNE^2), data = dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ URB, data = dataset)
m2 <- lm(GDP_PC ~ URB + I(URB^2), data = dataset)
anova(m1,m2,test="Chisq")

#-------------------------------------------- Forward Stepwise selection method & Normality test (Original Dataset) --------------------------------------------------------

# Define the null model (starting point) with only the intercept
null_model <- lm(GDP_PC ~ 1, data = dataset)

# Define the full model with all predictors
dataset$DEM_FL <- as.factor(dataset$DEM_FL)
full_model <- lm(GDP_PC ~ EXP +
                   COR + I(COR^2)  + ELE + I(ELE^2) + INT + I(INT^2)+ 
                   FDI+  LAB + I(LAB^2) + SFE + I(SFE^2)+ UNE + URB + I(URB^2)+ DEM_FL, 
                 data = dataset)
# Perform forward stepwise selection
stepwise_model <- step(null_model, 
                       scope = list(lower = null_model, upper = full_model), 
                       direction = "forward")

# View the selected model
summary(stepwise_model)

# Extract residuals
residuals <- residuals(stepwise_model)

# Perform Normality Test: Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals)

# Print the results
print(shapiro_test)

# Create a data frame for ggplot
residuals_df <- data.frame(residuals = residuals)

# 1. Q-Q plot
original_qq_plot <- ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq(color = "blue", size = 0.5) +  # Empty dots with black outline
  stat_qq_line(color = "red") +
  labs(title = "Residuals Q-Q (Original Dataset)", x = "Data Quantiles", y = "Theoretical Quantiles") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

# 2. Density plot
mean_gdp <- mean(residuals_df$residuals)
sd_gdp <- sd(residuals_df$residuals)

original_residual_distribution <- ggplot(residuals_df, aes(x = residuals)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Add a vertical line at zero
  geom_density( color = "blue",  alpha = 0.7) +
  labs(title = "Residuals Density (Original Dataset)", x = "Residuals", y = "Density") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20)
  ) +
  
  # Add normal distribution overlay
  stat_function(fun = dnorm, 
                args = list(mean = mean_gdp*4, sd = sd_gdp), 
                color = "red")   # Normal distribution line


#-------------------------------------------- Check Linearity (Normalized Dataset) --------------------------------------------------------

#Transform in a more normal distributed way the original dataset employing Yeo-Johnson transformation.
transformed_data <- preProcess(dataset, method = c("YeoJohnson"))

# Transform the dataset
normalized_dataset <- predict(transformed_data, newdata = dataset)

#In this section the liner model is compared to the quadratic one for each variable
#in order to understand which polinomial to introduce
m1 <- lm(GDP_PC ~ COR, data = normalized_dataset)
m2 <- lm(GDP_PC ~ COR + I(COR^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ EXP, data = normalized_dataset)
m2 <- lm(GDP_PC ~ EXP + I(EXP^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ ELE, data = normalized_dataset)
m2 <- lm(GDP_PC ~ ELE + I(ELE^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ INT, data = normalized_dataset)
m2 <- lm(GDP_PC ~ INT + I(INT^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ FDI, data = normalized_dataset)
m2 <- lm(GDP_PC ~ FDI + I(FDI^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ LAB, data = normalized_dataset)
m2 <- lm(GDP_PC ~ LAB + I(LAB^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ SFE, data = normalized_dataset)
m2 <- lm(GDP_PC ~ SFE + I(SFE^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ UNE, data = normalized_dataset)
m2 <- lm(GDP_PC ~ UNE + I(UNE^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")

m1 <- lm(GDP_PC ~ URB, data = normalized_dataset)
m2 <- lm(GDP_PC ~ URB + I(URB^2), data = normalized_dataset)
anova(m1,m2,test="Chisq")


#-------------------------------------------- Forward Stepwise Selection method & Normality test (Normalized Dataset) --------------------------------------------------------

# Define the null model (starting point) with only the intercept
null_model <- lm(GDP_PC ~ 1, data = normalized_dataset)

# Define the full model with all predictors
normalized_dataset$DEM_FL <- as.factor(normalized_dataset$DEM_FL)
full_model <- lm(GDP_PC ~ EXP + I(EXP^2)  +
                   COR + I(COR^2)  + ELE + INT + I(INT^2)+ 
                   FDI + LAB + SFE + I(SFE^2)+ UNE + URB, 
                 data = normalized_dataset)
# Perform forward stepwise selection
stepwise_model_normalized <- step(null_model, 
                                  scope = list(lower = null_model, upper = full_model), 
                                  direction = "forward")

# View the selected model
summary(stepwise_model_normalized)

residuals_normalized <- residuals(stepwise_model_normalized)

# Perform Shapiro-Wilk test
shapiro_test_normalized <- shapiro.test(residuals_normalized)

# Print the results
print(shapiro_test_normalized)

# Create a data frame for ggplot
residuals_df_normalized <- data.frame(residuals = residuals_normalized)

# 1. Q-Q plot
normalized_qq_plot <- ggplot(residuals_df_normalized, aes(sample = residuals)) +
  stat_qq(color = "blue", size = 0.5) +  # Empty dots with black outline
  stat_qq_line(color = "red") +
  labs(title = "Residuals Q-Q (Normalized Dataset)", x = "Data Quantiles", y = "Theoretical Quantiles") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = "NA", size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

# 2. Density plot

mean_gdp_normalized <- mean(residuals_df_normalized$residuals)
sd_gdp_normalized <- sd(residuals_df_normalized$residuals)


# Plot the distribution curve
normailzed_residual_distribution <- ggplot(residuals_df_normalized, aes(x = residuals_normalized)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +  # Add a vertical line at zero
  geom_density( color = "blue", alpha = 0.7) +
  labs(title = "Residuals Density (Normalized Dataset)", x = "Residuals", y = "Density") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20)
  ) +
  
  # Add normal distribution overlay
  stat_function(fun = dnorm, 
                args = list(mean = mean_gdp_normalized*4, sd = sd_gdp_normalized), 
                color = "red")   # Normal distribution line

# Arrange the two plots together
combined_distr_plot <- grid.arrange(original_residual_distribution, normailzed_residual_distribution, original_qq_plot, normalized_qq_plot, nrow = 2, ncol = 2)

# Save the combined plot as a PNG file
ggsave("combined_plot_with_normal_distribution.png", plot = combined_distr_plot, width = 12, height = 12, dpi = 300)


#-------------------------------------------- Outliers Before Regression (Z-score check) --------------------------------------------
# deselect non numeric variable to compute z-score
normalized_dataset_clean <- normalized_dataset %>% select( -Country_Code, -DEM_FL)

# Calculate Z-scores for all the numeric variables and detect outliers
# Initialize a list to store the outliers for each variable
outliers_list <- list()
outlier_indices <- c()

# Loop through all the variables in the dataset
for (var in colnames(normalized_dataset_clean)) {
  
  # Calculate Z-scores for the current variable
  z_scores <- scale(normalized_dataset_clean[[var]])
  
  # Find outliers (Z-scores greater than 3 or less than -3)
  outliers <- which(abs(z_scores) > 3)
  
  # Store the outliers in the list (if any)
  if (length(outliers) > 0) {
    outliers_list[[var]] <- normalized_dataset_clean[outliers, ]
  }
  
  # Store the unique outlier indices
  if (length(outliers) > 0) {
    outlier_indices <- unique(c(outlier_indices, outliers))
  }
}

# Display the list of outliers for each variable
outliers_list
outlier_indices

#Graph outliers on detected varaibles

# Set up a 2x2 plotting layout
par(mfrow = c(2, 1))

# Example for a specific variable, e.g., 'COR'
var <- 'EXP'

# Calculate z-scores for the specific variable
z_scores <- scale(normalized_dataset_clean[[var]])

# Step 2: Create a data frame with country_name as rownames and Z-scores as a column
data_for_plot <- data.frame(Z_Score = z_scores)
rownames(data_for_plot) <- rownames(normalized_dataset_clean)  # Use row names (Country_name)

# Create a scatter plot, highlighting outliers
EXP_OUTLIERS <- ggplot(data = data_for_plot, aes(x = seq_along(Z_Score), y = Z_Score)) +
  geom_point(aes(color = abs(Z_Score) > 3),size = 3) + 
  labs(title = paste("Outliers in", var, "(Z-Scores)"), x = "Index", y = "Z-Score") + 
  scale_color_manual(values = c("gray", "red"), guide = FALSE) +  # Red for outliers
  geom_hline(yintercept = 0, color = "black") +  # x-axis line
  geom_hline(yintercept = c(-3, 3), color = "blue", linetype = "dashed", size = 1,5) +  # Thresholds for Z-scores
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  ) +
  # Add labels for outliers
  geom_text(aes(label = ifelse(abs(Z_Score) > 3, rownames(data_for_plot), "")), 
            vjust = -1, color = "red", size = 5)

var <- 'FDI'

z_scores <- scale(normalized_dataset_clean[[var]])

# Step 2: Create a data frame with country_name as rownames and Z-scores as a column
data_for_plot <- data.frame(Z_Score = z_scores)
rownames(data_for_plot) <- rownames(normalized_dataset_clean)  # Use row names (Country_name)

# Create a scatter plot, highlighting outliers
FDI_OUTLIERS <- ggplot(data = data_for_plot, aes(x = seq_along(Z_Score), y = Z_Score)) +
  geom_point(aes(color = abs(Z_Score) > 3),size = 3) + 
  labs(title = paste("Outliers in", var, "(Z-Scores)"), x = "Index", y = "Z-Score") + 
  scale_color_manual(values = c("gray", "red"), guide = FALSE) +  # Red for outliers
  geom_hline(yintercept = 0, color = "black") +  # x-axis line
  geom_hline(yintercept = c(-3, 3), color = "blue", linetype = "dashed", size = 1,5) +  # Thresholds for Z-scores
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  ) +
  # Add labels for outliers
  geom_text(aes(label = ifelse(abs(Z_Score) > 3, rownames(data_for_plot), "")), 
            vjust = -1, color = "red", size = 5)

# Arrange the plots
outlier_analysis <- ggarrange(EXP_OUTLIERS, FDI_OUTLIERS,  nrow = 2, ncol = 1)

# Save the scatterplot matrix as a PNG file with specified dimensions
ggsave("outlier_analysis.png", plot = outlier_analysis, width = 16, height = 15, dpi = 200)

par(mfrow = c(1, 1))

#Clean all the datasets from outliers
dataset <- dataset[-outlier_indices, ]
normalized_dataset <- normalized_dataset[-outlier_indices, ]
normalized_dataset_clean <- normalized_dataset_clean[-outlier_indices, ]

#-------------------------------------------- Original vs Normalized Dataset Linearity Graph (GDP PC) --------------------------------------------------------

# linear trend + confidence interval
quadratic_regr <- ggplot(dataset, aes(x = COR, y = GDP_PC)) +
  geom_point(color = "black",fill = "black", shape = 21) +  # Set point color to blue
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", fill = "lightgray", se = TRUE) +
  labs(title = "Quadratic Regression GDP PC and COR",
       x = "COR",
       y = "GDP PC") +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 0, color = "black") +   # y-axis line
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 20)) 

quadratic_regr_norm <- ggplot(normalized_dataset, aes(x = COR, y = GDP_PC)) +
  geom_point(color = "black",fill = "black", shape = 21) +  # Set point color to blue
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", fill = "lightgray", se = TRUE) +
  #    geom_smooth(method = lm, color = "red", fill = "lightgray", se = TRUE) +
  labs(title = "Quadratic Regression GDP PC and COR (Transformed)",
       x = "COR",
       y = "GDP PC") +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 0, color = "black") +   # y-axis line
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 20))

# Prevent clipping of points near the axis

Biv_distr <- ggplot(dataset, aes(x = COR, y = GDP_PC)) +
  #geom_point(alpha = 0.6) +  # Scatter plot of points
  geom_density_2d(color = "blue") +        # Adding density contours
  labs(title = "Bivariate Distribution of GDP PC and COR",
       x = "COR",
       y = "GDP PC") +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 0, color = "black") +   # y-axis line
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 20))

Biv_distr_normalized <- ggplot(normalized_dataset, aes(x = COR, y = GDP_PC)) +
  #geom_point(alpha = 0.6) +  # Scatter plot of points
  geom_density_2d(color = "blue") +        # Adding density contours
  labs(title = "Bivariate Distribution of GDP PC and COR (Transformed)",
       x = "COR",
       y = "GDP PC") +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 0, color = "black") +   # y-axis line
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text.y = element_text(size = 20))

# Arrange the two plots together
combined_plot <- grid.arrange(quadratic_regr,  quadratic_regr_norm, Biv_distr,  Biv_distr_normalized,  nrow = 2, ncol = 2)

# Save the combined plot as a PNG file
ggsave("linearity_check.png", plot = combined_plot, width = 16, height = 12, dpi = 300)

#-------------------------------------------- Multicollinearity: Scatterplot matrix --------------------------------------------------------

# Custom function for coloring scatterplot points in the lower matrix based on correlation
color_points <- function(data, mapping) {
  x_var <- as_label(mapping$x)
  y_var <- as_label(mapping$y)
  corr <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  point_color <- ifelse(abs(corr) > 0.7, "red", "black")
  ggplot(data = data, mapping = mapping) +
    geom_point(color = point_color, alpha = 0.5) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text = element_text(size = 20)
    )
}

# Custom function for coloring correlation text in the upper matrix
color_cor <- function(data, mapping) {
  x_var <- as_label(mapping$x)
  y_var <- as_label(mapping$y)
  corr <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
  text_color <- ifelse(abs(corr) > 0.7, "red", "black")
  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = sprintf("%.2f", corr),
      color = text_color,
      size = 10
    ) +
    theme_void()
}

# Remove "GDP PC" field from the dataset
normalized_dataset_clean_subset <- normalized_dataset_clean %>%
  select(-`GDP_PC`, -`DEM_IND`)

colnames(normalized_dataset_clean_subset) <- c("EXP", "POL", "COR", "ELE", "INT", "FDI", "LAB", "SFE", "UNE", "URB")  # Replace with desired names


# Scatterplot matrix with the custom color functions
scatter_matrix <- ggpairs(
  normalized_dataset_clean_subset,
  upper = list(continuous = color_cor),
  lower = list(continuous = color_points),
  diag = list(continuous = "densityDiag")
) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text = element_text(size = 20)
  )

print(scatter_matrix)

# Save the scatterplot matrix as a PNG file with specified dimensions
ggsave("scatterplot_matrix.png", plot = scatter_matrix, width = 4000 / 100, height = 1500 / 100, dpi = 100)


#-------------------------------------------- IID Test: ANOVA Analysis on DEM FLAG  --------------------------------------------------------

# Run ANOVA between GDP_PC and a categorical variable DEM FL: 
#It is necessary to assest the difference between groups and introduce the DEM_FL Variable into regression
anova_result <- aov(GDP_PC ~ DEM_FL, data = normalized_dataset)

# Show ANOVA result
summary(anova_result)

#ANOVA Analysis GRAPHS
# Define a custom color palette
DEM_FL_colors <- c("Flawed democracy" = rgb(190/255, 232/255, 196/255), 
                     "Full democracy" = rgb(18/255, 80/255, 26/255), 
                     "Hybrid regime" = rgb(252/255, 237/255, 167/255), 
                     #"Authoritarian" = rgb(252/255, 197/255, 80/255))
                     "Authoritarian" = rgb(227/255, 138/255, 129/255))

# Calculate the mean of GDP_PC for each DEM_FL
mean_values <- normalized_dataset %>%
  group_by(DEM_FL) %>%
  summarize(mean_gdp = mean(GDP_PC, na.rm = TRUE))

# Reorder DEM_FL based on the mean GDP_PC values
normalized_dataset$DEM_FL <- factor(normalized_dataset$DEM_FL, 
                                      levels = mean_values$DEM_FL[order(mean_values$mean_gdp)])

# violin, boxplot, dots, and mean values
violin_boxplot <- ggplot(normalized_dataset, 
                         aes(x = factor(DEM_FL), y = GDP_PC, 
                             color = factor(DEM_FL), fill = factor(DEM_FL))) +
  # Violin plot with black border and high transparency (in front of dots)
  geom_violin(trim = FALSE, alpha = 0.2, color = "darkgray") +  # Black border for the violin
  # Boxplot overlay with black border and transparency (in front of dots)
  geom_boxplot(width = 0.2, outlier.size = 2, outlier.colour = "red", alpha = 0.5, color = "darkgray") +  # Black border for the boxplot
  # Jittered dots (placed behind violin and boxplot)
  geom_jitter(width = 0.2, height = 0, size = 3, alpha = 0.6, shape = 21, color = "black") +
  # Add mean value for each DEM_FL
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 4, color = "darkred") +
  stat_summary(fun = "mean", geom = "text", aes(label = round(after_stat(y), 2)), 
               color = "darkred", vjust = -1, size = 5) +
  # Customize the color palette
  scale_fill_manual(values = DEM_FL_colors) +
  scale_color_manual(values = DEM_FL_colors) +
  # Titles and labels
  labs(title = "GDP per Capita by Democracy Flag",
       x = "Democracy Flag",
       y = "GDP per Capita") +
  # Customize theme
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.text.y = element_text(size = 20),
    legend.position = "none"  # Remove the legend
  )


# Display the plot
print(violin_boxplot)


# ANOVA Diagnostics: Residuals vs Fitted and Q-Q Plot
# Fitting ANOVA model
anova_result <- aov(GDP_PC ~ DEM_FL, data = normalized_dataset)

# Assuming normalized_dataset has two columns: 'Group' and 'Score'

# Calculate the mean of GDP_PC for each DEM_FL group
mean_gdp <- normalized_dataset %>%
  group_by(DEM_FL) %>%
  summarize(mean_gdp = mean(GDP_PC, na.rm = TRUE))

# Create the density plot
anova_variation_within <- ggplot(normalized_dataset, aes(x = GDP_PC, fill = DEM_FL)) +
  geom_density(alpha = 0.4,show.legend = FALSE) +  # Add transparency to the density plot
  scale_fill_manual(values = DEM_FL_colors) +
  labs(title = "Variation Between Groups",
       x = "Score",
       y = "Frequency") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "none"
    ) + # Add black border to panel
  geom_vline(data = mean_gdp, aes(xintercept = mean_gdp, color = DEM_FL), 
             linetype = "dashed", size = 1,show.legend = FALSE)+
  scale_color_manual(values = DEM_FL_colors)  # Apply custom colors to the dashed lines

# Residuals vs Fitted plot
residuals_vs_fitted <- ggplot(data.frame(fitted = fitted(anova_result), 
                  residuals = residuals(anova_result),
                  DEM_FL = normalized_dataset$DEM_FL),  # Replace 'your_data' with your dataset containing DEM_FL
       aes(x = fitted, y = residuals, color = DEM_FL)) +  # Use color instead of fill for circular dots
  geom_point(shape = 16, size = 2) +  # Shape 16 for filled circles, size adjusted
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values", 
       x = "Fitted Values", 
       y = "Residuals") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "none") +
  scale_color_manual(name = "DEM_FL", values = DEM_FL_colors)  # Apply custom colors




# Q-Q plot for residuals
qq_plot <- ggplot(data.frame(sample = residuals(anova_result)), aes(sample = sample)) +
  stat_qq(color = "black",fill = "black", shape = 21) +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals", x = "Data Quantiles", y = "Theoretical Quantiles") +
  #geom_hline(yintercept = 0, color = "black") +  # x-axis line
  #geom_vline(xintercept = 0, color = "black") +   # y-axis line
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white"),  # Set background to white
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    plot.title = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20))


# Arrange the plots
anova_analysis <- ggarrange( violin_boxplot, anova_variation_within, residuals_vs_fitted, qq_plot,  nrow = 2, ncol = 2)
# Save the scatterplot matrix as a PNG file with specified dimensions
ggsave("anova_analysis.png", plot = anova_analysis, width = 16, height = 16, dpi = 300)


#-------- Semi-Final Model: Multiple Linear Regression on Normalized Dataset, after Outliers Detection (Forward Stepwise)  --------------------------------------------------------

# Define the null model (starting point) with only the intercept
null_model <- lm(GDP_PC ~ 1, data = normalized_dataset)

# Define the full model with all predictors
normalized_dataset$DEM_FL <- as.factor(normalized_dataset$DEM_FL)
full_model <- lm(GDP_PC ~ EXP + I(EXP^2)  +
                   COR + I(COR^2)  + ELE + INT + I(INT^2)+ 
                   FDI + LAB + SFE + I(SFE^2)+ UNE + URB + DEM_FL, 
                 data = normalized_dataset)
# Perform forward stepwise selection
semifinal_model <- step(null_model, 
                                  scope = list(lower = null_model, upper = full_model), 
                                  direction = "forward")

# View the selected model
summary(semifinal_model)


# 
# # Fit the linear regression model with forward stepwise selection method previously performed.
# normalized_dataset$DEM_FL <- as.factor(normalized_dataset$DEM_FL)
# final_model <- lm(GDP_PC ~  COR + I(COR^2) + SFE + ELE + URB  + INT + DEM_FL,
#                   data = normalized_dataset)
# 
# 
# # View the summary of the model
# summary(final_model)

#-------------------------------------------- Multicollinearity  --------------------------------------------------------

#Variance Inflation Factor (VIF)
vif(semifinal_model)

#-------------------------------------------- Interaction Variables --------------------------------------------------------

##when should I use interaction between variables?

#-------------------------------------------- Plots for Diagnostics post Regression (Semi-Final Model) --------------------------------------------------------

# Save the plot to a PNG file
png("model_diagnostics_residuals.png", width = 800, height = 800)  # Specify filename, width, and height

# Set axis and title text sizes
par(mfrow = c(2, 2),
    cex.axis = 1,  # Increase axis text size
    cex.lab = 1.7,   # Increase axis label size
    cex.main = 10)    # Increase main title size

plot(semifinal_model, 
     col = "black",    # Set dots to black
     pch = 19,         # Filled circles for dots
     cex = 1,        # Increase dot size
     lwd = 2)          # Increase line width

# Close the device to finalize the file
dev.off()


# Get residuals from the selected model
residuals <- residuals(semifinal_model)

# Create a data frame for residuals
residuals_df <- data.frame(residuals = residuals)

# Exogeneity Check: correlation between residuals and independent variables
cor(normalized_dataset[, c("COR", "SFE", "ELE", "URB", "INT")], residuals)

# Test for normality of residuals using Shapiro-Wilk test
shapiro.test(residuals_df$residuals)

# Durbin-Watson test for autocorrelation of residuals (for time-series data)
durbinWatsonTest(semifinal_model)

# 7. Breusch-Pagan test for heteroscedasticity (non-constant variance)
bptest(semifinal_model)

#-------------------------------------------- Data Cleaning post Regression (Outliers and leverage points) --------------------------------------------------------

# List of outliers/Leverage points to remove (Retrieved from the previous diagnostics plot on residuals)
outliers_leverage_point_list = c("Equatorial Guinea", "Jordan", "Kyrgyzstan", "Burundi")

# Remove the outlier countries from the datasets using their row names
dataset <- dataset[!rownames(dataset) %in% outliers_leverage_point_list, ]
normalized_dataset <- normalized_dataset[!rownames(normalized_dataset) %in% outliers_leverage_point_list, ]
normalized_dataset_clean <- normalized_dataset_clean[!rownames(normalized_dataset_clean) %in% outliers_leverage_point_list, ]

#--- Final Model: Multiple Linear Regression on Normalized Dataset, after Outliers Detection and post regression Diagnostics (Forward Stepwise)  --------------------------------------------------------

# Define the null model (starting point) with only the intercept
null_model <- lm(GDP_PC ~ 1, data = normalized_dataset)

# Define the full model with all predictors
normalized_dataset$DEM_FL <- as.factor(normalized_dataset$DEM_FL)
full_model <- lm(GDP_PC ~ EXP + I(EXP^2)  +
                   COR + I(COR^2)  + ELE + INT + I(INT^2)+ 
                   FDI + LAB + SFE + I(SFE^2)+ UNE + URB + DEM_FL, 
                 data = normalized_dataset)
# Perform forward stepwise selection
final_model <- step(null_model, 
                                  scope = list(lower = null_model, upper = full_model), 
                                  direction = "forward")

# View the selected model
summary(final_model)

bptest(final_model)

png("final_model_diagnostics_residuals.png", width = 800, height = 800)  # Set width and height as needed
par(mfrow = c(2, 2))
plot(final_model)
dev.off()

#-------------------------------------------- Ridge and Lasso with Full Statistical Metrics --------------------------------------------------------

# Prepare the data
X <- as.matrix(normalized_dataset_clean[, -which(names(normalized_dataset_clean) == "GDP_PC")])  # Exclude GDP_PC from predictors
y <- normalized_dataset_clean$GDP_PC  # Response variable
n <- nrow(X)  # Number of observations
p <- ncol(X)  # Number of predictors

# Ridge regression
ridge_model <- glmnet(X, y, alpha = 0)

# Cross-validation for Ridge to find the best lambda
cv_ridge <- cv.glmnet(X, y, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min

# Coefficients at the best lambda for Ridge
ridge_coefficients <- coef(cv_ridge, s = best_lambda_ridge)

# Predictions and RSS, MSE for Ridge
ridge_predictions <- predict(cv_ridge, s = best_lambda_ridge, newx = X)
ridge_rss <- sum((y - ridge_predictions)^2)
ridge_mse <- mean((y - ridge_predictions)^2)
ridge_resid_se <- sqrt(ridge_rss / (n - p - 1))  # Residual Standard Error for Ridge

# Print Ridge results
print(paste("Ridge RSS:", ridge_rss))
print(paste("Ridge MSE:", ridge_mse))
print(paste("Ridge Residual Standard Error:", round(ridge_resid_se, 4), "on", n - p - 1, "degrees of freedom"))

# Compute R-squared for Ridge
tss <- sum((y - mean(y))^2)  # Total sum of squares
r_squared_ridge <- 1 - (ridge_rss / tss)
print(paste("Ridge R-Squared:", r_squared_ridge))

# Adjusted R-squared for Ridge
adjusted_r_squared_ridge <- 1 - ((1 - r_squared_ridge) * (n - 1) / (n - p - 1))
print(paste("Adjusted R-Squared for Ridge:", adjusted_r_squared_ridge))

# F-statistic for Ridge
ridge_f_stat <- (r_squared_ridge / (1 - r_squared_ridge)) * ((n - p - 1) / p)
ridge_p_value <- pf(ridge_f_stat, p, n - p - 1, lower.tail = FALSE)
print(paste("Ridge F-Statistic:", ridge_f_stat))
print(paste("Ridge p-value:", ridge_p_value))

# Lasso regression
lasso_model <- glmnet(X, y, alpha = 1)

# Cross-validation for Lasso to find the best lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min

# Coefficients at the best lambda for Lasso
lasso_coefficients <- coef(cv_lasso, s = best_lambda_lasso)

# Predictions and RSS, MSE for Lasso
lasso_predictions <- predict(cv_lasso, s = best_lambda_lasso, newx = X)
lasso_rss <- sum((y - lasso_predictions)^2)
lasso_mse <- mean((y - lasso_predictions)^2)
lasso_resid_se <- sqrt(lasso_rss / (n - p - 1))  # Residual Standard Error for Lasso

# Print Lasso results
print(paste("Lasso RSS:", lasso_rss))
print(paste("Lasso MSE:", lasso_mse))
print(paste("Lasso Residual Standard Error:", round(lasso_resid_se, 4), "on", n - p - 1, "degrees of freedom"))

# Compute R-squared for Lasso
r_squared_lasso <- 1 - (lasso_rss / tss)
print(paste("Lasso R-Squared:", r_squared_lasso))

# Adjusted R-squared for Lasso
adjusted_r_squared_lasso <- 1 - ((1 - r_squared_lasso) * (n - 1) / (n - p - 1))
print(paste("Adjusted R-Squared for Lasso:", adjusted_r_squared_lasso))

# F-statistic for Lasso
lasso_f_stat <- (r_squared_lasso / (1 - r_squared_lasso)) * ((n - p - 1) / p)
lasso_p_value <- pf(lasso_f_stat, p, n - p - 1, lower.tail = FALSE)
print(paste("Lasso F-Statistic:", lasso_f_stat))
print(paste("Lasso p-value:", lasso_p_value))

# Plot Ridge and Lasso cross-validation curves together
png("ridge_lasso_regr.png", width = 800, height = 800)
par(mfrow = c(2, 1))
plot(cv_ridge, main = "Ridge Regression Cross-Validation")
plot(cv_lasso, main = "Lasso Regression Cross-Validation")
dev.off()

png("ridge_lasso_regr.png", width = 1600, height = 800)
# Set the layout for two plots and increase both axis label size and title size
par(mfrow = c(1, 2), cex.lab = 1.5, cex.main = 2)  # cex.lab for axis labels, cex.main for title size
# Ridge Regression Cross-Validation Plot
plot(cv_ridge, main = "Ridge Regression Cross-Validation", cex.lab = 1.5, cex.main = 2)
# Lasso Regression Cross-Validation Plot
plot(cv_lasso, main = "Lasso Regression Cross-Validation", cex.lab = 1.5, cex.main = 2)
dev.off()


png("ridge_lasso_regr.png", width = 1600, height = 800)

# Set the layout for two plots and increase axis label size
par(mfrow = c(1, 2),
    mar = c(5, 5, 5, 5), 
    cex.axis = 2,  # Increase axis text size
    cex.lab = 2,   # Increase axis label size
    cex.main = 2)

# Ridge Regression Cross-Validation Plot
plot(cv_ridge,  main = "")  # Suppress default title
mtext("Ridge Regression Cross-Validation", side = 3, line = 3, cex = 2)  # Add title above

# Lasso Regression Cross-Validation Plot
plot(cv_lasso,  main = "")  # Suppress default title
mtext("Lasso Regression Cross-Validation", side = 3, line = 3, cex = 2)  # Add title above

dev.off()

#-------------------------------------------- General Additive Models (GAM)  --------------------------------------------------------

# Open a PNG device to save the plot
png("gam_plot.png", width = 800, height = 1600)  # Adjust the width and height as needed

forward_stepwise_gam <- function(data, response, predictors) {
  # Initialize the base model with just the intercept
  base_formula <- as.formula(paste(response, "~ 1"))
  current_model <- gam(base_formula, data = data)
  
  # Store the best model and AIC
  best_model <- current_model
  best_aic <- AIC(current_model)
  
  # Initialize a vector to store the selected terms
  selected_terms <- c()
  
  # Keep track of remaining predictors
  remaining_predictors <- predictors
  
  # Iteratively add predictors using forward selection
  while (length(remaining_predictors) > 0) {
    aic_values <- c()
    new_formulas <- list()
    
    # Try adding each remaining predictor with both s() and non-s()
    for (pred in remaining_predictors) {
      # Add the current predictor without smoothing
      non_s_term <- pred
      candidate_terms_non_s <- c(selected_terms, non_s_term)
      
      # Construct the new formula for non-s() term
      new_formula_non_s <- as.formula(paste(response, "~", paste(candidate_terms_non_s, collapse = " + ")))
      new_formulas[[paste0(pred, "_non_s")]] <- new_formula_non_s
      
      # Fit the new model for non-s() term
      new_model_non_s <- gam(new_formula_non_s, data = data)
      aic_values[paste0(pred, "_non_s")] <- AIC(new_model_non_s)
      
      # Add the current predictor with smoothing
      s_term <- paste0("s(", pred, ")")
      candidate_terms_s <- c(selected_terms, s_term)
      
      # Construct the new formula for s() term
      new_formula_s <- as.formula(paste(response, "~", paste(candidate_terms_s, collapse = " + ")))
      new_formulas[[paste0(pred, "_s")]] <- new_formula_s
      
      # Fit the new model for s() term
      new_model_s <- gam(new_formula_s, data = data)
      aic_values[paste0(pred, "_s")] <- AIC(new_model_s)
      
      # Print formulas and AICs for debugging
      print(new_model_non_s$formula)
      print(AIC(new_model_non_s))
      print(new_model_s$formula)
      print(AIC(new_model_s))
    }
    
    # Find the best model (min AIC) among both non-s and s terms
    best_pred <- names(which.min(aic_values))
    best_aic_candidate <- min(aic_values)
    
    # If the new model improves AIC, update the best model
    if (best_aic_candidate < best_aic) {
      # Determine whether the best term is s() or non-s()
      if (grepl("_non_s$", best_pred)) {
        selected_terms <- unique(c(selected_terms, sub("_non_s$", "", best_pred)))  # Add non-s() term
      } else {
        selected_terms <- unique(c(selected_terms, paste0("s(", sub("_s$", "", best_pred), ")")))   # Add s() term
      }
      
      # Fit the best model
      best_model <- gam(new_formulas[[best_pred]], data = data)
      best_aic <- best_aic_candidate
      
      # Remove the selected predictor from remaining predictors
      remaining_predictors <- setdiff(remaining_predictors, sub("_(s|non_s)$", "", best_pred))
    } else {
      break
    }
  }
  
  # Return the best model
  return(best_model)
}


response <- "GDP_PC"

predictors <- c("COR", "SFE", "ELE", "URB", "INT", "EXP", "FDI", "LAB", "UNE","POL")

# Apply forward stepwise selection on the dataset
best_gam_model <- forward_stepwise_gam(data = normalized_dataset, response = response, predictors = predictors)
best_gam_model$formula

# Update the formula to include DEM_FL as a factor
updated_formula <- update(best_gam_model$formula, . ~ . + DEM_FL)

normalized_dataset$DEM_FL <- as.factor(normalized_dataset$DEM_FL)
# Fit the updated model with the new formula
gam1 <- gam(updated_formula, data = normalized_dataset)

gam1$formula

# Summary of the model
summary(gam1)
par(mfrow = c(4, 2), mar = c(5, 5, 5, 5))

# Plot the smooth terms in the final model
plot(
  gam(updated_formula, data = normalized_dataset), 
  se = TRUE, 
  residuals = TRUE, 
  cex.lab = 2,     # Increase axis label size
  cex.axis = 2,    # Increase axis tick label size
  cex.main = 1.8,    # Increase title size
  pch = 16,          # Use filled dots for residuals
  col = "black"      # Set dots color to black
)
# Close the PNG device to save the plot
dev.off()

# GAM Final Model Evaluation #

# Predictions from the model
predicted_values <- predict(gam1, newdata = normalized_dataset)

# Calculate Residuals
residuals <- normalized_dataset$GDP_PC - predicted_values

# Mean Squared Error (MSE)
mse <- mean(residuals^2)

# Residual Standard Error
residual_std_error <- sqrt(sum(residuals^2) / (nrow(normalized_dataset) - length(coef(gam1))))

# R-Squared and Adjusted R-Squared
rss <- sum(residuals^2)  # Residual Sum of Squares
tss <- sum((normalized_dataset$GDP_PC - mean(normalized_dataset$GDP_PC))^2)  # Total Sum of Squares
r_squared <- 1 - (rss / tss)
adj_r_squared <- 1 - ((1 - r_squared) * ((nrow(normalized_dataset) - 1) / (nrow(normalized_dataset) - length(coef(gam1)))))

# F-Statistic
f_statistic <- ((tss - rss) / (length(coef(gam1)) - 1)) / (rss / (nrow(normalized_dataset) - length(coef(gam1))))
p_value <- pf(f_statistic, df1 = length(coef(gam1)) - 1, df2 = nrow(normalized_dataset) - length(coef(gam1)), lower.tail = FALSE)

# Print Results
cat("MSE:", mse, "\n")
cat("Residual Standard Error:", residual_std_error, "\n")
cat("R-Squared:", r_squared, "\n")
cat("Adjusted R-Squared:", adj_r_squared, "\n")
cat("F-Statistic:", f_statistic, "on", length(coef(gam1)) - 1, "and", nrow(normalized_dataset) - length(coef(gam1)), "degrees of freedom, p-value:", p_value, "\n")
coef(gam1)

#-------------------------------------------- Regression Tree/Bagging/Random Forest --------------------------------------------------------

#REGRESSION TREE
png("regression_tree_prediction.png", width = 1600, height = 800)  # Adjust the width and height as needed
par(mfrow=c(1,2),mar = c(5, 5, 5, 5))

#divide dataset into train and test and check the errors
# Set seed for reproducibility
set.seed(2)

# Create the training set index
train = sample(1:nrow(normalized_dataset), 30)

# Subset the test dataset correctly
normalized_dataset.test = normalized_dataset[-train, ]

# Fit the tree model using the training set
tree.normalized_dataset = tree(GDP_PC ~ EXP + COR + ELE + 
                                 INT + FDI + LAB + SFE + 
                                 UNE + URB, 
                               data = normalized_dataset, subset = train)

# we prune minimizing the misclassification error
cv.normalized_dataset = cv.tree(tree.normalized_dataset,FUN=prune.tree)
names(cv.normalized_dataset)
cv.normalized_dataset
#The misclassification error is lower between 4 and 5 size
plot(cv.normalized_dataset,
     main = "",
     cex.lab = 2,     # Increase axis label size
     cex.axis = 2,    # Increase axis tick label size
     cex.main = 1.8,    # Increase title size
     pch = 16,        # Use filled dots for residuals
     lwd = 2          # Increase line width
     )
title(main = "Tree Complexity Plot", line = 3, cex.main = 1.5)


#Set the prune depth level at 3 as suggested by CV misclassification error 
tree.normalized_dataset = prune.tree(tree.normalized_dataset, best = 3)
# Predict the test set
tree.pred = predict(tree.normalized_dataset, normalized_dataset.test, type = "vector")
# Extract the actual GDP_PC values from the test set
GDP_PC.test = normalized_dataset$GDP_PC[-train]

# Open a PNG device to save the plot
# Plot predicted vs actual GDP_PC values
plot(tree.pred, GDP_PC.test, 
     xlab = "Predicted GDP_PC", 
     ylab = "Actual GDP_PC", 
     main = "",
     cex.lab = 2,     # Increase axis label size
     cex.axis = 2,    # Increase axis tick label size
     cex.main = 1.8,    # Increase title size
     pch = 16,        # Use filled dots for residuals
     lwd = 2          # Increase line width
     )
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line for reference
title(main = "Predicted vs Actual GDP_PC", line = 3, cex.main = 1.5)

dev.off()

png("regression_tree_plot.png", width = 1600, height = 800)  # Adjust the width and height as needed

tree.GDP2 = ctree(GDP_PC ~ EXP + COR + ELE + 
                    INT + FDI + LAB + SFE + 
                    UNE + URB, 
                  data = normalized_dataset, 
                  control = ctree_control(maxdepth = 3))

# Plot the pruned tree with larger labels
plot(tree.GDP2,
     gp = gpar(fontsize = 20)  # Adjust fontsize as needed
)
dev.off()

#MSE 
mean((tree.pred - GDP_PC.test)^2)

#BAGGING AND RANDOM FOREST

#bagging
png("bagging_rf_prediction.png", width = 1600, height = 800)  # Adjust the width and height as needed
par(mfrow=c(1,2))

set.seed(1)
bag.normalized_dataset = randomForest(GDP_PC ~ EXP + COR + ELE + 
                                        INT + FDI + LAB + SFE + 
                                        UNE + URB, 
                                      data = normalized_dataset, subset = train, ntree=500, mtry=9, importance=TRUE)

bag.normalized_dataset 

# Predict GDP_PC using the bagging model
yhat.bag = predict(bag.normalized_dataset, newdata = normalized_dataset[-train,])

# Extract the actual GDP_PC values from the test set
GDP_PC.test = normalized_dataset$GDP_PC[-train]

# Plot predicted values vs actual values
plot(yhat.bag, GDP_PC.test, 
     main = "Predicted vs Actual GDP PC (Bagging)",
     xlab = "Predicted GDP PC", 
     ylab = "Actual GDP PC",
     cex.lab = 2,     # Increase axis label size
     cex.axis = 2,    # Increase axis tick label size
     cex.main = 1.8,    # Increase title size
     pch = 16,        # Use filled dots for residuals
     lwd = 2          # Increase line width
)
# Add a 45-degree reference line for better comparison
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line for reference

# Extract the actual GDP_PC values from the test set
GDP_PC.test = normalized_dataset$GDP_PC[-train]

# Calculate the mean squared error
mse = mean((yhat.bag - GDP_PC.test)^2)

# Print the mean squared error
mse


##random forest

rf.normalized_dataset = randomForest(GDP_PC ~ EXP + COR + ELE + 
                                       INT + FDI + LAB + SFE + 
                                       UNE + URB, 
                                     data = normalized_dataset, subset = train, mtry=sqrt(9), ntree=500, importance = TRUE)

yhat.rf = predict(rf.normalized_dataset, newdata=normalized_dataset[-train,])
# Extract the actual DEM_IND values from the test set
GDP_PC.test = normalized_dataset$GDP_PC[-train]

rf.normalized_dataset

# Calculate residuals
residuals = GDP_PC.test - yhat.rf

# Calculate standard deviation of residuals (used as error bars)
residual_sd = sd(residuals)

# Plot predicted values vs actual values
plot(yhat.rf, GDP_PC.test, 
     main = "Predicted vs Actual GDP PC (Random Forest)",
     xlab = "Predicted GDP PC", 
     ylab = "Actual GDP PC",
     cex.lab = 2,     # Increase axis label size
     cex.axis = 2,    # Increase axis tick label size
     cex.main = 1.8,    # Increase title size
     pch = 16,        # Use filled dots for residuals
     lwd = 2          # Increase line width
)

# Add a 45-degree reference line for better comparison
abline(0, 1, col = "red", lwd = 2)  # Add a 45-degree line for reference

# Extract the actual DEM_IND values from the test set
GDP_PC.test = normalized_dataset$GDP_PC[-train]

# Calculate the mean squared error
mse = mean((yhat.rf - GDP_PC.test)^2)

# Print the mean squared error
mse

dev.off()

png("bag_predictors_importance.png", width = 1600, height = 800)  # Adjust the width and height as needed
importance(bag.normalized_dataset)
varImpPlot(rf.normalized_dataset,
           pch = 16,        # Use filled dots for points
           cex = 1.5,       # Increase the size of the dots
           cex.lab = 1.5,   # Increase axis label size
           cex.axis = 1.2,  # Increase axis tick size
           cex.main = 1.8,
           main = ""
)
title(main = "Variable Importance Plot (Bagging)", cex.main = 2.5)  # Adjust the size here
dev.off()
png("rf_predictors_importance.png", width = 1600, height = 800)  # Adjust the width and height as needed
importance(rf.normalized_dataset)
varImpPlot(rf.normalized_dataset,
           pch = 16,        # Use filled dots for points
           cex = 1.5,       # Increase the size of the dots
           cex.lab = 1.5,   # Increase axis label size
           cex.axis = 1.2,  # Increase axis tick size
           cex.main = 1.8,
           main = ""
)
title(main = "Variable Importance Plot (Random Forest)", cex.main = 2.5)  # Adjust the size here
dev.off()

###oobr Graph
png("oobr.png", width = 1600, height = 800)  # Adjust the width and height as needed

# Increase the margins
par(mar = c(5, 5, 5, 5))  # Margins: bottom, left, top, right


# Initialize variables
oob.err = double(9)
test.err = double(9)

# Loop through mtry values
for (mtry in 1:9) {
  fit = randomForest(GDP_PC ~ EXP + COR + ELE + 
                       INT + FDI + LAB + SFE + 
                       UNE + URB, 
                     data = normalized_dataset, subset = train, mtry = mtry, ntree = 500, importance = TRUE)
  oob.err[mtry] = fit$mse[500]
  pred = predict(fit, normalized_dataset[-train, ])
  test.err[mtry] = with(normalized_dataset[-train, ], mean((GDP_PC - pred)^2))
  cat(mtry, " ")
}

# Plot with customized sizes
matplot(
  1:9, cbind(test.err, oob.err), 
  pch = 19, col = c("red", "blue"), 
  type = "b", 
  ylab = "Mean Squared Error", 
  xlab = "Number of Predictors at Each Split", 
  main = "OOB vs Test Error Across mtry Values", 
  cex.main = 2,  # Increase title size
  cex.lab = 3, # Increase axis label size
  cex.axis = 3, # Increase axis tick label size
  lwd = 2,  # Increase line width
  cex = 2  # Increase dot size
)

# Add a legend
legend(
  "topright", 
  legend = c("OOB", "Test"), 
  pch = 19, 
  col = c("red", "blue"), 
  cex = 1.5  # Increase legend text size
)

dev.off()

par(mfrow = c(1, 1))


############################################# Unsupervised Learning ############################################# 

#Scale Dataset
dataset_NEW <- dataset %>%
  select(-GDP_PC, -LAB, -FDI, -SFE, -ELE, -Country_Code, -DEM_FL)
# View the cleaned dataset
head(dataset_NEW)

apply(dataset_NEW, 2, mean)

dataset_scaled <- scale(dataset_NEW)
dataset_scaled <- dataset_scaled[!(rownames(dataset_scaled) %in% c("Djibouti", "Guinea")), ]
# View the modified dataset
head(dataset_scaled)

apply(dataset_scaled, 2, mean)

#-------------------------------------------- PCA --------------------------------------------------------

# Step 1: Perform PCA on the scaled dataset
pca_result <- prcomp(dataset_scaled, scale. = TRUE)

# Check the results
pca_summary <- summary(pca_result)

# View the principal components (the transformed data)
pca_result$x

# View the proportion of variance explained by each principal component
pca_result$sdev^2 / sum(pca_result$sdev^2)

# Plot the scree plot to visualize the variance explained
png("pca_result.png", width = 1000, height = 500)  # Adjust the width and height as needed

# Step 2: Extract information from PCA summary
pca_data <- data.frame(
  PC = paste0("PC", 1:length(pca_summary$importance["Proportion of Variance", ])),
  Proportion = as.numeric(pca_summary$importance["Proportion of Variance", ]),
  Cumulative = as.numeric(pca_summary$importance["Cumulative Proportion", ])
)

# Step 3: Create the plot
ggplot(pca_data, aes(x = PC)) +
  # Bar plot for Proportion of Variance
  geom_bar(aes(y = Proportion), stat = "identity", fill = "lightgray", width = 0.6) +
  # Line plot for Cumulative Proportion
  geom_line(aes(y = Cumulative, group = 1), color = "red", size = 1) +
  geom_point(aes(y = Cumulative), color = "red", size = 2) +
  # Add a vertical dashed line at PC3
  geom_vline(xintercept = 3, linetype = "dashed", color = "black", size = 0.8) +
  # Add a label for the cumulative proportion of variance at PC3 slightly above
  geom_label(
    data = pca_data[3, ],
    aes(x = 3, y = Cumulative + 0.05, label = sprintf("%.2f%%", Cumulative * 100)),  # Offset label slightly above
    fill = "white",  # White background for label
    color = "black",
    fontface = "bold"
  ) +
  # Axis labels and title
  labs(
    title = "Proportion of Variance and Cumulative Proportion by PCA Components",
    x = "Principal Components",
    y = "Proportion of Variance"
  ) +
  # Add secondary y-axis
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Cumulative Proportion")
  ) +
  # Customize the plot appearance
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),  # Remove grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black border
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

dev.off()


##PCA Biplot with vectors

# Step 1: Perform PCA on the scaled dataset
pca_result <- prcomp(dataset_scaled, scale. = TRUE)

# Step 2: Apply k-means on PCA results (using first two components)
set.seed(123) # For reproducibility
pca_coords <- pca_result$x[, 1:2]  # Extract first two principal components
pca_kmeans_res <- kmeans(pca_coords, centers = 3, nstart = 10)  # K-means on PCA data

# Prepare data for plotting PCA vectors (loadings)
loadings <- data.frame(PC1 = pca_result$rotation[, 1], PC2 = pca_result$rotation[, 2], 
                       Variable = rownames(pca_result$rotation))

# Step 3: Visualize PCA with Clustering and Vectors using factoextra and ggplot2
png("pca_biplot_cluster_with_vectors.png", width = 800, height = 800)

fviz_pca_ind(pca_result, 
             geom.ind = "point",  
             col.ind = factor(pca_kmeans_res$cluster),  
             palette = c(rgb(132/255, 184/255, 137/255), rgb(252/255, 237/255, 167/255), rgb(227/255, 138/255, 129/255)) ,
             #palette = "jco",  
             addEllipses = TRUE,  
             legend.title = "Cluster") +
  geom_text(aes(label = rownames(dataset_scaled)),  
            vjust = -0.3, size = 3, check_overlap = TRUE) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white")
  ) +
  labs(title = "PCA Biplot with K-Means Clustering and Vectors") +
  
  # Add PCA vectors
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 8, yend = PC2 * 8), 
               arrow = arrow(length = unit(0.2, "cm")), color = "blue") +
  geom_text(data = loadings, aes(x = PC1 * 9.5, y = PC2 * 9.5, label = Variable), 
            color = "blue",size = 3, vjust = -0.5)+
  xlim(c(-5, 5)) +  # Adjust x-axis limit
  ylim(c(-8, 4))    # Adjust y-axis limit

dev.off()

###PCA 3D Graph

# Perform PCA on the scaled dataset
pca_result <- prcomp(dataset_scaled, scale. = TRUE)

# Get the first three principal components
pca_data <- as.data.frame(pca_result$x[, 1:3])

# Add cluster information (assume 'clusters' contains cluster assignments)
pca_data$cluster <- as.factor(pca_kmeans_res$cluster)  # Replace 'kmeans_res$cluster' with your clustering result

# Create a 3D scatter plot
fig <- plot_ly(
  data = pca_data,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~cluster,
  #rgb(132/255, 184/255, 137/255), rgb(252/255, 237/255, 167/255), rgb(227/255, 138/255, 129/255)
  colors = c(rgb(132/255, 184/255, 137/255), rgb(252/255, 237/255, 167/255), rgb(227/255, 138/255, 129/255)),  # Custom colors for clusters
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 5,  # Adjust marker size
    line = list(
      color = "black",  # Border color
      width = 1.5       # Border width
    ))
)

# Customize the plot layout
fig <- fig %>%
  layout(
    title = "3D PCA Cluster Visualization",
    scene = list(
      xaxis = list(
        title = "PC1",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      ),
      yaxis = list(
        title = "PC2",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      ),
      zaxis = list(
        title = "PC3",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      )
    ),
    margin = list(l = 0, r = 0, b = 0, t = 50)  # Adjust margins
  )

# Display the plot
fig

#-------------------------------------------- K-Means Clustering --------------------------------------------------------

# 2D K-Means PCA Clustering:
# Determine the optimal number of clusters using Elbow Method
fviz_nbclust(dataset_scaled, kmeans, method = "wss") + 
  labs(title = "Elbow Method for Optimal K") 

# Visualizing the clusters
png("K-Means_cluster.png", width = 800, height = 800)

# Running K-Means with the chosen number of clusters (e.g., k = 4)
set.seed(123) # For reproducibility
kmeans_res <- kmeans(dataset_scaled, centers = 3, nstart = 100)
fviz_cluster(kmeans_res, data = dataset_scaled, 
             geom = "point",
             palette = c(rgb(227/255, 138/255, 129/255), rgb(132/255, 184/255, 137/255), rgb(252/255, 237/255, 167/255)),
             addEllipses = TRUE, # Disable ellipses
             ellipse.type = "t",
             show.clust.cent = TRUE, # Remove cluster centers
             outlier.color = "black",
             ggtheme = theme_minimal()) +
  geom_text(aes(label = rownames(dataset_scaled)), vjust = -1, size = 3, check_overlap = TRUE) +
  
  # Add dashed lines passing through the origin
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Horizontal dashed line at y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Vertical dashed line at x = 0
  
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white")  # Set background to white
  ) +
  labs(title = "K-Means Clustering Visualization") +
  xlim(c(-5, 5)) +  # Adjust x-axis limit
  ylim(c(-8, 4))    # Adjust y-axis limit

# 3D K-Means PCA Clustering:
# Set seed for reproducibility
set.seed(123)

# Step 1: Apply K-Means Clustering
kmeans_res <- kmeans(dataset_scaled, centers = 3, nstart = 100)

# Step 2: Apply PCA to reduce data to 3 dimensions
pca_res <- prcomp(dataset_scaled, scale. = TRUE)
pca_data <- data.frame(pca_res$x[, 1:3])  # Select the first 3 principal components
pca_data$Cluster <- as.factor(kmeans_res$cluster)
pca_data$Country <- rownames(dataset_scaled)  # Add country names for labeling

# Step 3: Create a 3D scatter plot
plot <- plot_ly(
  data = pca_data,
  x = ~PC1, y = ~PC2, z = ~PC3,
  color = ~Cluster,
  colors = c(rgb(227/255, 138/255, 129/255),rgb(132/255, 184/255, 137/255),rgb(252/255, 237/255, 167/255)),  # Custom colors for clusters
  text = ~Country,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(
    size = 5,  # Adjust marker size
    line = list(
      color = "black",  # Border color
      width = 1.5       # Border width
    ))
)

# Customize plot appearance
plot <- plot %>%
  layout(
    title = "K-Means Clustering Visualization in 3D",
    scene = list(
      xaxis = list(
        title = "PC1",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      ),
      yaxis = list(
        title = "PC2",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      ),
      zaxis = list(
        title = "PC3",
        showgrid = TRUE,
        gridwidth = 1,  # Reduce grid density
        zeroline = FALSE,
        showline = TRUE,  # Add external border
        linecolor = "black",
        linewidth = 2
      )
    ),
    margin = list(l = 0, r = 0, b = 0, t = 50)  # Adjust margins
  )
# Render the plot
plot

#-------------------------------------------- Agglomerative Hierarchical Clustering: --------------------------------------------------------

#  1. Complete Linkage:

# Hierarchical Clustering using Complete Linkage
hc_complete <- hclust(dist(dataset_scaled), method = "complete")

png("complete_hier.png", width = 1600, height = 800)
# Plot Dendrogram
fviz_dend(hc_complete, cex = 0.5, k = 3, # Cut tree into 4 clusters
          k_colors = "jco", rect = TRUE, rect_border = "jco", rect_fill = TRUE) +
  labs(title = "Hierarchical Clustering - Complete Linkage")
dev.off()

#2. Average Linkage:

# Hierarchical Clustering using Average Linkage
hc_average <- hclust(dist(dataset_scaled), method = "average")

png("average_hier.png", width = 1600, height = 800)
# Plot Dendrogram
fviz_dend(hc_average, cex = 0.5, k = 3, 
          k_colors = "npg", rect = TRUE, rect_border = "npg", rect_fill = TRUE) +
  labs(title = "Hierarchical Clustering - Average Linkage")
dev.off()

#3. Ward's Method:

# Hierarchical Clustering using Ward's Method
hc_ward <- hclust(dist(dataset_scaled), method = "ward.D2")

png("ward_hier.png", width = 1600, height = 800)
# Plot Dendrogram 
fviz_dend(hc_ward, cex = 1, k = 3, palette = c(rgb(227/255, 138/255, 129/255),rgb(132/255, 184/255, 137/255), rgb(252/255, 197/255, 80/255))) +
  labs(title = "Hierarchical Clustering - Ward's Linkage")
dev.off()

#Divisive Hierarchical Clustering (DIANA):

# Divisive Hierarchical Clustering
diana_res <- diana(dataset_scaled)

png("divisive_hier.png", width = 1600, height = 800)
# Plot Dendrogram for Divisive Clustering
fviz_dend(as.hclust(diana_res), cex = 0.5, k = 4, 
          k_colors = "uchicago", rect = TRUE, rect_border = "uchicago", rect_fill = TRUE) +
  labs(title = "Divisive Hierarchical Clustering (DIANA)")

dev.off()
