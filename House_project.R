

#Load data
data = read.csv("~/Desktop/dissertation_file/house_data020.csv", header = T)


head(data)

## Check size of data
dim(data)

# Find missing values
is.na(data)

# Count missing values
sum(is.na(data))

colnames(data)
# Plot density of 'price' column
plot(density(data$price), main = "Density Plot of Price", xlab = "Price", ylab = "Density")

# Plot histogram of 'price' column
hist(data$price, main = "Histogram of Price", xlab = "Price", ylab = "Frequency")

# Plot density of log(price) column
plot(density(log(data$price)), main = "Density Plot of Log(Price)", xlab = "Log(Price)", ylab = "Density")

# Create histogram of log(price) column
hist(log(data$price), main = "Histogram of Log(Price)", xlab = "Log(Price)", ylab = "Frequency", breaks = "FD", col = "skyblue")

str(data)



# Convert categorical variables to factors 
data$house_type <- as.factor(data$house_type)
data$location <- as.factor(data$location)
data$constituency <- as.factor(data$constituency)
data$Area_Type <- as.factor(data$Area_Type)

# Check the correlation between price and other numeric variables
correlation <- cor(data[, c("price", "bedrooms", "bathrooms", "size", "borehole", "fibre_internet", "generator", 
                                  "staff_quarters", "swimming_pool", "gym")])

# Print the correlation matrix
print(correlation)

# Load the 'vcd' package for calculating Cramer's V
install.packages("vcd")
library(vcd)

# Define a function to calculate Cramer's V for association between categorical variables
cramer_v <- function(x, y) {
  chisq <- chisq.test(x, y)$statistic
  n <- sum(table(x))
  min_dim <- min(dim(table(x)))
  v <- sqrt((chisq/n) / (min_dim - 1))
  return(v)
}

# Calculate Cramer's V for association between categorical variables and "price"
categorical_variables <- c("house_type", "location", "constituency", "Area_Type")
cramer_v_values <- sapply(categorical_variables, function(var) {
  cramer_v(data[[var]], data$price)
})

# Print the Cramer's V values
names(cramer_v_values) <- categorical_variables
print(cramer_v_values)

# Function to calculate p-value using chi-square test
calculate_p_value <- function(x, y) {
  chisq_test <- chisq.test(x, y)
  return(chisq_test$p.value)
}

# Calculate p-values for association between categorical variables and "price"
p_values <- sapply(categorical_variables, function(var) {
  calculate_p_value(data[[var]], data$price)
})

# Print the p-values
names(p_values) <- categorical_variables
print(p_values)



# List of columns to include
columns_to_include <- c('bedrooms', 'size', 'bathrooms')

# Initialize an empty data frame to store results
result_df <- data.frame(
  Variables = character(length(columns_to_include)),
  Spearman_Coefficient = numeric(length(columns_to_include)),
  P_Value = numeric(length(columns_to_include)),
  SD = numeric(length(columns_to_include)),
  RMSE = numeric(length(columns_to_include))
)

# Loop through each column
for (i in seq_along(columns_to_include)) {
  col <- columns_to_include[i]
  
  # Calculate Spearman correlation coefficient
  spearman_result <- cor.test(data$price, data[[col]], method = "spearman")
  
  # Calculate Standard Deviation (SD)
  sd_val <- sd(data[[col]])
  
  # Calculate Root Mean Squared Error (RMSE)
  rmse_val <- sqrt(mean((data$price - data[[col]])^2))
  
  # Store results in the data frame
  result_df[i, ] <- list(
    Variables = col,
    Spearman_Coefficient = spearman_result$estimate,
    P_Value = spearman_result$p.value,
    SD = sd_val,
    RMSE = rmse_val
  )
}
print(result_df)

# Load necessary libraries
library(tibble)
library(dplyr)

# Assuming 'data' is your dataframe name
# Replace 'data' with your actual dataframe name

# Perform ANOVA test
anova_results <- aov(price ~ Area_Type + house_type + borehole + fibre_internet + generator + staff_quarters + swimming_pool + gym, data = data)

# Print ANOVA summary
summary_table <- summary(anova_results)



# Create a data.frame containing ANOVA results
anova_results <- data.frame(
  Df = c(4, 6, 1, 1, 1, 1, 1, 1, 3467),
  Sum_Sq = c(1.254e+18, 1.619e+18, 6.436e+15, 1.032e+14, 1.853e+16, 4.122e+16, 6.316e+15, 1.695e+15, 2.342e+18),
  Mean_Sq = c(3.135e+17, 2.699e+17, 6.436e+15, 1.032e+14, 1.853e+16, 4.122e+16, 6.316e+15, 1.695e+15, 6.755e+14),
  F_value = c(464.093, 399.562, 9.527, 0.153, 27.438, 61.018, 9.350, 2.509, NA),
  Pr_gt_F = c("< 2e-16", "< 2e-16", "0.00204", "0.69588", "1.72e-07", "7.44e-15", "0.00225", "0.11332", NA),
  row.names = c("Area_Type", "house_type", "borehole", "fibre_internet", "generator", "staff_quarters", "swimming_pool", "gym", "Residuals")
)

# Display summary of ANOVA results
summary(anova_results)


summary_table

data%>% select(!c(price, house_type, borehole,fibre_internet,generator,staff_quarters,
                  swimming_pool,gym))%>%
    tbl_summary(
      by=Area_Type
    )

data%>% select(!c(house_type, borehole,fibre_internet,generator,staff_quarters,
                  swimming_pool,gym))%>%
  tbl_summary(
    by=Area_Type
  )

#### RVF cases vs categorical variables of three levels and above (three levels and above please)



#============categorical variables(WORKING CODE)

#======================================================================================
#=========================================================================================

library(dplyr)
library(gt)

# Assuming 'data' is your data frame containing 'price', 'Area_Type', and 'house_type' columns

# Select relevant columns
df <- data %>% 
  dplyr::select(Area_Type, house_type, price)

# Remove rows with missing values
df <- df[complete.cases(df),]

# Initialize data frame to store results
selected_vars_overall_df <- data.frame(
  Variable = character(),
  chi_squared = numeric(),
  df = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each independent variable
for (var in names(df)[-3]) {  # Exclude 'price' column
  
  # Calculate the Kruskal-Wallis test between 'price' and the current variable
  corr_overall1 <- kruskal.test(df$price ~ df[, var])
  
  # Check if the p-value is NA
  if(is.na(corr_overall1$p.value)) next
  
  # Check if the test is significant (p-value < 0.05)
  if (corr_overall1$p.value < 0.05) {
    # If significant, add the variable and its results to the data frame
    selected_vars_overall_df <- rbind(
      selected_vars_overall_df,
      data.frame(
        Variable = var,
        chi_squared = corr_overall1$statistic,
        df = corr_overall1$parameter,
        p_value = corr_overall1$p.value,
        stringsAsFactors = FALSE
      )
    )
  }
}

# Sort the dataframe based on the chi-squared statistic
sorted_data_overall1 <- selected_vars_overall_df[order(selected_vars_overall_df$chi_squared, decreasing = TRUE),]

# Print the table using gt
gt(sorted_data_overall1, caption = "Selected significant categorical variables (>=3-levels)")



#=========================================================================================
#===========================================================================================
#The above output table provides the variables which are statistically significant with our output variable (PRICE). 
#These are the variables that will be included in developing the model. A total of 2 variables from the categorical 
#variables with three levels and above have been selected.  

####  price vs categorical variables of two levels
my_df2 <- data %>% dplyr::select(price, borehole, generator,staff_quarters,swimming_pool,gym)



#my_df2 <- my_df2[complete.cases(my_data_overall2),]

my_df2<- as.data.frame(my_df2)

#

#corr1 <- wilcox.test(my_data_cor1$No_Hu.Cs~my_data_cor1$OB_HU_LS) # use


selected_vars_overall2_df <- data.frame(Variable = character(),
                                        W  = numeric(),
                                        SE = numeric(),
                                        p_value = numeric(),
                                        stringsAsFactors = FALSE)

# Loop through each independent variable
for (var in names(my_df2)[-1]) {
  
  # Calculate the correlation coefficient and p-value between the variable and the dependent variable
  wilcox1 <- wilcox.test(my_df2$price~my_df2[,var], exact=FALSE, paired = FALSE, na.rm = TRUE)
  # check if the p-value is NA
  if(is.na(wilcox1$p.value)) next
  
  # Check if the correlation is significant (p-value < 0.05)
  if (wilcox1$p.value < 0.05) {
    # Calculate standard errors
    se <- aggregate(price ~ ., data = my_df2[, c("price", var)], function(x) sd(x) / sqrt(length(x)))
    # If significant, add the variable and its correlation coefficient and p-value to the data frame
    selected_vars_overall2_df <- rbind(selected_vars_overall2_df, data.frame(Variable = var,
                                                                             W = wilcox1$statistic,
                                                                             SE = se$price,
                                                                             p_value = wilcox1$p.value,
                                                                             stringsAsFactors = FALSE))
  }
}

# sort the dataframe based on the correlation coefficient
sorted_overall_data2 <- selected_vars_overall2_df[order(selected_vars_overall2_df$W,decreasing = TRUE),]



sorted_overall_data2 |> gt(caption = "Selected significant categorical variables (2-levels)")
sorted_overall_data2 

#### RVF cases vs numerical variables
##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


my_df3_cor <- data %>% dplyr::select(price, size, bedrooms, bathrooms)

#my_data_overall_cor2 <- my_data_overall_cor1[complete.cases(my_data_overall_cor2),]
my_df3_cor<- as.data.frame(my_df3_cor)


# Create an empty data frame to store the selected variables

selected_vars_overal_cases_df1 <- data.frame(Variable = character(),
                                             Speakman = numeric(),
                                             p_value = numeric(),
                                             stringsAsFactors = FALSE)

# Loop through each independent variable
for (var in names(my_df3_cor)[-1]) {
  
  # Calculate the correlation coefficient and p-value between the variable and the dependent variable
  corr_overal_cases2 <- cor.test(my_df3_cor[,var], my_df3_cor$price, method="spearman", exact=FALSE, na.rm=TRUE) #kendall
  # check if the p-value is NA
  if(is.na(corr_overal_cases2$p.value)) next
  
  # Check if the correlation is significant (p-value < 0.05)
  if (corr_overal_cases2$p.value < 0.05) {
    # If significant, add the variable and its correlation coefficient and p-value to the data frame
    selected_vars_overal_cases_df1 <- rbind(selected_vars_overal_cases_df1, data.frame(Variable = var,
                                                                                       SpeaKman = corr_overal_cases2$estimate,
                                                                                       p_value = corr_overal_cases2$p.value,
                                                                                       stringsAsFactors = FALSE))
  }
}

# sort the dataframe based on the correlation coefficient
sorted_overal_cases_data2 <- selected_vars_overal_cases_df1[order(selected_vars_overal_cases_df1$Speakman, 
                                                                  decreasing = TRUE),]


sorted_overal_cases_data2 |> gt(caption = "Selected significant numerical variables")

##=============================================================================================================================
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+BOOTSTRAP SE
library(dplyr)

# Assuming 'data' is your data frame containing 'price', 'borehole', 'generator', etc.

# Select relevant columns
my_df2 <- data %>% 
  dplyr::select(price, borehole, generator, staff_quarters, swimming_pool, gym)

# Remove rows with missing values
my_df2 <- my_df2[complete.cases(my_df2),]

# Initialize data frame to store results
selected_vars_overall2_df <- data.frame(
  Variable = character(),
  W = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Number of bootstrap iterations
n_bootstrap <- 1000

# Loop through each independent variable
for (var in names(my_df2)[-1]) {
  # Perform the Wilcoxon rank-sum test
  wilcox1 <- wilcox.test(my_df2$price ~ my_df2[, var], exact = FALSE, paired = FALSE, na.rm = TRUE)
  
  # Check if the p-value is NA
  if (is.na(wilcox1$p.value)) next
  
  # Perform bootstrapping
  boot_W <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    # Resample the data with replacement
    resampled_data <- my_df2[sample(nrow(my_df2), replace = TRUE), ]
    # Perform Wilcoxon rank-sum test on resampled data
    boot_W[i] <- wilcox.test(resampled_data$price ~ resampled_data[, var], exact = FALSE, paired = FALSE, na.rm = TRUE)$statistic
  }
  
  # Calculate standard error from bootstrapped statistics
  se <- sd(boot_W)
  
  # If significant, add the variable and its results to the data frame
  if (wilcox1$p.value < 0.05) {
    selected_vars_overall2_df <- rbind(
      selected_vars_overall2_df,
      data.frame(
        Variable = var,
        W = wilcox1$statistic,
        SE = se,
        p_value = wilcox1$p.value,
        stringsAsFactors = FALSE
      )
    )
  }
}

# sort the dataframe based on the Wilcoxon statistic
sorted_overall_data2 <- selected_vars_overall2_df[order(selected_vars_overall2_df$W, decreasing = TRUE),]

# Print the table
sorted_overall_data2|> gt(caption = "Selected significant numerical variables")
###&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library(dplyr)
library(gt)

# Assuming 'data' is your data frame containing 'price', 'size', 'bedrooms', and 'bathrooms' columns

# Select relevant columns
my_df3_cor <- data %>% 
  dplyr::select(price, size, bedrooms, bathrooms)

# Remove rows with missing values
my_df3_cor <- my_df3_cor[complete.cases(my_df3_cor),]

# Create an empty data frame to store the selected variables
selected_vars_overal_cases_df1 <- data.frame(
  Variable = character(),
  Speakman = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each independent variable
for (var in names(my_df3_cor)[-1]) {
  # Calculate the correlation coefficient and p-value between the variable and the dependent variable
  corr_overal_cases2 <- cor.test(my_df3_cor[, var], my_df3_cor$price, method = "spearman", exact = FALSE, na.rm = TRUE)
  # Check if the p-value is NA
  if (is.na(corr_overal_cases2$p.value)) next
  
  # Check if the correlation is significant (p-value < 0.05)
  if (corr_overal_cases2$p.value < 0.05) {
    # If significant, add the variable and its correlation coefficient and p-value to the data frame
    selected_vars_overal_cases_df1 <- rbind(
      selected_vars_overal_cases_df1,
      data.frame(
        Variable = var,
        Speakman = corr_overal_cases2$estimate,
        p_value = corr_overal_cases2$p.value,
        stringsAsFactors = FALSE
      )
    )
  }
}

# Convert 'Speakman' to numeric (in case it's not already)
selected_vars_overal_cases_df1$Speakman <- as.numeric(selected_vars_overal_cases_df1$Speakman)

# sort the dataframe based on the correlation coefficient
sorted_overal_cases_data2 <- selected_vars_overal_cases_df1[order(selected_vars_overal_cases_df1$Speakman, 
                                                                  decreasing = TRUE),]

# Print the table using gt
gt(sorted_overal_cases_data2, caption = "Selected significant numerical variables")
##{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
library(dplyr)
library(gt)

# Assuming 'data' is your data frame containing 'price', 'size', 'bedrooms', and 'bathrooms' columns

# Select relevant columns
my_df3_cor <- data %>% 
  dplyr::select(price, size, bedrooms, bathrooms)

# Remove rows with missing values
my_df3_cor <- my_df3_cor[complete.cases(my_df3_cor),]

# Number of bootstrap iterations
n_bootstrap <- 1000

# Create an empty data frame to store the selected variables
selected_vars_overal_cases_df1 <- data.frame(
  Variable = character(),
  Speakman = numeric(),
  p_value = numeric(),
  SE = numeric(),  # Placeholder for standard error
  stringsAsFactors = FALSE
)

# Loop through each independent variable
for (var in names(my_df3_cor)[-1]) {
  # Perform the Spearman correlation test
  corr_overal_cases2 <- cor.test(my_df3_cor[, var], my_df3_cor$price, method = "spearman", exact = FALSE, na.rm = TRUE)
  
  # Check if the p-value is NA
  if (is.na(corr_overal_cases2$p.value)) next
  
  # Perform bootstrapping
  boot_corr <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    # Resample the data with replacement
    resampled_data <- my_df3_cor[sample(nrow(my_df3_cor), replace = TRUE), ]
    # Perform Spearman correlation test on resampled data
    boot_corr[i] <- cor.test(resampled_data[, var], resampled_data$price, method = "spearman", exact = FALSE, na.rm = TRUE)$estimate
  }
  
  # Calculate standard error from bootstrapped statistics
  se <- sd(boot_corr)
  
  # Check if the correlation is significant (p-value < 0.05)
  if (corr_overal_cases2$p.value < 0.05) {
    # If significant, add the variable and its correlation coefficient, p-value, and standard error to the data frame
    selected_vars_overal_cases_df1 <- rbind(
      selected_vars_overal_cases_df1,
      data.frame(
        Variable = var,
        Speakman = corr_overal_cases2$estimate,
        SE = se,
        p_value = corr_overal_cases2$p.value,
        
        stringsAsFactors = FALSE
      )
    )
  }
}

# Print the table using gt
gt(selected_vars_overal_cases_df1, caption = "Selected significant numerical variables with standard errors")


###^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
library(dplyr)
library(gt)

# Assuming 'data' is your data frame containing 'price', 'Area_Type', and 'house_type' columns

# Select relevant columns
df <- data %>% 
  dplyr::select(Area_Type, house_type, price)

# Remove rows with missing values
df <- df[complete.cases(df),]

# Number of bootstrap iterations
n_bootstrap <- 1000

# Initialize data frame to store results
selected_vars_overall_df <- data.frame(
  Variable = character(),
  chi_squared = numeric(),
  df = numeric(),
  p_value = numeric(),
  SE = numeric(),  # Placeholder for standard error
  stringsAsFactors = FALSE
)

# Loop through each independent variable
for (var in names(df)[-3]) {  # Exclude 'price' column
  
  # Perform the Kruskal-Wallis test between 'price' and the current variable
  corr_overall1 <- kruskal.test(df$price ~ df[, var])
  
  # Check if the p-value is NA
  if(is.na(corr_overall1$p.value)) next
  
  # Check if the test is significant (p-value < 0.05)
  if (corr_overall1$p.value < 0.05) {
    # Perform bootstrapping
    boot_chi_squared <- numeric(n_bootstrap)
    for (i in 1:n_bootstrap) {
      # Resample the data with replacement
      resampled_data <- df[sample(nrow(df), replace = TRUE), ]
      # Perform Kruskal-Wallis test on resampled data
      boot_corr <- kruskal.test(resampled_data$price ~ resampled_data[, var])
      # Store the chi-squared statistic
      boot_chi_squared[i] <- boot_corr$statistic
    }
    
    # Calculate standard error from bootstrapped statistics
    se <- sd(boot_chi_squared)
    
    # If significant, add the variable and its results to the data frame
    selected_vars_overall_df <- rbind(
      selected_vars_overall_df,
      data.frame(
        Variable = var,
        chi_squared = corr_overall1$statistic,
        df = corr_overall1$parameter,
        SE = se,
        p_value = corr_overall1$p.value,
        
        stringsAsFactors = FALSE
      )
    )
  }
}

# Sort the dataframe based on the chi-squared statistic
sorted_data_overall1 <- selected_vars_overall_df[order(selected_vars_overall_df$chi_squared, decreasing = TRUE),]

# Print the table using gt
gt(sorted_data_overall1, caption = "Selected significant categorical variables (>=3-levels)")
###^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
library(dplyr)
library(gt)

# Assuming 'data' is your data frame containing 'price', 'borehole', 'generator', etc.

# Select relevant columns
my_df2 <- data %>% 
  dplyr::select(price, borehole, generator, staff_quarters, swimming_pool, gym,fibre_internet)

# Remove rows with missing values
my_df2 <- my_df2[complete.cases(my_df2),]

# Initialize data frame to store results
selected_vars_overall2_df <- data.frame(
  Variable = character(),
  W = numeric(),
  SE = numeric(),  # Placeholder for standard error
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Number of bootstrap iterations
n_bootstrap <- 1000

# Loop through each independent variable
for (var in names(my_df2)[-1]) {
  # Perform the Wilcoxon rank-sum test
  wilcox1 <- wilcox.test(my_df2$price ~ my_df2[, var], exact = FALSE, paired = FALSE, na.rm = TRUE)
  
  # Check if the p-value is NA
  if (is.na(wilcox1$p.value)) next
  
  # Perform bootstrapping
  boot_W <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    # Resample the data with replacement
    resampled_data <- my_df2[sample(nrow(my_df2), replace = TRUE), ]
    # Perform Wilcoxon rank-sum test on resampled data
    boot_W[i] <- wilcox.test(resampled_data$price ~ resampled_data[, var], exact = FALSE, paired = FALSE, na.rm = TRUE)$statistic
  }
  
  # Calculate standard error from bootstrapped statistics
  se <- sd(boot_W)
  
  # Add the variable and its results to the data frame
  selected_vars_overall2_df <- rbind(
    selected_vars_overall2_df,
    data.frame(
      Variable = var,
      W = wilcox1$statistic,
      SE = se,
      p_value = wilcox1$p.value,
      stringsAsFactors = FALSE
    )
  )
}

# sort the dataframe based on the Wilcoxon statistic
sorted_overall_data2 <- selected_vars_overall2_df[order(selected_vars_overall2_df$W, decreasing = TRUE),]

# Print the table
sorted_overall_data2 |> gt(caption = "Selected numerical variables with standard errors")

install.packages("spatialRF")


