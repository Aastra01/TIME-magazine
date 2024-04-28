library(dplyr)
library(ggplot2)


df0 <- read.csv("/Users/sutingting/Documents/DH project/Time_bodyDisplay_modified2.csv", stringsAsFactors = TRUE)
df <- df0[, c(2, 4, 5, 7, 8,9)]
head(df)
View(df)
summary(df)

#1. Data Cleaning and Validation

# Convert 'year' to a numeric type if it's not already
df$year <- as.numeric(as.character(df$year))

# Check for missing values and remove or impute them
df <- df %>%
  filter(complete.cases(.))

# Check for any obvious data entry errors
summary(df)

#2.EDA

# Frequency of each variable
# Frequency of each variable
freq_category <- table(df$category)
freq_gender <- table(df$gender)
freq_age <- table(df$age)
freq_decade <- table(df$decade)
freq_display <- table(df$body.display)

# Plotting bar plots to visualize the distribution of each categorical variable
par(mfrow = c(3, 2))
barplot(freq_category, main = "Frequency of Categories", col = "cornflowerblue")
barplot(freq_gender, main = "Gender Distribution", col = "tomato")
barplot(freq_age, main = "Age Distribution", col = "pink")
barplot(freq_decade, main = "Decade Distribution", col = "orange")
barplot(freq_display, main = "Body Display Distribution", col = "skyblue")


#3.Chi-Square Tests
table1<- table(df$category,df$gender)
table1
chisq.test(table1)

table2<- table(df$decade,df$gender)
table2
chisq.test(table2)

table3<- table(df$age,df$gender)
table3
chisq.test(table3)

table4<- table(df$body.display,df$gender)
table4
chisq.test(table4)


par(mfrow = c(1, 1))
# Mosaic plots to explore relationships between gender and other variables
mosaicplot(table(df$category, df$gender), main = "Category vs Gender", col = c("tomato", "cornflowerblue"))
mosaicplot(table(df$decade, df$gender), main = "Decade vs Gender", col = c("tomato", "cornflowerblue"))
mosaicplot(table(df$age, df$gender), main = "Age vs Gender", col = c("tomato", "cornflowerblue"))
mosaicplot(table(df$body.display, df$gender), main = "Body Display vs Gender", col = c("tomato", "cornflowerblue"))




#4.MCA

install.packages("FactoMineR")
library(FactoMineR)
df_resampled

# Perform MCA on selected columns
mca_result1 <- MCA(df [, c("gender","age")], graph = FALSE)
mca_result2 <- MCA(df [, c("gender","body.display")], graph = FALSE)
mca_result3 <- MCA(df [, c("gender","category")], graph = FALSE)

#print(summary(mca_result1))
#print(summary(mca_result2))
#print(summary(mca_result3))

library(factoextra)
# Plot the results

fviz_mca_biplot(mca_result1, 
                repel = TRUE,  # Avoid label overlapping
                invisible = "ind",  # Hide individual points, show only variable points
                ggtheme = theme_minimal()) +  # Use a minimal theme
  ggtitle("MCA Biplot - Gender and Age") 

fviz_mca_biplot(mca_result2, 
                repel = TRUE,  # Avoid label overlapping
                invisible = "ind",  # Hide individual points, show only variable points
                ggtheme = theme_minimal()) +  # Use a minimal theme
  ggtitle("MCA Biplot - Gender and Body Display") 


fviz_mca_biplot(mca_result3, 
               repel = TRUE,  # Avoid label overlapping
               invisible = "ind",  # Hide individual points, show only variable points
               ggtheme = theme_minimal()) +  # Use a minimal theme
  ggtitle("MCA Biplot - Gender and Category") 



#5.Time-Series Analysis
install.packages("lattice")

# Count the number of male and female per year
df2 <- df %>%
  group_by(year) %>%
  summarise(male = sum(gender == 'male'),
            female = sum(gender == 'female'))
df2

# plot the counts over time

# Plot for Male
male_plot <- ggplot(df2, aes(x = year, y = male)) +
  geom_line(color = "grey40") +
  geom_smooth(method = "loess", se = FALSE, color = "cornflowerblue") +
  labs(title = "Male Representation Over Time with Trend",
       x = "Year",
       y = "Count") +
  theme_minimal()

print(male_plot)

# Plot for Female
female_plot <- ggplot(df2, aes(x = year, y = female)) +
  geom_line(color = "grey40") +
  geom_smooth(method = "loess", se = FALSE, color = "tomato1") +
  labs(title = "Female Representation Over Time with Trend",
       x = "Year",
       y = "Count") +
  theme_minimal()

print(female_plot)




#6.random forest

install.packages("caret")
install.packages("ROSE")
library(caret)
library(ROSE)
library(caret)
library(randomForest)


# Use "over" method for oversampling, setting p to 0.5 to balance gender proportions
df_over <- ovun.sample(gender ~ category + age + decade + body.display, 
                       data = df, 
                       method = "over", 
                       #N = 3000,  # Increase N value to attempt generating more samples
                       p = 0.5, 
                       seed = 123)

# Get the oversampled data
df_resampled <- df_over$data

# View the result
View(df_resampled)
# Ensure gender column is of factor type
df_resampled$gender <- as.factor(df_resampled$gender)



# Split data into training and testing sets
set.seed(123)  # Set random seed for reproducibility
trainIndex <- createDataPartition(df_resampled$gender, p = 0.8, list = FALSE)
trainData <- df_resampled[trainIndex, ]
testData <- df_resampled[-trainIndex, ]

# Train the random forest model
rf_model <- randomForest(gender ~ category + age + decade + body.display, 
                         data = trainData, 
                         importance = TRUE, 
                         mtry = 3, 
                         ntree = 800)

# Use the model to predict on the test set
predictions <- predict(rf_model, testData)

# Calculate accuracy
accuracy <- sum(predictions == testData$gender) / nrow(testData)
print(paste("Accuracy:", accuracy))

# Print model output
print(rf_model)

# Display variable importance
importance(rf_model)

# Visualize variable importance
varImpPlot(rf_model, main = "Variable Importance in Random Forest Model")








