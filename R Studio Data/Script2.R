# Install required packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("psych")

# Import required packages
library(readxl)
library(magrittr)
library(dplyr)
library(graphics)
library(scales)
library(psych)
library(ggplot2)
library(tidyverse)

# Custom colors to use
custom_colors <- c("blue", "lightgreen", "lightpink", "lightyellow", "lightcoral", "lightskyblue", "green")

# Read and import the dataset as `data` from the excel file
data <- read_excel("data.xlsx")

# Visualize the whole dataset
View(data)

# Statistical summary of the data
summary(data)
describe(data)

campus <- 'Which Penn State campus are you from? (fill in your answer)'

# Rename Campus Names
# Change campus names to lower case to make it easier for analysis
data$`Which Penn State campus are you from? (fill in your answer)` <- tolower(data$`Which Penn State campus are you from? (fill in your answer)`)

# Penn State
data$`Which Penn State campus are you from? (fill in your answer)`[
  data$`Which Penn State campus are you from? (fill in your answer)` == 'psnk' | 
    data$`Which Penn State campus are you from? (fill in your answer)` == 'penn state new ken, will transfer to behrend' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'penn state new kensington' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'penn state new ken' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'new ken'] <- 'new kensington'

# Beaver
data$`Which Penn State campus are you from? (fill in your answer)`[
  data$`Which Penn State campus are you from? (fill in your answer)` == 'penn state beaver'] <- 'beaver'

# Behrend
data$`Which Penn State campus are you from? (fill in your answer)`[
  data$`Which Penn State campus are you from? (fill in your answer)` == 'beherend' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'erie-behrend' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'behrend' |
    data$`Which Penn State campus are you from? (fill in your answer)` == 'behrend (erie)'] <- 'erie'

# University Park
data$`Which Penn State campus are you from? (fill in your answer)`[
  data$`Which Penn State campus are you from? (fill in your answer)` == 'up'] <- 'university park'

# Hazleton
data$`Which Penn State campus are you from? (fill in your answer)`[
  data$`Which Penn State campus are you from? (fill in your answer)` == 'psu hazleton'] <- 'hazleton'

# Remove the one field in campus missing a record
data <- drop_na(data)

# Campuses by region
east_campuses <- c("philadelphia", "abington", "brandywine", "hazleton", "lehigh valley", "scranton", "wilkes barre", "world campus", "york")
west_campuses <- c("pittsburgh", "erie", "beaver", "fayette", "new kensington")
central_campuses <- c("university park", "altoona", "berks", "dubois", "greater allegheny", "harrisburg", "mont alto", "schuylkill", "shenango")

# Add a new column for Region
data$Region <- ifelse(data$`Which Penn State campus are you from? (fill in your answer)` %in% east_campuses, "East",
                    ifelse(data$`Which Penn State campus are you from? (fill in your answer)` %in% west_campuses, "West",
                           ifelse(data$`Which Penn State campus are you from? (fill in your answer)` %in% central_campuses, "Central", "Other")))
# Grouping Data by regions
data %>% 
  group_by(Region) %>% 
  summarise(count = n_distinct(Region))

# Showcase Number of responses based on the campus region
region_counts <- table(data$Region, data$`Which Penn State campus are you from? (fill in your answer)`)

# Create a stacked bar plot with custom colors
df <- as.data.frame(region_counts)
df$Region <- rownames(region_counts)

# Create a grouped stacked bar graph using ggplot2
ggplot(data = df, aes(x = Region, y = Freq, fill = `Var2`)) +
  geom_bar(stat = "identity") +
  labs(title = "Responses Count by Regions and Campuses", x = "Region", y = "Count") +
  scale_fill_discrete(name = "Campuses") +
  theme_minimal()

table(data$`Which Penn State campus are you from? (fill in your answer)`)

# Assessing number of responses to the question 'Which of the following apply to you?'
table(data$`Which of the following apply to you?`)

# Store data for the responses is 'I haven't considered making changes before.'
not_considered <- filter(data, `Which of the following apply to you?` == "I haven't considered making changes before.")
table(not_considered$`What is your primary concern when it comes to period products? (Select one)`)
not_considered_convinience <- table(not_considered$`How do you perceive the convenience of using reusable period products?`)

# Store data for the responses is 'I actively seek eco-friendly options.'
seek_friendly <- filter(data, `Which of the following apply to you?` == "I actively seek eco-friendly options.")
table(seek_friendly$`What is your primary concern when it comes to period products? (Select one)`)
seeking_friendly_convinience <- table(seek_friendly$`How do you perceive the convenience of using reusable period products?`)

# Create tables for the primary concerns in both groups
table_seek_friendly <- table(seek_friendly$`What is your primary concern when it comes to period products? (Select one)`)
table_not_considered <- table(not_considered$`What is your primary concern when it comes to period products? (Select one)`)

categories <- unique(c(names(table_seek_friendly), names(table_not_considered)))
combined_data <- data.frame(Concern_Category = categories)

# Initialize columns with counts from the tables
combined_data$Seek_Friendly <- 0
combined_data$Not_Considered <- 0

# Fill in the counts based on the tables
combined_data$Seek_Friendly[combined_data$Concern_Category %in% names(table_seek_friendly)] <- as.numeric(table_seek_friendly)
combined_data$Not_Considered[combined_data$Concern_Category %in% names(table_not_considered)] <- as.numeric(table_not_considered)

# Create a stacked bar plot
ggplot(data = combined_data, aes(x = Concern_Category)) +
  geom_bar(aes(y = Seek_Friendly, fill = "Seek Friendly"), stat = "identity", position = "identity") +
  geom_bar(aes(y = -Not_Considered, fill = "Not Considered"), stat = "identity", position = "identity") +
  scale_fill_manual(values = c("Seek Friendly" = "blue", "Not Considered" = "red")) +
  labs(
    title = "Primary Concerns for Eco-Friendly Seekers and Not Considered Eco-Friendly",
    x = "Primary Concern",
    y = "Count"
  ) +
  theme_minimal()


# Responses Comparison based on how individuals perceive eco-friendly options
counts_matrix <- data.frame(
  Category = rownames(seeking_friendly_convinience),
  Eco = seeking_friendly_convinience,
  NonEco = not_considered_convinience
)

ggplot(counts_matrix, aes(x = Category)) +
  geom_bar(data = counts_matrix, aes( y = Eco.Freq, fill = "Eco"), stat = "identity", position = "stack") +
  geom_bar(data = counts_matrix, aes( y = -NonEco.Freq, fill = "Non-eco"), stat = "identity", position = "stack") +
  scale_fill_manual(values = c("Eco" = "lightgreen", "Non-eco" = "lightcoral")) +
  labs(
    title = "Graph Comparing Perception of Eco-Friendly Period Products",
    x = "Categories",
    y = "Count"
  ) +
  theme_minimal()

# Sorting the dataset according to How do you feel about the environmental impact of your current period products?
sorted_data <- data[order(data$`How do you feel about the environmental impact of your current period products?`), ]


# Which campuses are dominant in the "Not Concerned" category?
not_concerned_counts <- table(sorted_data$`Which Penn State campus are you from? (fill in your answer)`[sorted_data$`How do you feel about the environmental impact of your current period products?` == "Not concerned"])
not_concerned_proportions <- prop.table(not_concerned_counts)

cat("Proportions in 'Not Concerned' category:\n")
print(not_concerned_proportions)
cat("\nCampus Dominant in 'Not Concerned' category:", names(not_concerned_proportions[which.max(not_concerned_proportions)]))

# Which campuses are most dominant in the "I am concerned" category?
concerned_counts <- table(sorted_data$`Which Penn State campus are you from? (fill in your answer)`[sorted_data$`How do you feel about the environmental impact of your current period products?` == "I am concerned."])
concerned_proportions <- prop.table(concerned_counts)

cat("\n\nProportions in 'I am concerned' category:\n")
print(concerned_proportions)
cat("\nCampus Dominant in 'I am concerned' category:", names(concerned_proportions[which.max(concerned_proportions)]))
    
# Tabulate the proportional data
concerned_proportional_data <- data.frame(
  Concern_Category = names(concerned_proportions),
  Proportion = as.numeric(concerned_proportions)
)
print(concerned_proportional_data)

not_concerned_proportional_data <- data.frame(
  Not_Concern_Category = names(not_concerned_proportions),
  Proportion = as.numeric(not_concerned_proportions)
)
print(not_concerned_proportional_data)

# Creating proportional visuals  
percentage_labels <- scales::percent(concerned_proportional_data$Proportion, scale = 100, accuracy = 0.01)
labels_with_percentage <- paste(concerned_proportional_data$Concern_Category, percentage_labels)
pie(concerned_proportional_data$Proportion, 
    labels = labels_with_percentage,
    col = custom_colors,
    main = "Proportions in 'Concerned' Category")

percentage_labels <- scales::percent(not_concerned_proportional_data$Proportion, scale = 100, accuracy = 0.01)
labels_with_percentage <- paste(not_concerned_proportional_data$Not_Concern_Category, percentage_labels)
pie(not_concerned_proportional_data$Proportion, 
    labels = labels_with_percentage,
    col = custom_colors,
    main = "Proportions in 'Not Concerned' Category")

# Visualize proportional data
concerned_proportional_data = concerned_proportional_data[order(-concerned_proportional_data$Proportion), ]
barplot(100*concerned_proportional_data$Proportion, 
        names.arg = concerned_proportional_data$Concern_Category, 
        main = "Concerned Proportional Data Visualization",
        xlab = "Campus",
        ylab = "Proportion",
        col = custom_colors)

not_concerned_proportional_data = not_concerned_proportional_data[order(-not_concerned_proportional_data$Proportion), ]
barplot(100*not_concerned_proportional_data$Proportion, 
        names.arg = not_concerned_proportional_data$Not_Concern_Category, 
        main = "Not Concerned Proportional Data Visualization",
        xlab = "Campus",
        ylab = "Proportion",
        col = custom_colors)

# Create a matrix of proportions for concerned and not concerned
concerned_proportions <- 100 * concerned_proportional_data$Proportion
not_concerned_proportions <- 100 * not_concerned_proportional_data$Proportion
proportions_matrix <- rbind(concerned_proportions, -not_concerned_proportions)

# Create a vector of labels for each category
categories <- not_concerned_proportional_data$Not_Concern_Category

# Create the stacked bar plot
barplot(proportions_matrix, 
        beside = TRUE, 
        col = custom_colors[1:2], 
        names.arg = categories, 
        main = "Proportional Data Visualization", 
        xlab = "Category", 
        ylab = "Proportion",
        legend.text = c("Concerned", "Not Concerned"))

