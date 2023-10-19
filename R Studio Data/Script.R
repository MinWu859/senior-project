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

# Read and import the dataset as `data` from the excel file
data <- read_excel("data.xlsx")

# Visualize the whole dataset
View(data)

# Statistical summary of the data
summary(data)
describe(data)

# Change campus names to lower case to make it easier for analysis
data$`Which Penn State campus are you from? (fill in your answer)` <- tolower(data$`Which Penn State campus are you from? (fill in your answer)`)

# Campuses by region
east_campuses <- c("philadelphia", "abington", "brandywine", "hazleton", "lehigh valley", "scranton", "wilkes-barre", "world campus", "york")
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
region_counts <- table(data$Region)
barplot(region_counts, main="Responses Count by Regions", xlab="Region", ylab="Count", col="skyblue", names.arg=names(region_counts))

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

# Responses Comparison based on how individuals perceive eco-friendly options
counts_matrix <- rbind(not_considered_convinience, seeking_friendly_convinience)
barplot(counts_matrix, beside = TRUE, col = c("lightblue", "lightgreen"),
        main = "Graph Comparing Perception of Eco-Friendly Period Products",
        legend.text = rownames(counts_matrix))

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
conserned_proportional_data <- data.frame(
  Concern_Category = names(concerned_proportions),
  Proportion = as.numeric(concerned_proportions)
)
print(conserned_proportional_data)

not_conserned_proportional_data <- data.frame(
  Not_Concern_Category = names(not_concerned_proportions),
  Proportion = as.numeric(not_concerned_proportions)
)
print(not_conserned_proportional_data)

# Creating proportional visuals  
percentage_labels <- scales::percent(conserned_proportional_data$Proportion, scale = 100, accuracy = 0.01)
labels_with_percentage <- paste(conserned_proportional_data$Concern_Category, percentage_labels)
pie(conserned_proportional_data$Proportion, 
    labels = labels_with_percentage, 
    main = "Proportions in 'Concerned' Category")

percentage_labels <- scales::percent(not_conserned_proportional_data$Proportion, scale = 100, accuracy = 0.01)
labels_with_percentage <- paste(not_conserned_proportional_data$Not_Concern_Category, percentage_labels)
pie(not_conserned_proportional_data$Proportion, 
    labels = labels_with_percentage, 
    main = "Proportions in 'Not Concerned' Category")

# Visualize proportional data
barplot(conserned_proportional_data$Proportion, 
        names.arg = conserned_proportional_data$Concern_Category, 
        main = "Concerned Proportional Data Visualization",
        xlab = "Campus",
        ylab = "Proportion",
        col = "skyblue")

barplot(not_conserned_proportional_data$Proportion, 
        names.arg = not_conserned_proportional_data$Not_Concern_Category, 
        main = "Not Concerned Proportional Data Visualization",
        xlab = "Campus",
        ylab = "Proportion",
        col = "skyblue")
