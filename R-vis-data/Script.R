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
custom_colors <- c("red", "blue", "darkgreen", "violet", "yellow", "lightcoral", "lightskyblue", "green", "orange", "pink", "maroon", "gray", "lightgreen", "black")

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

# Sort data
df$Var2 <- reorder(df$Var2, df$Freq)
df$Region <- reorder(df$Region, -df$Freq)

# Create a grouped stacked bar graph using ggplot2
ggplot(data = df, aes(x = Region, y = Freq, fill = `Var2`)) +
  geom_bar(stat = "identity") +
  labs(title = "Responses Count by Regions and Campuses", x = "Region", y = "Count") +
  scale_fill_manual(values = custom_colors, name = "Campuses") +
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
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Number of Reponses Showing Primary Concerns for Eco-Friendly Seekers and Not Considered Eco-Friendly Options",
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
  scale_fill_manual(values = custom_colors) +
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
    main = "Proportions of Campuses Concerned by the Environmental Impact Current Products Have")

percentage_labels <- scales::percent(not_concerned_proportional_data$Proportion, scale = 100, accuracy = 0.01)
labels_with_percentage <- paste(not_concerned_proportional_data$Not_Concern_Category, percentage_labels)
pie(not_concerned_proportional_data$Proportion, 
    labels = labels_with_percentage,
    col = custom_colors,
    main = "Proportions of Campuses Not Concerned of the Environmental Impact Current Products Have")

# Visualize proportional data
concerned_proportional_data = concerned_proportional_data[order(-concerned_proportional_data$Proportion), ]
barplot(100*concerned_proportional_data$Proportion, 
        names.arg = concerned_proportional_data$Concern_Category, 
        main = "Proportions of Responses by  Campus Concerned by the Environmental Impact Current Products Have",
        xlab = "Campus",
        ylab = "Proportion",
        col = custom_colors)

not_concerned_proportional_data = not_concerned_proportional_data[order(-not_concerned_proportional_data$Proportion), ]
barplot(100*not_concerned_proportional_data$Proportion, 
        names.arg = not_concerned_proportional_data$Not_Concern_Category, 
        main = "Proportions of Responses by Campus Not Concerned by the Environmental Impact Current Products Have",
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
        main = "Comparing the Proportions of Responses by Campus in the Concerned and Not Concerned Categories of Current Products Environmental Impact", 
        xlab = "Category", 
        ylab = "Proportion",
        legend.text = c("Concerned", "Not Concerned"))

# Affordability Options
affordability_counts = table(data$Region, data$`How would you rate the affordability of the period products you currently use?`)
df_afford <- as.data.frame(affordability_counts)
df_afford$Region <- rownames(affordability_counts)

# Sort Values
df_afford$Var2 <- reorder(df_afford$Var2, df_afford$Freq)
df_afford$Region <- reorder(df_afford$Region, -df_afford$Freq)

# Plot
ggplot(data = df_afford, aes(x = Region, y = Freq, fill = `Var2`)) + 
  geom_bar(stat = "identity") +
  labs(title = "Products Affordability Responses Count by Regions", x = "Region", y = "Affordability Count") +
  scale_fill_manual(values = custom_colors, name = "Affordability") +
  theme_minimal()

affordability_counts = table(data$`Which Penn State campus are you from? (fill in your answer)`, data$`How would you rate the affordability of the period products you currently use?`)
df_afford_campus <- as.data.frame(affordability_counts)

# Sort Values
df_afford_campus$Var2 <- reorder(df_afford_campus$Var2, df_afford_campus$Freq)
df_afford_campus$Var1 <- reorder(df_afford_campus$Var1, -df_afford_campus$Freq)



# Plot
ggplot(data = df_afford_campus, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity") +
  labs(title = "Products Affordability Responses Count by Campus", x = "Campus", y = "Affordability Count") +
  scale_fill_manual(values = custom_colors, name = "Affordability") +
  theme_minimal()

# Product used
#' @param counts The table containing counts
#' @param bar_name The name of the plot
#' @param x The x-axis label
#' @param y The y-axis label
#' @param name The name of the legend
#'
#' @return Plot the stacked bar plot
compare_stacked_bars <- function(counts, bar_name, x, y, name) {
  # Create a stacked bar plot with custom colors
  df <- as.data.frame(counts)

  # Sort data
  df$Var2 <- reorder(df$Var2, df$Freq)
  df$Var1 <- reorder(df$Var1, -df$Freq)
  
  # Create a grouped stacked bar graph using ggplot2
  ggplot(data = df, aes(x = Var1, y = Freq, fill = `Var2`)) +
    geom_bar(stat = "identity") +
    labs(title = bar_name, x = x, y = y) +
    scale_fill_manual(values = custom_colors, name = name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Products Used by Campus
prod_used_campus = table(data$`Which type of period products do you typically use? (Select all that apply)`, data$`Which Penn State campus are you from? (fill in your answer)`)
compare_stacked_bars(prod_used_campus, "Products Used Responses Count by Campus", "Products Used Count","Campus Responses Count", "Campus")

# Products used vs Recommend Status
prod_used_recommend = table(data$`Which type of period products do you typically use? (Select all that apply)`, data$`How likely are you to recommend your preferred period products to other students?`)
compare_stacked_bars(prod_used_recommend, "Products Recommend Status Responses Count by Products Used", "Products Used", "Products Recommends Status Count", "Recommend Status")

# Products used vs Allergic Reactions
prod_used_allergy = table(data$`Which type of period products do you typically use? (Select all that apply)`, data$`Have you encountered any allergic reaction to any period product? (If yes, please specify your answer in "other" selection)`)
compare_stacked_bars(prod_used_allergy, "Products Allergic Reactions Responses Count by Products Used", "Products Used", "Products Allergic Reaction", "Allergic Reaction")

# Education and Stigma
educ_stigma = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`Did you receive any menstrual education before coming to college?`)
compare_stacked_bars(educ_stigma, "Experienced Menstral Education Before Campus and Experienced Period Stigma Responses", "Experienced Education Before Campus?", "Experienced Stigma Responses Count", "Experienced Stigma?")

# Stigma and Campus
stigma_campus = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`Which Penn State campus are you from? (fill in your answer)`)
compare_stacked_bars(stigma_campus, "Experienced Period Stigma Responses by Campuses", "Experienced Stigma Responses Count", "Campus count", "Campus")

plot_bar <- function(counts, bar_name, x, y, name){
  # Create a stacked bar plot
  df <- as.data.frame(counts)
  df <- df %>% filter(Var1 == "Yes")
  
  # Sort data
  df$Var2 <- reorder(df$Var2, -df$Freq)
  
  # Create a grouped stacked bar graph using ggplot2
  ggplot(data = df, aes(x = Var2, y = Freq, fill = `Var2`)) +
    geom_bar(stat = "identity") +
    labs(title = bar_name, x = x, y = y) +
    scale_fill_manual(values = custom_colors, name = name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Stigma and Recommend
stigma_recommend = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`How likely are you to recommend your preferred period products to other students?`)
compare_stacked_bars(stigma_recommend, "Products Recommend Status Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Products Recommend Status Count", "Recommend Status")

# Stigma and Affordability
stigma_afford = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`How would you rate the affordability of the period products you currently use?`)
compare_stacked_bars(stigma_afford, "Products Affordability Status Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Products Affordability Status Count", "Products Affordability Status")

# Stigma and Allergic Reactions
stigma_allergy = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`Have you encountered any allergic reaction to any period product? (If yes, please specify your answer in "other" selection)`)
compare_stacked_bars(stigma_allergy, "Products Allergic Reactions Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Products Allergic Reactions Count", "Products Allergic Reactions")

# Stigma and Reusable Products Learning
stigma_reusable_learn = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`Are you interested in learning more about reusable period products?`)
compare_stacked_bars(stigma_reusable_learn, "Interested to Learn about Reusable Products Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Interested to Learn about Reusable Products Count",  "Interested to Learn about Reusable Products")

# Stigma and Perception of Reusable Products
stigma_reusable_perception = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`How do you perceive the convenience of using reusable period products?`)
compare_stacked_bars(stigma_reusable_perception, "Perception of Reusable Products Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Perception of Reusable Products Responses Count", "Perception of Reusable Products")

# Stigma and Variety Products
stigma_variety = table(data$`Have you ever encountered period stigma before? Period stigma is a sense of shame or embarrassment about your menstrual period.`, data$`How important is it for you to have a variety of period product options available on campus?`)
compare_stacked_bars(stigma_variety, "Importance of Products Variety on Campus Responses vs Experienced Period Stigma Responses", "Experienced Period Stigma Response", "Importance of Variety Products on Campus Responses Count", "Importance of Variety Products on Campus")

