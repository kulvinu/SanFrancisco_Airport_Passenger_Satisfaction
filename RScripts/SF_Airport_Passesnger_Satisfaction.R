install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")

library(dplyr)
library(ggplot2)
library(scales)

# Importing dataset
pass_info <- read.csv("assignment-2-k2443219.csv")

attach(pass_info)

# Custom color scale
cbp2 <- c("#CC79A7", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00")
cbp3 <- c("#648FFF", "#785EF0", "#DC267F", "#FE6100", 
          "#FFB000", "#009E73")

# Distribution of 'good'
ggplot(pass_info, aes(x = factor(good, levels = c(0, 1), labels = c("Not Good", "Good")), fill = factor(good, levels = c(0, 1), labels = c("Bad", "Good")))) +
  geom_bar() +
  scale_fill_manual(values = c('#FE6100','#009E73'), labels = c("Not Good", "Good")) + 
  scale_y_continuous(breaks = seq(0, 20000, by = 200)) + 
  labs(
    title = "Distribution of Satisfaction Levels of the SF Airport ",
    x = "Satisfaction Levels",
    y = "Count of Passengers"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )


# Distribution of 'dirty'
ggplot(pass_info, aes(x = dirty)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Reported Unclean Locations",
    x = "Number of Locations Reported as Dirty",
    y = "Count of Passengers"
  ) +
  theme_minimal()


# Distribution of 'wait'
ggplot(pass_info, aes(x = wait)) +
  geom_histogram(binwidth = 0.5, fill = "darkorange", color = "black") +
  labs(
    title = "Distribution of Time Spent Waiting at the Airport",
    x = "Wait Time (Hours)",
    y = "Count of Passengers"
  ) +
  theme_minimal()

# Distribution of Lastyear
ggplot(pass_info, aes(x = lastyear)) +
  geom_histogram(binwidth = 1, fill = "#CC79A7", color = "black") +
  labs(
    title = "Distribution of Flights from SFO in the Last Year",
    x = "Number of Flights in the Last Year",
    y = "Count of Passengers"
  ) +
  theme_minimal()

# Distribution of usa
ggplot(pass_info, aes(
  x = factor(usa, levels = c(0, 1), labels = c("International", "Domestic (USA)")),
  fill = factor(usa, levels = c(0, 1), labels = c("International", "Domestic (USA)"))
)) +
  geom_bar() +
  scale_fill_manual(values = c("#785EF0", "#DC267F"), labels = c("International", "Domestic (USA)")) +
  labs(
    title = "Distribution of Passengers by Destination Type",
    x = "Destination",
    y = "Count of Passengers",
    fill = "Destination Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

# Calculate descriptive statistics for each variable
descriptive_stats <- data.frame(
  Variable = c("good", "dirty", "wait", "lastyear", "usa"),
  Type = c("Binary", "Count", "Continuous", "Count", "Binary"),
  Statistics = c(
    # Percentages for binary variable 'good'
    paste0(round(mean(pass_info$good) * 100, 2), "% Good, ", round(mean(1 - pass_info$good) * 100, 2), "% Not Good"),
    # Summary for count variable 'dirty'
    paste0("Mean = ", round(mean(pass_info$dirty), 2), 
           ", SD = ", round(sd(pass_info$dirty), 2), 
           ", Median = ", median(pass_info$dirty), 
           ", Q1 = ", quantile(pass_info$dirty, 0.25), 
           ", Q3 = ", quantile(pass_info$dirty, 0.75)),
    # Summary for continuous variable 'wait'
    paste0("Mean = ", round(mean(pass_info$wait), 2), 
           ", SD = ", round(sd(pass_info$wait), 2), 
           ", Median = ", median(pass_info$wait), 
           ", Q1 = ", quantile(pass_info$wait, 0.25), 
           ", Q3 = ", quantile(pass_info$wait, 0.75)),
    # Summary for count variable 'lastyear'
    paste0("Mean = ", round(mean(pass_info$lastyear), 2), 
           ", SD = ", round(sd(pass_info$lastyear), 2), 
           ", Median = ", median(pass_info$lastyear), 
           ", Q1 = ", quantile(pass_info$lastyear, 0.25), 
           ", Q3 = ", quantile(pass_info$lastyear, 0.75)),
    # Percentages for binary variable 'usa'
    paste0(round(mean(pass_info$usa) * 100, 2), "% USA, ", round(mean(1 - pass_info$usa) * 100, 2), "% Not USA")
  )
)

# Print descriptive statistics
print(descriptive_stats)

summary(pass_info[c("dirty","wait", "lastyear")]) -> stats
print(stats)

ggplot(good_data, aes(x = factor(usa, labels = c("International", "Domestic")), y = wait, color = factor(good, labels = c("Good")))) +
  geom_jitter(width = 0.45, alpha = 0.7) +
  labs(x = "Destination",
       y = "Wait Time (Hours)",
       color = "Passenger Rating",
       title = "Wait Time by Destination where Passenger Rating was Good") +
  theme_minimal()

ggplot(pass_info, aes(x = factor(usa, labels = c("International", "Domestic")), y = wait, color = factor(good, labels = c("Not Good", "Good")))) +
  geom_jitter(alpha = 0.6) +
  facet_wrap(~good, labeller = labeller(good = c("0" = "Not Good", "1" = "Good"))) +
  labs(
    title = "Wait Time by Destination for Satisfied vs. Unsatisfied Passengers",
    x = "Destination",
    y = "Wait Time (Hours)",
    color = "Passenger Rating"
  ) +
  theme_minimal()

domestic_data <- subset(pass_info, usa == 1)
international_data <- subset(pass_info, usa == 0)










domestic_data$wait_group <- cut(
  domestic_data$wait,
  breaks = c(seq(0, 5, 1), 6),  
  labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"),  
  include.lowest = TRUE,  
  right = FALSE           
)
international_data$wait_group <- cut(
  international_data$wait,
  breaks = c(seq(0, 5, 1), 6),  
  labels = c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6"),  
  include.lowest = TRUE,  
  right = FALSE           
)

# Zoomed in view of Satisfaction of Domestic Flights by the Wait
ggplot(domestic_data, aes(x = wait_group, fill = factor(good, labels = c("Not Good", "Good")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Satisfaction Levels by Wait Time Range (Domestic)",
    x = "Wait Time Range (Hours)",
    y = "Percentage",
    fill = "Passenger Rating"
  ) +
  theme_minimal()

# Zoomed in view of Satisfaction of International Flights by the Wait
ggplot(international_data, aes(x = wait_group, fill = factor(good, labels = c("Not Good", "Good")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Satisfaction Levels by Wait Time Range (International)",
    x = "Wait Time Range (Hours)",
    y = "Percentage",
    fill = "Passenger Rating"
  ) +
  theme_minimal()

# Models

# Initial Model
initial_model <- glm(good ~ dirty + wait + lastyear + usa, data = pass_info, family = binomial)
summary(initial_model)
AIC(initial_model)

# Remove Last year
model_2 <- glm(good ~ dirty + wait + usa, data = pass_info, family = binomial)
summary(model_2)
AIC(model_2)

# Remove  Last year and usa
model_3 <- glm(good ~ dirty + wait, data = pass_info, family = binomial)
summary(model_3)
AIC(model_3)

# Interactions
model_4 <- glm(good ~ dirty*wait, data = pass_info, family = binomial)
summary(model_4)
AIC(model_4)

model_5 <- glm(good ~ dirty*lastyear, data = pass_info, family = binomial)
summary(model_5)
AIC(model_5)

model_6 <- glm(good ~ dirty+wait*usa, data = pass_info, family = binomial)
summary(model_6)

model_7 <- glm(good ~ dirty*wait*usa, data = pass_info, family = binomial)
summary(model_7)

good_data <- subset(pass_info, good == 1)
