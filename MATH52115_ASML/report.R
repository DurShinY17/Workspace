## ----setup, include=FALSE------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------
set.seed(1)

library(tidyverse)

install.packages("randomForest")
library(randomForest)



## ------------------------------------------------------------------
hotel_data <- read.csv("hotels.csv")

colSums(is.na(hotel_data))
hotel_data_clean <- hotel_data %>% drop_na()

summary(hotel_data_clean)



## ------------------------------------------------------------------
# lead_time vs Cancellation_rate (boxplot)
ggplot(hotel_data_clean, aes(x = as.factor(is_canceled), 
                             y = lead_time)) +
  geom_boxplot(fill="lightblue") +
  theme_minimal() +
  labs(
    title = "Lead Time vs Cancellation",
    x = "Cancellation (0 = No, 1 = Yes)",
    y = "Lead Time (days)"
  )


# Presence of children vs Cancellation_rate (bar plot)
hotel_data_clean %>%
  mutate(children_flag = ifelse(children > 0, 
                                "Has Children", 
                                "No Children")) %>%
  group_by(children_flag) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = children_flag, 
             y = cancellation_rate, 
             fill = children_flag)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Children",
    x = "Children",
    y = "Cancellation Rate"
  )

# Presence of babies vs Cancellation_rate (bar plot)
hotel_data_clean %>%
  mutate(babies_flag = ifelse(babies > 0, 
                              "Has Babies", 
                              "No Babies")) %>%
  group_by(babies_flag) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = babies_flag, 
             y = cancellation_rate, 
             fill = babies_flag)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Babies",
    x = "Babies",
    y = "Cancellation Rate"
  )

# is_repeated_guest vs Cancellation_rate (bar plot)
hotel_data_clean %>%
  group_by(is_repeated_guest) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = factor(is_repeated_guest), 
             y = cancellation_rate)) +
  geom_bar(stat="identity", fill="lightgreen") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Repeat Guests",
    x = "Repeated Guest (0 = No, 1 = Yes)",
    y = "Cancellation Rate"
  )

# previous_cancellations vs Cancellation_rate (bar plot)
hotel_data_clean %>%
  group_by(previous_cancellations) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = factor(previous_cancellations), 
             y = cancellation_rate)) +
  geom_bar(stat="identity", fill="salmon") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Previous Cancellations",
    x = "Number of Previous Cancellations",
    y = "Cancellation Rate"
  )

# reserved_room_type vs Cancellation_rate (bar plot)
hotel_data_clean %>%
  group_by(reserved_room_type) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = reserved_room_type, 
             y = cancellation_rate)) +
  geom_bar(stat="identity", fill="orange") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Reserved Room Type",
    x = "Room Type",
    y = "Cancellation Rate"
  )

# Deposit type Cancellation_rate (bar plot)
hotel_data_clean %>%
  group_by(deposit_type) %>%
  summarise(cancellation_rate = mean(is_canceled)) %>%
  ggplot(aes(x = deposit_type, 
             y = cancellation_rate, 
             fill = deposit_type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Cancellation Rate by Deposit Type",
    x = "Deposit Type",
    y = "Cancellation Rate"
  )

library(dplyr)

# Previous Cancellations = 1 or more than 12
hotel_data_clean %>%
  filter(previous_cancellations == 1 | previous_cancellations >= 12) %>%
  group_by(previous_cancellations) %>%
  summarise(
    n_bookings = n(),
    cancellation_rate = mean(is_canceled)
  )

# Room Type = "P"
hotel_data_clean %>%
  filter(reserved_room_type == "P") %>%
  summarise(
    n_bookings = n(),
    cancellation_rate = mean(is_canceled)
  )

# Deposit Type = "Non Refund"
hotel_data_clean %>%
  filter(deposit_type == "Non Refund") %>%
  summarise(
    n_bookings = n(),
    cancellation_rate = mean(is_canceled)
  )



## ------------------------------------------------------------------
hotel_data_model <- hotel_data_clean %>%
  mutate(
    
    is_canceled = factor(is_canceled), 
    
    # Deposit type → categorical
    deposit_type = factor(deposit_type),
    
    # Previous cancellations: 0–11 and 12+
    previous_cancellations_cat = ifelse(previous_cancellations >= 12, 
                                        "12+", 
                                        as.character(previous_cancellations)),
    previous_cancellations_cat = factor(previous_cancellations_cat,
                                        levels = c(as.character(0:11), "12+")),
    
    # Babies: convert to presence/absence
    babies_flag = factor(ifelse(babies > 0, "Yes", "No")),
    
    # Repeated guest: convert to categorical
    is_repeated_guest = factor(is_repeated_guest,
                               levels = c(0,1),
                               labels = c("No","Yes"))
  ) %>%
  
  # Select variables used for modelling
  select(
    is_canceled,
    lead_time,
    babies_flag,
    is_repeated_guest,
    previous_cancellations_cat,
    deposit_type
  )


## ------------------------------------------------------------------
n <- nrow(hotel_data_model)

train_index <- sample(1:n, size = 0.8 * n)

train_data <- hotel_data_model[train_index, ]
test_data  <- hotel_data_model[-train_index, ]


## ------------------------------------------------------------------
log_model <- glm(
  is_canceled ~ .,
  data = train_data,
  family = binomial
)

thre_pred <- 0.5

log_prob <- predict(log_model, test_data, type = "response")
log_pred <- ifelse(log_prob > thre_pred, 1, 0)
log_pred <- factor(log_pred)

# accuracy
mean(log_pred == test_data$is_canceled)

# confusion matrix
table(Predicted = log_pred, Actual = test_data$is_canceled)


## ------------------------------------------------------------------
rf_model <- randomForest(
  is_canceled ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

# importance(rf_model)
varImpPlot(rf_model)

pred <- predict(rf_model, test_data)
mean(pred == test_data$is_canceled)

table(Predicted = pred, Actual = test_data$is_canceled)



## ------------------------------------------------------------------
knitr::purl("C_summative.Rmd", output = "report.R")

