install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Import the data
df <- read.csv("C:/Users/amban/OneDrive/Desktop/amban details/Diwali Sales Data.csv", encoding = "UTF-8")

# Data Cleaning
df <- df %>%
  select(-Status, -unnamed1) %>%
  drop_na() %>%
  mutate(Amount = as.integer(Amount))
print(df)

# Exploratory Data Analysis
# Gender Analysis
gender_count <- df %>%
  count(Gender)
ggplot(gender_count, aes(x = Gender, y = n, fill = Gender)) +
  geom_bar(stat = "identity",color = "black", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5,color = "white", size = 5)

gender_sales <- df %>%
  group_by(Gender,Marital_Status) %>%
  summarize(Total_Amount = sum(Amount))
ggplot(gender_sales, aes(x = Gender, y = Total_Amount,fill = Marital_Status)) +
  geom_bar(stat = "identity",position = "dodge") +
  geom_text(aes(label = Total_Amount), vjust = -0.5,position = position_dodge(width = 0.9)) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Total Amount by Gender and Marital Status",
       x = "Gender",
       y = "Total Amount")


# Age Group Analysis
age_count <- df %>%
  count(`Age.Group`, Gender)
ggplot(age_count, aes(x = `Age.Group`, y = n, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n), position = position_dodge(width = 1), vjust = -0.5)

age_sales <- df %>%
  group_by(`Age.Group`) %>%
  summarize(Total_Amount = sum(Amount))
ggplot(age_sales, aes(x = `Age.Group`, y = Total_Amount,fill =Age.Group )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Amount), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(title = "Total Amount by Age Group and Total Amount",
       x = "Age Group",
       y = "Total Amount")

# State Analysis
state_orders <- df %>%
  group_by(State) %>%
  summarize(Total_Orders = sum(Orders)) %>%
  top_n(10, wt = Total_Orders)


ggplot(state_orders, aes(x = reorder(State, -Total_Orders), y = Total_Orders,fill =State )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Orders), vjust = -0.5) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10)
  )

state_sales <- df %>%
  group_by(State) %>%
  summarize(Total_Amount = sum(Amount)) %>%
  top_n(10, wt = Total_Amount)
ggplot(state_sales, aes(x = reorder(State, -Total_Amount), y = Total_Amount,fill =State )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Amount), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by state and Total Amount",
       x = "state",
       y = "Total Amount")

# Marital Status Analysis
marital_count <- df %>%
  count(Marital_Status)
ggplot(marital_count, aes(x = Marital_Status, y = n,fill =Marital_Status )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5)+
  theme_minimal() +
  labs(title = "Maternal Status",
       x = "Maternal Status",
       y = "Number")

marital_sales <- df %>%
  group_by(Marital_Status, Gender) %>%
  summarize(Total_Amount = sum(Amount))
ggplot(marital_sales, aes(x = Marital_Status, y = Total_Amount, fill = Gender)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Total_Amount), position = position_dodge(width = 1), vjust = -0.5)

# Occupation Analysis
occupation_count <- df %>%
  count(Occupation)
ggplot(occupation_count, aes(x = reorder(Occupation, -n), y = n,fill =Occupation )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by Occupationt",
       x = "Occupation",
       y = "Number")

occupation_sales <- df %>%
  group_by(Occupation) %>%
  summarize(Total_Amount = sum(Amount))
ggplot(occupation_sales, aes(x = reorder(Occupation, -Total_Amount), y = Total_Amount,fill = Occupation)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Amount), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by Occupationt",
       x = "Occupation",
       y = "number")

# Product Category Analysis
product_count <- df %>%
  count(Product_Category)
ggplot(product_count, aes(x = reorder(Product_Category, -n), y = n,fill =Product_Category )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by Product Category",
       x = "Product Category",
       y = "Number")

product_sales <- df %>%
  group_by(Product_Category) %>%
  summarize(Total_Amount = sum(Amount)) %>%
  top_n(10, wt = Total_Amount)
ggplot(product_sales, aes(x = reorder(Product_Category, -Total_Amount), y = Total_Amount,fill=Product_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Amount), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by Product Category",
       x = "Product Category",
       y = "number")


# Top Sold Products
top_products <- df %>%
  group_by(Product_ID) %>%
  summarize(Total_Orders = sum(Orders)) %>%
  top_n(10, wt = Total_Orders)
ggplot(top_products, aes(x = reorder(Product_ID, -Total_Orders), y = Total_Orders,fill=Product_ID)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Total_Orders), vjust = -0.5)+
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,size = 10))+
  labs(title = "Total Amount by Product Category",
       x = "Product Category",
       y = "number")

# Conclusion
# Married women aged 26-35 from UP, Maharashtra, and Karnataka, working in IT, Healthcare, and Aviation, are the primary buyers of Food, Clothing, and Electronics during Diwali.
