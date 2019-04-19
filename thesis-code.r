#### R CODE FOR THESIS
library(tidyverse)
library(gridExtra)

## Simple housing example
number_of_bedrooms <- c(10, 8, 6, 5, 1, 15)
number_of_bathrooms <- c(6, 5, 4, 3, 2, 8)
square_footage <- c(5000, 4000, 3000,2000, 1000, 9000)
price_of_houee <- c(500000, 400000, 300000,250000, 100000, 1000000)
house_data <- data.frame(number_of_rooms, square_footage, price_of_houee)
house_data

numb_bedrooms_trend <- house_data %>%
  ggplot(aes(x = number_of_bedrooms, y = price_of_houee )) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Number of Bedrooms") + 
  ylab("Price of Home")

numb_bathrooms_trend <- house_data %>%
  ggplot(aes(x = number_of_bathrooms, y = price_of_houee )) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Number of Bathrooms") + 
  ylab("Price of Home")


square_footage_trend <- house_data %>%
  ggplot(aes(x = square_footage, y = price_of_houee )) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Square Footage") + 
  ylab("Price of Home")

grid.arrange(numb_bedrooms_trend, numb_bathrooms_trend, square_footage_trend, nrow = 1)


# Simple Linear Regression
#Let's do a simple linear regression
model_house_price <- lm(price_of_houee ~ number_of_bedrooms, data = house_data)
model_house_price

#Let's make a scatterplot of fat vs calories with our model1 line
house_data %>%
  ggplot(aes(x = number_of_bedrooms, y = price_of_houee)) +
  geom_point() +
  geom_abline(intercept = model1$coefficients[[1]],
              slope = model1$coefficients[[2]],
              col = "red")



## Figure 2 -- random data to show how difficult it might be to untangle more complicated data
library(openintro)
View(starbucks)

starbucks %>%
  ggplot(aes(x = carb, y = calories, color = type)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Carbohydrates") +
  ylab("Calories") +
  ggtitle("Starbucks Menu Items Classified by Carbohydrates and Calories") 


##### Used in the text description on the need for higher order classifiers
# analyzing petite items
petite.items <- starbucks %>%
  filter(type == "petite")
petite.items
# 177.7778
mean(petite.items$calories)
# 23.33333
mean(petite.items$carb)

# analyzing bakery items
bistro.items <- starbucks %>%
  filter(type == "bistro box")
bistro.items

subset.starbucks <- starbucks %>%
  filter(type == "bistro box" | type == "petite")

# 377.5
mean(bistro.items$calories)

# 33.625
mean(bistro.items$carb)


linear.menuitems <- lm(calories~carb, data = subset.starbucks)
summary(linear.menuitems)
starbucks %>%
  filter(type == "petite" | type == "bistro box") %>%
  ggplot(aes(x = carb, y = calories, color = type)) +
  geom_point() +
  geom_abline(intercept = linear.menuitems$coefficients[[1]],
              slope = linear.menuitems$coefficients[[2]],
              col = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Carbohydrates") +
  ylab("Calories") +
  ggtitle("Classifying Starbucks' petite and bistro box items") 







### Graphing time over epochs
library(tidyverse)
library(ggplot2)
accuracy <- c( 0.547375, 0.58865625, 0.61796875,
               0.61634375, 0.6154375, 0.625125,
               0.62253125, 0.6290625, 0.64690625,
               0.64221875)

mean(times)

times <- c(517.086581945,
           502.528299093,
           505.651524782,
           503.983413935,
           506.136595964,
           505.855699062,
           505.65735507,
           504.123223066,
           503.380324125,
           507.194954157)

times_no_outlier <- ds %>%
  select(times) %>%
  filter(times < 515)

mean_times_no_outlier <- mean(as.numeric(times_no_outlier$times))

mean_times_no_outlier
epochs <- c(1:10)
ds <- data.frame(times, accuracy, as.factor(epochs))
ds

ds %>%
  ggplot(aes(x = as.factor(epochs), y = times)) +
  geom_point() + 
  geom_hline(yintercept=mean(times), linetype="dashed", 
             color = "red", size=1) + 
  geom_hline(yintercept=mean_times_no_outlier, 
             color = "red", size=1)

### Citation for open intro
openintro.citation <- citation(package = "openintro", lib.loc = NULL)
## S3 method for class 'citation':
toBibtex(openintro.citation)
