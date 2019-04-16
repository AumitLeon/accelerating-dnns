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
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


