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

bistro.subset <- starbucks %>%
  filter(type == "bistro box")

petite.subset <- starbucks %>%
  filter(type == "petite")

parfait.subset <- starbucks %>%
  filter(type == "parfait")

hb.subset <- starbucks %>%
  filter(type == "hot breakfast")

salad.subset <- starbucks %>%
  filter(type == "salad")

salad.subset

sandwich.subset <- starbucks %>%
  filter(type == "sandwich")

bakery.subset <- starbucks %>%
  filter(type == "bakery")

linear.bistro <- lm(calories~carb, data = bistro.subset)
summary(linear.bistro)
linear.petite <- lm(calories~carb, data = petite.subset)
summary(linear.petite)
linear.parfait <- lm(calories~carb, data = parfait.subset)
linear.hb <- lm(calories~carb, data = hb.subset)
linear.salad <- lm(calories~carb, data = salad.subset)
summary(linear.salad)
linear.sandwich <- lm(calories~carb, data = sandwich.subset)
linear.bakery <- lm(calories~carb, data = bakery.subset)

summary(linear.menuitems)
starbucks %>%
  ggplot(aes(x = carb, y = calories, color = type)) +
  geom_point() +
  geom_abline(intercept = linear.bistro$coefficients[[1]],
              slope = linear.bistro$coefficients[[2]],
              col = "#916114") +
  geom_abline(intercept = linear.petite$coefficients[[1]],
              slope = linear.petite$coefficients[[2]],
              col = "#06d7db") +
  geom_abline(intercept = linear.parfait$coefficients[[1]],
              slope = linear.parfait$coefficients[[2]],
              col = "#4add06") +
  geom_abline(intercept = linear.hb$coefficients[[1]],
              slope = linear.hb$coefficients[[2]],
              col = "#377c17") +
  geom_abline(intercept = linear.salad$coefficients[[1]],
              slope = linear.salad$coefficients[[2]],
              col = "#a905db") +
  geom_abline(intercept = linear.sandwich$coefficients[[1]],
              slope = linear.sandwich$coefficients[[2]],
              col = "#ff0284") +
  geom_abline(intercept = linear.bakery$coefficients[[1]],
              slope = linear.bakery$coefficients[[2]],
              col = "red") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title = element_text(hjust = 0.5)) +
  xlab("Carbohydrates") +
  ylab("Calories") +
  ggtitle("Linear Regression on Starbucks Menuitems") 




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


### baseline for tiny imagenet on hilo, 20 epochs
tiny_imagenet_baseline_times <- c(214.68288731575012, 211.12992477416992,
                                      210.81934261322021, 211.72019124031067,
                                      213.47818851470947, 211.5935640335083,
                                      213.569185256958, 212.35295462608337,
                                      212.81613421440125, 212.68782663345337,
                                      211.95569825172424, 212.20582628250122,
                                      212.48106789588928, 211.81737112998962,
                                      212.5187270641327, 212.26867079734802,
                                      211.73185539245605, 211.93052458763123,
                                      212.04156804084778, 213.07500457763672)

imagenet_no_outlier_mean <- mean(211.12992477416992,
     210.81934261322021, 211.72019124031067,
     213.47818851470947, 211.5935640335083,
     213.569185256958, 212.35295462608337,
     212.81613421440125, 212.68782663345337,
     211.95569825172424, 212.20582628250122,
     212.48106789588928, 211.81737112998962,
     212.5187270641327, 212.26867079734802,
     211.73185539245605, 211.93052458763123,
     212.04156804084778, 213.07500457763672)

base_epochs <- (1:20)
imagenet_baseline <- data.frame(tiny_imagenet_baseline_times, base_epochs)
linear_imagenet_baseline <- lm(tiny_imagenet_baseline_times~base_epochs, data = imagenet_baseline)
mean_imagenet_baseline_time <- mean(tiny_imagenet_baseline_times)
imagenet_baseline %>%
  ggplot(aes(x = as.factor(base_epochs), y = tiny_imagenet_baseline_times)) +
  geom_point() +
  scale_x_discrete(labels = imagenet_baseline$base_epochs) +
  geom_hline(yintercept=mean_imagenet_baseline_time, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Baseline AlexNet Training Times for Tiny ImageNet") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
                                         max(imagenet_baseline$tiny_imagenet_baseline_times), 
                                         length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
imagenet_baseline %>%
  ggplot(aes(y = tiny_imagenet_baseline_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Quantiles for AlexNet Baseline Trained on Tiny ImageNet") + 
  ylab("Time (in seconds)") 
  

summary(tiny_imagenet_baseline_times)


### baseline for cats vs dogs on kona 
cats_v_dogs_baseline_times <- c(267.71980571746826, 264.93071126937866,
                                264.719571352005, 264.73812222480774,
                                264.7255039215088, 264.67498540878296,
                                264.6265387535095, 264.66553950309753,
                                264.6624593734741, 264.64767622947693,
                                264.629114151001, 264.7116436958313,
                                264.707555770874, 264.674063205719,
                                264.68566155433655, 264.69661831855774,
                                264.6608073711395, 264.6624195575714,
                                264.7836937904358, 264.6882960796356)

cats_v_dogs_baseline <- data.frame(cats_v_dogs_baseline_times, base_epochs)
linear_cats_v_dogs_baseline <- lm(cats_v_dogs_baseline_times~base_epochs, 
                                  data = cats_v_dogs_baseline)
mean_cats_v_dogs_baseline_time <- mean(cats_v_dogs_baseline_times)
cats_v_dogs_baseline %>%
  ggplot(aes(x = as.factor(base_epochs), y = cats_v_dogs_baseline_times)) +
  geom_point() + 
  geom_hline(yintercept=mean_cats_v_dogs_baseline_time, linetype="dashed", 
             color = "red", size=1) +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Baseline AlexNet Training Times for Dogs vs. Cats") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  scale_y_continuous(breaks = sort(c(seq(min(cats_v_dogs_baseline$cats_v_dogs_baseline_times), 
                                         max(cats_v_dogs_baseline$cats_v_dogs_baseline_times), 
                                         length.out=5), mean(cats_v_dogs_baseline_times))))

cats_v_dogs_baseline %>%
  ggplot(aes(y = cats_v_dogs_baseline_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Quantiles for AlexNet Baseline Trained on Dogs vs. Cats") + 
  ylab("Time (in seconds)")

########### GCP baselines
######## Imagenet baseline
tiny_imagenet_baseline_times <- c(285.1113991737366,
                                  296.4805302619934,
                                  301.84294080734253,
                                  276.3925197124481,
                                  275.896409034729,
                                  276.6298336982727,
                                  276.121958732605,
                                  277.11504006385803,
                                  275.2559063434601,
                                  276.1726002693176,
                                  276.00473403930664,
                                  277.44313073158264,
                                  277.21570324897766,
                                  306.78744554519653,
                                  299.74441289901733,
                                  277.375137090683,
                                  277.5321755409241,
                                  277.32739901542664,
                                  274.518235206604,
                                  273.96184039115906)

imagenet_no_outlier_mean <- mean(tiny_imagenet_baseline_times)

base_epochs <- (1:20)
imagenet_baseline <- data.frame(tiny_imagenet_baseline_times, base_epochs)
linear_imagenet_baseline <- lm(tiny_imagenet_baseline_times~base_epochs, data = imagenet_baseline)
mean_imagenet_baseline_time <- mean(tiny_imagenet_baseline_times)
imagenet_baseline %>%
  ggplot(aes(x = as.factor(base_epochs), y = tiny_imagenet_baseline_times)) +
  geom_point() +
  scale_x_discrete(labels = imagenet_baseline$base_epochs) +
  geom_hline(yintercept=mean_imagenet_baseline_time, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Baseline AlexNet Training Times for Tiny ImageNet") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
  #scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
   #                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
    #                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_a <- imagenet_baseline %>%
  ggplot(aes(y = tiny_imagenet_baseline_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill = "#f4425f") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Baseline Quantiles") + 
  ylab("Time (in seconds)") 


summary(tiny_imagenet_baseline_times)


######### imagenet data parallelism
tiny_imagenet_data_par_times <- c(315.42944407463074,
                                  304.15195059776306,
                                  305.4780387878418,
                                  303.94845485687256,
                                  304.14166045188904,
                                  302.6108024120331,
                                  303.6155095100403,
                                  304.58258533477783,
                                  308.2988877296448,
                                  304.4963285923004,
                                  303.5239233970642,
                                  304.12168073654175,
                                  304.212566614151,
                                  302.0964448451996,
                                  303.83139872550964,
                                  303.6523599624634,
                                  304.4472236633301,
                                  305.3031096458435,
                                  303.82727885246277,
                                  302.82883310317993)

imagenet_data_par_mean <- mean(tiny_imagenet_data_par_times)

base_epochs <- (1:20)
imagenet_data_par <- data.frame(tiny_imagenet_data_par_times, base_epochs)
linear_imagenet_baseline <- lm(tiny_imagenet_data_par_times~base_epochs, data = imagenet_data_par)
mean_imagenet_baseline_time <- mean(tiny_imagenet_baseline_times)
imagenet_data_par %>%
  ggplot(aes(x = as.factor(base_epochs), y = tiny_imagenet_data_par_times)) +
  geom_point() +
  scale_x_discrete(labels = imagenet_data_par$base_epochs) +
  geom_hline(yintercept=imagenet_data_par_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Data Parallel") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_b <- imagenet_data_par %>%
  ggplot(aes(y = tiny_imagenet_data_par_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill="#02b200") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Data Parallel Quantiles") + 
  ylab("Time (in seconds)") 


summary(tiny_imagenet_data_par_times)


######### imagenet model parallelism
tiny_imagenet_model_par_times <- c(292.87947130203247,
                                  288.62062788009644,
                                  288.0639281272888,
                                  286.89507722854614,
                                  287.004118680954,
                                  284.3997530937195,
                                  286.88837242126465,
                                  285.38033294677734,
                                  286.589763879776,
                                  287.1137056350708,
                                  287.79371523857117,
                                  287.5744125843048,
                                  286.17095708847046,
                                  287.15358710289,
                                  285.8938641548157,
                                  287.5565917491913,
                                  286.2428529262543,
                                  287.1708381175995,
                                  285.4333441257477,
                                  286.0349895954132)

imagenet_model_par_mean <- mean(tiny_imagenet_model_par_times)

base_epochs <- (1:20)
imagenet_model_par <- data.frame(tiny_imagenet_model_par_times, base_epochs)
linear_imagenet_model_par <- lm(tiny_imagenet_model_par_times~base_epochs, data = imagenet_model_par)
mean_imagenet_baseline_time <- mean(tiny_imagenet_baseline_times)
imagenet_model_par %>%
  ggplot(aes(x = as.factor(base_epochs), y = tiny_imagenet_model_par_times)) +
  geom_point() +
  scale_x_discrete(labels = imagenet_model_par$base_epochs) +
  geom_hline(yintercept=imagenet_model_par_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Model Parallel AlexNet Training Times for Tiny ImageNet") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_c <- imagenet_model_par %>%
  ggplot(aes(y = tiny_imagenet_model_par_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill="#16b7fc") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Model Parallel Quantiles") + 
  ylab("Time (in seconds)") 

summary(tiny_imagenet_model_par_times)


###### OWT AlexNet with tiny imagenet
tiny_imagenet_owt_times <- c(311.92822647094727,
                                   297.27328085899353,
                                   295.75776624679565,
                                   297.3653335571289,
                                   306.72817182540894,
                                   298.9958996772766,
                                   302.17162895202637,
                                   301.73268461227417,
                                   300.51871490478516,
                                   292.7672712802887,
                                   293.4373700618744,
                                   293.61094522476196,
                                   292.528315782547,
                                   293.2643268108368,
                                   290.24955654144287,
                                   293.95406341552734,
                                   294.75524640083313,
                                   290.1531980037689,
                                   293.53870582580566,
                                   293.7157609462738)

imagenet_owt_mean <- mean(tiny_imagenet_owt_times)

base_epochs <- (1:20)
imagenet_owt <- data.frame(tiny_imagenet_owt_times, base_epochs)
linear_imagenet_owt <- lm(tiny_imagenet_owt_times~base_epochs, data = imagenet_owt)
mean_imagenet_owt_time <- mean(tiny_imagenet_owt_times)
imagenet_model_par %>%
  ggplot(aes(x = as.factor(base_epochs), y = tiny_imagenet_owt_times)) +
  geom_point() +
  scale_x_discrete(labels = imagenet_owt$base_epochs) +
  geom_hline(yintercept=imagenet_owt_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("OWT AlexNet Training Times for Tiny ImageNet") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_d <- imagenet_owt %>%
  ggplot(aes(y = tiny_imagenet_owt_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill = "#a717b5") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("OWT Quantiles") + 
  ylab("Time (in seconds)") 

summary(tiny_imagenet_owt_times)
##### grid arrange for quantiles
grid.arrange(plot_a, plot_b, plot_c, plot_d, nrow = 2)


#### Boxplot for summary of results
colnames(imagenet_owt) <- c("Times", "Epochs")
colnames(imagenet_baseline) <- c("Times", "Epochs")
colnames(imagenet_data_par) <- c("Times", "Epochs")
colnames(imagenet_model_par) <- c("Times", "Epochs")

Parallelism <- c(rep("Baseline", 20), rep("Data Parallel", 20), 
                 rep("Model Parallel", 20), rep("OWT", 20))

Times <- c(imagenet_baseline$Times, imagenet_data_par$Times,
           imagenet_model_par$Times, imagenet_owt$Times)


Epochs <- c(rep(1:20, 4))
total_imagenet <- data.frame(Epochs, Times, Parallelism)
total_imagenet
ggplot(data = total_imagenet, aes(x=Parallelism, y=Times)) + 
  geom_boxplot(aes(fill = Parallelism)) +
  ggtitle("Comparing Parallelism Methods for Tiny ImageNet") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=14, face = "bold"))
  

################### dogs vs cats

###### dogs vs cats baseline
pets_baseline_times <- c(273.1264386177063,
                         265.5733218193054,
                         266.690167427063,
                         267.64993596076965,
                         265.81146574020386,
                         265.9575345516205,
                         266.7438111305237,
                         265.63804817199707,
                         272.15925908088684,
                         271.42750358581543,
                         263.88380336761475,
                         265.2283556461334,
                         269.33322381973267,
                         263.3153643608093,
                         263.97815442085266,
                         265.01727747917175,
                         266.82183623313904,
                         262.85456895828247,
                         267.0085918903351,
                         263.5229957103729)

pets_baseline_mean <- mean(pets_baseline_times)

base_epochs <- (1:20)
pets_baseline <- data.frame(pets_baseline_times, base_epochs)
linear_pets_baseline <- lm(pets_baseline_times~base_epochs, data = pets_baseline)
mean_pets_baseline_time <- mean(pets_baseline_times)
pets_baseline %>%
  ggplot(aes(x = as.factor(base_epochs), y = pets_baseline_times)) +
  geom_point() +
  scale_x_discrete(labels = pets_baseline$base_epochs) +
  geom_hline(yintercept=pets_baseline_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Baseline AlexNet Training Times for Dogs vs. Cats") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_a <- pets_baseline %>%
  ggplot(aes(y = pets_baseline_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill = "#f4425f") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Baseline Quantiles") + 
  ylab("Time (in seconds)") 

summary(pets_baseline_times)

##### dogs vs cata model parallel
pets_model_par_times <- c(322.74258279800415,
                          317.8002679347992,
                          317.04869866371155,
                          316.5186538696289,
                          317.8608446121216,
                          318.3497989177704,
                          321.62047576904297,
                          325.7439742088318,
                          327.09818720817566,
                          323.0035252571106,
                          325.5974087715149,
                          319.86520075798035,
                          318.927964925766,
                          318.9944362640381,
                          319.56293177604675,
                          319.916543006897,
                          318.57092809677124,
                          317.8332691192627,
                          320.36946535110474,
                          320.6583230495453)

pets_model_par_mean <- mean(pets_model_par_times)

base_epochs <- (1:20)
pets_model_par <- data.frame(pets_model_par_times, base_epochs)
linear_pets_model_par <- lm(pets_model_par_times~base_epochs, data = pets_model_par)
mean_pets_baseline_time <- mean(pets_baseline_times)
pets_model_par %>%
  ggplot(aes(x = as.factor(base_epochs), y = pets_model_par_times)) +
  geom_point() +
  scale_x_discrete(labels = pets_model_par$base_epochs) +
  geom_hline(yintercept=pets_model_par_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Model Parallel AlexNet Training Times for Dogs vs. Cats") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_c <- pets_model_par %>%
  ggplot(aes(y = pets_model_par_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill="#16b7fc") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Model Parallel Quantiles") + 
  ylab("Time (in seconds)") 

summary(pets_model_par_times)


##### dogs vs cata data parallel
pets_data_par_times <- c(323.4444077014923,
                         314.34855127334595,
                         320.2664325237274,
                         322.54166865348816,
                         314.9359519481659,
                         314.78264594078064,
                         311.80594301223755,
                         312.3381028175354,
                         313.61301040649414,
                         311.99184465408325,
                         313.1800560951233,
                         313.7701108455658,
                         311.183132648468,
                         314.42223834991455,
                         313.5469627380371,
                         312.3278913497925,
                         309.1110484600067,
                         312.91374373435974,
                         315.65222549438477,
                         313.21239709854126)

pets_data_par_mean <- mean(pets_data_par_times)

base_epochs <- (1:20)
pets_data_par <- data.frame(pets_data_par_times, base_epochs)
linear_pets_data_par <- lm(pets_data_par_times~base_epochs, data = pets_data_par)
mean_pets_data_par_time <- mean(pets_data_par_times)
pets_model_par %>%
  ggplot(aes(x = as.factor(base_epochs), y = pets_data_par_times)) +
  geom_point() +
  scale_x_discrete(labels = pets_data_par$base_epochs) +
  geom_hline(yintercept=pets_data_par_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("Data Parallel AlexNet Training Times for Dogs vs. Cats") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_b <- pets_data_par %>%
  ggplot(aes(y = pets_data_par_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill="#02b200") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("Data Parallel Quantiles") + 
  ylab("Time (in seconds)") 

summary(pets_data_par_times)

##### dogs vs cata owt
pets_owt_times <- c(215.43526577949524,
                    209.57561612129211,
                    208.61965560913086,
                    210.07073831558228,
                    209.38343477249146,
                    211.34646129608154,
                    211.8912546634674,
                    211.25672793388367,
                    214.29262232780457,
                    211.42903852462769,
                    212.82750415802002,
                    211.56258058547974,
                    209.35528111457825,
                    208.49498200416565,
                    208.9939968585968,
                    209.68085980415344,
                    209.41107964515686,
                    208.32677006721497,
                    209.2557454109192,
                    209.1611044406891)

pets_owt_mean <- mean(pets_owt_times)

base_epochs <- (1:20)
pets_owt <- data.frame(pets_owt_times, base_epochs)
linear_pets_owt <- lm(pets_owt_times~base_epochs, data = pets_owt)
mean_pets_owt_time <- mean(pets_owt_times)
pets_owt %>%
  ggplot(aes(x = as.factor(base_epochs), y = pets_owt_times)) +
  geom_point() +
  scale_x_discrete(labels = pets_owt$base_epochs) +
  geom_hline(yintercept=pets_owt_mean, linetype="dashed", 
             color = "red", size=1) +
  #geom_abline(intercept = linear_imagenet_baseline$coefficients[[1]],
  #            slope = linear_imagenet_baseline$coefficients[[2]],
  #            col = "#42e5f4") +
  xlab("Epochs") +
  ylab("Time (in seconds)") +
  ggtitle("OWT AlexNet Training Times for Dogs vs. Cats") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12))
#scale_y_continuous(breaks = sort(c(seq(min(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                      max(imagenet_baseline$tiny_imagenet_baseline_times), 
#                                     length.out=5), mean(tiny_imagenet_baseline_times))))

# box plot of times for imagenet
plot_d <- pets_owt %>%
  ggplot(aes(y = pets_owt_times)) +
  geom_boxplot(color="black", outlier.colour="red", outlier.shape=8, fill = "#a717b5") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("OWT Quantiles") + 
  ylab("Time (in seconds)") 



summary(pets_owt_times)

## conclusive results
type_of_model <- c("Baseline", "Data Parallel", "Model Parallel", "OWT")
base_epochs
281.7
Data Parallel
304.7
Model Parallel
287.0*
  OWT
296.7

####### tiny imagenet totals
type_of_model <- c(rep("Baseline", 20), rep("Model Parallel", 20), 
                   rep("Data Parallel", 20), rep("OWT", 20))
length(type_of_model)
epochs <- c(rep(1:20, 4))
epoch_times <- c(tiny_imagenet_baseline_times, tiny_imagenet_model_par_times,
                 tiny_imagenet_data_par_times, tiny_imagenet_owt_times)
tiny_imagenet_totals <- data.frame(type_of_model, epochs, epoch_times)

tiny_imagenet_totals %>%
  ggplot(aes(x = as.factor(epochs), y = epoch_times, color = type_of_model)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("AlexNet Training Epochs with Tiny ImageNet") + 
  ylab("Time (in seconds)") + 
  xlab("Epochs") +
  labs(color='Type of Parallelism') 

#####  totals boxplot for ggplot 
colnames(pets_baseline) <- c("Times", "Epochs")
colnames(pets_data_par) <- c("Times", "Epochs")
colnames(pets_model_par) <- c("Times", "Epochs")
colnames(pets_owt) <- c("Times", "Epochs")

Parallelism <- c(rep("Baseline", 20), rep("Data Parallel", 20), 
                 rep("Model Parallel", 20), rep("OWT", 20))

Times <- c(pets_baseline$Times, pets_data_par$Times,
           pets_model_par$Times, pets_owt$Times)


Epochs <- c(rep(1:20, 4))
total_pets <- data.frame(Epochs, Times, Parallelism)
total_pets
ggplot(data = total_pets, aes(x=Parallelism, y=Times)) + 
  geom_boxplot(aes(fill = Parallelism)) +
  ggtitle("Comparing Parallelism Methods for Dogs vs. Cats") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size=14, face = "bold"))

###### dogs vs. cats totals
pets_epoch_times <- c(pets_baseline_times, pets_model_par_times, pets_data_par_times, pets_owt_times)
pets_totals <- data.frame(type_of_model, epochs, pets_epoch_times)

pets_totals %>%
  ggplot(aes(x = as.factor(epochs), y = pets_epoch_times, color = type_of_model)) +
  geom_point() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12)) +
  ggtitle("AlexNet Training Epochs with Dogs Vs. Cats") + 
  ylab("Time (in seconds)") + 
  xlab("Epochs") +
  labs(color='Type of Parallelism') 




### Citation for open intro
openintro.citation <- citation(package = "openintro", lib.loc = NULL)
## S3 method for class 'citation':
toBibtex(openintro.citation)
