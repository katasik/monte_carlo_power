

library(tidyverse) # data transformation + viz
library(simstudy) # gamma parameters
library(broom) # getting standardized parameters

#Define model
learning_time ~ pep_effect + self_efficacy



#sample size
n<- 300

#Get number from previous study
#I need to take dispersion parameter from the gamma regression where the predictors were included
#and the mean will be the exponentiated intercept from the model

simstudy::gammaGetShapeRate(139.2624, 0.9015763)


#generate 300 random numbers for gamma distr.
learning_random <-
  rgamma(n = 300, # How many values we want? 
         rate = 0.007964594,
         shape =  1.109168) 

#plot it
hist(learning_random)


#generate 300 random numbers for normal distr. for implicit measure
pep_effect_random <-
  rnorm(n = 300, # How many values we want? 
        mean = 0,
        sd = 1) 

#plot it
hist(pep_effect_random)

#generate 300 random numbers for normal distr. for self-efficacy
self_efficacy_random <-
  rnorm(n = 300, # How many values we want? 
        mean = 0,
        sd = 1) 

#plot it
hist(self_efficacy_random)



# Create a prototype dataframe 
learning_df <-  
  tibble(id = gl(n = n, k = 1), # Generate the id (everyone has 1 measurement)
         #need to logtransform the outcome variable for the model
         intercept = log(learning_random), # Use the log of the random learning_random variable that we generated
         growth_mindset = pep_effect_random, # Use the random implicit measure variable that we generated
         self_efficacy = self_efficacy_random # Use the random self-efficacy variable that we generated
  ) %>% 
  # Add the effect size of growth mindset(.155) and self-efficacy(.159)
  mutate(log_learning_time = intercept+ growth_mindset * 0.155 + self_efficacy * 0.159,
         #need to exponentiate it back because the model will logtransform it
         learning_time = exp(log_learning_time))


#check back-transformed learning variable
hist(learning_df$learning_time)


#check model with prototype dataset
glm(learning_time~growth_mindset +self_efficacy, 
    family = Gamma(link = "log"),
    data=learning_df) %>% 
  summary()


# Create a function that generates a dataset based on theoretical parameters
# n: number of observations in a cell
# means: a vector of theoretical means for the predictors
# sds: a vector of theoretical sds for the predictors
# rate:  a value for the rate parameter of the gamma variable
# shape: a value for the shape parameter of the gamma variable
# effects: effects of predictors on outcomes

# These parameters are set here for testing purposes

n = 500
means = c(0, 0) # GM, self-efficacy
sds = c(1, 1)  # GM, self-efficacy
rate = 0.007419769 #outcome
shape =  1.056137 #outcome
effects_main = c(0.155, 0.159) # GM, self-efficacy


generate_dataset <- function(n, means, sds, shape, rate,
                             effects_main){
  
  # First I just define the variables
  # Then I add the sample size and the theoretical parameters
  tibble(id = gl(n = n, k = 1), # Generate the id (everyone has 1 measurement)
         intercept = log(rgamma(n = n, 
                rate = rate,
                shape =  shape)),
         growth_mindset = rnorm(n = n, 
                                  mean = means[1],
                                  sd = sds[1]),
         self_efficacy = rnorm(n = n, 
                               mean = means[2],
                               sd = sds[2])) %>% 
    # Then I generate data into the cells as nested list values
    mutate(log_learning_time = intercept+growth_mindset * effects_main[1] + self_efficacy * effects_main[2],
           learning_time = exp(log_learning_time)) 
  
}


## Let's verify that the function works as intended

learning_df <-
  generate_dataset(n = 1000,
                   means = c(0, 0), # GM, self-efficacy
                   sds = c(1, 1),  # GM, self-efficacy
                   rate = 0.007419769, #outcome
                   shape =  1.056137, #outcome
                   effects_main = c(0.155, 0.159)) # GM, self-efficacy


glm(learning_time~growth_mindset +self_efficacy, 
    family = Gamma(link = "log"),
    data=learning_df) %>% 
  summary()



# 3.Calculating statistical power

## Generating replications

#First, I generate multiple replications for each dataset, so I can simulate the randomness of the statistical processes.

sim_data <- 
  tibble(dataset = 1:50) %>% 
  # Now, I simply iterate through each line, and create several datasets
  mutate(data = map(dataset, 
                    ~generate_dataset(n = 1000,
                                      means = c(0, 0), # GM, self-efficacy
                                      sds = c(1, 1),  # GM, self-efficacy
                                      rate = 0.007419769, #outcome
                                      shape =  1.056137, #outcome
                                      effects_main = c(0.155, 0.159))))
sim_data
# Let's check what's in each individual dataset
unnest(sim_data, data)


# Now, I need to test the hypothesis on each generated dataset.
# Again, I can use the nested structure, so I will store the model for each dataset in a variable of the dataframe.


sim_result <-
  sim_data %>% 
  mutate(model = map(data, 
                     # Run the formula to test the hypothesis
                     ~glm(learning_time~growth_mindset +self_efficacy, 
                          family = Gamma(link = "log"),
                          data=learning_df) %>% 
                       # Put the results into a tidy dataframe
                       tidy()
  ))

# What's in the model variable? A tibble of statistical output
sim_result %>% 
  unnest(model)


# I can select the relevant row and check if the p value is below the significance level.
sim_result %>% 
  # Get the relevant p values out 
  mutate(p = map_dbl(model,
                     ~filter(.x, str_detect(term, "growth_mindset")) %>% 
                       pull(p.value)),
         # Generate a variable that evaluates if the significance is under the threshold
         sig = p <= .05) %>% 
  # I can now calculate the proportion when the H0 was rejected
  summarise(power = mean(sig))



# 4. Calculate power for different sample sizes


# To be able to create replications for each scenario that I test, I need to define the sample sizes, and also the number of repliactions for each sample size.
# The more datasets I have for each sample size, the more reliable our estimation gets.
# Each dataset should be numbered, just as each participant in the datasets.
# I can use `crossing()` function to do this.
# Crossing creates rows for every combinations of the specified variables in a dataset.
# 
# Let's define the minimum sample size as 30, and increase it until 300 with increments of 30.
# I can do this using the `seq()` function.


# These parameters define the parameter matrix

replications = 10000 # The number of replications 
min_sample = 30
max_sample = 300
sample_increment = 30

# These parameters define the datasets

means = c(0, 0) # GM, self-efficacy
sds = c(1, 1)  # GM, self-efficacy
rate = 0.007419769 #outcome
shape =  1.056137 #outcome
effects_main = c(0.155, 0.159) # GM, self-efficacy

# These parameters define the evaluation of the results
significance_level = .05 # The threshold for false positives (I will need this later)

# This is how the function call should look like:
multiple_n <-
  crossing(group_size = seq(from = min_sample, 
                            to = max_sample, 
                            by = sample_increment),
           replication = 1:replications) %>% 
  # Now, I simply iterate through each line, and create a dataset
  mutate(data = map(group_size, 
                    ~generate_dataset(n = .x,
                                      means = c(0, 0), # GM, self-efficacy
                                      sds = c(1, 1),  # GM, self-efficacy
                                      rate = 0.007419769, #outcome
                                      shape =  1.056137, #outcome
                                      effects_main = c(0.155, 0.159))))

multiple_n


## Calculate the required sample size
# 
# Now, I can calculate the statistical power for each sample size.
# Thus, I can learn how many participants are needed to achieve a certain statistical power.


multiple_result <-
  multiple_n %>% 
  mutate(model = map(data, 
                     # Run the formula to test the hypothesis
                     ~glm(learning_time~growth_mindset +self_efficacy, 
                          family = Gamma(link = "log"),
                          data = .x) %>% 
                       # Put the results into a tidy dataframe
                       tidy()
  ))



multiple_result 


# I can  select the term of interest that evaluates the hypothesis, and see if it is significant or not

multiple_power <-
  multiple_result %>% 
  # Extract the p value of the interaction from the model
  mutate(p = map_dbl(model,
                     ~filter(.x, str_detect(term, "growth_mindset")) %>% 
                       pull(p.value)),
         # Generate a variable that evaluates if the significance is under the threshold
         sig = p <= significance_level) %>% 
  # See the proportion when rejected the null hypothesis for each sample size. This is the statistical power!
  group_by(group_size) %>% 
  summarise(power = mean(sig))


# Visualize results
multiple_power %>% 
  ggplot() +
  aes(x = group_size, y = power) +
  geom_point() +
  geom_line(alpha = .7, size = 1.2) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  geom_hline(yintercept = .8, lty = "dashed", color = "red") +
  labs(x = "Sample size",
       title = "Statistical power as a function of sample size")


## Interpolate between sample sizes

# When I created the parameter matrix, I could have chosen to increase the sample size one-by-one, instead of using increments of 30.
# This would be very slow.
# But if I want to calculate the exact sample size to achieve 80% power, I can intrapolate between the values (just as the line interpolated on the figure).



interpolated_power <-
  # Create all possible sample sizes between the two extremes
  tibble(group_size = min_sample:max_sample) %>% 
  # Add existing data
  left_join(multiple_power, by = "group_size") %>% 
  # Use linear interpolation
  mutate(power = approx(x = group_size,
                        y = power,
                        xout = group_size)$y)


head(interpolated_power, 20)

# Calculate the exact number of participants needed in each cell for specific power
interpolated_power %>% 
  transmute(group_size,
            `>= .80` = power >= .80) %>% 
  pivot_longer(-group_size,
               names_to = "power") %>% 
  # Keep only the value where the power first surpasses the threshold
  filter(value) %>% 
  group_by(power) %>% 
  summarise(required_cell_size = first(group_size))



