#Part 2: Distributions and Functions

library(readr)

twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")

#hey so I just spent 6 hours doing MIDTERM_A WOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
#have mercy on me considering this fact plz

#Question 2a: Adult Bird Mass vs. Egg Mass

head(twoa)
names(twoa)
summary(twoa)

#Scatterplot cuz why not
plot(twoa[[2]] ~ twoa[[1]],
     main = "Adult Bird Mass vs. Egg Mass",
     xlab = "Adult Body Mass (g)",
     ylab = "Egg Mass (g)",
     pch = 16)

#Log-log plot
plot(twoa[[2]] ~ twoa[[1]],
     main = "Adult Bird Mass vs. Egg Mass (Log-Log)",
     xlab = "Adult Body Mass (g)",
     ylab = "Egg Mass (g)",
     log = "xy",
     pch = 16)

#Egg mass distribution
hist(twoa[[2]],
     main = "Distribution of Egg Mass",
     xlab = "Egg Mass (g)",
     col = "magenta")

#Log-transformed egg mass
hist(log(twoa[[2]]),
     main = "Distribution of Log Egg Mass",
     xlab = "Log Egg Mass",
     col = "magenta")

#POWER MODEL YESSS

#Power model using log-transformed variables
egg_power <- lm(log(twoa[[2]]) ~ log(twoa[[1]]))


summary(egg_power)

#Plot original observations (Yes I'm double running this just let me leave it plz)
plot(twoa[[2]] ~ twoa[[1]],
     main = "Adult Bird Mass vs. Egg Mass",
     xlab = "Adult Body Mass (g)",
     ylab = "Egg Mass (g)",
     pch = 16)

#Add fitted power curve
curve(exp(coef(egg_power)[1]) * x^coef(egg_power)[2],
      add = TRUE,
      col = "magenta",
      lwd = 2)
#looks right 

#Histogram
hist(residuals(egg_power),
     main = "Residual Distribution: Egg Mass Power Model",
     xlab = "Residuals (Log Scale)",
     col = "magenta",
     border = "white")

#Residuals against fitted values
plot(fitted(egg_power), residuals(egg_power),
     main = "Egg Mass: Residuals vs. Fitted",
     xlab = "Fitted Log(Egg Mass)",
     ylab = "Residuals",
     pch = 16)

#linemaxxing
abline(h = 0, col = "magenta")

#Function: Power function
#Distribution: Lognormal distribution (provisional)

#The relationship between adult bird body mass and egg mass seems
#to follow a power function in this case, since the eggs mass mostly increases as
#adult body mass increases, but at a slower rate. This seems logical given ecological trends we see. 
#The relationship becomes approximately linear when both variables
#are plotted on logarithmic axes, which supports an allometric
#scaling relationship. A lognormal distribution would be appropriate because egg mass is
#a positive, continuous variable that is strongly right-skewed.
#The distribution becomes considerably more symmetrical after
#logarithmic transformation, although some skewness remains.
#Examining residuals from the fitted power model will help determine
#whether a lognormal distribution adequately describes the variation
#around the relationship. The fitted curve appears to rise above several of the largest 
#observations, which could indicate that the power function doesn't describe the largest 
#species quite as accurately as smaller species, however the R squared value I checked looked strong


#Question 2b: Fish Abundance vs. Species Richness

head(twob)
names(twob)
summary(twob)

#Scatterplot of fish abundance vs. species richness
plot(twob[[2]] ~ twob[[1]],
     main = "Fish Abundance vs. Species Richness",
     xlab = "Total Fish Abundance",
     ylab = "Number of Species",
     pch = 16)

#Plot with logarithmic X-axis
plot(twob[[2]] ~ twob[[1]],
     main = "Fish Abundance vs. Species Richness (Log X)",
     xlab = "Total Fish Abundance",
     ylab = "Number of Species",
     log = "x",
     pch = 16)

#Species richness
hist(twob[[2]],
     main = "Distribution of Fish Species Richness",
     xlab = "Number of Species",
     col = "magenta",
     border = "white")

#Compare mean and variance
mean(twob[[2]])
var(twob[[2]])

#Linear model
fish_linear <- lm(twob[[2]] ~ twob[[1]])

#Logarithmic model
fish_log <- lm(twob[[2]] ~ log(twob[[1]]))

#Compare R-squared values
summary(fish_linear)$r.squared
summary(fish_log)$r.squared

#Compare AIC
AIC(fish_linear, fish_log)

#call me a beaver the way im boutta LOG

#Function: Logarithmic
#Distribution: Negative binomial

#The apparent relationship between fish abundance and species richness
#to me appears to follow a logarithmic function with species richness
#increasing rapidly at lower numbers before gradually leveling off.
#The logarithmic model explained alot of the variation,
#compared to smaller amount for the linear model. The logarithmic
#model also had a considerably lower AIC (1806.096 vs. 1866.273),
#supporting it as the better of the two functions tested. 
#Species richness is a discrete count variable, with a mean of 21.784
#and variance of 112.556. Since the variance is approximately 5.17
#times greater than the mean, the data show substantial marginal
#overdispersion, making a negative binomial distribution a reasonable
#candidate. However, some of this variation may be explained by
#differences in fish abundance among surveys, or seasonal discrepancies, so from a data
#standpoint, its fine, but could still have some bias to survey methods. 

#Question 2c: Rugosity vs. Hard-Bottom Coverage

head(twoc)
names(twoc)
summary(twoc)

#Scatterplot of rugosity vs. hard-bottom coverage
plot(twoc[[2]] ~ twoc[[1]],
     main = "Rugosity vs. Hard-Bottom Coverage",
     xlab = "Rugosity",
     ylab = "Hard-Bottom Coverage (%)",
     pch = 16)

#waitttttt I KNOW THIS DATA IVE READ ABOUT THIS BEFORE MUHAHAHAHAHA

#Distribution of hard-bottom coverage
hist(twoc[[2]],
     main = "Distribution of Hard-Bottom Coverage",
     xlab = "Hard-Bottom Coverage (%)",
     col = "magenta",
     border = "white")

#Where mah unique coverage values
sort(unique(twoc[[2]]))

#Check for zero and maximum coverage
sum(twoc[[2]] == 0)
sum(twoc[[2]] == 100)

#Compare mean and median
mean(twoc[[2]])
median(twoc[[2]])

#unclear results, gonna check some tests

#Linear model
rug_linear <- lm(twoc[[2]] ~ twoc[[1]])

summary(rug_linear)

#Plot linear relationship
plot(twoc[[2]] ~ twoc[[1]],
     main = "Rugosity vs. Hard-Bottom Coverage",
     xlab = "Rugosity",
     ylab = "Hard-Bottom Coverage (%)",
     pch = 16)

abline(rug_linear, col = "magenta", lwd = 2)

#Residuals vs. fitted values
plot(fitted(rug_linear), residuals(rug_linear),
     main = "Rugosity Model: Residuals vs. Fitted",
     xlab = "Fitted Hard-Bottom Coverage",
     ylab = "Residuals",
     pch = 16)

abline(h = 0, col = "magenta")

#Number of stations at 0% and 100% coverage
sum(twoc[[2]] == 0)
sum(twoc[[2]] == 100)

#Check linear model R-squared
summary(rug_linear)$r.squared

#Check model coefficients
coef(rug_linear)

#Function: Logarithmic 
#Distribution: Beta distribution with some limitations

#The relationship between rugosity and hard-bottom coverage appears
#to show a weak positive logarithmic trend, although there is considerable
#variation in coverage, particularly at lower rugosity values.
#A logarithmic function may be appropriate because hard-bottom coverage
#appears to increase more rapidly at lower rugosity values before
#leveling off as rugosity increases. However, the relationship is weak,
#and rugosity alone does not appear to be a strong predictor of coverage.
#The scatterplot also suggests that variation decreases as rugosity increases,
#meaning that the variance is not constant. The distribution of
#hard-bottom coverage is strongly left-skewed, with most observations
#concentrated near 100%. A beta distribution could be appropriate
#because coverage is a continuous percentage bounded between 0 and 100.
#However, the dataset contains 2 observations at 0% and 18 at 100%,
#which cannot be represented by a standard beta distribution.
#A zero-one-inflated beta distribution may therefore be more appropriate.

#Part 3: Statistical Power and Simulations

#Set seed for reproducibility
set.seed(123)

#Establish parameters for the simulation
n <- 25
intercept <- 2
slope <- 1
sd_noise <- 5

#Generate predictor and response variables
x <- runif(n, min = 0, max = 10)
y <- intercept + slope * x + rnorm(n, mean = 0, sd = sd_noise)

#Plot simulated linear relationship
plot(x, y,
     main = "Simulated Linear Relationship",
     xlab = "Predictor (x)",
     ylab = "Response (y)",
     pch = 16)

#Fit linear model
sim_model <- lm(y ~ x)

#Add regression line
abline(sim_model, col = "magenta", lwd = 2)

#Examine model summary
summary(sim_model)


#Now ima simulate the same relationship 1000 times
#to estimate statistical power

n_sims <- 1000

#Store p-values
p_values <- numeric(n_sims)

#Repeat simulation
for (i in 1:n_sims) {
  
  x <- runif(n, min = 0, max = 10)
  
  y <- intercept + slope * x +
    rnorm(n, mean = 0, sd = sd_noise)
  
  model <- lm(y ~ x)
  
  p_values[i] <- summary(model)$coefficients[2, 4]
  
}

#Calculate statistical power
power <- mean(p_values < 0.05)

power


#Now compare different sample sizes and noise levels

#Four sample sizes
sample_sizes <- c(10, 25, 50, 100)

#Standard deviations from 1 to 20
sd_values <- 1:20

#Create matrix to store statistical power
power_results <- matrix(
  NA,
  nrow = length(sd_values),
  ncol = length(sample_sizes)
)

#Run simulations across sample sizes and noise levels
for (j in 1:length(sample_sizes)) {
  
  n <- sample_sizes[j]
  
  for (k in 1:length(sd_values)) {
    
    sd_noise <- sd_values[k]
    
    #Store p-values
    p_values <- numeric(n_sims)
    
    #Repeat simulation
    for (i in 1:n_sims) {
      
      x <- runif(n, min = 0, max = 10)
      
      y <- intercept + slope * x +
        rnorm(n, mean = 0, sd = sd_noise)
      
      model <- lm(y ~ x)
      
      p_values[i] <- summary(model)$coefficients[2, 4]
      
    }
    
    #Calculate statistical power
    power_results[k, j] <- mean(p_values < 0.05)
    
  }
  
}

#View power results
power_results


#Plot statistical power for different sample sizes
matplot(sd_values, power_results,
        type = "l",
        lty = 1,
        lwd = 2,
        col = c("blue", "magenta", "red", "darkgreen"),
        main = "Effect of Sample Size and Noise on Statistical Power",
        xlab = "Standard Deviation (Noise)",
        ylab = "Statistical Power",
        ylim = c(0, 1))

#Add 80% power reference line
abline(h = 0.8, col = "black", lty = 2)

#Add legend
legend("topright",
       legend = paste("n =", sample_sizes),
       col = c("blue", "magenta", "red", "darkgreen"),
       lty = 1,
       lwd = 2)


#Figure 3. Statistical power across standard deviations ranging
#from 1 to 20 for sample sizes of 10, 25, 50, and 100.
#Power was calculated using 1,000 simulated linear regressions
#per combination, with significance determined at p < 0.05.
#The dashed horizontal line represents 80% statistical power.


#Conclusion

#The simulations show that statistical power decreases as the
#standard deviation of noise increases, while increasing sample
#size generally improves statistical power. At lower standard
#deviations, even smaller sample sizes can detect the relationship
#fairly consistently. However, as noise increases, larger sample
#sizes are needed to maintain sufficient statistical power.
#The sample size of 100 maintained approximately 80% power
#at a standard deviation of 10, while smaller sample sizes
#performed considerably worse. In this case, halving the standard
#deviation of noise would likely be more beneficial than doubling
#sample size, since reducing noise has a greater effect on the
#precision of the estimated relationship.

