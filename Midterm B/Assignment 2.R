#Zachary Gray 
#PCB 6455 / BSC 4933: Biometry
#Dr. Brian Maitner
#10/09/2026
 #Assignment 2: Distributions

#Part 1: Hypothetical Distributions

# 1. Number of surviving individuals within a forest plot
#A.Discrete
#B.Binomial distribution
# Each individual either survives or dies, counted in whole numbers, making it discrete.
# Assuming that independent survival events have equal 
# probabilities, I'd use a binomial distribution.

# 2. Species abundance (counts of individuals in a plot)
#A.Discrete
#B.Poisson distribution
# Discrete, because individuals are counted in whole
# numbers. I'd pick a Poisson distribution because it models the
# number of individuals within a defined area well, and can assume independent
# occurrences/constant average density.

# 3. Pollinator visitation rate (visits per flower per unit time)
#A.Continuous
#B.Gamma distribution
# Visitation rate is treated as continuous because it measures visits
# per unit time rather than a whole number count, which would likely result in a decimal. 
# I'd use a Gamma distribution since positive visitation rates may vary among different flowers.
# if we have that data at least.... 

# 4. Ages (in years) of surviving individuals within a reserve
#A. Discrete
#B. Gamma distribution
# Age is likely discrete because counting in years, however could be converted to continuous if they counted
# fractional ages. A Gamma distribution I think is appropriate because ages are nonnegative
# and may be right-skewed depending on our taxa, with fewer individuals surviving to older ages.

# 5. Body size (e.g., body mass, length)
#A. Continuous
#B. Lognormal distribution
# Body size is continuous because measurements such as mass and length
# can take fractional values. I'd use a lognormal distribution for sizes in general
# because they're always positive and in my cases often right-skewed. 

# 6. Biomass per unit area
#A. Continuous
#B. Lognormal distribution
# Continuous because it is measured not counted. 
# I think a lognormal distribution is appropriate because biomass
# is also going to be positive, and usually plots are semi-consistent, with relatively few
# plots containing exceptionally high biomass unless there is a clear abiotic reasoning.

# 7. Time to germination
#A. Continuous
#B. Weibull distribution
# Time to germination is continuous because it is elapsed time. 
# A Weibull distribution is appropriate
# because germination times are positive and the probability can change over time.
# Could also do lognormal but thats boring and I don't like it. 

# 8. Proportion of habitat covered by vegetation
#A. Continuous
#B. Beta distribution
# Continuous because it takes on fractional values most of the time and is 
# between 0 and 1. A beta distribution is appropriate because it
# models continuous proportional data bounded between 0 and 1. 
# Also i hate quantifying vegetation and ground cover too many shapes fr. 

# 9. Larval settlement success (number settled out of larvae released)
#A. Discrete
#B. Binomial distribution
# Discrete because larvae are counted in whole numbers here. 
# I'd use a binomial distribution for the specific case, because each larva as counted
# either settles or fails to settle, given a known number released
# and assuming independent outcomes. Could be converted to a percent 
# and run some other ways as well which is what I would do in this case. 

# 10. Proportion of infected fish in a population (parasite prevalence)
#A. Continuous
#B. Beta distribution 
# Continuous because it represents a proportion between 0 and 1.
# I'd do a beta distribution because it models proportional data bounded between 0 and 1.
# like how I'd convert and run #9 first personally.


# 11. Daily temperature at a field site
#A. Continuous
#B. Normal distribution
# Continuous because it can take fractional values.
# Normal distribution because temperatures often
# cluster around a mean, with fewer observations at either extreme. 
# (Hey wow kinda like my data set yay)

# 12. Population growth rate (next year's population size /
#    this year's population size)
#A. Continuous
#B. Lognormal distribution
# Population growth rate is treated as continuous because it is a
# ratio. I'd do a lognormal distribution because growth ratios are
# generally positive and may be right-skewed once again depending on taxa. 

#Part 2: Focal Dataset Distributions

install.packages("readxl")
library(readxl)

snakes <- read_excel("canebrake_data.xlsx")

#Numeric Variable 1: Size

#Remove observations with missing size measurements
snakes_clean <- snakes[!is.na(snakes$Size), ]

#Examine variable type and summary statistics
class(snakes_clean$Size)
summary(snakes_clean$Size)

#Continuous Variable

#Plot original size distribution
hist(snakes_clean$Size,
     main = "Distribution of Canebrake Rattlesnake Sizes",
     xlab = "Snake Size",
     ylab = "Frequency",
     col = "magenta",
     border = "yellow")

#Calculate mean and median
mean(snakes_clean$Size)
median(snakes_clean$Size)

#Plot log-transformed size distribution
hist(log(snakes_clean$Size[snakes_clean$Size > 0]),
     main = "Log-Transformed Snake Sizes",
     xlab = "Log(Snake Size)",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")

#In this case, my size data is continuous, because I am not measuring whole numbers, but rather
#actual sizes of animals measured and theoretically can take any value within that range,. 
#Oddly enough my median is in fact a whole number, but that is just very lucky in this 
#case we had a whole number measurement as the median. 
# The mean size was 2.611, while the median was 3, suggesting
# some left skew in the distribution due to juveniles. The log-transformed histogram
# does not appear approximately normal, so I am interpreting the
# original histogram in this case. The distribution appears potentially bimodal,
# rather than following a clear normal or lognormal distribution.

#Numeric Variable 2: Maximum Temperature

#Check column names to identify the maximum temperature variable
names(snakes)

#Remove observations with missing maximum temperature measurements
#Replace MaxTemp with the actual column name if different
snakes_temp <- snakes[!is.na(snakes$MaxTemp), ]

#Examine variable type and summary statistics
class(snakes_temp$MaxTemp)
summary(snakes_temp$MaxTemp)

#Continuous Variable

#Plot original temperature distribution
hist(snakes_temp$MaxTemp,
     main = "Distribution of Maximum Temperatures",
     xlab = "Maximum Temperature (F)",
     ylab = "Frequency",
     col = "magenta",
     border = "yellow")

#Calculate mean and median
mean(snakes_temp$MaxTemp)
median(snakes_temp$MaxTemp)

#Plot log-transformed temperature distribution
hist(log(snakes_temp$MaxTemp[snakes_temp$MaxTemp > 0]),
     main = "Log-Transformed Maximum Temperatures",
     xlab = "Log(Maximum Temperature)",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")

# My maximum temperature in this case is a discrete variable in this case because temperature is
# being as a whole number and not a continuous factor with decimal inclusions. 
# The mean maximum temperature was 72.82353 degrees F, while the median
# was 73 degrees F. The difference between these values suggests
# that the distribution is every so slightly left skewed, due to a few small outlier data.
# I am interpreting the original histogram because the temperature
# measurements appear approximately normally distributed, with most
# observations concentrated around the mean and fewer observations
# occurring at either extreme.

#Numeric Variable 3: Encounter Time, Frequency

#Convert encounter times into numerical values using a 24-hour decimal format
snakes$TimeNumeric <- as.numeric(
  format(strptime(snakes$Time, format = "%I:%M%p"), "%H")
) +
  as.numeric(
    format(strptime(snakes$Time, format = "%I:%M%p"), "%M")
  ) / 60

#Check that times were converted correctly
head(snakes[, c("Time", "TimeNumeric")])

#Check for unusually encounter times (had some issues here to check for)
snakes[!is.na(snakes$TimeNumeric) & snakes$TimeNumeric < 5,
       c("Time", "TimeNumeric")]

#Examine variable type and summary statistics
class(snakes$TimeNumeric)
summary(snakes$TimeNumeric)

#Check for missing values
sum(is.na(snakes$TimeNumeric))

#Continuous Variable

#Plot original encounter time distribution
hist(snakes$TimeNumeric,
     main = "Distribution of Canebrake Rattlesnake Encounter Times",
     xlab = "Time of Day (24-hour decimal)",
     ylab = "Frequency",
     col = "magenta",
     breaks = 12)

#Calculate mean and median
mean(snakes$TimeNumeric, na.rm = TRUE)
median(snakes$TimeNumeric, na.rm = TRUE)


# Encounter time in this case is a continuous variable because timeis
# being measured to include fractional hours.
# The mean encounter time was approximately 12.620 hours (12:37 PM),
# while the median was 12.725 hours (12:44 PM). These values are
# relatively close, suggesting little overall skew in the data.
# Again, a normal distribution is fairly reasonable for
# approximation since the data shows observations concentrated
# around midday, with progressively fewer encounters earlier and
# later in the day.
# Granted, encounter times are influenced by survey effort, since
# observations are more likely to occur during periods when field
# surveys are being conducted. Therefore, this distribution describes
# recorded encounter times rather than necessarily representing
# the actual daily activity patterns. The area this study was conducted
#can be described as "the hood" by some, and thus post-sunset activity was
#limited. I am also a data bum, and therefore pre-sunrise activity was not quantified either. 

