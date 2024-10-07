#AIM 1: Examine effects of increasing age on marathon performance in men and women
#AIM 2: Explore the impact of environmental conditions on marathon performance, and whether the impact differs across age and gender.
#AIM 3: Identify the weather parameters (WBGT, Flag conditions, temperature, etc) that have the largest impact on marathon performance.

ori_data = read.csv("/Users/evancheng/Downloads/project1 (1).csv")
 
summary(ori_data)

#Library
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gt)
library(gridExtra)

# Calculate the percentage of missing data 
missing_data_summary <- ori_data %>%
  summarise_all(funs(sum(is.na(.))/n()*100))
print(missing_data_summary)

# Convert categorical variables to factors
ori_data <- ori_data %>%
  mutate(
    Sex..0.F..1.M. = factor(Sex..0.F..1.M., labels = c("Female", "Male")),
    Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D. = factor(Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D.),
    Flag = factor(Flag) 
  )

# Data Cleaning
clean_data <- na.omit(ori_data)
head(clean_data)

#Rename for Summary Table 
renamed_data <- ori_data %>%
  rename(
    Race = Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D.,
    Gender = Sex..0.F..1.M.,
    Heat_Stress_Indicator = Flag,
    Age = Age..yr.,
    Percent_Current_Record = X.CR,
    Dry_Bulb_Temp_C = Td..C,
    Wet_Bulb_Temp_C = Tw..C,
    Relative_Humidity = X.rh,
    Black_Globe_Temp_C = Tg..C,
    Solar_Radiation = SR.W.m2,
    Dew_Point_C = DP,
    Wind_Speed_Km_hr = Wind,
    Wet_Bulb_Globe_Temp = WBGT
  )



#Plots for Data Exploration 
# Density Plot for All Players' Performance
ggplot(clean_data, aes(x=X.CR)) +
  geom_density(fill="steelblue", alpha=0.7) +
  ggtitle("Figure 6. Density Plot of Performance") +
  xlab("Performance") +
  ylab("Density") +
  theme_minimal()

# Density Plot for Performance vs. Age
ggplot(clean_data, aes(x=Age..yr., y=X.CR)) +
  geom_point(alpha=0.4, color="steelblue") +
  ggtitle("Performance vs. Age") +
  xlab("Age") +
  ylab("Performance") +
  theme_minimal()

# Scatter Plot of Performance vs. Age by Gender
ggplot(clean_data, aes(x=Age..yr., y=X.CR, color=factor(Sex..0.F..1.M.))) +
  geom_point(alpha=0.4) +
  scale_color_manual(values=c("lightyellow", "steelblue"), labels=c("Female", "Male")) +
  ggtitle("Figure 5. Performance vs. Age by Gender") +
  xlab("Age") +
  ylab("Performance") +
  theme_minimal() +
  labs(color="Gender")

# Create individual plots
p1 <- ggplot(clean_data, aes(x=WBGT, y=`X.CR`)) +
  geom_point(alpha=0.4, color="lightblue") +
  ggtitle("Figure 1. Performance vs. WBGT") +
  xlab("Wet Bulb Globe Temperature (°C)") +
  ylab("Performance")

p2 <- ggplot(clean_data, aes(x=`Td..C`, y=`X.CR`)) +
  geom_point(alpha=0.4, color="lightgreen") +
  ggtitle("Figure 2. Performance vs. Dry Bulb Temperature") +
  xlab("Dry Bulb Temperature (°C)") +
  ylab("Performance")

p3 <- ggplot(clean_data, aes(x=`X.rh`, y=`X.CR`)) +
  geom_point(alpha=0.4, color="pink") +
  ggtitle("Figure 3. Performance vs. Humidity") +
  xlab("Relative Humidity (%)") +
  ylab("Performance")

p4 <- ggplot(clean_data, aes(x=`SR.W.m2`, y=`X.CR`)) +
  geom_point(alpha=0.4, color="lightyellow") +
  ggtitle("Figure 4. Performance vs. Solar Radiation") +
  xlab("Solar Radiation (W/m2)") +
  ylab("Performance")

grid.arrange(p1, p2, p3, p4, ncol=2)

#Need summary table for the data 


#AIM 1: Examine effects of increasing age on marathon performance in men and women

library(lme4)
model_course <- lmer(X.CR ~ Age..yr. * Sex..0.F..1.M. + (1|Race..0.Boston..1.Chicago..2.NYC..3.TC..4.D.), data = clean_data)
summary(model_course)


#Aim 2 Environmental variable x Performance -  Mutiple Linear Regression 
library(stats)

# Exclude ages less than 18
clean_data_adults <- subset(clean_data, Age..yr. >= 18)

# Multiple linear regression model 
lm_model <- lm(X.CR ~ WBGT + X.rh + SR.W.m2 + Age..yr. + `Sex..0.F..1.M.` +
                 WBGT:Age..yr. + WBGT:`Sex..0.F..1.M.` +
                 X.rh:Age..yr. + X.rh:`Sex..0.F..1.M.` +
                 SR.W.m2:Age..yr. + SR.W.m2:`Sex..0.F..1.M.`, 
               data = clean_data_adults)

summary(lm_model)

#Plot not helpful
plot(clean_data_adults$X.CR, predict(lm_model), main = "Fitted vs. Actual Values",
     xlab = "Actual Performance", ylab = "Predicted Performance")


#Aim 3 Weather parameter with the largest impact on performance 
library(stats)

# Multiple linear regression 
lm_model_full <- lm(X.CR ~ WBGT + Flag + Td..C + Tw..C + X.rh + SR.W.m2 + DP + Wind, 
                    data = clean_data)
summary


# Standardize variables
clean_data_std <- clean_data
clean_data_std$WBGT <- scale(clean_data$WBGT)
clean_data_std$Flag <- as.factor(clean_data$Flag) # Keep Flag as a factor
clean_data_std$Td..C <- scale(clean_data$Td..C)
clean_data_std$Tw..C <- scale(clean_data$Tw..C)
clean_data_std$X.rh <- scale(clean_data$X.rh)
clean_data_std$SR.W.m2 <- scale(clean_data$SR.W.m2)
clean_data_std$DP <- scale(clean_data$DP)
clean_data_std$Wind <- scale(clean_data$Wind)

# Multiple linear regression with standardized variables
lm_model_std <- lm(X.CR ~ WBGT + Flag + Td..C + Tw..C + X.rh + SR.W.m2 + DP + Wind, 
                   data = clean_data_std)


summary(lm_model_std)