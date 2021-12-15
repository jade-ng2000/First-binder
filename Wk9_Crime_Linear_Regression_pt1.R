library(tidyverse)
library(Hmisc)
library(performance)

crime <- read_csv('https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv')
head(crime)

crime <- separate(crime, col = 'City, State', into = c('City', 'State'))
head(crime)

# rename columns and get rid of violent crimes heading
crime <- crime %>%
  rename(House_price = index_nsa) %>%
  rename(Violent_Crimes = 'Violent Crimes')
head(crime)

#scatterplot
crime %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = 'Population',
       y = 'Violent Crimes')

#pearson's R
rcorr(crime$Population, crime$Violent_Crimes)
# -r = 0.81

crime_filtered <- filter(crime, Population <2000000)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes)) + geom_point(alphas = .25) +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = 'Population',
       y = 'Violent Crimes')

#pearson's R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)
# r = 0.69

crime_filtered <- filter(crime_filtered, Year == 2015)

crime_filtered %>%
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = 'Population',
       y = 'Violent Crimes')

#pearson's R
rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)
#r = 0.65

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered) #mean of outcome used as predictor
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered) #population size used to predict violent crimes outcome

#check assumptions
check_model(model2)

anova(model1, model2)

#interpreting models
summary(model2)
#intercept corresponds to where our regression line intercepts the y-axis
# Population parameter corresponds to the slope of our line = gradient

#3 challenge tasks

#1) Check whether the same relationship holds for population size and robberies in 2015
crime_filtered #check column titles

crime_filtered %>%
  ggplot(aes(x = Population, y = Robberies, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  labs(x = 'Population',
       y = 'Robberies')

rcorr(crime_filtered$Population, crime_filtered$Robberies)
#r = 0.63

model1 <- lm(Robberies ~1, data = crime_filtered)
model2 <- lm(Robberies ~ Population, data = crime_filtered)

check_model(model2)

anova(model1, model2)

summary(model2)
# similar correlation r value

#2) Are house prices predicted by the number of violent crimes in 2015?
crime_filtered
crime_filtered %>%
  ggplot(aes(x = Violent_Crimes, y = House_price, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(20000, 5000) +
  theme_minimal() +
  labs(x = 'Violent Crimes',
       y = 'House Prices')

rcorr(crime_filtered$Violent_Crimes, crime_filtered$House_price)
#r = -0.18

model1 <- lm(House_price ~0, data = crime_filtered)
model2 <- lm(House_price ~ Violent_Crimes, data = crime_filtered)

check_model(model2)

anova(model1, model2)

summary(model2)
# very slightly negative correlation: as crime increases, house prices drop - insignificant?

#3) Are house prices predicted by population size in 2015?
crime_filtered
crime_filtered %>%
  ggplot(aes(x = Population, y = House_price, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(1000000, 5000) +
  theme_minimal() +
  labs(x = 'Population Size',
       y = 'House Prices')

rcorr(crime_filtered$Population, crime_filtered$House_price)
#r = 0.14

model1 <- lm(House_price ~1, data = crime_filtered)
model2 <- lm(House_price ~ Population, data = crime_filtered)

check_model(model2)

anova(model1, model2)

summary(model2)
#as population size grows, the house price increases slightly - significant?