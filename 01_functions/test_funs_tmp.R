### Funktionen aus einem Skript sourcen
library(tidyverse)

# Datensatz zum Testen erstellen
df <- data.frame(
  age = c(23, 34, 42, 49, 53, 41, 57, 31, 44, 39, 18, 42),
  gender = c("M", "F", "F", "M", "M", "F", "M", "F", "M", "F", 'F', 'F'),
  education = c("High School", "Bachelors", "Masters", "PhD", "High School", "Bachelors", "Masters", "Bachelors", "PhD", "High School", "High School", "PhD"),
  salary = c(52000, 68000, 84000, 96000, 51000, 70000, 89000, 59000, 79000, 46000, 34000, 67000),
  experience = c(1, 4, 2, 8, 1.5, 6, 3, 3, 7, 5, 2, 8),
  stringsAsFactors = FALSE
)

df$gender2 <- ifelse(df$gender == 'M', 1, 0)



### Funktionen laden
source('P:/Bereich_QER_A&D/Themen/Methoden/R/R_IGES/01_functions/funs_reg_outputs.R')


### Testen
# Achtung: source() funktioniert nicht, wenn gesourctes Skript nicht durchläuft
# --> am besten ausschließlich Funktionen ablegen & testen, ob source() funktioniert


# Linear regression of salary on age & experience
linear_model <- lm(salary ~ age + experience, data = df)
summary(linear_model)

output <- reg_output(linear_model)


# Binomial regression of gender on age, experience & salary
binomial_model <- glm(gender2 ~ scale(age) + scale(experience) + scale(salary), data = df, family = binomial)
summary(binomial_model)

output <- logreg_output(binomial_model)


# Multilevel model of salary on age, with a random intercept for education
multilevel_model <- lme4::lmer(salary ~ age + (1 | education), data = df)
summary(multilevel_model)

output <- mlm_output(multilevel_model)


# Multilevel binomial regression of gender on education, with a random intercept for age
multilevel_binomial_model <- lme4::glmer(gender2 ~ scale(salary) + experience + 
                                           (1 | education), data = df, family = binomial)

summary(multilevel_binomial_model)

output <- mlm_logreg_output(multilevel_binomial_model)
