### Skript enthält Funktionen zum Abspeichern von (Regressions-)Outputs
# alle Funktionen erfordern das Laden von tidyverse


#### Test ---------------
### Testdatensatz zum Testen der Funktionen 
df <- data.frame(
  age = c(23, 34, 27, 49, 53, 41, 29, 31, 44, 39),
  gender = c("M", "F", "F", "M", "M", "F", "M", "F", "M", "F"),
  education = c("High School", "Bachelors", "Masters", "PhD", "High School", "Bachelors", "Masters", "Bachelors", "PhD", "High School"),
  salary = c(52000, 68000, 84000, 96000, 51000, 70000, 89000, 59000, 79000, 46000),
  experience = c(1, 4, 2, 8, 10, 6, 3, 3, 7, 5),
  stringsAsFactors = FALSE
)

df$gender2 <- ifelse(df$gender == 'M', 1, 0)


### Regressionsmodelle zum Testen der Funktionen
# Linear regression of salary on age
linear_model <- lm(salary ~ age, data = df)
#summary(linear_model)

# Binomial regression of gender on education
binomial_model <- glm(gender2 ~ education + experience, data = df, family = binomial)
#summary(binomial_model)

# Multilevel model of salary on age, with a random intercept for education
multilevel_model <- lme4::lmer(salary ~ age + (1 | education), data = df)
#summary(multilevel_model)

# Multilevel binomial regression of gender on education, with a random intercept for age
multilevel_binomial_model <- lme4::glmer(gender2 ~ age + scale(salary, scale=TRUE) + experience + (1 | education), data = df, family = binomial)
#summary(multilevel_binomial_model)


#### Funktionen ---------------
##### 1. Einfache Modelle -----

### Output einer linearen Regression 
# function takes linear model and returns a data frame with b, SE, lower_CI, upper_CI & p
reg_output <- function(model) {
  coeffs <- as.data.frame(coef(summary(model)))
  coeffs <- coeffs %>%
    rename(se = `Std. Error`, 
           p = `Pr(>|t|)`) %>% 
    mutate(b = Estimate,
           lower_CI = (b - 1.96 * se),
           upper_CI = (b + 1.96 * se)) %>%
    mutate_at(vars(b, lower_CI, upper_CI), funs(round(., 2))) %>%
    mutate_at(., vars(p), funs(round(., 3))) %>%
    tibble::rownames_to_column(var = 'Parameter') %>%
    dplyr::select(Parameter, b, lower_CI, upper_CI, p)
  return(coeffs)
}

tmp <- reg_output(linear_model)


### Output einer logistischen Regression 
# function takes binomial glm-model and returns a data frame with OR, SE, lower_CI, upper_CI & p
logreg_output <- function(model) {
  coeffs <- as.data.frame(coef(summary(model)))
  coeffs <- coeffs %>%
    rename(se = `Std. Error`, 
           p = `Pr(>|z|)`) %>%
    mutate(OR = exp(Estimate),
           lower_CI = exp(Estimate - 1.96 * se),
           upper_CI = exp(Estimate + 1.96 * se)) %>%
    mutate_at(vars(OR, lower_CI, upper_CI), ~ round(., 2)) %>%
    mutate_at(vars(p), ~ round(., 3)) %>%
    tibble::rownames_to_column(var = 'Parameter') %>%
    dplyr::select(Parameter, OR, lower_CI, upper_CI, p)
  return(coeffs)
}

tmp <- logreg_output(binomial_model)



##### 2. Mehrebenenmodelle ------

### Output eines Mehrebenenmodells mit kontinuierlicher Outcome-Variable
# function takes linear Multilevel Model (lmer or glmer-object) and returns a data frame 
# with b, SE, lower_CI, upper_CI & p + rendom effects (var, sd); 
# function requires installation of lme4-package

mlm_output <- function(model) {
  coeffs <- as.data.frame(coef(summary(model)))
  ## fixed effects
  coeffs <- coeffs %>% 
    rename(se = `Std. Error`) %>% 
    mutate(lower_CI = Estimate - 1.96 * se,
           upper_CI = Estimate + 1.96 * se) %>% 
    # use normal distribution to approximate p-value
    mutate(p = 2 * (1 - pnorm(abs(`t value`)))) %>% 
    mutate_at(vars(Estimate, lower_CI, upper_CI), funs(round(., 2))) %>%
    mutate_at(vars(p), funs(round(., 4))) %>%
    tibble::rownames_to_column(var = 'Parameter') %>%
    dplyr::select(Parameter, Estimate, lower_CI, upper_CI, p)
  
  ## random effects
  re <- as.data.frame(lme4::VarCorr(model)) %>%
    mutate_at(vars(vcov, sdcor), funs(round(., 6)))
  
  output <- bind_rows(coeffs, re)
  return(output)
}

tmp <- mlm_output(multilevel_model)



### Output eines logistischen Mehrebenenmodells
# function takes binomial Multilevel Model (lmer or glmer-object) and returns a data frame 
# with OR, SE, lower_CI, upper_CI & p + rendom effects (var, sd); 
# function requires installation of lme4 package
mlm_logreg_output <- function(model) {
  ## fixed effects
  coeffs <- as.data.frame(coef(summary(model)))
  coeffs <- coeffs %>%
    rename(se = `Std. Error`, 
           p = `Pr(>|z|)`) %>%
    mutate(OR = exp(Estimate),
           lower_CI = exp(Estimate - 1.96 * se),
           upper_CI = exp(Estimate + 1.96 * se)) %>%
    mutate_at(vars(OR, lower_CI, upper_CI), ~ round(., 2)) %>%
    mutate_at(vars(p), ~ round(., 3)) %>%
    tibble::rownames_to_column(var = 'Parameter') %>%
    dplyr::select(Parameter, OR, lower_CI, upper_CI, p)
  
  ## random effects
  re <- as.data.frame(lme4::VarCorr(model)) %>%
    mutate_at(vars(vcov, sdcor), ~ round(., 2))
  
  output <- bind_rows(coeffs, re)
  return(output)
}

tmp <- mlm_logreg_output(multilevel_binomial_model)

