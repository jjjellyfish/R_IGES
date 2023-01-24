### Skript enthält Funktionen zum Abspeichern von (Regressions-)Outputs


#### 1. Einfache Modelle ####

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

#tmp <- logreg_output(m_kv_pp_c1_kvoffen)





#### 2. Mehrebenenmodelle

### Output eines logistischen Mehrebenenmodells
# function takes binomial Multilevel Model (lmer or glmer-object) and returns a data frame 
# with OR, SE, lower_CI, upper_CI & p
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
  
  ## rendom effects
  re <- as.data.frame(VarCorr(model)) %>%
    mutate_at(vars(vcov, sdcor), ~ round(., 2))
  
  output <- bind_rows(coeffs, re)
  return(output)
}

#tmp <- mlm_logreg_output(m_kv_c1)

