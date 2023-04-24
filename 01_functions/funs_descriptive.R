### Skript enthält Funktionen zur Durchführung und Darstellung deskriptiver Analysen
# Erfordert tidyverse und janitor package

#### Utility ####
# Umwandlung eines numerischen Werts in ein character mit Komma als Dezimalseperator
komma_char = function(x, n.decimal = 2){ # x = vektor
  return(format(round(x, n.decimal), n.small = 2, decimal.mark = 2))
}

#### Kontinuierliche Werte ####
# Einfache "Kurzform" für dypler's summarise-Funktion; Gibt ein summary-table als tibble aus
# Kann in dplyr pipe genutzt und per group_by gruppiert werden
describe_tbl = function(x, col){
  x = x %>% 
    summarise(n = n(),
              mean = mean({{col}}, na.rm = TRUE),
              SD = SD({{col}}, na.rm = TRUE),
              median = median({{col}}, na.rm = TRUE),
              min = min({{col}}, na.rm = TRUE),
              max = max({{col}}, na.rm = TRUE))
  return(x)
}

#### Kategoriale Variablen ####
# Gibt eine Kreuztabelle über "item" nach "group" aus inklusive Reihenprozent und Spaltensummen. Input ist ein Datensatz und die Variablen (NICHT als string)
# Kann in dplyr pipe genutzt werden
cross_tbl_seperate = function(x, item, group){
    x %>% 
      count({{group}}, {{item}}) %>% 
      spread({{group}}, n) %>% 
      mutate(across(-1, ~replace_na(., 0))) %>%
      mutate(across(-1, ~round(./sum(.)*100, 1), .names = "{.col}_%")) %>%
      select(1, order(colnames(.))[order(colnames(.)) != 1]) %>% 
      janitor::adorn_totals("row") %>% 
      return()
}

# Gibt eine Kreuztabelle über "item" nach "group" aus. cross_tbl für kategorialen Variablen und describe_tbl für numerische Variablen
# Kategoriale Variablen werden mit n(%) angegeben und numerische mit MW(SD)
# Kategoriale
cross_tbl_brackets = function(x, item, group){
  x %>% 
    count({{group}}, {{item}}) %>% 
    spread({{group}}, n) %>% 
    mutate(across(-1, ~replace_na(., 0))) %>%
    mutate(across(-1, ~paste0(. ,
                              " (",
                              komma_char(./sum(.)*100),
                              ")"))) %>% 
    return()
}

# Numerische
describe_tbl_brackets = function(x, item, group){
  x %>% 
    group_by({{group}}) %>% 
    summarise("MW" = paste0(komma_char(mean({{item}})),
                                 " (",
                                 komma_char(sd({{item}})),
                                 ")")) %>% 
    spread({{group}}, "MW") %>% 
    return()
}

# Zählt Item mit Mehrfachantworten aus. Input ist der Datenframe, und alle Antworten als Dummy-Variablen.
# Gibt Prozente aus für alle Reihen mit mindestens einer Antwort (!is.na); Mit "n_only" = TRUE kann nur das entsprechende n ausgegeben werden
# Kann in dplyr pipe genutzt werden 
mehrfach_antwort <- function(data, vars, name="Var", n_only = FALSE){
  n_var <- data %>%
    filter(!(if_all({{vars}}, ~is.na(.)))) %>% 
    count() %>% 
    pull(n)
  if (n_only){
    return(n_var)
  } else {
    return <- data %>% 
      select({{vars}}) %>% 
      sapply(table) %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      set_names(c("Antwort", "n")) %>% 
      mutate("%" = round(n/n_var*100, 1)) %>% 
      arrange(desc(n))
  }
  return(return)
}