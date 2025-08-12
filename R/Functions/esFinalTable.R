esFinalTable <- function(Names) {
  
  bind_rows(
    lapply(c(Names), function(Name) {
      bind_rows(
        lapply(c("Mean", "Var"), function(Feature) 
          esSumm(Name, Feature))) %>%
        mutate(Variable = Name)
    })) %>%
    select(Variable, everything()) %>%
    mutate(Feature = ifelse(Feature == "Var", "Variance", Feature)) %>%
    mutate(Variable = case_match(Variable, 
                                 "PD" ~ "Pest density", 
                                 "ParAb" ~ "Parasitoid abundance",
                                 "ParR" ~ "Parasitism rate",
                                 .default = Variable))
  
}