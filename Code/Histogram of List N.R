N <- read_csv("~/Downloads/List N Disinfectants for Use Against SARS-CoV-2 (COVID-19)  Pesticide Registration  US EPA.csv")

a=table(N$`Active Ingredient(s)`) %>% as_tibble()
hist()

N %>%
  count(`Active Ingredient(s)`) %>%
  ggplot(aes(x = reorder(`Active Ingredient(s)`, -n), y = n)) +
  geom_bar(stat = 'identity') +
  xlab("Active Ingredient")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
