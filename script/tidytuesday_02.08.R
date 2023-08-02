library(writexl)
library(tidyverse)
library(ggplot2)

setwd("C:\\Users\\pbslins\\Desktop\\tidytuesday")

states <- read.csv("states.csv", header = TRUE)

states_etimologia <- read.csv("state_name_etymology.csv", header = TRUE)

head(states)
glimpse(states)
head(states_etimologia)
glimpse(states_etimologia)


states_final <- left_join(states, states_etimologia, by = "state")

states_final2 <- states_final[!duplicated(states_final$state), ]



ggplot(states_final2, aes(x=state, y=water_area_km2, fill=state)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

states_mutate <- states_final2 %>%
  mutate(n_representatives = case_when(n_representatives < 5 ~ "Menos de 5",
                                       n_representatives >= 5 & n_representatives < 10 ~ "De 5 a 10",
                                       n_representatives >= 10 & n_representatives < 20 ~ "De 10 a 20",
                                       n_representatives >= 20 ~ "Mais de 20"))


ggplot(states_mutate, aes(x=n_representatives, y=population_2020, fill=n_representatives)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  ggtitle("Representantes por tamanho populacional") +
  xlab("Nº representantes na Câmara dos Representantes") +
  ylab("Tamanho populacional")
  