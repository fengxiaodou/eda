


######## Exercise Part 1 ############

gender <- read.csv("Data/gender.csv", header = T,sep = ",",stringsAsFactors = F) 
profile <- read.csv("Data/profile.csv", header = T,sep = ",",stringsAsFactors = F)

library(dplyr)
fertility <- inner_join(gender, profile, by =c("id"), copy = FALSE, suffix = c(".x", ".y")) %>% 
  arrange(desc(id))

write.csv(fertility, "Results/fertility.csv", row.names = FALSE)

######## Exercise Part 2 ############


max_couple <- fertility %>% 
  group_by(gender1, gender2) %>% 
  summarise(count = n_distinct(id)) %>% 
  ungroup() %>% 
  mutate(max = max(count)) %>% 
  filter(count == max)

write.csv(max_couple, "Results/max_couple.csv", row.names = FALSE)


pct <- fertility %>%
  mutate(work_type = ifelse(work<=4, 1, 2)) %>% 
  group_by(afam, hispanic, other, work_type) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(work_type == 1)
 
write.csv(max_couple, "Results/pct.csv", row.names = FALSE)


boy_pct <- fertility %>% 
  filter(age >= 22 & age <= 24) %>% 
  group_by(gender1) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(gender1 == "male")

write.csv(max_couple, "Results/boy_pct.csv", row.names = FALSE)  

######## Exercise Part 3 ############

fertility_long <- read.csv("Data/fertility_long.csv", header = T,sep = ",",stringsAsFactors = T) 

library(ggplot2)

fertility_long %>% 
  filter(race_code != 0 & child > 1) %>% 
  group_by(race_code, age, child) %>% 
  summarise(count = n_distinct(id)) %>% 
  ggplot(aes(age, count, fill = race_code)) + 
  geom_bar(stat = "identity", position = "dodge" , width = 0.3)


 


fertility_long %>% 
  filter(child ==1 ) %>%
  group_by(race_code, gender) %>% 
  summarise(n = n_distinct(id)) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(race_code, prop, fill = gender)) + 
  geom_bar(stat = "identity", position = position_dodge(), width = 5)

  
          