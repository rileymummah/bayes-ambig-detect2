
datasets <- seq(1:12000)

# Check by model
data %>%
  filter(model == 'model3') %>% 
  mutate(num = str_remove(dataset, 'dataset')) %>% 
  select(num, flag) -> mod

# Partial runs
mod %>%
  group_by(num) %>%
  summarize(n=n()) %>%
  filter(n < 6) # Change number to match model

# If partial runs exist
filter(mod, num %in% c(1597, 2146))

# Fully missing
datasets[!(datasets %in% mod$num)]
