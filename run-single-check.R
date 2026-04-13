
datasets <- seq(1:12000)

# Check by model
data %>%
  filter(model == 'model1') %>% 
  mutate(num = str_remove(dataset, 'dataset')) %>% 
  select(num, flag) -> mod

# ModU - 5
# Mod1 - 15
# Mod2 - 12
# Mod3 - 6

# Partial runs
mod %>%
  group_by(num) %>%
  summarize(n=n()) %>%
  filter(n < 15) # Change number to match model

# If partial runs exist
filter(mod, num %in% c(5756))

# Fully missing
datasets[!(datasets %in% mod$num)]
