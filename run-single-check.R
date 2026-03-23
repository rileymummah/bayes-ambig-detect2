
datasets <- seq(1:12000)

# Check by model
data %>%
  filter(model == 'model3') %>% 
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
  filter(n < 6) # Change number to match model

# If partial runs exist
filter(mod, num %in% c(10888, 3151, 5121, 8344, 8832))

# Fully missing
datasets[!(datasets %in% mod$num)]
