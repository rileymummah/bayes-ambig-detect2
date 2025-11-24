
## ---------------------------
## Objective: To visualize model simulation output
##
## Input:
##   combined-output.rds
##   parameter_combos.csv
##
## Output:
##   Figures 2-4
##   Figures S1-S17
##
## ---------------------------


## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(ggdist)
library(NatParksPalettes) # National Park-inspired color palettes


## load data ---------------------------
output <- readRDS('output/combined-output.rds') %>%
  mutate(nsites = factor(nsites, levels = c('l.sites','h.sites'),
                         labels = c('Sites (n=20)', 'Sites (n=100)')),
         nindiv = factor(nindiv, levels = c('l.indiv','h.indiv'),
                         labels = c('Individuals (n=5)', 'Individuals (n=50)')),
         ntests = factor(ntests, levels = c('low','med','high'),
                         labels = c('Sampling device (n=1)',
                                    'Sampling devices (n=2)',
                                    'Sampling devices (n=3)')),
         model = case_when(model == 'modelU' ~ 'ModelU',
                           model == 'model1' ~ 'Model1',
                           model == 'model2' ~ 'Model2',
                           model == 'model3' ~ 'Model3'),
         model.flag = paste0(model,'-', flag),
         param.set = as.numeric(param.set)) %>%
  # Remove ModelU-ND and Model-D comparisons
  filter(!(model.flag %in% c('ModelU-D','ModelU-ND'))) %>%
  mutate(model.flag = ifelse(model == 'ModelU', model, model.flag))


param.combos <- read.csv('data/parameter_combos.csv',
                         header = T) %>%
  rownames_to_column() %>%
  mutate(dataset = paste0('dataset',rowname)) %>%
  select(-rowname)

# graphical settings -----------------------------------------------------------------
col.values <- c(natparks.pals("Glacier", n=5, type='discrete')[c(1,2,4)],
                natparks.pals("DeathValley", n=7, type='discrete')[c(1:3)],
                natparks.pals("Olympic", n=9, type='discrete')[c(2:4)],
                'black')


shape.values <- c(15,15,15,
                  16,16,16,
                  17,17,17,
                  18)


# Figure 2 ----------------------------------------------------------------

# Only plot psi and theta11
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         parameter = factor(parameter, levels = c('theta11','psi'))) %>%
  group_by(model.flag, nsites, nindiv, ntests, parameter) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n()) %>% 
  ggplot(aes(x=parameter, y=coverage, col=model.flag,
             shape=model.flag)) +
  geom_point(position = position_dodge(0.85), alpha = 0.6, size = 3) +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_x_discrete(labels = c(expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  coord_flip() +
  labs(y = '% Coverage of the True Parameter') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('fig2-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 6.5, height = 7.5, unit = 'in',
       dpi = 600)


# Figure S1 ---------------------------------------------------------------

# Plot all parameters
output %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         parameter = factor(parameter,
                            levels = c('p01','p001','p101','p11','p111',
                                       'theta11','psi'))) %>%
  filter(parameter != 'deviance') %>%
  group_by(model.flag, nsites, nindiv, ntests, parameter) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n()) %>%
  ggplot(aes(x=parameter, y=coverage, col=model.flag,
             shape=model.flag)) +
  geom_point(position = position_dodge(0.85), size = 2.5, alpha = 0.7) +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_x_discrete(labels = c(expression('False positive ('*p[01]*')'),
                              expression('False positive ('*p[001]*')'),
                              expression('False positive ('*p[101]*')'),
                              expression('True positive ('*p[11]*')'),
                              expression('True positive ('*p[111]*')'),
                              expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_y_continuous(breaks = seq(0, 1, by=0.2)) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  coord_flip() +
  labs(y = '% Coverage of the True Parameter') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        panel.spacing.x = unit(3, "mm")) +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS1-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 8, height = 9, unit = 'in',
       dpi = 600)


# Figure S2 ---------------------------------------------------------------
output %>%
  mutate(parameter = factor(parameter,
                            levels = c('p01','p001','p101','p11','p111',
                                       'theta11','psi')),
         converge = ifelse(Rhat < 1.1, 1, 0)) %>%
  filter(parameter != 'deviance') %>% 
  group_by(model.flag, nsites, nindiv, ntests, parameter) %>%
  summarize(converge = sum(converge, na.rm = T)) %>%
  ggplot(aes(x=parameter, y=converge, col=model.flag,
             shape=model.flag)) +
  geom_point(position = position_dodge(0.85), size = 2.5, alpha = 0.7) +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_x_discrete(labels = c(expression('False positive ('*p[01]*')'),
                              expression('False positive ('*p[001]*')'),
                              expression('False positive ('*p[101]*')'),
                              expression('True positive ('*p[11]*')'),
                              expression('True positive ('*p[111]*')'),
                              expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  coord_flip() +
  labs(y = 'Total runs converged') +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        panel.spacing.x = unit(3, "mm")) +
  guides(colour = guide_legend(nrow = 3))


ggsave(paste0('figS2-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 8, height = 9, unit = 'in',
       dpi = 600)


# Figure S3 ---------------------------------------------------------------

# Psi is binned
output %>%
  filter(parameter == 'psi') %>%
  mutate(psi.factor = cut(true.mean,
                          breaks = seq(0,1, by=0.1),
                          right = F),
         coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  group_by(model.flag, nsites, nindiv, ntests, psi.factor) %>% 
  summarize(coverage = sum(coverage, na.rm = T)/n()) %>%
  ggplot() +
  geom_jitter(aes(x = psi.factor, y = coverage,
                  col = model.flag, shape = model.flag),
              size = 2, width = 0.25, alpha = 0.65) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression('Occupancy ('*psi*')'), y = '% Coverage', color = 'Model', fill = 'Model') +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        # axis.title.y = element_blank(),
        axis.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS3-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 600,
       height = 8.5, width = 11, units = 'in')


# Figure S4 ---------------------------------------------------------------

output %>%
  filter(parameter == 'theta11') %>%
  mutate(theta11.factor = cut(true.mean,
                              breaks = seq(0,1, by=0.1),
                              right = F),
         coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  group_by(model.flag, nsites, nindiv, ntests, theta11.factor) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n()) %>%
  ggplot() +
  geom_jitter(aes(x = theta11.factor, y = coverage,
                  col = model.flag, shape = model.flag),
              size = 2, width = 0.25, alpha = 0.65) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression('Prevalence ('*theta[11]*')'), 
       y = '% Coverage', color = 'Model', fill = 'Model') +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS4-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 600,
       height = 8.5, width = 11, units = 'in')


# Figure S5: Accuracy -----------------------------------------------------
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(parameter = factor(parameter, levels=c('theta11','psi'))) %>%
  ggplot(aes(x = parameter, y = abs(acc), col = model.flag, group = model.flag)) +
  geom_hline(yintercept = 0, col = 'black', linetype = 'dashed') +
  stat_halfeye(aes(fill = model.flag),
                   # fill = after_scale(colorspace::lighten(fill, .7))),
               adjust = 0.5, point_size = 2, position = position_dodgejust(width = 0.9),
               .width = c(0.5, 0.95)) + # Sets intervals for stat_pointinterval()
  scale_x_discrete(labels = c(expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(values = col.values) +
  scale_fill_manual(values = col.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = '', y = 'Accuracy', color = 'Model', fill = 'Model') +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS5-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


## Figure S6: Bias --------------------------------------------------------
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(parameter = factor(parameter, levels=c('theta11','psi'))) %>%
  ggplot(aes(x = parameter, y = acc, col = model.flag, group = model.flag)) +
  geom_hline(yintercept = 0, col = 'grey25') +
  stat_halfeye(aes(fill = model.flag),
               adjust = 0.5, point_size = 1, position = position_dodgejust(width = 0.9),
               .width = c(0.5, 0.95)) + # Sets intervals for stat_pointinterval()
  scale_x_discrete(labels = c(expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(values = col.values) +
  scale_fill_manual(values = col.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = '', y = 'Bias', color = 'Model', fill = 'Model') +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS6-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


## Figure S7: Precision (95% BCI WIDTH) ------------------------------------
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(parameter = factor(parameter, levels = c('theta11','psi'))) %>%
  ggplot(aes(x = parameter, y = bci.width, col = model.flag, group = model.flag)) +
  geom_hline(yintercept = 0, col = 'black') +
  stat_halfeye(aes(fill = model.flag),
               adjust = 0.5, point_size = 2, position = position_dodgejust(width = 0.9),
               .width = c(0.5, 0.95)) + # Sets intervals for stat_pointinterval()
  scale_x_discrete(labels = c(expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(values = col.values) +
  scale_fill_manual(values = col.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = '', y = 'Precision', color = 'Model', fill = 'Model') +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal') +
  guides(colour = guide_legend(nrow = 3))

ggsave(paste0('figS7-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


# Figure 3: noU, ntests=2 --------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('Model1-noU','Model2-noU',
                           'Model3-noU','ModelU'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('fig3-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S8: noU, ntests=1 -------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-noU',
                           'Model2-noU','Model1-noU'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.5) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS8-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# Figure S9: noU, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-noU',
                           'Model2-noU','Model1-noU'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.5) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS9-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# Figure S10: ND, ntests=1 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS10-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S11: ND, ntests=2 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS11-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S12: ND, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS12-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S13: D, ntests=1 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>% 
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>% 
  unnest(cols = c(true.mean_psi, true.mean_theta11, acc_psi, acc_theta11, 
                  coverage_psi, coverage_theta11)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS13-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S14: D, ntests=2 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS14-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# Figure S15: D, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'Both',
                              coverage_psi == T & coverage_theta11 == F ~ 'Occupancy only',
                              coverage_psi == F & coverage_theta11 == T ~ 'Prevalence only',
                              coverage_psi == F & coverage_theta11 == F ~ 'Neither')) %>%
  ggplot() +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=coverage),
             size = 2, alpha = 0.6) +
  scale_color_manual(values = c('lightgrey','#1b9e77','#d95f02','#7570b3')) +
  facet_grid(model.flag ~ nsites + nindiv + ntests) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep="")),
       color = 'Parameter recovered?') +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave(paste0('figS15-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# Figure 4 ----------------------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         nsites == 'Sites (n=20)',
         nindiv == 'Individuals (n=5)',
         ntests == "Sampling devices (n=2)",
         model.flag == 'ModelU') %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter, values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'both',
                              coverage_psi == T & coverage_theta11 == F ~ 'psi',
                              coverage_psi == F & coverage_theta11 == T ~ 'theta11',
                              coverage_psi == F & coverage_theta11 == F ~ 'neither')) -> tmp

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=acc_psi),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Accuracy: ',psi, sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw() -> error.psi

# To save other sample size combos, comment out the filtering above and save the error.psi figure.


ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=acc_theta11),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Accuracy: ', theta[11], sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw() -> error.theta11

# To save other sample size combos, comment out the filtering above and save the error.theta11 figure.


cowplot::plot_grid(error.psi, error.theta11)

ggsave(paste0('fig4-',Sys.Date(),'.png'),
       width = 11, height = 4, units = 'in',
       path = 'output/figures/',
       dpi = 500)



# Figure S16 ---------------------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag == 'ModelU') %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, acc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter, values_from = c(true.mean, acc, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'both',
                              coverage_psi == T & coverage_theta11 == F ~ 'psi',
                              coverage_psi == F & coverage_theta11 == T ~ 'theta11',
                              coverage_psi == F & coverage_theta11 == F ~ 'neither')) -> tmp

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=acc_psi),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Accuracy: ',psi, sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw()

ggsave(paste0('figS16-',Sys.Date(),'.png'),
       width = 12, height = 9, units = 'in',
       path = 'output/figures/',
       dpi = 500)


# Figure S17 ---------------------------------------------------------------

# Make sure that you have the tmp file from Figure S6 loaded

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=acc_theta11),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Accuracy: ', theta[11], sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw()

ggsave(paste0('figS17-',Sys.Date(),'.png'),
       width = 12, height = 9, units = 'in',
       path = 'output/figures/',
       dpi = 500)


# End script
