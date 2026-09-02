
## ---------------------------
## Objective: To visualize model simulation output
##
## Input:
##   combined-output.rds
##   parameter_combos.csv
##
## Output:
##   Figures 3-5
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
  # filter(!(model.flag %in% c('ModelU-D','ModelU-ND'))) %>%
  mutate(model.flag = ifelse(model == 'ModelU', model, model.flag))

  


param.combos <- read.csv('data/parameter_combos.csv',
                         header = T) %>%
  rownames_to_column() %>%
  mutate(dataset = paste0('dataset',rowname)) %>%
  select(-rowname)

# graphical settings -----------------------------------------------------------------
# scales::show_col(natparks.pals("DeathValley", n=9))

# col.values <- c(natparks.pals("Glacier", n=5, type='discrete')[c(1,2,4)],
#                 natparks.pals("DeathValley", n=7, type='discrete')[c(1:3)],
#                 natparks.pals("Olympic", n=9, type='discrete')[c(2:4)],
#                 'black')

# dark, med, light
col.values <- c('#3d18e9','#785EF0','#b3a4f7',
                '#9c1959','#DC267F','#e768a6',
                '#b37b00','#FFB000','#ffc84d',
                'black')


shape.values <- c(15,15,15,
                  16,16,16,
                  17,17,17,
                  18)


# FigS1: dist Us ------------------------------------------------------------

sim.datasets <- readRDS("data/sim.datasets.rds")

# How many Us are present in each dataset?
count.U <- data.frame(dataset = NA,
                      nsites = NA,
                      nindiv = NA,
                      ntests = NA,
                      Total = NA,
                      Negative = NA,
                      Positive = NA,
                      Equivocal = NA)



for (i in 1:length(sim.datasets)) {
  tmp <- data.frame(dataset = paste0('dataset',i),
                    # Total sites for datasetX
                    nsites = sim.datasets[[i]]$nsites,
                    nindiv = sim.datasets[[i]]$nindiv,
                    ntests = sim.datasets[[i]]$ntests,
                    # Total tests available in datasetX
                    Total = sim.datasets[[i]]$nsites * sim.datasets[[i]]$nindiv * sim.datasets[[i]]$ntests,
                    # Total non-detections across all sites within datasetX
                    Negative = sum(sim.datasets[[i]]$y[,,1]),
                    # Total detections across all sites within datasetX
                    Positive = sum(sim.datasets[[i]]$y[,,2]),
                    # Total Us across all sites within datasetX
                    Equivocal = sum(sim.datasets[[i]]$y[,,3]))
  count.U <- bind_rows(count.U, tmp)
}

# Each row in count.U represents one site in datasetX (one sample size-param combo)
count.U <- count.U %>%
  # First row is NAs
  slice(-1) %>%
  distinct() %>%
  mutate(equiv.perc = Equivocal/Total,
         nsites = factor(nsites, levels = c(20, 100),
                         labels = c('Sites (n=20)', 'Sites (n=100)')),
         nindiv = factor(nindiv, levels = c(5, 50),
                         labels = c('Individuals (n=5)', 'Individuals (n=50)')),
         ntests = factor(ntests, levels = c(1, 2, 3),
                         labels = c('Sampling device (n=1)',
                                    'Sampling devices (n=2)',
                                    'Sampling devices (n=3)')))

# Distribution of Us by sample size
# Again hard to visualize the individual observations
ggplot(count.U, aes(x = equiv.perc)) +
  geom_histogram(binwidth = 0.005, col = 'grey25', fill = 'grey75') +
  scale_x_continuous(labels = scales::percent) +
  # Allow x to rescale to show the highest percentages of Us
  facet_grid(ntests ~ nsites + nindiv, scales = "free_x")  +
  labs(x = "Percent Equivocal") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8),
        legend.position = 'bottom',
        legend.direction = 'horizontal')

ggsave(paste0('figS1-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)



# FigS2: 2D % Us -------------------------------------------------------------


# Combine counts of U with model output
count.U %>%
  select(-nsites, -nindiv, -ntests) %>%
  left_join(output, ., by = c("dataset")) -> data


# Distribution of equiv increases as psi and theta11 increase
data %>%
  filter(model.flag == 'ModelU',
         parameter %in% c('psi','theta11')) %>% 
  select(dataset, model.flag, parameter, true.mean, bias, equiv.perc,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests, equiv.perc),
              names_from = parameter, values_from = c(true.mean, bias)) %>% 
  ggplot(aes(x = true.mean_theta11, y = true.mean_psi, col = equiv.perc*100)) +
  geom_point() +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_color_viridis_c("Percent\nambiguous\ndetections", option = 'mako', direction=-1) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 8),
        # legend.title = element_blank(),
        legend.text = element_text(size = 7),
        strip.text = element_text(size = 8))

ggsave(paste0('figS2-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)



# Figure 3 ----------------------------------------------------------------

# Only plot psi and theta11
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         parameter = factor(parameter, levels = c('theta11','psi'))) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n(),
            .by = c(model.flag, nsites, nindiv, ntests, parameter)) %>% 
  ggplot(aes(x=parameter, y=coverage, col=model.flag,
             shape=model.flag)) +
  geom_hline(yintercept = 0.95, lty = 2) +
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

ggsave(paste0('fig3-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 6.5, height = 7.5, unit = 'in',
       dpi = 600)


# FigS3: % runs converged---------------------------------------------------------------
output %>%
  mutate(parameter = factor(parameter,
                            levels = c('p01','p11','p001','p101','b3','b2','p111',
                                       'theta11','psi')),
         converge = ifelse(Rhat < 1.1, 1, 0)) %>%
  filter(parameter != 'deviance') %>% 
  summarize(converge = sum(converge, na.rm = T),
            .by = c(model.flag, nsites, nindiv, ntests, parameter)) %>%
  ggplot(aes(x=parameter, y=converge/1000, col=model.flag,
             shape=model.flag)) +
  geom_point(position = position_dodge(0.85), size = 2.5, alpha = 0.7) +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_x_discrete(labels = c(expression('Detection probability ('*p["01"]*')'),
                              expression('Detection probability ('*p[11]*')'),
                              expression('Detection probability ('*p["001"]*')'),
                              expression('Detection probability ('*p[101]*')'),
                              expression('Classification probability ('*b[3]*')'),
                              expression('Classification probability ('*b[2]*')'),
                              expression('Detection probability ('*p[111]*')'),
                              expression('Prevalence ('*theta[11]*')'),
                              expression('Occupancy ('*psi*')'))) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  coord_flip() +
  labs(y = '% Total runs converged') +
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


ggsave(paste0('figS3-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 8, height = 9, unit = 'in',
       dpi = 600)



# FigS4: occ/prev coverage ---------------------------------------------------------------

# Plot all parameters
output %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         parameter = factor(parameter,
                            levels = c('p01','p11','p001','p101','b3','b2','p111',
                                       'theta11','psi'))) %>%
  filter(parameter != 'deviance') %>%
  summarize(coverage = sum(coverage, na.rm = T)/n(),
            .by = c(model.flag, nsites, nindiv, ntests, parameter)) %>%
  ggplot(aes(x=parameter, y=coverage, col=model.flag,
             shape=model.flag)) +
  geom_hline(yintercept = 0.95, lty = 2) +
  geom_point(position = position_dodge(0.85), size = 2.5, alpha = 0.7) +
  facet_grid(ntests ~ nsites + nindiv) +
  scale_x_discrete(labels = c(expression('Detection probability ('*p["01"]*')'),
                              expression('Detection probability ('*p[11]*')'),
                              expression('Detection probability ('*p["001"]*')'),
                              expression('Detection probability ('*p[101]*')'),
                              expression('Classification probability ('*b[3]*')'),
                              expression('Classification probability ('*b[2]*')'),
                              expression('Detection probability ('*p[111]*')'),
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

ggsave(paste0('figS4-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 8, height = 9, unit = 'in',
       dpi = 600)





# FigS5: occ coverage (bin) ---------------------------------------------------------------
# Psi is binned
output %>%
  filter(parameter == 'psi') %>%
  mutate(psi.factor = cut(true.mean,
                          breaks = seq(0,1, by=0.1),
                          right = F),
         coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n(),
            .by = c(model.flag, nsites, nindiv, ntests, psi.factor)) %>%
  ggplot() +
  geom_hline(yintercept = 0.95, lty = 2) +
  geom_jitter(aes(x = psi.factor, y = coverage,
                  col = model.flag, shape = model.flag),
              size = 2, width = 0.25, alpha = 0.65) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression('Occupancy ('*psi*')'), y = '% Coverage', color = 'Model') +
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

ggsave(paste0('figS5-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 600,
       height = 8.5, width = 11, units = 'in')


# FigS6: prev coverage (bin) ---------------------------------------------------------------
# theta is binned
output %>%
  filter(parameter == 'theta11') %>%
  mutate(theta11.factor = cut(true.mean,
                              breaks = seq(0,1, by=0.1),
                              right = F),
         coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  summarize(coverage = sum(coverage, na.rm = T)/n(),
            .by = c(model.flag, nsites, nindiv, ntests, theta11.factor)) %>%
  ggplot() +
  geom_hline(yintercept = 0.95, lty = 2) +
  geom_jitter(aes(x = theta11.factor, y = coverage,
                  col = model.flag, shape = model.flag),
              size = 2, width = 0.25, alpha = 0.65) +
  scale_color_manual(name='', values = col.values) +
  scale_shape_manual(name='', values = shape.values) +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression('Prevalence ('*theta[11]*')'),
       y = '% Coverage', color = 'Model') +
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

ggsave(paste0('figS6-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 600,
       height = 8.5, width = 11, units = 'in')


# FigS7: accuracy -----------------------------------------------------
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(parameter = factor(parameter, levels=c('theta11','psi'))) %>%
  ggplot(aes(x = parameter, y = abs(bias), col = model.flag, group = model.flag)) +
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

ggsave(paste0('figS7-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


## FigS8: bias --------------------------------------------------------
output %>%
  filter(parameter %in% c('psi','theta11')) %>%
  mutate(parameter = factor(parameter, levels=c('theta11','psi'))) %>%
  ggplot(aes(x = parameter, y = bias, col = model.flag, group = model.flag)) +
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

ggsave(paste0('figS8-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


## FigS9: precision (95% BCI WIDTH) ------------------------------------
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

ggsave(paste0('figS9-',Sys.Date(),'.png'),
       path = 'output/figures/',
       width = 9, height = 7, unit = 'in',
       dpi = 600)


# Figure 4: noU, ntests=2 --------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('Model1-noU','Model2-noU',
                           'Model3-noU','ModelU'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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

ggsave(paste0('fig4-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# FigS10: noU, ntests=1 -------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-noU',
                           'Model2-noU','Model1-noU'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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

ggsave(paste0('figS10-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# FigS11: noU, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-noU',
                           'Model2-noU','Model1-noU'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-noU',
                                        'Model2-noU','Model3-noU'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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

ggsave(paste0('figS11-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# FigS12: ND, ntests=1 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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


# FigS13: ND, ntests=2 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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


# FigS14: ND, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-ND',
                           'Model2-ND','Model1-ND'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-ND',
                                        'Model2-ND','Model3-ND'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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


# FigS15: D, ntests=1 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling device (n=1)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>% 
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>% 
  unnest(cols = c(true.mean_psi, true.mean_theta11, bias_psi, bias_theta11, 
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

ggsave(paste0('figS15-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# FigS16: D, ntests=2 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling devices (n=2)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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

ggsave(paste0('figS16-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')


# FigS17: D, ntests=3 ------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag %in% c('ModelU','Model3-D',
                           'Model2-D','Model1-D'),
         ntests == "Sampling devices (n=3)") %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5,
         model.flag = factor(model.flag, 
                             levels = c('ModelU','Model1-D',
                                        'Model2-D','Model3-D'))) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter,
              values_from = c(true.mean, bias, coverage)) %>%
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

ggsave(paste0('figS17-',Sys.Date(),'.png'),
       path = 'output/figures/',
       dpi = 500,
       width = 12, height = 12, units='in')

# Figure 5: 2D bias ----------------------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         nsites == 'Sites (n=20)',
         nindiv == 'Individuals (n=5)',
         ntests == "Sampling devices (n=2)",
         model.flag == 'ModelU') %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter, values_from = c(true.mean, bias, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'both',
                              coverage_psi == T & coverage_theta11 == F ~ 'psi',
                              coverage_psi == F & coverage_theta11 == T ~ 'theta11',
                              coverage_psi == F & coverage_theta11 == F ~ 'neither')) -> tmp

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=bias_psi),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Bias: ',psi, sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw() -> error.psi

# To save other sample size combos, comment out the filtering above and save the error.psi figure.


ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=bias_theta11),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Bias: ', theta[11], sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw() -> error.theta11

# To save other sample size combos, comment out the filtering above and save the error.theta11 figure.


cowplot::plot_grid(error.psi, error.theta11, labels = 'AUTO')

ggsave(paste0('fig5-',Sys.Date(),'.png'),
       width = 11, height = 4, units = 'in',
       path = 'output/figures/',
       dpi = 500)



# FigS18: 2D bias psi ---------------------------------------------------------------

output %>%
  filter(parameter %in% c('theta11','psi'),
         model.flag == 'ModelU') %>%
  mutate(coverage = q97.5 > true.mean & true.mean > q2.5) %>%
  select(dataset, model.flag, parameter, true.mean, coverage, bias,
         nsites, nindiv, ntests) %>%
  pivot_wider(id_cols = c(dataset, model.flag, nsites, nindiv, ntests),
              names_from = parameter, values_from = c(true.mean, bias, coverage)) %>%
  mutate(coverage = case_when(coverage_psi == T & coverage_theta11 == T ~ 'both',
                              coverage_psi == T & coverage_theta11 == F ~ 'psi',
                              coverage_psi == F & coverage_theta11 == T ~ 'theta11',
                              coverage_psi == F & coverage_theta11 == F ~ 'neither')) -> tmp

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=bias_psi),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Bias: ',psi, sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw()

ggsave(paste0('figS18-',Sys.Date(),'.png'),
       width = 12, height = 9, units = 'in',
       path = 'output/figures/',
       dpi = 500)


# FigS19: 2D bias theta ---------------------------------------------------------------

# Make sure that you have the tmp file from Figure S6 loaded

ggplot(tmp) +
  geom_point(aes(x=true.mean_theta11, y=true.mean_psi, col=bias_theta11),
             size = 2, alpha = 0.75) +
  scale_color_viridis_c(expression(paste('Bias: ', theta[11], sep='')),
                        limits = c(-1,1), option='turbo') +
  facet_grid(ntests ~ nsites + nindiv) +
  labs(x = expression(paste("True prevalence (", theta[11],")", sep="")),
       y = expression(paste("True occupancy (", psi,")", sep=""))) +
  theme_bw()

ggsave(paste0('figS19-',Sys.Date(),'.png'),
       width = 12, height = 9, units = 'in',
       path = 'output/figures/',
       dpi = 500)


# End script

