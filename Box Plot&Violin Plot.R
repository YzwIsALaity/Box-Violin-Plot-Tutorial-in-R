# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(ggpubr)

Dt <- read.csv('Box Plot&Violin Plot.csv')
kable(head(Dt))

# Set Illness and Sex as factors
Dt$Illness <- factor(Dt$Illness, levels = c("Healthy Control", "Mild", "Moderate", "Severe"))
Dt$Sex <- factor(Dt$Sex, levels = c('Male', 'Female'))

# Version 0.0
ggplot(Dt, aes(x = Illness, y = Biomarker)) +
  geom_boxplot(alpha = 0.5) + 
  xlab('Illness Severity') + ylab('Biomarker Level') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

# we can preselect color for each box 
Col <- c("#FF0000", "#80FF00")
# Version 1.0
ggplot(Dt, aes(x = Illness, y = Biomarker, fill = Sex)) +
  geom_boxplot(alpha = 0.5) +                                       # 'alpha = 0.5' control the transparency of colors
  scale_fill_manual(values = Col) +                                 # use preselected color
  xlab('Illness Severity') + ylab('Biomarker Level') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

# Version 2.0
ggplot(Dt, aes(x = Illness, y = Biomarker, fill = Sex)) +
  geom_boxplot(alpha = 0.5) +                                       # 'alpha = 0.5' control the transparency of colors
  stat_compare_means(aes(group = Sex),                              # 'group = Sex' choose groups for comparison
                     method = 'wilcox.test',                        # we use wilcox test for comparison
                     label = 'p.format', 
                     vjust = -0.65) +
  scale_fill_manual(values = Col) +                                 # use preselected color
  xlab('Illness Severity') + ylab('Biomarker Level') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

# Version 3.0
ggplot(Dt, aes(x = Illness, y = Biomarker)) +
  geom_violin(width = 1, 
              position = position_dodge(0.7)) +                     # each violin has larger size
  geom_boxplot(alpha = 0.7, width = 0.3, 
               position = position_dodge(0.7)) +                    # this size of each box is smaller than a violin
  xlab('Illness Severity') + ylab('Biomarker Level') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

# Version 4.0
ggplot(Dt, aes(x = Illness, y = Biomarker, fill = Sex)) +
  geom_violin(aes(fill = Sex), width = 1, alpha = 0.5,
              position = position_dodge(1)) +     
  geom_boxplot(aes(fill = Sex), alpha = 0.5, width = 0.1, 
               position = position_dodge(1)) +                     
  stat_compare_means(aes(group = Sex),                              # 'group = Sex' choose groups for comparison
                     method = 'wilcox.test',                        # we use wilcox test for comparison
                     label = 'p.format', 
                     vjust = -0.65) +
  scale_fill_manual(values = Col) +                                 # use preselected color
  xlab('Illness Severity') + ylab('Biomarker Level') +
  theme_bw() +                                                      # dark-on-light theme
  theme(panel.border = element_blank(),                             # these four are for the background and grid
        panel.background = element_blank(),                    
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line.x = element_line(),                               # these two are for the axis line
        axis.line.y = element_line(),
        axis.text.x = element_text(colour = "black", size = 11),    # there two are for texts in axes
        axis.text.y = element_text(colour = "black", size = 11),
        axis.ticks.x = element_line(),                              # these two are for ticks in axes
        axis.ticks.y = element_line(),
        axis.title.x = element_text(colour = "black", size = 11, face = 'bold', vjust = -1),                              
        axis.title.y = element_text(colour = "black", size = 11, face = 'bold'),
        legend.title = element_text(colour = "black", size = 11, face = 'bold')) 

