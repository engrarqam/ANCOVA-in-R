library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyverse)

anxiety <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANCOVA/one_way_ancova.csv')
anxiety

######################################## Check assumptions

##################### Linearity assumption

ggscatter(
  anxiety, x = "pretest", y = "posttest",
  color = "group", add = "reg.line"
)  +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)
  )     

##################### Homogeneity of regression slopes

anxiety %>% anova_test(posttest ~ group*pretest)

##################### Normality of residuals

# Fit the model, the covariate goes first
model <- lm(posttest ~ pretest + group, data = anxiety)
model

# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>% select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)


# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

##################### Homogeneity of variances(homoscedasticity)
model.metrics %>% levene_test(.resid ~ group)

##################### Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

################################## Computation
res.aov <- anxiety %>% anova_test(posttest ~ pretest + group)
get_anova_table(res.aov)

################################## Post-hoc test
# Pairwise comparisons
library(emmeans)
pwc <- anxiety %>% 
  emmeans_test(
    posttest ~ group, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc


# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)

################################# Report
# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group", y = "emmean", 
       color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07")) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

