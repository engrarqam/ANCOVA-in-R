library(tidyverse)
library(ggpubr)
library(rstatix)
library(tidyverse)

stress <- read.csv('C:/Users/Administrator/Ai/R/inferential/ANCOVA/two_way_ancova.csv')
stress

######################################## Check assumptions

##################### Linearity assumption

ggscatter(
  stress, x = "age", y = "score",
  facet.by  = c("exercise", "treatment"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)   

##################### Homogeneity of regression slopes

stress %>%
  anova_test(
    score ~ age + treatment + exercise + 
      treatment*exercise + age*treatment +
      age*exercise + age*exercise*treatment
  )

stress %>%
  unite(col = "group", treatment, exercise) %>%
  anova_test(score ~ group*age)

##################### Normality of residuals

# Fit the model, the covariate goes first
model <- lm(score ~ age + treatment*exercise, data = stress)

# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

##################### Homogeneity of variances(homoscedasticity)
levene_test(.resid ~ treatment*exercise, data = model.metrics)

##################### Outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

################################## Computation
res.aov <- stress %>% 
  anova_test(score ~ age + treatment*exercise)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

############## Compute simple main effects

# Effect of treatment at each level of exercise
stress %>%
  group_by(exercise) %>%
  anova_test(score ~ age + treatment)

############# Compute pairwise comparisons
# Pairwise comparisons
pwc <- stress %>% 
  group_by(exercise) %>%
  emmeans_test(
    score ~ treatment, covariate = age,
    p.adjust.method = "bonferroni"
  )
pwc %>% filter(exercise == "high")

######## You can do the same post-hoc analyses for the exercise variable at each level of treatment variable

############## Compute simple main effects

# Effect of exercise at each level of treatment
stress %>%
  group_by(treatment) %>%
  anova_test(score ~ age + exercise)

############# Compute pairwise comparisons
# Pairwise comparisons
pwc2 <- stress %>% 
  group_by(treatment) %>%
  emmeans_test(
    score ~ exercise, covariate = age,
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
pwc2 %>% filter(treatment == "yes")




############################################## Report
# Create a line plot:
# Line plot
lp <- ggline(
  get_emmeans(pwc), x = "exercise", y = "emmean", 
  color = "treatment", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = treatment), 
    width = 0.1
  )

# Add p-values
# Comparisons between treatment group at each exercise level
pwc <- pwc %>% add_xy_position(x = "exercise", fun = "mean_se", step.increase = 0.2)
pwc.filtered <- pwc %>% filter(exercise == "high")
lp + 
  stat_pvalue_manual(
    pwc.filtered, hide.ns = TRUE, tip.length = 0,
    bracket.size = 0
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )











