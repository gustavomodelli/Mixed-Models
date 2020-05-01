library(tidyverse)
library(lme4)
library(lmtest)
library(sjPlot)
library(vtree)

island <- read_csv('Islander_data.csv')
head(island)
dim(island)

island$id <- 1:nrow(island)
island <- island %>% 
  mutate(
    Drug = fct_recode(Drug,
                      Alprazolam = 'A',
                      Triazolam = 'T',
                      Placebo = 'S'))

island <- island %>% 
  mutate(
    Drug = fct_relevel(Drug, 'Placebo', 'Alprazolam','Triazolam')
  )

vtree(island, 'Drug Dosage', summary=c("Diff \nScore: mean (SD) %mean% (%SD%)"))

anal <- aov(Diff ~ Drug, data = island)
model <- lm(Diff ~ Drug + Drug: Dosage, data = island)
summary(model)

##Data in long format
island_long <- island %>% 
  pivot_longer(
    cols = starts_with('Mem_Score_'),
    names_to = 'time',
    values_to = 'Mem_Score'
  ) 

island_long$time <- str_replace(island_long$time, 'Mem_Score_', '')

island_long <- island_long %>% 
  mutate(
    time = fct_relevel(time, 'Before', 'After')
  )

ggplot(island_long, aes(time, Mem_Score, fill = time))+
  geom_boxplot()+
  facet_wrap(~ Drug)


ggplot(island_long, aes(time, Mem_Score, fill = Drug))+
  geom_boxplot()+
  facet_wrap(~ as.factor(Dosage))

island_long %>% 
  group_by(Drug, time) %>% 
  summarise(Mem_score = mean(Mem_Score),
            sd_m = sd(Mem_Score) ) %>% 
  ggplot(aes(time, Mem_score, group = Drug, color = Drug))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = Mem_score-sd_m, ymax = Mem_score+sd_m),
                width = .2, position=position_dodge(0.05))


model0 <- lmer(Mem_Score ~ time + Drug + (1 | id), data = island_long)
tab_model(model0)

model1 <- lmer(Mem_Score ~ time + Drug + Drug:Dosage + (1 | id), data = island_long)
summary(model1)
tab_model(model1)

model2 <- lmer(Mem_Score ~ age + time + Drug + Drug:Dosage + (1 | id), data = island_long)
summary(model2)
tab_model(model2)
plot_model(model2)
