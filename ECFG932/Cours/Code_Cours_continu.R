rm(list = ls())

library(tidyverse)
library(xtable)

#-- Création des données

fromages <- read.csv("fromages.csv",sep=";") %>%
  mutate(Consommateur = as.factor(Consommateur),
         Produit=as.factor(Produit))

my_modif <- function(x){
  n <- length(x)
  y <- x+rnorm(n,0,0.2)
  return(y)
}

my_modif_add <- function(x){return(x+2.1)}

set.seed(123)

fromages_cont <- fromages %>%
  mutate_if(.predicate = is.numeric,
            .funs = my_modif) %>%
  mutate_at(4:12,.funs = my_modif_add) %>%
  mutate_if(.predicate = is.numeric,
            .funs=round,1)

summary(fromages_cont)

xtable(head(fromages_cont))

#-- ANOVA

mod_aov1 <- aov(data = fromages_cont,formula = sel_g~Produit+Consommateur)
mod_aov2 <- aov(data = fromages_cont,formula = fr_g~Produit+Consommateur)
mod_aov3 <- aov(data = fromages_cont,formula = crem_tx~Produit+Consommateur)
sum_aov1 <- summary(mod_aov1)
sum_aov2 <- summary(mod_aov2)
sum_aov3 <- summary(mod_aov3)

sum_aov1
sum_aov2
sum_aov3

tab_test <- cbind.data.frame(mod_aov1$coefficients[1:8],
                             mod_aov2$coefficients[1:8],
                             mod_aov3$coefficients[1:8])
colnames(tab_test) <- c("sel_g","fr_g","crem_tx")
tab_test[1,] <- rep(0,3)

xtable(tab_test)

#-- Analyse du beaufort

beaufort <- fromages_cont %>%
  filter(Produit=="B") %>%
  select(-Produit,-Consommateur,-Liking)
n <- 72
beaufort_effect <- beaufort %>%
  pivot_longer(cols=1:9,names_to = "Attribut",
               values_to = "Note_JAR") %>%
  group_by(Attribut) %>%
  mutate(Mean = mean(Note_JAR),
         SD = sd(Note_JAR)) %>%
  ungroup() %>%
  select(-Note_JAR) %>%
  unique() %>%
  mutate(T_stat = sqrt(n)*(Mean-5)/SD)
xtable(t(beaufort_effect))
