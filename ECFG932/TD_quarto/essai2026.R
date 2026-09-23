rm(list=ls())

# LIBRAIRIES

library(tidyverse)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(SensoMineR)

# Question 1

orange <- read.csv("orange.csv",sep=";") %>%
  mutate(Consumer = as.factor(Consumer),
         Juice=as.factor(Juice))
# Question 2

orange_cart_pref <- orange %>%
  pivot_wider(id_cols = 1,names_from = 1,values_from = 3) %>%
  column_to_rownames(var = "Juice")

PCA_cart_pref <- PCA(X = orange_cart_pref,graph = FALSE)

fviz_pca_biplot(PCA_cart_pref)

# Question 4

# 2JPR et 7TWA sont segmentants, tout comme 6TPR et 4JWR. On pourrait dire de même pour 1JPA et 3JWA, même
# si 3JWA est assez proche de l'origine (c'est donc moins marqué).
# La pulpe semble être un facteur très segmentant.

# Question 5

table_liking <- orange %>%
  group_by(Juice) %>%
  summarise(Mean_Liking = mean(Liking))

# Question 6

ggplot(table_liking)+aes(x=Juice,y=Mean_Liking,fill = Mean_Liking)+
  geom_bar(col='grey50',stat = "identity")+
  theme_minimal()+
  scale_fill_distiller(palette = "RdYlBu")+
  labs(title = "Liking moyen par jus",
       x="Jus d'orange",
       y="Liking moyen (sur 10)")+
  geom_text(aes(label = round(Mean_Liking,2)),position = position_stack(vjust = 0.9))+
  guides(fill="none")

# Question 7

ggplot(orange) + aes(x=Juice,y=Liking,fill = Juice)+
  geom_boxplot(color="grey50")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")+
  labs(title = "Distribution des likings par jus.",
       x="Jus d'orange")+
  guides(fill="none")

# Question 8

orange_graph_jar <- orange %>%
  pivot_longer(cols=4:9,names_to = "Attribut",values_to = "Note_JAR")%>%
  select(-Liking,-Consumer) %>%
  group_by(Juice,Attribut,Note_JAR) %>%
  summarise(Effectifs= n()) %>%
  ungroup() %>%
  complete(Juice,Attribut,Note_JAR,fill = list(Effectifs=0)) %>%
  mutate(Note_JAR = as.factor(Note_JAR))

ggplot(orange_graph_jar) + aes(x=Attribut,y=Effectifs,fill = Note_JAR)+
  geom_bar(stat="identity",color="grey50")+
  facet_wrap(~Juice)+
  scale_fill_brewer(palette = "RdYlBu",direction = -1)+
  theme_minimal()+
  labs(title = "Notes JAR des jus d'orange pour chaque attribut.")+
  guides(fill=guide_legend(title = "Note JAR"))

# Question 10

cat3 <- function(x){
  as.factor(1*(x==1 | x==2) + 2*(x==3) + 3*(x==4 | x==5))
}


# Question 11

orange3cat <- orange %>%
  mutate_at(4:9,.funs = cat3)

# Question 12

for(i in 4:9){
  orange3cat[,i] <- paste(colnames(orange3cat)[i],orange3cat[,i],sep="_")
}

orange3cat <- orange3cat %>%
  mutate_at(4:9,.funs = as.factor)

# Question 13

jar_orange <- JAR(x=orange3cat,
                  col.p = 2,
                  col.j = 1,
                  col.pref = 3,
                  jarlevel = 2)

jar_orange$penalty1
jar_orange$Frequency

# Question 14

par(mfrow=c(2,4))
for (i in 1:nlevels(orange3cat$Juice)){
  plot(jar_orange,name.prod=levels(orange3cat$Juice)[i],model=1, 
       ylab="mean drops (all products")
  points(jar_orange$Frequency[,i],jar_orange$penalty1[,1],
         pch=19, col=rep(c("skyblue","tomato")))
}
par(mfrow=c(1,1))

# Question 15

# Pas d'analyse des pénalités par produit.

# Question 16, 17,18, 19

df_penalty <- orange %>%
  select(-Consumer) %>%
  mutate_at(3:8,.funs = cat3) %>%
  pivot_longer(cols = 3:8,names_to = "Attribut",values_to = "Note_JAR") %>%
  group_by(Juice,Attribut,Note_JAR) %>%
  mutate(Mean_Liking = mean(Liking),
         SD_Liking = sd(Liking),
         Effectifs = n()) %>%
  ungroup() %>%
  select(-Liking) %>%
  unique() %>%
  arrange(Juice,Attribut,Note_JAR) %>%
  group_by(Juice,Attribut) %>%
  mutate(Mean_drop = Mean_Liking[2]-Mean_Liking,
         DDL = Effectifs+Effectifs[2]-2)%>%
  ungroup()

# Question 20

T_calc <- function(mean_drop,n_jar,n_else,sd_jar,sd_else){
  mean_drop*sqrt(n_jar+n_else-2)/
    sqrt(((n_jar-1)*sd_jar^2 + (n_else-1)*sd_else^2)*(1/n_jar+1/n_else))
}

# Question 21

borne_sup <- function(mean_drop,n_jar,n_else,sd_jar,sd_else){
  mean_drop + qt(0.975,n_jar+n_else-2)*
    sqrt(((n_jar-1)*sd_jar^2 + (n_else-1)*sd_else^2)*(1/n_jar+1/n_else))/
    sqrt(n_jar+n_else-2)
}

borne_inf <- function(mean_drop,n_jar,n_else,sd_jar,sd_else){
  mean_drop - qt(0.975,n_jar+n_else-2)*
    sqrt(((n_jar-1)*sd_jar^2 + (n_else-1)*sd_else^2)*(1/n_jar+1/n_else))/
    sqrt(n_jar+n_else-2)
}

# Question 22

df_penalty <- df_penalty %>%
  group_by(Juice,Attribut) %>%
  mutate(T_stat = T_calc(Mean_drop,Effectifs[2],Effectifs,SD_Liking[2],SD_Liking),
         b_inf = borne_inf(Mean_drop,Effectifs[2],Effectifs,SD_Liking[2],SD_Liking),
         b_sup = borne_sup(Mean_drop,Effectifs[2],Effectifs,SD_Liking[2],SD_Liking),
         Freq = Effectifs/sum(Effectifs)) %>%
  ungroup()

# Question 23, 24, 25

df_penalty <- df_penalty %>%
  mutate(pvalue = 1-pt(T_stat,DDL),
         Freq_OK = (Freq >= 0.2 & Effectifs >=15),
         significatif = pvalue <0.05) %>%
  filter(Note_JAR!=2)

# Question 26

ggplot(df_penalty) + aes(x=Freq,y=Mean_drop,group = Juice,colour=significatif)+
  geom_point()+
  geom_segment(aes(y=b_inf,yend = b_sup))+
  geom_text(aes(label = paste(Attribut,Note_JAR,sep="_")),size=2,
            position = position_nudge(x=0.04))+
  geom_vline(xintercept = 0.2,colour="skyblue",linetype="dashed")+
  theme_bw()+
  facet_wrap(~Juice)

# Question 27

df_penalty <- df_penalty %>%
  mutate(Penalite_W = Freq*Mean_drop*Freq_OK)
