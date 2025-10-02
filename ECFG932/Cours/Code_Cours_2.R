rm(list=ls())

library(tidyverse)
library(FactoMineR)
library(factoextra)

#-- Importation des données

fromages <- read.csv("fromages.csv",sep=";") %>%
  mutate_all(.funs = as.factor) %>%
  mutate(Liking=as.numeric(Liking))

#-- Cartoraphie interne des préférences

df_cart <- fromages %>%
  pivot_wider(id_cols = Produit,names_from = Consommateur,values_from = Liking) %>%
  as.data.frame()
row.names(df_cart) <- c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire")
df_cart <- df_cart %>% select(-Produit)


pca_cart <- PCA(df_cart,quali.sup = 1,graph = FALSE)

fviz_pca_biplot(pca_cart)
fviz_screeplot(pca_cart)


#-- Analyse des pénalités (Tous produits confondus)

cat3 <- function(x){
  res <- 1*(x<3) + 2*(x==3)+3*(x>3)
  return(as.factor(res))
}

df_penalty_all <- fromages %>%
  select(-Consommateur,-Produit) %>%
  mutate_at(2:10,.funs = as.numeric) %>%
  mutate_at(2:10,.funs = cat3) %>%
  pivot_longer(2:10,names_to = "Attribut",values_to = "EVAL_JAR") %>%
  arrange(Attribut,EVAL_JAR) %>%
  group_by(Attribut,EVAL_JAR) %>%
  mutate(Mean_Liking=mean(Liking),
         SD = sd(Liking),
         Nb_cat = n()) %>%
  group_by(Attribut) %>%
  mutate(Freq = Nb_cat/n()) %>%
  ungroup() %>%
  select(-Liking) %>%
  unique() %>%
  complete(Attribut,EVAL_JAR)

sum(is.na(df_penalty_all))

T_calc <- function(m_drop,n1,n2,s1,s2){
  sqrt(n1+n2-2)*m_drop/sqrt(((n1-1)*s1^2+(n2-1)*s2^2)*(1/n1+1/n2))
}

df_penalty_all <- df_penalty_all %>%
  group_by(Attribut) %>%
  mutate(Mean_drop = Mean_Liking[2]-Mean_Liking,
         T_stat = T_calc(Mean_drop,Nb_cat,Nb_cat[2],SD,SD[2]),
         DDL = Nb_cat+Nb_cat[2]-2,
         p_value = pt(-T_stat,DDL),
         Borne_Inf = Mean_drop - 
           qt(0.975,DDL)*sqrt(((Nb_cat-1)*SD^2+(Nb_cat[2]-1)*SD[2]^2)*(1/Nb_cat+1/Nb_cat[2]))/sqrt(DDL),
         Borne_Sup = Mean_drop +
           qt(0.975,DDL)*sqrt(((Nb_cat-1)*SD^2+(Nb_cat[2]-1)*SD[2]^2)*(1/Nb_cat+1/Nb_cat[2]))/sqrt(DDL)) %>%
  ungroup() %>%
  mutate(Comm= Freq >= 0.2 & Nb_cat >=15,
         Significatif = p_value < 0.05) %>%
  filter(EVAL_JAR!=2) %>%
  filter(Comm==TRUE) %>%
  select(-Comm)

#-- Analyse des pénalités (Beaufort)

beaufort <- fromages %>% filter(Produit=="B") %>%
  select(-Produit,-Consommateur) %>%
  mutate_all(.funs = as.numeric) %>%
  mutate_at(2:10,.funs = cat3)

df_penalty_beaufort <- beaufort %>%
  pivot_longer(cols=2:10,names_to = "Attribut",values_to = "Eval_JAR") %>%
  arrange(Attribut,Eval_JAR) %>%
  group_by(Attribut,Eval_JAR) %>%
  mutate(Mean_Liking = mean(Liking),
         SD=sd(Liking),
         Nb_cat=n()) %>%
  ungroup() %>%
  complete(Attribut,Eval_JAR) %>%
  select(-Liking) %>%
  unique()

sum(is.na(df_penalty_beaufort))

df_penalty_beaufort <- df_penalty_beaufort %>%
  group_by(Attribut) %>%
  mutate(Freq =Nb_cat/sum(Nb_cat),
         Mean_Drop = Mean_Liking[2]-Mean_Liking,
         T_stat = T_calc(Mean_Drop,Nb_cat,Nb_cat[2],SD,SD[2]),
         DDL=Nb_cat+Nb_cat[2]-2,
         p_value=pt(-T_stat,DDL),
         Borne_Inf = Mean_Drop -
           qt(0.975,DDL)*sqrt(((Nb_cat-1)*SD^2+(Nb_cat[2]-1)*SD[2]^2)*(1/Nb_cat+1/Nb_cat[2]))/sqrt(DDL),
         Borne_Sup = Mean_Drop +
           qt(0.975,DDL)*sqrt(((Nb_cat-1)*SD^2+(Nb_cat[2]-1)*SD[2]^2)*(1/Nb_cat+1/Nb_cat[2]))/sqrt(DDL)) %>%
  ungroup() %>%
  mutate(Comm = Freq >=0.2 & Nb_cat >=15,
         Significatif = p_value <0.05,
         Significatif = factor(Significatif,levels = c(TRUE,FALSE),labels=c("Oui","Non")), 
         Penalty_W = Mean_Drop*Freq*Comm) %>%
  filter(Eval_JAR!=2) %>%
  mutate(Attribut = paste(Attribut,Eval_JAR,sep="_"))
  

ggplot(df_penalty_beaufort) + aes(x=Attribut,y=Penalty_W,fill = Significatif)+
  geom_bar(stat = "identity",color="grey20")+
  geom_hline(yintercept = 1,linetype="dashed",color='skyblue3',linewidth=1)+
  theme_bw()+
  labs(x="Attribut",y="Pénalité pondérée",title = "Pénalités pondérées des attributs sur le beaufort")+
  scale_fill_brewer(palette = "Set2",direction = -1)+
  theme(axis.text.x = element_text(angle=90,vjust=0.6))

#-- Codage Dummy variables

dummy_codage <- function(x){
  y <- as.numeric(x)
  res <- -2*(y==1) + (-1)*(y==2) + 0*(y==3) + 1*(y==4) + 2*(y==5)
  return(res)
}

fromages_dummy <- fromages %>%
  mutate_at(.vars = 4:12,.funs = dummy_codage)
fromages_dummy2 <- matrix(rep(NA,nrow(fromages)*(3+2*9)),nrow = nrow(fromages)) %>%
  as.data.frame()

for(i in 0:8){
  fromages_dummy2[,4+2*i] <- fromages_dummy[4+i]*(fromages_dummy[4+i]<0)
  fromages_dummy2[,5+2*i] <- fromages_dummy[4+i]*(fromages_dummy[4+i]>0)
  colnames(fromages_dummy2)[4+2*i] <- paste(colnames(fromages_dummy)[4+i],"-",sep="")
  colnames(fromages_dummy2)[5+2*i] <- paste(colnames(fromages_dummy)[4+i],"+",sep="")
}

fromages_dummy2[,1:3] <- fromages_dummy[,1:3]

colnames(fromages_dummy2)[1:3] <- colnames(fromages_dummy)[1:3]

fromages_dummy2 <- fromages_dummy2 %>% select(-Consommateur)
xtable(head(fromages_dummy2[,1:6]))
#-- ACP sur dummy variables
library(RColorBrewer)
pca_dummy <- PCA(X = fromages_dummy2,quali.sup = 1,
                 quanti.sup = 2,graph = FALSE)

plot.PCA(pca_dummy,choix="var",
         select = "contrib 10",
         col.var = rep(brewer.pal(3,"Set2")[1:2],9),col.quanti.sup = brewer.pal(8,"Set2")[3])

plot.PCA(pca_dummy,choix = "ind",invisible = "ind",
         col.quali = brewer.pal(8,"Set2"))
plotellipses(pca_dummy,invisible="ind")
