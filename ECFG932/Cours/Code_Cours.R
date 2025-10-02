rm(list=ls())

#-- Libraries
library(tidyverse)
library(xtable)
library(RColorBrewer)
library(reshape2)
library(SensoMineR)

#-- Importation des données
fromages <- read.csv("fromages.csv",sep=";")
summary(fromages)
fromages <- fromages %>%
  mutate(Consommateur = as.factor(Consommateur),
         Produit = factor(Produit,
                          levels = c("B","C1","C2","CE","E","M","R","S"),
                          labels=c("Beaufort","Cantal1","Cantal2","Comté","Emmental",
                                   "Morbier","Reblochon","St-Nectaire"))) %>%
  mutate_at(.vars = 4:12,.funs = as.factor) 

#-- Représentations graphiques

### Notes

df_moy_liking <- fromages %>%
  group_by(Produit) %>%
  summarise(Note = mean(Liking)) %>%
  ungroup() %>%
  arrange(Note)

ggplot(df_moy_liking)+aes(x=Produit,y=Note,fill=Note)+
  geom_bar(stat = "identity",col="grey20",width = 0.5,show.legend = FALSE)+
  theme_bw()+
  labs(y="Liking",title = "Note moyenne des fromages")+
  scale_fill_distiller(palette="RdYlBu")+
  geom_text(aes(label=round(Note,2)),position = position_stack(vjust = 0.9))

ggplot(fromages)+aes(x=Produit,y=Liking,fill = Produit)+
  geom_boxplot(show.legend = FALSE)+
  theme_bw()+
  labs(title = "Distribution des notes par fromage")+
  scale_fill_brewer(palette="Set2")

  
### Liens Attributs-Produits

df_prod <- fromages %>%
  reframe(across(.cols = 4:11,.fns = table),.by = Produit) %>%
  mutate_at(.vars = 2:9,.funs = as.numeric) %>%
  group_by(Produit) %>%
  mutate(Note_Jar = as.factor(1:5)) %>%
  ungroup() %>%
  pivot_longer(cols = 2:9,names_to = "Attribut",values_to = "Effectif")

ggplot(df_prod)+aes(x=Attribut,y=Effectif,fill = Note_Jar)+
  geom_bar(stat = "identity",color="grey20")+
  facet_wrap(~Produit,nrow=4,ncol=3)+
  theme_bw()+
  labs(title = "Résumé des produits par attribut")+
  guides(fill=guide_legend(title = "Note JAR"))+
  theme(axis.text.x = element_text(angle=90,size = 8,vjust = 0.5))+
  scale_fill_brewer(palette = "RdYlBu",direction = -1)

ggplot(df_prod)+aes(x=Produit,y=Effectif,fill = Note_Jar)+
  geom_bar(stat = "identity",color="grey20")+
  facet_wrap(~Attribut,nrow=4,ncol=3)+
  theme_bw()+
  labs(title = "Résumé des attributs par produit")+
  guides(fill=guide_legend(title = "Note JAR"))+
  theme(axis.text.x = element_text(angle=90,size = 8,vjust = 0.5))+
  scale_fill_brewer(palette = "RdYlBu",direction = -1)

#-- Analyse des pénalités

### Tous produits confondus

cat3 <- function(x){
  1*(x<3)+2*(x==3)+3*(x>3)
}

fromages_3cat <- fromages %>%
  mutate_at(.vars = 4:12,.funs = as.numeric) %>%
  mutate_at(.vars = 4:12,.funs = cat3)

for(i in 4:12){
  fromages_3cat[,i] <- factor(fromages_3cat[,i],
                              levels = c(1,2,3),
                              labels = paste(colnames(fromages_3cat)[i],1:3,sep="_"))
}


JAR_fromages <- JAR(x=fromages_3cat,
    col.p = 2,
    col.j=1,
    col.pref=3,
    jarlevel=2)

JAR_fromages$penalty1

plot(JAR_fromages,name.prod = "Beaufort")

### Produit Beaufort

beaufort_3cat <- fromages %>%
  filter(Produit=="Beaufort") %>%
  mutate_at(.vars=4:12,.funs=as.numeric) %>%
  mutate_at(.vars=4:12,.funs = cat3) %>%
  mutate_at(.vars=4:12,.funs=as.factor) %>%
  select(-Consommateur,-Produit) %>%
  pivot_longer(cols=2:10,names_to = "Attribut",values_to = "Note_JAR")
summary(beaufort_3cat)

freq_beaufort_3cat <- beaufort_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  mutate(Effectifs = n(),
         Frequence = n()/72) %>%
  ungroup() %>%
  filter(Note_JAR!=2) %>%
  select(-Liking) %>%
  unique() %>%
  arrange(Attribut,Note_JAR)

penalty_beaufort <- beaufort_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  summarise(Moyenne = mean(Liking)) %>%
  group_by(Attribut) %>%
  mutate(Moyenne_JAR = Moyenne[2]) %>%
  ungroup() %>%
  filter(Note_JAR!=2) %>%
  mutate(Penalite = Moyenne_JAR-Moyenne) %>%
  mutate(Attribut=paste(Attribut,Note_JAR)) %>%
  select(-Note_JAR) %>%
  arrange(Attribut) %>%
  mutate(Effectifs = freq_beaufort_3cat$Effectifs,
         Frequence=freq_beaufort_3cat$Frequence) %>%
  mutate(Validite = Frequence >= 0.2 & Effectifs >=15) %>%
  mutate(Penalite = Penalite*Validite)

ggplot(penalty_beaufort) +aes(x=Attribut,y=Penalite,fill = Validite)+
  geom_bar(stat = "identity",color="grey20")+
  theme_bw()+
  scale_fill_brewer(palette="Set2")+
  theme(axis.text.x = element_text(angle=75,vjust = 0.5))+
  labs(title = "Pénalités des différents attributs sur le Beaufort",
       y="Pénalité")+
  guides(fill="none")

#### On regarde la significativité

sign_beaufort <- fromages_3cat %>%
  filter(Produit=="Beaufort") %>%
  select(-Produit,-Consommateur) %>%
  filter(sel_g =="sel_g_3" | sel_g == "sel_g_2") %>%
  select(Liking,sel_g) %>%
  group_by(sel_g) %>%
  summarise(Variance = var(Liking)*71/72,
            Moyenne = mean(Liking)) %>%
  ungroup()

sel_test <-sqrt(65)*(sign_beaufort$Moyenne[2]-
            sign_beaufort$Moyenne[1])/(sqrt(18*sign_beaufort$Variance[2]+49*sign_beaufort$Variance[1])*(1/18+1/49))

pt(sel_test,df = 65)
sel_test
qt(0.05,df=65)

#### On regarde la significativité des attributs pour le beaufort

freq_sign_beaufort <- beaufort_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  mutate(Effectifs = n(),
         Frequence = n()/72) %>%
  ungroup() %>%
  select(-Liking) %>%
  unique() %>%
  arrange(Attribut,Note_JAR)

var_beaufort <- beaufort_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  summarise(Var = var(Liking)*71/72) %>%
  ungroup()

my_t_stat <- function(n1,n2,m1,m2,v1,v2){
  sqrt(n1+n2-2)*(m1-m2)/(sqrt((n1*v1+n2*v2)*(1/n1+1/n2)))
}

sign_beaufort <- beaufort_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  summarise(Moyenne = mean(Liking)) %>%
  group_by(Attribut) %>%
  mutate(Moyenne_JAR = Moyenne[2]) %>%
  ungroup() %>%
  mutate(Penalite = Moyenne_JAR-Moyenne) %>%
  arrange(Attribut,Note_JAR) %>%
  mutate(Effectifs = freq_sign_beaufort$Effectifs,
         Frequence = freq_sign_beaufort$Frequence) %>%
  mutate(Validite = Frequence >= 0.2 & Effectifs >=15) %>%
  #mutate(Penalite = Penalite*Validite,Var = var_beaufort$Var) %>%
  mutate(Var = var_beaufort$Var) %>%
  group_by(Attribut) %>%
  mutate(T_beaufort = my_t_stat(n1=Effectifs,n2=Effectifs[2],
                                m1=Moyenne,m2=Moyenne_JAR,
                                v1=Var,v2=Var[2]),
         DL = Effectifs+Effectifs[2]-2) %>%
  ungroup() %>%
  #filter(Note_JAR!=2) %>%
  mutate(p_beaufort = pt(T_beaufort,df=DL)) %>%
  mutate(signifificativite = p_beaufort < 0.05) %>%
  select(Attribut,Note_JAR,Effectifs,Frequence,Moyenne,Var,Validite,Penalite,T_beaufort,p_beaufort,signifificativite)%>%
  #mutate(Attribut=paste(Attribut,Note_JAR,sep="_")) %>%
  mutate(Frequence=round(Frequence*100,2),
         Moyenne=round(Moyenne,2),
         Penalite=round(Penalite,2))

ggplot(sign_beaufort)+aes(x=Frequence,y=Penalite,color = signifificativite)+
  geom_point()+
  geom_text(aes(label = Attribut),size=2,position = position_nudge(x=,y=-0.1),
            show.legend = FALSE)+
  theme_bw()+
  labs(title = "Graphique croisé des pénalités et des pourcentages pour le Beaufort",
       x="Pourcentage de consommateurs insatisfaits",y="Pénalité")+
  scale_color_brewer(palette = "Set2",labels=c("Non","Oui"))+
  guides(color=guide_legend(title = "Significativité à 5%"))+
  geom_vline(xintercept = 20,linetype="dashed",color="skyblue",
             linewidth=1.2)
sign_beaufort <- sign_beaufort %>%
  filter(Validite)
#### Intervalles de confiance des pénalités

conf_beaufort <- sign_beaufort %>%
  select(Attribut,Note_JAR,Effectifs,Frequence,Moyenne,
         Var,Penalite,signifificativite) %>%
  group_by(Attribut) %>%
  mutate(Borne_Inf=Moyenne[2]-Moyenne+qt(0.025,
                                         Effectifs+Effectifs[2]-2)*sqrt((Effectifs*Var+Effectifs[2]*Var[2])*(1/Effectifs+1/Effectifs[2]))/sqrt(Effectifs+Effectifs[2]-2),
         Borne_Sup=Moyenne[2]-Moyenne-qt(0.025,
                                         Effectifs+Effectifs[2]-2)*sqrt((Effectifs*Var+Effectifs[2]*Var[2])*(1/Effectifs+1/Effectifs[2]))/sqrt(Effectifs+Effectifs[2]-2)) %>%
  ungroup() %>%
  filter(Note_JAR!=2) %>%
  mutate(Attribut=paste(Attribut,Note_JAR,sep="_")) %>%
  filter(Note_JAR!=2) %>%
  mutate(Borne_Inf=if_else(Borne_Inf<0,0,Borne_Inf))

ggplot(conf_beaufort)+aes(x=Frequence,y=Penalite,color=signifificativite)+
  geom_point()+
  geom_segment(aes(x=Frequence,y=Borne_Inf,xend=Frequence,yend = Borne_Sup),color="black",show.legend = FALSE)+
  geom_text(aes(label=Attribut),position = position_nudge(x=0.25,y=0.35),
            size=2.5,show.legend = FALSE)+
  theme_bw()+
  scale_color_brewer(palette = "Set2",labels=c("Non","Oui"))+
  guides(color=guide_legend(title = "Significativité"))+
  labs(y="Pénalité",x="Pourcentage",title = "Intervalles de confiance au niveau 95% des pénalités")+
  geom_vline(xintercept = 20,linetype="dashed",color="skyblue",linewidth=1.5)

### TEST ANOVA

beaufort_aov <- fromages_3cat %>%
  filter(Produit=="Beaufort",
         sel_g!="sel_g_1") %>%
  select(-Produit,-Consommateur)  
summary(beaufort_aov)
mod_aov <- lm(data=beaufort_aov,
              formula = Liking~sel_g)
sum_aov <- summary(mod_aov)
sum_aov$coefficients[2,4]/2
x <- beaufort_aov$Liking[which(beaufort_aov$sel_g=="sel_g_3")]
y <- beaufort_aov$Liking[which(beaufort_aov$sel_g=="sel_g_2")]
t.test(x,y,var.equal = TRUE,alternative = "less")
#-- Analyse exploratoire

### ACP
library(FactoMineR)
library(factoextra)
rm(list = ls())

fromages <- read.csv("fromages.csv",sep=";") %>%
  select(-Consommateur)
summary(fromages)

fromages[577,] <- c("Ideal",9,rep(3,9))

fromages <- fromages %>%
  mutate(Produit=as.factor(Produit)) %>%
  mutate_at(2:11,as.numeric)

pca1 <- PCA(X=fromages,quali.sup = 1,quanti.sup = 2,graph = FALSE)

plot.PCA(pca1,choix="var")
plotellipses(pca1,invisible="ind")

summary(fromages)
fromages_agg <- fromages %>%
  group_by(Produit) %>%
  mutate_at(.vars = 2:11,.funs=mean) %>%
  ungroup() %>%
  unique()

pca2 <- PCA(X=fromages_agg,quali.sup = 1,quanti.sup = 2,graph = FALSE)

plot.PCA(pca2,choix="var")
plotellipses(pca2,invisible="ind")

### ACM

fromages_cat <- fromages %>%
  filter(Produit!="Ideal") %>%
  select(-Liking) %>%
  mutate_at(2:10,as.factor)

mca1 <- MCA(fromages_cat,quali.sup = 1,graph=FALSE)
ncat <- 5
njar <- 9
plot(mca1,invisible = "ind",choix="ind",col.quali.sup = "black",
     col.var=rep(brewer.pal(5,"Set2"),length=ncat*njar),
     ggoptions = list(size=4))

### ACM avec données agrégées

fromages <- fromages %>%
  filter(Produit != "Ideal") %>%
  mutate(Produit=factor(Produit)) %>%
  select(-Liking)


fromages_long <- fromages %>%
  pivot_longer(cols=2:10,names_to = "Attribut",values_to = "Note_JAR") %>%
  mutate(Attribut = paste(Attribut,Note_JAR,sep="_"),.keep = "unused")%>%
  mutate(Attribut=as.factor(Attribut)) %>%
  complete(Produit,Attribut) %>%
  arrange(Produit,Attribut)

df <- fromages_long %>%
  group_by(Produit,Attribut) %>%
  summarise(Counts = n()) %>%
  ungroup()

fromages_agg <- df %>%
  pivot_wider(names_from = Attribut,values_from = Counts) %>%
  mutate_all(.funs = replace_na,replace=0)
  
ca1 <- CA(fromages_agg,quali.sup = 1)
windows(width=600,height=1000)
plot(ca1,col.quali.sup = "black",col.col = rep(brewer.pal(5,"Set2"),9),
     ggoptions = list(size=2.5))
