rm(list=ls())

#-- LIBRAIRIES

library(tidyverse)
library(monochromeR)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(colorblindcheck)

#-- IMPORTATION DES DONNEES

fromages <- read.csv("fromages.csv",sep=";")

fromages <- fromages %>%
  mutate(Consommateur = as.factor(Consommateur),
         Produit = as.factor(Produit))

summary(fromages)

#-- ANALYSE EXPLORATOIRE DES DONNEES

#---- Cartographie interne des préférences

carte_fromages <- fromages %>%
  pivot_wider(id_cols = Produit,names_from = Consommateur,values_from = Liking) %>%
  mutate(Produit = factor(Produit,
                          levels = c("CE","M","B","R","C1","E","C2","S"),
                          labels = c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire"))) %>%
  as.data.frame()

rownames(carte_fromages) <- as.character(carte_fromages$Produit)

carte_fromages <- carte_fromages %>%
  select(-Produit)

carte_ACP <- PCA(carte_fromages,graph=FALSE)

fviz_pca_biplot(carte_ACP,col.ind = "#db162f",col.var = "#617489",labelsize=3)+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Cartographie interne des préférences",
       subtitle = "Préférences des 72 consommateurs sur les 8 fromages.")+
  theme(plot.title=element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))

#---- Représentation des likings

# Diagramme en barres

df_likings_bar <- data.frame(Fromage = as.factor(rownames(carte_fromages)),
                             Mean_Liking=rowMeans(carte_fromages)) 


ggplot(df_likings_bar)+aes(x=Fromage,y=Mean_Liking,fill=Mean_Liking)+
  geom_bar(stat = "identity",col="grey50")+
  geom_text(aes(label = round(Mean_Liking,2)),position = position_stack(vjust = 0.9))+
  scale_fill_distiller(palette = "RdYlBu")+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Note moyenne des fromages",
       subtitle = "Moyenne calculée sur les 72 participants.",
       y="Note moyenne")+
  theme(plot.title=element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))+
  guides(fill="none")

# Boxplots

df_likings_boxplot <- fromages %>%
  select(Produit,Liking) %>%
  mutate(Produit=factor(Produit,
                        levels = c("CE","M","B","R","C1","E","C2","S"),
                        labels = c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire")))


ggplot(df_likings_boxplot) + aes(x=Produit,y=Liking,fill=Produit)+
  geom_boxplot()+
  scale_fill_brewer(palette="Set2")+
  theme_minimal(paper = "#fcf4eb")+
  labs(title = "Distribution des notes données aux fromages.",
       subtitle = "Likings donnés par les 72 participants.",
       y="Note hédonique")+
  theme(plot.title=element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))+
  guides(fill="none")

#---- Représentation des notes JAR

# Représentation par produit
summary(fromages)

distrib_JAR <- fromages %>%
  mutate_at(4:12,.funs = as.factor) %>%
  reframe(across(.cols=4:11,.fns = table),.by = Produit) %>%
  group_by(Produit) %>%
  mutate(Note_JAR = as.factor(1:5))%>%
  ungroup() %>%
  pivot_longer(2:9,names_to = "Attribut",values_to = "Effectifs") %>%
  mutate(Attribut=as.factor(Attribut),
         Effectifs=as.numeric(Effectifs),
         Produit = factor(Produit,
                          levels = c("CE","M","B","R","C1","E","C2","S"),
                          labels = c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire")))

ggplot(distrib_JAR) + aes(x=Attribut,y=Effectifs,fill=Note_JAR)+
  geom_bar(stat="identity",colour="grey50")+
  scale_fill_brewer(palette="RdYlBu",direction = -1)+
  facet_wrap(~Produit)+
  theme_bw(paper = "#fcf4eb")+
  labs(title = "Résumé des produits par attribut")+
  theme(plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle=45,vjust = 0.7))+
  guides(fill=guide_legend(title = "Note JAR"))

# Représentation par attribut

ggplot(distrib_JAR) + aes(x=Produit,y=Effectifs,fill=Note_JAR)+
  geom_bar(stat="identity",colour="grey50")+
  scale_fill_brewer(palette="RdYlBu",direction = -1)+
  facet_wrap(~Attribut)+
  theme_bw(paper = "#fcf4eb")+
  labs(title = "Résumé des attributs par produit")+
  theme(plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle=45,vjust = 0.7))+
  guides(fill=guide_legend(title = "Note JAR"))

#---- ACP sur les individus (8x72=576)

fromages_ACP <- fromages %>%
  select(-Consommateur) %>%
  mutate_if(.predicate = is.character,.funs = as.numeric) %>%
  mutate_at(.vars=1,.funs = as.character)

# Création du produit ideal

fromages_ACP[577,] <- c("Idéal",9,rep(3,9))

fromages_ACP <- fromages_ACP %>%
  mutate_at(.vars=2:11,.funs = as.numeric) %>%
  mutate(Produit=factor(Produit,
                          levels = c("CE","M","B","R","C1","E","C2","S","Idéal"),
                          labels = c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire","Idéal")))

PCA_fromages <- PCA(X = fromages_ACP,quali.sup = 1,quanti.sup = 2,graph=FALSE)

plot.PCA(PCA_fromages,choix="var",col.var = "#617489")+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Cercle des corrélations")+
  theme(plot.title = element_text(face="bold",size=rel(1.4)))


couleurs <- colorRampPalette(brewer.pal(8,"Set1"))(9)

plotellipses(PCA_fromages,invisible="ind",col.hab = couleurs)+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Représentation des produits",
       subtitle = "Avec les ellipses de confiance au niveau 95%.")+
  theme(plot.title = element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))

# Encodage en dummy variables

fromages_dummy <- matrix(0,ncol=2*(ncol(fromages)-3)+1,nrow = nrow(fromages)) %>%
  as.data.frame()
for(i in 1:(ncol(fromages)-3)){
  colnames(fromages_dummy)[2*i] <- paste0(colnames(fromages)[3+i],"-")
  colnames(fromages_dummy)[2*i+1] <- paste0(colnames(fromages)[3+i],"+")
}

colnames(fromages_dummy)[1] <- "Produit"

fromages_dummy$Produit <- fromages$Produit

for(j in 2:ncol(fromages_dummy)){
  if (j%%2 ==0){
    fromages_dummy[,j] <- -2*(fromages[,3+j%/%2]==1) -1*(fromages[,3+j%/%2]==2)
  }else{
    fromages_dummy[,j] <- 1*(fromages[,3+j%/%2]==4) +2*(fromages[,3+j%/%2]==5)
  }
}

Dummy_ACP <- PCA(X = fromages_dummy,quali.sup = 1,graph = FALSE)

# Cercle des corrélations
plot.PCA(Dummy_ACP,choix="var",col.var = rep(c("#617489","#db162f"),9))+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Cercle des corrélations",
       subtitle = "Après encodage en dummy variables.")+
  theme(plot.title = element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))

# Représentation des individus

plotellipses(Dummy_ACP,invisible="ind",col.hab = brewer.pal(8,"Dark2"))+
  theme_minimal(paper="#fcf4eb")+
  labs(title = "Représentation des produits",
       subtitle = "Avec les ellipses de confiance au niveau 95%.")+
  theme(plot.title = element_text(face="bold",size=rel(1.4)),
        plot.subtitle = element_text(face="italic",size=rel(1)))

#---- ACM

fromages_MCA <- fromages %>%
  select(-Liking,-Consommateur) %>%
  mutate_at(.vars=2:10,.funs=as.factor)%>%
  mutate(Produit = factor(Produit,
                          levels = c("CE","M","B","R","C1","E","C2","S","Idéal"),
                          labels = c("Comté","Morbier","Beaufort","Reblochon","Cantal1","Emmental","Cantal2","St-Nectaire","Idéal")))

result_MCA <- MCA(fromages_MCA,quali.sup = 1,graph=FALSE)  

couleurs_mca_var <- brewer.pal(5,"Set2")


plot.MCA(result_MCA,invisible = "ind",col.var= rep(couleurs_mca_var,8),
         col.quali.sup = "black",ggoptions = list(size=3))+
  theme_minimal(paper="#fcf4eb")+
  labs(title="Analyse des correspondances multiples")+
  theme(plot.title = element_text(face="bold",size=rel(1.4)))

#-- ANALYSE DES PENALITES

# Regroupement en 3 catégories

cat3 <- function(x){
  return(as.factor(1*(x==1 | x==2) +2*(x==3)+ 3*(x==4 | x==5)))
}

fromages_3cat <- fromages %>%
  select(-Consommateur) %>%
  mutate(Produit = factor(Produit,
                          levels = c("CE","M","B","R","C1","E","C2","S","Idéal"),
                          labels = c("Comté","Morbier","Beaufort","Reblochon",
                                     "Cantal1","Emmental","Cantal2","St-Nectaire","Idéal")))%>%
  mutate_at(.vars=3:11,.funs=cat3) %>%
  filter(Produit=="Beaufort") %>%
  select(-Produit) %>%
  pivot_longer(cols = 2:10,names_to = "Attribut",values_to = "Note_JAR")

fromages_mean_drop <- fromages_3cat %>%
  group_by(Attribut,Note_JAR) %>%
  mutate(Mean=mean(Liking)) %>%
  ungroup()%>%
  select(-Liking) %>%
  unique() %>%
  arrange(Attribut,Note_JAR)%>%
  group_by(Attribut) %>%
  mutate(mean_drop=Mean[2]-Mean)%>%
  ungroup() %>%
  filter(Note_JAR!=2)%>%
  unique() %>%
  select(-Mean) %>%
  mutate(Attribut=paste(Attribut,Note_JAR,sep="_")) %>%
  select(-Note_JAR)

ggplot(fromages_mean_drop)+aes(x=Attribut,y=mean_drop)+
  geom_bar(stat = "identity",col="grey50",fill="#617489")+
  theme_minimal(paper="#fcf4eb")
         