gym <- read.csv("gym_members.csv")
library(tidyverse)
gym2 <- gym %>%
  select(Age,Gender,Weight..kg.,Height..m.,Workout_Frequency..days.week.,Session_Duration..hours.,Workout_Type) %>%
  rename(Sexe=Gender,Poids=Weight..kg.,Taille=Height..m.,Type=Workout_Type) %>%
  mutate(Vol = Workout_Frequency..days.week.*Session_Duration..hours.,
         Sexe=as.factor(Sexe),
         Type=factor(Type,levels=c("Yoga","HIIT","Cardio","Strength"),
                     labels=c("Yoga","HIIT","Cardio","Force")),
         .keep = "unused")

ggplot(gym2) + aes(x=Vol)+
  geom_histogram(fill="skyblue",colour="grey20",bins=10)+
  theme_minimal()+
  labs(x="Heures",y="Effectif",title = "Volume hebdomadaire passée à la salle de sport")

table <- gym2 %>%
  select(Type) %>%
  table() 

gym2_sum <- gym2 %>%
  group_by(Type) %>%
  summarise(Effectifs = n()) %>%
  ungroup() %>%
  mutate(Frequence=round(100*Effectifs/nrow(gym2),2))
library(RColorBrewer)
ggplot(gym2_sum)+aes(x=Type,y=Effectifs)+
  geom_bar(stat="identity",width = 0.3,fill=brewer.pal(8,"Set2")[2])+
  theme_minimal()+
  labs(title = "Nombre d'adhérent par type de sport")+
  geom_text(aes(label=Effectifs),position = position_stack(vjust = 0.95))

ggplot(gym2_sum)+aes(x=Frequence,y="",fill=Type)+
  geom_bar(stat = "identity")+
  coord_polar("x")+
  theme_void()+
  labs(title = "Répartition des adhérents par type de sport")+
  scale_fill_brewer(palette="Set2")+
  geom_text(aes(label=paste(Frequence,"%")),position = position_stack(vjust = 0.5))

gym3 <- gym2 %>%
  mutate(Niveau = sample(x=1:4,size=nrow(gym),replace = TRUE,prob = c(0.4,0.3,0.18,0.12))) %>%
  mutate(Niveau=as.factor(Niveau))

ggplot(gym3)+aes(x=Niveau)+
  geom_bar(fill=brewer.pal(n=8,"Set2")[2])+
  theme_minimal()+
  labs(title="Nombre d'adhérents par niveau",
       y="Effectif")

table(gym3$Niveau)

n_bins <- c(1,10,30,200)

for(i in n_bins){
  p<-ggplot(gym3) + aes(x=Vol)+
    geom_histogram(colour="grey20",fill=brewer.pal(n=8,name="Set2")[3],
                   bins=i)+
    theme_minimal()+
    labs(title = "Distribution du volume horarie hebdomadaire des usagers",
         x="Volume horaire",
         y="Effectifs")
  print(p)
}

ggplot(gym3)+aes(y = Vol,x="")+
  geom_boxplot(width=0.15,outlier.colour = brewer.pal(3,"Set2")[2])+
  theme_minimal()+
  labs(x="",
       y="Volume horaire",
       title="Distribution du volume horaire hebdomadaire des usagers")

# Moyenne
mean(gym3$Vol)
# Médiane
median(gym3$Vol)
# Quartiles Q1 et Q3
quantile(gym3$Vol,probs = c(0.25,0.75))
# MAD
mad(gym3$Vol,constant = 1)
# Variance et Ecart-type
var(gym3$Vol)*972/973
sd(gym3$Vol)*sqrt(972/973)

Q1 <-quantile(gym3$Vol,probs = c(0.25,0.75))[1]
Q3 <-quantile(gym3$Vol,probs = c(0.25,0.75))[2]
Q1 -1.5*(Q3-Q1)
Q3 +1.5*(Q3-Q1)

median(gym3$Vol)-4.45*mad(gym3$Vol,constant = 1)
median(gym3$Vol)+4.45*mad(gym3$Vol,constant = 1)

mean(gym3$Vol)-3*sd(gym3$Vol)*sqrt(972/973)
mean(gym3$Vol)+3*sd(gym3$Vol)*sqrt(972/973)
