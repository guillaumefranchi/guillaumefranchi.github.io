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
