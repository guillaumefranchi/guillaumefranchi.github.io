library(tidyverse)
library(RColorBrewer)

df <- read.csv("gym_members.csv")

modif_sport <- function(type){
  if(type=="Force"){
    return(sample(c("Masculin","Feminin"),1,prob = c(0.7,0.3)))
  } else if(type=="Yoga"){
    return(sample(c("Masculin","Feminin"),1,prob=c(0.15,0.85)))
  } else if(type=="HIIT"){
    return(sample(c("Masculin","Feminin"),1,prob=c(0.45,0.55)))
  } else{
    return(sample(c("Masculin","Feminin"),1,prob=c(0.5,0.5)))
  }
}

df <- df %>%
  mutate("Volume_hebdo(h)" = Workout_Frequency..days.week.*Session_Duration..hours.) %>%
  rename(Sexe = Gender,"Poids(kg)"= Weight..kg.,
         "Taille(m)"=Height..m.,
         BPM_max=Max_BPM,
         BPM_moy=Avg_BPM,
         BPM_repos=Resting_BPM,
         "Duree_moy_session(h)"=Session_Duration..hours.,
         Calories=Calories_Burned,
         Type=Workout_Type,
         Pourcentage_graisses=Fat_Percentage,
         Conso_eau=Water_Intake..liters.,
         "Frequence_entrainement(Jour/Semaine)"=Workout_Frequency..days.week.,
         IMC=BMI) %>%
  select(-Experience_Level) %>%
  select(Age,Sexe,Type,"Taille(m)","Poids(kg)","Volume_hebdo(h)") %>%
  mutate(Sexe=factor(Sexe,levels = c("Male","Female"),labels=c("Masculin","Feminin"))) %>%
  mutate(Type=as.factor(Type)) %>%
  mutate(Type = factor(Type,levels=c("Cardio","HIIT","Strength","Yoga"),
                       labels=c("Cardio","HIIT","Force","Yoga"))) 

for(i in 1:nrow(df)){
  df$Sexe[i] <- modif_sport(df$Type[i])
}
head(df)

library(xtable)
xtable(head(df))

ggplot(df) + aes(x="",y=`Volume_hebdo(h)`)+
  geom_boxplot(width=0.1)+
  theme_minimal()+
  labs(title = "Volume hebdomadaire passée à la salle de sport",
       x="",
       y="Heures")

ggplot(df) + aes(x=`Volume_hebdo(h)`)+
  geom_histogram(bins = 10,color="grey20",fill="skyblue",alpha=0.5)+
  theme_minimal()+
  labs(title = "Volume hebdomadaire passée à la salle de sport",
       x="",
       y="Heures")
summary(df)

df_table <- df %>% select(Sexe,Type) %>%
  table()

xtable(df_table)

df_sum <- df %>% group_by(Sexe,Type) %>%
  summarise(Number = n()) %>%
  group_by(Type) %>%
  mutate(Prop = Number/sum(Number)) %>%
  ungroup()

ggplot(df_sum) + aes(x=Type,y=Prop,fill=Sexe) +
  geom_bar(stat = "identity")+
  scale_fill_discrete(type = brewer.pal(n=3,name="Set2"))+
  theme_minimal()+
  labs(title = "Répartition par type de sport",
       y="Pourcentage")+
  geom_text(aes(label=paste(round(100*Prop,0),"%")),position = position_stack(vjust = 0.5))
