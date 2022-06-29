# Partie 2 : Analyse 

# Chargement de la table de données 
## Appel de la bibliothèque nécessaire pour ouvrir des ficher du format dta
library(haven) 
library(dplyr)

## Définissons le chemin d'accès à la tables de données
path <- "C:\\Users\\HP ProBook\\Documents\\Mes Cours ISEP2\\R\\Projet\\Base de données"

## importation des données  
my_data <- read_dta(paste(path,"Base_projetf.dta",sep="\\")) # tables de donnée apurée
head(my_data)

## petit résumé de la table
library(tidyverse)
glimpse(my_data)
df <- data.frame(my_data) # Convertir en data frame
## recodons les variables
library(labelled)
variables <- c("Sexe", "Situation_Matrimoniale", "Nationalite"
               ,"Ne_dans_sa_localite_de_residence","Localite_d_origine_des_migrants", "Principale_cause_de_migration", "Peut_lire_français"              
              ,"Peut_lire_la_langue_locale","Peut_lire_une_autre_langue","Fait_une_ecole_formelles","Raisons_non_frequentation_EF"    
              ,"Fait_une_ecole_non_formelle","Gerant_de_l_ecole" ,"Resultat_2019_2020","Raisons_d_abandon_de_l_ecole","Diplome_le_plus_eleve"           
              ,"Insuffisance_livres_fornitures", "Insuffisance_tablesb_equipements","Absenteisme_enseignants_greve"   
              ,"Enseignement_pas_satisfaisant", "Effectifs_plethoriques","Insuffisance_d_enseignants"      
             ,"Manque_de_toilettes", "Frequence_des_cotisations", "Salle_de_classe_en_mauvais_etat" )
df[variables] <- to_factor(df[variables])


# Caractéristiques sociodémographiques

## sexe
# fréquences
prop.table(table(df["Sexe"]))
# graphique en secteur
library(plotrix)
pie3D(table(df["Sexe"]),labels = c("Homme","Femme") ,col = c("aquamarine","pink"), main = "Sexe")

## situation Matrimoniale
### Fréquences
prop.table(table(df["Situation_Matrimoniale"]))
### diagramme en baton 
barplot(prop.table(table(df["Situation_Matrimoniale"])),xlab="Situation matrimoniale",ylab="Effectif")

## Age 
### statististiques descriptives 
summary(df["Age"])

### histogramme
library(ggplot2)
#### histogramme classique
ggplot(df, aes(x=Age)) + 
  geom_histogram(color="black", fill="green") +
  theme(legend.position="top")+
  scale_x_continuous(breaks=seq(0,110,10), limits=c(0,110))

#### Histogramme par sexe
ggplot(df, aes(x=Age, color=Sexe, fill=Sexe)) +
  geom_histogram(position="dodge") +
  theme(legend.position="top")+
  scale_color_brewer(palette="Paired") + 
  theme_classic()+
  scale_x_continuous(breaks=seq(0,110,10), limits=c(0,110))


## Age au premier mariage
### statististiques descriptives 
summary(df["Age_au_premier_mariage"])

## description de age moyen de marriage par sexe
fem <- df[which(df["Sexe"] == "Féminin"),]
hom <- df[which(df["Sexe"] == "Masculin"),]
summary(fem["Age_au_premier_mariage"])
summary(hom["Age_au_premier_mariage"])

### histogramme
#### histogramme classique
ggplot(df, aes(x=Age_au_premier_mariage)) + 
  geom_histogram(color="black", fill="green") +
  theme(legend.position="top")+
  scale_x_continuous(breaks=seq(0,70,10), limits=c(0,70))

#### Histogramme par sexe
ggplot(df, aes(x=Age_au_premier_mariage, color=Sexe, fill=Sexe)) +
  geom_histogram(position="dodge") +
  theme(legend.position="top")+
  scale_color_brewer(palette="Paired") + 
  theme_classic()+
  scale_x_continuous(breaks=seq(0,70,10), limits=c(0,70))

## origine des migrants
### Fréquences
prop.table(table(df["Localite_d_origine_des_migrants"]))
### diagramme en baton 
barplot(prop.table(table(df["Localite_d_origine_des_migrants"]))
        ,xlab="Localite d'origine des migrants",ylab="Effectif",col = "red")

## Principale cause de migration
### Fréquences
prop.table(table(df["Principale_cause_de_migration"]))
### diagramme en baton 
barplot(prop.table(table(df["Principale_cause_de_migration"]))
        ,xlab="Principale cause de migration",ylab="Effectif",col = "purple")


## Analyse univariée
### statistiques descriptives pour les variables quantitatives

carac_quant <- c("Age_au_premier_mariage","Age")
summary(df[carac_quant])

carac_qual <- c("Sexe","Situation_Matrimoniale","Nationalite",
                "Ne_dans_sa_localite_de_residence","Localite_d_origine_des_migrants","Principale_cause_de_migration")
carac_quant <- c("Age_au_premier_mariage","Age")

# Education 

## Accècibilité aux écoles
acces <- c("Fait_une_ecole_formelles","Fait_une_ecole_non_formelle")
for (x in acces) {
  ### Fréquences
  print(prop.table(table(df[x])))
  pie3D(table(df[x]),labels = c("Oui","Non") ,col = c("aquamarine","pink"), main = x)
}

## Raisons de non Fréquentation 

### Fréquences
prop.table(table(df["Raisons_non_frequentation_EF"]))
### diagramme en baton 
barplot(prop.table(table(df["Raisons_non_frequentation_EF"]))
        ,xlab="Raisons de non frequentation d'une école formelle",ylab="Effectif",col = "turquoise")

## Résultat pour l'année scolaire 2019-2020

prop.table(table(df["Resultat_2019_2020"]))
### diagramme en baton 
barplot(prop.table(table(df["Resultat_2019_2020"]))
        ,xlab="Résultat pour l'année scolaire 2019-2020",ylab="Effectif",col = "blue")

## Raison d'abandon des classe

prop.table(table(df["Raisons_d_abandon_de_l_ecole"]))
### diagramme en baton 
barplot(prop.table(table(df["Raisons_d_abandon_de_l_ecole"]))
        ,xlab="Raison d'abandon des classe pour l'année scolaire 2019-2020",ylab="Effectif",col = "rosybrown1")


## Principales difficultés rencontrées dans le cadre de la scolarité
difficulte <- c("Insuffisance_livres_fornitures", "Insuffisance_tablesb_equipements","Absenteisme_enseignants_greve"   
                ,"Enseignement_pas_satisfaisant", "Effectifs_plethoriques","Insuffisance_d_enseignants"      
                ,"Manque_de_toilettes", "Frequence_des_cotisations", "Salle_de_classe_en_mauvais_etat")

for(x in difficulte){
  print(prop.table(table(df[x])))
  pie3D(table(df[x]),labels = c("Oui","Non") ,col = c("aquamarine","pink"), main = x)
}



# Caractéristiques sociodémographiques et éducation 

# Abordons le niveau d'alphabétisation selon le sexe 

# croisons sexe et peut_lire_français 

tab5 <- table(df$Peut_lire_français,df$Sexe)
print(tab5)

library("questionr")
print(cprop(tab5))

# Croisons sexe et peut ?crire en fran?ais 

tab6 <- table(df$Peut_ecrire_français,df$Sexe)
print(cprop(tab6))

# croisons sexe et comprendre un texte en fran?ais 

ggplot(df) +
  aes(x = comprendre_un_texte_en_français, y = Sexe) +
  geom_count(colour="hotpink", alpha=.2) +
  xlab("Peut comprendre un texte en français") +
  ylab("Sexe")+labs(size="effectifs")

# Croisons sexe et fait_une_ecole_formelle

tab11 <- table(df$Sexe,df$Fait_une_ecole_formelles)
print(tab11)

#Representation 
library(viridis)
mosaicplot(tab11,main = "Frequentation de l'école formelle en fonction du sexe", col = viridis(2) )

chisq.test(tab11) #test d KHI2

#croisons sexe et diplome le plus ?lev?

tab12 <- table(df$Diplome_le_plus_eleve,df$Sexe)
print(tab12)

ggplot(df) +
  aes(x = Sexe, y = Diplome_le_plus_eleve) +
  geom_count(colour="green", alpha=.2) +
  xlab("Sexe") +
  ylab("Diplome le plus élévé")+labs(size="effectifs")






# Création d'indicateur : Indicateur de maîtrise d'une langue (IML)

## Etape 1 indice simple = proportion de oui pour les variables langue
langue <- c("Peut_lire_français" , "Peut_lire_la_langue_locale","Peut_lire_une_autre_langue",
            "Peut_ecrire_français", "Peut_ecrire_la_langue_locale","Peut_ecrire_une_autre_langue",
            "comprendre_un_texte_en_français","comprendre_texte_langue_locale", "comprendre_texte_en_autre_langue" )
indices_s <- c()
for (x in langue) {
  y <- data.frame(prop.table(table(df[x])))
  indices_s <- c( y[which(y[x] == "Oui" | y[x] == 1 ),2],indices_s)
}
Indice <- data.frame(indices_s)
row.names(Indice) <- langue

### visualisation avec ggplot2
ggplot(data=Indice, aes(x= row.names(Indice),y=indices_s)) +
  geom_bar(stat = "identity",fill="cyan")+
  geom_text(aes(label= round(indices_s*100)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()

## Etape 2 : IML pour chacune des langues 
## IML = moyenne géométrique (indices simples de l'étape 1)
### Regrouper les variables par langue
Francais <- c("Peut_lire_français","Peut_ecrire_français","comprendre_un_texte_en_français")
Langue_locale <- c("Peut_lire_la_langue_locale","Peut_ecrire_la_langue_locale","comprendre_texte_langue_locale")
Autre_langues <- c("Peut_lire_une_autre_langue","Peut_ecrire_une_autre_langue","comprendre_texte_en_autre_langue")

### calcul de l'IML pour chaque groupe de langue
library(forestmangr)
IML <- data.frame (c(pow(Indice[Francais[1],]* Indice[Francais[2],]*Indice[Francais[3],],1/length(Francais)),
         pow(Indice[Langue_locale[1],]*Indice[Langue_locale[2],]*Indice[Langue_locale[3],],1/length(Langue_locale)),
         pow(Indice[Autre_langues[1],]*Indice[Autre_langues[2],]*Indice[Autre_langues[3],],1/length(Autre_langues))))

rownames(IML) <- c("Français", "Langue locale", "Autre langues") 
colnames(IML) <- "Indice_de_maîtrise_de_langue"

### visualisation avec ggplot2
ggplot(data=IML, aes(x= row.names(IML),y=Indice_de_maîtrise_de_langue)) +
  geom_bar(stat = "identity",fill="cyan")+
  geom_text(aes(label= round(IML[,1],2)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()


## Etape 3 : Indice globale de maîtrise de langue IGML
## IGML = moyenne géométrique (IML de l'étape 2)
IGML <- pow(IML[1,]*IML[2,]*IML[3,], 1/length(IML))
print(IGML)

