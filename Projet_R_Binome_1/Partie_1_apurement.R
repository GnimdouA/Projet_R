# Partie 1 : APUREMENT

# Chargement des bases 
## Appel de la bibliothèque nécessaire pour ouvrir des ficher du format dta
library(haven) 
library(dplyr)

## Définissons le chemin d'accès aux tables de données
path <- "C:\\Users\\HP ProBook\\Documents\\Mes Cours ISEP2\\R\\Projet\\Base de données"

## importation des bases des sections 1 & 2 
section_1 <- read_dta(paste(path,"s01_me_SEN_2021.dta",sep="\\")) # tables de la section 1
section_2 <-  read_dta(paste(path,"s02_me_SEN_2021.dta",sep="\\"))# tables de la section 2

# choisissons nos variables d'intérêt, tout en les renommant 
## section 1: caractéristiques
carac_men <- section_1[1:6] # variables d'identification tel que la clé du ménage
names(carac_men)[6] <- "member_id"
carac_men[c("Sexe","Situation_Matrimoniale","Age_au_premier_mariage","Nationalite","Ne_dans_sa_localite_de_residence",
            "Localite_d_origine_des_migrants","Principale_cause_de_migration"
            ,"Annee_de_naissance")] <- section_1[c("s01q01","s01q07","s01q10","s01q15","s01q17", "s01q19", "s01q21", "s01q03c")]


## section 2: Education 
edu <- section_2[1:6] # variables d'identification tel que la clé du ménage
names(edu)[6] <- "member_id"
edu[c("Peut_lire_français", "Peut_lire_la_langue_locale", "Peut_lire_une_autre_langue", "Peut_ecrire_français", "Peut_ecrire_la_langue_locale", "Peut_ecrire_une_autre_langue")] <- section_2[c("s02q01__1","s02q01__2", "s02q01__3", "s02q02__1", "s02q02__2", "s02q02__3")]
edu[c("Peut_comprendre_un_texte_en_français", "Peut_comprendre_un_texte_en_langue_locale", "Peut_comprendre_un_texte_en_autre_langue")] <- section_2[c("s02q02a__1","s02q02a__2", "s02q02a__3")]
edu[c("Fait_une_ecole_formelles","Raisons_de_non_frequentation_d_ecole_formelle","Fait_une_ecole_non_formelle","Age_d_entre_a_l_ecole","Gerant_de_l_ecole")] <- section_2[c("s02q03","s02q04","s02q05","s02q07","s02q09")]
edu[c("Fermeture_de_l_ecole","Reste_en_contact_avec_l_ecole","Resultat_2019_2020","Raisons_d_abandon_de_l_ecole")] <- section_2[c("s02q09a","s02q09b","s02q10","s02q11")]
edu[c("Diplome_le_plus_eleve","Reste_en_contact_avec_l_ecole_par_SMS","Reste_en_contact_avec_l_ecole_par_applications_mobiles","Reste_en_contact_avec_l_ecole_par_Email","Reste_en_contact_avec_l_ecole_par_courier","Reste_en_contact_avec_l_ecole_Telephone")] <- section_2[c("s02q33","s02q09c__1","s02q09c__2","s02q09c__3","s02q09c__4","s02q09c__5")]
edu[c("Insuffisance_de_livres_et_ou_de_fornitures","Insuffisance_de_tables_bancs_et_d_equipements","Absenteisme_des_enseignants_greve","Enseignement_pas_satisfaisant","Effectifs_plethoriques","Insuffisance_d_enseignants","Manque_de_toilettes","Frequence_des_cotisations","Salle_de_classe_en_mauvais_etat")] <- section_2[c("s02q18__1","s02q18__2","s02q18__3","s02q18__4","s02q18__5","s02q18__6","s02q18__7","s02q18__8","s02q18__9")]

# fusion des deux sous tables pour avoir notre table de données d'étude
## création de la clé de fusion 
carac_men <- mutate(carac_men, key = paste(interview__key,interview__id,id_menage,member_id))
edu <- mutate(edu, key = paste(interview__key,interview__id,id_menage,member_id))
my_data <- merge(carac_men, edu, by = "key")

# faisons une première sauvegarde de cette base
write.csv(my_data, paste(path,"Base_projet.csv", sep="\\"))
## exportons à présent nos données d'études sous format dta.
# write_dta(my_data, paste(path,"Base_projet.dta", sep="\\"))
dim(my_data)

# Supprimons les doublons et les variables en double qui ne nous interressent pas
# c'est à dire les identifiants des ménages
my_data1 <- distinct(my_data) # suppression des doublons
dim(my_data1)
my_data2 <- my_data1[-c(2:7,16:21)] # suppression des colones ininteressantes

# calculons l'âge des individus
my_data2 <- mutate(my_data2, Age = 2022 - Annee_de_naissance)

# excluons les individus de moins de 3 ans car ils ne nous intérressent pas
my_data3 <- subset(my_data2, Age > 3) 
dim(my_data3)

# sauvegardons la base ainsi ontenue dans un ficher csv.
write.csv(my_data3, paste(path,"Base_projet2.csv", sep="\\"))

# Recodons les valeurs des variables 
# Métode 1 (fastidieuse)
## sexe: 1 = masculin & 2 = féminin

my_data4 <- mutate(my_data3, Sexe = if_else(Sexe == 1, "Homme", "Femme", missing = NULL))

# Méthode 2 : transformer en type factor moins fastidieux
# Recodons les valeurs des variables grâce à la fonction to_factor de la librairie "labelled"
library(labelled) 
str(my_data3) # pour détecter les variables catégorielles à recoder
# variables à recoder
variables = c("Sexe", "Situation_Matrimoniale", "Nationalite","Ne_dans_sa_localite_de_residence"
              ,"Localite_d_origine_des_migrants","Principale_cause_de_migration","Fait_une_ecole_formelles"
              ,"Raisons_de_non_frequentation_d_ecole_formelle","Fait_une_ecole_non_formelle","Gerant_de_l_ecole",
              'Fermeture_de_l_ecole',"Reste_en_contact_avec_l_ecole","Resultat_2019_2020","Raisons_d_abandon_de_l_ecole"
              ,"Diplome_le_plus_eleve","Insuffisance_de_livres_et_ou_de_fornitures","Insuffisance_de_tables_bancs_et_d_equipements"
              ,'Absenteisme_des_enseignants_greve',"Enseignement_pas_satisfaisant","Effectifs_plethoriques","Insuffisance_d_enseignants",
              "Manque_de_toilettes",'Frequence_des_cotisations',"Salle_de_classe_en_mauvais_etat",
              "Peut_lire_français","Peut_lire_la_langue_locale","Peut_lire_une_autre_langue")

my_data4[variables] <- to_factor(my_data3[variables]) # recodage

# Gestion des valeurs manquantes 

## visualisation générale des valeurs manquantes 

library(visdat)
vis_miss(my_data4)

## suppression des colones n'ayant pratiquement que des NA : volet covid

covid <- c("Fermeture_de_l_ecole","Reste_en_contact_avec_l_ecole","Reste_en_contact_avec_l_ecole_par_SMS",
           "Reste_en_contact_avec_l_ecole_par_applications_mobiles","Reste_en_contact_avec_l_ecole_par_Email",
           "Reste_en_contact_avec_l_ecole_par_courier","Reste_en_contact_avec_l_ecole_Telephone")
my_data4 <- select(my_data4,- covid)

## Variables de maîtrise de la langue
langue <- c("Peut_lire_français","Peut_lire_la_langue_locale","Peut_lire_une_autre_langue",
            "Peut_ecrire_français", "Peut_ecrire_la_langue_locale", "Peut_ecrire_une_autre_langue",
            "Peut_comprendre_un_texte_en_français","Peut_comprendre_un_texte_en_langue_locale",
            "Peut_comprendre_un_texte_en_autre_langue")


### gestion des valeurs manquantes
### cherchons le mode et detection des na et remplacement par le mode
for (x in langue){
  y <- sort(table(my_data4[x]))# on range les modalité en fonction de leurs 
  # fréquence dans l'ordre croissant. Ainsi le mode est le premier élément de y
  print(y) # pour voir les différentes modalités et le mode
  y <- data.frame(y)
  na <- which(is.na(my_data4[x])) # detection des valeurs manquantes
  my_data4[na,x] <- y[2,x] # remplacement des na
}



## variables de fréquentation d'une école ou non et raisons de non fréquentation 

### Gestion des variables manquantes pour variables de fréquentation d'une école ou non et raisons de non fréquentation 
### les resultat au cours de l'années 2019-2020, les 
formation <- c("Fait_une_ecole_formelles","Raisons_de_non_frequentation_d_ecole_formelle","Fait_une_ecole_non_formelle"
               ,"Diplome_le_plus_eleve","Resultat_2019_2020", "Raisons_d_abandon_de_l_ecole","Gerant_de_l_ecole")
for (x in formation){
  y <- sort(table(my_data4[x]))# on range les modalité en fonction de leurs 
  # fréquence dans l'ordre croissant. Ainsi le mode est le premier élément de y
  y <- data.frame(y)
  na <- which(is.na(my_data4[x])) # detection des valeurs manquantes
  my_data4[na,x] <- y[2,x] # remplacement des na
}


## gestion des NA pour l'âge d'entrée à l'école

x <- which(is.na(my_data4["Age_d_entre_a_l_ecole"])) # detection des Na 
my_data4[x,"Age_d_entre_a_l_ecole"] <- median(my_data4[which(is.na(my_data4["Age_d_entre_a_l_ecole"])== FALSE),"Age_d_entre_a_l_ecole"]) # imputation  
# par la médiane


## gestion des variables relatives aux difficultés rencontrés à l'école

library(VIM)
dificulties <- c("Insuffisance_de_livres_et_ou_de_fornitures","Insuffisance_de_tables_bancs_et_d_equipements"
                 ,"Absenteisme_des_enseignants_greve", "Enseignement_pas_satisfaisant", "Effectifs_plethoriques",
                 "Insuffisance_d_enseignants", "Manque_de_toilettes", "Frequence_des_cotisations", "Salle_de_classe_en_mauvais_etat")
## cherchons toutes les lignes qui présentes des NA pour ces variables 
dificulties_na <- c() # vecteur qui recueillera les index de ces lignes
for (x in dificulties){
  x <- which(is.na(my_data4[x])) # detection des valeur maquantes
  dificulties_na <- c(dificulties_na, x) # ajout des index aux autres précédement déterminés 
}
y = data_frame(dificulties_na)
dificulties_na <- dificulties_na[!duplicated(y)] # supprimer les doublons car on ne veux pas que les index se repètent
data_s = my_data4[10:36] # variables portant sur l'éducation
my_data4[dificulties_na,10:36] <- kNN(data_s[dificulties_na,]) # imputation

## Variable Sexe
x <- which(is.na(my_data4["Sexe"])) # detection des valeur maquantes
my_data4[x,"Sexe"] <- "Féminin"  # imputation 

## variable Nationalité
x <- which(is.na(my_data4["Nationalite"])) # detection des valeur maquantes
my_data4[x,"Nationalite"] <- "Sénégal"  # imputation 


## Autres variables de caractéristiques sociodémographiques
demographie <- c("Situation_Matrimoniale", "Age_au_premier_mariage",
                 "Ne_dans_sa_localite_de_residence","Localite_d_origine_des_migrants","Principale_cause_de_migration" )

demographie_na <- c() # vecteur qui recueillera les index de ces lignes
for (x in demographie){
  x <- which(is.na(my_data4[x])) # detection des valeur maquantes
  demographie_na <- c(demographie_na, x) # ajout des index aux autres précédement déterminés 
}
y = data_frame(demographie_na)
demographie_na <- demographie_na[!duplicated(y)] # supprimer les doublons car on ne veux pas que les index se repètent

library(mice) 
my_data4[demographie_na,] <-  complete(mice(my_data4[demographie_na,],meth = "cart"),1) # imputation

# visualisation des na
vis_miss(my_data4)
basef <- my_data4

# Gestion des valeurs aberrantes

## Age au premier mariage
## les célibataires ne sont pas consernés

y <- which(basef["Situation_Matrimoniale"] == "Célibataire" | basef["Age_au_premier_mariage"] == 9999)
basef[y,"Age_au_premier_mariage"] <- NA

## Variables sur la migration
## conserver les données d'origines pour faire des analyses plus pertinantes
migr <- c("Localite_d_origine_des_migrants","Principale_cause_de_migration")
basef[migr] <- to_factor(my_data3[migr])


## Variables de maîtrise de la langue
### Gestion valeurs aberrantes
### jettons un coup d'oeil sur les modalités de nos variables
### pour voir si tout va bien

for(x in langue){
  print(table(basef[x]))
}
### rendre les modalités conformes au questionnaire

langue2 <- c("Peut_ecrire_français", "Peut_ecrire_la_langue_locale", "Peut_ecrire_une_autre_langue",
             "Peut_comprendre_un_texte_en_français","Peut_comprendre_un_texte_en_langue_locale",
             "Peut_comprendre_un_texte_en_autre_langue")
for(x in langue2){
  y <- which(basef[x] == 0)
  basef[y,x] <- 2
}


## Age d'entré à l'école
## ceux qui n'ont fait aucune formation ne doivent renseigner cette partie
y <- which(basef["Fait_une_ecole_formelles"] == "Non" & basef["Fait_une_ecole_non_formelle"] == "Non")
basef[y,"Age_d_entre_a_l_ecole"] <- NA

### detection des outliers grâce au boxplot
library(ggplot2)
ggplot(my_data4, aes( y=Age_d_entre_a_l_ecole,fill=Age_d_entre_a_l_ecole)) +
  geom_boxplot()+
  ggtitle("Age d'entré à l'école") 

### recupérer les valeurs aberrantes
outlier <- boxplot.stats(basef$Age_d_entre_a_l_ecole)$out
### recupérer les positions des outliers
outlier_ind <- which(basef$Age_d_entre_a_l_ecole %in% c(outlier))
### remplacer les outliers par la moyenne
basef[outlier_ind,"Age_d_entre_a_l_ecole"] <- mean(basef$Age_d_entre_a_l_ecole) 

## on fait pareil pour l'age
### detection des outliers grâce au boxplot
ggplot(my_data4, aes( y=Age,fill=Age)) +
  geom_boxplot()+
  ggtitle("Age") 

### recupérer les valeurs aberrantes
outlier <- boxplot.stats(basef$Age)$out
### recupérer les positions des outliers
outlier_ind <- which(basef$Age %in% c(outlier))
### remplacer les outliers par la médiane
basef[outlier_ind,"Age"] <- median(basef$Age) 


## Variables de fréquentation ou non d'une école et des raison de non fréquentation
## vérification de réponses : faire en sorte que la réponse à la question 
## "quels sont les raisons de non fréquentation d'une école formelle ?" 
## pour un individu qui en fréquente déjà une soit "Fait une école formelle"

x <- which(base["Fait_une_ecole_formelles"] == "Oui") # detecter les 
# observations où les individus font des études formules
base[x,"Raisons_de_non_frequentation_d_ecole_formelle"] <- NA # pour chaque individu
# (ligne) identifié précédemment, il faut imposer qu'il ne donne pas de raison
# de non fréquentation d'école formelle

## Raison d'abandon des classes et de non fréquentation d'école formelle
## cherchons les hommes qui ont pour raison d'abandon des classe ou de non fréquentation "c'est une fille" et 
## remplaçons la raison par NA
### raison de non fréquentation
x1 <- which(basef["Sexe"] == "Masculin" & basef["Raisons_de_non_frequentation_d_ecole_formelle"] == "C'est une fille")
basef[x1,"Raisons_de_non_frequentation_d_ecole_formelle"] <- NA
### raisons d'abandon; on gèrera aussi les cas des homme ayant pour raison "grossesse"
x <- which(basef["Sexe"] == "Masculin" & basef["Raisons_d_abandon_de_l_ecole"] == "C'est une fille")
x2 <- which(basef["Sexe"] == "Masculin" & basef["Raisons_d_abandon_de_l_ecole"] == "Grossesse")
basef[x,"Raisons_d_abandon_de_l_ecole"] <- NA
basef[x2,"Raisons_d_abandon_de_l_ecole"] <- NA

## Gérant de l'école: la réponse de tous ceux qui ne font aucune école doit être NA 
## pareil pour les difficultés rencontrés dans le cadre de sa scolarité
var <- c("Gerant_de_l_ecole",dificulties)
for(x in var){
  y <- which(basef["Fait_une_ecole_formelles"] == "Non" & basef["Fait_une_ecole_non_formelle"] == "Non")
  basef[y,x] <- NA
}

# visualisation des NA sur nos données finales
base_finale <- basef
vis_miss(base_finale)

# sauvegardons la base ainsi ontenue dans un ficher dta et un autre csv.
# mais avant il faut changer les noms qui sont trop longs pour pouvoir les exporter en format .dta
name <- c("Peut_comprendre_un_texte_en_français", "Peut_comprendre_un_texte_en_langue_locale",
          "Peut_comprendre_un_texte_en_autre_langue", "Raisons_de_non_frequentation_d_ecole_formelle"
          ,"Insuffisance_de_livres_et_ou_de_fornitures","Insuffisance_de_tables_bancs_et_d_equipements"
          ,"Absenteisme_des_enseignants_greve")
new_names <- c("comprendre_un_texte_en_français", "comprendre_texte_langue_locale",
               "comprendre_texte_en_autre_langue", "Raisons_non_frequentation_EF"
               ,"Insuffisance_livres_fornitures","Insuffisance_tablesb_equipements"
               ,"Absenteisme_enseignants_greve")

for(i in 1:length(name)){
  names(base_finale)[names(base_finale) == name[i]] = new_names[i]
}

write.csv(base_finale, paste(path,"Base_projetf.csv", sep="\\"))

write_dta(base_finale, paste(path,"Base_projetf.dta", sep="\\"))


