Mini-Projet Informatique 

Durée = 5 jours. 

Date début = 25/11/2025            Date fin = 27/11/2025 

Travail individuel 

NB : au cours du travail, prière de procéder à faire une capture d’écran concernant le JCL et le résultat obtenu de l’exécution de chaque question. Ceci composera à la fin un support de présentation du travail réalisé.  

Thème : Développement d’un mini-projet COBOL sous environnement Z/os pour le suivi clientèle dans le secteur financier. 

Sujet 

Une institution financière propose de suivre les comptes de ces clients en chiffres et sur des périodes étalées dans le temps pour mener des opérations commerciales et de marketing. 

Le travail sera réalisé sur une plateforme informatique IBM Z/os en se basant principalement sur l’utilisation des outils de travail uniquement ISPF, JCL, VSAM et COBOL. 

Création des Library pour contenir les membres de ce travail :  

Ce travail nécessite la création de trois Library pour stocker les membres à créer au cours de sa réalisation. Les Library à définir doivent porter le nom sous la forme suivante : 

Nom-candidat.FINANCE.SOURCE (programme et JCL) 

 Nom-candidat.FINANCE.LINK (programme Objet) 

Nom-candidat.FINANCE.LOAD (programme exécutable) 

Le Nom-candidat correspond au nom de chacun de vous (se rapporter au support de cours pour voir la structure de chaque Library). 

1ère partie : chargement des données de base et des fichiers référentiels 

La structure des fichiers nécessaires pour le développement de l’application, est présentée ci-dessous : 

-  Un fichier clientèle regroupant les informations relatives à chaque client 

-  Trois fichiers référentiels en relation avec le fichier client.  

Les fichiers mis à la disposition de l’équipe développement sont les suivants : 

Fichier Région (Nom-candidat.FINANCE.REGION ) composé de : 

Code région : 2 caractères numériques			Le code est unique 

Id région : 15 caractères 

Exemple : 

01PARIS 

02MARSEILLE 

03LYON 

04LILLE 

Fichier Nature Compte (Nom-candidat.FINANCE.NATCOMPT ) composé de : 

Code Compte : 2 caractères numériques 				Le code est unique 

Nature Compte : 30 caractères  

Exemple : 

20COMPTE EPARGNE 

25COMPTE CHEQUE 

30COMPTE COMMERCIAL 

35COMPTE COMPAGNE AGRICOLE 

40COMPTE CDI  

Fichier Activité Professionnelle (Nom-candidat.FINANCE.PROFESSION ) composé de :       

Code Profession : 2 caractères numériques 				Le code est unique 

Libellé Profession : 20 caractères  

Exemple : 

05MEDECIN 

10INGENIEUR 

15COMPTABLE 

20COMMERCANT 

25FONCTIONNAIRE 

30PRIVEE  

Fichier CLIENT (Nom-candidat.FINANCE.CLIENT) composé de : 

Numéro de compte                   : 3 caractères numériques		Le numéro est unique 

Code région                                : 2 caractères numériques 

Nature compte                           : 2 caractères numériques 

Nom client                                   : 10 caractères alphabétiques 

Prénom client                              : 10 caractères alphabétiques 

Date naissance                            : 8 caractères numériques (AAAAMMJJ) 

Sexe                                               : 1 caractère (M  F) 

Activité professionnelle             : 2 caractères numériques 

Situation sociale                          : 1 caractère alphabétique (C  M  D  V) 

Adresse                                         : 10 caractères 

Solde                                             : 10 caractères numériques  

Position                                        : 2 caractères  (DB / CR) 

Sur la base de ces données, préparer un Data SET ESDS contenant au moins 20 enregistrements codifiés selon la structure de données ci-dessus. Ce Data set sera exploité par la suite pour différentes opérations concernant les clients. 

Le modèle de données doit être comme suit :  

0010320NOMCLIENT1 PRNCLIENT1 19901102F15CADRCLIENT1    125000CR 

0150225NOMCLIENT2 PRNCLIENT2 19950503M20CADCLIENT2       10000DB 

ETC …. 

Ce Data Set sera composé des différents enregistrements comme suit pour permettre de faire les analyses nécessaires : 

Répartir l’ensemble des Activités Professionnelles sur les différents Clients. 

Répartir l’ensemble des Régions aussi sur les différents Clients. 

Répartir l’ensemble des Nature des Comptes sur les différents Clients. 

Equilibrer entre la présence des deux Sexes dans ce Data Set Clients. 

Définir certains Clients CR et d’autres DB. 

 

Procéder par la création des Data Set données en utilisant aussi bien utilitaire IEBGENER et IDCAMS. Les PS sont créés au préalable, par la suite vous créez et charger les Data Set VSAM à partir de ces PS. 

Le choix de l’utilitaire est au choix de l’utilisateur mais les deux doivent être utilisés. Les Data Set à créer sont :  

Fichier Client 

Fichier Région 

Fichier Nature Compte 

Fichier Activité Professionnelle 

Les JCL peuvent être conçus de manière séparée. 

2ème partie : Utilisation des commandes et utilitaires VSAM  

En utilisant l’utilitaire SORT, créer trois DATA SET VSAM contenant chacun les clients de profession COMPTABLE, FONCTIONNAIRE et MEDECIN portant les noms suivants :  

Nom-candidat.FINANCE.CLIENT.COMPTABLE 

Nom-candidat.FINANCE.CLIENT.FONCTIONNAIRE 

Nom-candidat.FINANCE.CLIENT.MEDECIN 

En utilisant l’utilitaire SORT, créer deux DATA SET VSAM contenant chacun les clients CR et les clients DB. 

En utilisant l’utilitaire SORT, créer une répartition par REGION dans quatre Data Set différents. 

En utilisant l’utilitaire IDCAMS, lister le contenu des deux Data Set DB et CR créer ci-dessus. 

En utilisant l’utilitaire SORT, trier le Data Set Nom-candidat.FINANCE.CLIENT selon le numéro de compte. 

Charger le Data SET Nom-candidat.FINANCE.CLIENT déjà trié dans un nouveau Data Set KSDS portant le nom Nom-candidat.FINANCE.CLIENT.KSDS et dont la KEYS est le Numéro de compte. 

En utilisant l’utilitaire IDCAMS, créer un index secondaire AIX sur le Data Set CLIENT dont la KEYS secondaire est composée du code REGION. Conserver ce Data Set en lui affectant un nom : Nom-candidat.AIX.REGION. 

En utilisant l’utilitaire IDCAMS, créer un index secondaire AIX sur le Data Set CLIENT dont la KEYS secondaire est composée du code Activité Professionnelle. Conserver ce Data Set en lui affectant un nom : Nom-candidat.AIX.ACTPROF. 

Editer le Data Set Nom-candidat.FINANCE.CLIENT en utilisant  l’instruction Write selon l’ordre par REGION et par suite par ACTIVITE PROFESSIONNELLE. Cette édition sera réalisée en faisant un saut de deux lignes en passant d’une REGION à une autre et de même pour les ACTIVITE PROFESSIONNELLE. 

En utilisant l’utilitaire SORT, fusionner les deux Data Set Nom-candidat.FINANCE.CLIENT.FONCTIONNAIRE et Nom-candidat.FINANCE.CLIENT.COMPTABLE dans un Data Set nommé Nom-candidat.FINANCE.CLIENT.FUSION. 

En utilisant l’utilitaire SORT, créer un nouveau Data Set CLIENT réduit, composé des champs suivants uniquement : (nom du fichier : Nom-candidat.CLIENT.REDUIT) 

Numéro de compte : 3 caractères numériques		Le numéro est unique 

Code région                                : 2 caractères numériques 

Nature compte                           : 2 caractères numériques 

Nom client                                   : 10 caractères alphabétiques 

Prénom client                              : 10 caractères alphabétiques 

Activité professionnelle             : 2 caractères numériques 

Situation sociale                          : 1 caractère alphabétique (C  M  D  V) 

Solde                                             : 10 caractères numériques  

Position                                        : 2 caractères  (DB / CR) 

3ème partie : Mise à jour des données et programmation COBOL 

Ecrire un programme COBOL permettant d’ajouter un nouveau client dans le Data Set CLIENT (KSDS). 

Editer sous forme de tableau les trois Data Set REGION, ACTIVITE PROFESSIONNELLE et NATURE COMPTE sous forme de trois sous programmes COBOL séparés. Le contenu de chaque Data Set doit être édité avec l’option d’une interligne vide entre deux lignes et sur une page séparée pour chaque Data Set (utiliser les paramètres ADVANCING PAGE et ADVANVING LINE)   

Calculer le Montant Général et la Moyenne des Comptes CLIENT Débiteurs et des Comptes CLIENT Créditeurs et éditer le résultat sur un fichier Spool.  

 

Calculer la valeur des Comptes Débiteurs et des Comptes Créditeurs par REGION en utilisant la variable conditionnelle (88). Les valeurs de cette variable seront les valeurs des codes régions.  

Lister les cinq premiers Clients DEBITEURS (en valeurs) en se basant sur le Data Set créé précédemment (Exercice 07). 

Créer un nouveau Data Set pour gérer les mouvements des Clients, composé des champs suivants : (pas plus de 20 enregistrements au maximum et utiliser des numéros de compte défini dans le Data Set CLIENT) 

Fichier MOUVEMENT (Nom-candidat.CLIENT.MOUV) composé de :                           

Numéro de compte : 3 caractères numériques 

Libellé Mouvement : 15 caractères alphabétiques 

Montant-Mouvement : 6 caractères numériques 

Sens Mouvement : 2 caractères (DB/CR) 

Nature Mouvement : (CHQ / VER / VIR) 

Date Mouvement : Date (AAAA/MM/JJ) 

 

Ecrire un programme COBOL permettant de calculer le Montant des mouvements d’un CLIENT ainsi que le nombre de ces mouvements. Le programme reçoit l’information du Client à traiter par l’instruction ACCEPT et le numéro de compte sera fourni par une donnée In-stream. (Tri interne avec la condition de récupérer par le tri que les enregistrements relatifs au client mentionné par la donnée In-stream). 

Editer un Relevé de compte des mouvements d’un CLIENT selon ce modèle : (les données seront saisies idem que la question précédente). 

Les lignes tracées de ce tableau peuvent être remplacées et tracées par le caractère * ou =. 

         Nom Client :                                                    Numéro de compte : 

Date opération 

Libellé 

Crédit 

Débit 

………………….. 

………………….. 

………………….. 

…………………………… 

…………………… 

……. 

…………………….. 

…………………. 

DATE :  

Composer des données fictives de trois mois des mouvements clients tel que définis dans la question N° 18 et fusionner les données des Data Set en utilisant l’instruction MERGE interne de COBOL. 

     =======================<<<<<<<fin du Sujet>>>>>>>======================= 

 

 

BON TRAVAIL 