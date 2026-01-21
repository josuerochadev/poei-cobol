# Annexe 2 - Partie 3 : Ecrans BMS

---

**Navigation entre les parties de l'annexe :**

- [Partie 1 : Programmes COBOL](annexes-2-partie1.md)
- [Partie 2 : JCL et Definitions](annexes-2-partie2.md)
- **Partie 3 : Ecrans BMS** (vous etes ici)

---

## Table des matieres

1. [CLIAFF - Affichage Client](#1-cliaff---affichage-client)
2. [CLIAJT - Ajout Client](#2-cliajt---ajout-client)
3. [CLIMAJ - Mise a jour Client](#3-climaj---mise-a-jour-client)
4. [CLISUP - Suppression Client](#4-clisup---suppression-client)
5. [CLIDEL - Suppression Generique Client](#5-clidel---suppression-generique-client)
6. [CLILIST - Liste Generique des Clients](#6-clilist---liste-generique-des-clients)
7. [CLISTAT - Statistiques par Region](#7-clistat---statistiques-par-region)

---

## 1. CLIAFF - Affichage Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIAFF |
| **Map** | MAPAFF |
| **Transaction** | AFFI |
| **Fonction** | Ecran d'affichage des informations d'un client. Permet de saisir un numero de compte et d'afficher toutes les donnees associees au client (region, nature de compte, nom, prenom, date de naissance, sexe, activite professionnelle, situation sociale, adresse, solde et position). |

```asm
***********************************************************************
*  MAPSET : CLIAFF - Affichage Client
*  Transaction : AFFI
*  Fil Rouge CICS - Exercice 2
***********************************************************************
CLIAFF   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAFF   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AFFICHAGE CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(6,25),LENGTH=20,ATTRB=ASKIP
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(7,25),LENGTH=20,ATTRB=ASKIP
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(11,24),LENGTH=10,ATTRB=ASKIP
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,24),LENGTH=10,ATTRB=ASKIP
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,25),LENGTH=10,ATTRB=ASKIP
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(19,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 2. CLIAJT - Ajout Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIAJT |
| **Map** | MAPAJT |
| **Transaction** | AJOU |
| **Fonction** | Ecran de saisie pour l'ajout d'un nouveau client. Tous les champs sont en mode saisie (UNPROT) pour permettre la creation complete d'un enregistrement client dans le fichier VSAM. |

```asm
***********************************************************************
*  MAPSET : CLIAJT - Ajout Client
*  Transaction : AJOU
*  Fil Rouge CICS - Exercice 6
***********************************************************************
CLIAJT   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAJT   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,28),LENGTH=24,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AJOUT CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONES DE SAISIE - TOUS LES CHAMPS EN UNPROT
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 3. CLIMAJ - Mise a jour Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIMAJ |
| **Map** | MAPMAJ |
| **Transaction** | MAJO |
| **Fonction** | Ecran de mise a jour des donnees client. Le numero de compte est d'abord saisissable pour la recherche, puis passe en lecture seule apres affichage des donnees. Cette gestion dynamique des attributs se fait dans le programme COBOL via le suffixe 'A'. |

```asm
***********************************************************************
*  MAPSET : CLIMAJ - Mise a jour Client
*  Transaction : MAJO
*  Fil Rouge CICS - Exercice 9
*
*  PARTICULARITE MISE A JOUR :
*  ---------------------------
*  Le numero de compte est d'abord saisissable (recherche),
*  puis passe en lecture seule apres affichage des donnees.
*  Cette gestion dynamique des attributs se fait dans le programme
*  COBOL via le suffixe 'A' (ex: NUMCPTA pour modifier l'attribut).
*
*  Attribut DFHBMASK = X'20' = ASKIP (protege, normal)
*  Attribut DFHBMUNN = X'4C' = UNPROT + NUM (saisie numerique)
***********************************************************************
CLIMAJ   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPMAJ   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** MISE A JOUR CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* NUMERO DE COMPTE - CHAMP CLE
* Commence en UNPROT pour la saisie initiale (recherche)
* Le programme passera l'attribut a ASKIP apres affichage
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(Cle - non modifiable)'
*----------------------------------------------------------------------
* ZONES DE SAISIE/MODIFICATION
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 4. CLISUP - Suppression Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLISUP |
| **Map** | MAPSUP |
| **Transaction** | SUPP / SULE |
| **Fonction** | Ecran de suppression d'un client. Le numero de compte est saisi pour rechercher le client, les donnees sont affichees en lecture seule pour confirmation. Un champ CONFIRM (O/N) permet de valider la suppression. Deux modes d'utilisation : SUPP (suppression directe) et SULE (suppression avec lecture prealable). |

```asm
***********************************************************************
*  MAPSET : CLISUP - Suppression Client
*  Transaction : SUPP / SULE
*  Fil Rouge CICS - Exercice 12
*
*  PARTICULARITE SUPPRESSION :
*  ---------------------------
*  Le numero de compte est saisi pour rechercher le client.
*  Les donnees sont affichees en lecture seule pour confirmation.
*  Un champ CONFIRM (O/N) permet de valider la suppression.
*
*  Deux modes d'utilisation :
*  - SUPP : Suppression directe (Ex 13-14)
*  - SULE : Suppression avec lecture prealable (Ex 15)
***********************************************************************
CLISUP   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPSUP   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** SUPPRESSION CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT (LECTURE SEULE)
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(6,25),LENGTH=20,ATTRB=ASKIP
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(7,25),LENGTH=20,ATTRB=ASKIP
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(11,24),LENGTH=10,ATTRB=ASKIP
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,24),LENGTH=10,ATTRB=ASKIP
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,25),LENGTH=10,ATTRB=ASKIP
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='CONFIRMER SUPPRESSION (O/N) :'
CONFIRM  DFHMDF POS=(18,33),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(18,35),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(21,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(21,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher/Confirmer  PF3=Quitter  CLEAR=X
               Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 5. CLIDEL - Suppression Generique Client

| Element | Valeur |
|---------|--------|
| **Mapset** | CLIDEL |
| **Map** | MAPDEL |
| **Transaction** | DELG |
| **Fonction** | Ecran de suppression generique permettant la suppression par prefixe (1 a 5 caracteres) ou par cle complete (6 caracteres). Mode pseudo-conversationnel a 2 phases : Phase 1 pour la saisie et le comptage, Phase 2 pour la confirmation et la suppression effective. |

```asm
***********************************************************************
*  MAPSET : CLIDEL - Suppression Generique Client
*  Transaction : DELG
*  Fil Rouge CICS - Exercice 17
*
*  PARTICULARITE :
*  ---------------
*  Permet la suppression par prefixe (1 a 5 car) ou cle complete (6 car)
*  - Prefixe : Supprime tous les clients correspondants
*  - Cle complete : Supprime un seul client
*
*  Le champ PREFIXE est en PIC X (pas NUM) pour eviter la
*  justification a droite des valeurs numeriques.
*
*  MODE PSEUDO-CONVERSATIONNEL A 2 PHASES :
*  Phase 1 : Saisie prefixe/cle -> Comptage et affichage
*  Phase 2 : Confirmation O/N -> Suppression effective
***********************************************************************
CLIDEL   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPDEL   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,20),LENGTH=40,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** SUPPRESSION GENERIQUE CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE OU CLE COMPLETE
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='PREFIXE OU CLE COMPLETE :'
PREFIXE  DFHMDF POS=(5,28),LENGTH=6,ATTRB=(UNPROT,IC)
         DFHMDF POS=(5,35),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(5,37),LENGTH=30,ATTRB=ASKIP,                       X
               INITIAL='(1 a 6 caracteres)'
*----------------------------------------------------------------------
* ZONE D'INFORMATION - NOMBRE DE CLIENTS
*----------------------------------------------------------------------
         DFHMDF POS=(8,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='CLIENTS CORRESPONDANTS  :'
NBCLI    DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(8,35),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(10,2),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='CONFIRMER (O/N)         :'
CONFIRM  DFHMDF POS=(10,28),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,30),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(14,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(15,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(15,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 6. CLILIST - Liste Generique des Clients

| Element | Valeur |
|---------|--------|
| **Mapset** | CLILIST |
| **Map** | MAPLGEN |
| **Transaction** | LGEN |
| **Fonction** | Ecran de liste generique des clients avec pagination. Permet de rechercher des clients par prefixe et d'afficher jusqu'a 10 clients par page avec navigation PF7/PF8. Affiche pour chaque client : numero de compte, region, nom, prenom, solde et position. |

```asm
***********************************************************************
*  MAPSET : CLILIST - Liste Generique des Clients
*  Transaction : LGEN
*  Fil Rouge CICS - Exercice 18
***********************************************************************
CLILIST  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPLGEN  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,18),LENGTH=44,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** LISTE GENERIQUE DES CLIENTS ***'
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE
*----------------------------------------------------------------------
         DFHMDF POS=(3,2),LENGTH=10,ATTRB=ASKIP,INITIAL='PREFIXE :'
PREFIXE  DFHMDF POS=(3,13),LENGTH=6,ATTRB=(UNPROT,IC)
         DFHMDF POS=(3,20),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(3,22),LENGTH=18,ATTRB=ASKIP,                       X
               INITIAL='(1 a 6 caracteres)'
*----------------------------------------------------------------------
* EN-TETE DES COLONNES
*----------------------------------------------------------------------
         DFHMDF POS=(5,1),LENGTH=50,ATTRB=(ASKIP,BRT),                  X
               INITIAL='NUMCPT RG NOM        PRENOM     SOLDE      POS'
*----------------------------------------------------------------------
* LIGNE 1
*----------------------------------------------------------------------
L1NUM    DFHMDF POS=(7,1),LENGTH=6,ATTRB=ASKIP
L1REG    DFHMDF POS=(7,8),LENGTH=2,ATTRB=ASKIP
L1NOM    DFHMDF POS=(7,11),LENGTH=10,ATTRB=ASKIP
L1PRE    DFHMDF POS=(7,22),LENGTH=10,ATTRB=ASKIP
L1SOL    DFHMDF POS=(7,33),LENGTH=10,ATTRB=ASKIP
L1POS    DFHMDF POS=(7,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 2
*----------------------------------------------------------------------
L2NUM    DFHMDF POS=(8,1),LENGTH=6,ATTRB=ASKIP
L2REG    DFHMDF POS=(8,8),LENGTH=2,ATTRB=ASKIP
L2NOM    DFHMDF POS=(8,11),LENGTH=10,ATTRB=ASKIP
L2PRE    DFHMDF POS=(8,22),LENGTH=10,ATTRB=ASKIP
L2SOL    DFHMDF POS=(8,33),LENGTH=10,ATTRB=ASKIP
L2POS    DFHMDF POS=(8,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 3
*----------------------------------------------------------------------
L3NUM    DFHMDF POS=(9,1),LENGTH=6,ATTRB=ASKIP
L3REG    DFHMDF POS=(9,8),LENGTH=2,ATTRB=ASKIP
L3NOM    DFHMDF POS=(9,11),LENGTH=10,ATTRB=ASKIP
L3PRE    DFHMDF POS=(9,22),LENGTH=10,ATTRB=ASKIP
L3SOL    DFHMDF POS=(9,33),LENGTH=10,ATTRB=ASKIP
L3POS    DFHMDF POS=(9,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 4
*----------------------------------------------------------------------
L4NUM    DFHMDF POS=(10,1),LENGTH=6,ATTRB=ASKIP
L4REG    DFHMDF POS=(10,8),LENGTH=2,ATTRB=ASKIP
L4NOM    DFHMDF POS=(10,11),LENGTH=10,ATTRB=ASKIP
L4PRE    DFHMDF POS=(10,22),LENGTH=10,ATTRB=ASKIP
L4SOL    DFHMDF POS=(10,33),LENGTH=10,ATTRB=ASKIP
L4POS    DFHMDF POS=(10,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 5
*----------------------------------------------------------------------
L5NUM    DFHMDF POS=(11,1),LENGTH=6,ATTRB=ASKIP
L5REG    DFHMDF POS=(11,8),LENGTH=2,ATTRB=ASKIP
L5NOM    DFHMDF POS=(11,11),LENGTH=10,ATTRB=ASKIP
L5PRE    DFHMDF POS=(11,22),LENGTH=10,ATTRB=ASKIP
L5SOL    DFHMDF POS=(11,33),LENGTH=10,ATTRB=ASKIP
L5POS    DFHMDF POS=(11,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 6
*----------------------------------------------------------------------
L6NUM    DFHMDF POS=(12,1),LENGTH=6,ATTRB=ASKIP
L6REG    DFHMDF POS=(12,8),LENGTH=2,ATTRB=ASKIP
L6NOM    DFHMDF POS=(12,11),LENGTH=10,ATTRB=ASKIP
L6PRE    DFHMDF POS=(12,22),LENGTH=10,ATTRB=ASKIP
L6SOL    DFHMDF POS=(12,33),LENGTH=10,ATTRB=ASKIP
L6POS    DFHMDF POS=(12,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 7
*----------------------------------------------------------------------
L7NUM    DFHMDF POS=(13,1),LENGTH=6,ATTRB=ASKIP
L7REG    DFHMDF POS=(13,8),LENGTH=2,ATTRB=ASKIP
L7NOM    DFHMDF POS=(13,11),LENGTH=10,ATTRB=ASKIP
L7PRE    DFHMDF POS=(13,22),LENGTH=10,ATTRB=ASKIP
L7SOL    DFHMDF POS=(13,33),LENGTH=10,ATTRB=ASKIP
L7POS    DFHMDF POS=(13,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 8
*----------------------------------------------------------------------
L8NUM    DFHMDF POS=(14,1),LENGTH=6,ATTRB=ASKIP
L8REG    DFHMDF POS=(14,8),LENGTH=2,ATTRB=ASKIP
L8NOM    DFHMDF POS=(14,11),LENGTH=10,ATTRB=ASKIP
L8PRE    DFHMDF POS=(14,22),LENGTH=10,ATTRB=ASKIP
L8SOL    DFHMDF POS=(14,33),LENGTH=10,ATTRB=ASKIP
L8POS    DFHMDF POS=(14,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 9
*----------------------------------------------------------------------
L9NUM    DFHMDF POS=(15,1),LENGTH=6,ATTRB=ASKIP
L9REG    DFHMDF POS=(15,8),LENGTH=2,ATTRB=ASKIP
L9NOM    DFHMDF POS=(15,11),LENGTH=10,ATTRB=ASKIP
L9PRE    DFHMDF POS=(15,22),LENGTH=10,ATTRB=ASKIP
L9SOL    DFHMDF POS=(15,33),LENGTH=10,ATTRB=ASKIP
L9POS    DFHMDF POS=(15,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* LIGNE 10
*----------------------------------------------------------------------
L10NUM   DFHMDF POS=(16,1),LENGTH=6,ATTRB=ASKIP
L10REG   DFHMDF POS=(16,8),LENGTH=2,ATTRB=ASKIP
L10NOM   DFHMDF POS=(16,11),LENGTH=10,ATTRB=ASKIP
L10PRE   DFHMDF POS=(16,22),LENGTH=10,ATTRB=ASKIP
L10SOL   DFHMDF POS=(16,33),LENGTH=10,ATTRB=ASKIP
L10POS   DFHMDF POS=(16,44),LENGTH=2,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE INFORMATIONS PAGINATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=6,ATTRB=ASKIP,INITIAL='PAGE :'
PAGNUM   DFHMDF POS=(18,9),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,13),LENGTH=1,ATTRB=ASKIP,INITIAL='/'
PAGTOT   DFHMDF POS=(18,15),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,22),LENGTH=7,ATTRB=ASKIP,INITIAL='TOTAL :'
CLITOT   DFHMDF POS=(18,30),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,36),LENGTH=10,ATTRB=ASKIP,INITIAL='CLIENT(S)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=60,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Chercher  PF7=Prec  PF8=Suiv  PF3=Quitter'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

## 7. CLISTAT - Statistiques par Region

| Element | Valeur |
|---------|--------|
| **Mapset** | CLISTAT |
| **Map** | MAPSTAT |
| **Transaction** | STAT |
| **Fonction** | Ecran de statistiques par region. Affiche pour une region donnee : le nombre total de clients, le nombre et la somme des clients debiteurs (DB), le nombre et la somme des clients crediteurs (CR). Regions disponibles : 01-Paris, 02-Marseille, 03-Lyon, 04-Lille. |

```asm
***********************************************************************
*  MAPSET : CLISTAT - Statistiques par Region
*  Transaction : STAT
*  Fil Rouge CICS - Exercice 19
*
*  FONCTIONNALITE :
*  ----------------
*  Affiche les statistiques d'une region :
*  - Nombre total de clients
*  - Nombre et somme des clients debiteurs (DB)
*  - Nombre et somme des clients crediteurs (CR)
*
*  REGIONS DISPONIBLES :
*  01 - Paris     02 - Marseille
*  03 - Lyon      04 - Lille
*
*  MODE PSEUDO-CONVERSATIONNEL :
*  - Premier passage : Affichage ecran de saisie
*  - Passages suivants : Calcul et affichage des statistiques
***********************************************************************
CLISTAT  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPSTAT  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,20),LENGTH=40,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** STATISTIQUES PAR REGION ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - CODE REGION
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION           :'
CODREG   DFHMDF POS=(4,28),LENGTH=2,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,31),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(4,33),LENGTH=40,ATTRB=ASKIP,                       X
               INITIAL='(01=Paris, 02=Marseille, 03=Lyon, 04=Lille)'
*----------------------------------------------------------------------
* NOM DE LA REGION
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='REGION                :'
NOMREG   DFHMDF POS=(6,28),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* SEPARATEUR
*----------------------------------------------------------------------
         DFHMDF POS=(8,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* STATISTIQUES GLOBALES
*----------------------------------------------------------------------
         DFHMDF POS=(10,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='NOMBRE TOTAL DE CLIENTS         :'
NBTOT    DFHMDF POS=(10,38),LENGTH=5,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* STATISTIQUES DEBITEURS
*----------------------------------------------------------------------
         DFHMDF POS=(12,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='CLIENTS DEBITEURS (DB)          :'
NBDB     DFHMDF POS=(12,38),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='SOMME DES SOLDES DEBITEURS      :'
MTDB     DFHMDF POS=(13,38),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* STATISTIQUES CREDITEURS
*----------------------------------------------------------------------
         DFHMDF POS=(15,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='CLIENTS CREDITEURS (CR)         :'
NBCR     DFHMDF POS=(15,38),LENGTH=5,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,2),LENGTH=35,ATTRB=ASKIP,                       X
               INITIAL='SOMME DES SOLDES CREDITEURS     :'
MTCR     DFHMDF POS=(16,38),LENGTH=15,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* SEPARATEUR
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Calculer  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

---

**Navigation entre les parties de l'annexe :**

- [Partie 1 : Programmes COBOL](annexes-2-partie1.md)
- [Partie 2 : JCL et Definitions](annexes-2-partie2.md)
- **Partie 3 : Ecrans BMS** (vous etes ici)
