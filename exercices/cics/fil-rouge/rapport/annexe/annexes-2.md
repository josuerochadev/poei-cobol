# Annexe 2 - Code Source Complet du Fil Rouge CICS

## Introduction

Cette annexe contient l'integralite du code source developpe pour le projet fil-rouge CICS de gestion de clients bancaires. Le code est organise en trois parties pour faciliter la consultation.

## Organisation des fichiers

| Partie | Fichier | Contenu |
|--------|---------|---------|
| Partie 1 | [annexes-2-partie1.md](annexes-2-partie1.md) | Programmes COBOL (PRGCLIA, PRGAJT, PRGMAJ) |
| Partie 2 | [annexes-2-partie2.md](annexes-2-partie2.md) | Programmes COBOL (PRGSUP, PRGDELG, PRGLGEN, PRGSTAT) |
| Partie 3 | [annexes-2-partie3.md](annexes-2-partie3.md) | Ecrans BMS (CLIAFF, CLIAJT, CLIMAJ, CLISUP, CLIDEL, CLILIST, CLISTAT) |

## Recapitulatif des programmes

### Programmes COBOL (7 fichiers)

| Programme | Transaction | Fonction | Exercice |
|-----------|-------------|----------|----------|
| PRGCLIA | AFFI | Affichage client par numero | 3 |
| PRGAJT | AJOU | Ajout d'un nouveau client | 7 |
| PRGMAJ | MAJO | Mise a jour client existant | 10 |
| PRGSUP | SUPP | Suppression avec confirmation | 13 |
| PRGDELG | DELG | Suppression generique par prefixe | 17 |
| PRGLGEN | LGEN | Liste paginee par prefixe | 18 |
| PRGSTAT | STAT | Statistiques par region (AIX/PATH) | 19 |

### Ecrans BMS (7 fichiers)

| Mapset | Map | Fonction |
|--------|-----|----------|
| CLIAFF | MAPAFF | Ecran affichage client |
| CLIAJT | MAPAJT | Ecran ajout client |
| CLIMAJ | MAPMAJ | Ecran mise a jour |
| CLISUP | MAPSUP | Ecran suppression |
| CLIDEL | MAPDEL | Ecran suppression generique |
| CLILIST | MAPLGEN | Ecran liste paginee |
| CLISTAT | MAPSTAT | Ecran statistiques |

## Structure d'un enregistrement client

```
Position  Longueur  Champ         Description
01-06     6         NUMCPT        Numero de compte (cle primaire)
07-08     2         CODREG        Code region (01-04)
09-10     2         NATCPT        Nature du compte
11-20     10        NOM           Nom du client
21-30     10        PRENOM        Prenom du client
31-38     8         DATNAISS      Date de naissance (AAAAMMJJ)
39        1         SEXE          Sexe (M/F)
40-41     2         ACTPRO        Activite professionnelle
42        1         SITSO         Situation sociale (C/M/D/V)
43-52     10        ADRESSE       Adresse
53-62     10        SOLDE         Solde du compte
63-64     2         POSITION      Position (CR/DB)
65-80     16        FILLER        Reserve
```

## Codes de reference

### Codes Region
- 01 : Paris
- 02 : Marseille
- 03 : Lyon
- 04 : Lille

### Codes Sexe
- M : Masculin
- F : Feminin

### Codes Situation Sociale
- C : Celibataire
- M : Marie(e)
- D : Divorce(e)
- V : Veuf/Veuve

### Codes Position
- CR : Crediteur
- DB : Debiteur
