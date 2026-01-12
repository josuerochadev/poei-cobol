# Fil Rouge CICS-VSAM : Gestion Clientele Financiere

Mini-projet COBOL-CICS sous environnement z/OS pour l'alimentation du Data Set CLIENT d'une institution financiere.

## Objectifs

- Creer et gerer un fichier VSAM CLIENT via transactions CICS
- Implementer les operations CRUD (Create, Read, Update, Delete)
- Utiliser les commandes CICS : READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, ENDBR
- Maitriser les MAPs BMS et la validation des donnees

## Structure du fichier CLIENT

| Champ | Type | Longueur | Description |
|-------|------|----------|-------------|
| Numero de compte | NUM | 6 | Cle unique |
| Code region | NUM | 2 | Code region |
| Nature compte | NUM | 2 | Type de compte |
| Nom client | ALPHA | 10 | Nom |
| Prenom client | ALPHA | 10 | Prenom |
| Date naissance | NUM | 8 | Format AAAAMMJJ |
| Sexe | ALPHA | 1 | M ou F |
| Activite professionnelle | NUM | 2 | Code profession |
| Situation sociale | ALPHA | 1 | C, M, D ou V |
| Adresse | ALPHA | 10 | Adresse |
| Solde | NUM | 10 | Montant |
| Position | ALPHA | 2 | DB ou CR |

**Longueur totale : 64 octets**

## Organisation des exercices

### Partie 0 : Preparation de l'environnement (1 exercice)

| Ex | Dossier | Description |
|----|---------|-------------|
| 0 | `p0-ex00-creation-libraries/` | Creation des 3 libraries PDS (SOURCE, LINK, LOAD) |

### Partie 1 : Creation du Data Set et Affichage (5 exercices)

| Ex | Dossier | Description |
|----|---------|-------------|
| 1 | `p1-ex01-definition-vsam/` | Definition du Data Set CLIENT dans CICS (FCT) |
| 2 | `p1-ex02-map-affichage/` | Creation de la MAP BMS pour affichage |
| 3 | `p1-ex03-prog-affichage/` | Programme COBOL-CICS d'affichage client |
| 4 | `p1-ex04-transaction-ceda/` | Creation transaction via CEDA |
| 5 | `p1-ex05-test-cedf/` | Test avec debugger CEDF |

### Partie 2 : Operations CRUD (10 exercices)

| Ex | Dossier | Description |
|----|---------|-------------|
| 6 | `p2-ex06-map-ajout/` | MAP pour ajout de client |
| 7 | `p2-ex07-prog-ajout/` | Programme d'ajout (WRITE) |
| 8 | `p2-ex08-transaction-ajout/` | Transaction d'ajout |
| 9 | `p2-ex09-map-maj/` | MAP pour mise a jour |
| 10 | `p2-ex10-prog-maj/` | Programme de mise a jour (REWRITE) |
| 11 | `p2-ex11-transaction-maj/` | Transaction de mise a jour |
| 12 | `p2-ex12-map-suppression/` | MAP pour suppression |
| 13 | `p2-ex13-prog-suppression/` | Programme de suppression (DELETE) |
| 14 | `p2-ex14-transaction-suppression/` | Transaction de suppression |
| 15 | `p2-ex15-suppression-avec-lecture/` | Suppression precedee de lecture |

### Partie 3 : Operations avancees (4 exercices)

| Ex | Dossier | Description |
|----|---------|-------------|
| 16 | `p3-ex16-clients-generiques/` | Creation clients avec codes 111xxx, 444xxx, 777xxx |
| 17 | `p3-ex17-suppression-generique/` | Suppression par code generique (STARTBR) |
| 18 | `p3-ex18-lecture-readnext/` | Lecture successive (READNEXT, ENDBR) |
| 19 | `p3-ex19-statistiques-region/` | Statistiques par region (DB/CR) |

## Messages d'erreur standards

```
'ENREGISTREMENT EN DOUBLE'
'ZONE NUMERIQUE, RESAISIR CE CHAMP'
'SAISIE CORRECTE, CONTINUER LA SAISIE (O/N) : '
'REGION INEXISTANTE, SAISIR CODE REGION'
'CLIENT INEXISTANT'
'SUPPRESSION EFFECTUEE'
'MISE A JOUR EFFECTUEE'
```

## Transactions

| Code | Description | Programme |
|------|-------------|-----------|
| AFFI | Affichage client | CLIAFF |
| AJOU | Ajout client | CLIAJT |
| MAJO | Mise a jour client | CLIMAJ |
| SUPP | Suppression client | CLISUP |
| SULE | Suppression avec lecture | CLISUL |
| STAT | Statistiques region | CLISTAT |

## Arborescence

```
fil-rouge/
├── README.md
├── RAPPORT-PROJET.md
├── data/
├── images-pt1/
├── images-pt2/
├── images-pt3/
├── p0-ex00-creation-libraries/
├── p1-ex01-definition-vsam/
├── p1-ex02-map-affichage/
├── ...
└── p3-ex19-statistiques-region/
```

> **Note** : Les copybooks sont generes automatiquement dans l'emulateur lors de l'assemblage des MAPs BMS.

## Environnement

- **Systeme** : z/OS sous Hercules (TK4-/TK5)
- **Interface** : TSO/ISPF, CICS
- **Fichier** : VSAM KSDS
- **Commandes CICS** : READ, WRITE, REWRITE, DELETE, STARTBR, READNEXT, ENDBR
