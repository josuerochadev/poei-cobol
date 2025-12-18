# Exercices Z/OS et TSO/ISPF

## Organisation

Les exercices sont organises en deux parties :
- **Theorie** : QCM pour valider les connaissances theoriques
- **Pratique** : Exercices hands-on sur environnement z/OS

---

## Partie Theorique (QCM)

Les QCM sont organises par chapitre du cours et permettent de valider la comprehension des concepts.

| Fichier | Chapitre | Nombre de questions |
|---------|----------|---------------------|
| `theorie/qcm-01-presentation-zos.md` | I - Presentation generale z/OS | 20 questions |
| `theorie/qcm-02-fonctionnement-zos.md` | II - Fonctionnement z/OS | 22 questions |
| `theorie/qcm-03-tso.md` | III - TSO | 22 questions |
| `theorie/qcm-04-ispf.md` | IV - ISPF/PDF | 25 questions |
| `theorie/qcm-05-architecture-zos.md` | V - Architecture z/OS | 25 questions |

**Total : 114 questions**

### Themes couverts par QCM

#### QCM-01 : Presentation generale z/OS
- Historique des mainframes (System/360, z/Architecture)
- Terminologie IBM (Data Set, PDS, VSAM, JCL, SPOOL, DASD)
- Avantages des mainframes
- Codes retour et codes ABEND

#### QCM-02 : Fonctionnement z/OS
- Memoire virtuelle (pages, frames, slots)
- DAT (Dynamic Address Translation)
- Espace d'adressage (24 bits, 31 bits, 64 bits)
- Gestion des taches (TCB, SRB)
- Types de fichiers VSAM

#### QCM-03 : TSO
- Commandes LOGON/LOGOFF
- ALLOCATE, DELETE, RENAME
- LISTDS, LISTCAT, LISTALC
- SUBMIT, STATUS, CANCEL
- PROFILE, HELP, SEND

#### QCM-04 : ISPF/PDF
- Navigation ISPF (raccourcis, touches de fonction)
- Utilitaires (3.1, 3.2, 3.4, SDSF)
- Commandes editeur (FIND, CHANGE, COLS)
- Commandes ligne (I, D, C, M, R, X, >, <)
- Commandes bloc (DD, CC, MM, XX)

#### QCM-05 : Architecture z/OS
- SYSPLEX (Basic, Parallel, Coupling Facility)
- SMS (Data Class, Storage Class, Management Class, ACS)
- RACF (ADDUSER, ADDSD, PERMIT, niveaux d'acces)

---

## Partie Pratique

### Chapitre 03 - TSO

| Fichier | Description |
|---------|-------------|
| `03-exercices-tso.md` | 14 exercices sur les commandes TSO |

**Exercices couverts :**
1. Ouverture de session (LOGON)
2. Creation PDS avec ALLOCATE
3. Creation dataset modele
4. Creation avec LIKE (selon modele)
5-6. Creation datasets supplementaires
7. LISTCAT avec LEVEL
8-9. Suppression avec DELETE
10. LISTDS avec variations (STATUS, MEMBERS, HISTORY, ALL)
11. Renommage avec RENAME
12. Message operateur avec SEND
13-14. Exercices complementaires

---

### Chapitre 04 - ISPF/PDF

| Fichier | Description |
|---------|-------------|
| `04-exercices-ispf.md` | 15 exercices sur ISPF/PDF |

**Exercices couverts :**

*Partie 1 - Gestion des membres de bibliotheque :*
1. Lister le contenu d'une Library
2. Creer un nouveau membre
3. Afficher les caracteristiques d'une Library
4. Creer une nouvelle Library
5. Copier un membre
6. Renommer un membre

*Partie 2 - Navigation volumes et fichiers :*
7. Afficher le contenu d'un volume (disque)
8. Afficher les caracteristiques d'un fichier
9. Afficher la VTOC d'un volume
10. Difference entre COPY et MOVE

*Partie 3 - Editeur ISPF avance :*
11. Creer un membre avec l'editeur
12. Deplacer du texte APRES une position (MM + A)
13. Deplacer du texte AVANT une position (MM + B)
14. Exclusion de lignes et commandes F, L, S
15. COLS, TABS, BNDS, MASK et PROFILE

---

## Prerequis

- Acces a un environnement z/OS (ou emulateur)
- Userid TSO valide
- Droits de creation de datasets

## Notes

Ces exercices sont concus pour etre executes sur un mainframe IBM z/OS.
Pour les environnements de formation, un emulateur comme Hercules ou
IBM Z Development and Test Environment peut etre utilise.

## Progression recommandee

1. **Theorie d'abord** : Faire les QCM pour valider la comprehension
2. **Pratique ensuite** : Realiser les exercices hands-on
3. **Revision** : Refaire les QCM apres la pratique pour consolider

---
*Formation z/OS et TSO/ISPF - M2i Formation*
