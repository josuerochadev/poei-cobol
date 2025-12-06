# Exercices Z/OS et TSO/ISPF

## Organisation

Les exercices sont organisés par chapitre du cours.

## Chapitres

### Chapitre 03 - TSO

| Fichier | Description |
|---------|-------------|
| `03-exercices-tso.md` | 14 exercices sur les commandes TSO |

**Exercices couverts :**
1. Ouverture de session (LOGON)
2. Création PDS avec ALLOCATE
3. Création dataset modèle
4. Création avec LIKE (selon modèle)
5-6. Création datasets supplémentaires
7. LISTCAT avec LEVEL
8-9. Suppression avec DELETE
10. LISTDS avec variations (STATUS, MEMBERS, HISTORY, ALL)
11. Renommage avec RENAME
12. Message opérateur avec SEND
13-14. Exercices complémentaires

---

### Chapitre 04 - ISPF/PDF

| Fichier | Description |
|---------|-------------|
| `04-exercices-ispf.md` | 15 exercices sur ISPF/PDF |

**Exercices couverts :**

*Partie 1 - Gestion des membres de bibliothèque :*
1. Lister le contenu d'une Library
2. Créer un nouveau membre
3. Afficher les caractéristiques d'une Library
4. Créer une nouvelle Library
5. Copier un membre
6. Renommer un membre

*Partie 2 - Navigation volumes et fichiers :*
7. Afficher le contenu d'un volume (disque)
8. Afficher les caractéristiques d'un fichier
9. Afficher la VTOC d'un volume
10. Différence entre COPY et MOVE

*Partie 3 - Éditeur ISPF avancé :*
11. Créer un membre avec l'éditeur
12. Déplacer du texte APRÈS une position (MM + A)
13. Déplacer du texte AVANT une position (MM + B)
14. Exclusion de lignes et commandes F, L, S
15. COLS, TABS, BNDS, MASK et PROFILE

## Prérequis

- Accès à un environnement z/OS (ou émulateur)
- Userid TSO valide
- Droits de création de datasets

## Notes

Ces exercices sont conçus pour être exécutés sur un mainframe IBM z/OS.
Pour les environnements de formation, un émulateur comme Hercules ou
IBM Z Development and Test Environment peut être utilisé.
