# Exercices TSO/ISPF - Chapitre III

## Objectifs

Ces exercices permettent de pratiquer la gestion des datasets via **ISPF** (interface privilégiée) sur un émulateur Hercules (TK4-/TK5).

Les commandes TSO équivalentes sont mentionnées pour référence.

---

## Prérequis

- Émulateur Hercules avec TK4- ou TK5
- Session TSO/ISPF active
- Userid : FTEST (ou votre userid)

---

## Navigation ISPF - Rappel

```
┌─────────────────────────────────────────────────────────────────┐
│                    MENU PRINCIPAL ISPF                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   0  Settings      - Paramètres                                 │
│   1  View          - Visualiser (lecture seule)                 │
│   2  Edit          - Éditer                                     │
│   3  Utilities     - Utilitaires  ◄── DATASETS                  │
│   4  Foreground    - Compilation                                │
│   5  Batch         - Soumission batch                           │
│   6  Command       - Commandes TSO                              │
│   ...                                                           │
│                                                                  │
│   Option 3 (Utilities) :                                        │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  1  Library    - Membres PDS                            │   │
│   │  2  Data Set   - Opérations sur datasets                │   │
│   │  3  Move/Copy  - Copier/Déplacer                        │   │
│   │  4  Dslist     - Liste de datasets  ◄── LE PLUS UTILISÉ│   │
│   │  ...                                                    │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Raccourci** : Taper `=3.4` depuis n'importe quel écran pour aller directement à DSLIST.

---

## Exercice 1 : Ouverture de session

**Objectif** : Se connecter à TSO/ISPF

**Via l'écran LOGON :**
```
┌─────────────────────────────────────────────────────────────────┐
│                    TSO/E LOGON                                   │
│                                                                  │
│   Userid    ===> FTEST                                          │
│   Password  ===> ________                                       │
│   Procedure ===> IKJACCNT                                       │
│   Command   ===> ISPF         ◄── Lance ISPF automatiquement    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

Appuyer sur **Entrée** pour se connecter.

---

## Exercice 2 : Création d'un PDS (bibliothèque)

**Objectif** : Créer `FTEST.PROD.LIBTEST` (PO, LRECL=80, RECFM=FB)

### Méthode ISPF (Option 3.2)

1. Aller à **Option 3.2** (taper `=3.2` ou naviguer via menus)

2. Remplir l'écran :
```
┌─────────────────────────────────────────────────────────────────┐
│                 DATA SET UTILITY                                 │
│                                                                  │
│   Option ===> A                    ◄── A = Allocate             │
│                                                                  │
│   Data Set Name ===> FTEST.PROD.LIBTEST                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée**, remplir les attributs :
```
┌─────────────────────────────────────────────────────────────────┐
│                 ALLOCATE NEW DATA SET                            │
│                                                                  │
│   Data Set Name : FTEST.PROD.LIBTEST                            │
│                                                                  │
│   Management class  ===>                                        │
│   Storage class     ===>                                        │
│   Volume serial     ===> PUB001    (ou laisser vide si SMS)     │
│   Device type       ===>                                        │
│   Data class        ===>                                        │
│   Space units       ===> TRACK     (TRACK, CYL, ou BLOCK)       │
│   Primary quantity  ===> 2                                      │
│   Secondary quantity===> 1                                      │
│   Directory blocks  ===> 5         ◄── Obligatoire pour PDS     │
│   Record format     ===> FB        ◄── Fixed Blocked            │
│   Record length     ===> 80                                     │
│   Block size        ===> 800       ◄── Multiple de LRECL        │
│   Data set type     ===> PDS       (PDS, LIBRARY, ou vide)      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

4. Appuyer **Entrée** pour créer.

**Message attendu** : `DATA SET ALLOCATED`

### Commande TSO équivalente (Option 6)
```
ALLOC DA('FTEST.PROD.LIBTEST') NEW CATALOG +
      DSORG(PO) RECFM(FB) LRECL(80) BLKSIZE(800) +
      SPACE(2,1) TRACKS DIR(5)
```

---

## Exercice 3 : Création d'un dataset modèle

**Objectif** : Créer `FTEST.DEV.MODEL` (PS, LRECL=80, RECFM=FB)

### Méthode ISPF (Option 3.2)

1. **Option 3.2**, puis **A** (Allocate)

2. Data Set Name : `FTEST.DEV.MODEL`

3. Attributs :
```
┌─────────────────────────────────────────────────────────────────┐
│   Space units       ===> TRACK                                  │
│   Primary quantity  ===> 1                                      │
│   Secondary quantity===> 1                                      │
│   Directory blocks  ===>           ◄── Vide pour PS (séquentiel)│
│   Record format     ===> FB        ◄── Fixed Blocked            │
│   Record length     ===> 80                                     │
│   Block size        ===> 27920     ◄── Optimal (349 x 80)       │
│   Data set type     ===>           ◄── Vide pour PS             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Note** : Pas de Directory blocks = dataset séquentiel (PS)

---

## Exercice 4 : Création selon un modèle (LIKE)

**Objectif** : Créer `FTEST.DEV.FILE` basé sur `FTEST.DEV.MODEL`

### Méthode ISPF (Option 3.2)

1. **Option 3.2**, puis **A** (Allocate)

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│   Data Set Name ===> FTEST.DEV.FILE                             │
│                                                                  │
│   Based on Data Set ===> FTEST.DEV.MODEL    ◄── Le modèle       │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée** - les attributs sont pré-remplis depuis le modèle.

4. Appuyer **Entrée** à nouveau pour confirmer.

### Commande TSO équivalente
```
ALLOC DA('FTEST.DEV.FILE') NEW CATALOG LIKE('FTEST.DEV.MODEL')
```

---

## Exercice 5-6 : Création de datasets supplémentaires

**Objectif** : Créer `FTEST.PROD.FILE` et `FTEST.EXPLOI.FILE`

Répéter l'exercice 4 avec :
- `FTEST.PROD.FILE` basé sur `FTEST.DEV.MODEL`
- `FTEST.EXPLOI.FILE` basé sur `FTEST.DEV.MODEL`

---

## Exercice 7 : Lister le catalogue (DSLIST)

**Objectif** : Afficher tous les datasets FTEST.*

### Méthode ISPF (Option 3.4) - RECOMMANDÉE

1. Aller à **Option 3.4** (taper `=3.4`)

2. Remplir :
```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA SET LIST UTILITY                         │
│                                                                  │
│   Dsname Level ===> FTEST                                       │
│   Volume       ===>           (vide = catalogués seulement)     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

3. Appuyer **Entrée**

**Résultat** :
```
┌─────────────────────────────────────────────────────────────────┐
│ DSLIST - Data Sets Matching FTEST                    Row 1 of 5 │
│ Command ===>                                                    │
│                                                                  │
│ Command     Name                                     Volume     │
│ -------     ----                                     ------     │
│             FTEST.DEV.FILE                           PUB001     │
│             FTEST.DEV.MODEL                          PUB001     │
│             FTEST.EXPLOI.FILE                        PUB001     │
│             FTEST.PROD.FILE                          PUB001     │
│             FTEST.PROD.LIBTEST                       PUB001     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

**Commandes ligne disponibles** (taper devant le nom) :
- `i` = Info (attributs)
- `e` = Edit
- `b` = Browse
- `d` = Delete
- `r` = Rename
- `m` = Members (pour PDS)

### Commande TSO équivalente
```
LISTCAT LEVEL(FTEST)
```

---

## Exercice 8 : Suppression d'un dataset

**Objectif** : Supprimer `FTEST.PROD.LIBTEST`

### Méthode ISPF (Option 3.4)

1. Aller à **Option 3.4**, Dsname Level = `FTEST`

2. Taper `d` devant `FTEST.PROD.LIBTEST` :
```
│ Command     Name                                                │
│ d           FTEST.PROD.LIBTEST                       PUB001     │
```

3. Appuyer **Entrée**

4. Confirmer la suppression (si demandé)

**Message** : `DATA SET DELETED`

### Alternative : Option 3.2
1. Option 3.2, taper **D** (Delete)
2. Data Set Name = `FTEST.PROD.LIBTEST`
3. Entrée, confirmer

### Commande TSO équivalente
```
DELETE 'FTEST.PROD.LIBTEST'
```

---

## Exercice 9 : Suppression de EXPLOI.FILE

**Objectif** : Supprimer `FTEST.EXPLOI.FILE`

Même méthode que l'exercice 8 :
- Option 3.4, taper `d` devant le dataset
- Ou Option 3.2 avec **D**

---

## Exercice 10 : Afficher les attributs d'un dataset

**Objectif** : Explorer les attributs de `FTEST.DEV.FILE`

### Méthode ISPF (Option 3.4)

1. Option 3.4, Dsname Level = `FTEST`

2. Taper `i` (info) devant `FTEST.DEV.FILE` :
```
│ Command     Name                                                │
│ i           FTEST.DEV.FILE                           PUB001     │
```

3. Appuyer **Entrée**

**Résultat** :
```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA SET INFORMATION                          │
│                                                                  │
│  Data Set Name  : FTEST.DEV.FILE                                │
│                                                                  │
│  General Data:                          Current Allocation:     │
│   Volume serial : PUB001                 Allocated tracks : 1   │
│   Device type   : 3390                   Used tracks      : 0   │
│   Organization  : PS                     Extents          : 1   │
│   Record format : FB                                            │
│   Record length : 80                    Creation date: 2025/001 │
│   Block size    : 27920                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Alternative : Option 3.2
1. Option 3.2, taper **I** (Info)
2. Data Set Name = `FTEST.DEV.FILE`

### Commande TSO équivalente
```
LISTDS 'FTEST.DEV.FILE' ALL
```

---

## Exercice 11 : Renommer un dataset

**Objectif** : Renommer `FTEST.DEV.MODEL` en `FTEST.DEV.MODELE`

### Méthode ISPF (Option 3.4)

1. Option 3.4, Dsname Level = `FTEST`

2. Taper `r` (rename) devant `FTEST.DEV.MODEL` :
```
│ Command     Name                                                │
│ r           FTEST.DEV.MODEL                          PUB001     │
```

3. Appuyer **Entrée**

4. Saisir le nouveau nom :
```
┌─────────────────────────────────────────────────────────────────┐
│                    RENAME DATA SET                               │
│                                                                  │
│   Current Name: FTEST.DEV.MODEL                                 │
│                                                                  │
│   New Name ===> FTEST.DEV.MODELE                                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

5. Appuyer **Entrée**

**Message** : `DATA SET RENAMED`

### Alternative : Option 3.2
1. Option 3.2, taper **R** (Rename)
2. Old Name = `FTEST.DEV.MODEL`
3. New Name = `FTEST.DEV.MODELE`

### Commande TSO équivalente
```
RENAME 'FTEST.DEV.MODEL' 'FTEST.DEV.MODELE'
```

---

## Exercice 12 : Message à l'opérateur

**Objectif** : Envoyer un message à la console opérateur

### Méthode ISPF (Option 6 ou préfixe TSO)

Cette commande n'a pas d'équivalent menu ISPF, il faut utiliser TSO.

**Option 1 : Via Option 6 (Command)**
1. Aller à **Option 6**
2. Taper la commande :
```
SEND 'DEMARRAGE PRODUCTION PREVU A 22H00' OPERATOR
```

**Option 2 : Préfixe TSO depuis n'importe quel écran**
Sur la ligne de commande ISPF, taper :
```
Command ===> TSO SEND 'DEMARRAGE PRODUCTION PREVU A 22H00' OPERATOR
```

**Message** : `MESSAGE SENT TO OPERATOR`

Le message apparaît sur la console Hercules.

---

## Exercice 13 : Renommer DEV.FILE

**Objectif** : Renommer `FTEST.DEV.FILE` en `FTEST.DEV.COMPTA`

### Méthode ISPF (Option 3.4)

1. Option 3.4, taper `r` devant `FTEST.DEV.FILE`
2. Nouveau nom : `FTEST.DEV.COMPTA`
3. Entrée

---

## Exercice 14 : Attributs de PROD.LIBTEST

**Objectif** : Afficher les caractéristiques de `FTEST.PROD.LIBTEST`

**Note** : Ce dataset a été supprimé à l'exercice 8.
Recréez-le d'abord via Option 3.2 (voir exercice 2).

### Méthode ISPF

1. Option 3.4, Dsname Level = `FTEST.PROD`
2. Taper `i` devant `FTEST.PROD.LIBTEST`
3. Consulter les informations affichées

---

## Récapitulatif des datasets

| Dataset | Type | RECFM | Statut final |
|---------|------|-------|--------------|
| FTEST.PROD.LIBTEST | PO | FB | Supprimé (Ex.8), recréer pour Ex.14 |
| FTEST.DEV.MODEL | PS | FB | Renommé → FTEST.DEV.MODELE |
| FTEST.DEV.FILE | PS | FB | Renommé → FTEST.DEV.COMPTA |
| FTEST.PROD.FILE | PS | FB | Existe |
| FTEST.EXPLOI.FILE | PS | FB | Supprimé (Ex.9) |

---

## Aide-mémoire ISPF

```
┌─────────────────────────────────────────────────────────────────┐
│                    RACCOURCIS ISPF                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  NAVIGATION                                                     │
│  ──────────────────────────────────────────────────────────────  │
│  =3.2         Aller directement à Data Set Utility              │
│  =3.4         Aller directement à DSLIST (le plus utilisé)      │
│  =6           Aller à Command (pour commandes TSO)              │
│  =X           Quitter ISPF                                      │
│  PF3          Retour / Sortie                                   │
│                                                                  │
│  OPTION 3.2 - DATA SET UTILITY                                  │
│  ──────────────────────────────────────────────────────────────  │
│  A            Allocate (créer)                                  │
│  D            Delete (supprimer)                                │
│  R            Rename (renommer)                                 │
│  I            Info (attributs)                                  │
│  C            Catalog                                           │
│  U            Uncatalog                                         │
│                                                                  │
│  OPTION 3.4 - DSLIST (commandes ligne)                          │
│  ──────────────────────────────────────────────────────────────  │
│  i            Info (attributs)                                  │
│  e            Edit                                              │
│  b            Browse (lecture seule)                            │
│  d            Delete                                            │
│  r            Rename                                            │
│  m            Members (liste membres PDS)                       │
│  c            Copy                                              │
│  z            Compress (PDS)                                    │
│                                                                  │
│  COMMANDES TSO (depuis ISPF)                                    │
│  ──────────────────────────────────────────────────────────────  │
│  TSO commande         Exécuter une commande TSO                 │
│  TSO SEND 'msg' USER(uid)     Envoyer message                   │
│  TSO LISTDS 'dsn' ALL         Attributs dataset                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Nettoyage (optionnel)

Pour supprimer tous les datasets créés :

1. Option 3.4, Dsname Level = `FTEST`
2. Taper `d` devant chaque dataset à supprimer
3. Appuyer Entrée et confirmer
