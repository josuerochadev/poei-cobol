# Exercices TSO - Chapitre III

## Objectifs

Ces exercices permettent de pratiquer les commandes TSO essentielles :
- Connexion et gestion de session
- Création et gestion de datasets
- Commandes utilitaires (LISTCAT, LISTDS, RENAME)
- Communication avec l'opérateur

---

## Exercice 1 : Ouverture de session

**Objectif** : Se connecter à TSO

**Instructions** :
1. Ouvrir une session en utilisant la commande LOGON
2. Utiliser la transaction TSO pour s'identifier

**Solution** :
```
LOGON userid
```
ou depuis l'écran de connexion TSO :
```
┌─────────────────────────────────────────────────────────────────┐
│                    TSO/E LOGON                                   │
│                                                                  │
│   Userid    ===> FTEST___                                       │
│   Password  ===> ________                                       │
│   Procedure ===> IKJACCNT                                       │
│   Command   ===> ISPF                                           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercice 2 : Création d'un PDS (Partitioned Data Set)

**Objectif** : Créer une bibliothèque avec ALLOCATE

**Spécifications** :
- Nom : `FTEST.PROD.LIBTEST`
- Taille : 2 Tracks
- Organisation : PO (Partitioned)
- LRECL : 80
- Format : Fixe (F)

**Solution** :
```
ALLOC DA('FTEST.PROD.LIBTEST') NEW CATALOG +
      DSORG(PO) RECFM(F) LRECL(80) BLKSIZE(80) +
      SPACE(2,1) TRACKS DIR(5)
```

**Explication** :
- `DA('...')` : Nom du dataset (avec quotes si HLQ différent du userid)
- `NEW CATALOG` : Création et catalogage
- `DSORG(PO)` : Organisation Partitioned (bibliothèque)
- `RECFM(F)` : Format fixe
- `LRECL(80)` : Longueur enregistrement 80 octets
- `SPACE(2,1) TRACKS` : 2 tracks primaires, 1 track secondaire
- `DIR(5)` : 5 blocs directory (pour les membres)

---

## Exercice 3 : Création d'un dataset modèle

**Objectif** : Créer un dataset qui servira de modèle

**Spécifications** :
- Nom : `FTEST.DEV.MODEL`
- Organisation : PS (Physical Sequential)
- LRECL : 80
- Format : FB (Fixed Blocked)

**Solution** :
```
ALLOC DA('FTEST.DEV.MODEL') NEW CATALOG +
      DSORG(PS) RECFM(F,B) LRECL(80) BLKSIZE(27920) +
      SPACE(1,1) TRACKS
```

**Explication** :
- `DSORG(PS)` : Organisation séquentielle
- `RECFM(F,B)` : Format fixe bloqué
- `BLKSIZE(27920)` : 349 enregistrements par bloc (27920/80)

---

## Exercice 4 : Création selon un modèle (LIKE)

**Objectif** : Créer un dataset basé sur le modèle de l'exercice 3

**Spécifications** :
- Nom : `FTEST.DEV.FILE`
- Basé sur : `FTEST.DEV.MODEL`

**Solution** :
```
ALLOC DA('FTEST.DEV.FILE') NEW CATALOG +
      LIKE('FTEST.DEV.MODEL')
```

**Explication** :
- `LIKE('...')` : Copie les attributs (DSORG, RECFM, LRECL, BLKSIZE) du modèle
- Seul le nom change, tous les autres attributs sont hérités

---

## Exercice 5 : Création de datasets supplémentaires

**Objectif** : Créer deux datasets supplémentaires selon le même modèle

**Spécifications** :
- Dataset 1 : `FTEST.PROD.FILE`
- Dataset 2 : `FTEST.EXPLOI.FILE`

**Solution** :
```
ALLOC DA('FTEST.PROD.FILE') NEW CATALOG +
      LIKE('FTEST.DEV.MODEL')

ALLOC DA('FTEST.EXPLOI.FILE') NEW CATALOG +
      LIKE('FTEST.DEV.MODEL')
```

---

## Exercice 6 : Pas de spécification (voir exercice 5)

---

## Exercice 7 : Commande LISTCAT avec LEVEL

**Objectif** : Lister les entrées du catalogue correspondant à un préfixe

**Solution** :
```
LISTCAT LEVEL(FTEST)
```

**Résultat attendu** :
```
NONVSAM ------- FTEST.PROD.LIBTEST
NONVSAM ------- FTEST.DEV.MODEL
NONVSAM ------- FTEST.DEV.FILE
NONVSAM ------- FTEST.PROD.FILE
NONVSAM ------- FTEST.EXPLOI.FILE
```

**Variantes** :
```
LISTCAT LEVEL(FTEST.PROD)
```
Affiche uniquement les datasets commençant par FTEST.PROD

```
LISTCAT LEVEL(FTEST) ALL
```
Affiche les détails complets de chaque entrée

---

## Exercice 8 : Suppression du fichier LIBTEST

**Objectif** : Supprimer le dataset `FTEST.PROD.LIBTEST`

**Solution** :
```
DELETE 'FTEST.PROD.LIBTEST'
```

**Résultat attendu** :
```
ENTRY FTEST.PROD.LIBTEST DELETED
```

**Vérification** :
```
LISTCAT LEVEL(FTEST.PROD)
```
Le dataset FTEST.PROD.LIBTEST ne doit plus apparaître.

---

## Exercice 9 : Suppression du fichier EXPLOI.FILE

**Objectif** : Supprimer le dataset `FTEST.EXPLOI.FILE`

**Solution** :
```
DELETE 'FTEST.EXPLOI.FILE'
```

**Résultat attendu** :
```
ENTRY FTEST.EXPLOI.FILE DELETED
```

---

## Exercice 10 : Commande LISTDS avec variations

**Objectif** : Explorer les différentes options de LISTDS

**Solutions** :

### Option 1 : LISTDS simple
```
LISTDS 'FTEST.DEV.FILE'
```
**Résultat** :
```
FTEST.DEV.FILE
--RECFM-FB--LRECL-80--BLKSIZE-27920
```

### Option 2 : LISTDS avec STATUS
```
LISTDS 'FTEST.DEV.FILE' STATUS
```
**Résultat** :
```
FTEST.DEV.FILE
--RECFM-FB--LRECL-80--BLKSIZE-27920
--VOLUMES--
  VOL001
--DEVICE TYPES--
  3390
```

### Option 3 : LISTDS avec HISTORY
```
LISTDS 'FTEST.DEV.FILE' HISTORY
```
**Résultat** :
```
FTEST.DEV.FILE
--RECFM-FB--LRECL-80--BLKSIZE-27920
--CREATED 2025/001  EXPIRES 0000/000  LAST REFERENCE 2025/001
```

### Option 4 : LISTDS avec ALL
```
LISTDS 'FTEST.DEV.FILE' ALL
```
Affiche toutes les informations combinées.

### Option 5 : LISTDS sur un PDS avec MEMBERS
```
LISTDS 'SYS1.PROCLIB' MEMBERS
```
**Résultat** :
```
SYS1.PROCLIB
--RECFM-FB--LRECL-80--BLKSIZE-27920
--MEMBERS--
  ASMACL
  ASMACLEG
  ASMACLG
  ...
```

---

## Exercice 11 : Renommer un dataset créé avec ALLOC

**Objectif** : Renommer le dataset `FTEST.DEV.MODEL`

**Solution** :
```
RENAME 'FTEST.DEV.MODEL' 'FTEST.DEV.MODELE'
```

**Résultat attendu** :
```
IKJ56701I DSNAME CHANGED TO FTEST.DEV.MODELE
```

**Vérification** :
```
LISTCAT LEVEL(FTEST.DEV)
```
Doit afficher `FTEST.DEV.MODELE` au lieu de `FTEST.DEV.MODEL`

---

## Exercice 12 : Message à l'opérateur

**Objectif** : Envoyer un message à la console opérateur

**Solution** :
```
SEND 'DEMARRAGE PRODUCTION PREVU A 22H00' OPERATOR
```

**Résultat attendu** :
```
IKJ56701I MESSAGE SENT TO OPERATOR
```

Le message apparaît sur la console opérateur :
```
*01 FTEST    DEMARRAGE PRODUCTION PREVU A 22H00
```

**Variante avec l'heure actuelle** :
```
SEND 'PRODUCTION DEMARREE - HEURE: 22:00:00' OPERATOR
```

---

## Exercice 13 : Renommer DEV.FILE en DEV.COMPTA

**Objectif** : Renommer `FTEST.DEV.FILE` en `FTEST.DEV.COMPTA`

**Solution** :
```
RENAME 'FTEST.DEV.FILE' 'FTEST.DEV.COMPTA'
```

**Résultat attendu** :
```
IKJ56701I DSNAME CHANGED TO FTEST.DEV.COMPTA
```

**Vérification** :
```
LISTDS 'FTEST.DEV.COMPTA'
```

---

## Exercice 14 : Lister les caractéristiques de PROD.LIBTEST

**Objectif** : Afficher les attributs du dataset `FTEST.PROD.LIBTEST`

**Note** : Si vous avez effectué l'exercice 8, ce dataset a été supprimé.
Recréez-le d'abord :
```
ALLOC DA('FTEST.PROD.LIBTEST') NEW CATALOG +
      DSORG(PO) RECFM(F) LRECL(80) BLKSIZE(80) +
      SPACE(2,1) TRACKS DIR(5)
```

**Solution** :
```
LISTDS 'FTEST.PROD.LIBTEST' ALL
```

**Résultat attendu** :
```
FTEST.PROD.LIBTEST
--RECFM-F--LRECL-80--BLKSIZE-80
--VOLUMES--
  VOL001
--DEVICE TYPES--
  3390
--CREATED 2025/001  EXPIRES 0000/000  LAST REFERENCE 2025/001
```

---

## Récapitulatif des datasets créés

| Dataset | Type | RECFM | LRECL | Statut final |
|---------|------|-------|-------|--------------|
| FTEST.PROD.LIBTEST | PO | F | 80 | Supprimé (Ex.8) puis recréé (Ex.14) |
| FTEST.DEV.MODEL | PS | FB | 80 | Renommé en FTEST.DEV.MODELE (Ex.11) |
| FTEST.DEV.FILE | PS | FB | 80 | Renommé en FTEST.DEV.COMPTA (Ex.13) |
| FTEST.PROD.FILE | PS | FB | 80 | Existe |
| FTEST.EXPLOI.FILE | PS | FB | 80 | Supprimé (Ex.9) |

---

## Nettoyage (optionnel)

Pour supprimer tous les datasets créés :
```
DELETE 'FTEST.PROD.LIBTEST'
DELETE 'FTEST.DEV.MODELE'
DELETE 'FTEST.DEV.COMPTA'
DELETE 'FTEST.PROD.FILE'
```

---

## Aide-mémoire des commandes utilisées

```
┌─────────────────────────────────────────────────────────────────┐
│                    COMMANDES UTILISÉES                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  LOGON userid                                                   │
│  Connexion à TSO                                                │
│                                                                  │
│  ALLOC DA('dsn') NEW CATALOG DSORG(xx) RECFM(xx) LRECL(xx)     │
│  Création d'un dataset                                          │
│                                                                  │
│  ALLOC DA('dsn') NEW CATALOG LIKE('model')                     │
│  Création selon un modèle                                       │
│                                                                  │
│  DELETE 'dsn'                                                   │
│  Suppression d'un dataset                                       │
│                                                                  │
│  LISTCAT LEVEL(prefix)                                          │
│  Liste les datasets d'un préfixe                               │
│                                                                  │
│  LISTDS 'dsn' [STATUS|MEMBERS|HISTORY|ALL]                     │
│  Affiche les attributs d'un dataset                            │
│                                                                  │
│  RENAME 'old' 'new'                                             │
│  Renomme un dataset                                             │
│                                                                  │
│  SEND 'message' OPERATOR                                        │
│  Envoie un message à la console                                │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```
