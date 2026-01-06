# Chapitre VII - GDG et Codes Retour VSAM

## VII-1. Generation Data Groups (GDG)

### Definition

Un GDG (Generation Data Group) est un groupe de Data Sets lies par un nom commun, utilises pour collecter des données periodiques (journalier, hebdomadaire, mensuel).

### Structure d'un GDG

```
Base GDG: TESTGDG.PAIE.MENSUEL
     |
     +-- TESTGDG.PAIE.MENSUEL.G0001V00  (Generation 1)
     +-- TESTGDG.PAIE.MENSUEL.G0002V00  (Generation 2)
     +-- TESTGDG.PAIE.MENSUEL.G0003V00  (Generation 3)
     +-- ...

G: Numero de generation (0000-9999)
V: Numero de version (00-99)
```

### Référencement Relatif

| Référence | Signification |
|-----------|---------------|
| `GDG.BASE(0)` | Generation actuelle (la plus recente) |
| `GDG.BASE(+1)` | Nouvelle generation a créer |
| `GDG.BASE(-1)` | Generation precedente |
| `GDG.BASE(-2)` | Avant-derniere generation |

### Exemple de Référencement

```jcl
//STEP1    EXEC PGM=MYPROG
//INPUT    DD DSN=TESTGDG.PAIE.MENSUEL(0),DISP=SHR      <- Lire actuelle
//OUTPUT   DD DSN=TESTGDG.PAIE.MENSUEL(+1),DISP=(...    <- Creer nouvelle
//OLDDATA  DD DSN=TESTGDG.PAIE.MENSUEL(-1),DISP=SHR     <- Lire precedente
```

---

## VII-2. DEFINE GENERATIONDATAGROUP

### Syntaxe

```jcl
DEFINE GENERATIONDATAGROUP (
  NAME(entryname)                    /* Max 35 caracteres */
  LIMIT(limit)                       /* Max 255 generations */
  [EMPTY|NOEMPTY]
  [SCRATCH|NOSCRATCH]
  [TO(date)|FOR(days)]
) -
[CATALOG(catname)]
```

### Parametres

| Parametre | Description |
|-----------|-------------|
| **NAME** | Nom de la base GDG (max 35 caracteres) |
| **LIMIT** | Nombre max de generations en ligne (1-255) |
| **EMPTY** | Supprime TOUTES les generations quand limite atteinte |
| **NOEMPTY** | Supprime uniquement la plus ancienne generation |
| **SCRATCH** | Supprime entrée VTOC quand generation decataloguee |
| **NOSCRATCH** | Conserve entrée VTOC |

### EMPTY vs NOEMPTY

```
Avec LIMIT(3) et NOEMPTY:
+----------------------------------------------------------+
| Avant creation G0004:                                     |
| G0001, G0002, G0003 (3 generations)                       |
|                                                           |
| Apres creation G0004:                                     |
| G0002, G0003, G0004 (G0001 supprimee)                     |
+----------------------------------------------------------+

Avec LIMIT(3) et EMPTY:
+----------------------------------------------------------+
| Avant creation G0004:                                     |
| G0001, G0002, G0003 (3 generations)                       |
|                                                           |
| Apres creation G0004:                                     |
| G0004 uniquement (toutes les anciennes supprimees)        |
+----------------------------------------------------------+
```

### Exemple de Creation

```jcl
//DEFGDG   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG (NAME(TESTGDG.PAIE.MENSUEL) -
    LIMIT(5) -
    NOEMPTY -
    SCRATCH)
  IF LASTCC = 0 THEN -
    LISTCAT ENTRIES(TESTGDG.PAIE.MENSUEL) ALL
/*
```

---

## VII-3. Creation d'une Generation

### Syntaxe JCL

```jcl
//GENGDG   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
00001CLIENT1        DB  1000CR  3500JUIN 2025
00005CLIENT2        DB 50000CR 13500JUIN 2025
00003CLIENT3        DB25000000DB  3500JUIN 2025
/*
//SYSUT2   DD DSN=TESTGDG.PAIE.MENSUEL(+1),
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,
//            SPACE=(TRK,(1,1)),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0)
//SYSIN    DD DUMMY
```

### Points Importants

- `(+1)` créé une nouvelle generation
- La generation recoit automatiquement le suffixe G####V00
- DISP doit etre (NEW,CATLG) pour cataloguer la generation
- Plusieurs `(+1)` dans le meme job créént des generations distinctes

---

## VII-4. ALTER GDG

### Syntaxe

```jcl
ALTER gdg-base-name
  [EMPTY|NOEMPTY]
  [SCRATCH|NOSCRATCH]
  [LIMIT(new-limit)]
  [ADDVOLUMES(volser...)]
  [REMOVEVOLUMES(volser...)]
  [TO(date)|FOR(days)]
```

### Exemple

```jcl
//ALTERGDG EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  ALTER TESTGDG.PAIE.MENSUEL -
    NOSCRATCH -
    EMPTY -
    LIMIT(10)
/*
```

---

## VII-5. DELETE GDG

### Supprimer uniquement l'index GDG

```jcl
//DELGDG   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (TESTGDG.PAIE.MENSUEL) GDG PURGE
/*
```

> **Note :** Les generations existantes restent cataloguees.

### Supprimer index + toutes les generations

```jcl
//DELGDGF  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE (TESTGDG.PAIE.MENSUEL) GDG FORCE
/*
```

---

## VII-6. Concatenation des Generations

### Lire toutes les generations

```jcl
//READALL  EXEC PGM=MYPROG
//INPUT    DD DSN=TESTGDG.PAIE.MENSUEL,DISP=SHR
```

Sans numéro relatif, TOUTES les generations sont concatenees dans l'ordre inverse (plus recente en premier).

### Exemple avec REPRO

```jcl
//CONCAGDG JOB CONCAT,'GDG CONCAT'
//STEPCONC EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=TESTGDG.PAIE.MENSUEL,DISP=SHR
//SYSUT2   DD DSN=TESTGDG.PAIE.BACKUP,
//            DISP=(NEW,CATLG,DELETE),
//            UNIT=SYSDA,SPACE=(CYL,(2,2),RLSE),
//            DCB=(LRECL=80,RECFM=FB,BLKSIZE=0,DSORG=PS)
//SYSIN    DD *
  REPRO INFILE(SYSUT1) OUTFILE(SYSUT2)
/*
```

---

## VII-7. Codes Retour VSAM

### Codes Frequents

| Code | Description |
|------|-------------|
| 00 | Operation reussie |
| 02 | Clé AIX en double trouvee |
| 04 | Enregistrement de longueur fixe invalide |
| 05 | Fichier non present a l'OPEN |
| 10 | Fin de fichier atteinte |
| 14 | Lecture hors limites (RRDS) |
| 20 | Clé invalide (KSDS/RRDS) |
| 21 | Erreur de sequence ou changement de clé |
| 22 | Clé primaire en double |
| 23 | Enregistrement/fichier non trouve |
| 24 | Clé hors limites |
| 30 | Erreur E/S permanente |
| 34 | Enregistrement hors limites |
| 35 | Fichier non present a l'OPEN |
| 37 | OPEN avec mauvais mode |
| 38 | OPEN fichier verrouille |
| 39 | OPEN echoue - attributs conflictuels |
| 41 | OPEN fichier deja ouvert |
| 42 | CLOSE fichier non ouvert |
| 43 | REWRITE sans READ prealable |
| 44 | REWRITE longueur differente |
| 46 | READ au-dela fin de fichier |
| 47 | READ fichier non ouvert I-O/INPUT |
| 48 | WRITE fichier non ouvert I-O/OUTPUT |
| 49 | DELETE/REWRITE fichier non ouvert I-O |
| 91 | Echec mot de passe/autorisation |
| 92 | Erreur logique |
| 93 | Ressources non disponibles |
| 96 | Pas d'instruction DD pour le fichier |
| 97 | OPEN reussi, intégrité vérifiée |
| 98 | Fichier verrouille - OPEN echoue |
| 99 | Enregistrement verrouille |

### Codes par Categorie

#### Codes de Succes (00-04)

| Code | Signification |
|------|---------------|
| 00 | Succes complet |
| 02 | Doublon AIX (si NONUNIQUEKEY) |
| 04 | Longueur incorrecte pour fixe |

#### Codes de Fin (05-14)

| Code | Signification |
|------|---------------|
| 05 | Fichier optionnel non trouve |
| 10 | Fin de fichier (AT END) |
| 14 | Hors limites RRDS |

#### Codes de Clé (20-24)

| Code | Signification |
|------|---------------|
| 20 | Clé invalide |
| 21 | Sequence incorrecte |
| 22 | Doublon clé primaire |
| 23 | Enregistrement non trouve |
| 24 | Depassement limites |

#### Codes E/S (30-39)

| Code | Signification |
|------|---------------|
| 30 | Erreur E/S permanente |
| 34 | Depassement limites |
| 35 | Fichier non trouve (OPEN) |
| 37 | Mode incompatible |
| 38 | Fichier verrouille |
| 39 | Conflit attributs |

#### Codes Logiques (41-49)

| Code | Signification |
|------|---------------|
| 41 | Deja ouvert |
| 42 | Non ouvert (CLOSE) |
| 43 | REWRITE sans READ |
| 44 | Changement longueur |
| 46 | READ apres EOF |
| 47 | Mode incorrect (READ) |
| 48 | Mode incorrect (WRITE) |
| 49 | Mode incorrect (DELETE/REWRITE) |

#### Codes Systeme (91-99)

| Code | Signification |
|------|---------------|
| 91 | Autorisation refusee |
| 92 | Erreur logique VSAM |
| 93 | Ressources insuffisantes |
| 96 | DD manquante |
| 97 | OPEN OK, VERIFY effectue |
| 98 | Verrouillage fichier |
| 99 | Verrouillage enregistrement |

---

## VII-8. Calcul de Taille d'un Data Set

### Formule

```
Taille (octets) = Nombre_enregistrements x Taille_enregistrement
Taille (tracks) = Taille (octets) / Taille_track_du_disque
```

### Exemple de Calcul

**Données :**
- Taille enregistrement : 80 caracteres
- Nombre estime : 5000 enregistrements initiaux
- Evolution : 1000 enregistrements/mois
- Disque : 3390-3 (Track = 56,664 octets)

**Calcul :**

```
Primaire = 5000 x 80 = 400,000 octets
Secondaire = 1000 x 80 = 80,000 octets

Primaire (tracks) = 400,000 / 56,664 = 7.06 tracks -> 9 tracks (marge)
Secondaire (tracks) = 80,000 / 56,664 = 1.41 tracks -> 3 tracks
```

**Resultat :**

```jcl
DEFINE CLUSTER (...
  TRACKS(9 3)
  ...
```

---

## Résumé du Chapitre

| Concept | Description |
|---------|-------------|
| **GDG** | Groupe de generations liees par un nom commun |
| **LIMIT** | Nombre max de generations (1-255) |
| **EMPTY** | Vide toutes les generations quand limite atteinte |
| **NOEMPTY** | Supprime uniquement la plus ancienne |
| **(0)** | Generation actuelle |
| **(+1)** | Nouvelle generation |
| **(-1)** | Generation precedente |
| **Code 00** | Succes |
| **Code 10** | Fin de fichier |
| **Code 22** | Doublon clé |
| **Code 23** | Non trouve |

---

## Aide-Memoire

```
GDG:
- LIMIT(n): Max n generations
- EMPTY: Vide tout a la limite
- NOEMPTY: Supprime la plus vieille
- SCRATCH: Efface VTOC aussi

Références:
- (0) = actuelle
- (+1) = nouvelle
- (-1) = precedente
- (sans) = toutes concatenees

Codes retour critiques:
- 00 = OK
- 10 = Fin fichier
- 22 = Doublon clé
- 23 = Non trouve
- 35 = Fichier inexistant
- 39 = Conflit attributs

Calcul espace:
- Taille = Nb_enreg x Longueur_enreg
- Tracks = Taille / 56664 (3390)
- Ajouter 20-30% de marge
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI - Analyse de la Sortie LISTCAT](06-analyse-listcat.md) | [Module VSAM](README.md) |

---
*Formation VSAM - M2i Formation*
