# Exercice 1 : Definition du Data Set CLIENT dans CICS

## Objectif

Definir le Data Set CLIENT dans la procedure de demarrage de CICS et comme ressource VSAM a utiliser par les programmes. Les operations de lecture, ecriture et suppression seront autorisees sur ce Data Set.

## Etapes

### 1. Creation du fichier VSAM (IDCAMS)

Executer le JCL `DEFVSAM.jcl` pour creer le cluster VSAM KSDS.

**Parametres du cluster :**

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| NAME | ROCHA.CICS.CLIENT | Nom du cluster |
| INDEXED | - | Type KSDS (Key Sequenced) |
| VOLUMES(FDDBAS) | - | Volume DASD cible (TK4-) |
| KEYS(6 0) | 6 octets, position 0 | Cle = numero compte |
| RECORDSIZE(80 80) | 80 octets fixe | Longueur enregistrement (64 donnees + 16 filler) |
| TRACKS(5 5) | 5 prim, 5 sec | Espace alloue |
| FREESPACE(20 10) | 20% CI, 10% CA | Espace libre pour insertions |
| SHAREOPTIONS(2 3) | - | Partage cross-region/system |

### 2. Integration dans CICS (FCT)

Pour que CICS puisse acceder au fichier VSAM, il faut le declarer dans la **FCT (File Control Table)**.

#### Methode 1 : Via CEDA (en ligne)

```
CEDA DEFINE FILE(FCLIENT) GROUP(CLIGROUP)
     DSNAME(ROCHA.CICS.CLIENT)
     ADD(YES)
     BROWSE(YES)
     DELETE(YES)
     READ(YES)
     UPDATE(YES)
     RECORDFORMAT(F)
     RECORDSIZE(80)
     KEYLENGTH(6)
     STATUS(ENABLED)
     OPENTIME(FIRSTREF)

CEDA INSTALL FILE(FCLIENT) GROUP(CLIGROUP)
```

**Explication des parametres :**

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| FILE | FCLIENT | Nom logique dans CICS (8 car max) |
| DSNAME | ROCHA.CICS.CLIENT | Nom physique du dataset |
| ADD(YES) | - | Autoriser WRITE (ajout) |
| BROWSE(YES) | - | Autoriser STARTBR/READNEXT |
| DELETE(YES) | - | Autoriser DELETE |
| READ(YES) | - | Autoriser READ |
| UPDATE(YES) | - | Autoriser REWRITE |
| RECORDFORMAT(F) | - | Enregistrements fixes |
| RECORDSIZE(80) | - | Taille enregistrement (64 donnees + 16 filler) |
| KEYLENGTH(6) | - | Longueur de la cle |
| STATUS(ENABLED) | - | Fichier actif |
| OPENTIME(FIRSTREF) | - | Ouverture au premier acces |

#### Methode 2 : Via macro assembleur (batch)

```
DFHFCT TYPE=FILE,
       FILE=FCLIENT,
       DATASET=ROCHA.CICS.CLIENT,
       ACCMETH=VSAM,
       SERVREQ=(READ,UPDATE,DELETE,ADD,BROWSE),
       RECFORM=FIXED,
       LRECL=80,
       KEYLEN=6,
       FILSTAT=(ENABLED,OPENED)
```

### 3. Verification

Apres installation, verifier avec CEMT :

```
CEMT INQUIRE FILE(FCLIENT)
```

Resultat attendu :
```
FILE(FCLIENT)   Dsn(ROCHA.CICS.CLIENT)
                Ena Ope Rea Upd Add Bro Del
                Vsam Ksds
```

## Fichiers

- `DEFVSAM.jcl` : JCL de creation du cluster VSAM
- `LOADVSAM.jcl` : JCL de chargement des donnees initiales
- `CLIENT.dat` : Fichier de donnees (reference)

---

## 4. Chargement des donnees initiales

Apres la creation du cluster et l'integration dans CICS, charger les donnees de test avec `LOADVSAM.jcl`.

### Donnees chargees

**Clients de base (000001-000010) :**

| NumCpt | Region | Nom | Position | Usage |
|--------|--------|-----|----------|-------|
| 000001 | 01 (Paris) | DUPONT | CR | Tests CRUD |
| 000002 | 01 (Paris) | MARTIN | DB | Tests CRUD |
| 000003 | 02 (Marseille) | BERNARD | CR | Tests CRUD |
| 000004 | 02 (Marseille) | PETIT | DB | Tests CRUD |
| 000005 | 03 (Lyon) | ROBERT | CR | Tests CRUD |
| 000006 | 03 (Lyon) | RICHARD | DB | Tests CRUD |
| 000007 | 04 (Lille) | DURAND | CR | Tests CRUD |
| 000008 | 04 (Lille) | MOREAU | DB | Tests CRUD |
| 000009 | 01 (Paris) | LAURENT | CR | Tests CRUD |
| 000010 | 02 (Marseille) | SIMON | DB | Tests CRUD |

**Clients generiques 222xxx (pour exercice 18 - READNEXT) :**

| NumCpt | Region | Nom | Position |
|--------|--------|-----|----------|
| 222001 | 01 (Paris) | LEROY | CR |
| 222002 | 01 (Marseille) | ROUX | DB |
| 222003 | 02 (Lyon) | DAVID | CR |
| 222004 | 03 (Lille) | BERTRAND | DB |
| 222005 | 04 (Paris) | MOREL | CR |

### Repartition pour exercice 19 (statistiques)

| Region | Nb Clients | Debiteurs | Crediteurs |
|--------|------------|-----------|------------|
| 01 Paris | 5 | 1 | 4 |
| 02 Marseille | 4 | 2 | 2 |
| 03 Lyon | 3 | 1 | 2 |
| 04 Lille | 3 | 2 | 1 |

### Clients a creer manuellement (exercice 16)

Les clients suivants seront crees via la transaction AJOU :
- **111001 a 111005** : 5 clients (seront supprimes exercice 17)
- **444001 a 444005** : 5 clients
- **777001 a 777005** : 5 clients

## Commandes utiles

```
# Lister le contenu du fichier (vide au depart)
CEMT SET FILE(FCLIENT) OPEN
CEMT SET FILE(FCLIENT) ENABLED

# Fermer le fichier (pour maintenance)
CEMT SET FILE(FCLIENT) CLOSED

# Verifier l'etat
CEMT INQ FILE(FCLIENT)
```

## Captures d'ecran attendues

1. Execution JCL DEFVSAM (RC=0)
2. LISTCAT du cluster cree
3. CEDA DEFINE FILE
4. CEDA INSTALL FILE
5. CEMT INQUIRE FILE (verification)
