# Exercices CICS - Chapitre VI

## Thème : Couche de Traitement - Commandes VSAM

Ce TP met en pratique les 4 commandes CICS de manipulation de fichiers VSAM : READ, WRITE, REWRITE et DELETE.

## Objectifs

- Maîtriser les commandes READ, WRITE, REWRITE, DELETE
- Comprendre la séquence READ UPDATE → REWRITE/DELETE
- Gérer les codes retour (RESP/RESP2)
- Intégrer présentation (BMS) et accès données (VSAM)

## Fichiers

```
chapitre-06/
├── bms/
│   └── TESTSET.bms       # Mapset commun
├── cobol/
│   ├── PROGREAD.cbl      # Exercice 1 : Lecture
│   ├── PROGWRIT.cbl      # Exercice 2 : Écriture
│   ├── PROGREWT.cbl      # Exercice 3 : Mise à jour
│   └── PROGDELT.cbl      # Exercice 4 : Suppression
├── copybooks/
│   └── MAPTEST.cpy       # Zone symbolique
├── jcl/
│   ├── DEFVSAM.jcl       # Définition fichier VSAM
│   └── LOADDATA.jcl      # Chargement données test
└── README.md
```

## Structure de données

### Fichier EMPLOYE (VSAM KSDS)

| Champ | PIC | Longueur | Description |
|-------|-----|----------|-------------|
| EMP-ID | X(6) | 6 | Clé primaire |
| EMP-NAME | X(30) | 30 | Nom de l'employé |
| EMP-DEPT | X(10) | 10 | Département |
| EMP-SALAIRE | 9(7)V99 | 9 | Salaire |
| EMP-ETAT-CRED | X(1) | 1 | Y=Crédit, N=Sans crédit |
| FILLER | X(24) | 24 | Réservé |

**Longueur totale** : 80 octets

### Copybook WS-EMPLOYE

```cobol
       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).
```

---

## Exercice 1 : Commande READ (PROGREAD)

### Objectif

Lire un enregistrement VSAM par sa clé.

### Transaction : `READ`

### Syntaxe

```cobol
       EXEC CICS READ
           FILE('EMPLOYE')
           INTO(WS-EMPLOYE)
           RIDFLD(WS-REC-KEY)
           RESP(WS-RESP)
       END-EXEC.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Lecture réussie |
| 13 | DFHRESP(NOTFND) | Enregistrement non trouvé |
| 12 | DFHRESP(FILENOTFOUND) | Fichier non défini |

### Test

1. Lancer la transaction : `READ`
2. Saisir une clé existante : `EMP001`
3. Vérifier l'affichage des données

---

## Exercice 2 : Commande WRITE (PROGWRIT)

### Objectif

Créer un nouvel enregistrement dans le fichier VSAM.

### Transaction : `WRIT`

### Syntaxe

```cobol
       EXEC CICS WRITE
           FILE('EMPLOYE')
           FROM(WS-EMPLOYE)
           RIDFLD(EMP-ID)
           RESP(WS-RESP)
       END-EXEC.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Écriture réussie |
| 14 | DFHRESP(DUPREC) | Clé déjà existante |
| 18 | DFHRESP(NOSPACE) | Plus d'espace |

### Programme fourni

Le programme `PROGWRIT.cbl` :
1. Initialise un enregistrement avec clé `EMP099`
2. Écrit l'enregistrement dans le fichier
3. Gère le cas de clé en double (DUPREC)

### Test

1. Lancer la transaction : `WRIT`
2. Message attendu : `ENREGISTREMENT CREE: EMP099`
3. Relancer : Message `ERREUR: CLE EMP099 EXISTE DEJA`

---

## Exercice 3 : Commande REWRITE (PROGREWT)

### Objectif

Mettre à jour un enregistrement existant.

### Transaction : `REWT`

### Règle importante

```
┌─────────────────────────────────────────────────────────────────────────┐
│  ATTENTION : REWRITE nécessite un READ UPDATE préalable !               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. READ FILE('XXX') ... UPDATE    ◄── Verrouille l'enregistrement     │
│  2. Modifications en mémoire                                            │
│  3. REWRITE FILE('XXX') ...        ◄── Écrit et déverrouille           │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Syntaxe

```cobol
      *    Lecture avec verrouillage
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               UPDATE
               RESP(WS-RESP)
           END-EXEC.

      *    Modification des données
           MOVE 'NOUVEAU NOM' TO EMP-NAME.
           ADD 1000 TO EMP-SALAIRE.

      *    Réécriture
           EXEC CICS REWRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RESP(WS-RESP)
           END-EXEC.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Mise à jour réussie |
| 16 | DFHRESP(INVREQ) | Pas de READ UPDATE préalable |

### Programme fourni

Le programme `PROGREWT.cbl` :
1. Lit l'enregistrement `EMP001` avec UPDATE
2. Modifie le nom et le salaire
3. Réécrit l'enregistrement

### Test

1. Lancer la transaction : `REWT`
2. Message attendu : `EMP001 MODIFIE AVEC SUCCES`
3. Vérifier avec `READ` que les données ont changé

---

## Exercice 4 : Commande DELETE (PROGDELT)

### Objectif

Supprimer un enregistrement du fichier VSAM.

### Transaction : `DELT`

### Deux méthodes

```
┌─────────────────────────────────────────────────────────────────────────┐
│  MÉTHODE 1 : DELETE direct                                              │
│  ────────────────────────                                               │
│  EXEC CICS DELETE FILE('XXX') RIDFLD(key) END-EXEC                     │
│                                                                          │
│  MÉTHODE 2 : Après READ UPDATE                                          │
│  ────────────────────────────                                           │
│  1. READ FILE('XXX') ... UPDATE                                        │
│  2. EXEC CICS DELETE FILE('XXX') END-EXEC  (pas de RIDFLD)            │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Syntaxe (méthode 2 - recommandée)

```cobol
      *    Lecture avec verrouillage
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               UPDATE
               RESP(WS-RESP)
           END-EXEC.

      *    Suppression
           EXEC CICS DELETE
               FILE('EMPLOYE')
               RESP(WS-RESP)
           END-EXEC.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Suppression réussie |
| 13 | DFHRESP(NOTFND) | Enregistrement non trouvé |

### Programme fourni

Le programme `PROGDELT.cbl` :
1. Lit l'enregistrement `EMP099` avec UPDATE
2. Supprime l'enregistrement
3. Affiche un message de confirmation

### Test

1. D'abord créer `EMP099` avec `WRIT`
2. Lancer la transaction : `DELT`
3. Message attendu : `EMP099 SUPPRIME AVEC SUCCES`
4. Relancer `DELT` : Message `ERREUR: EMP099 NON TROUVE`

---

## Tableau récapitulatif

| Commande | Prérequis | Usage |
|----------|-----------|-------|
| **READ** | - | Lecture simple |
| **READ UPDATE** | - | Lecture avec verrouillage |
| **WRITE** | - | Création nouvel enregistrement |
| **REWRITE** | READ UPDATE | Mise à jour enregistrement |
| **DELETE** | READ UPDATE (recommandé) | Suppression enregistrement |
| **UNLOCK** | READ UPDATE | Libérer sans modifier |

---

## Exercice 5 (Avancé) : Créer un programme CRUD complet

### Objectif

Créer un programme unique `PROGCRUD` qui gère les 4 opérations selon un code action.

### Spécifications

- Transaction : `CRUD`
- Code action en paramètre :
  - `C` = Create (WRITE)
  - `R` = Read
  - `U` = Update (REWRITE)
  - `D` = Delete

### Structure COMMAREA

```cobol
       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CREATE       VALUE 'C'.
               88  CA-READ         VALUE 'R'.
               88  CA-UPDATE       VALUE 'U'.
               88  CA-DELETE       VALUE 'D'.
           05  CA-RESP             PIC 9(4).
           05  CA-MESSAGE          PIC X(60).
           05  CA-EMPLOYE.
               10  CA-EMP-ID       PIC X(6).
               10  CA-EMP-NAME     PIC X(30).
               10  CA-EMP-DEPT     PIC X(10).
               10  CA-EMP-SALAIRE  PIC 9(7)V99.
```

---

## Prérequis

1. **Fichier VSAM EMPLOYE** défini et chargé
   - Voir [jcl/DEFVSAM.jcl](jcl/DEFVSAM.jcl)
   - Voir [jcl/LOADDATA.jcl](jcl/LOADDATA.jcl)

2. **Définitions CICS** :
   ```
   CEDA DEF FILE(EMPLOYE) GROUP(TESTGRP) ...
   CEDA DEF TRANSACTION(READ) GROUP(TESTGRP) PROGRAM(PROGREAD)
   CEDA DEF TRANSACTION(WRIT) GROUP(TESTGRP) PROGRAM(PROGWRIT)
   CEDA DEF TRANSACTION(REWT) GROUP(TESTGRP) PROGRAM(PROGREWT)
   CEDA DEF TRANSACTION(DELT) GROUP(TESTGRP) PROGRAM(PROGDELT)
   ```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Couche Présentation](../chapitre-05/) | [Chapitre VII - Couche Données](../chapitre-07/) |

---
*Formation COBOL - Module CICS - TP Chapitre VI*
