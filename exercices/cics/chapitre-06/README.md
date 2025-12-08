# Exercices CICS - Chapitre VI

## Thème : Couche de Traitement - Commande READ

Ce TP met en pratique la commande CICS READ pour lire un enregistrement dans un fichier VSAM KSDS.

## Objectifs

- Comprendre la structure d'un programme CICS avec MAP
- Maîtriser la commande READ et ses options
- Gérer les codes retour (RESP/RESP2)
- Intégrer présentation (BMS) et accès données (VSAM)

## Fichiers

```
chapitre-06/
├── bms/
│   └── TESTSET.bms       # Mapset contenant MAPTEST
├── cobol/
│   └── PROGTEST.cbl      # Programme principal
├── copybooks/
│   └── MAPTEST.cpy       # Zone symbolique de la MAP
└── README.md
```

## Architecture du programme

```
┌─────────────────────────────────────────────────────────────────┐
│                     PROGTEST - Architecture                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────────────┐  │
│  │  Terminal   │◄──►│   MAPTEST   │◄──►│     PROGTEST        │  │
│  │  3270       │    │   (BMS)     │    │                     │  │
│  └─────────────┘    └─────────────┘    │  ┌───────────────┐  │  │
│                                        │  │ RECEIVE MAP   │  │  │
│                                        │  │ READ FILE     │  │  │
│                                        │  │ SEND MAP      │  │  │
│                                        │  └───────┬───────┘  │  │
│                                        └──────────┼──────────┘  │
│                                                   │              │
│                                                   ▼              │
│                                        ┌─────────────────────┐  │
│                                        │  EMPLOYE (KSDS)     │  │
│                                        │  Clé: EMP-ID (6)    │  │
│                                        └─────────────────────┘  │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Structure de l'enregistrement EMPLOYE

| Champ | Type | Longueur | Description |
|-------|------|----------|-------------|
| EMP-ID | X(6) | 6 | Code employé (clé primaire) |
| EMP-NAME | X(30) | 30 | Nom de l'employé |
| EMP-DEPT | X(10) | 10 | Département |
| EMP-SALAIRE | 9(7)V99 COMP-3 | 5 | Salaire |
| EMP-ETAT-CRED | X(1) | 1 | Y=Crédit, N=Sans crédit |

**Longueur totale** : 52 octets

## Les 3 parties à intégrer (selon le support)

### Partie 1 : WORKING-STORAGE (structure données)

```cobol
      ******************************************************************
      * Structure du Data Set EMPLOYE (WS-REC-DATA)
      ******************************************************************
       01  WS-REC-DATA.
           05  EMP-ID               PIC X(6).
           05  EMP-NAME             PIC X(30).
           05  EMP-DEPT             PIC X(10).
           05  EMP-SALAIRE          PIC 9(7)V99 COMP-3.
           05  EMP-ETAT-CRED        PIC X(1).
               88  EMP-A-CREDIT     VALUE 'Y'.
               88  EMP-SANS-CREDIT  VALUE 'N'.
```

### Partie 2 : COPY de la MAP

```cobol
      ******************************************************************
      * Zone symbolique de la MAP
      ******************************************************************
       COPY MAPTEST.
```

### Partie 3 : Commande READ

```cobol
      ******************************************************************
      * Commande READ - Lecture par cle
      ******************************************************************
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-REC-DATA)
               RIDFLD(WS-REC-KEY)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 4000-AFFICHER-EMPLOYE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Employe non trouve' TO WS-MESSAGE
               WHEN OTHER
                   MOVE 'Erreur lecture' TO WS-MESSAGE
           END-EVALUATE.
```

## Codes retour READ (RESP)

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Lecture réussie |
| 12 | DFHRESP(FILENOTFOUND) | Fichier non défini |
| 13 | DFHRESP(NOTFND) | Enregistrement non trouvé |
| 19 | DFHRESP(NOTOPEN) | Fichier non ouvert |
| 22 | DFHRESP(DISABLED) | Fichier désactivé |

## Prérequis

1. **Fichier VSAM EMPLOYE** défini et chargé
   - Voir [tp-gestion-credits/jcl/DEFVSAM.jcl](../tp-gestion-credits/jcl/DEFVSAM.jcl)
   - Voir [tp-gestion-credits/jcl/LOADDATA.jcl](../tp-gestion-credits/jcl/LOADDATA.jcl)

2. **Définition FCT** dans CICS
   ```
   DEFINE FILE(EMPLOYE) GROUP(...)
          DSNAME(USER.CICS.EMPLOYE)
          STATUS(ENABLED) ...
   ```

3. **Transaction TEST** définie
   ```
   DEFINE TRANSACTION(TEST) GROUP(...)
          PROGRAM(PROGTEST)
   ```

## Compilation

### 1. Assembler le BMS
```jcl
//ASMMAP   EXEC PGM=DFHMAPS
//SYSIN    DD DSN=USER.SOURCE(TESTSET),DISP=SHR
//SYSLIN   DD DSN=USER.LOADLIB(TESTSET),DISP=SHR
```

### 2. Compiler le COBOL
```jcl
//COBCICS  EXEC DFHYITVL
//COBOL.SYSIN DD DSN=USER.SOURCE(PROGTEST),DISP=SHR
//LKED.SYSLMOD DD DSN=USER.LOADLIB(PROGTEST),DISP=SHR
```

## Test

1. Ouvrir une session CICS
2. Taper la transaction : `TEST`
3. Saisir un code employé (ex: `EMP001`)
4. Appuyer sur ENTER
5. Vérifier l'affichage des données
6. PF3 pour quitter

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Couche Présentation](../../../cours/cics/05-couche-presentation.md) | [Chapitre VII - Couche Données](../../../cours/cics/07-couche-donnees.md) |

---
*Formation COBOL - Module CICS - TP Chapitre VI*
