# Suivi des Ameliorations - Exercices CICS Pratique

> Fichier de suivi pour reprendre le travail si la conversation est interrompue.

## Statut Global

| Phase | Statut |
|-------|--------|
| Exercices Presentation (TEST, IO, IO1) | TERMINE |
| Exercices VSAM (READ, WRITE, REWRITE, DELETE) | TERMINE |
| Exercices BROWSE (PGSTART, PGPATHF) | TERMINE |
| Ameliorations BMS | TERMINE |
| JCL | OK (aucune correction) |

---

## PHASE 1 : PRESENTATION (TERMINE)

### Fichiers corriges

| Fichier | Corrections appliquees |
|---------|------------------------|
| `bms/MAPTEST.bms` | TYPE=DSECT, MAPATTS/DSATTS, couleurs, ligne instruction |
| `bms/MAPIO.bms` | TYPE=DSECT, nom sur TYPE=FINAL |
| `bms/MAPIO1.bms` | CHPOUT: UNPROT->PROT, PICOUT corrige |
| `cobol/PROGIO.cbl` | Double SEND->DATAONLY seul, supprime STOP RUN |
| `cobol/PROGIO1.cbl` | Supprime STOP RUN |

---

## PHASE 2 : EXERCICES VSAM (TERMINE)

### 2.1 PROGREAD (READ) - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| COPY DFHAID | Ajoute pour gestion touches |
| WS-RESPCODE | Gestion des codes retour CICS |
| Gestion touches | PF3/CLEAR pour quitter (EIBAID) |
| Validation saisie | SPACES, LOW-VALUES, NUMERIC |
| RESP() sur READ | Evite ABEND si cle inexistante |
| ERREUR-PARA | Affichage propre des erreurs |
| STOP RUN | Supprime |

### 2.2 PRGWRIT (WRITE) - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| Structure paragraphes | MAIN-PARA, TRAITER-SAISIE, DEMANDER-CONTINUER, FIN-PROGRAM |
| COPY DFHAID | Gestion touches |
| WS-CONTINUER | Variable propre de controle |
| Validation saisie | Code client obligatoire |
| Verification WRITE | Message si erreur apres WRITE |
| Messages clairs | "CLIENT CREE OK", "CLIENT EXISTE DEJA" |
| STOP RUN | Supprime |

### 2.3 PRGREWR (REWRITE) - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| Structure paragraphes | TRAITER-MODIFICATION, AFFICHER-DONNEES, DEMANDER-CONTINUER |
| COPY DFHAID | Gestion touches |
| Validation saisie | Code client obligatoire |
| Gestion NOTFND | Message si client inexistant |
| Verification REWRITE | Message si erreur |
| STOP RUN | Supprime |

### 2.4 PRGRDEL (DELETE) - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| COPY DFHAID | Gestion touches |
| Validation saisie | Code obligatoire et numerique |
| **CONFIRMATION** | Affiche donnees, demande ENTER pour confirmer |
| UNLOCK | Libere le verrou si annulation (PF3) |
| Messages clairs | "CONFIRMER SUPPRESSION?", "SUPPRESSION ANNULEE" |
| STOP RUN | Supprime |

---

## PHASE 3 : BROWSE (TERMINE)

### 3.1 PGSTART.cbl - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| HANDLE AID -> EIBAID | Methode moderne apres RECEIVE |
| Validation saisie | Cle de depart obligatoire |
| Double SEND simplifie | Un seul SEND avec ERASE FREEKB |
| Gestion touches boucle | PF3/CLEAR dans la boucle de lecture |
| STOP RUN | Supprime |
| Paragraphes structures | FIN-BROWSE, AFFICHER-MESSAGE, FIN-PROGRAM |

### 3.2 PGPATHF.cbl - CORRIGE

| Amelioration | Description |
|--------------|-------------|
| HANDLE AID -> EIBAID | Methode moderne |
| IGNORE CONDITION | Supprime (gestion explicite) |
| Validation saisie | Nom client obligatoire |
| Gestion DUPKEY | Conservee (normale pour AIX) |
| STOP RUN | Supprime |

---

## PHASE 4 : AMELIORATIONS BMS (TERMINE)

### 4.1 MAPREAD.bms - AMELIORE

| Amelioration | Description |
|--------------|-------------|
| Titre MAP1 | "=== CONSULTATION CLIENT ===" en TURQUOISE |
| Instructions MAP1 | "ENTER=Rechercher | PF3=Quitter | CLEAR=Annuler" |
| CDECLT2 dans MAP2 | Nouveau champ CODE CLIENT en WHITE |
| SOLDE en YELLOW | Mise en valeur du solde |
| Instructions MAP2 | "ENTER=Suivant | PF3=Quitter | CLEAR=Annuler" |

### 4.2 MAPWRIT.bms - AMELIORE

| Amelioration | Description |
|--------------|-------------|
| Titres | "=== GESTION CLIENT ===" et "=== DONNEES CLIENT ===" |
| Stoppers | Champ PROT apres chaque saisie |
| PICIN/PICOUT | Sur SOLDE pour formatage numerique |
| Message en RED | MSGINF en couleur rouge |
| Instructions | En bas de chaque MAP |

### 4.3 MAPPATH.bms - AMELIORE

| Amelioration | Description |
|--------------|-------------|
| Titre | "=== RECHERCHE PAR NOM (INDEX ALT) ===" |
| NOM en WHITE | Mise en valeur de la cle recherchee |
| SOLDE en YELLOW | Coherence avec autres MAPs |
| Instructions | Dans MAP1 et MAP2 |

### 4.4 MAPNAME.bms - AMELIORE

| Amelioration | Description |
|--------------|-------------|
| TYPE=DSECT | Correction TYPE=MAP vers TYPE=DSECT |
| TYPE=FINAL | Ajout nom MAPNAME sur TYPE=FINAL |

### 4.5 MDELG.bms - AMELIORE

| Amelioration | Description |
|--------------|-------------|
| Titre | "=== SUPPRESSION GENERIQUE DE RECORD ===" en TURQUOISE |
| Faute corrigee | IDENTIFINAT -> IDENTIFIANT |
| HILIGHT=REVERSE | Sur champs de saisie VALKEY et LENKEY |
| IC sur VALKEY | Curseur initial |
| NBRDEL en YELLOW | Mise en valeur du compteur |
| Instructions | "ENTER=Supprimer | PF3=Quitter | CLEAR=Annuler" |

### 4.6 PROGREAD.cbl - MISE A JOUR

| Amelioration | Description |
|--------------|-------------|
| CDECLT2O | Utilisation du nouveau champ MAP2 |

---

## PHASE 5 : CORRECTIONS FINALES

| Fichier | Correction |
|---------|------------|
| PRGRGEN.cbl | STOP RUN supprime, CDECLTO -> CDECLT2O |
| PRWRSPL.cbl | STOP RUN supprime |
| PDELGEN.cbl | STOP RUN supprime |
| PRGRDEL.cbl | CDECLTO -> CDECLT2O |
| PGSTART.cbl | CDECLTO -> CDECLT2O |
| PROGNAME.cbl | GOBACK supprime |

---

## PHASE 6 : JCL (VERIFIE - AUCUNE CORRECTION)

Tous les JCL sont bien structures et documentes :

- [x] `jcl/ASSBLMAP.jcl` - OK (DFHMAPS, parametres corrects)
- [x] `jcl/COMPPGR.jcl` - OK (DFHYITVL, DFHELII inclus)
- [x] `jcl/DEFVSAM.jcl` - OK (SET MAXCC=0, bon DEFINE)
- [x] `jcl/DEFPATH.jcl` - OK (AIX + BLDINDEX + PATH + LISTCAT)
- [x] `jcl/LOADDATA.jcl` - OK (structure documentee)

---

## Resume des ameliorations communes appliquees

### 1. Gestion des touches (EIBAID au lieu de HANDLE AID)

```cobol
       COPY DFHAID.
       ...
       EXEC CICS RECEIVE MAP(...) END-EXEC.

       IF EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
           GO TO FIN-PROGRAM
       END-IF.
```

### 2. Validation de saisie

```cobol
       IF CDECLTI = SPACES OR CDECLTI = LOW-VALUES
           MOVE 'ERREUR: VEUILLEZ SAISIR UN CODE' TO WS-MESSAGE
           GO TO ERREUR-PARA
       END-IF.

       IF CDECLTI NOT NUMERIC
           MOVE 'ERREUR: CODE DOIT ETRE NUMERIQUE' TO WS-MESSAGE
           GO TO ERREUR-PARA
       END-IF.
```

### 3. Gestion RESP sur commandes VSAM

```cobol
       EXEC CICS READ FILE('FCLIENT')
           INTO(WS-REC-DATA)
           RIDFLD(WS-KEY)
           RESP(WS-RESPCODE)
       END-EXEC.

       IF WS-RESPCODE = DFHRESP(NOTFND)
           MOVE 'ENREGISTREMENT NON TROUVE' TO WS-MESSAGE
           GO TO ERREUR-PARA
       END-IF.

       IF WS-RESPCODE NOT = DFHRESP(NORMAL)
           MOVE 'ERREUR LECTURE FICHIER' TO WS-MESSAGE
           GO TO ERREUR-PARA
       END-IF.
```

### 4. Structure paragraphes propre

```cobol
       MAIN-PARA.
           ...
           GO TO FIN-PROGRAM.

       ERREUR-PARA.
           EXEC CICS SEND TEXT FROM(WS-MESSAGE) ... END-EXEC.
           GO TO FIN-PROGRAM.

       FIN-PROGRAM.
           EXEC CICS RETURN END-EXEC.
```

---

## Fichiers modifies

### BMS (8 fichiers)

- `bms/MAPTEST.bms`
- `bms/MAPIO.bms`
- `bms/MAPIO1.bms`
- `bms/MAPREAD.bms`
- `bms/MAPWRIT.bms`
- `bms/MAPPATH.bms`
- `bms/MAPNAME.bms`
- `bms/MDELG.bms`

### COBOL (13 fichiers)

- `cobol/PROGIO.cbl`
- `cobol/PROGIO1.cbl`
- `cobol/PROGREAD.cbl`
- `cobol/PRGWRIT.cbl`
- `cobol/PRGREWR.cbl`
- `cobol/PRGRDEL.cbl`
- `cobol/PGSTART.cbl`
- `cobol/PGPATHF.cbl`
- `cobol/PRGRGEN.cbl`
- `cobol/PRWRSPL.cbl`
- `cobol/PDELGEN.cbl`
- `cobol/PROGNAME.cbl`
- `cobol/PROGTEST.cbl` (non modifie)

---

*Derniere mise a jour : Toutes les phases terminees*
