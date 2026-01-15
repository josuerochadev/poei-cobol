# Partie 2c : Operations de Suppression (DELETE)

[< Partie 2b : Mise a jour](04-partie-2b-maj.md) | [Retour au sommaire](00-introduction.md) | [Partie 3 : Avancees >](06-partie-3-avancees.md)

---

Cette section couvre les exercices 12 a 15 : MAP de suppression, programme de suppression avec la commande DELETE, et deux variantes de transaction.

## Comparaison des commandes CICS pour suppression

| Commande | Usage | Prerequis |
|----------|-------|-----------|
| **DELETE** simple | Supprime directement par la cle | Aucun |
| **READ + DELETE** | Affiche avant suppression | READ pour confirmation visuelle |

---

## Exercice 12 : MAP pour suppression

### Enonce

Creer ou adapter la MAP precedente pour une operation de suppression de CLIENT dans le Data Set CLIENT.

### Mon travail

J'ai cree une nouvelle MAP de suppression (CLISUP) qui combine les caracteristiques des MAPs precedentes :

1. **Zone de saisie** : Le numero de compte est saisissable (UNPROT) pour la recherche
2. **Zones d'affichage** : Toutes les donnees client sont en lecture seule (ASKIP,BRT) pour confirmation visuelle avant suppression
3. **Zone de confirmation** : Un champ CONFIRM (O/N) permet de valider ou annuler la suppression

**Differences avec les autres MAPs :**

| Aspect | CLIAFF (Affichage) | CLIAJT (Ajout) | CLIMAJ (Maj) | CLISUP (Suppression) |
|--------|-------------------|----------------|--------------|---------------------|
| NUMCPT | UNPROT (saisie) | UNPROT (saisie) | UNPROT->ASKIP | UNPROT (saisie) |
| Autres champs | ASKIP (affichage) | UNPROT (saisie) | UNPROT (modif) | ASKIP (affichage) |
| Confirmation | Non | Non | Non | Oui (O/N) |
| Titre | AFFICHAGE | AJOUT | MISE A JOUR | SUPPRESSION |

**Flux de suppression en 2 phases :**

```
Phase 1 (Recherche)           Phase 2 (Confirmation)
+------------------------+    +------------------------+
| NUMCPT: ______ [saisie]|    | NUMCPT: 100001         |
| Autres: vides          |    | NOM: DUPONT            |
| CONFIRM: _             | -> | PRENOM: JEAN           |
|                        |    | ...                    |
| Message: Saisir numero |    | CONFIRM: _ [O/N]       |
+------------------------+    | Message: Confirmer ?   |
                              +------------------------+
```

### Resolution

**MAP BMS : CLISUP.bms**

Le code source est stocke dans `ROCHA.CICS.SOURCE(CLISUP)`. Voici le code complet :

```
***********************************************************************
*  MAPSET : CLISUP - Suppression Client
*  Transaction : SUPP / SULE
*  Fil Rouge CICS - Exercice 12
*
*  PARTICULARITE SUPPRESSION :
*  ---------------------------
*  Le numero de compte est saisi pour rechercher le client.
*  Les donnees sont affichees en lecture seule pour confirmation.
*  Un champ CONFIRM (O/N) permet de valider la suppression.
*
*  Deux modes d'utilisation :
*  - SUPP : Suppression directe (Ex 13-14)
*  - SULE : Suppression avec lecture prealable (Ex 15)
***********************************************************************
CLISUP   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPSUP   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** SUPPRESSION CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* ZONE DE SAISIE - NUMERO DE COMPTE (CLE)
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONES D'AFFICHAGE - DONNEES CLIENT (LECTURE SEULE)
*----------------------------------------------------------------------
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(6,25),LENGTH=20,ATTRB=ASKIP
LIBREG   DFHMDF POS=(6,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(7,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(7,25),LENGTH=20,ATTRB=ASKIP
LIBNAT   DFHMDF POS=(7,46),LENGTH=15,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(8,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(9,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(10,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(11,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(11,24),LENGTH=10,ATTRB=ASKIP
LIBSEX   DFHMDF POS=(11,35),LENGTH=8,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(12,19),LENGTH=2,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(13,19),LENGTH=1,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(13,24),LENGTH=10,ATTRB=ASKIP
LIBSIT   DFHMDF POS=(13,35),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(14,19),LENGTH=10,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(15,19),LENGTH=12,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(16,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(16,19),LENGTH=2,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(16,25),LENGTH=10,ATTRB=ASKIP
LIBPOS   DFHMDF POS=(16,36),LENGTH=10,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='CONFIRMER SUPPRESSION (O/N) :'
CONFIRM  DFHMDF POS=(18,33),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(18,35),LENGTH=1,ATTRB=ASKIP
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(20,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(21,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(21,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(23,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Rechercher/Confirmer  PF3=Quitter  CLEAR=X
               Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**Apercu de l'ecran MAPSUP :**

```
+------------------------------------------------------------------------------+
|                        *** SUPPRESSION CLIENT ***                            |
|------------------------------------------------------------------------------|
|                                                                              |
|  NUMERO COMPTE : ______                                                      |
|                                                                              |
|  CODE REGION   : __                     _______________                      |
|  NATURE COMPTE : __                     _______________                      |
|  NOM           : __________                                                  |
|  PRENOM        : __________                                                  |
|  DATE NAISSANCE: __________                                                  |
|  SEXE          : _         ________                                          |
|  ACTIVITE PRO  : __                                                          |
|  SITUATION SOC : _         ____________                                      |
|  ADRESSE       : __________                                                  |
|  SOLDE         : ____________                                                |
|  POSITION      : __         __________                                       |
|                                                                              |
|  CONFIRMER SUPPRESSION (O/N) : _                                             |
|                                                                              |
|------------------------------------------------------------------------------|
|  MESSAGE : ____________________________________________________________      |
|                                                                              |
|  ENTER=Rechercher/Confirmer  PF3=Quitter  CLEAR=Effacer                      |
+------------------------------------------------------------------------------+
```

**Zones de la MAP :**

| Zone | Longueur | Attribut | Description |
|------|----------|----------|-------------|
| NUMCPT | 6 | UNPROT,NUM,IC | Numero de compte (cle de recherche) |
| CODREG | 2 | ASKIP,BRT | Code region (affichage) |
| LIBREG | 15 | ASKIP,BRT | Libelle region |
| NATCPT | 2 | ASKIP,BRT | Nature compte (affichage) |
| LIBNAT | 15 | ASKIP,BRT | Libelle nature |
| NOM | 10 | ASKIP,BRT | Nom client (affichage) |
| PRENOM | 10 | ASKIP,BRT | Prenom client (affichage) |
| DATNA | 10 | ASKIP,BRT | Date naissance (affichage) |
| SEXE | 1 | ASKIP,BRT | Sexe (affichage) |
| LIBSEX | 8 | ASKIP,BRT | Libelle sexe |
| ACTPRO | 2 | ASKIP,BRT | Activite professionnelle |
| SITSO | 1 | ASKIP,BRT | Situation sociale |
| LIBSIT | 12 | ASKIP,BRT | Libelle situation |
| ADRESSE | 10 | ASKIP,BRT | Adresse (affichage) |
| SOLDE | 12 | ASKIP,BRT | Solde compte (affichage) |
| POSIT | 2 | ASKIP,BRT | Position (DB/CR) |
| LIBPOS | 10 | ASKIP,BRT | Libelle position |
| CONFIRM | 1 | UNPROT | Confirmation O/N (saisie) |
| MSG | 60 | ASKIP,BRT | Zone message |

**JCL d'assemblage : ASMSUP.jcl**

```jcl
//ROCHA12 JOB (ACCT),'ASSEMBL BMS CLISUP',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 12
//* ASSEMBLAGE DE LA MAP BMS CLISUP (SUPPRESSION CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Le copybook genere contiendra pour chaque champ :
//*   - NOMCPTx  ou x = I (input), O (output), L (longueur), A (attr)
//*   - CONFIRMI pour recevoir la confirmation O/N
//*
//* Prerequis :
//*   - Source BMS copie dans ROCHA.CICS.SOURCE(CLISUP)
//*   - Libraries ROCHA.CICS.* existantes
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//* ASSEMBLAGE DE LA MAP BMS
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLISUP',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLISUP),DISP=SHR
/*
//
```

### Definition CICS

```
CEDA DEFINE MAPSET(CLISUP) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLISUP) GROUP(CLIGROUP)
```

### Verification

```
CEDA VIEW MAPSET(CLISUP) GROUP(CLIGROUP)
```

### Utilisation

#### 1. Copier le source BMS dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CLISUP
Copier le contenu de CLISUP.bms
```

#### 2. Soumettre le JCL d'assemblage

```
ISPF 3.4 > ROCHA.CICS.JCL (ou ROCHA.CICS.SOURCE)
Edit member ASMSUP (copier ASMSUP.jcl)
SUB (submit)
```

#### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre CLISUP present dans ROCHA.CICS.LOAD
- Copybook CLISUP present dans ROCHA.CICS.LINK

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex12-1 : Source BMS dans ISPF EDIT - ROCHA.CICS.SOURCE(CLISUP)
2. pt2ex12-2 : Soumission JCL assemblage BMS ASMSUP
3. pt2ex12-3 : SDSF - Job output avec RC=0000
4. pt2ex12-4 : Verification ROCHA.CICS.LOAD - membre CLISUP present
5. pt2ex12-5 : CEDA DEFINE MAPSET(CLISUP)
6. pt2ex12-6 : CEDA VIEW MAPSET(CLISUP) - verification definition
-->

---

## Exercice 13 : Programme de suppression (DELETE)

### Enonce

Creer le PROGRAMME pour une operation de suppression d'un CLIENT dans le Data Set CLIENT en precisant le code CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Resolution

**Programme : PRGSUP.cbl**

```cobol
       2000-SUPPRIMER-CLIENT.
           MOVE NUMCPTI TO CLI-NUMCPT

      * Verification existence
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CLIENT INEXISTANT' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Suppression
           EXEC CICS DELETE
               FILE('FCLIENT')
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'ERREUR SUPPRESSION' TO MSGO
           END-IF.
```

### Captures d'ecran

<!-- ![pt2ex13-1](images-pt2/pt2ex13-1.png) -->

---

## Exercice 14 : Transaction de suppression

### Resolution

```
CEDA DEFINE TRANSACTION(SUPP) GROUP(CLIGROUP)
     PROGRAM(PRGSUP)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'ecran

<!-- ![pt2ex14-1](images-pt2/pt2ex14-1.png) -->

---

## Exercice 15 : Suppression avec lecture prealable

### Enonce

Reprendre cette operation de suppression en la precedant par une operation de lecture. Definir une transaction independante de la precedente.

### Mon travail

Cette version affiche d'abord les donnees du client avant de demander confirmation pour la suppression. Cela permet a l'utilisateur de verifier qu'il supprime le bon client.

### Resolution

**Programme : PRGSUL.cbl** (Suppression avec Lecture)

```cobol
       01 WS-PHASE             PIC X(01).
          88 PHASE-SAISIE      VALUE '1'.
          88 PHASE-CONFIRM     VALUE '2'.

       2000-TRAITEMENT.
           EVALUATE TRUE
               WHEN PHASE-SAISIE
                   PERFORM 3000-RECHERCHER-CLIENT
               WHEN PHASE-CONFIRM
                   PERFORM 4000-CONFIRMER-SUPPRESSION
           END-EVALUATE.

       3000-RECHERCHER-CLIENT.
      * Lecture et affichage du client
           EXEC CICS READ FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM AFFICHER-DONNEES
               MOVE 'CONFIRMER SUPPRESSION (O/N) ?' TO MSGO
               MOVE '2' TO WS-PHASE
           END-IF.

       4000-CONFIRMER-SUPPRESSION.
      * Si confirmation, suppression
           IF CONFIRMI = 'O'
               EXEC CICS DELETE FILE('FCLIENT')
                   RIDFLD(CLI-NUMCPT)
                   RESP(WS-RESP)
               END-EXEC
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'SUPPRESSION ANNULEE' TO MSGO
           END-IF
           MOVE '1' TO WS-PHASE.
```

**Transaction independante :**

```
CEDA DEFINE TRANSACTION(SULE) GROUP(CLIGROUP)
     PROGRAM(PRGSUL)
```

### Captures d'ecran

<!-- ![pt2ex15-1](images-pt2/pt2ex15-1.png) -->

---

[< Partie 2b : Mise a jour](04-partie-2b-maj.md) | [Retour au sommaire](00-introduction.md) | [Partie 3 : Avancees >](06-partie-3-avancees.md)
