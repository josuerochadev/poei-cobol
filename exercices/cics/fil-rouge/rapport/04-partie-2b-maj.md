# Partie 2b : Operations de Mise a Jour (REWRITE)

[< Partie 2a : Ajout](03-partie-2a-ajout.md) | [Retour au sommaire](00-introduction.md) | [Partie 2c : Suppression >](05-partie-2c-suppression.md)

---

Cette section couvre les exercices 9 a 11 : MAP de mise a jour, programme de modification avec la commande REWRITE, et definition de la transaction MAJO.

## Difference WRITE vs REWRITE

| Aspect | WRITE (Ajout) | REWRITE (Mise a jour) |
|--------|---------------|----------------------|
| Client | Ne doit PAS exister | DOIT exister |
| Cle | Nouvelle | Existante (non modifiable) |
| Prerequis | Aucun | READ UPDATE obligatoire |
| Erreur typique | DUPREC (doublon) | NOTFND (inexistant) |

---

## Exercice 9 : MAP pour mise a jour

### Enonce

Creer ou adapter la MAP precedente pour une operation de mise a jour de CLIENT dans le Data Set CLIENT.

### Mon travail

La MAP de mise a jour differe de celle d'ajout par la **gestion dynamique des attributs** :

1. **Phase 1 (Recherche)** : Le numero de compte est saisissable (UNPROT) pour permettre la recherche du client
2. **Phase 2 (Affichage)** : Apres lecture du client, le numero de compte passe en lecture seule (ASKIP) car la cle d'un enregistrement VSAM ne peut pas etre modifiee
3. **Phase 3 (Modification)** : L'utilisateur modifie les autres champs et valide

Cette gestion dynamique se fait dans le programme COBOL via le **suffixe 'A'** (Attribut) du copybook genere :

```cobol
* Proteger le numero de compte apres affichage
MOVE DFHBMASK TO NUMCPTA
```

### Resolution

**MAP BMS : CLIMAJ.bms**

```
***********************************************************************
*  MAPSET : CLIMAJ - Mise a jour Client
*  Transaction : MAJO
*  Fil Rouge CICS - Exercice 9
*
*  PARTICULARITE MISE A JOUR :
*  ---------------------------
*  Le numero de compte est d'abord saisissable (recherche),
*  puis passe en lecture seule apres affichage des donnees.
*  Cette gestion dynamique des attributs se fait dans le programme
*  COBOL via le suffixe 'A' (ex: NUMCPTA pour modifier l'attribut).
***********************************************************************
CLIMAJ   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPMAJ   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,25),LENGTH=30,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** MISE A JOUR CLIENT ***'
         DFHMDF POS=(2,1),LENGTH=78,ATTRB=ASKIP,                        X
               INITIAL='------------------------------------------------X
               ------------------------------'
*----------------------------------------------------------------------
* NUMERO DE COMPTE - CHAMP CLE
* Commence en UNPROT pour la saisie initiale (recherche)
* Le programme passera l'attribut a ASKIP apres affichage
*----------------------------------------------------------------------
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(4,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(4,26),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(Cle - non modifiable)'
*----------------------------------------------------------------------
* ZONES DE SAISIE/MODIFICATION (meme structure que CLIAJT)
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(5,22),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='(01=PAR,02=MAR,03=LYO,04=LIL)'
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(6,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(6,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NOM           :'
NOM      DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(7,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(8,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(8,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(9,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(9,28),LENGTH=12,ATTRB=ASKIP,                       X
               INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SEXE          :'
SEXE     DFHMDF POS=(10,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(10,21),LENGTH=10,ATTRB=ASKIP,                      X
               INITIAL='(M ou F)'
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(11,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(11,22),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(12,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(12,21),LENGTH=15,ATTRB=ASKIP,                      X
               INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(13,19),LENGTH=10,ATTRB=UNPROT
         DFHMDF POS=(13,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(14,19),LENGTH=10,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(14,30),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(15,2),LENGTH=16,ATTRB=ASKIP,                       X
               INITIAL='POSITION      :'
POSIT    DFHMDF POS=(15,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(15,22),LENGTH=12,ATTRB=ASKIP,                      X
               INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
         DFHMDF POS=(18,1),LENGTH=78,ATTRB=ASKIP,                       X
               INITIAL='------------------------------------------------X
               ------------------------------'
         DFHMDF POS=(19,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(19,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Reinitialiser'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**JCL d'assemblage : ASMMAJ.jcl**

```jcl
//ROCHA09 JOB (ACCT),'ASSEMBL BMS CLIMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 9
//* ASSEMBLAGE DE LA MAP BMS CLIMAJ (MISE A JOUR CLIENT)
//*
//* Ce JCL assemble le source BMS et genere :
//*   - Le module MAP physique dans ROCHA.CICS.LOAD
//*   - Le copybook DSECT dans ROCHA.CICS.LINK
//*
//* Le copybook genere contiendra pour chaque champ :
//*   - NOMCPTx  ou x = I (input), O (output), L (longueur), A (attr)
//*   - Le suffixe 'A' permet de modifier l'attribut dynamiquement
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//* ASSEMBLAGE DE LA MAP BMS
//*
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIMAJ',RMODE=24
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIMAJ),DISP=SHR
/*
//
```

### Concept cle : Attributs dynamiques BMS

Le copybook genere par l'assemblage BMS contient pour chaque champ un suffixe `A` permettant de modifier l'attribut a l'execution :

| Constante CICS | Valeur | Description |
|----------------|--------|-------------|
| DFHBMASK | X'20' | ASKIP - Protege, intensite normale |
| DFHBMPRF | X'28' | ASKIP - Protege, brillant |
| DFHBMUNN | X'4C' | UNPROT + NUM - Saisie numerique |
| DFHBMUNP | X'40' | UNPROT - Saisie alphanumerique |
| DFHBMFSE | X'08' | MDT force - Champ marque comme modifie |
| DFHBMPRO | X'20' | PROT - Protege (synonyme de DFHBMASK) |

> **Note** : Ces constantes sont definies dans le copybook DFHBMSCA qu'il faut inclure dans le programme avec `COPY DFHBMSCA`.

### Definition CICS

```
CEDA DEFINE MAPSET(CLIMAJ) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

### Verification

```
CEDA VIEW MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour verifier un mapset, utiliser `CEDA VIEW`.

### Captures d'ecran

<!-- ![pt2ex09-1](images-pt2/pt2ex09-1.png) -->

---

## Exercice 10 : Programme de mise a jour (REWRITE)

### Enonce

Creer le PROGRAMME pour une operation de mise a jour d'un CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Mon travail

Le programme PRGMAJ implemente un mode **pseudo-conversationnel a 3 phases** :

1. **Phase RECHERCHE** : Saisie du numero de compte, verification existence
2. **Phase AFFICHAGE** : Affichage des donnees actuelles, NUMCPT protege (ASKIP)
3. **Phase VALIDATION** : Reception modifications, validation, READ UPDATE + REWRITE

**Points techniques importants :**

| Aspect | Explication |
|--------|-------------|
| COPY DFHBMSCA | Copybook pour les constantes d'attribut (DFHBMASK, etc.) |
| COMMAREA etendue | Sauvegarde la phase ET le numero de compte |
| READ UPDATE atomique | Le READ UPDATE et REWRITE doivent etre dans la meme UOW |
| NUMCPT sauvegarde | Necessaire car un champ ASKIP n'est pas transmis par le terminal |

### Mode pseudo-conversationnel a 3 phases

```
+------------------+     +-------------------+     +-------------------+
|   PHASE 1        |     |   PHASE 2         |     |   PHASE 3         |
|   RECHERCHE      | --> |   AFFICHAGE       | --> |   VALIDATION      |
+------------------+     +-------------------+     +-------------------+
|                  |     |                   |     |                   |
| NUMCPT: UNPROT   |     | NUMCPT: ASKIP     |     | NUMCPT: ASKIP     |
| Autres: vides    |     | Autres: remplis   |     | Autres: modifies  |
|                  |     |                   |     |                   |
| Action: Saisie   |     | Action: READ      |     | Action: REWRITE   |
| du numero        |     | + Affichage       |     | apres validation  |
+------------------+     +-------------------+     +-------------------+
```

### Resolution

**Programme complet : PRGMAJ.cbl**

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGMAJ.
      ******************************************************************
      * PROGRAMME : PRGMAJ
      * FONCTION  : Mise a jour d'un client existant
      * TRANSACTION : MAJO
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPMAJ (MAPSET CLIMAJ)
      *
      * MODE PSEUDO-CONVERSATIONNEL A 3 PHASES :
      * ----------------------------------------
      * Phase 1 (RECHERCHE) :
      *   - Affiche ecran vide pour saisie numero compte
      *   - NUMCPT en UNPROT (saisissable)
      *   - Autres champs vides
      *
      * Phase 2 (AFFICHAGE) :
      *   - Lit le client avec READ UPDATE (verrouillage)
      *   - Affiche les donnees actuelles
      *   - NUMCPT passe en ASKIP (protege, cle non modifiable)
      *   - Autres champs en UNPROT pour modification
      *
      * Phase 3 (VALIDATION) :
      *   - Recoit les modifications
      *   - Valide les donnees
      *   - REWRITE pour sauvegarder
      *
      * DIFFERENCE AVEC AJOUT (WRITE) :
      * - READ UPDATE obligatoire avant REWRITE
      * - La cle (NUMCPT) ne peut pas etre modifiee
      * - Le client doit exister (pas de creation)
      *
      * FIL ROUGE CICS - EXERCICE 10
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase et le numero de compte entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-RECHERCHE  VALUE '1'.
              88 PHASE-AFFICHAGE  VALUE '2'.
              88 PHASE-VALIDATION VALUE '3'.
           05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLIMAJ)
      *-----------------------------------------------------------------
       COPY CLIMAJ.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT          PIC X(06).
           05 CLI-CODREG          PIC X(02).
           05 CLI-NATCPT          PIC X(02).
           05 CLI-NOM             PIC X(10).
           05 CLI-PRENOM          PIC X(10).
           05 CLI-DATNAISS        PIC X(08).
           05 CLI-SEXE            PIC X(01).
           05 CLI-ACTPRO          PIC X(02).
           05 CLI-SITSO           PIC X(01).
           05 CLI-ADRESSE         PIC X(10).
           05 CLI-SOLDE           PIC X(10).
           05 CLI-POSITION        PIC X(02).
           05 FILLER              PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP                PIC S9(08) COMP VALUE 0.
       01  WS-ERREUR              PIC X(01) VALUE 'N'.
           88 ERREUR-DETECTEE     VALUE 'O'.
           88 PAS-ERREUR          VALUE 'N'.
       01  WS-MSG-FIN             PIC X(40)
           VALUE 'TRANSACTION MAJO TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * SAUVEGARDE DES DONNEES SAISIES (EVITE ECRASEMENT PAR LOW-VALUES)
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-NUMCPT           PIC X(06).
           05 WS-NUMCPTL          PIC S9(04) COMP.
           05 WS-CODREG           PIC X(02).
           05 WS-CODREGL          PIC S9(04) COMP.
           05 WS-NATCPT           PIC X(02).
           05 WS-NOM              PIC X(10).
           05 WS-NOML             PIC S9(04) COMP.
           05 WS-PRENOM           PIC X(10).
           05 WS-DATNAISS         PIC X(08).
           05 WS-DATNAISSL        PIC S9(04) COMP.
           05 WS-SEXE             PIC X(01).
           05 WS-SEXEL            PIC S9(04) COMP.
           05 WS-ACTPRO           PIC X(02).
           05 WS-SITSO            PIC X(01).
           05 WS-SITSOL           PIC S9(04) COMP.
           05 WS-ADRESSE          PIC X(10).
           05 WS-SOLDE            PIC X(10).
           05 WS-POSITION         PIC X(02).
           05 WS-POSITL           PIC S9(04) COMP.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
      * OBLIGATOIRE pour acceder aux donnees du RETURN precedent
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
           05 LS-PHASE            PIC X(01).
           05 LS-NUMCPT-SAVED     PIC X(06).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
      * Point d'entree du programme
      * Gestion du mode pseudo-conversationnel
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Phase recherche
                   PERFORM 1000-INIT-RECHERCHE
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT-RECHERCHE
               WHEN OTHER
      *            Traitement selon la phase en cours
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('MAJO')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT-RECHERCHE.
      *-----------------------------------------------------------------
      * Affichage ecran initial pour saisie numero compte
      * NUMCPT en UNPROT (saisissable)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE 'SAISIR LE NUMERO DE COMPTE A MODIFIER' TO MSGO
           MOVE '1' TO WS-PHASE
           MOVE SPACES TO WS-NUMCPT-SAVED

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la phase en cours
      *-----------------------------------------------------------------
           MOVE 'N' TO WS-ERREUR

           EVALUATE TRUE
               WHEN PHASE-RECHERCHE
                   PERFORM 3000-RECHERCHER-CLIENT THRU 3000-FIN
               WHEN PHASE-AFFICHAGE
               WHEN PHASE-VALIDATION
                   PERFORM 4000-VALIDER-MODIFICATION THRU 4000-FIN
           END-EVALUATE.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-RECHERCHER-CLIENT.
      *-----------------------------------------------------------------
      * Phase 1 -> 2 : Recherche du client par son numero
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du numero saisi
           MOVE NUMCPTI TO WS-NUMCPT
           MOVE NUMCPTL TO WS-NUMCPTL

      *    Controle numero de compte
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Lecture du client (sans UPDATE car on affiche seulement)
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Client trouve - Affichage des donnees
           PERFORM 3100-AFFICHER-CLIENT

      *    Passage en phase AFFICHAGE/VALIDATION
           MOVE '2' TO WS-PHASE
           MOVE WS-NUMCPT TO WS-NUMCPT-SAVED.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Affiche les donnees du client dans la MAP
      * NUMCPT passe en ASKIP (protege)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO

      *    Transfert des donnees vers la MAP
           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NATCPT   TO NATCPTO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-ACTPRO   TO ACTPROO
           MOVE CLI-SITSO    TO SITSOO
           MOVE CLI-ADRESSE  TO ADRESSEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITO

      *    IMPORTANT : Proteger le numero de compte (cle non modifiable)
      *    DFHBMASK = X'20' = ASKIP (protege, intensite normale)
           MOVE DFHBMASK TO NUMCPTA

           MOVE 'CLIENT TROUVE - MODIFIER ET VALIDER AVEC ENTER' TO MSGO

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       4000-VALIDER-MODIFICATION.
      *-----------------------------------------------------------------
      * Phase 2/3 : Reception et validation des modifications
      *
      * IMPORTANT - MISE A JOUR vs AJOUT :
      * En mise a jour, l'utilisateur ne modifie que certains champs.
      * Les champs non modifies ont une longueur = 0 (terminal n'envoie
      * que les champs modifies). On doit donc :
      *   1. Relire le client pour avoir ses donnees actuelles
      *   2. Ne remplacer que les champs modifies (longueur > 0)
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE WS-NUMCPT-SAVED TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'AUCUNE MODIFICATION - ENTREZ DES DONNEES' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    SAUVEGARDE DES DONNEES MAP AVANT ECRASEMENT PAR LOW-VALUES
           MOVE WS-NUMCPT-SAVED TO WS-NUMCPT
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NATCPTI   TO WS-NATCPT
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE PRENOMI   TO WS-PRENOM
           MOVE DATNAI    TO WS-DATNAISS
           MOVE DATNAL    TO WS-DATNAISSL
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE ACTPROI   TO WS-ACTPRO
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE ADRESSEI  TO WS-ADRESSE
           MOVE SOLDEI    TO WS-SOLDE
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL

      *    RELECTURE DU CLIENT POUR AVOIR LES DONNEES ACTUELLES
           MOVE WS-NUMCPT TO CLI-NUMCPT
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE LOW-VALUES TO MAPMAJO
               MOVE WS-NUMCPT TO NUMCPTO
               MOVE DFHBMASK TO NUMCPTA
               MOVE 'ERREUR RELECTURE CLIENT - REESSAYEZ' TO MSGO
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    FUSION : Ne remplacer que les champs modifies (longueur > 0)
      *    Les champs non modifies gardent leur valeur actuelle (CLI-*)
           PERFORM 4050-FUSIONNER-MODIFICATIONS

      *    Validation des donnees finales
           PERFORM 4100-VALIDER-DONNEES THRU 4100-FIN

           IF ERREUR-DETECTEE
               MOVE DFHBMASK TO NUMCPTA
               EXEC CICS SEND MAP('MAPMAJ')
                   MAPSET('CLIMAJ')
                   ERASE
               END-EXEC
               GO TO 4000-FIN
           END-IF

      *    Ecriture de l'enregistrement
           PERFORM 4300-ECRIRE-MODIFICATION THRU 4300-FIN

           MOVE DFHBMASK TO NUMCPTA
           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4050-FUSIONNER-MODIFICATIONS.
      *-----------------------------------------------------------------
      * Fusionne les modifications de l'utilisateur avec les donnees
      * actuelles du client. Seuls les champs modifies (longueur > 0)
      * remplacent les valeurs existantes.
      *-----------------------------------------------------------------
      *    Code region : si modifie, prendre la nouvelle valeur
           IF WS-CODREGL > 0
               MOVE WS-CODREG TO CLI-CODREG
           ELSE
               MOVE CLI-CODREG TO WS-CODREG
           END-IF

      *    Nature compte : pas de longueur, on prend si non vide
           IF WS-NATCPT NOT = SPACES AND WS-NATCPT NOT = LOW-VALUES
               MOVE WS-NATCPT TO CLI-NATCPT
           ELSE
               MOVE CLI-NATCPT TO WS-NATCPT
           END-IF

      *    Nom
           IF WS-NOML > 0
               MOVE WS-NOM TO CLI-NOM
           ELSE
               MOVE CLI-NOM TO WS-NOM
           END-IF

      *    Prenom : pas de longueur obligatoire
           IF WS-PRENOM NOT = SPACES AND WS-PRENOM NOT = LOW-VALUES
               MOVE WS-PRENOM TO CLI-PRENOM
           ELSE
               MOVE CLI-PRENOM TO WS-PRENOM
           END-IF

      *    Date naissance
           IF WS-DATNAISSL > 0
               MOVE WS-DATNAISS TO CLI-DATNAISS
           ELSE
               MOVE CLI-DATNAISS TO WS-DATNAISS
           END-IF

      *    Sexe
           IF WS-SEXEL > 0
               MOVE WS-SEXE TO CLI-SEXE
           ELSE
               MOVE CLI-SEXE TO WS-SEXE
           END-IF

      *    Activite pro : pas de longueur obligatoire
           IF WS-ACTPRO NOT = SPACES AND WS-ACTPRO NOT = LOW-VALUES
               MOVE WS-ACTPRO TO CLI-ACTPRO
           ELSE
               MOVE CLI-ACTPRO TO WS-ACTPRO
           END-IF

      *    Situation sociale
           IF WS-SITSOL > 0
               MOVE WS-SITSO TO CLI-SITSO
           ELSE
               MOVE CLI-SITSO TO WS-SITSO
           END-IF

      *    Adresse : pas de longueur obligatoire
           IF WS-ADRESSE NOT = SPACES AND WS-ADRESSE NOT = LOW-VALUES
               MOVE WS-ADRESSE TO CLI-ADRESSE
           ELSE
               MOVE CLI-ADRESSE TO WS-ADRESSE
           END-IF

      *    Solde : pas de longueur obligatoire
           IF WS-SOLDE NOT = SPACES AND WS-SOLDE NOT = LOW-VALUES
               MOVE WS-SOLDE TO CLI-SOLDE
           ELSE
               MOVE CLI-SOLDE TO WS-SOLDE
           END-IF

      *    Position
           IF WS-POSITL > 0
               MOVE WS-POSITION TO CLI-POSITION
           ELSE
               MOVE CLI-POSITION TO WS-POSITION
           END-IF.

      *-----------------------------------------------------------------
       4100-VALIDER-DONNEES.
      *-----------------------------------------------------------------
      * Controles de conformite des donnees finales (apres fusion)
      * Note: Les variables WS-* contiennent soit la modification de
      * l'utilisateur, soit la valeur actuelle du client (via fusion)
      * Donc on ne verifie plus les longueurs, seulement les valeurs.
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO
           MOVE WS-NUMCPT TO NUMCPTO

      *    Controle code region (01, 02, 03 ou 04)
           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle nom (obligatoire)
           IF WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle sexe (M ou F)
           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle situation sociale (C, M, D ou V)
           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF

      *    Controle position (DB ou CR)
           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4100-FIN
           END-IF.

       4100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4300-ECRIRE-MODIFICATION.
      *-----------------------------------------------------------------
      * Mise a jour de l'enregistrement avec READ UPDATE + REWRITE
      *
      * IMPORTANT : Le REWRITE necessite un READ UPDATE prealable
      * dans la meme unite de travail (UOW).
      *
      * Les variables WS-* contiennent les donnees finales (apres fusion
      * des modifications utilisateur avec les donnees actuelles).
      *-----------------------------------------------------------------
      *    READ UPDATE pour verrouiller l'enregistrement
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR VERROUILLAGE - REESSAYEZ' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4300-FIN
           END-IF

      *    Reappliquer les modifications sur l'enregistrement lu
           MOVE WS-CODREG    TO CLI-CODREG
           MOVE WS-NATCPT    TO CLI-NATCPT
           MOVE WS-NOM       TO CLI-NOM
           MOVE WS-PRENOM    TO CLI-PRENOM
           MOVE WS-DATNAISS  TO CLI-DATNAISS
           MOVE WS-SEXE      TO CLI-SEXE
           MOVE WS-ACTPRO    TO CLI-ACTPRO
           MOVE WS-SITSO     TO CLI-SITSO
           MOVE WS-ADRESSE   TO CLI-ADRESSE
           MOVE WS-SOLDE     TO CLI-SOLDE
           MOVE WS-POSITION  TO CLI-POSITION

      *    REWRITE - Mise a jour effective
           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPMAJO
                   MOVE WS-NUMCPT TO NUMCPTO
                   MOVE 'MISE A JOUR EFFECTUEE - NOUVEAU OU PF3'
                       TO MSGO
      *            Retour en phase recherche pour nouveau client
                   MOVE '1' TO WS-PHASE
                   MOVE SPACES TO WS-NUMCPT-SAVED
               WHEN OTHER
                   MOVE 'ERREUR MISE A JOUR - CONTACTEZ SUPPORT' TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.

       4300-FIN.
           EXIT.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
      * Fin de la transaction
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
```

### Sections importantes du programme

#### 1. La LINKAGE SECTION (obligatoire pour DFHCOMMAREA)

```cobol
       LINKAGE SECTION.
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
      * OBLIGATOIRE pour acceder aux donnees du RETURN precedent
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
           05 LS-PHASE            PIC X(01).
           05 LS-NUMCPT-SAVED     PIC X(06).
```

**Pourquoi la LINKAGE SECTION est obligatoire :**

En mode pseudo-conversationnel, CICS passe les donnees du RETURN precedent via DFHCOMMAREA. Sans la LINKAGE SECTION, le programme ne peut pas acceder a ces donnees.

La variable speciale EIBCALEN (dans EIB) contient la longueur de la COMMAREA recue :
- `EIBCALEN = 0` : Premier appel (pas de COMMAREA)
- `EIBCALEN > 0` : Rappel avec COMMAREA

Le code dans 0000-PRINCIPAL utilise cette information :
```cobol
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - pas de COMMAREA
                   PERFORM 1000-INIT-RECHERCHE
               WHEN OTHER
      *            COMMAREA presente - copier dans WORKING-STORAGE
                   MOVE DFHCOMMAREA TO WS-COMMAREA
```

#### 2. Le paragraphe 4050-FUSIONNER-MODIFICATIONS

Ce paragraphe est crucial pour la mise a jour. En mode mise a jour (contrairement a l'ajout), l'utilisateur ne modifie que certains champs. Le terminal BMS ne transmet que les champs modifies (les autres ont une longueur = 0).

**Probleme :** Si on ecrit directement les valeurs de la MAP, les champs non modifies seraient ecrases par des espaces ou LOW-VALUES.

**Solution :** Fusionner les modifications de l'utilisateur avec les donnees actuelles du client.

```cobol
      *    Code region : si modifie, prendre la nouvelle valeur
           IF WS-CODREGL > 0
               MOVE WS-CODREG TO CLI-CODREG    <- Utilisateur a modifie
           ELSE
               MOVE CLI-CODREG TO WS-CODREG    <- Garder valeur actuelle
           END-IF
```

La logique est la suivante :
- Si la longueur du champ saisi (suffixe L) > 0 : l'utilisateur a modifie ce champ
- Si la longueur = 0 : l'utilisateur n'a pas touche ce champ, on garde la valeur existante

Pour les champs sans variable de longueur, on teste si le champ est different de SPACES et LOW-VALUES :
```cobol
           IF WS-NATCPT NOT = SPACES AND WS-NATCPT NOT = LOW-VALUES
               MOVE WS-NATCPT TO CLI-NATCPT
           ELSE
               MOVE CLI-NATCPT TO WS-NATCPT
           END-IF
```

#### 3. Le READ UPDATE + REWRITE atomique

En CICS, la commande REWRITE necessite un READ UPDATE prealable dans la **meme unite de travail (UOW)**. Or, en mode pseudo-conversationnel, chaque interaction utilisateur termine la tache CICS (et donc l'UOW).

**Consequence :** On ne peut pas faire READ UPDATE en phase 2 et REWRITE en phase 3.

**Solution :** Faire les deux operations dans le meme paragraphe, juste avant la mise a jour effective :

```cobol
       4300-ECRIRE-MODIFICATION.
      *    1. READ UPDATE : verrouille l'enregistrement
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE                 <-- Option cle : verrouillage
               RESP(WS-RESP)
           END-EXEC

      *    2. Application des modifications sur l'enregistrement
           MOVE WS-CODREG TO CLI-CODREG
           ...

      *    3. REWRITE : ecrit l'enregistrement modifie
           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC
```

**Sequence des lectures :**

```
Passage 1 (RECHERCHE) : READ simple -> Affichage
                        (pas de verrouillage car fin de tache apres)

Passage 2 (VALIDATION) : READ UPDATE -> Modifications -> REWRITE
                         (atomique, meme UOW)
```

**JCL de compilation : CMPMAJ.jcl**

```jcl
//ROCHA10 JOB (ACCT),'COMPILE PRGMAJ',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* PROJET FIL ROUGE CICS - EXERCICE 10
//* COMPILATION DU PROGRAMME COBOL-CICS PRGMAJ (MISE A JOUR CLIENT)
//*
//* Copybooks requis :
//*   - DFHAID   : Codes touches fonction
//*   - DFHBMSCA : Constantes attributs (DFHBMASK, etc.)
//*   - CLIMAJ   : Structure MAP generee
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGMAJ),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGMAJ(R)
/*
//
```

### Definition CICS

```
CEDA DEFINE PROGRAM(PRGMAJ) GROUP(CLIGROUP) LANGUAGE(COBOL)
CEDA INSTALL PROGRAM(PRGMAJ) GROUP(CLIGROUP)
```

### Verification

```
CEMT INQ PROGRAM(PRGMAJ)
```

Resultat attendu : `Prog(PRGMAJ) Cob Ena`

### Utilisation

#### 1. Copier le source COBOL dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member PRGMAJ
Copier le contenu de PRGMAJ.cbl
```

#### 2. Soumettre le JCL de compilation

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CMPMAJ (copier CMPMAJ.jcl)
SUB (submit)
```

#### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre PRGMAJ present dans ROCHA.CICS.LOAD

#### 4. Definir le programme dans CICS

```
CEDA DEFINE PROGRAM(PRGMAJ) GROUP(CLIGROUP) LANGUAGE(COBOL)
CEDA INSTALL PROGRAM(PRGMAJ) GROUP(CLIGROUP)
```

### Points importants

1. **COPY DFHBMSCA** : Ajoute pour avoir acces aux constantes d'attribut (DFHBMASK, etc.)

2. **Sauvegarde du NUMCPT** : Le numero est sauvegarde dans WS-NUMCPT-SAVED car une fois en ASKIP, il n'est plus transmis par le terminal

3. **READ sans UPDATE en phase 2** : La premiere lecture (affichage) n'utilise pas UPDATE car le verrouillage ne persiste pas entre les passages pseudo-conversationnels

4. **READ UPDATE + REWRITE atomique** : Les deux commandes sont executees dans le meme paragraphe pour garantir l'atomicite

5. **Retour en phase 1** : Apres une mise a jour reussie, le programme revient en phase RECHERCHE pour permettre la modification d'un autre client

### Captures d'ecran

<!-- ![pt2ex10-1](images-pt2/pt2ex10-1.png) -->

---

## Exercice 11 : Transaction de mise a jour

### Enonce

Definir une transaction independante de la precedente pour appeler le programme de mise a jour.

### Mon travail

La transaction MAJO est le point d'entree utilisateur pour la mise a jour.

**Architecture CICS - Liaison Transaction/Programme/MAP/Fichier :**

```
+-------------+     +-------------+     +-------------+
| TRANSACTION | --> | PROGRAMME   | --> | MAPSET      |
|    MAJO     |     |   PRGMAJ    |     |   CLIMAJ    |
+-------------+     +-------------+     +-------------+
                           |
                           v
                    +-------------+
                    |   FICHIER   |
                    |   FCLIENT   |
                    +-------------+
```

Une transaction CICS est le point d'entree utilisateur. Elle fait le lien entre :
- Le code transaction saisi par l'utilisateur (MAJO)
- Le programme COBOL-CICS a executer (PRGMAJ)

Le programme utilise ensuite le mapset (CLIMAJ) pour l'interface et le fichier (FCLIENT) pour les donnees.

### Resolution

**Definition de la transaction :**

```
CEDA DEFINE TRANSACTION(MAJO) GROUP(CLIGROUP) PROGRAM(PRGMAJ)
```

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| TRANSACTION | MAJO | Code transaction (4 caracteres max) |
| GROUP | CLIGROUP | Groupe de ressources du projet |
| PROGRAM | PRGMAJ | Programme COBOL a executer |

**Installation de la transaction :**

```
CEDA INSTALL TRANSACTION(MAJO) GROUP(CLIGROUP)
```

> **Bonne pratique** : Installer uniquement la ressource ajoutee plutot que tout le groupe. Reinstaller le groupe peut causer des problemes si FCLIENT est ouvert.

### Verification

```
CEDA VIEW TRANSACTION(MAJO) GROUP(CLIGROUP)
CEMT INQ PROGRAM(PRGMAJ)
```

### Test

```
MAJO
```

Comportement attendu :
1. Ecran de saisie du numero de compte
2. Saisir un numero existant (ex: 100001)
3. Affichage des donnees du client (NUMCPT protege)
4. Modifier les champs souhaites
5. ENTER pour valider -> Message "MISE A JOUR EFFECTUEE"

### Ressources du groupe CLIGROUP apres exercice 11

| Type | Nom | Description |
|------|-----|-------------|
| FILE | FCLIENT | Fichier VSAM clients |
| MAPSET | CLIAFF | Ecran affichage |
| MAPSET | CLIAJT | Ecran ajout |
| MAPSET | CLIMAJ | Ecran mise a jour |
| PROGRAM | PRGCLIA | Programme affichage |
| PROGRAM | PRGAJT | Programme ajout |
| PROGRAM | PRGMAJ | Programme mise a jour |
| TRANSACTION | AFFI | Transaction affichage |
| TRANSACTION | AJOU | Transaction ajout |
| TRANSACTION | MAJO | Transaction mise a jour |

### Captures d'ecran

<!-- ![pt2ex11-1](images-pt2/pt2ex11-1.png) -->

---

[< Partie 2a : Ajout](03-partie-2a-ajout.md) | [Retour au sommaire](00-introduction.md) | [Partie 2c : Suppression >](05-partie-2c-suppression.md)
