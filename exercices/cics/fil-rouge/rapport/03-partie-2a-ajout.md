# Partie 2a : Operations d'Ajout (WRITE)

[< Partie 1 : Affichage](02-partie-1-affichage.md) | [Retour au sommaire](00-introduction.md) | [Partie 2b : Mise a jour >](04-partie-2b-maj.md)

---

Cette section couvre les exercices 6 a 8 : creation de la MAP d'ajout, programme d'ajout avec la commande WRITE, et definition de la transaction AJOU.

## Comparaison des commandes CICS pour l'ecriture

| Commande | Usage | Prerequis | Erreur typique |
|----------|-------|-----------|----------------|
| **WRITE** | Ajouter un nouvel enregistrement | Le client ne doit PAS exister | DUPREC (doublon) |
| **REWRITE** | Modifier un enregistrement existant | READ UPDATE obligatoire | NOTFND (inexistant) |

---

# Partie 2 : Operations CRUD

## Exercice 6 : MAP pour ajout de client

### Enonce

Creer ou adapter la MAP precedente pour une operation d'ajout de CLIENT dans le Data Set CLIENT.

### Mon travail

J'ai adapte la MAP d'affichage (CLIAFF) pour creer une nouvelle MAP de saisie (CLIAJT). La principale difference est que tous les champs sont maintenant saisissables (UNPROT) au lieu d'etre en affichage seul (ASKIP).

**Differences entre CLIAFF et CLIAJT :**

| Aspect | CLIAFF (Affichage) | CLIAJT (Ajout) |
|--------|-------------------|----------------|
| NUMCPT | UNPROT (saisie cle) | UNPROT (saisie) |
| Autres champs | ASKIP (affichage) | UNPROT (saisie) |
| Libelles (region, sexe...) | Affiches | Non affiches |
| Titre | "AFFICHAGE CLIENT" | "AJOUT CLIENT" |
| Touches | ENTER=Rechercher | ENTER=Valider |

### Resolution

**MAP BMS : CLIAJT.bms**

Le code source est stocke dans `ROCHA.CICS.SOURCE(CLIAJT)`. Voici le code complet :

```
***********************************************************************
*  MAPSET : CLIAJT - Ajout Client
*  Transaction : AJOU
*  Fil Rouge CICS - Exercice 6
***********************************************************************
CLIAJT   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPAJT   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* TITRE
*----------------------------------------------------------------------
         DFHMDF POS=(1,28),LENGTH=24,ATTRB=(ASKIP,BRT),                 X
               INITIAL='*** AJOUT CLIENT ***'
*----------------------------------------------------------------------
* ZONES DE SAISIE - TOUS LES CHAMPS EN UNPROT
*----------------------------------------------------------------------
         DFHMDF POS=(3,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NUMERO COMPTE :'
NUMCPT   DFHMDF POS=(3,19),LENGTH=6,ATTRB=(UNPROT,NUM,IC)
         DFHMDF POS=(3,26),LENGTH=1,ATTRB=ASKIP
*
         DFHMDF POS=(4,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(4,19),LENGTH=2,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(4,22),LENGTH=20,ATTRB=ASKIP,                       X
               INITIAL='(01=Paris,02=Mars...)'
*
         DFHMDF POS=(5,2),LENGTH=16,ATTRB=ASKIP,                        X
               INITIAL='NATURE COMPTE :'
NATCPT   DFHMDF POS=(5,19),LENGTH=2,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(6,2),LENGTH=16,ATTRB=ASKIP,INITIAL='NOM           :'
NOM      DFHMDF POS=(6,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(7,2),LENGTH=16,ATTRB=ASKIP,INITIAL='PRENOM        :'
PRENOM   DFHMDF POS=(7,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(8,2),LENGTH=16,ATTRB=ASKIP,INITIAL='DATE NAISSANCE:'
DATNA    DFHMDF POS=(8,19),LENGTH=8,ATTRB=(UNPROT,NUM)
         DFHMDF POS=(8,28),LENGTH=10,ATTRB=ASKIP,INITIAL='(AAAAMMJJ)'
*
         DFHMDF POS=(9,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SEXE          :'
SEXE     DFHMDF POS=(9,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(9,21),LENGTH=8,ATTRB=ASKIP,INITIAL='(M ou F)'
*
         DFHMDF POS=(10,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ACTIVITE PRO  :'
ACTPRO   DFHMDF POS=(10,19),LENGTH=2,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(11,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SITUATION SOC :'
SITSO    DFHMDF POS=(11,19),LENGTH=1,ATTRB=UNPROT
         DFHMDF POS=(11,21),LENGTH=12,ATTRB=ASKIP,INITIAL='(C/M/D/V)'
*
         DFHMDF POS=(12,2),LENGTH=16,ATTRB=ASKIP,INITIAL='ADRESSE       :'
ADRESSE  DFHMDF POS=(12,19),LENGTH=10,ATTRB=UNPROT
*
         DFHMDF POS=(13,2),LENGTH=16,ATTRB=ASKIP,INITIAL='SOLDE         :'
SOLDE    DFHMDF POS=(13,19),LENGTH=10,ATTRB=(UNPROT,NUM)
*
         DFHMDF POS=(14,2),LENGTH=16,ATTRB=ASKIP,INITIAL='POSITION      :'
POSIT    DFHMDF POS=(14,19),LENGTH=2,ATTRB=UNPROT
         DFHMDF POS=(14,22),LENGTH=10,ATTRB=ASKIP,INITIAL='(DB ou CR)'
*----------------------------------------------------------------------
* ZONE MESSAGE ET TOUCHES FONCTION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=10,ATTRB=ASKIP,INITIAL='MESSAGE :'
MSG      DFHMDF POS=(18,13),LENGTH=60,ATTRB=(ASKIP,BRT)
*
         DFHMDF POS=(22,2),LENGTH=70,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Valider  PF3=Quitter  CLEAR=Effacer'
***********************************************************************
         DFHMSD TYPE=FINAL
         END
```

**Apercu de l'ecran MAPAJT :**

```
+------------------------------------------------------------------------------+
|                         *** AJOUT CLIENT ***                                 |
|                                                                              |
|  NUMERO COMPTE : ______                                                      |
|  CODE REGION   : __     (01=Paris,02=Mars...)                                |
|  NATURE COMPTE : __                                                          |
|  NOM           : __________                                                  |
|  PRENOM        : __________                                                  |
|  DATE NAISSANCE: ________  (AAAAMMJJ)                                        |
|  SEXE          : _  (M ou F)                                                 |
|  ACTIVITE PRO  : __                                                          |
|  SITUATION SOC : _  (C/M/D/V)                                                |
|  ADRESSE       : __________                                                  |
|  SOLDE         : __________                                                  |
|  POSITION      : __  (DB ou CR)                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|  MESSAGE : ____________________________________________________________      |
|                                                                              |
|                                                                              |
|                                                                              |
|  ENTER=Valider  PF3=Quitter  CLEAR=Effacer                                   |
+------------------------------------------------------------------------------+
```

**Zones de saisie :**

| Zone | Longueur | Attribut | Aide affichee |
|------|----------|----------|---------------|
| NUMCPT | 6 | UNPROT,NUM,IC | - |
| CODREG | 2 | UNPROT,NUM | (01=Paris,02=Mars...) |
| NATCPT | 2 | UNPROT,NUM | - |
| NOM | 10 | UNPROT | - |
| PRENOM | 10 | UNPROT | - |
| DATNA | 8 | UNPROT,NUM | (AAAAMMJJ) |
| SEXE | 1 | UNPROT | (M ou F) |
| ACTPRO | 2 | UNPROT,NUM | - |
| SITSO | 1 | UNPROT | (C/M/D/V) |
| ADRESSE | 10 | UNPROT | - |
| SOLDE | 10 | UNPROT,NUM | - |
| POSIT | 2 | UNPROT | (DB ou CR) |

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex06-1 : Source BMS dans ISPF EDIT - ROCHA.CICS.SOURCE(CLIAJT)
2. pt2ex06-2 : Soumission JCL assemblage BMS
3. pt2ex06-3 : SDSF - Job output avec RC=0000
4. pt2ex06-4 : Ecran MAPAJT vide - pret pour saisie
-->

---

## Exercice 7 : Programme d'ajout (WRITE)

### Enonce

Creer le PROGRAMME pour une operation d'ajout d'un nouveau CLIENT dans le Data Set CLIENT. Un controle de conformite de donnee et de doublure doit etre effectue.

### Mon travail

J'ai developpe le programme PRGAJT qui gere l'ajout de nouveaux clients avec les fonctionnalites suivantes :

1. **Mode pseudo-conversationnel** : Premier passage affiche ecran vide, passages suivants traitent la saisie
2. **Gestion MAPFAIL** : Detection si l'utilisateur n'a saisi aucune donnee
3. **Controles de conformite** avant ecriture :
   - Numero de compte obligatoire et numerique (6 chiffres)
   - Code region valide (01, 02, 03 ou 04)
   - Nom obligatoire
   - Sexe valide (M ou F)
   - Situation sociale valide (C, M, D ou V)
   - Position valide (DB ou CR)
4. **Verification de doublure** : READ pour verifier que le client n'existe pas deja
5. **Ecriture VSAM** : WRITE avec gestion des erreurs (DUPREC, etc.)

**Point technique** : La commande `EXIT PARAGRAPH` n'etant pas supportee sur la version COBOL de TK4-, j'ai utilise le pattern `GO TO paragraphe-FIN` pour sortir des validations en cas d'erreur.

### Resolution

**Programme : PRGAJT.cbl**

Le code source est stocke dans `ROCHA.CICS.SOURCE(PRGAJT)`. Voici les extraits principaux :

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGAJT.
      ******************************************************************
      * PROGRAMME : PRGAJT - Ajout client
      * TRANSACTION : AJOU
      * MODE : Pseudo-conversationnel
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMMAREA.
           05 WS-FLAG-INIT         PIC X(01) VALUE 'N'.

      * Copybooks CICS
       COPY DFHAID.
       COPY CLIAJT.

       01  ENR-CLIENT.
           05 CLI-NUMCPT           PIC X(06).
           05 CLI-CODREG           PIC X(02).
           05 CLI-NATCPT           PIC X(02).
           05 CLI-NOM              PIC X(10).
           05 CLI-PRENOM           PIC X(10).
           05 CLI-DATNAISS         PIC X(08).
           05 CLI-SEXE             PIC X(01).
           05 CLI-ACTPRO           PIC X(02).
           05 CLI-SITSO            PIC X(01).
           05 CLI-ADRESSE          PIC X(10).
           05 CLI-SOLDE            PIC X(10).
           05 CLI-POSITION         PIC X(02).
           05 FILLER               PIC X(16).

       01  WS-RESP                 PIC S9(08) COMP VALUE 0.
       01  WS-ERREUR               PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN EIBAID = DFHPF3
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN OTHER
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('AJOU')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       2000-TRAITEMENT.
           MOVE 'N' TO WS-ERREUR

           EXEC CICS RECEIVE MAP('MAPAJT')
               MAPSET('CLIAJT')
               RESP(WS-RESP)
           END-EXEC

      * Gestion MAPFAIL (aucune donnee transmise)
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPAJTO
               MOVE 'AUCUNE DONNEE SAISIE - VEUILLEZ REMPLIR' TO MSGO
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
      * Important : sauvegarder aussi les champs longueur (L) pour les validations
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE NUMCPTL   TO WS-NUMCPTL
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL
           ...

      * Validation des donnees
           PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verification doublure (client existe deja ?)
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Preparation et ecriture de l'enregistrement
           PERFORM 2300-PREPARER-ENREGISTREMENT
           PERFORM 2400-ECRIRE-CLIENT

           EXEC CICS SEND MAP('MAPAJT')
               MAPSET('CLIAJT')
           END-EXEC.

       2000-FIN.
           EXIT.

       2100-VALIDER-DONNEES.
           MOVE LOW-VALUES TO MAPAJTO

      * Controle numero de compte (utilise variables WS- sauvegardees)
           IF WS-NUMCPTL = 0 OR WS-NUMCPT = SPACES
               MOVE 'NUMERO DE COMPTE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-NUMCPT NOT NUMERIC
               MOVE 'NUMERO DE COMPTE DOIT ETRE NUMERIQUE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle code region (utilise WS-CODREG sauvegardee)
           IF WS-CODREGL = 0 OR WS-CODREG = SPACES
               MOVE 'CODE REGION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-CODREG NOT = '01' AND WS-CODREG NOT = '02'
              AND WS-CODREG NOT = '03' AND WS-CODREG NOT = '04'
               MOVE 'CODE REGION INVALIDE (01/02/03/04)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle nom (obligatoire)
           IF WS-NOML = 0 OR WS-NOM = SPACES
               MOVE 'NOM OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle sexe (utilise WS-SEXE sauvegardee)
           IF WS-SEXEL = 0 OR WS-SEXE = SPACES
               MOVE 'SEXE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SEXE NOT = 'M' AND WS-SEXE NOT = 'F'
               MOVE 'SEXE INVALIDE (M OU F)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle situation sociale (utilise WS-SITSO sauvegardee)
           IF WS-SITSOL = 0 OR WS-SITSO = SPACES
               MOVE 'SITUATION SOCIALE OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-SITSO NOT = 'C' AND WS-SITSO NOT = 'M'
              AND WS-SITSO NOT = 'D' AND WS-SITSO NOT = 'V'
               MOVE 'SITUATION INVALIDE (C/M/D/V)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

      * Controle position (utilise WS-POSITION sauvegardee)
           IF WS-POSITL = 0 OR WS-POSITION = SPACES
               MOVE 'POSITION OBLIGATOIRE' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF

           IF WS-POSITION NOT = 'DB' AND WS-POSITION NOT = 'CR'
               MOVE 'POSITION INVALIDE (DB OU CR)' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 2100-FIN
           END-IF.

       2100-FIN.
           EXIT.

       2200-VERIFIER-DOUBLURE.
           MOVE WS-NUMCPT TO CLI-NUMCPT

           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT EN DOUBLE - CE CLIENT EXISTE DEJA'
                   TO MSGO
               MOVE 'O' TO WS-ERREUR
           END-IF.

       2400-ECRIRE-CLIENT.
           EXEC CICS WRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPAJTO
                   MOVE 'CLIENT AJOUTE AVEC SUCCES - NOUVEAU OU PF3'
                       TO MSGO
               WHEN DFHRESP(DUPREC)
                   MOVE 'ENREGISTREMENT EN DOUBLE' TO MSGO
                   MOVE 'O' TO WS-ERREUR
               WHEN OTHER
                   MOVE 'ERREUR ECRITURE FICHIER - CONTACTEZ SUPPORT'
                       TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.
```

**JCL de compilation : CMPAJT.jcl (ROCHA06)**

```jcl
//ROCHA06 JOB (ACCT),'COMPILE PRGAJT',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* COMPILATION DU PROGRAMME COBOL-CICS PRGAJT (AJOUT CLIENT)
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
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGAJT),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGAJT(R)
/*
```

**Structure du programme :**

| Paragraphe | Fonction |
|------------|----------|
| 0000-PRINCIPAL | Point d'entree, aiguillage selon EIBCALEN et EIBAID |
| 1000-PREMIER-PASSAGE | Affichage de l'ecran vide pour saisie |
| 2000-TRAITEMENT | Reception saisie, validations, ecriture |
| 2100-VALIDER-DONNEES | Controles de conformite des champs |
| 2200-VERIFIER-DOUBLURE | Verification que le client n'existe pas |
| 2300-PREPARER-ENREGISTREMENT | Transfert MAP vers enregistrement |
| 2400-ECRIRE-CLIENT | WRITE VSAM avec gestion erreurs |
| 9000-FIN-PROGRAMME | Message de fin et RETURN sans TRANSID |

**Commandes CICS utilisees :**

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'ecran (avec ERASE au premier passage) |
| RECEIVE MAP | Recevoir la saisie avec RESP pour MAPFAIL |
| READ FILE | Verifier si client existe (doublure) |
| WRITE FILE | Ecrire le nouveau client |
| RETURN TRANSID | Retour pseudo-conversationnel |

**Messages d'erreur geres :**

| Message | Contexte |
|---------|----------|
| AUCUNE DONNEE SAISIE | MAPFAIL - utilisateur a appuye ENTER sans rien saisir |
| NUMERO DE COMPTE OBLIGATOIRE | Champ NUMCPT vide (longueur = 0) |
| NUMERO DE COMPTE DOIT ETRE NUMERIQUE | Champ NUMCPT contient des caracteres non numeriques |
| CODE REGION OBLIGATOIRE | Champ CODREG vide |
| CODE REGION INVALIDE | Code region different de 01/02/03/04 |
| NOM OBLIGATOIRE | Champ NOM vide |
| SEXE OBLIGATOIRE | Champ SEXE vide |
| SEXE INVALIDE | Sexe different de M ou F |
| SITUATION SOCIALE OBLIGATOIRE | Champ SITSO vide |
| SITUATION INVALIDE | Situation differente de C/M/D/V |
| POSITION OBLIGATOIRE | Champ POSIT vide |
| POSITION INVALIDE | Position differente de DB ou CR |
| ENREGISTREMENT EN DOUBLE | Client avec ce numero existe deja (READ a trouve un enregistrement) |
| CLIENT AJOUTE AVEC SUCCES | WRITE VSAM reussi |
| ERREUR ECRITURE FICHIER | Erreur VSAM inattendue (ni NORMAL ni DUPREC) |

### Points techniques importants

#### 1. Sauvegarde des donnees MAP (MODE=INOUT)

Avec `MODE=INOUT` et `STORAGE=AUTO` dans BMS, les zones input (I) et output (O) partagent la meme memoire. Il faut sauvegarder les donnees dans des variables WS- apres le `RECEIVE MAP` :

```cobol
      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE SEXEI     TO WS-SEXE
           MOVE POSITI    TO WS-POSITION
```

#### 2. PERFORM THRU pour les GO TO

Quand un paragraphe utilise `GO TO paragraphe-FIN`, il faut inclure le paragraphe FIN dans la plage du PERFORM avec `THRU` :

```cobol
           PERFORM 2000-TRAITEMENT THRU 2000-FIN
           PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
```

Sans `THRU`, le `GO TO` sort du PERFORM et le programme continue sequentiellement au lieu de retourner a l'appelant.

#### 3. ERASE sur les SEND MAP d'erreur

Pour que le message d'erreur s'affiche correctement, ajouter `ERASE` au SEND MAP :

```cobol
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF
```

### Difficultes rencontrees et solutions

#### Probleme 1 : Ecrasement des donnees saisies par LOW-VALUES

**Symptome** : Apres le `RECEIVE MAP`, les donnees saisies etaient perdues lors du `MOVE LOW-VALUES TO MAPAJTO` dans le paragraphe de validation.

**Cause** : Avec `MODE=INOUT` et `STORAGE=AUTO` dans la definition BMS, les zones input (suffixe I) et output (suffixe O) partagent la meme zone memoire. Le `MOVE LOW-VALUES TO MAPAJTO` ecrasait donc les donnees recues.

**Solution** : Sauvegarder les donnees saisies dans des variables Working-Storage (prefixe WS-) immediatement apres le `RECEIVE MAP`, avant tout `MOVE LOW-VALUES`.

```cobol
      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
      * Sauvegarder aussi les champs longueur (suffixe L) pour les validations
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE NUMCPTL   TO WS-NUMCPTL
           MOVE CODREGI   TO WS-CODREG
           MOVE CODREGL   TO WS-CODREGL
           MOVE NOMI      TO WS-NOM
           MOVE NOML      TO WS-NOML
           MOVE SEXEI     TO WS-SEXE
           MOVE SEXEL     TO WS-SEXEL
           MOVE SITSOI    TO WS-SITSO
           MOVE SITSOL    TO WS-SITSOL
           MOVE POSITI    TO WS-POSITION
           MOVE POSITL    TO WS-POSITL
```

#### Probleme 2 : Validations ignorees - le client etait ajoute malgre les erreurs

**Symptome** : Meme avec des donnees invalides (sexe = 'X'), le client etait ajoute dans le fichier. Les messages d'erreur s'affichaient dans CEDF mais le programme continuait jusqu'au WRITE.

**Cause** : Le `GO TO paragraphe-FIN` dans les validations sortait de la plage du `PERFORM`, ce qui faisait continuer le programme sequentiellement vers les paragraphes suivants (2200, 2300, 2400...) au lieu de retourner a l'appelant.

En COBOL, quand on fait :
```cobol
       PERFORM 2100-VALIDER-DONNEES
```

Et dans 2100-VALIDER-DONNEES on fait :
```cobol
       GO TO 2100-FIN
```

Le `GO TO` sort du PERFORM car `2100-FIN` est un paragraphe separe. Le programme continue alors sequentiellement apres 2100-FIN.

**Solution** : Utiliser la clause `THRU` pour inclure le paragraphe FIN dans la plage du PERFORM :

```cobol
       PERFORM 2000-TRAITEMENT THRU 2000-FIN
       ...
       PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
       ...
       PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
```

Avec `THRU`, le `GO TO 2100-FIN` reste dans la plage du PERFORM, et apres le `EXIT` de 2100-FIN, le controle retourne correctement a l'appelant.

#### Probleme 3 : Message d'erreur non visible sans CEDF

**Symptome** : Le message d'erreur de validation s'affichait dans CEDF mais pas sur l'ecran normal.

**Cause** : Le `SEND MAP` apres detection d'erreur n'avait pas l'option `ERASE`, donc l'ecran precedent restait visible.

**Solution** : Ajouter `ERASE` au `SEND MAP` d'erreur :

```cobol
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF
```

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex07-1 : Source COBOL dans ISPF EDIT - ROCHA.CICS.SOURCE(PRGAJT)
2. pt2ex07-2 : Soumission JCL CMPAJT - compilation du programme
3. pt2ex07-3 : SDSF - Job output avec RC=0000 pour compilation
4. pt2ex07-4 : Verification ROCHA.CICS.LOAD - membre PRGAJT present
5. pt2ex07-5 : Ecran MAPAJT vide - premier passage (message "SAISIR LES DONNEES...")
6. pt2ex07-6 : Test erreur de validation - message "SEXE INVALIDE"
7. pt2ex07-7 : Test doublon - message "ENREGISTREMENT EN DOUBLE"
8. pt2ex07-8 : Ajout reussi - message "CLIENT AJOUTE AVEC SUCCES"
-->

---

## Exercice 8 : Transaction d'ajout

### Enonce

Suivre cette operation par l'ajout d'une nouvelle Transaction dans le GROUP et activer la transaction en mode debugger CEDF et sans debugger.

### Mon travail

Pour que la transaction AJOU fonctionne, je dois definir et installer trois ressources CICS :

1. **MAPSET CLIAJT** : L'ecran BMS compile (exercice 6)
2. **PROGRAM PRGAJT** : Le programme COBOL-CICS compile (exercice 7)
3. **TRANSACTION AJOU** : Le code de 4 caracteres qui lance le programme

L'ordre de definition est important : le programme doit etre defini avant la transaction (car TRANSACTION reference PROGRAM).

### Resolution

**Etape 1 : Definition des ressources**

```
CEDA DEFINE MAPSET(CLIAJT) GROUP(CLIGROUP)

CEDA DEFINE PROGRAM(PRGAJT) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA DEFINE TRANSACTION(AJOU) GROUP(CLIGROUP)
     PROGRAM(PRGAJT)
```

**Etape 2 : Installation des ressources**

*Option A : Installation individuelle (recommandee)*

```
CEDA INSTALL MAPSET(CLIAJT) GROUP(CLIGROUP)
CEDA INSTALL PROGRAM(PRGAJT) GROUP(CLIGROUP)
CEDA INSTALL TRANSACTION(AJOU) GROUP(CLIGROUP)
```

*Option B : Installation du groupe complet*

```
CEDA INSTALL GROUP(CLIGROUP)
```

> **Note** : Si certaines ressources sont deja installees (FCLIENT, CLIAFF, PRGCLIA, AFFI), des erreurs "ALREADY INSTALLED" apparaitront. C'est normal et les nouvelles ressources seront quand meme installees.

**Etape 3 : Verification avec CEMT et CEDA**

```
CEDA VIEW MAPSET(CLIAJT) GROUP(CLIGROUP)
```
Resultat attendu : Affichage de la definition du mapset

```
CEMT INQ PROG(PRGAJT)
```
Resultat attendu : `Pro(PRGAJT) Len(...) Cob Ena Pri`

```
CEMT INQ TRAN(AJOU)
```
Resultat attendu : `Tra(AJOU) Pro(PRGAJT) Ena`

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour verifier un mapset, utiliser `CEDA VIEW MAPSET(nom) GROUP(groupe)`.

**Tableau recapitulatif du groupe CLIGROUP apres exercice 8 :**

| Ressource | Nom | Defini dans | Description |
|-----------|-----|-------------|-------------|
| FILE | FCLIENT | Exercice 1 | Fichier VSAM CLIENT |
| MAPSET | CLIAFF | Exercice 4 | Ecran d'affichage |
| PROGRAM | PRGCLIA | Exercice 4 | Programme d'affichage |
| TRANSACTION | AFFI | Exercice 4 | Transaction d'affichage |
| MAPSET | CLIAJT | Exercice 8 | Ecran d'ajout |
| PROGRAM | PRGAJT | Exercice 8 | Programme d'ajout |
| TRANSACTION | AJOU | Exercice 8 | Transaction d'ajout |

**Etape 4 : Test avec CEDF**

```
CEDF
AJOU
```

Observer les points d'arret :
1. SEND MAP (ecran vide)
2. RETURN TRANSID (fin premier passage)
3. RECEIVE MAP (reception saisie)
4. READ FILE (verification doublure)
5. WRITE FILE (ecriture client)
6. SEND MAP (message succes)
7. RETURN TRANSID (fin traitement)

> **Note importante sur NOTFND** : Lors du point d'arret 4 (READ FILE), CEDF affiche souvent une reponse `NOTFND`. C'est le comportement **attendu et normal** ! Ce READ sert a verifier que le client n'existe pas deja (controle de doublure). Si NOTFND est retourne, cela signifie que le numero de compte est disponible et que le programme peut proceder au WRITE. Ce n'est pas une erreur mais une verification reussie.

**Etape 5 : Test sans debugger**

Depuis un ecran CICS vierge (sans CEDF actif) :

```
AJOU
```

Tester les scenarios suivants :
- Saisir un nouveau client complet et valider → message "CLIENT AJOUTE AVEC SUCCES"
- Ressaisir le meme numero → message "ENREGISTREMENT EN DOUBLE"
- Saisir un sexe invalide → message "SEXE INVALIDE"
- Appuyer ENTER sans rien saisir → message "AUCUNE DONNEE SAISIE"
- Appuyer PF3 → fin de la transaction

### Captures d'ecran

<!--
Suggestions de captures d'ecran pour cet exercice :

1. pt2ex08-1 : CEDA DEFINE MAPSET(CLIAJT) - definition du mapset
2. pt2ex08-2 : CEDA DEFINE PROGRAM(PRGAJT) - definition du programme
3. pt2ex08-3 : CEDA DEFINE TRANSACTION(AJOU) - definition de la transaction
4. pt2ex08-4 : CEDA INSTALL avec message de succes
5. pt2ex08-5 : CEMT INQ TRAN(AJOU) - verification transaction active
6. pt2ex08-6 : Test CEDF - point d'arret sur WRITE FILE
7. pt2ex08-7 : Ecran MAPAJT - saisie d'un nouveau client
8. pt2ex08-8 : Message "CLIENT AJOUTE AVEC SUCCES" apres ajout
9. pt2ex08-9 : Verification avec AFFI - le nouveau client existe
-->

---

[< Partie 1 : Affichage](02-partie-1-affichage.md) | [Retour au sommaire](00-introduction.md) | [Partie 2b : Mise a jour >](04-partie-2b-maj.md)
