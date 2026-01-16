       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGSTAT.
      ******************************************************************
      * PROGRAMME : PRGSTAT
      * FONCTION  : Statistiques clients par region
      * TRANSACTION : STAT
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPSTAT (MAPSET CLISTAT)
      *
      * MODE PSEUDO-CONVERSATIONNEL :
      * -----------------------------
      * - Saisie d'un code region (01, 02, 03 ou 04)
      * - Parcours complet du fichier avec STARTBR/READNEXT
      * - Calcul et affichage des statistiques :
      *   - Nombre total de clients de la region
      *   - Nombre et somme des clients debiteurs (DB)
      *   - Nombre et somme des clients crediteurs (CR)
      *
      * REGIONS :
      * 01 - Paris      02 - Marseille
      * 03 - Lyon       04 - Lille
      *
      * FIL ROUGE CICS - EXERCICE 19
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde le code region entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-CODE-REGION-SAVED PIC X(02) VALUE SPACES.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      *-----------------------------------------------------------------
       COPY CLISTAT.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
       01  ENR-CLIENT.
           05 CLI-NUMCPT         PIC X(06).
           05 CLI-CODREG         PIC X(02).
           05 CLI-NATCPT         PIC X(02).
           05 CLI-NOM            PIC X(10).
           05 CLI-PRENOM         PIC X(10).
           05 CLI-DATNAISS       PIC X(08).
           05 CLI-SEXE           PIC X(01).
           05 CLI-ACTPRO         PIC X(02).
           05 CLI-SITSO          PIC X(01).
           05 CLI-ADRESSE        PIC X(10).
           05 CLI-SOLDE          PIC X(10).
           05 CLI-POSITION       PIC X(02).
           05 FILLER             PIC X(16).

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP               PIC S9(08) COMP VALUE 0.
       01  WS-MSG-FIN            PIC X(40)
           VALUE 'TRANSACTION STAT TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * VARIABLES POUR LA NAVIGATION VSAM
      *-----------------------------------------------------------------
       01  WS-BROWSE.
           05 WS-CLE-DEBUT       PIC X(06) VALUE LOW-VALUES.
           05 WS-CLE-COURANTE    PIC X(06) VALUE SPACES.
           05 WS-FIN-BROWSE      PIC X(01) VALUE 'N'.
              88 FIN-BROWSE      VALUE 'O'.
              88 PAS-FIN-BROWSE  VALUE 'N'.

      *-----------------------------------------------------------------
      * VARIABLES DE SAISIE
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-CODE-REGION     PIC X(02).
           05 WS-CODE-REGIONL    PIC S9(04) COMP.

      *-----------------------------------------------------------------
      * TABLE DES NOMS DE REGIONS
      *-----------------------------------------------------------------
       01  WS-TABLE-REGIONS.
           05 FILLER             PIC X(17) VALUE '01PARIS          '.
           05 FILLER             PIC X(17) VALUE '02MARSEILLE      '.
           05 FILLER             PIC X(17) VALUE '03LYON           '.
           05 FILLER             PIC X(17) VALUE '04LILLE          '.
       01  WS-TAB-REGIONS REDEFINES WS-TABLE-REGIONS.
           05 WS-REGION OCCURS 4 TIMES.
              10 WS-REG-CODE     PIC X(02).
              10 WS-REG-NOM      PIC X(15).

      *-----------------------------------------------------------------
      * STATISTIQUES CALCULEES
      *-----------------------------------------------------------------
       01  WS-STATS.
           05 WS-NB-TOTAL        PIC 9(05) VALUE 0.
           05 WS-NB-DEBITEURS    PIC 9(05) VALUE 0.
           05 WS-MT-DEBITEURS    PIC 9(12) VALUE 0.
           05 WS-NB-CREDITEURS   PIC 9(05) VALUE 0.
           05 WS-MT-CREDITEURS   PIC 9(12) VALUE 0.

      *-----------------------------------------------------------------
      * VARIABLES POUR CONVERSION SOLDE
      *-----------------------------------------------------------------
       01  WS-SOLDE-NUM          PIC 9(10) VALUE 0.
       01  WS-NOM-REGION         PIC X(15) VALUE SPACES.
       01  WS-INDEX              PIC 9(01) VALUE 0.

      *-----------------------------------------------------------------
      * FORMATS D'AFFICHAGE
      *-----------------------------------------------------------------
       01  WS-MT-EDIT            PIC ZZZ,ZZZ,ZZZ,ZZ9.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  DFHCOMMAREA.
           05 LS-CODE-REGION-SAVED PIC X(02).

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Affichage initial
                   PERFORM 1000-INIT
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Reinitialiser
                   PERFORM 1000-INIT
               WHEN OTHER
      *            Restaurer la COMMAREA et traiter
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('STAT')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT.
      *-----------------------------------------------------------------
      * Affichage ecran initial
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSTATO
           MOVE 'SAISIR UN CODE REGION (01, 02, 03 OU 04)'
               TO MSGO
           MOVE SPACES TO WS-CODE-REGION-SAVED

           EXEC CICS SEND MAP('MAPSTAT')
               MAPSET('CLISTAT')
               FROM(MAPSTATO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Recevoir la saisie et calculer les statistiques
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPSTAT')
               MAPSET('CLISTAT')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPSTATO
               MOVE 'VEUILLEZ SAISIR UN CODE REGION' TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Sauvegarde du code region saisi
           MOVE CODREGI TO WS-CODE-REGION
           MOVE CODREGL TO WS-CODE-REGIONL

      *    Controle code region non vide
           IF WS-CODE-REGIONL = 0 OR WS-CODE-REGION = SPACES
               MOVE LOW-VALUES TO MAPSTATO
               MOVE 'CODE REGION OBLIGATOIRE (01, 02, 03 OU 04)' TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Verification que le code region est valide
           PERFORM 2100-VERIFIER-REGION

           IF WS-NOM-REGION = SPACES
               MOVE LOW-VALUES TO MAPSTATO
               MOVE WS-CODE-REGION TO CODREGO
               MOVE 'CODE REGION INVALIDE - SAISIR 01, 02, 03 OU 04'
                   TO MSGO
               EXEC CICS SEND MAP('MAPSTAT')
                   MAPSET('CLISTAT')
                   FROM(MAPSTATO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF

      *    Sauvegarder pour la COMMAREA
           MOVE WS-CODE-REGION TO WS-CODE-REGION-SAVED

      *    Calculer les statistiques
           PERFORM 3000-CALCULER-STATS

      *    Afficher les resultats
           PERFORM 4000-AFFICHER-RESULTATS.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       2100-VERIFIER-REGION.
      *-----------------------------------------------------------------
      * Verifie que le code region est valide et recupere le nom
      *-----------------------------------------------------------------
           MOVE SPACES TO WS-NOM-REGION

           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 4 OR WS-NOM-REGION NOT = SPACES
               IF WS-CODE-REGION = WS-REG-CODE(WS-INDEX)
                   MOVE WS-REG-NOM(WS-INDEX) TO WS-NOM-REGION
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       3000-CALCULER-STATS.
      *-----------------------------------------------------------------
      * Parcours complet du fichier pour calculer les statistiques
      *-----------------------------------------------------------------
           INITIALIZE WS-STATS
           MOVE 'N' TO WS-FIN-BROWSE

      *    Positionner au debut du fichier
           MOVE LOW-VALUES TO WS-CLE-DEBUT

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               RESP(WS-RESP)
           END-EXEC

      *    Gestion explicite des erreurs STARTBR
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
      *            Fichier vide
                   GO TO 3000-FIN
               WHEN DFHRESP(ENDFILE)
      *            Fichier vide
                   GO TO 3000-FIN
               WHEN OTHER
      *            Autre erreur
                   GO TO 3000-FIN
           END-EVALUATE

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

      *    Boucle de lecture de tous les enregistrements
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Verifier si le client est de la region demandee
                       IF CLI-CODREG = WS-CODE-REGION
                           ADD 1 TO WS-NB-TOTAL
      *                    Convertir le solde en numerique
                           PERFORM 3100-CONVERTIR-SOLDE
      *                    Verifier si debiteur ou crediteur
                           IF CLI-POSITION = 'DB'
                               ADD 1 TO WS-NB-DEBITEURS
                               ADD WS-SOLDE-NUM TO WS-MT-DEBITEURS
                           ELSE
                               ADD 1 TO WS-NB-CREDITEURS
                               ADD WS-SOLDE-NUM TO WS-MT-CREDITEURS
                           END-IF
                       END-IF
               END-EVALUATE
           END-PERFORM

      *    Fermeture du browse
           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3100-CONVERTIR-SOLDE.
      *-----------------------------------------------------------------
      * Convertit le solde texte en numerique
      * Le solde est stocke en PIC X(10), format numerique
      *-----------------------------------------------------------------
           MOVE 0 TO WS-SOLDE-NUM
           MOVE FUNCTION NUMVAL(CLI-SOLDE) TO WS-SOLDE-NUM.

      *-----------------------------------------------------------------
       4000-AFFICHER-RESULTATS.
      *-----------------------------------------------------------------
      * Affiche les resultats des statistiques
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPSTATO

      *    Code et nom de la region
           MOVE WS-CODE-REGION TO CODREGO
           MOVE WS-NOM-REGION TO NOMREGO

      *    Statistiques totales
           MOVE WS-NB-TOTAL TO NBTOTO

      *    Statistiques debiteurs
           MOVE WS-NB-DEBITEURS TO NBDBO
           MOVE WS-MT-DEBITEURS TO WS-MT-EDIT
           MOVE WS-MT-EDIT TO MTDBO

      *    Statistiques crediteurs
           MOVE WS-NB-CREDITEURS TO NBCRO
           MOVE WS-MT-CREDITEURS TO WS-MT-EDIT
           MOVE WS-MT-EDIT TO MTCRO

      *    Message de resultat
           IF WS-NB-TOTAL = 0
               MOVE 'AUCUN CLIENT DANS CETTE REGION' TO MSGO
           ELSE
               MOVE 'STATISTIQUES CALCULEES AVEC SUCCES' TO MSGO
           END-IF

           EXEC CICS SEND MAP('MAPSTAT')
               MAPSET('CLISTAT')
               FROM(MAPSTATO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       9000-FIN-PROGRAMME.
      *-----------------------------------------------------------------
           EXEC CICS SEND TEXT
               FROM(WS-MSG-FIN)
               LENGTH(40)
               ERASE
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.
