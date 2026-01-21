       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGLGEN.
      ******************************************************************
      * PROGRAMME : PRGLGEN
      * FONCTION  : Liste generique des clients par prefixe
      * TRANSACTION : LGEN
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPLGEN (MAPSET CLILIST)
      *
      * MODE PSEUDO-CONVERSATIONNEL AVEC PAGINATION :
      * ---------------------------------------------
      * - Saisie d'un prefixe (1 a 6 caracteres)
      * - Affichage de 10 clients par page
      * - Navigation : PF7 (page prec.) / PF8 (page suiv.)
      *
      * COMMAREA :
      * - Sauvegarde du prefixe et de la position de navigation
      * - Permet de parcourir tout le fichier par pages
      *
      * OPTIMISATIONS IMPLEMENTEES :
      * ---------------------------
      * 1. FSET dans BMS : PREFIXE renvoye automatiquement
      * 2. DATAONLY : Reaffichage sans renvoyer la structure map
      * 3. CURSOR dynamique : Positionnement sur le champ en erreur
      * 4. REDEFINES sur MAPLGENO : Acces indexe aux lignes
      *    -> Remplace 60 MOVE par 6 MOVE dans une boucle PERFORM
      *
      * FIL ROUGE CICS - EXERCICE 18
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * CONSTANTES
      *-----------------------------------------------------------------
       01  WS-LIGNES-PAR-PAGE    PIC 9(02) VALUE 10.

      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la position de navigation entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PREFIXE-SAVED   PIC X(06) VALUE SPACES.
           05 WS-LONGUEUR-SAVED  PIC 9(01) VALUE 0.
           05 WS-DERNIERE-CLE    PIC X(06) VALUE SPACES.
           05 WS-PAGE-COURANTE   PIC 9(03) VALUE 0.
           05 WS-TOTAL-CLIENTS   PIC 9(05) VALUE 0.
           05 WS-TOTAL-PAGES     PIC 9(03) VALUE 0.
           05 WS-FIN-FICHIER     PIC X(01) VALUE 'N'.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
       COPY DFHBMSCA.

      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      *-----------------------------------------------------------------
       COPY CLILIST.

      *-----------------------------------------------------------------
      * REDEFINES POUR ACCES INDEXE AUX LIGNES DE LA MAP
      * Structure : PREFIXE(9) + LIGNES(580) + PAGINATION(20) + MSG(63)
      * Chaque ligne = 58 octets (6 champs x (L:2 + A:1 + O:var))
      *-----------------------------------------------------------------
       01  MAPLGENO-REDEF REDEFINES MAPLGENO.
           05 FILLER                   PIC X(09).
           05 MAP-LIGNES-ZONE.
              10 MAP-LN OCCURS 10 TIMES.
      *          NUMCPT : L(2) + A(1) + O(6) = 9
                 15 MAP-LN-NUML        PIC S9(4) COMP.
                 15 MAP-LN-NUMA        PIC X.
                 15 MAP-LN-NUMO        PIC X(06).
      *          REGION : L(2) + A(1) + O(2) = 5
                 15 MAP-LN-REGL        PIC S9(4) COMP.
                 15 MAP-LN-REGA        PIC X.
                 15 MAP-LN-REGO        PIC X(02).
      *          NOM : L(2) + A(1) + O(10) = 13
                 15 MAP-LN-NOML        PIC S9(4) COMP.
                 15 MAP-LN-NOMA        PIC X.
                 15 MAP-LN-NOMO        PIC X(10).
      *          PRENOM : L(2) + A(1) + O(10) = 13
                 15 MAP-LN-PREL        PIC S9(4) COMP.
                 15 MAP-LN-PREA        PIC X.
                 15 MAP-LN-PREO        PIC X(10).
      *          SOLDE : L(2) + A(1) + O(10) = 13
                 15 MAP-LN-SOLL        PIC S9(4) COMP.
                 15 MAP-LN-SOLA        PIC X.
                 15 MAP-LN-SOLO        PIC X(10).
      *          POSITION : L(2) + A(1) + O(2) = 5
                 15 MAP-LN-POSL        PIC S9(4) COMP.
                 15 MAP-LN-POSA        PIC X.
                 15 MAP-LN-POSO        PIC X(02).
           05 FILLER                   PIC X(83).

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
           VALUE 'TRANSACTION LGEN TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * VARIABLES POUR LA NAVIGATION VSAM
      *-----------------------------------------------------------------
       01  WS-BROWSE.
           05 WS-CLE-DEBUT       PIC X(06) VALUE SPACES.
           05 WS-CLE-COURANTE    PIC X(06) VALUE SPACES.
           05 WS-FIN-BROWSE      PIC X(01) VALUE 'N'.
              88 FIN-BROWSE      VALUE 'O'.
              88 PAS-FIN-BROWSE  VALUE 'N'.

      *-----------------------------------------------------------------
      * VARIABLES DE SAISIE
      *-----------------------------------------------------------------
       01  WS-SAISIE.
           05 WS-PREFIXE         PIC X(06).
           05 WS-PREFIXEL        PIC S9(04) COMP.

      *-----------------------------------------------------------------
      * LONGUEUR DU PREFIXE
      *-----------------------------------------------------------------
       01  WS-LONGUEUR           PIC 9(01) VALUE 0.
       01  WS-INDEX              PIC 9(02) VALUE 0.
       01  WS-IDX-LN             PIC 9(02) VALUE 0.

      *-----------------------------------------------------------------
      * COMPTEURS
      *-----------------------------------------------------------------
       01  WS-COMPTEURS.
           05 WS-COMPTEUR        PIC 9(05) VALUE 0.
           05 WS-LIGNE-COURANTE  PIC 9(02) VALUE 0.
           05 WS-CLIENTS-SAUVES  PIC 9(02) VALUE 0.

      *-----------------------------------------------------------------
      * TABLE DES CLIENTS A AFFICHER (10 MAX)
      *-----------------------------------------------------------------
       01  WS-TABLE-CLIENTS.
           05 WS-CLI OCCURS 10 TIMES.
              10 WS-CLI-NUM      PIC X(06).
              10 WS-CLI-REG      PIC X(02).
              10 WS-CLI-NOM      PIC X(10).
              10 WS-CLI-PRE      PIC X(10).
              10 WS-CLI-SOL      PIC X(10).
              10 WS-CLI-POS      PIC X(02).

      *-----------------------------------------------------------------
      * MESSAGE FORMATE
      *-----------------------------------------------------------------
       01  WS-MSG-RESULT         PIC X(60) VALUE SPACES.

      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
       01  DFHCOMMAREA.
           05 LS-PREFIXE-SAVED   PIC X(06).
           05 LS-LONGUEUR-SAVED  PIC 9(01).
           05 LS-DERNIERE-CLE    PIC X(06).
           05 LS-PAGE-COURANTE   PIC 9(03).
           05 LS-TOTAL-CLIENTS   PIC 9(05).
           05 LS-TOTAL-PAGES     PIC 9(03).
           05 LS-FIN-FICHIER     PIC X(01).

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
               TRANSID('LGEN')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-INIT.
      *-----------------------------------------------------------------
      * Affichage ecran initial
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPLGENO
           MOVE 'SAISIR UN PREFIXE (1-6 CAR) ET APPUYER SUR ENTER'
               TO MSGO
           MOVE SPACES TO WS-PREFIXE-SAVED
           MOVE 0 TO WS-LONGUEUR-SAVED
           MOVE SPACES TO WS-DERNIERE-CLE
           MOVE 0 TO WS-PAGE-COURANTE
           MOVE 0 TO WS-TOTAL-CLIENTS
           MOVE 0 TO WS-TOTAL-PAGES
           MOVE 'N' TO WS-FIN-FICHIER

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Aiguillage selon la touche pressee
      *-----------------------------------------------------------------
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 3000-CHERCHER THRU 3000-FIN
               WHEN DFHPF8
                   PERFORM 4000-PAGE-SUIVANTE THRU 4000-FIN
               WHEN DFHPF7
                   PERFORM 5000-PAGE-PRECEDENTE THRU 5000-FIN
               WHEN OTHER
                   MOVE LOW-VALUES TO MAPLGENO
                   MOVE 'TOUCHE NON RECONNUE - UTILISER ENTER/PF7/PF8/PF3'
                       TO MSGO
                   PERFORM 6100-RESTAURER-AFFICHAGE
           END-EVALUATE.

      *-----------------------------------------------------------------
       3000-CHERCHER.
      *-----------------------------------------------------------------
      * Nouvelle recherche avec prefixe saisi
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPLGEN')
               MAPSET('CLILIST')
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'VEUILLEZ SAISIR UN PREFIXE' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   DATAONLY
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarde du prefixe saisi
           MOVE PREFIXEI TO WS-PREFIXE
           MOVE PREFIXEL TO WS-PREFIXEL

      *    Controle prefixe non vide
           IF WS-PREFIXEL = 0 OR WS-PREFIXE = SPACES
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREFIXE OBLIGATOIRE (1 A 6 CARACTERES)' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   DATAONLY
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calcul de la longueur effective du prefixe
           PERFORM 3050-CALCULER-LONGUEUR

      *    Controle : au moins 1 caractere
           IF WS-LONGUEUR = 0
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREFIXE INVALIDE - MIN 1 CARACTERE' TO MSGO
               MOVE -1 TO PREFIXEL
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   DATAONLY
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Sauvegarder le prefixe pour la navigation
           MOVE WS-PREFIXE TO WS-PREFIXE-SAVED
           MOVE WS-LONGUEUR TO WS-LONGUEUR-SAVED

      *    Compter le total de clients
           PERFORM 3100-COMPTER-TOTAL

           IF WS-TOTAL-CLIENTS = 0
      *        Reinitialiser la COMMAREA AVANT le SEND MAP
               MOVE SPACES TO WS-PREFIXE-SAVED
               MOVE 0 TO WS-LONGUEUR-SAVED
               MOVE SPACES TO WS-DERNIERE-CLE
               MOVE 0 TO WS-PAGE-COURANTE
               MOVE 0 TO WS-TOTAL-PAGES
               MOVE 'N' TO WS-FIN-FICHIER
      *        Preparer l'ecran
               MOVE LOW-VALUES TO MAPLGENO
               MOVE WS-PREFIXE TO PREFIXEO
               MOVE 'AUCUN CLIENT TROUVE - SAISIR AUTRE PREFIXE' TO MSGO
      *        Envoyer l'ecran (simplifie comme PRGDELG)
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   ERASE
               END-EXEC
               GO TO 3000-FIN
           END-IF

      *    Calculer le nombre de pages
           DIVIDE WS-TOTAL-CLIENTS BY WS-LIGNES-PAR-PAGE
               GIVING WS-TOTAL-PAGES REMAINDER WS-INDEX
           IF WS-INDEX > 0
               ADD 1 TO WS-TOTAL-PAGES
           END-IF

      *    Afficher la premiere page
           MOVE 1 TO WS-PAGE-COURANTE
           MOVE 'N' TO WS-FIN-FICHIER
           PERFORM 6000-AFFICHER-PAGE.

       3000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3050-CALCULER-LONGUEUR.
      *-----------------------------------------------------------------
           MOVE 0 TO WS-LONGUEUR
           PERFORM VARYING WS-INDEX FROM 6 BY -1
               UNTIL WS-INDEX < 1 OR WS-LONGUEUR > 0
               IF WS-PREFIXE(WS-INDEX:1) NOT = SPACE
                   MOVE WS-INDEX TO WS-LONGUEUR
               END-IF
           END-PERFORM.

      *-----------------------------------------------------------------
       3100-COMPTER-TOTAL.
      *-----------------------------------------------------------------
      * Compte le nombre total de clients correspondant au prefixe
      *-----------------------------------------------------------------
           MOVE 0 TO WS-TOTAL-CLIENTS
           MOVE 'N' TO WS-FIN-BROWSE

      *    Construction de la cle de debut
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT
           MOVE WS-LONGUEUR TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3100-FIN
           END-IF

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

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
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR-SAVED) NOT =
                       WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       ADD 1 TO WS-TOTAL-CLIENTS
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.

       3100-FIN.
           EXIT.

      *-----------------------------------------------------------------
       4000-PAGE-SUIVANTE.
      *-----------------------------------------------------------------
      * PF8 - Afficher la page suivante
      *-----------------------------------------------------------------
           IF WS-PAGE-COURANTE >= WS-TOTAL-PAGES
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'DERNIERE PAGE ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 4000-FIN
           END-IF

           IF WS-FIN-FICHIER = 'O'
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'FIN DU FICHIER ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 4000-FIN
           END-IF

           ADD 1 TO WS-PAGE-COURANTE
           PERFORM 6000-AFFICHER-PAGE.

       4000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       5000-PAGE-PRECEDENTE.
      *-----------------------------------------------------------------
      * PF7 - Afficher la page precedente
      *-----------------------------------------------------------------
           IF WS-PAGE-COURANTE <= 1
               MOVE LOW-VALUES TO MAPLGENO
               MOVE 'PREMIERE PAGE ATTEINTE' TO MSGO
               PERFORM 6100-RESTAURER-AFFICHAGE
               GO TO 5000-FIN
           END-IF

           SUBTRACT 1 FROM WS-PAGE-COURANTE
           MOVE 'N' TO WS-FIN-FICHIER
           PERFORM 6000-AFFICHER-PAGE.

       5000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       6000-AFFICHER-PAGE.
      *-----------------------------------------------------------------
      * Affiche la page courante (10 clients)
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPLGENO
           MOVE 0 TO WS-LIGNE-COURANTE
           MOVE 'N' TO WS-FIN-BROWSE

      *    Initialiser la table des clients
           INITIALIZE WS-TABLE-CLIENTS

      *    Construction de la cle de debut
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED) TO WS-CLE-DEBUT
           MOVE WS-LONGUEUR-SAVED TO WS-INDEX
           ADD 1 TO WS-INDEX
           PERFORM UNTIL WS-INDEX > 6
               MOVE '0' TO WS-CLE-DEBUT(WS-INDEX:1)
               ADD 1 TO WS-INDEX
           END-PERFORM

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR POSITIONNEMENT FICHIER' TO MSGO
               EXEC CICS SEND MAP('MAPLGEN')
                   MAPSET('CLILIST')
                   FROM(MAPLGENO)
                   FREEKB
                   CURSOR
                   ERASE
               END-EXEC
               GO TO 6000-FIN
           END-IF

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

      *    Sauter les enregistrements des pages precedentes
           COMPUTE WS-COMPTEUR = (WS-PAGE-COURANTE - 1) * 10
           PERFORM WS-COMPTEUR TIMES
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'O' TO WS-FIN-BROWSE
               END-IF
           END-PERFORM

      *    Lire les 10 clients de cette page
           PERFORM UNTIL FIN-BROWSE OR WS-LIGNE-COURANTE >= 10
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                       MOVE 'O' TO WS-FIN-FICHIER
                   WHEN WS-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR-SAVED) NOT =
                       WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED)
                       MOVE 'O' TO WS-FIN-BROWSE
                       MOVE 'O' TO WS-FIN-FICHIER
                   WHEN OTHER
                       ADD 1 TO WS-LIGNE-COURANTE
                       MOVE CLI-NUMCPT TO WS-CLI-NUM(WS-LIGNE-COURANTE)
                       MOVE CLI-CODREG TO WS-CLI-REG(WS-LIGNE-COURANTE)
                       MOVE CLI-NOM TO WS-CLI-NOM(WS-LIGNE-COURANTE)
                       MOVE CLI-PRENOM TO WS-CLI-PRE(WS-LIGNE-COURANTE)
                       MOVE CLI-SOLDE TO WS-CLI-SOL(WS-LIGNE-COURANTE)
                       MOVE CLI-POSITION TO WS-CLI-POS(WS-LIGNE-COURANTE)
                       MOVE WS-CLE-COURANTE TO WS-DERNIERE-CLE
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

      *    Transferer les donnees vers la MAP via REDEFINES
      *    (Optimisation : 60 MOVE -> 6 MOVE dans une boucle)
           PERFORM VARYING WS-IDX-LN FROM 1 BY 1 UNTIL WS-IDX-LN > 10
               MOVE WS-CLI-NUM(WS-IDX-LN) TO MAP-LN-NUMO(WS-IDX-LN)
               MOVE WS-CLI-REG(WS-IDX-LN) TO MAP-LN-REGO(WS-IDX-LN)
               MOVE WS-CLI-NOM(WS-IDX-LN) TO MAP-LN-NOMO(WS-IDX-LN)
               MOVE WS-CLI-PRE(WS-IDX-LN) TO MAP-LN-PREO(WS-IDX-LN)
               MOVE WS-CLI-SOL(WS-IDX-LN) TO MAP-LN-SOLO(WS-IDX-LN)
               MOVE WS-CLI-POS(WS-IDX-LN) TO MAP-LN-POSO(WS-IDX-LN)
           END-PERFORM

      *    Informations de pagination
           MOVE WS-PREFIXE-SAVED TO PREFIXEO
           MOVE WS-PAGE-COURANTE TO PAGNUMO
           MOVE WS-TOTAL-PAGES TO PAGTOTO
           MOVE WS-TOTAL-CLIENTS TO CLITOTO

      *    Message
           IF WS-FIN-FICHIER = 'O'
               MOVE 'FIN DE LISTE - PF7 POUR REVENIR' TO MSGO
           ELSE
               MOVE 'PF7=PREC  PF8=SUIV  PF3=QUITTER' TO MSGO
           END-IF

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
               FREEKB
               CURSOR
               ERASE
           END-EXEC.

       6000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       6100-RESTAURER-AFFICHAGE.
      *-----------------------------------------------------------------
      * Restaure l'affichage de la page courante avec un message
      *-----------------------------------------------------------------
           MOVE WS-PREFIXE-SAVED TO PREFIXEO
           MOVE WS-PAGE-COURANTE TO PAGNUMO
           MOVE WS-TOTAL-PAGES TO PAGTOTO
           MOVE WS-TOTAL-CLIENTS TO CLITOTO

           EXEC CICS SEND MAP('MAPLGEN')
               MAPSET('CLILIST')
               FROM(MAPLGENO)
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
