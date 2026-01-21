       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGCLIA.
      ******************************************************************
      * PROGRAMME : PRGCLIA
      * FONCTION  : Affichage d'un client par numero de compte
      * TRANSACTION : AFFI
      * FICHIER   : FCLIENT (VSAM KSDS)
      * MAP       : MAPAFF (MAPSET CLIAFF)
      *
      * MODE PSEUDO-CONVERSATIONNEL :
      *   - Premier passage : Affiche ecran vide
      *   - Passages suivants : Lit et affiche le client
      *   - PF3 : Quitter la transaction
      *
      * OPTIMISATIONS IMPLEMENTEES :
      * ---------------------------
      * 1. FSET dans BMS : NUMCPT renvoye automatiquement
      * 2. DATAONLY : Reaffichage sans renvoyer la structure map
      * 3. CURSOR dynamique : Positionnement sur le champ en erreur
      *
      * FIL ROUGE CICS - EXERCICE 3
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-FLAG-INIT         PIC X(01) VALUE 'N'.
              88 PREMIER-PASSAGE   VALUE 'N'.
              88 PASSAGE-SUIVANT   VALUE 'O'.

      *-----------------------------------------------------------------
      * COPYBOOKS CICS
      *-----------------------------------------------------------------
       COPY DFHAID.
      *-----------------------------------------------------------------
      * COPYBOOK GENERE PAR ASSEMBLAGE BMS (DSECT)
      * Stocke dans ROCHA.CICS.LINK(CLIAFF)
      *-----------------------------------------------------------------
       COPY CLIAFF.

      *-----------------------------------------------------------------
      * STRUCTURE ENREGISTREMENT CLIENT (80 OCTETS)
      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
      * VARIABLES DE TRAVAIL
      *-----------------------------------------------------------------
       01  WS-RESP                 PIC S9(08) COMP VALUE 0.
       01  WS-NUMCPT               PIC X(06) VALUE SPACES.
       01  WS-MSG-FIN              PIC X(40)
           VALUE 'TRANSACTION AFFI TERMINEE - AU REVOIR'.

      *-----------------------------------------------------------------
      * LIBELLES POUR AFFICHAGE
      *-----------------------------------------------------------------
       01  WS-LIB-REGION.
           05 FILLER               PIC X(17) VALUE '01 - PARIS      '.
           05 FILLER               PIC X(17) VALUE '02 - MARSEILLE  '.
           05 FILLER               PIC X(17) VALUE '03 - LYON       '.
           05 FILLER               PIC X(17) VALUE '04 - LILLE      '.
       01  WS-TAB-REGION REDEFINES WS-LIB-REGION.
           05 WS-REGION            PIC X(17) OCCURS 4.

       01  WS-LIB-SEXE.
           05 FILLER               PIC X(08) VALUE 'MASCULIN'.
           05 FILLER               PIC X(08) VALUE 'FEMININ '.
       01  WS-TAB-SEXE REDEFINES WS-LIB-SEXE.
           05 WS-SEXE-LIB          PIC X(08) OCCURS 2.

       01  WS-LIB-SITSO.
           05 FILLER               PIC X(12) VALUE 'CELIBATAIRE '.
           05 FILLER               PIC X(12) VALUE 'MARIE(E)    '.
           05 FILLER               PIC X(12) VALUE 'DIVORCE(E)  '.
           05 FILLER               PIC X(12) VALUE 'VEUF(VE)    '.

       01  WS-IDX                  PIC 9(02) VALUE 0.

      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************

      *-----------------------------------------------------------------
       0000-PRINCIPAL.
      *-----------------------------------------------------------------
      * Point d'entree du programme
      *-----------------------------------------------------------------
           EVALUATE TRUE
               WHEN EIBCALEN = 0
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN EIBAID = DFHPF3
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
                   PERFORM 1000-PREMIER-PASSAGE
               WHEN OTHER
                   PERFORM 2000-TRAITEMENT
           END-EVALUATE

           EXEC CICS RETURN
               TRANSID('AFFI')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *-----------------------------------------------------------------
       1000-PREMIER-PASSAGE.
      *-----------------------------------------------------------------
      * Affichage de l'ecran vide avec message de saisie
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAFFO
           MOVE 'SAISIR LE NUMERO DE COMPTE ET APPUYER SUR ENTREE'
               TO MSGO
           MOVE 'O' TO WS-FLAG-INIT

           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
               ERASE
           END-EXEC.

      *-----------------------------------------------------------------
       2000-TRAITEMENT.
      *-----------------------------------------------------------------
      * Reception des donnees et recherche du client
      *-----------------------------------------------------------------
           EXEC CICS RECEIVE MAP('MAPAFF')
               MAPSET('CLIAFF')
               RESP(WS-RESP)
           END-EXEC

      * Gestion MAPFAIL (aucune donnee transmise)
           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE LOW-VALUES TO MAPAFFO
               MOVE 'ERREUR RECEPTION - RESSAISIR' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPAFF')
                   MAPSET('CLIAFF')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Verifier que le numero de compte est saisi
           IF NUMCPTL = 0 OR NUMCPTI = SPACES
               MOVE LOW-VALUES TO MAPAFFO
               MOVE 'VEUILLEZ SAISIR UN NUMERO DE COMPTE' TO MSGO
               MOVE -1 TO NUMCPTL
               EXEC CICS SEND MAP('MAPAFF')
                   MAPSET('CLIAFF')
                   DATAONLY CURSOR
               END-EXEC
               GO TO 2000-FIN
           END-IF

      * Preparer la cle de recherche
           MOVE NUMCPTI TO WS-NUMCPT

      * Lecture du fichier VSAM
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(WS-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

      * Traitement du resultat
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 3000-AFFICHER-CLIENT
               WHEN DFHRESP(NOTFND)
                   MOVE LOW-VALUES TO MAPAFFO
                   MOVE WS-NUMCPT TO NUMCPTO
                   MOVE 'CLIENT INEXISTANT - VERIFIEZ LE NUMERO'
                       TO MSGO
               WHEN OTHER
                   MOVE LOW-VALUES TO MAPAFFO
                   MOVE 'ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT'
                       TO MSGO
           END-EVALUATE

           EXEC CICS SEND MAP('MAPAFF')
               MAPSET('CLIAFF')
           END-EXEC.

       2000-FIN.
           EXIT.

      *-----------------------------------------------------------------
       3000-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Transfert des donnees du fichier vers la MAP
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPAFFO

      * Donnees directes
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

      * Libelle region
           EVALUATE CLI-CODREG
               WHEN '01'
                   MOVE '01 - PARIS' TO LIBREGO
               WHEN '02'
                   MOVE '02 - MARSEILLE' TO LIBREGO
               WHEN '03'
                   MOVE '03 - LYON' TO LIBREGO
               WHEN '04'
                   MOVE '04 - LILLE' TO LIBREGO
               WHEN OTHER
                   MOVE 'REGION INCONNUE' TO LIBREGO
           END-EVALUATE

      * Libelle sexe
           EVALUATE CLI-SEXE
               WHEN 'M'
                   MOVE 'MASCULIN' TO LIBSEXO
               WHEN 'F'
                   MOVE 'FEMININ' TO LIBSEXO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBSEXO
           END-EVALUATE

      * Libelle situation sociale
           EVALUATE CLI-SITSO
               WHEN 'C'
                   MOVE 'CELIBATAIRE' TO LIBSITO
               WHEN 'M'
                   MOVE 'MARIE(E)' TO LIBSITO
               WHEN 'D'
                   MOVE 'DIVORCE(E)' TO LIBSITO
               WHEN 'V'
                   MOVE 'VEUF(VE)' TO LIBSITO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBSITO
           END-EVALUATE

      * Libelle position
           EVALUATE CLI-POSITION
               WHEN 'CR'
                   MOVE 'CREDITEUR' TO LIBPOSO
               WHEN 'DB'
                   MOVE 'DEBITEUR' TO LIBPOSO
               WHEN OTHER
                   MOVE 'INCONNU' TO LIBPOSO
           END-EVALUATE

           MOVE 'CLIENT TROUVE - PF3=QUITTER OU NOUVELLE RECHERCHE'
               TO MSGO.

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

