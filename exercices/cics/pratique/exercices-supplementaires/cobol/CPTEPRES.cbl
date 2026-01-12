      ******************************************************************
      * Programme : CPTEPRES - Gestion Comptes (Présentation)
      * Transaction : CPTE
      * Fonction : Interface utilisateur gestion comptes
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPTEPRES.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CPTESET.

       01  WS-RESP                 PIC S9(8) COMP.

       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-BROWSE-NEXT  VALUE 'N'.
               88  CA-BROWSE-PREV  VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
               88  CA-OK           VALUE 00.
               88  CA-NOTFND       VALUE 13.
               88  CA-ENDFILE      VALUE 20.
           05  CA-MESSAGE          PIC X(50).
           05  CA-NUM-COMPTE       PIC X(11).
           05  CA-COMPTE-DATA.
               10  CA-CLIENT       PIC X(6).
               10  CA-TYPE         PIC X(1).
               10  CA-LIBELLE      PIC X(20).
               10  CA-SOLDE        PIC S9(9)V99.
               10  CA-DATEOUV      PIC X(8).
               10  CA-DATEDER      PIC X(8).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(100).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           IF EIBCALEN = 0
               PERFORM 1000-AFFICHER-ECRAN-VIDE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

           EXEC CICS
               RETURN TRANSID('CPTE')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-AFFICHER-ECRAN-VIDE.

           INITIALIZE CPTEMAPO
           MOVE 'Entrez un numero de compte' TO MSGO

           EXEC CICS
               SEND MAP('CPTEMAP')
                    MAPSET('CPTESET')
                    FROM(CPTEMAPO)
                    ERASE
           END-EXEC.

       2000-TRAITER-SAISIE.

           EXEC CICS
               RECEIVE MAP('CPTEMAP')
                       MAPSET('CPTESET')
                       INTO(CPTEMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-ECRAN-VIDE
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-RECHERCHER-COMPTE
               WHEN DFHPF7
                   PERFORM 2200-COMPTE-PRECEDENT
               WHEN DFHPF8
                   PERFORM 2300-COMPTE-SUIVANT
               WHEN DFHPF3
                   PERFORM 9000-RETOUR-MENU
               WHEN OTHER
                   MOVE 'Touche non autorisee' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

       2100-RECHERCHER-COMPTE.

           IF NUMCPTL = 0 OR NUMCPTI = SPACES
               MOVE 'Numero compte obligatoire' TO MSGO
               MOVE DFHBMDAR TO NUMCPTA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

           INITIALIZE WS-COMMAREA
           SET CA-CONSULTER TO TRUE
           MOVE NUMCPTI TO CA-NUM-COMPTE

           EXEC CICS
               LINK PROGRAM('CPTETRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2100-EXIT.
           EXIT.

       2110-AFFICHER-RESULTAT.

           MOVE CA-NUM-COMPTE TO NUMCPTO
           MOVE CA-CLIENT     TO CLIENTO
           MOVE CA-TYPE       TO TYPEO
           MOVE CA-LIBELLE    TO LIBELLO
           MOVE CA-SOLDE      TO SOLDEO
           MOVE CA-DATEOUV    TO DATEOUVO
           MOVE CA-DATEDER    TO DATEDERO
           MOVE 'Compte trouve - PF7/PF8=Nav PF3=Menu'
               TO MSGO

           EXEC CICS
               SEND MAP('CPTEMAP')
                    MAPSET('CPTESET')
                    FROM(CPTEMAPO)
                    ERASE
           END-EXEC.

       2200-COMPTE-PRECEDENT.

           IF CA-NUM-COMPTE = SPACES
               MOVE 'Recherchez un compte d''abord' TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2200-EXIT
           END-IF

           SET CA-BROWSE-PREV TO TRUE

           EXEC CICS
               LINK PROGRAM('CPTETRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2200-EXIT.
           EXIT.

       2300-COMPTE-SUIVANT.

           IF CA-NUM-COMPTE = SPACES
               MOVE 'Recherchez un compte d''abord' TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2300-EXIT
           END-IF

           SET CA-BROWSE-NEXT TO TRUE

           EXEC CICS
               LINK PROGRAM('CPTETRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2300-EXIT.
           EXIT.

       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('CPTEMAP')
                    MAPSET('CPTESET')
                    FROM(CPTEMAPO)
                    DATAONLY
           END-EXEC.

       9000-RETOUR-MENU.

           EXEC CICS
               XCTL PROGRAM('MENUPRES')
           END-EXEC.
