      ******************************************************************
      * Programme : CLNTPRES - Consultation Client (Présentation)
      * Transaction : CLNT
      * Fonction : Interface utilisateur consultation client
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTPRES.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CLNTSET.

       01  WS-RESP                 PIC S9(8) COMP.

       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-LISTER-CPT   VALUE 'L'.
           05  CA-CODE-RETOUR      PIC 9(2).
               88  CA-OK           VALUE 00.
               88  CA-NOTFND       VALUE 13.
           05  CA-MESSAGE          PIC X(50).
           05  CA-NUM-CLIENT       PIC X(6).
           05  CA-CLIENT-DATA.
               10  CA-NOM          PIC X(25).
               10  CA-PRENOM       PIC X(20).
               10  CA-ADRESSE      PIC X(30).
               10  CA-VILLE        PIC X(20).
               10  CA-CODEPOST     PIC X(5).
               10  CA-TEL          PIC X(10).
               10  CA-DATEOUV      PIC X(8).
               10  CA-REGION       PIC X(2).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           IF EIBCALEN = 0
               PERFORM 1000-AFFICHER-ECRAN-VIDE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

           EXEC CICS
               RETURN TRANSID('CLNT')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-AFFICHER-ECRAN-VIDE.

           INITIALIZE CLNTMAPO
           MOVE 'Entrez un numero client (CLI001-CLI010)'
               TO MSGO

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    ERASE
           END-EXEC.

       2000-TRAITER-SAISIE.

           EXEC CICS
               RECEIVE MAP('CLNTMAP')
                       MAPSET('CLNTSET')
                       INTO(CLNTMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-ECRAN-VIDE
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-RECHERCHER-CLIENT
               WHEN DFHPF5
                   PERFORM 2200-LISTER-COMPTES
               WHEN DFHPF3
                   PERFORM 9000-RETOUR-MENU
               WHEN OTHER
                   MOVE 'Touche non autorisee' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

       2100-RECHERCHER-CLIENT.

           IF NUMCLIL = 0 OR NUMCLII = SPACES
               MOVE 'Numero client obligatoire' TO MSGO
               MOVE DFHBMDAR TO NUMCLIA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

           INITIALIZE WS-COMMAREA
           SET CA-CONSULTER TO TRUE
           MOVE NUMCLII TO CA-NUM-CLIENT

           EXEC CICS
               LINK PROGRAM('CLNTTRT')
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

           MOVE CA-NUM-CLIENT TO NUMCLIO
           MOVE CA-NOM        TO NOMO
           MOVE CA-PRENOM     TO PRENOMO
           MOVE CA-ADRESSE    TO ADRESSEO
           MOVE CA-VILLE      TO VILLEO
           MOVE CA-CODEPOST   TO CPOSTO
           MOVE CA-TEL        TO TELO
           MOVE CA-DATEOUV    TO DATEOUVO
           MOVE CA-REGION     TO REGIONO
           MOVE 'Client trouve - PF5=Voir comptes PF3=Menu'
               TO MSGO

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    ERASE
           END-EXEC.

       2200-LISTER-COMPTES.

           IF CA-NUM-CLIENT = SPACES
               MOVE 'Recherchez un client d''abord' TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2200-EXIT
           END-IF

           EXEC CICS
               XCTL PROGRAM('CPTEPRES')
                    COMMAREA(WS-COMMAREA)
           END-EXEC.

       2200-EXIT.
           EXIT.

       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('CLNTMAP')
                    MAPSET('CLNTSET')
                    FROM(CLNTMAPO)
                    DATAONLY
           END-EXEC.

       9000-RETOUR-MENU.

           EXEC CICS
               XCTL PROGRAM('MENUPRES')
           END-EXEC.
