      ******************************************************************
      * Programme : CLNTTRT - Consultation Client (Traitement)
      * Fonction : Logique métier consultation client
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTTRT.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY CLIENT.

       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(11).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(130).

       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
           05  CA-CODE-RETOUR      PIC 9(2).
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

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR

           IF CA-CONSULTER
               PERFORM 1000-CONSULTER-CLIENT
           END-IF

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

       1000-CONSULTER-CLIENT.

           INITIALIZE WS-DAO-COMMAREA
           MOVE 'READ' TO DAO-ACTION
           MOVE 'CLIENT' TO DAO-FICHIER
           MOVE CA-NUM-CLIENT TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CLNTDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               IF DAO-RESP = 13
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Client non trouve' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture client' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

           MOVE DAO-DATA TO CLIENT-REC
           MOVE CLI-NOM      TO CA-NOM
           MOVE CLI-PRENOM   TO CA-PRENOM
           MOVE CLI-ADRESSE  TO CA-ADRESSE
           MOVE CLI-VILLE    TO CA-VILLE
           MOVE CLI-CODEPOST TO CA-CODEPOST
           MOVE CLI-TEL      TO CA-TEL
           MOVE CLI-DATEOUV  TO CA-DATEOUV
           MOVE CLI-REGION   TO CA-REGION
           MOVE 00 TO CA-CODE-RETOUR.

       1000-EXIT.
           EXIT.
