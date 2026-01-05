      ******************************************************************
      * Programme : CPTETRT - Gestion Comptes (Traitement)
      * Fonction : Logique métier gestion comptes
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CPTETRT.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY COMPTE.

       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(11).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(60).

       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-BROWSE-NEXT  VALUE 'N'.
               88  CA-BROWSE-PREV  VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
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

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR

           EVALUATE TRUE
               WHEN CA-CONSULTER
                   PERFORM 1000-CONSULTER-COMPTE
               WHEN CA-BROWSE-NEXT
                   PERFORM 2000-BROWSE-SUIVANT
               WHEN CA-BROWSE-PREV
                   PERFORM 3000-BROWSE-PRECEDENT
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

       1000-CONSULTER-COMPTE.

           INITIALIZE WS-DAO-COMMAREA
           MOVE 'READ' TO DAO-ACTION
           MOVE 'COMPTE' TO DAO-FICHIER
           MOVE CA-NUM-COMPTE TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CPTEDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               IF DAO-RESP = 13
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Compte non trouve' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture compte' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

           PERFORM 1100-TRANSFERT-DONNEES.

       1000-EXIT.
           EXIT.

       1100-TRANSFERT-DONNEES.

           MOVE DAO-DATA TO COMPTE-REC
           MOVE CPT-NUM     TO CA-NUM-COMPTE
           MOVE CPT-CLIENT  TO CA-CLIENT
           MOVE CPT-TYPE    TO CA-TYPE
           MOVE CPT-LIBELLE TO CA-LIBELLE
           MOVE CPT-SOLDE   TO CA-SOLDE
           MOVE CPT-DATEOUV TO CA-DATEOUV
           MOVE CPT-DATEDER TO CA-DATEDER
           MOVE 00 TO CA-CODE-RETOUR.

       2000-BROWSE-SUIVANT.

           MOVE 'NEXT' TO DAO-ACTION
           MOVE 'COMPTE' TO DAO-FICHIER
           MOVE CA-NUM-COMPTE TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CPTEDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 20 TO CA-CODE-RETOUR
               MOVE 'Fin de fichier atteinte' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           PERFORM 1100-TRANSFERT-DONNEES.

       2000-EXIT.
           EXIT.

       3000-BROWSE-PRECEDENT.

           MOVE 'PREV' TO DAO-ACTION
           MOVE 'COMPTE' TO DAO-FICHIER
           MOVE CA-NUM-COMPTE TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CPTEDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 20 TO CA-CODE-RETOUR
               MOVE 'Debut de fichier atteint' TO CA-MESSAGE
               GO TO 3000-EXIT
           END-IF

           PERFORM 1100-TRANSFERT-DONNEES.

       3000-EXIT.
           EXIT.
