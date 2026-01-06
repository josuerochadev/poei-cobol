       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGWRIT.
      *================================================================*
      * Programme : PROGWRIT - Ecriture VSAM avec verification         *
      * Transaction : WRIT                                             *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      * Logique : READ prealable pour verifier si cle existe           *
      *           Si cle existe    -> pas d'ecriture                   *
      *           Si cle n'existe pas -> ecriture WRITE                *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-REC-KEY              PIC X(6).

       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).

       01  WS-MSG                  PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.

      *================================================================*
       0000-MAIN.
      *================================================================*
      *    Initialiser la cle de l'enregistrement a ajouter
           MOVE 'EMP099' TO WS-REC-KEY.

      *----------------------------------------------------------------*
      *    Lecture de la cle dans le Data Set
      *----------------------------------------------------------------*
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               RESP(WS-RESP)
           END-EXEC.

      *----------------------------------------------------------------*
      *    Si lecture correcte, la cle existe deja
      *----------------------------------------------------------------*
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ERREUR: CLE EXISTE DEJA - PAS D ECRITURE' TO WS-MSG
               PERFORM 9000-AFFICHER-MSG
               EXEC CICS RETURN END-EXEC
           END-IF.

      *----------------------------------------------------------------*
      *    Cle n'existe pas, on peut ecrire
      *----------------------------------------------------------------*
           IF WS-RESP NOT = DFHRESP(NOTFND)
               MOVE 'ERREUR LECTURE INATTENDUE' TO WS-MSG
               PERFORM 9000-AFFICHER-MSG
               EXEC CICS RETURN END-EXEC
           END-IF.

      *    Preparer le nouvel enregistrement
           INITIALIZE WS-EMPLOYE.
           MOVE WS-REC-KEY        TO EMP-ID.
           MOVE 'NOUVEL EMPLOYE'  TO EMP-NAME.
           MOVE 'NOUVEAU'         TO EMP-DEPT.
           MOVE 025000.00         TO EMP-SALAIRE.
           MOVE 'N'               TO EMP-ETAT-CRED.

      *----------------------------------------------------------------*
      *    Ecriture dans le Data Set par WRITE
      *----------------------------------------------------------------*
           EXEC CICS WRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RIDFLD(EMP-ID)
               RESP(WS-RESP)
           END-EXEC.

      *    Verifier resultat
           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'ENREGISTREMENT CREE AVEC SUCCES' TO WS-MSG
           ELSE
               MOVE 'ERREUR ECRITURE VSAM' TO WS-MSG
           END-IF.

           PERFORM 9000-AFFICHER-MSG.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.

      *================================================================*
       9000-AFFICHER-MSG.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               LENGTH(60)
               ERASE
           END-EXEC.
