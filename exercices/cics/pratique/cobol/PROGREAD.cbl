       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGREAD.
      *================================================================*
      * Programme : PROGREAD - Lecture client VSAM (Chapitre VIII)     *
      * Transaction : TRDF                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Lecture aleatoire par cle primaire               *
      *================================================================*
      * Touches : ENTER = Lire | PF3/CLEAR = Quitter                   *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY MAPREAD.

       01  WS-RESPCODE           PIC S9(8) COMP.
       01  WS-REC-LEN            PIC S9(4) COMP.
       01  WS-KEY-LEN            PIC S9(4) COMP.
       01  WS-REC-KEY            PIC 9(3).
       01  WS-MESSAGE            PIC X(40).

       01  WS-REC-DATA.
           05  WS-CDECLT         PIC 9(3).
           05  WS-CODREG         PIC 99.
           05  WS-NATCPT         PIC 99.
           05  WS-NOMCPT         PIC X(10).
           05  WS-PRNCPT         PIC X(10).
           05  WS-DTNCPT         PIC 9(8).
           05  WS-SEXCPT         PIC X(1).
           05  WS-APRCPT         PIC 9(2).
           05  WS-SOCCPT         PIC X(1).
           05  WS-ADRCPT         PIC X(10).
           05  WS-SLDCPT         PIC 9(10).
           05  WS-POSCPT         PIC X(02).
           05  FILLER            PIC X(19).

       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE 80  TO WS-REC-LEN.
           MOVE 3   TO WS-KEY-LEN.
           MOVE SPACES TO WS-MESSAGE.

      *--- Affichage ecran de saisie ---
           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPREAD') MAPONLY FREEKB ERASE
           END-EXEC.

      *--- Reception de la cle saisie ---
           EXEC CICS RECEIVE MAP('MAP1')
               MAPSET('MAPREAD')
           END-EXEC.

      *--- Gestion des touches ---
           IF EIBAID = DFHPF3 OR EIBAID = DFHCLEAR
               EXEC CICS RETURN END-EXEC
           END-IF.

      *--- Validation de la saisie ---
           IF CDECLTI = SPACES OR CDECLTI = LOW-VALUES
               MOVE 'ERREUR: VEUILLEZ SAISIR UN CODE CLIENT'
                   TO WS-MESSAGE
               GO TO ERREUR-PARA
           END-IF.

           IF CDECLTI NOT NUMERIC
               MOVE 'ERREUR: LE CODE DOIT ETRE NUMERIQUE'
                   TO WS-MESSAGE
               GO TO ERREUR-PARA
           END-IF.

           MOVE CDECLTI TO WS-REC-KEY.

      *--- Lecture VSAM avec gestion d'erreur ---
           EXEC CICS READ FILE('FCLIENT') INTO(WS-REC-DATA)
               LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
               KEYLENGTH(WS-KEY-LEN)
               RESP(WS-RESPCODE)
           END-EXEC.

           IF WS-RESPCODE = DFHRESP(NOTFND)
               MOVE 'ERREUR: CLIENT NON TROUVE' TO WS-MESSAGE
               GO TO ERREUR-PARA
           END-IF.

           IF WS-RESPCODE NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR: PROBLEME LECTURE FICHIER' TO WS-MESSAGE
               GO TO ERREUR-PARA
           END-IF.

      *--- Transfert donnees vers MAP ---
           PERFORM AFFECT-DONNEE.

      *--- Affichage resultat ---
           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPREAD') ERASE FREEKB
           END-EXEC.

           EXEC CICS RETURN END-EXEC.

      *================================================================*
       ERREUR-PARA.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM(WS-MESSAGE)
               LENGTH(40)
               ERASE
           END-EXEC.

           EXEC CICS RETURN END-EXEC.

      *================================================================*
       AFFECT-DONNEE.
      *================================================================*
           MOVE WS-CDECLT TO CDECLT2O.
           MOVE WS-CODREG TO CODREGO.
           MOVE WS-NATCPT TO NATCPTO.
           MOVE WS-NOMCPT TO NOMCPTO.
           MOVE WS-PRNCPT TO PRNCPTO.
           MOVE WS-DTNCPT TO DTNCPTO.
           MOVE WS-SEXCPT TO SEXCPTO.
           MOVE WS-APRCPT TO APRCPTO.
           MOVE WS-SOCCPT TO SOCCPTO.
           MOVE WS-ADRCPT TO ADRCPTO.
           MOVE WS-SLDCPT TO SLDCPTO.
           MOVE WS-POSCPT TO POSCPTO.
