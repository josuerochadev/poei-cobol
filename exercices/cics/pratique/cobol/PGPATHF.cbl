       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGPATHF.
      *================================================================*
      * Programme : PGPATHF - BROWSE avec ALTERNATE INDEX (AIX)        *
      * Transaction : TPTH                                             *
      * Fichier : PCLIENT (PATH vers AIX sur NOMCPT)                   *
      * Description : Parcours sequentiel par nom de client            *
      *               Utilise une cle alternative (NOMCPT)             *
      *================================================================*
      * Chapitre VIII - Exercice 14 : BROWSE avec cle ALTERNATE        *
      *----------------------------------------------------------------*
      * Structure du programme :                                       *
      *   1. Envoi MAP1 pour saisir nom client (cle generique)         *
      *   2. Reception du nom saisi                                    *
      *   3. STARTBR avec FILE('PCLIENT') et cle NOMCPT                *
      *   4. READNEXT pour lire les enregistrements                    *
      *   5. Affichage des donnees sur MAP2                            *
      *   6. RECEIVE pour attendre l'utilisateur (ENTER = suivant)     *
      *   7. ENDBR pour fermer le browse                               *
      *----------------------------------------------------------------*
      * Pre-requis :                                                   *
      *   - AIX defini sur FCLIENT (champ NOMCPT, position 7, lg 10)   *
      *   - PATH defini (FTEST.CICS.FCLIENT.PATH)                      *
      *   - Definition CICS : FILE(PCLIENT) DSN(PATH)                  *
      *================================================================*
      * Version corrigee : ajout RECEIVE entre chaque affichage        *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPPATH.

       01  WS-RESPCODE          PIC S9(8) COMP.
       01  WS-REC-LEN           PIC S9(4) COMP.
       01  WS-KEY-LEN           PIC S9(4) COMP.
       01  WS-REC-KEY           PIC X(10).
       01  WS-MESSAGE           PIC X(50).
       01  WS-REQID             PIC S9(4) COMP VALUE 1.
       01  WS-COUNT             PIC 9(2) VALUE 0.

       01  WS-REC-DATA.
           05  WS-CDECLT        PIC 9(3).
           05  WS-CODREG        PIC 99.
           05  WS-NATCPT        PIC 99.
           05  WS-NOMCPT        PIC X(10).
           05  WS-PRNCPT        PIC X(10).
           05  WS-DTNCPT        PIC 9(8).
           05  WS-SEXCPT        PIC X(1).
           05  WS-APRCPT        PIC 9(2).
           05  WS-SOCCPT        PIC X(1).
           05  WS-ADRCPT        PIC X(10).
           05  WS-SLDCPT        PIC 9(10).
           05  WS-POSCPT        PIC X(02).
           05  FILLER           PIC X(19).

       PROCEDURE DIVISION.

       MAIN-PARA.

           MOVE 80   TO WS-REC-LEN.
           MOVE SPACES TO WS-REC-KEY.
           MOVE 5    TO WS-KEY-LEN.

      *------- Ignorer condition DUPKEY (cle non-unique) --------------
           EXEC CICS IGNORE CONDITION
               DUPKEY
               ENDFILE
           END-EXEC.

      *------- Envoi MAP1 pour saisir le nom client -------------------
           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPPATH') MAPONLY FREEKB ERASE
           END-EXEC.

      *------- Reception du nom saisi ---------------------------------
           EXEC CICS RECEIVE MAP('MAP1')
               MAPSET('MAPPATH')
           END-EXEC.

           MOVE GENNOMI TO WS-REC-KEY.

      *================================================================*
      *        STARTBR - Positionnement via AIX (NOMCPT)               *
      *================================================================*
           EXEC CICS STARTBR FILE('PCLIENT')
               RIDFLD(WS-REC-KEY) GENERIC
               KEYLENGTH(WS-KEY-LEN) REQID(WS-REQID)
               RESP(WS-RESPCODE)
           END-EXEC.

           IF WS-RESPCODE NOT = DFHRESP(NORMAL)
               IF WS-RESPCODE = DFHRESP(NOTFND)
                   MOVE 'AUCUN CLIENT TROUVE POUR CE NOM'
                       TO WS-MESSAGE
               ELSE
                   MOVE 'ERREUR STARTBR AIX - VERIFIER PATH'
                       TO WS-MESSAGE
               END-IF
               GO TO ERREUR-PARA
           END-IF.

      *================================================================*
      *        Boucle de lecture READNEXT via AIX                      *
      *================================================================*
           MOVE 10 TO WS-KEY-LEN.
           MOVE 0  TO WS-COUNT.

       BOUCLE-LECTURE.
      *------- READNEXT - Lecture enregistrement suivant via AIX ------
           EXEC CICS READNEXT FILE('PCLIENT') INTO(WS-REC-DATA)
               LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
               KEYLENGTH(WS-KEY-LEN) REQID(WS-REQID)
               RESP(WS-RESPCODE)
           END-EXEC.

           IF WS-RESPCODE = DFHRESP(ENDFILE)
               MOVE 'FIN DE FICHIER ATTEINTE' TO WS-MESSAGE
               GO TO FIN-BROWSE
           END-IF.

           IF WS-RESPCODE NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR READNEXT AIX' TO WS-MESSAGE
               GO TO FIN-BROWSE
           END-IF.

           ADD 1 TO WS-COUNT.
           PERFORM AFFECT-DONNEE.

      *------- Envoi MAP2 avec donnees --------------------------------
           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPPATH') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPPATH') DATAONLY FREEKB
           END-EXEC.

      *------- Attente action utilisateur (ENTER = suivant) -----------
           EXEC CICS RECEIVE
               RESP(WS-RESPCODE)
           END-EXEC.

      *------- Verifier si PF3 pour quitter ---------------------------
           IF EIBAID = DFHPF3
               MOVE 'ABANDON PAR UTILISATEUR (PF3)' TO WS-MESSAGE
               GO TO FIN-BROWSE
           END-IF.

      *------- Continuer si moins de 3 enregistrements lus ------------
           IF WS-COUNT < 3
               GO TO BOUCLE-LECTURE
           END-IF.

           MOVE 'PARCOURS AIX TERMINE - CLIENTS AFFICHES'
               TO WS-MESSAGE.

      *================================================================*
      *        ENDBR - Fin du browse                                   *
      *================================================================*
       FIN-BROWSE.
           EXEC CICS ENDBR FILE('PCLIENT')
               REQID(WS-REQID)
               RESP(WS-RESPCODE)
           END-EXEC.

           GO TO FIN-PROGRAM.

       ERREUR-PARA.
           CONTINUE.

       FIN-PROGRAM.
           EXEC CICS SEND TEXT
               FROM(WS-MESSAGE)
               LENGTH(50)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

           STOP RUN.

      *================================================================*
      *        AFFECT-DONNEE - Transfert donnees vers MAP              *
      *================================================================*
       AFFECT-DONNEE.
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
