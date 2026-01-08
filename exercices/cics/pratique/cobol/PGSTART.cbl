       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGSTART.
      *================================================================*
      * Programme : PGSTART - BROWSE avec STARTBR/READNEXT/ENDBR       *
      * Transaction : TSRT                                             *
      * Fichier : FCLIENT (KSDS)                                       *
      * Description : Parcours sequentiel avec cle generique           *
      *               Lit 3 enregistrements a partir d'une cle donnee  *
      *================================================================*
      * Chapitre VIII - Exercice 13 : Commandes BROWSE                 *
      *----------------------------------------------------------------*
      * Structure du programme :                                       *
      *   1. Envoi MAP1 pour saisir cle de depart                      *
      *   2. Reception de la cle saisie                                *
      *   3. STARTBR avec cle generique (KEYLENGTH=2)                  *
      *   4. READNEXT pour lire les enregistrements                    *
      *   5. Affichage des donnees sur MAP2                            *
      *   6. RECEIVE pour attendre l'utilisateur (ENTER = suivant)     *
      *   7. ENDBR pour fermer le browse                               *
      *================================================================*
      * Version corrigee : ajout RECEIVE entre chaque affichage        *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY MAPREAD.

       01  WS-RESPCODE          PIC S9(8) COMP.
       01  WS-REC-LEN           PIC S9(4) COMP.
       01  WS-KEY-LEN           PIC S9(4) COMP.
       01  WS-REC-KEY           PIC 9(3).
       01  WS-MESSAGE           PIC X(50).
       01  WS-REQID             PIC S9(4) COMP VALUE 1.
       01  WS-COUNT             PIC 9(2) VALUE 0.
       01  WS-EIBAID            PIC X(1).

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

           MOVE 80  TO WS-REC-LEN.
           MOVE 000 TO WS-REC-KEY.
           MOVE 2   TO WS-KEY-LEN.

      *------- Envoi MAP1 pour saisir la cle de depart ---------------
           EXEC CICS SEND MAP('MAP1')
               MAPSET('MAPREAD') MAPONLY FREEKB ERASE
           END-EXEC.

      *------- Reception de la cle saisie ----------------------------
           EXEC CICS RECEIVE MAP('MAP1')
               MAPSET('MAPREAD')
           END-EXEC.

           MOVE CDECLTI TO WS-REC-KEY.

      *================================================================*
      *        STARTBR - Positionnement avec cle generique             *
      *================================================================*
           EXEC CICS STARTBR FILE('FCLIENT')
               RIDFLD(WS-REC-KEY) GENERIC
               KEYLENGTH(WS-KEY-LEN) REQID(WS-REQID)
               RESP(WS-RESPCODE)
           END-EXEC.

           IF WS-RESPCODE NOT = DFHRESP(NORMAL)
               IF WS-RESPCODE = DFHRESP(NOTFND)
                   MOVE 'AUCUN ENREGISTREMENT TROUVE POUR CETTE CLE'
                       TO WS-MESSAGE
               ELSE
                   MOVE 'ERREUR STARTBR - CODE: ' TO WS-MESSAGE
               END-IF
               GO TO ERREUR-PARA
           END-IF.

      *================================================================*
      *        Boucle de lecture READNEXT                              *
      *================================================================*
           MOVE 3 TO WS-KEY-LEN.
           MOVE 0 TO WS-COUNT.

       BOUCLE-LECTURE.
      *------- READNEXT - Lecture enregistrement suivant ---------------
           EXEC CICS READNEXT FILE('FCLIENT') INTO(WS-REC-DATA)
               LENGTH(WS-REC-LEN) RIDFLD(WS-REC-KEY)
               KEYLENGTH(WS-KEY-LEN) REQID(WS-REQID)
               RESP(WS-RESPCODE)
           END-EXEC.

           IF WS-RESPCODE = DFHRESP(ENDFILE)
               MOVE 'FIN DE FICHIER ATTEINTE' TO WS-MESSAGE
               GO TO FIN-BROWSE
           END-IF.

           IF WS-RESPCODE NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR READNEXT' TO WS-MESSAGE
               GO TO FIN-BROWSE
           END-IF.

           ADD 1 TO WS-COUNT.
           PERFORM AFFECT-DONNEE.

      *------- Envoi MAP2 avec donnees --------------------------------
           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPREAD') MAPONLY FREEKB ERASE
           END-EXEC.

           EXEC CICS SEND MAP('MAP2')
               MAPSET('MAPREAD') DATAONLY FREEKB
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

           MOVE 'PARCOURS TERMINE - 3 ENREGISTREMENTS AFFICHES'
               TO WS-MESSAGE.

      *================================================================*
      *        ENDBR - Fin du browse                                   *
      *================================================================*
       FIN-BROWSE.
           EXEC CICS ENDBR FILE('FCLIENT')
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
           MOVE WS-CDECLT TO CDECLTO.
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
