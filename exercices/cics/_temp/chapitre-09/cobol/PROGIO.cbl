       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGIO.
      *================================================================*
      * Programme : PROGIO - Echange de donnees via MAP (Chapitre IX)  *
      * Transaction : TIO                                              *
      * Description : Demonstration SEND MAP / RECEIVE MAP             *
      *               Mode pseudo-conversationnel                      *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Codes reponse CICS ---
       01  WS-RESP                 PIC S9(8) COMP.

      *--- Indicateur premier passage ---
       01  WS-FIRST-TIME           PIC X(1) VALUE 'Y'.
           88 FIRST-TIME           VALUE 'Y'.
           88 NOT-FIRST-TIME       VALUE 'N'.

      *--- Copybook DFHAID pour les touches ---
       COPY DFHAID.

      *--- Zone symbolique MAP (generee par BMS) ---
       01  MAP1I.
           05  FILLER              PIC X(12).
           05  VAR1L               PIC S9(4) COMP.
           05  VAR1F               PIC X.
           05  FILLER REDEFINES VAR1F.
               10  VAR1A           PIC X.
           05  VAR1I               PIC X(20).
           05  VAR2L               PIC S9(4) COMP.
           05  VAR2F               PIC X.
           05  FILLER REDEFINES VAR2F.
               10  VAR2A           PIC X.
           05  VAR2I               PIC X(20).
           05  OUTVAR1L            PIC S9(4) COMP.
           05  OUTVAR1F            PIC X.
           05  FILLER REDEFINES OUTVAR1F.
               10  OUTVAR1A        PIC X.
           05  OUTVAR1I            PIC X(20).
           05  OUTVAR2L            PIC S9(4) COMP.
           05  OUTVAR2F            PIC X.
           05  FILLER REDEFINES OUTVAR2F.
               10  OUTVAR2A        PIC X.
           05  OUTVAR2I            PIC X(20).
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAP1O REDEFINES MAP1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  VAR1O               PIC X(20).
           05  FILLER              PIC X(3).
           05  VAR2O               PIC X(20).
           05  FILLER              PIC X(3).
           05  OUTVAR1O            PIC X(20).
           05  FILLER              PIC X(3).
           05  OUTVAR2O            PIC X(20).
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
       01  WS-COMMAREA.
           05  CA-FIRST-TIME       PIC X(1).

       PROCEDURE DIVISION.

      *================================================================*
       0000-MAIN.
      *================================================================*
      *    Verifier si premier passage (COMMAREA vide)
           IF EIBCALEN = 0
               SET FIRST-TIME TO TRUE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               SET NOT-FIRST-TIME TO TRUE
           END-IF.

      *    Traitement selon le contexte
           IF FIRST-TIME
               PERFORM 1000-FIRST-TIME
           ELSE
               PERFORM 2000-PROCESS-INPUT
           END-IF.

           STOP RUN.

      *================================================================*
       1000-FIRST-TIME.
      *================================================================*
      *    Premier affichage de l'ecran
           INITIALIZE MAP1O.
           MOVE 'Saisissez VAR1 et VAR2, puis ENTER' TO MSGO.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('MAPIO')
               FROM(MAP1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

      *    Retour CICS avec COMMAREA
           MOVE 'N' TO CA-FIRST-TIME.
           EXEC CICS RETURN
               TRANSID('TIO ')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       2000-PROCESS-INPUT.
      *================================================================*
      *    Verifier la touche pressee
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-FIN
               WHEN DFHCLEAR
                   PERFORM 1000-FIRST-TIME
               WHEN DFHENTER
                   PERFORM 2100-TRAITER-DONNEES
               WHEN OTHER
                   PERFORM 2200-TOUCHE-INVALIDE
           END-EVALUATE.

      *================================================================*
       2100-TRAITER-DONNEES.
      *================================================================*
      *    Recevoir les donnees de l'ecran
           INITIALIZE MAP1I.

           EXEC CICS RECEIVE
               MAP('MAP1')
               MAPSET('MAPIO')
               INTO(MAP1I)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               IF WS-RESP = DFHRESP(MAPFAIL)
                   MOVE 'Aucune donnee saisie - Ressaisissez' TO MSGO
               ELSE
                   MOVE 'Erreur RECEIVE MAP' TO MSGO
               END-IF
               EXEC CICS SEND
                   MAP('MAP1')
                   MAPSET('MAPIO')
                   FROM(MAP1O)
                   DATAONLY
                   FREEKB
               END-EXEC
               EXEC CICS RETURN
                   TRANSID('TIO ')
                   COMMAREA(WS-COMMAREA)
                   LENGTH(LENGTH OF WS-COMMAREA)
               END-EXEC
           END-IF.

      *    Traiter les donnees : copier INPUT vers OUTPUT
           MOVE VAR1I TO OUTVAR1O.
           MOVE VAR2I TO OUTVAR2O.
           MOVE 'Donnees recues et affichees avec succes' TO MSGO.

      *    Renvoyer l'ecran avec les resultats
           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('MAPIO')
               FROM(MAP1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TIO ')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       2200-TOUCHE-INVALIDE.
      *================================================================*
           MOVE 'Touche non reconnue - Utilisez ENTER, PF3 ou CLEAR'
               TO MSGO.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('MAPIO')
               FROM(MAP1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TIO ')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TIO terminee - Au revoir')
               LENGTH(38)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
