       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGREAD.
      *================================================================*
      * Programme : PRGREAD - Lecture VSAM via MAP (Chapitre IX)       *
      * Transaction : TREA                                             *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      * Description : READ avec interface ecran BMS                    *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Codes reponse CICS ---
       01  WS-RESP                 PIC S9(8) COMP.

      *--- Copybook DFHAID pour les touches ---
       COPY DFHAID.

      *--- Structure enregistrement EMPLOYE ---
       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).

      *--- Zone symbolique MAP (generee par BMS) ---
       01  MAPREA1I.
           05  FILLER              PIC X(12).
           05  EMPCLEL             PIC S9(4) COMP.
           05  EMPCLEF             PIC X.
           05  FILLER REDEFINES EMPCLEF.
               10  EMPCLEA         PIC X.
           05  EMPCLEI             PIC X(6).
           05  EMPNOML             PIC S9(4) COMP.
           05  EMPNOMF             PIC X.
           05  FILLER REDEFINES EMPNOMF.
               10  EMPNOMA         PIC X.
           05  EMPNOMI             PIC X(30).
           05  EMPDEPTL            PIC S9(4) COMP.
           05  EMPDEPTF            PIC X.
           05  FILLER REDEFINES EMPDEPTF.
               10  EMPDEPTA        PIC X.
           05  EMPDEPTI            PIC X(10).
           05  EMPSALL             PIC S9(4) COMP.
           05  EMPSALF             PIC X.
           05  FILLER REDEFINES EMPSALF.
               10  EMPSALA         PIC X.
           05  EMPSALI             PIC 9(7)V99.
           05  EMPSTATL            PIC S9(4) COMP.
           05  EMPSTATF            PIC X.
           05  FILLER REDEFINES EMPSTATF.
               10  EMPSTATA        PIC X.
           05  EMPSTATI            PIC X(15).
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAPREA1O REDEFINES MAPREA1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  EMPCLEO             PIC X(6).
           05  FILLER              PIC X(3).
           05  EMPNOMO             PIC X(30).
           05  FILLER              PIC X(3).
           05  EMPDEPTO            PIC X(10).
           05  FILLER              PIC X(3).
           05  EMPSALO             PIC 9(7)V99.
           05  FILLER              PIC X(3).
           05  EMPSTATO            PIC X(15).
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
       01  WS-REC-KEY              PIC X(6).
       01  WS-COMMAREA.
           05  CA-FIRST-TIME       PIC X(1).

       PROCEDURE DIVISION.

      *================================================================*
       0000-MAIN.
      *================================================================*
           IF EIBCALEN = 0
               PERFORM 1000-FIRST-TIME
           ELSE
               PERFORM 2000-PROCESS-INPUT
           END-IF.

           STOP RUN.

      *================================================================*
       1000-FIRST-TIME.
      *================================================================*
      *    Premier affichage de l'ecran
           INITIALIZE MAPREA1O.
           MOVE 'Saisissez le code employe puis ENTER' TO MSGO.

           EXEC CICS SEND
               MAP('MAPREA1')
               MAPSET('MAPREAD')
               FROM(MAPREA1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CA-FIRST-TIME.
           EXEC CICS RETURN
               TRANSID('TREA')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       2000-PROCESS-INPUT.
      *================================================================*
           EVALUATE EIBAID
               WHEN DFHPF3
                   PERFORM 9000-FIN
               WHEN DFHCLEAR
                   PERFORM 1000-FIRST-TIME
               WHEN DFHENTER
                   PERFORM 2100-READ-EMPLOYE
               WHEN OTHER
                   PERFORM 2200-TOUCHE-INVALIDE
           END-EVALUATE.

      *================================================================*
       2100-READ-EMPLOYE.
      *================================================================*
      *    Recevoir les donnees de l'ecran
           INITIALIZE MAPREA1I.

           EXEC CICS RECEIVE
               MAP('MAPREA1')
               MAPSET('MAPREAD')
               INTO(MAPREA1I)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'Saisissez un code employe' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Verifier que le code est saisi
           IF EMPCLEL = 0 OR EMPCLEI = SPACES
               MOVE 'Code employe obligatoire' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Lecture VSAM
           MOVE EMPCLEI TO WS-REC-KEY.
           INITIALIZE WS-EMPLOYE.

           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               RESP(WS-RESP)
           END-EXEC.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2110-AFFICHER-EMPLOYE
               WHEN DFHRESP(NOTFND)
                   INITIALIZE MAPREA1O
                   MOVE EMPCLEI TO EMPCLEO
                   MOVE 'Employe non trouve' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'Fichier EMPLOYE non ouvert' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN OTHER
                   MOVE 'Erreur lecture VSAM' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2110-AFFICHER-EMPLOYE.
      *================================================================*
      *    Remplir les champs de sortie
           MOVE EMP-ID        TO EMPCLEO.
           MOVE EMP-NAME      TO EMPNOMO.
           MOVE EMP-DEPT      TO EMPDEPTO.
           MOVE EMP-SALAIRE   TO EMPSALO.

           EVALUATE EMP-ETAT-CRED
               WHEN 'O'
                   MOVE 'CREDIT ACTIF' TO EMPSTATO
               WHEN 'N'
                   MOVE 'PAS DE CREDIT' TO EMPSTATO
               WHEN OTHER
                   MOVE 'INCONNU' TO EMPSTATO
           END-EVALUATE.

           MOVE 'Employe trouve' TO MSGO.
           PERFORM 2300-SEND-DATAONLY.

      *================================================================*
       2200-TOUCHE-INVALIDE.
      *================================================================*
           MOVE 'Touche invalide - Utilisez ENTER, PF3 ou CLEAR'
               TO MSGO.
           PERFORM 2300-SEND-DATAONLY.

      *================================================================*
       2300-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
               MAP('MAPREA1')
               MAPSET('MAPREAD')
               FROM(MAPREA1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TREA')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TREA terminee')
               LENGTH(25)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
