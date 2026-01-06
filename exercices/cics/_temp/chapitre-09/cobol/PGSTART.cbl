       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGSTART.
      *================================================================*
      * Programme : PGSTART - Navigation VSAM Browse (Chapitre IX)     *
      * Transaction : TBRW                                             *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      * Description : STARTBR/READNEXT/READPREV/ENDBR                  *
      *               Affichage liste paginee                          *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Constantes ---
       01  WS-PAGE-SIZE            PIC 9(2) COMP VALUE 10.

      *--- Codes reponse CICS ---
       01  WS-RESP                 PIC S9(8) COMP.

      *--- Copybook DFHAID ---
       COPY DFHAID.

      *--- Structure enregistrement EMPLOYE ---
       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
           05  EMP-FILLER          PIC X(24).

      *--- Table pour stocker les enregistrements de la page ---
       01  WS-TABLE-EMP.
           05  WS-EMP-ENTRY OCCURS 10 TIMES.
               10  TE-ID           PIC X(6).
               10  TE-NAME         PIC X(30).
               10  TE-DEPT         PIC X(10).
               10  TE-SAL          PIC 9(7)V99.

      *--- Zone symbolique MAP (simplifiee) ---
       01  MAPBRW1I.
           05  FILLER              PIC X(12).
           05  STARTKEYL           PIC S9(4) COMP.
           05  STARTKEYF           PIC X.
           05  FILLER REDEFINES STARTKEYF.
               10  STARTKEYA       PIC X.
           05  STARTKEYI           PIC X(6).
      *    Lignes employes (10 lignes x 4 champs)
           05  WS-LINE-DATA OCCURS 10 TIMES.
               10  EMPIDL          PIC S9(4) COMP.
               10  EMPIDF          PIC X.
               10  EMPIDI          PIC X(6).
               10  EMPNOML         PIC S9(4) COMP.
               10  EMPNOMF         PIC X.
               10  EMPNOMI         PIC X(30).
               10  EMPDEPL         PIC S9(4) COMP.
               10  EMPDEPF         PIC X.
               10  EMPDEPI         PIC X(10).
               10  EMPSALL         PIC S9(4) COMP.
               10  EMPSALF         PIC X.
               10  EMPSALI         PIC 9(7)V99.
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAPBRW1O REDEFINES MAPBRW1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  STARTKEYO           PIC X(6).
           05  WS-LINE-OUT OCCURS 10 TIMES.
               10  FILLER          PIC X(3).
               10  EMPIDO          PIC X(6).
               10  FILLER          PIC X(3).
               10  EMPNOMO         PIC X(30).
               10  FILLER          PIC X(3).
               10  EMPDEPO         PIC X(10).
               10  FILLER          PIC X(3).
               10  EMPSALO         PIC 9(7)V99.
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
       01  WS-REC-KEY              PIC X(6).
       01  WS-FIRST-KEY            PIC X(6).
       01  WS-LAST-KEY             PIC X(6).
       01  WS-COUNT                PIC 9(2) COMP.
       01  WS-IDX                  PIC 9(2) COMP.

      *--- COMMAREA pour sauvegarder l'etat ---
       01  WS-COMMAREA.
           05  CA-FIRST-TIME       PIC X(1).
           05  CA-FIRST-KEY        PIC X(6).
           05  CA-LAST-KEY         PIC X(6).
           05  CA-DIRECTION        PIC X(1).
               88  CA-FORWARD      VALUE 'F'.
               88  CA-BACKWARD     VALUE 'B'.

       PROCEDURE DIVISION.

      *================================================================*
       0000-MAIN.
      *================================================================*
           IF EIBCALEN = 0
               PERFORM 1000-FIRST-TIME
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-PROCESS-INPUT
           END-IF.

           STOP RUN.

      *================================================================*
       1000-FIRST-TIME.
      *================================================================*
           INITIALIZE MAPBRW1O.
           MOVE SPACES TO WS-REC-KEY.
           PERFORM 3000-BROWSE-FORWARD.

           EXEC CICS SEND
               MAP('MAPBRW1')
               MAPSET('MAPBRWS')
               FROM(MAPBRW1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CA-FIRST-TIME.
           SET CA-FORWARD TO TRUE.
           EXEC CICS RETURN
               TRANSID('TBRW')
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
               WHEN DFHPF7
                   PERFORM 2100-PAGE-PREV
               WHEN DFHPF8
                   PERFORM 2200-PAGE-NEXT
               WHEN DFHENTER
                   PERFORM 2300-RECHERCHER
               WHEN OTHER
                   MOVE 'Touche invalide' TO MSGO
                   PERFORM 2400-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2100-PAGE-PREV.
      *================================================================*
      *    Page precedente : partir de la premiere cle - 1
           IF CA-FIRST-KEY = SPACES OR CA-FIRST-KEY = LOW-VALUES
               MOVE 'Debut du fichier' TO MSGO
               PERFORM 2400-SEND-DATAONLY
           END-IF.

           MOVE CA-FIRST-KEY TO WS-REC-KEY.
           PERFORM 3100-BROWSE-BACKWARD.
           PERFORM 2400-SEND-DATAONLY.

      *================================================================*
       2200-PAGE-NEXT.
      *================================================================*
      *    Page suivante : partir de la derniere cle
           IF CA-LAST-KEY = SPACES OR CA-LAST-KEY = HIGH-VALUES
               MOVE 'Fin du fichier' TO MSGO
               PERFORM 2400-SEND-DATAONLY
           END-IF.

           MOVE CA-LAST-KEY TO WS-REC-KEY.
           PERFORM 3000-BROWSE-FORWARD.
           PERFORM 2400-SEND-DATAONLY.

      *================================================================*
       2300-RECHERCHER.
      *================================================================*
      *    Recevoir la cle de depart
           INITIALIZE MAPBRW1I.

           EXEC CICS RECEIVE
               MAP('MAPBRW1')
               MAPSET('MAPBRWS')
               INTO(MAPBRW1I)
               RESP(WS-RESP)
           END-EXEC.

           IF STARTKEYL > 0
               MOVE STARTKEYI TO WS-REC-KEY
           ELSE
               MOVE SPACES TO WS-REC-KEY
           END-IF.

           PERFORM 3000-BROWSE-FORWARD.
           PERFORM 2400-SEND-DATAONLY.

      *================================================================*
       2400-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
               MAP('MAPBRW1')
               MAPSET('MAPBRWS')
               FROM(MAPBRW1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TBRW')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       3000-BROWSE-FORWARD.
      *================================================================*
      *    Navigation vers l'avant
           INITIALIZE WS-TABLE-EMP.
           INITIALIZE MAPBRW1O.
           MOVE 0 TO WS-COUNT.
           SET CA-FORWARD TO TRUE.

      *    Demarrer le browse
           EXEC CICS STARTBR
               FILE('EMPLOYE')
               RIDFLD(WS-REC-KEY)
               GTEQ
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Aucun enregistrement trouve' TO MSGO
               GO TO 3000-EXIT
           END-IF.

      *    Lire les enregistrements
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-PAGE-SIZE

               EXEC CICS READNEXT
                   FILE('EMPLOYE')
                   INTO(WS-EMPLOYE)
                   RIDFLD(WS-REC-KEY)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(NORMAL)
                   ADD 1 TO WS-COUNT
                   MOVE EMP-ID      TO TE-ID(WS-IDX)
                   MOVE EMP-NAME    TO TE-NAME(WS-IDX)
                   MOVE EMP-DEPT    TO TE-DEPT(WS-IDX)
                   MOVE EMP-SALAIRE TO TE-SAL(WS-IDX)

                   IF WS-IDX = 1
                       MOVE EMP-ID TO CA-FIRST-KEY
                   END-IF
                   MOVE EMP-ID TO CA-LAST-KEY
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *    Terminer le browse
           EXEC CICS ENDBR
               FILE('EMPLOYE')
               RESP(WS-RESP)
           END-EXEC.

      *    Remplir l'ecran
           PERFORM 4000-REMPLIR-ECRAN.

           IF WS-COUNT = 0
               MOVE 'Aucun enregistrement' TO MSGO
           ELSE
               STRING WS-COUNT ' employe(s) affiches'
                      DELIMITED SIZE INTO MSGO
           END-IF.

       3000-EXIT.
           EXIT.

      *================================================================*
       3100-BROWSE-BACKWARD.
      *================================================================*
      *    Navigation vers l'arriere
           INITIALIZE WS-TABLE-EMP.
           INITIALIZE MAPBRW1O.
           MOVE 0 TO WS-COUNT.
           SET CA-BACKWARD TO TRUE.

      *    Demarrer le browse
           EXEC CICS STARTBR
               FILE('EMPLOYE')
               RIDFLD(WS-REC-KEY)
               GTEQ
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Erreur positionnement' TO MSGO
               GO TO 3100-EXIT
           END-IF.

      *    Lire vers l'arriere
           PERFORM VARYING WS-IDX FROM WS-PAGE-SIZE BY -1
                   UNTIL WS-IDX < 1

               EXEC CICS READPREV
                   FILE('EMPLOYE')
                   INTO(WS-EMPLOYE)
                   RIDFLD(WS-REC-KEY)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(NORMAL)
                   ADD 1 TO WS-COUNT
                   MOVE EMP-ID      TO TE-ID(WS-IDX)
                   MOVE EMP-NAME    TO TE-NAME(WS-IDX)
                   MOVE EMP-DEPT    TO TE-DEPT(WS-IDX)
                   MOVE EMP-SALAIRE TO TE-SAL(WS-IDX)

                   IF WS-IDX = WS-PAGE-SIZE
                       MOVE EMP-ID TO CA-LAST-KEY
                   END-IF
                   MOVE EMP-ID TO CA-FIRST-KEY
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

      *    Terminer le browse
           EXEC CICS ENDBR
               FILE('EMPLOYE')
               RESP(WS-RESP)
           END-EXEC.

      *    Remplir l'ecran
           PERFORM 4000-REMPLIR-ECRAN.

           IF WS-COUNT = 0
               MOVE 'Debut du fichier' TO MSGO
           ELSE
               STRING WS-COUNT ' employe(s) affiches'
                      DELIMITED SIZE INTO MSGO
           END-IF.

       3100-EXIT.
           EXIT.

      *================================================================*
       4000-REMPLIR-ECRAN.
      *================================================================*
      *    Copier les donnees de la table vers l'ecran
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-PAGE-SIZE
               MOVE TE-ID(WS-IDX)   TO EMPIDO(WS-IDX)
               MOVE TE-NAME(WS-IDX) TO EMPNOMO(WS-IDX)
               MOVE TE-DEPT(WS-IDX) TO EMPDEPO(WS-IDX)
               MOVE TE-SAL(WS-IDX)  TO EMPSALO(WS-IDX)
           END-PERFORM.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TBRW terminee')
               LENGTH(25)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
