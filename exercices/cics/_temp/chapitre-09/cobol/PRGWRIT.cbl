       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGWRIT.
      *================================================================*
      * Programme : PRGWRIT - Ecriture VSAM via MAP (Chapitre IX)      *
      * Transaction : TWRT                                             *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      * Description : WRITE avec interface ecran BMS                   *
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
       01  MAPWRT1I.
           05  FILLER              PIC X(12).
           05  EMPIDL              PIC S9(4) COMP.
           05  EMPIDF              PIC X.
           05  FILLER REDEFINES EMPIDF.
               10  EMPIDA          PIC X.
           05  EMPIDI              PIC X(6).
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
           05  EMPCREDL            PIC S9(4) COMP.
           05  EMPCREDF            PIC X.
           05  FILLER REDEFINES EMPCREDF.
               10  EMPCREDA        PIC X.
           05  EMPCREDI            PIC X(1).
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAPWRT1O REDEFINES MAPWRT1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  EMPIDO              PIC X(6).
           05  FILLER              PIC X(3).
           05  EMPNOMO             PIC X(30).
           05  FILLER              PIC X(3).
           05  EMPDEPTO            PIC X(10).
           05  FILLER              PIC X(3).
           05  EMPSALO             PIC 9(7)V99.
           05  FILLER              PIC X(3).
           05  EMPCREDO            PIC X(1).
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
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
           INITIALIZE MAPWRT1O.
           MOVE 'Saisissez les donnees employe puis ENTER' TO MSGO.

           EXEC CICS SEND
               MAP('MAPWRT1')
               MAPSET('MAPWRIT')
               FROM(MAPWRT1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CA-FIRST-TIME.
           EXEC CICS RETURN
               TRANSID('TWRT')
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
                   PERFORM 2100-WRITE-EMPLOYE
               WHEN OTHER
                   MOVE 'Touche invalide - ENTER, PF3 ou CLEAR'
                       TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2100-WRITE-EMPLOYE.
      *================================================================*
      *    Recevoir les donnees de l'ecran
           INITIALIZE MAPWRT1I.

           EXEC CICS RECEIVE
               MAP('MAPWRT1')
               MAPSET('MAPWRIT')
               INTO(MAPWRT1I)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'Saisissez les donnees employe' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Validation des champs obligatoires
           IF EMPIDL = 0 OR EMPIDI = SPACES
               MOVE 'Code employe obligatoire' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

           IF EMPNOML = 0 OR EMPNOMI = SPACES
               MOVE 'Nom obligatoire' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Preparer l'enregistrement
           INITIALIZE WS-EMPLOYE.
           MOVE EMPIDI   TO EMP-ID.
           MOVE EMPNOMI  TO EMP-NAME.
           MOVE EMPDEPTI TO EMP-DEPT.
           MOVE EMPSALI  TO EMP-SALAIRE.

           IF EMPCREDI = 'O' OR EMPCREDI = 'o'
               MOVE 'O' TO EMP-ETAT-CRED
           ELSE
               MOVE 'N' TO EMP-ETAT-CRED
           END-IF.

           MOVE SPACES TO EMP-FILLER.

      *    Ecriture VSAM
           EXEC CICS WRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RIDFLD(EMP-ID)
               RESP(WS-RESP)
           END-EXEC.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   STRING 'Employe ' EMP-ID ' cree avec succes'
                          DELIMITED SIZE INTO MSGO
                   INITIALIZE MAPWRT1O
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(DUPREC)
                   STRING 'Erreur: Code ' EMP-ID ' existe deja'
                          DELIMITED SIZE INTO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(NOSPACE)
                   MOVE 'Fichier VSAM plein' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'Fichier EMPLOYE non ouvert' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN OTHER
                   MOVE 'Erreur ecriture VSAM' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2300-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
               MAP('MAPWRT1')
               MAPSET('MAPWRIT')
               FROM(MAPWRT1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TWRT')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TWRT terminee')
               LENGTH(25)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
