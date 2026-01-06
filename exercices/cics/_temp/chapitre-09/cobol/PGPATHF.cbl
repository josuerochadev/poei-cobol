       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGPATHF.
      *================================================================*
      * Programme : PGPATHF - Acces via Alternate Index (Chapitre IX)  *
      * Transaction : TALT                                             *
      * Fichier : EMPLOYE (KSDS) via PATH EMPNOM (AIX sur NOM)         *
      * Description : Lecture par index alternatif (nom employe)       *
      *               Demonstration du parcours par PATH               *
      *================================================================*

       DATA DIVISION.
       WORKING-STORAGE SECTION.

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

      *--- Zone symbolique MAP ---
       01  MAPALT1I.
           05  FILLER              PIC X(12).
      *    Cle de recherche (nom)
           05  NOMCLEL             PIC S9(4) COMP.
           05  NOMCLEF             PIC X.
           05  FILLER REDEFINES NOMCLEF.
               10  NOMCLEA         PIC X.
           05  NOMCLEI             PIC X(30).
      *    Resultats trouves
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
      *    Nombre de doublons
           05  DUPCNTL             PIC S9(4) COMP.
           05  DUPCNTF             PIC X.
           05  FILLER REDEFINES DUPCNTF.
               10  DUPCNTA         PIC X.
           05  DUPCNTI             PIC 9(3).
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAPALT1O REDEFINES MAPALT1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  NOMCLEO             PIC X(30).
           05  FILLER              PIC X(3).
           05  EMPIDO              PIC X(6).
           05  FILLER              PIC X(3).
           05  EMPNOMO             PIC X(30).
           05  FILLER              PIC X(3).
           05  EMPDEPTO            PIC X(10).
           05  FILLER              PIC X(3).
           05  EMPSALO             PIC 9(7)V99.
           05  FILLER              PIC X(3).
           05  DUPCNTO             PIC 9(3).
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
       01  WS-AIX-KEY              PIC X(30).
       01  WS-DUP-COUNT            PIC 9(3) VALUE 0.
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
           INITIALIZE MAPALT1O.
           STRING 'Recherche par NOM via Alternate Index (PATH). '
                  'Saisissez un nom.'
                  DELIMITED SIZE INTO MSGO.

           EXEC CICS SEND
               MAP('MAPALT1')
               MAPSET('MAPREAD')
               FROM(MAPALT1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CA-FIRST-TIME.
           EXEC CICS RETURN
               TRANSID('TALT')
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
                   PERFORM 2100-READ-BY-AIX
               WHEN OTHER
                   MOVE 'Touche invalide' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2100-READ-BY-AIX.
      *================================================================*
      *    Recevoir les donnees
           INITIALIZE MAPALT1I.

           EXEC CICS RECEIVE
               MAP('MAPALT1')
               MAPSET('MAPREAD')
               INTO(MAPALT1I)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'Saisissez un nom a rechercher' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

           IF NOMCLEL = 0 OR NOMCLEI = SPACES
               MOVE 'Nom obligatoire' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Preparer la cle AIX
           MOVE NOMCLEI TO WS-AIX-KEY.

      *    Lecture via PATH (Alternate Index)
      *    Le FILE defini doit pointer vers le PATH de l'AIX
           EXEC CICS READ
               FILE('EMPNOM')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-AIX-KEY)
               GTEQ
               RESP(WS-RESP)
           END-EXEC.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 2110-AFFICHER-RESULTAT
               WHEN DFHRESP(NOTFND)
                   MOVE 'Aucun employe avec ce nom' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'PATH EMPNOM non ouvert' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN DFHRESP(FILENOTFOUND)
                   MOVE 'PATH EMPNOM non defini' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
               WHEN OTHER
                   STRING 'Erreur lecture AIX - RESP=' WS-RESP
                          DELIMITED SIZE INTO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2110-AFFICHER-RESULTAT.
      *================================================================*
      *    Afficher le premier enregistrement trouve
           MOVE EMP-ID      TO EMPIDO.
           MOVE EMP-NAME    TO EMPNOMO.
           MOVE EMP-DEPT    TO EMPDEPTO.
           MOVE EMP-SALAIRE TO EMPSALO.

      *    Compter les doublons (meme nom)
           MOVE 1 TO WS-DUP-COUNT.
           PERFORM 2120-COUNT-DUPLICATES.

           MOVE WS-DUP-COUNT TO DUPCNTO.
           STRING 'Trouve: ' EMP-ID ' (Nb doublons: '
                  WS-DUP-COUNT ')'
                  DELIMITED SIZE INTO MSGO.

           PERFORM 2300-SEND-DATAONLY.

      *================================================================*
       2120-COUNT-DUPLICATES.
      *================================================================*
      *    Parcourir pour compter les enregistrements avec meme nom
      *    Utilise le browse pour compter les doublons

           EXEC CICS STARTBR
               FILE('EMPNOM')
               RIDFLD(WS-AIX-KEY)
               GTEQ
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 2120-EXIT
           END-IF.

      *    Lire le premier (deja compte)
           EXEC CICS READNEXT
               FILE('EMPNOM')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-AIX-KEY)
               RESP(WS-RESP)
           END-EXEC.

      *    Compter les suivants avec le meme debut de cle
           PERFORM UNTIL WS-RESP NOT = DFHRESP(NORMAL)
               EXEC CICS READNEXT
                   FILE('EMPNOM')
                   INTO(WS-EMPLOYE)
                   RIDFLD(WS-AIX-KEY)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(NORMAL)
      *            Verifier si c'est toujours le meme nom
                   IF EMP-NAME(1:NOMCLEL) = NOMCLEI(1:NOMCLEL)
                       ADD 1 TO WS-DUP-COUNT
                   ELSE
                       EXIT PERFORM
                   END-IF
               END-IF
           END-PERFORM.

           EXEC CICS ENDBR
               FILE('EMPNOM')
               RESP(WS-RESP)
           END-EXEC.

       2120-EXIT.
           EXIT.

      *================================================================*
       2300-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
               MAP('MAPALT1')
               MAPSET('MAPREAD')
               FROM(MAPALT1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TALT')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TALT terminee')
               LENGTH(25)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
