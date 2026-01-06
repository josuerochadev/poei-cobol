       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRGRGEN.
      *================================================================*
      * Programme : PRGRGEN - Lecture generique VSAM (Chapitre IX)     *
      * Transaction : TGEN                                             *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      * Description : READ avec options EQUAL/GTEQ et GENERIC          *
      *               Demonstration des differents modes de lecture    *
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
       01  MAPGEN1I.
           05  FILLER              PIC X(12).
      *    Mode de recherche (E=EQUAL, G=GTEQ, P=GENERIC)
           05  MODEL               PIC S9(4) COMP.
           05  MODEF               PIC X.
           05  FILLER REDEFINES MODEF.
               10  MODEA           PIC X.
           05  MODEI               PIC X(1).
      *    Cle de recherche
           05  CLEL                PIC S9(4) COMP.
           05  CLEF                PIC X.
           05  FILLER REDEFINES CLEF.
               10  CLEA            PIC X.
           05  CLEI                PIC X(6).
      *    Longueur cle (pour GENERIC)
           05  KEYLNL              PIC S9(4) COMP.
           05  KEYLNF              PIC X.
           05  FILLER REDEFINES KEYLNF.
               10  KEYLNA          PIC X.
           05  KEYLNI              PIC 9(1).
      *    Resultats
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
           05  MSGL                PIC S9(4) COMP.
           05  MSGF                PIC X.
           05  FILLER REDEFINES MSGF.
               10  MSGA            PIC X.
           05  MSGI                PIC X(70).

       01  MAPGEN1O REDEFINES MAPGEN1I.
           05  FILLER              PIC X(12).
           05  FILLER              PIC X(3).
           05  MODEO               PIC X(1).
           05  FILLER              PIC X(3).
           05  CLEO                PIC X(6).
           05  FILLER              PIC X(3).
           05  KEYLNO              PIC 9(1).
           05  FILLER              PIC X(3).
           05  EMPIDO              PIC X(6).
           05  FILLER              PIC X(3).
           05  EMPNOMO             PIC X(30).
           05  FILLER              PIC X(3).
           05  EMPDEPTO            PIC X(10).
           05  FILLER              PIC X(3).
           05  MSGO                PIC X(70).

      *--- Zone de travail ---
       01  WS-REC-KEY              PIC X(6).
       01  WS-KEYLEN               PIC S9(4) COMP.
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
           INITIALIZE MAPGEN1O.
           MOVE 'E' TO MODEO.
           MOVE 6   TO KEYLNO.
           STRING 'Mode: E=EQUAL G=GTEQ P=GENERIC (partiel). '
                  'Entrez cle puis ENTER'
                  DELIMITED SIZE INTO MSGO.

           EXEC CICS SEND
               MAP('MAPGEN1')
               MAPSET('MAPREAD')
               FROM(MAPGEN1O)
               ERASE
               FREEKB
               RESP(WS-RESP)
           END-EXEC.

           MOVE 'N' TO CA-FIRST-TIME.
           EXEC CICS RETURN
               TRANSID('TGEN')
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
                   PERFORM 2100-READ-GENERIQUE
               WHEN OTHER
                   MOVE 'Touche invalide' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2100-READ-GENERIQUE.
      *================================================================*
           INITIALIZE MAPGEN1I.

           EXEC CICS RECEIVE
               MAP('MAPGEN1')
               MAPSET('MAPREAD')
               INTO(MAPGEN1I)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(MAPFAIL)
               MOVE 'Saisissez les parametres' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *    Preparer la cle
           MOVE CLEI TO WS-REC-KEY.

      *    Lecture selon le mode choisi
           EVALUATE MODEI
               WHEN 'E'
                   PERFORM 2110-READ-EQUAL
               WHEN 'G'
                   PERFORM 2120-READ-GTEQ
               WHEN 'P'
                   PERFORM 2130-READ-GENERIC
               WHEN OTHER
                   MOVE 'Mode invalide (E, G ou P)' TO MSGO
                   PERFORM 2300-SEND-DATAONLY
           END-EVALUATE.

      *================================================================*
       2110-READ-EQUAL.
      *================================================================*
      *    Lecture exacte par cle
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               EQUAL
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 2200-AFFICHER-RESULTAT
           ELSE
               MOVE 'Employe non trouve (EQUAL)' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *================================================================*
       2120-READ-GTEQ.
      *================================================================*
      *    Lecture >= cle (premier enregistrement >= cle)
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               GTEQ
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 2200-AFFICHER-RESULTAT
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 'Aucun employe >= cle saisie' TO MSGO
               ELSE
                   MOVE 'Erreur lecture GTEQ' TO MSGO
               END-IF
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *================================================================*
       2130-READ-GENERIC.
      *================================================================*
      *    Lecture par cle partielle
           IF KEYLNI < 1 OR KEYLNI > 6
               MOVE 'Longueur cle: 1 a 6' TO MSGO
               PERFORM 2300-SEND-DATAONLY
           END-IF.

           MOVE KEYLNI TO WS-KEYLEN.

           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-REC-KEY)
               KEYLENGTH(WS-KEYLEN)
               GENERIC
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 2200-AFFICHER-RESULTAT
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 'Aucun employe avec ce prefixe' TO MSGO
               ELSE
                   MOVE 'Erreur lecture GENERIC' TO MSGO
               END-IF
               PERFORM 2300-SEND-DATAONLY
           END-IF.

      *================================================================*
       2200-AFFICHER-RESULTAT.
      *================================================================*
           MOVE EMP-ID   TO EMPIDO.
           MOVE EMP-NAME TO EMPNOMO.
           MOVE EMP-DEPT TO EMPDEPTO.

           STRING 'Trouve: ' EMP-ID ' - Mode ' MODEI
                  DELIMITED SIZE INTO MSGO.

           PERFORM 2300-SEND-DATAONLY.

      *================================================================*
       2300-SEND-DATAONLY.
      *================================================================*
           EXEC CICS SEND
               MAP('MAPGEN1')
               MAPSET('MAPREAD')
               FROM(MAPGEN1O)
               DATAONLY
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('TGEN')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      *================================================================*
       9000-FIN.
      *================================================================*
           EXEC CICS SEND TEXT
               FROM('Transaction TGEN terminee')
               LENGTH(25)
               ERASE
               FREEKB
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.
