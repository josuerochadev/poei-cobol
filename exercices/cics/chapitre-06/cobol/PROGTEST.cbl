       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGTEST.
      ******************************************************************
      * Programme : PROGTEST
      * TP Chapitre VI - Mise en pratique de la commande READ
      *
      * Description :
      *   Ce programme illustre la lecture d'un enregistrement VSAM
      *   (fichier EMPLOYE) et l'affichage des donnees via une MAP BMS.
      *
      * Transaction : TEST
      * Map         : MAPTEST (dans TESTSET)
      * Fichier     : EMPLOYE (KSDS)
      *
      * Fonctionnement :
      *   1. Affichage de l'ecran de saisie (MAPTEST)
      *   2. Saisie du code employe par l'utilisateur
      *   3. Lecture du fichier EMPLOYE avec la commande READ
      *   4. Affichage des donnees ou message d'erreur
      ******************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

      ******************************************************************
      * PARTIE 1 : Variables de travail
      ******************************************************************
       01  WS-VARIABLES.
           05  WS-RESP              PIC S9(8) COMP VALUE 0.
           05  WS-RESP2             PIC S9(8) COMP VALUE 0.
           05  WS-REC-KEY           PIC X(6).
           05  WS-MESSAGE           PIC X(70).
           05  WS-SALAIRE-EDIT      PIC ZZZ,ZZZ.99.

      ******************************************************************
      * PARTIE 2 : Structure du Data Set EMPLOYE (WS-REC-DATA)
      * Remplace la zone generique par la structure reelle
      ******************************************************************
       01  WS-REC-DATA.
           05  EMP-ID               PIC X(6).
           05  EMP-NAME             PIC X(30).
           05  EMP-DEPT             PIC X(10).
           05  EMP-SALAIRE          PIC 9(7)V99 COMP-3.
           05  EMP-ETAT-CRED        PIC X(1).
               88  EMP-A-CREDIT     VALUE 'Y'.
               88  EMP-SANS-CREDIT  VALUE 'N'.

      ******************************************************************
      * PARTIE 3 : Zone symbolique de la MAP
      * Generee par l'assemblage du BMS ou en copybook
      ******************************************************************
       COPY MAPTEST.

      ******************************************************************
      * PARTIE 4 : Constantes CICS
      ******************************************************************
       01  WS-CONSTANTES.
           05  WS-TRANS-ID          PIC X(4)  VALUE 'TEST'.
           05  WS-MAPSET            PIC X(8)  VALUE 'TESTSET'.
           05  WS-MAP               PIC X(8)  VALUE 'MAPTEST'.
           05  WS-FILE-EMPLOYE      PIC X(8)  VALUE 'EMPLOYE'.

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entree du programme
      ******************************************************************
       0000-PRINCIPAL.

           EXEC CICS HANDLE AID
               PF3(9000-FIN)
               ANYKEY(1000-TRAITEMENT)
           END-EXEC.

           EXEC CICS HANDLE CONDITION
               ERROR(8000-ERREUR)
           END-EXEC.

      *--- Premier passage : afficher ecran vide
           IF EIBCALEN = 0
               PERFORM 2000-AFFICHER-ECRAN
           ELSE
      *--- Retour : traiter la saisie
               PERFORM 1000-TRAITEMENT
           END-IF.

           EXEC CICS RETURN
               TRANSID(WS-TRANS-ID)
               COMMAREA(WS-VARIABLES)
               LENGTH(LENGTH OF WS-VARIABLES)
           END-EXEC.

           STOP RUN.

      ******************************************************************
      * 1000-TRAITEMENT : Traitement de la saisie utilisateur
      ******************************************************************
       1000-TRAITEMENT.

      *--- Recevoir les donnees de la MAP
           EXEC CICS RECEIVE
               MAP(WS-MAP)
               MAPSET(WS-MAPSET)
               INTO(MAPTESTI)
               RESP(WS-RESP)
           END-EXEC.

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Erreur reception MAP' TO WS-MESSAGE
               PERFORM 2000-AFFICHER-ECRAN
               GO TO 1000-EXIT
           END-IF.

      *--- Verifier si code employe saisi
           IF CODEEMPL = SPACES OR CODEEMPL = '______'
               MOVE 'Veuillez saisir un code employe' TO WS-MESSAGE
               PERFORM 2000-AFFICHER-ECRAN
               GO TO 1000-EXIT
           END-IF.

      *--- Preparer la cle de recherche
           MOVE CODEEMPI TO WS-REC-KEY.

      *--- Lire l'enregistrement employe
           PERFORM 3000-LIRE-EMPLOYE.

       1000-EXIT.
           EXIT.

      ******************************************************************
      * 2000-AFFICHER-ECRAN : Envoyer la MAP a l'ecran
      ******************************************************************
       2000-AFFICHER-ECRAN.

           MOVE WS-MESSAGE TO MSGO.
           MOVE -1 TO CODEEMPL.

           EXEC CICS SEND
               MAP(WS-MAP)
               MAPSET(WS-MAPSET)
               FROM(MAPTESTO)
               ERASE
               CURSOR
               RESP(WS-RESP)
           END-EXEC.

           MOVE SPACES TO WS-MESSAGE.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 3000-LIRE-EMPLOYE : Lecture fichier EMPLOYE avec READ
      * C'est ici que se trouve la mise en pratique de la commande READ
      ******************************************************************
       3000-LIRE-EMPLOYE.

      *===================================================================
      * COMMANDE READ CICS
      * - FILE     : Nom du fichier dans la FCT
      * - INTO     : Zone de reception (structure EMPLOYE)
      * - RIDFLD   : Zone contenant la cle de recherche
      * - RESP     : Code retour de l'operation
      *===================================================================
           EXEC CICS READ
               FILE(WS-FILE-EMPLOYE)
               INTO(WS-REC-DATA)
               RIDFLD(WS-REC-KEY)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC.

      *--- Analyser le code retour
           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   PERFORM 4000-AFFICHER-EMPLOYE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Employe non trouve dans le fichier'
                       TO WS-MESSAGE
                   PERFORM 5000-EFFACER-DONNEES
               WHEN DFHRESP(FILENOTFOUND)
                   MOVE 'Fichier EMPLOYE non defini dans FCT'
                       TO WS-MESSAGE
               WHEN DFHRESP(NOTOPEN)
                   MOVE 'Fichier EMPLOYE non ouvert'
                       TO WS-MESSAGE
               WHEN DFHRESP(DISABLED)
                   MOVE 'Fichier EMPLOYE desactive'
                       TO WS-MESSAGE
               WHEN OTHER
                   STRING 'Erreur READ - RESP=' WS-RESP
                       ' RESP2=' WS-RESP2
                       DELIMITED BY SIZE INTO WS-MESSAGE
           END-EVALUATE.

           PERFORM 2000-AFFICHER-ECRAN.

       3000-EXIT.
           EXIT.

      ******************************************************************
      * 4000-AFFICHER-EMPLOYE : Transferer donnees vers MAP
      ******************************************************************
       4000-AFFICHER-EMPLOYE.

           MOVE EMP-ID        TO CODEEMPO.
           MOVE EMP-NAME      TO NOMEMPO.
           MOVE EMP-DEPT      TO DEPTEMPO.

      *--- Formater le salaire
           MOVE EMP-SALAIRE   TO WS-SALAIRE-EDIT.
           MOVE WS-SALAIRE-EDIT TO SALEMPO.

      *--- Afficher le statut credit
           IF EMP-A-CREDIT
               MOVE 'CREDIT EN COURS' TO STATEMPO
           ELSE
               MOVE 'AUCUN CREDIT'    TO STATEMPO
           END-IF.

           MOVE 'Employe trouve' TO WS-MESSAGE.

       4000-EXIT.
           EXIT.

      ******************************************************************
      * 5000-EFFACER-DONNEES : Vider les champs de la MAP
      ******************************************************************
       5000-EFFACER-DONNEES.

           MOVE SPACES TO NOMEMPO.
           MOVE SPACES TO DEPTEMPO.
           MOVE SPACES TO SALEMPO.
           MOVE SPACES TO STATEMPO.

       5000-EXIT.
           EXIT.

      ******************************************************************
      * 8000-ERREUR : Gestion des erreurs CICS
      ******************************************************************
       8000-ERREUR.

           EXEC CICS SEND TEXT
               FROM('Erreur CICS - Programme PROGTEST')
               LENGTH(35)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

       8000-EXIT.
           EXIT.

      ******************************************************************
      * 9000-FIN : Fin du programme (PF3)
      ******************************************************************
       9000-FIN.

           EXEC CICS SEND TEXT
               FROM('Fin PROGTEST - Au revoir')
               LENGTH(25)
               ERASE
           END-EXEC.

           EXEC CICS RETURN
           END-EXEC.

       9000-EXIT.
           EXIT.
