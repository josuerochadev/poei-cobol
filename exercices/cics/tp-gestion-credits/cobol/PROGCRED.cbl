      ******************************************************************
      * Programme : PROGCRED
      * Transaction : CRED
      * Fonction  : Gestion des credits employes (version simplifiee)
      *
      * Usage : CRED EMP001  (saisir ID apres la transaction)
      *
      * Description :
      * 1. Recevoir ID-EMPL saisi par l'utilisateur
      * 2. Verifier si l'employe a un credit (ETAT-CRED-EMPL = 'Y')
      * 3. Lire les informations credit dans CRE-EMP
      * 4. Payer une echeance et mettre a jour RESTE-CREDIT
      *    Si RESTE-CREDIT = 0, positionner ETAT-CRED-EMPL a 'N'
      *
      * Fichiers VSAM :
      * - EMPLOYE : Informations employes (KSDS, cle 6 car, 80 oct)
      * - CREDEMP : Informations credits (KSDS, cle 6 car, 40 oct)
      *
      * Auteur    : Formation CICS
      * Date      : 2024
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCRED.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Variables de controle CICS ---
       01  WS-RESP                 PIC S9(8) COMP VALUE 0.
       01  WS-RESP2                PIC S9(8) COMP VALUE 0.

      *--- Zone de saisie ---
       01  WS-INPUT-DATA           PIC X(80) VALUE SPACES.
       01  WS-INPUT-LEN            PIC S9(4) COMP VALUE 80.
       01  WS-ID-EMPL              PIC X(6) VALUE SPACES.

      *--- Structure EMPLOYE (80 octets) - Format DISPLAY ---
       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
               88  EMP-A-CREDIT    VALUE 'Y'.
               88  EMP-SANS-CREDIT VALUE 'N'.
           05  EMP-FILLER          PIC X(24).

      *--- Structure CREDIT (40 octets) - Format DISPLAY ---
       01  WS-CREDIT.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(7)V99.
           05  CRD-MONTANT-ECH     PIC 9(5)V99.
           05  CRD-RESTE           PIC 9(7)V99.

      *--- Zone message pour affichage ---
       01  WS-MESSAGE              PIC X(79) VALUE SPACES.

      *--- Variables de travail pour affichage ---
       01  WS-SALAIRE-EDIT         PIC ZZZ,ZZ9.99.
       01  WS-MONTANT-EDIT         PIC ZZZ,ZZ9.99.
       01  WS-RESTE-EDIT           PIC ZZZ,ZZ9.99.
       01  WS-ECH-EDIT             PIC ZZ,ZZ9.99.
       01  WS-RESP-EDIT            PIC -9(8).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entree du programme
      ******************************************************************
       0000-PRINCIPAL.

           PERFORM 0100-RECEVOIR-ID

           IF WS-ID-EMPL = SPACES
               MOVE 'Usage: CRED EMP001 (saisir ID employe)'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
               PERFORM 9000-FIN-PROGRAMME
           END-IF

           PERFORM 1000-LIRE-EMPLOYE

           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM 2000-AFFICHER-EMPLOYE
               IF EMP-A-CREDIT
                   PERFORM 3000-LIRE-CREDIT
                   IF WS-RESP = DFHRESP(NORMAL)
                       PERFORM 4000-AFFICHER-CREDIT
                       PERFORM 5000-PAYER-ECHEANCE
                   END-IF
               ELSE
                   MOVE 'Cet employe n''a pas de credit en cours'
                       TO WS-MESSAGE
                   PERFORM 9100-AFFICHER-MESSAGE
               END-IF
           END-IF

           PERFORM 9000-FIN-PROGRAMME.

      ******************************************************************
      * 0100-RECEVOIR-ID : Reception de l'ID employe saisi
      ******************************************************************
       0100-RECEVOIR-ID.

           EXEC CICS RECEIVE
               INTO(WS-INPUT-DATA)
               LENGTH(WS-INPUT-LEN)
               RESP(WS-RESP)
           END-EXEC

      *    EOC (End Of Chain) est normal pour RECEIVE terminal
           IF WS-RESP = DFHRESP(NORMAL) OR
              WS-RESP = DFHRESP(EOC)
      *        Extraire l'ID (apres 'CRED ')
               IF WS-INPUT-LEN > 5
                   MOVE WS-INPUT-DATA(6:6) TO WS-ID-EMPL
               END-IF
           END-IF.

      ******************************************************************
      * 1000-LIRE-EMPLOYE : Lecture fichier EMPLOYE par cle
      ******************************************************************
       1000-LIRE-EMPLOYE.

           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-ID-EMPL)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(NOTFND)
                   STRING 'ERREUR: Employe ' DELIMITED BY SIZE
                          WS-ID-EMPL DELIMITED BY SIZE
                          ' non trouve' DELIMITED BY SIZE
                       INTO WS-MESSAGE
                   PERFORM 9100-AFFICHER-MESSAGE
               WHEN OTHER
                   MOVE WS-RESP TO WS-RESP-EDIT
                   STRING 'ERREUR VSAM lecture EMPLOYE, RESP='
                          DELIMITED BY SIZE
                          WS-RESP-EDIT DELIMITED BY SIZE
                       INTO WS-MESSAGE
                   PERFORM 9100-AFFICHER-MESSAGE
           END-EVALUATE.

      ******************************************************************
      * 2000-AFFICHER-EMPLOYE : Affichage infos employe
      ******************************************************************
       2000-AFFICHER-EMPLOYE.

           MOVE '--- INFORMATIONS EMPLOYE ---' TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'ID      : ' DELIMITED BY SIZE
                  EMP-ID DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Nom     : ' DELIMITED BY SIZE
                  EMP-NAME DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Dept    : ' DELIMITED BY SIZE
                  EMP-DEPT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE EMP-SALAIRE TO WS-SALAIRE-EDIT
           STRING 'Salaire : ' DELIMITED BY SIZE
                  WS-SALAIRE-EDIT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Credit  : ' DELIMITED BY SIZE
                  EMP-ETAT-CRED DELIMITED BY SIZE
                  ' (Y=Oui, N=Non)' DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE.

      ******************************************************************
      * 3000-LIRE-CREDIT : Lecture fichier CRE-EMP par cle
      ******************************************************************
       3000-LIRE-CREDIT.

           EXEC CICS READ
               FILE('CREDEMP')
               INTO(WS-CREDIT)
               RIDFLD(WS-ID-EMPL)
               RESP(WS-RESP)
               RESP2(WS-RESP2)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               STRING 'ERREUR: Credit non trouve pour '
                      DELIMITED BY SIZE
                      WS-ID-EMPL DELIMITED BY SIZE
                   INTO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
           END-IF.

      ******************************************************************
      * 4000-AFFICHER-CREDIT : Affichage infos credit
      ******************************************************************
       4000-AFFICHER-CREDIT.

           MOVE SPACES TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE '--- INFORMATIONS CREDIT ---' TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Libelle : ' DELIMITED BY SIZE
                  CRD-LIBELLE DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-MONTANT-TOTAL TO WS-MONTANT-EDIT
           STRING 'Montant total : ' DELIMITED BY SIZE
                  WS-MONTANT-EDIT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-MONTANT-ECH TO WS-ECH-EDIT
           STRING 'Echeance      : ' DELIMITED BY SIZE
                  WS-ECH-EDIT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-RESTE TO WS-RESTE-EDIT
           STRING 'Reste a payer : ' DELIMITED BY SIZE
                  WS-RESTE-EDIT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE.

      ******************************************************************
      * 5000-PAYER-ECHEANCE : Paiement d'une echeance
      * Sequence : READ UPDATE -> Calcul -> REWRITE
      ******************************************************************
       5000-PAYER-ECHEANCE.

           MOVE SPACES TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE '--- PAIEMENT ECHEANCE ---' TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

      *--- Lecture avec verrouillage (UPDATE) ---
           EXEC CICS READ
               FILE('CREDEMP')
               INTO(WS-CREDIT)
               RIDFLD(WS-ID-EMPL)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR: Impossible de verrouiller le credit'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
               GO TO 5000-EXIT
           END-IF

      *--- Calcul nouveau reste ---
           SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE

           IF CRD-RESTE < 0
               MOVE 0 TO CRD-RESTE
           END-IF

      *--- Reecriture du credit ---
           EXEC CICS REWRITE
               FILE('CREDEMP')
               FROM(WS-CREDIT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR: Echec mise a jour credit'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
               GO TO 5000-EXIT
           END-IF

           MOVE CRD-RESTE TO WS-RESTE-EDIT
           STRING 'Echeance payee ! Nouveau reste : '
                  DELIMITED BY SIZE
                  WS-RESTE-EDIT DELIMITED BY SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

      *--- Si reste = 0, solder le credit (maj EMPLOYE) ---
           IF CRD-RESTE = 0
               PERFORM 6000-SOLDER-CREDIT
           END-IF.

       5000-EXIT.
           EXIT.

      ******************************************************************
      * 6000-SOLDER-CREDIT : Mise a jour ETAT-CRED-EMPL a 'N'
      * Sequence : READ UPDATE -> Modification -> REWRITE
      ******************************************************************
       6000-SOLDER-CREDIT.

           MOVE SPACES TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE '*** CREDIT SOLDE ! ***' TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

      *--- Lecture EMPLOYE avec verrouillage ---
           EXEC CICS READ
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-ID-EMPL)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR: Impossible de verrouiller employe'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
               GO TO 6000-EXIT
           END-IF

      *--- Passage de l'etat credit a 'N' ---
           SET EMP-SANS-CREDIT TO TRUE

      *--- Reecriture de l'employe ---
           EXEC CICS REWRITE
               FILE('EMPLOYE')
               FROM(WS-EMPLOYE)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Etat credit employe mis a jour (N)'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
           ELSE
               MOVE 'ERREUR: Echec mise a jour employe'
                   TO WS-MESSAGE
               PERFORM 9100-AFFICHER-MESSAGE
           END-IF.

       6000-EXIT.
           EXIT.

      ******************************************************************
      * 9000-FIN-PROGRAMME : Retour CICS
      ******************************************************************
       9000-FIN-PROGRAMME.

           EXEC CICS RETURN
           END-EXEC.

      ******************************************************************
      * 9100-AFFICHER-MESSAGE : Affichage via SEND TEXT
      ******************************************************************
       9100-AFFICHER-MESSAGE.

           EXEC CICS SEND TEXT
               FROM(WS-MESSAGE)
               LENGTH(79)
               ACCUM
           END-EXEC

           MOVE SPACES TO WS-MESSAGE.
