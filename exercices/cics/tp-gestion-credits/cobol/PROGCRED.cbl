      ******************************************************************
      * Programme : PROGCRED
      * Transaction : CRED
      * Fonction  : Gestion des credits employes (version simplifiee)
      *
      * Description :
      * 1. Lire ID-EMPL (saisi au niveau programme)
      * 2. Verifier si l'employe a un credit (ETAT-CRED-EMPL = 'Y')
      * 3. Lire les informations credit dans CRE-EMP
      * 4. Payer une echeance et mettre a jour RESTE-CREDIT
      *    Si RESTE-CREDIT = 0, positionner ETAT-CRED-EMPL a 'N'
      *
      * Fichiers VSAM :
      * - EMPLOYE : Informations employes (KSDS, cle 6 car)
      * - CREDEMP : Informations credits (KSDS, cle 6 car)
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

      *--- Cle de recherche ---
       01  WS-ID-EMPL              PIC X(6) VALUE 'EMP001'.

      *--- Structure EMPLOYE (52 octets) ---
       01  WS-EMPLOYE.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99 COMP-3.
           05  EMP-ETAT-CRED       PIC X(1).
               88  EMP-A-CREDIT    VALUE 'Y'.
               88  EMP-SANS-CREDIT VALUE 'N'.

      *--- Structure CREDIT (40 octets) ---
       01  WS-CREDIT.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(7)V99 COMP-3.
           05  CRD-MONTANT-ECH     PIC 9(5)V99 COMP-3.
           05  CRD-RESTE           PIC 9(7)V99 COMP-3.

      *--- Zone message pour affichage ---
       01  WS-MESSAGE              PIC X(79) VALUE SPACES.
       01  WS-LIGNE                PIC X(79) VALUE SPACES.

      *--- Variables de travail pour affichage ---
       01  WS-SALAIRE-EDIT         PIC ZZZ,ZZ9.99.
       01  WS-MONTANT-EDIT         PIC ZZZ,ZZ9.99.
       01  WS-RESTE-EDIT           PIC ZZZ,ZZ9.99.
       01  WS-ECH-EDIT             PIC ZZ,ZZ9.99.

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entree du programme
      ******************************************************************
       0000-PRINCIPAL.

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
                   STRING 'ERREUR: Employe ' DELIMITED SIZE
                          WS-ID-EMPL DELIMITED SIZE
                          ' non trouve' DELIMITED SIZE
                       INTO WS-MESSAGE
                   PERFORM 9100-AFFICHER-MESSAGE
               WHEN OTHER
                   STRING 'ERREUR VSAM lecture EMPLOYE, RESP='
                          DELIMITED SIZE
                          WS-RESP DELIMITED SIZE
                       INTO WS-MESSAGE
                   PERFORM 9100-AFFICHER-MESSAGE
           END-EVALUATE.

      ******************************************************************
      * 2000-AFFICHER-EMPLOYE : Affichage infos employe
      ******************************************************************
       2000-AFFICHER-EMPLOYE.

           MOVE '--- INFORMATIONS EMPLOYE ---' TO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'ID      : ' DELIMITED SIZE
                  EMP-ID DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Nom     : ' DELIMITED SIZE
                  EMP-NAME DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Dept    : ' DELIMITED SIZE
                  EMP-DEPT DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE EMP-SALAIRE TO WS-SALAIRE-EDIT
           STRING 'Salaire : ' DELIMITED SIZE
                  WS-SALAIRE-EDIT DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           STRING 'Credit  : ' DELIMITED SIZE
                  EMP-ETAT-CRED DELIMITED SIZE
                  ' (Y=Oui, N=Non)' DELIMITED SIZE
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
                      DELIMITED SIZE
                      WS-ID-EMPL DELIMITED SIZE
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

           STRING 'Libelle : ' DELIMITED SIZE
                  CRD-LIBELLE DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-MONTANT-TOTAL TO WS-MONTANT-EDIT
           STRING 'Montant total : ' DELIMITED SIZE
                  WS-MONTANT-EDIT DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-MONTANT-ECH TO WS-ECH-EDIT
           STRING 'Echeance      : ' DELIMITED SIZE
                  WS-ECH-EDIT DELIMITED SIZE
               INTO WS-MESSAGE
           PERFORM 9100-AFFICHER-MESSAGE

           MOVE CRD-RESTE TO WS-RESTE-EDIT
           STRING 'Reste a payer : ' DELIMITED SIZE
                  WS-RESTE-EDIT DELIMITED SIZE
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
                  DELIMITED SIZE
                  WS-RESTE-EDIT DELIMITED SIZE
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
