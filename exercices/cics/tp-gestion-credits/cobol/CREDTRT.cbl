      ******************************************************************
      * Programme : CREDTRT
      * Couche    : Traitement (Business Logic)
      * Fonction  : Logique métier gestion des crédits employés
      *
      * Description :
      * - Lecture des informations employé
      * - Vérification de l'état crédit
      * - Lecture des détails du crédit
      * - Calcul du nouveau reste après paiement
      * - Mise à jour de l'état crédit si soldé
      *
      * Appel : Via LINK depuis CREDPRES
      * Auteur    : Formation CICS
      * Date      : 2024
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDTRT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *─── Copybooks structures de données ────────────────────────────
           COPY EMPLOYE.
           COPY CREDEMP.

      *─── COMMAREA pour appel couche données ─────────────────────────
       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
               88  DAO-READ        VALUE 'READ'.
               88  DAO-READ-UPD    VALUE 'UPDT'.
               88  DAO-REWRITE     VALUE 'REWT'.
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

      *─── COMMAREA échange avec couche présentation ──────────────────
       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-PAYER        VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
               88  CA-OK           VALUE 00.
               88  CA-NOTFND       VALUE 13.
               88  CA-PAS-CREDIT   VALUE 20.
               88  CA-ERREUR       VALUE 99.
           05  CA-MESSAGE          PIC X(60).
           05  CA-ID-EMPL          PIC X(6).
           05  CA-EMPLOYE-DATA.
               10  CA-NAME         PIC X(30).
               10  CA-DEPT         PIC X(10).
               10  CA-SALAIRE      PIC 9(7)V99.
               10  CA-ETAT-CRED    PIC X(1).
           05  CA-CREDIT-DATA.
               10  CA-LIBELLE      PIC X(20).
               10  CA-MONTANT-TOT  PIC 9(7)V99.
               10  CA-MONTANT-ECH  PIC 9(5)V99.
               10  CA-RESTE        PIC 9(7)V99.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Dispatcher des actions
      ******************************************************************
       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR
           MOVE SPACES TO CA-MESSAGE

      *─── Dispatch selon l'action demandée ───────────────────────────
           EVALUATE TRUE
               WHEN CA-CONSULTER
                   PERFORM 1000-CONSULTER-EMPLOYE
               WHEN CA-PAYER
                   PERFORM 2000-PAYER-ECHEANCE
               WHEN OTHER
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Action non reconnue' TO CA-MESSAGE
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA

           EXEC CICS
               RETURN
           END-EXEC.

      ******************************************************************
      * 1000-CONSULTER-EMPLOYE : Récupération données employé + crédit
      ******************************************************************
       1000-CONSULTER-EMPLOYE.

      *─── Appel couche données : lecture employé ─────────────────────
           INITIALIZE WS-DAO-COMMAREA
           SET DAO-READ TO TRUE
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC

      *─── Analyse du retour ──────────────────────────────────────────
           IF DAO-RESP NOT = 0
               IF DAO-RESP = 13
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Employe non trouve' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture employe' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

      *─── Transfert des données employé vers COMMAREA ────────────────
           MOVE DAO-DATA TO EMPLOYE-REC
           MOVE EMP-NAME TO CA-NAME
           MOVE EMP-DEPT TO CA-DEPT
           MOVE EMP-SALAIRE TO CA-SALAIRE
           MOVE EMP-ETAT-CRED TO CA-ETAT-CRED

      *─── Si crédit actif, lire les détails ──────────────────────────
           IF EMP-A-CREDIT
               PERFORM 1100-LIRE-CREDIT
           ELSE
               INITIALIZE CA-CREDIT-DATA
               MOVE 00 TO CA-CODE-RETOUR
           END-IF.

       1000-EXIT.
           EXIT.

      ******************************************************************
      * 1100-LIRE-CREDIT : Lecture des informations crédit
      ******************************************************************
       1100-LIRE-CREDIT.

           SET DAO-READ TO TRUE
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP = 0
               MOVE DAO-DATA TO CREDIT-REC
               MOVE CRD-LIBELLE TO CA-LIBELLE
               MOVE CRD-MONTANT-TOTAL TO CA-MONTANT-TOT
               MOVE CRD-MONTANT-ECH TO CA-MONTANT-ECH
               MOVE CRD-RESTE TO CA-RESTE
               MOVE 00 TO CA-CODE-RETOUR
           ELSE
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture credit' TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * 2000-PAYER-ECHEANCE : Traitement paiement d'une échéance
      ******************************************************************
       2000-PAYER-ECHEANCE.

      *─── Lecture crédit avec verrouillage (pour mise à jour) ────────
           INITIALIZE WS-DAO-COMMAREA
           SET DAO-READ-UPD TO TRUE
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture credit pour MAJ' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           MOVE DAO-DATA TO CREDIT-REC

      *─── RÈGLE MÉTIER : Calcul du nouveau reste ─────────────────────
           SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE

      *─── Protection contre valeurs négatives ────────────────────────
           IF CRD-RESTE < 0
               MOVE 0 TO CRD-RESTE
           END-IF

      *─── Mise à jour du crédit ──────────────────────────────────────
           SET DAO-REWRITE TO TRUE
           MOVE CREDIT-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur mise a jour credit' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

      *─── RÈGLE MÉTIER : Si crédit soldé, mettre à jour employé ──────
           IF CRD-RESTE = 0
               PERFORM 2100-SOLDER-CREDIT
           END-IF

      *─── Mise à jour COMMAREA pour retour ───────────────────────────
           MOVE CRD-RESTE TO CA-RESTE
           MOVE 00 TO CA-CODE-RETOUR

           IF CRD-RESTE = 0
               MOVE 'N' TO CA-ETAT-CRED
               MOVE 'Credit solde ! Felicitations !' TO CA-MESSAGE
           ELSE
               MOVE 'Echeance payee avec succes' TO CA-MESSAGE
           END-IF.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 2100-SOLDER-CREDIT : Mise à jour état employé (crédit soldé)
      ******************************************************************
       2100-SOLDER-CREDIT.

      *─── Lecture employé pour mise à jour ───────────────────────────
           INITIALIZE WS-DAO-COMMAREA
           SET DAO-READ-UPD TO TRUE
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               GO TO 2100-EXIT
           END-IF

           MOVE DAO-DATA TO EMPLOYE-REC

      *─── Passage de l'état crédit à 'N' ─────────────────────────────
           SET EMP-SANS-CREDIT TO TRUE

      *─── Réécriture de l'employé ────────────────────────────────────
           SET DAO-REWRITE TO TRUE
           MOVE EMPLOYE-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
                    LENGTH(LENGTH OF WS-DAO-COMMAREA)
           END-EXEC.

       2100-EXIT.
           EXIT.
