      ******************************************************************
      * Programme : CREDTRT - Couche Traitement
      * Fonction : Logique métier gestion des crédits employés
      *
      * Description :
      *   Ce programme contient la logique métier de l'application.
      *   Il orchestre les opérations de consultation et de paiement
      *   en appelant la couche données (CREDDAO).
      *
      * Actions :
      *   'C' = Consulter employé et son crédit
      *   'P' = Payer une échéance de crédit
      *
      * Appels : CREDDAO (couche données)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDTRT.
       AUTHOR. FORMATION-CICS.
       DATE-WRITTEN. 2024-01.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *─── Copybooks structures de données ───────────────────────────
           COPY EMPLOYE.
           COPY CREDEMP.

      *─── COMMAREA pour appel couche données ────────────────────────
       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

      *─── COMMAREA reçue de la couche présentation ──────────────────
       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-PAYER        VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
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
      * 0000-PRINCIPAL : Point d'entrée - Aiguillage des actions
      ******************************************************************
       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR

           EVALUATE TRUE
               WHEN CA-CONSULTER
                   PERFORM 1000-CONSULTER-EMPLOYE
               WHEN CA-PAYER
                   PERFORM 2000-PAYER-ECHEANCE
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-CONSULTER-EMPLOYE : Récupération données employé + crédit
      ******************************************************************
       1000-CONSULTER-EMPLOYE.

      *─── Lecture employé ────────────────────────────────────────────
           INITIALIZE WS-DAO-COMMAREA
           MOVE 'READ' TO DAO-ACTION
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               IF DAO-RESP = 13
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Employé non trouvé' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture employé' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

      *─── Transfert données employé ──────────────────────────────────
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
      * 1100-LIRE-CREDIT : Lecture détails crédit
      ******************************************************************
       1100-LIRE-CREDIT.

           MOVE 'READ' TO DAO-ACTION
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
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
               MOVE 'Erreur lecture crédit' TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * 2000-PAYER-ECHEANCE : Traitement paiement d'une échéance
      ******************************************************************
       2000-PAYER-ECHEANCE.

      *─── Lecture crédit avec verrouillage ───────────────────────────
           MOVE 'UPDT' TO DAO-ACTION
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture crédit pour MAJ' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           MOVE DAO-DATA TO CREDIT-REC

      *─── Calcul du nouveau reste ────────────────────────────────────
           SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE

           IF CRD-RESTE < 0
               MOVE 0 TO CRD-RESTE
           END-IF

      *─── Mise à jour crédit ─────────────────────────────────────────
           MOVE 'REWT' TO DAO-ACTION
           MOVE CREDIT-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur mise à jour crédit' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

      *─── Si crédit soldé, mettre à jour employé ─────────────────────
           IF CRD-RESTE = 0
               PERFORM 2100-SOLDER-CREDIT
           END-IF

      *─── Mise à jour COMMAREA retour ────────────────────────────────
           MOVE CRD-RESTE TO CA-RESTE
           MOVE 00 TO CA-CODE-RETOUR
           IF CRD-RESTE = 0
               MOVE 'N' TO CA-ETAT-CRED
               MOVE 'Crédit soldé ! Félicitations !' TO CA-MESSAGE
           ELSE
               MOVE 'Échéance payée avec succès' TO CA-MESSAGE
           END-IF.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 2100-SOLDER-CREDIT : Mise à jour employé quand crédit soldé
      ******************************************************************
       2100-SOLDER-CREDIT.

      *─── Lecture employé pour MAJ ───────────────────────────────────
           MOVE 'UPDT' TO DAO-ACTION
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               GO TO 2100-EXIT
           END-IF

           MOVE DAO-DATA TO EMPLOYE-REC
           SET EMP-SANS-CREDIT TO TRUE

      *─── Réécriture employé ─────────────────────────────────────────
           MOVE 'REWT' TO DAO-ACTION
           MOVE EMPLOYE-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC.

       2100-EXIT.
           EXIT.
