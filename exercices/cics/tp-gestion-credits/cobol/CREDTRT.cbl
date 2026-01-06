      ******************************************************************
      * Programme : CREDTRT - Couche Traitement
      * Fonction : Logique metier gestion des credits employes
      *
      * Description :
      *   Ce programme contient la logique metier de l'application.
      *   Il orchestre les operations de consultation et de paiement
      *   en appelant la couche donnees (CREDDAO).
      *
      * Actions :
      *   'C' = Consulter employe et son credit
      *   'P' = Payer une echeance de credit
      *
      * Appels : CREDDAO (couche donnees)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDTRT.
       AUTHOR. FORMATION-CICS.
       DATE-WRITTEN. 2024-01.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Structure enregistrement EMPLOYE (80 octets) --------------
       01  EMPLOYE-REC.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
               88 EMP-A-CREDIT         VALUE 'Y'.
               88 EMP-SANS-CREDIT      VALUE 'N'.
           05  FILLER              PIC X(24).

      *--- Structure enregistrement CREDIT (80 octets) ---------------
       01  CREDIT-REC.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(9)V99.
           05  CRD-MONTANT-ECH     PIC 9(7)V99.
           05  CRD-RESTE           PIC 9(9)V99.
           05  FILLER              PIC X(23).

      *--- COMMAREA pour appel couche donnees ------------------------
       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

      *--- COMMAREA recue de la couche presentation ------------------
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
      * 0000-PRINCIPAL : Point d'entree - Aiguillage des actions
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
      * 1000-CONSULTER-EMPLOYE : Recuperation donnees employe + credit
      ******************************************************************
       1000-CONSULTER-EMPLOYE.

      *--- Lecture employe --------------------------------------------
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
                   MOVE 'Employe non trouve' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture employe' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

      *--- Transfert donnees employe ----------------------------------
           MOVE DAO-DATA TO EMPLOYE-REC
           MOVE EMP-NAME TO CA-NAME
           MOVE EMP-DEPT TO CA-DEPT
           MOVE EMP-SALAIRE TO CA-SALAIRE
           MOVE EMP-ETAT-CRED TO CA-ETAT-CRED

      *--- Si credit actif, lire les details --------------------------
           IF EMP-A-CREDIT
               PERFORM 1100-LIRE-CREDIT
           ELSE
               INITIALIZE CA-CREDIT-DATA
               MOVE 00 TO CA-CODE-RETOUR
           END-IF.

       1000-EXIT.
           EXIT.

      ******************************************************************
      * 1100-LIRE-CREDIT : Lecture details credit
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
               MOVE 'Erreur lecture credit' TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * 2000-PAYER-ECHEANCE : Traitement paiement d'une echeance
      ******************************************************************
       2000-PAYER-ECHEANCE.

      *--- Lecture credit avec verrouillage ---------------------------
           MOVE 'UPDT' TO DAO-ACTION
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture credit pour MAJ' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           MOVE DAO-DATA TO CREDIT-REC

      *--- Calcul du nouveau reste ------------------------------------
           SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE

           IF CRD-RESTE < 0
               MOVE 0 TO CRD-RESTE
           END-IF

      *--- Mise a jour credit -----------------------------------------
           MOVE 'REWT' TO DAO-ACTION
           MOVE CREDIT-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur mise a jour credit' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

      *--- Si credit solde, mettre a jour employe ---------------------
           IF CRD-RESTE = 0
               PERFORM 2100-SOLDER-CREDIT
           END-IF

      *--- Mise a jour COMMAREA retour --------------------------------
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
      * 2100-SOLDER-CREDIT : Mise a jour employe quand credit solde
      ******************************************************************
       2100-SOLDER-CREDIT.

      *--- Lecture employe pour MAJ -----------------------------------
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

      *--- Reecriture employe -----------------------------------------
           MOVE 'REWT' TO DAO-ACTION
           MOVE EMPLOYE-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC.

       2100-EXIT.
           EXIT.
