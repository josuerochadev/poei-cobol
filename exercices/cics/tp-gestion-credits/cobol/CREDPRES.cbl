      ******************************************************************
      * Programme : CREDPRES
      * Couche    : Présentation
      * Transaction : CRED
      * Fonction  : Interface utilisateur gestion des crédits employés
      *
      * Description :
      * - Affichage de l'écran BMS CREDMAP
      * - Réception des saisies utilisateur
      * - Validation du format de l'ID employé
      * - Appel de la couche traitement (CREDTRT)
      * - Gestion des touches fonction (ENTER, PF3, PF5)
      *
      * Auteur    : Formation CICS
      * Date      : 2024
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDPRES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *─── Copybooks CICS ─────────────────────────────────────────────
           COPY DFHAID.
           COPY DFHBMSCA.

      *─── Copybook BMS généré ────────────────────────────────────────
           COPY CREDSET.

      *─── Variables de travail ───────────────────────────────────────
       01  WS-VARIABLES.
           05  WS-RESP             PIC S9(8) COMP VALUE 0.
           05  WS-RESP2            PIC S9(8) COMP VALUE 0.

      *─── COMMAREA pour échange avec couche traitement ───────────────
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

      *─── État du programme ──────────────────────────────────────────
       01  WS-ETAT                 PIC X(1) VALUE 'I'.
           88  PREMIER-PASSAGE     VALUE 'I'.
           88  SAISIE-EN-COURS     VALUE 'S'.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entrée du programme
      ******************************************************************
       0000-PRINCIPAL.

      *─── Premier passage : afficher écran vide ──────────────────────
           IF EIBCALEN = 0
               SET PREMIER-PASSAGE TO TRUE
               PERFORM 1000-AFFICHER-ECRAN-VIDE
           ELSE
      *─── Passages suivants : traiter la saisie ──────────────────────
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

      *─── Retour pseudo-conversationnel ──────────────────────────────
           EXEC CICS
               RETURN TRANSID('CRED')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

      ******************************************************************
      * 1000-AFFICHER-ECRAN-VIDE : Premier affichage
      ******************************************************************
       1000-AFFICHER-ECRAN-VIDE.

           INITIALIZE CREDMAPO
           MOVE 'Entrez un ID employe (EMP001-EMP006) et ENTER'
               TO MSGO

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    ERASE
           END-EXEC.

      ******************************************************************
      * 2000-TRAITER-SAISIE : Gestion des actions utilisateur
      ******************************************************************
       2000-TRAITER-SAISIE.

      *─── Réception des données saisies ──────────────────────────────
           EXEC CICS
               RECEIVE MAP('CREDMAP')
                       MAPSET('CREDSET')
                       INTO(CREDMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               IF WS-RESP = DFHRESP(MAPFAIL)
                   MOVE 'Aucune donnee saisie' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
               ELSE
                   PERFORM 1000-AFFICHER-ECRAN-VIDE
               END-IF
               GO TO 2000-EXIT
           END-IF

      *─── Analyse de la touche appuyée ───────────────────────────────
           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-RECHERCHER-EMPLOYE
               WHEN DFHPF5
                   PERFORM 2200-PAYER-ECHEANCE
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN DFHCLEAR
                   PERFORM 1000-AFFICHER-ECRAN-VIDE
               WHEN OTHER
                   MOVE 'Touche non autorisee - Utilisez ENTER, PF3, PF5
      -               ' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 2100-RECHERCHER-EMPLOYE : Recherche d'un employé
      ******************************************************************
       2100-RECHERCHER-EMPLOYE.

      *─── Validation du format ───────────────────────────────────────
           IF IDEMPLL = 0 OR IDEMPLI = SPACES
               MOVE 'ID employe obligatoire (ex: EMP001)' TO MSGO
               MOVE DFHBMDAR TO IDEMPLA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Vérification format EMPnnn ─────────────────────────────────
           IF IDEMPLI(1:3) NOT = 'EMP'
               MOVE 'Format invalide - Utilisez EMPnnn (ex: EMP001)'
                   TO MSGO
               MOVE DFHBMDAR TO IDEMPLA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Appel de la couche traitement ──────────────────────────────
           INITIALIZE WS-COMMAREA
           SET CA-CONSULTER TO TRUE
           MOVE IDEMPLI TO CA-ID-EMPL

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
                    LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC

      *─── Affichage du résultat ──────────────────────────────────────
           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               MOVE DFHBMDAR TO IDEMPLA
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2100-EXIT.
           EXIT.

      ******************************************************************
      * 2110-AFFICHER-RESULTAT : Affichage des données employé/crédit
      ******************************************************************
       2110-AFFICHER-RESULTAT.

      *─── Initialisation de l'écran de sortie ────────────────────────
           INITIALIZE CREDMAPO

      *─── Transfert des données employé ──────────────────────────────
           MOVE CA-ID-EMPL     TO IDEMPLO
           MOVE CA-NAME        TO NOMO
           MOVE CA-DEPT        TO DEPTO
           MOVE CA-SALAIRE     TO SALO

      *─── Affichage crédit si existant ───────────────────────────────
           IF CA-ETAT-CRED = 'Y'
               MOVE CA-LIBELLE     TO LIBCREDO
               MOVE CA-MONTANT-TOT TO MTTOTALO
               MOVE CA-MONTANT-ECH TO MTECHO
               MOVE CA-RESTE       TO RESTEO
               MOVE 'Credit actif - Appuyez PF5 pour payer une echeance
      -               ' TO MSGO
           ELSE
               MOVE SPACES TO LIBCREDO
               MOVE 0 TO MTTOTALO MTECHO RESTEO
               MOVE 'Cet employe n''a pas de credit en cours'
                   TO MSGO
           END-IF

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    ERASE
           END-EXEC.

      ******************************************************************
      * 2200-PAYER-ECHEANCE : Paiement d'une échéance de crédit
      ******************************************************************
       2200-PAYER-ECHEANCE.

      *─── Vérification : un crédit doit être affiché ─────────────────
           IF CA-ETAT-CRED NOT = 'Y'
               MOVE 'Aucun credit a payer pour cet employe' TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2200-EXIT
           END-IF

      *─── Appel traitement pour paiement ─────────────────────────────
           SET CA-PAYER TO TRUE

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
                    LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC

      *─── Affichage du résultat ──────────────────────────────────────
           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
               IF CA-ETAT-CRED = 'N'
                   MOVE 'FELICITATIONS ! Credit solde !' TO MSGO
                   EXEC CICS
                       SEND MAP('CREDMAP')
                            MAPSET('CREDSET')
                            FROM(CREDMAPO)
                            DATAONLY
                            ALARM
                   END-EXEC
               END-IF
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2200-EXIT.
           EXIT.

      ******************************************************************
      * 3000-AFFICHER-MESSAGE : Affichage d'un message
      ******************************************************************
       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    DATAONLY
                    CURSOR
           END-EXEC.

      ******************************************************************
      * 9000-QUITTER : Fin de la transaction
      ******************************************************************
       9000-QUITTER.

           EXEC CICS
               SEND TEXT FROM('Transaction CRED terminee. Au revoir.')
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
