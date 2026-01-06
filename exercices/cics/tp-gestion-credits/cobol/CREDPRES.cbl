      ******************************************************************
      * Programme : CREDPRES - Couche Presentation
      * Transaction : CRED
      * Fonction : Interface utilisateur gestion credits employes
      *
      * Description :
      *   Ce programme gere l'interface utilisateur de l'application
      *   de gestion des credits. Il affiche l'ecran BMS, recoit les
      *   saisies et appelle la couche Traitement via LINK.
      *
      * Écrans : CREDMAP (MAPSET CREDSET)
      * Touches : ENTER=Rechercher, PF5=Payer, PF3=Quitter
      *
      * Appels : CREDTRT (couche traitement)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDPRES.
       AUTHOR. FORMATION-CICS.
       DATE-WRITTEN. 2024-01.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Copybooks systeme CICS ------------------------------------
           COPY DFHAID.
           COPY DFHBMSCA.

      *--- Copybook ecran BMS ----------------------------------------
           COPY CREDSET.

      *--- Variables de travail --------------------------------------
       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-MSG-FIN              PIC X(40)
           VALUE 'Fin de transaction CRED. Au revoir.'.

      *--- COMMAREA pour echange avec couche traitement --------------
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

      *--- Indicateur etat transaction -------------------------------
       01  WS-ETAT                 PIC X(1) VALUE 'I'.
           88  PREMIER-PASSAGE     VALUE 'I'.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Point d'entree du programme
      ******************************************************************
       0000-PRINCIPAL.

           IF EIBCALEN = 0
               SET PREMIER-PASSAGE TO TRUE
               PERFORM 1000-AFFICHER-ECRAN-VIDE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

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
           MOVE 'Entrez un ID employe (EMP001-EMP006)'
               TO MSGO

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    ERASE
           END-EXEC.

      ******************************************************************
      * 2000-TRAITER-SAISIE : Traitement des entrees utilisateur
      ******************************************************************
       2000-TRAITER-SAISIE.

           EXEC CICS
               RECEIVE MAP('CREDMAP')
                       MAPSET('CREDSET')
                       INTO(CREDMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-ECRAN-VIDE
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-RECHERCHER-EMPLOYE
               WHEN DFHPF5
                   PERFORM 2200-PAYER-ECHEANCE
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN OTHER
                   MOVE 'Touche non autorisee' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 2100-RECHERCHER-EMPLOYE : Recherche d'un employe
      ******************************************************************
       2100-RECHERCHER-EMPLOYE.

      *--- Validation format ------------------------------------------
           IF IDEMPLL = 0 OR IDEMPLI = SPACES
               MOVE 'ID employe obligatoire' TO MSGO
               MOVE DFHBMDAR TO IDEMPLA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *--- Appel couche traitement ------------------------------------
           INITIALIZE WS-COMMAREA
           SET CA-CONSULTER TO TRUE
           MOVE IDEMPLI TO CA-ID-EMPL

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

      *--- Affichage resultat -----------------------------------------
           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2100-EXIT.
           EXIT.

      ******************************************************************
      * 2110-AFFICHER-RESULTAT : Affichage donnees employe/credit
      ******************************************************************
       2110-AFFICHER-RESULTAT.

           MOVE CA-ID-EMPL     TO IDEMPLO
           MOVE CA-NAME        TO NOMO
           MOVE CA-DEPT        TO DEPTO
           MOVE CA-SALAIRE     TO SALO

           IF CA-ETAT-CRED = 'Y'
               MOVE CA-LIBELLE     TO LIBCREDO
               MOVE CA-MONTANT-TOT TO MTTOTALO
               MOVE CA-MONTANT-ECH TO MTECHO
               MOVE CA-RESTE       TO RESTEO
               MOVE 'Credit actif - PF5 pour payer une echeance'
                   TO MSGO
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
      * 2200-PAYER-ECHEANCE : Paiement d'une echeance
      ******************************************************************
       2200-PAYER-ECHEANCE.

           IF CA-ETAT-CRED NOT = 'Y'
               MOVE 'Aucun credit a payer pour cet employe'
                   TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2200-EXIT
           END-IF

           SET CA-PAYER TO TRUE

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2200-EXIT.
           EXIT.

      ******************************************************************
      * 3000-AFFICHER-MESSAGE : Affichage message sans effacer
      ******************************************************************
       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    DATAONLY
           END-EXEC.

      ******************************************************************
      * 9000-QUITTER : Fin de la transaction
      ******************************************************************
       9000-QUITTER.

           EXEC CICS
               SEND TEXT FROM(WS-MSG-FIN)
                    LENGTH(40)
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
