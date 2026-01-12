      ******************************************************************
      * Programme : MENUPRES - Menu Principal Application Bancaire
      * Transaction : MENU
      * Fonction : Navigation vers les différentes fonctions
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENUPRES.
       AUTHOR. FORMATION-CICS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY MENUSET.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-CHOIX                PIC X(1).

       01  WS-COMMAREA.
           05  CA-RETOUR           PIC X(1).
               88  CA-DEPUIS-CLNT  VALUE 'C'.
               88  CA-DEPUIS-CPTE  VALUE 'P'.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(10).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           IF EIBCALEN = 0
               PERFORM 1000-AFFICHER-MENU
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

           EXEC CICS
               RETURN TRANSID('MENU')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-AFFICHER-MENU.

           INITIALIZE MENUMAPO
           MOVE 'Bienvenue - Selectionnez une option'
               TO MSGO

           EXEC CICS
               SEND MAP('MENUMAP')
                    MAPSET('MENUSET')
                    FROM(MENUMAPO)
                    ERASE
           END-EXEC.

       2000-TRAITER-SAISIE.

           EXEC CICS
               RECEIVE MAP('MENUMAP')
                       MAPSET('MENUSET')
                       INTO(MENUMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-MENU
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-TRAITER-CHOIX
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN OTHER
                   MOVE 'Touche non autorisee' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

       2100-TRAITER-CHOIX.

           MOVE CHOIXI TO WS-CHOIX

           EVALUATE WS-CHOIX
               WHEN '1'
                   EXEC CICS
                       XCTL PROGRAM('CLNTPRES')
                   END-EXEC
               WHEN '2'
                   EXEC CICS
                       XCTL PROGRAM('CPTEPRES')
                   END-EXEC
               WHEN OTHER
                   MOVE 'Choix invalide (1 ou 2)' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('MENUMAP')
                    MAPSET('MENUSET')
                    FROM(MENUMAPO)
                    DATAONLY
           END-EXEC.

       9000-QUITTER.

           EXEC CICS
               SEND TEXT FROM('Merci. A bientot.')
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
