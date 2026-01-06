      ******************************************************************
      * Programme : CREDDAO - Couche Donnees (Data Access Object)
      * Fonction : Acces aux fichiers VSAM EMPLOYE et CRE-EMP
      *
      * Description :
      *   Ce programme encapsule tous les acces aux fichiers VSAM.
      *   Il fournit une interface uniforme pour les operations CRUD.
      *
      * Actions supportees :
      *   'READ' = Lecture simple
      *   'UPDT' = Lecture avec verrouillage (pour mise a jour)
      *   'REWT' = Reecriture apres verrouillage
      *   'WRIT' = Creation nouvel enregistrement
      *   'DELT' = Suppression
      *
      * Fichiers : EMPLOYE (KSDS), CREDEMP (KSDS)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDDAO.
       AUTHOR. FORMATION-CICS.
       DATE-WRITTEN. 2024-01.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *--- Variables de travail --------------------------------------
       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-FICHIER              PIC X(8).
       01  WS-DATA-BUFFER          PIC X(100).

      *--- COMMAREA interface ----------------------------------------
       01  WS-COMMAREA.
           05  DAO-ACTION          PIC X(4).
               88  DAO-READ        VALUE 'READ'.
               88  DAO-READ-UPD    VALUE 'UPDT'.
               88  DAO-REWRITE     VALUE 'REWT'.
               88  DAO-WRITE       VALUE 'WRIT'.
               88  DAO-DELETE      VALUE 'DELT'.
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(122).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Aiguillage selon l'action demandee
      ******************************************************************
       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 0 TO DAO-RESP

           EVALUATE TRUE
               WHEN DAO-READ
                   PERFORM 1000-LIRE
               WHEN DAO-READ-UPD
                   PERFORM 1100-LIRE-POUR-MAJ
               WHEN DAO-REWRITE
                   PERFORM 2000-REECRIRE
               WHEN DAO-WRITE
                   PERFORM 3000-ECRIRE
               WHEN DAO-DELETE
                   PERFORM 4000-SUPPRIMER
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-LIRE : Lecture simple sans verrouillage
      ******************************************************************
       1000-LIRE.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 1100-LIRE-POUR-MAJ : Lecture avec verrouillage
      ******************************************************************
       1100-LIRE-POUR-MAJ.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 2000-REECRIRE : Mise a jour apres READ UPDATE
      ******************************************************************
       2000-REECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               REWRITE FILE(DAO-FICHIER)
                       FROM(WS-DATA-BUFFER)
                       RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 3000-ECRIRE : Creation nouvel enregistrement
      ******************************************************************
       3000-ECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               WRITE FILE(DAO-FICHIER)
                     FROM(WS-DATA-BUFFER)
                     RIDFLD(DAO-CLE)
                     RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 4000-SUPPRIMER : Suppression d'un enregistrement
      ******************************************************************
       4000-SUPPRIMER.

           EXEC CICS
               DELETE FILE(DAO-FICHIER)
                      RIDFLD(DAO-CLE)
                      RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 9000-TRAITER-RESP : Conversion code retour CICS
      ******************************************************************
       9000-TRAITER-RESP.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO DAO-RESP
                   MOVE WS-DATA-BUFFER TO DAO-DATA
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO DAO-RESP
               WHEN DFHRESP(DUPREC)
                   MOVE 14 TO DAO-RESP
               WHEN DFHRESP(NOSPACE)
                   MOVE 18 TO DAO-RESP
               WHEN DFHRESP(DISABLED)
                   MOVE 22 TO DAO-RESP
               WHEN OTHER
                   MOVE 99 TO DAO-RESP
           END-EVALUATE.
