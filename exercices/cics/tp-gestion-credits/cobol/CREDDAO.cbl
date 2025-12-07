      ******************************************************************
      * Programme : CREDDAO
      * Couche    : Données (Data Access Object)
      * Fonction  : Accès aux fichiers VSAM EMPLOYE et CREDEMP
      *
      * Description :
      * - Lecture simple (READ)
      * - Lecture pour mise à jour (READ UPDATE)
      * - Mise à jour (REWRITE)
      * - Gestion centralisée des erreurs VSAM
      *
      * Fichiers gérés :
      * - EMPLOYE : Informations employés
      * - CREDEMP : Informations crédits
      *
      * Appel : Via LINK depuis CREDTRT
      * Auteur    : Formation CICS
      * Date      : 2024
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDDAO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *─── Variables de travail ───────────────────────────────────────
       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-RESP2                PIC S9(8) COMP.
       01  WS-DATA-BUFFER          PIC X(100).
       01  WS-FICHIER-EN-COURS     PIC X(8).

      *─── COMMAREA d'échange ─────────────────────────────────────────
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
      * 0000-PRINCIPAL : Dispatcher des opérations
      ******************************************************************
       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 0 TO DAO-RESP
           MOVE DAO-FICHIER TO WS-FICHIER-EN-COURS

      *─── Dispatch selon l'action demandée ───────────────────────────
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
               WHEN OTHER
                   MOVE 99 TO DAO-RESP
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA

           EXEC CICS
               RETURN
           END-EXEC.

      ******************************************************************
      * 1000-LIRE : Lecture simple
      ******************************************************************
       1000-LIRE.

           EXEC CICS
               READ FILE(WS-FICHIER-EN-COURS)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 1100-LIRE-POUR-MAJ : Lecture avec verrouillage (UPDATE)
      ******************************************************************
       1100-LIRE-POUR-MAJ.

           EXEC CICS
               READ FILE(WS-FICHIER-EN-COURS)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    UPDATE
                    RESP(WS-RESP)
                    RESP2(WS-RESP2)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 2000-REECRIRE : Mise à jour après READ UPDATE
      ******************************************************************
       2000-REECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               REWRITE FILE(WS-FICHIER-EN-COURS)
                       FROM(WS-DATA-BUFFER)
                       RESP(WS-RESP)
                       RESP2(WS-RESP2)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 3000-ECRIRE : Création d'un nouvel enregistrement
      ******************************************************************
       3000-ECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               WRITE FILE(WS-FICHIER-EN-COURS)
                     FROM(WS-DATA-BUFFER)
                     RIDFLD(DAO-CLE)
                     RESP(WS-RESP)
                     RESP2(WS-RESP2)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 4000-SUPPRIMER : Suppression d'un enregistrement
      ******************************************************************
       4000-SUPPRIMER.

           EXEC CICS
               DELETE FILE(WS-FICHIER-EN-COURS)
                      RIDFLD(DAO-CLE)
                      RESP(WS-RESP)
                      RESP2(WS-RESP2)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 9000-TRAITER-RESP : Gestion centralisée des codes retour
      ******************************************************************
       9000-TRAITER-RESP.

           EVALUATE WS-RESP
      *─── Succès ─────────────────────────────────────────────────────
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO DAO-RESP
                   MOVE WS-DATA-BUFFER TO DAO-DATA

      *─── Enregistrement non trouvé ──────────────────────────────────
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO DAO-RESP

      *─── Clé en double ──────────────────────────────────────────────
               WHEN DFHRESP(DUPREC)
                   MOVE 14 TO DAO-RESP

      *─── Requête invalide (ex: REWRITE sans READ UPDATE) ────────────
               WHEN DFHRESP(INVREQ)
                   MOVE 16 TO DAO-RESP

      *─── Plus d'espace disponible ───────────────────────────────────
               WHEN DFHRESP(NOSPACE)
                   MOVE 18 TO DAO-RESP

      *─── Fichier non ouvert ─────────────────────────────────────────
               WHEN DFHRESP(NOTOPEN)
                   MOVE 19 TO DAO-RESP

      *─── Fichier désactivé ──────────────────────────────────────────
               WHEN DFHRESP(DISABLED)
                   MOVE 22 TO DAO-RESP

      *─── Enregistrement verrouillé par une autre tâche ──────────────
               WHEN DFHRESP(RECORDBUSY)
                   MOVE 31 TO DAO-RESP

      *─── Erreur logique VSAM ────────────────────────────────────────
               WHEN DFHRESP(ILLOGIC)
                   MOVE 32 TO DAO-RESP

      *─── Autres erreurs ─────────────────────────────────────────────
               WHEN OTHER
                   MOVE 99 TO DAO-RESP

           END-EVALUATE.
