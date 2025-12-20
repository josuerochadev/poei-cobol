       IDENTIFICATION DIVISION.
       PROGRAM-ID. C10PERSUP.
       AUTHOR. FORMATION COBOL.
      ******************************************************************
      * PROGRAMME : C10-PERS-UPDATE
      * OBJET     : Modification d'un enregistrement
      *             - Recherche par N° SS (cle secondaire)
      *             - Modification du Nom et Prenom
      *             - REWRITE de l'enregistrement
      * EXERCICE  : TP Chapitre X - Exercice 4
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-PERSONNEL
               ASSIGN TO 'PERSONNEL.KSDS'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ENR-MATRICULE
               ALTERNATE RECORD KEY IS ENR-NUM-SS WITH DUPLICATES
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD F-PERSONNEL
           RECORDING MODE IS F
           RECORD CONTAINS 66 CHARACTERS.
       01 ENR-PERSONNEL.
          05 ENR-MATRICULE         PIC 9(6).
          05 ENR-NOM               PIC X(15).
          05 ENR-PRENOM            PIC X(15).
          05 ENR-SALAIRE           PIC 9(6).
          05 ENR-PRIMES            PIC 9(6).
          05 ENR-REVENU-ANNUEL     PIC 9(8).
          05 ENR-NUM-SS            PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX.
          88 WS-OK                 VALUE '00'.
          88 WS-NOT-FOUND          VALUE '23'.

       01 WS-NUM-SS-RECHERCHE      PIC 9(10).
       01 WS-NOUVEAU-NOM           PIC X(15).
       01 WS-NOUVEAU-PRENOM        PIC X(15).
       01 WS-CONFIRMER             PIC X VALUE SPACE.

      *----------------------------------------------------------------*
      * Sauvegarde des anciennes valeurs
      *----------------------------------------------------------------*
       01 WS-ANCIEN-NOM            PIC X(15).
       01 WS-ANCIEN-PRENOM         PIC X(15).

       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
           DISPLAY '=================================================='
           DISPLAY '  EXERCICE 4 : MODIFICATION ENREGISTREMENT        '
           DISPLAY '  Mise a jour du Nom et Prenom                    '
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN I-O F-PERSONNEL
           IF NOT WS-OK
               DISPLAY 'Erreur ouverture I-O : ' WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM 1000-RECHERCHER-SALARIE
           PERFORM 9000-FERMER

           STOP RUN.

      *----------------------------------------------------------------*
      * Recherche du salarie par N° SS
      *----------------------------------------------------------------*
       1000-RECHERCHER-SALARIE.
           DISPLAY 'N° SS du salarie a modifier (10 chiffres) : '
           ACCEPT WS-NUM-SS-RECHERCHE

           MOVE WS-NUM-SS-RECHERCHE TO ENR-NUM-SS

           READ F-PERSONNEL KEY IS ENR-NUM-SS
               INVALID KEY
                   DISPLAY '*** N° SS non trouve ***'
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY '--- Salarie actuel ---'
                   DISPLAY 'Matricule : ' ENR-MATRICULE
                   DISPLAY 'Nom       : ' ENR-NOM
                   DISPLAY 'Prenom    : ' ENR-PRENOM
                   DISPLAY 'N° SS     : ' ENR-NUM-SS
                   DISPLAY ' '
                   PERFORM 2000-MODIFIER-SALARIE
           END-READ.

      *----------------------------------------------------------------*
      * Modification du Nom et Prenom
      *----------------------------------------------------------------*
       2000-MODIFIER-SALARIE.
      *    Sauvegarder les anciennes valeurs
           MOVE ENR-NOM TO WS-ANCIEN-NOM
           MOVE ENR-PRENOM TO WS-ANCIEN-PRENOM

           DISPLAY '--- Entrez les nouvelles valeurs ---'
           DISPLAY 'Nouveau NOM (15 car. max, vide=pas de modif) : '
           ACCEPT WS-NOUVEAU-NOM

           IF WS-NOUVEAU-NOM NOT = SPACES
               MOVE WS-NOUVEAU-NOM TO ENR-NOM
           END-IF

           DISPLAY 'Nouveau PRENOM (15 car. max, vide=pas de modif) : '
           ACCEPT WS-NOUVEAU-PRENOM

           IF WS-NOUVEAU-PRENOM NOT = SPACES
               MOVE WS-NOUVEAU-PRENOM TO ENR-PRENOM
           END-IF

      *    Afficher le resume des modifications
           DISPLAY ' '
           DISPLAY '--- Resume des modifications ---'
           DISPLAY 'NOM    : ' WS-ANCIEN-NOM ' -> ' ENR-NOM
           DISPLAY 'PRENOM : ' WS-ANCIEN-PRENOM ' -> ' ENR-PRENOM
           DISPLAY ' '
           DISPLAY 'Confirmer la modification ? (O/N) : '
           ACCEPT WS-CONFIRMER

           IF WS-CONFIRMER = 'O' OR WS-CONFIRMER = 'o'
               PERFORM 3000-ECRIRE-MODIFICATION
           ELSE
               DISPLAY 'Modification annulee'
           END-IF.

      *----------------------------------------------------------------*
      * Ecriture de la modification (REWRITE)
      *----------------------------------------------------------------*
       3000-ECRIRE-MODIFICATION.
           REWRITE ENR-PERSONNEL
               INVALID KEY
                   DISPLAY '*** Erreur REWRITE : ' WS-FILE-STATUS
               NOT INVALID KEY
                   DISPLAY ' '
                   DISPLAY '*** Modification enregistree avec succes ***'
                   DISPLAY 'Matricule : ' ENR-MATRICULE
                   DISPLAY 'Nouveau NOM    : ' ENR-NOM
                   DISPLAY 'Nouveau PRENOM : ' ENR-PRENOM
           END-REWRITE.

      *----------------------------------------------------------------*
       9000-FERMER.
           CLOSE F-PERSONNEL
           DISPLAY ' '
           DISPLAY 'Fin du programme'.

