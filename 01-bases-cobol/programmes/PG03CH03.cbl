       IDENTIFICATION DIVISION.
       PROGRAM-ID. PG03CH03.
       AUTHOR.     N GAIGI.
      *----------------------------------------------------------------
      * PROGRAMME D'AFFICHAGE: - VARIABLES CONDITIONS OU BOOLEENNES 88
      *                        - VARIABLE DE REARRANGEMENT 66
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-INDIVIDU.
           05   WS-ADRESSE.
                10  WS-RUE        PIC X(20).
                10  WS-CODEPOSTAL PIC X(5) VALUE '84200'.
           05   WS-IDENTITE.
               10 WS-NOM        PIC X(15) VALUE 'MAE'.
               10 WS-PRENOM     PIC X(15) VALUE 'CHRISTOPHE'.
       66  WS-LISTECP RENAMES WS-CODEPOSTAL THRU WS-PRENOM.
       01  ETATMATRIMONIAL PIC X.
           88 CELIBATAIRE VALUE 'C'.
           88 MARIE       VALUE 'M'.
           88 DIVORCE     VALUE 'D'.
           88 PACS        VALUE 'P'.
       PROCEDURE DIVISION.
           PERFORM INIT.
           PERFORM TRAIT.
           PERFORM FIN.
       INIT.
           SET MARIE TO TRUE.
       TRAIT.
           IF CELIBATAIRE THEN DISPLAY 'ETAT MATRIMONIAL: CELIBATAIRE'.
           IF MARIE       THEN DISPLAY 'ETAT MATRIMONIAL: MARIE(E)'.
           IF DIVORCE     THEN DISPLAY 'ETAT MATRIMONIAL: DIVORCE(E)'.
           IF PACS        THEN DISPLAY 'ETAT MATRIMONIAL: PACSE(E)'.
           DISPLAY 'LISTECP :' WS-LISTECP.
       FIN.
           STOP RUN.
