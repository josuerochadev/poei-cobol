       IDENTIFICATION DIVISION.
       PROGRAM-ID. C08RELINIT.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : Creation fichier BUFFER pour Edition Releve
      *
      * Genere un fichier BUFFER contenant des enregistrements
      * de type A (Agence), C (Client), R (RIB), M (Mouvement)
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FBUFFER ASSIGN TO 'BUFFER.DAT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.
       FD  FBUFFER
           RECORD CONTAINS 80 CHARACTERS.
       01  ENR-BUFFER.
           10  ID-TAB             PIC X.
           10  LIGNE-DATA         PIC X(79).

       WORKING-STORAGE SECTION.
       01  WS-FS                  PIC XX.
       01  WS-CPT                 PIC 9(3) VALUE 0.

      *----------------------------------------------------------------
      * Enregistrement AGENCE (A) - 80 car
      * Format: A + CODE(7) + LIBELLE(30) + FILLER(42)
      *----------------------------------------------------------------
       01  WS-AGENCE.
           05  WS-AG-TYPE         PIC X VALUE 'A'.
           05  WS-AG-CODE         PIC 9(7).
           05  WS-AG-LIBELLE      PIC X(30).
           05  FILLER             PIC X(42) VALUE SPACES.

      *----------------------------------------------------------------
      * Enregistrement CLIENT (C) - 80 car
      * Format: C + CODE(5) + NOM(20) + PRENOM(20) + CODEAG(7) + FILL(27)
      *----------------------------------------------------------------
       01  WS-CLIENT.
           05  WS-CL-TYPE         PIC X VALUE 'C'.
           05  WS-CL-CODE         PIC 9(5).
           05  WS-CL-NOM          PIC X(20).
           05  WS-CL-PRENOM       PIC X(20).
           05  WS-CL-CODEAG       PIC 9(7).
           05  FILLER             PIC X(27) VALUE SPACES.

      *----------------------------------------------------------------
      * Enregistrement RIB (R) - 80 car
      * Format: R + CLT(5) + CPTE(23) + DATE(8) + SOLDE(11) + SENS(1)
      *         + FILLER(31)
      *----------------------------------------------------------------
       01  WS-RIB.
           05  WS-RIB-TYPE        PIC X VALUE 'R'.
           05  WS-RIB-CLT         PIC 9(5).
           05  WS-RIB-CPTE        PIC X(23).
           05  WS-RIB-DATE        PIC 9(8).
           05  WS-RIB-SOLDE       PIC 9(9)V9(2).
           05  WS-RIB-SENS        PIC X.
           05  FILLER             PIC X(31) VALUE SPACES.

      *----------------------------------------------------------------
      * Enregistrement MOUVEMENT (M) - 80 car
      * Format: M + CLT(5) + DATE(8) + MONTANT(11) + SENS(1) + FILL(54)
      *----------------------------------------------------------------
       01  WS-MVTC.
           05  WS-MV-TYPE         PIC X VALUE 'M'.
           05  WS-MV-CLT          PIC 9(5).
           05  WS-MV-DATE         PIC 9(8).
           05  WS-MV-MONT         PIC 9(9)V9(2).
           05  WS-MV-SENS         PIC X.
           05  FILLER             PIC X(54) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  Creation fichier BUFFER - Donnees de test'
           DISPLAY '=================================================='
           DISPLAY ' '

           OPEN OUTPUT FBUFFER
           IF WS-FS NOT = '00'
               DISPLAY 'Erreur ouverture : ' WS-FS
               STOP RUN
           END-IF

           DISPLAY 'Fichier BUFFER.DAT cree.'
           DISPLAY ' '

      *----------------------------------------------------------------
      * AGENCES (code avec cle de controle modulo 7)
      * Cle = 7 - (6 premiers chiffres MOD 7)
      *----------------------------------------------------------------
      * Agence 1 : 000001 MOD 7 = 1, cle = 7-1 = 6 -> 0000016
           MOVE '0000016' TO WS-AG-CODE
           MOVE 'AGENCE ASTRE' TO WS-AG-LIBELLE
           MOVE WS-AGENCE TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'AGENCE: ' WS-AG-CODE ' ' WS-AG-LIBELLE

      * Agence 2 : 000021 MOD 7 = 0, cle = 7-0 = 7 -> 0000217
           MOVE '0000217' TO WS-AG-CODE
           MOVE 'AGENCE NOUVELLE GENERATION' TO WS-AG-LIBELLE
           MOVE WS-AGENCE TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'AGENCE: ' WS-AG-CODE ' ' WS-AG-LIBELLE

      * Agence avec cle erronee (pour test)
           MOVE '0007222' TO WS-AG-CODE
           MOVE 'AGENCE ERREUR CLE' TO WS-AG-LIBELLE
           MOVE WS-AGENCE TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'AGENCE: ' WS-AG-CODE ' (CLE ERRONEE - TEST)'

      *----------------------------------------------------------------
      * CLIENTS
      *----------------------------------------------------------------
           MOVE 00001 TO WS-CL-CODE
           MOVE 'MAVINGA KINAVIDI' TO WS-CL-NOM
           MOVE 'HERITIER' TO WS-CL-PRENOM
           MOVE 0000217 TO WS-CL-CODEAG
           MOVE WS-CLIENT TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'CLIENT: ' WS-CL-CODE ' ' WS-CL-NOM

           MOVE 00321 TO WS-CL-CODE
           MOVE 'SERVETTAZ' TO WS-CL-NOM
           MOVE 'MORGAN' TO WS-CL-PRENOM
           MOVE 0000217 TO WS-CL-CODEAG
           MOVE WS-CLIENT TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'CLIENT: ' WS-CL-CODE ' ' WS-CL-NOM

           MOVE 01210 TO WS-CL-CODE
           MOVE 'VAZ' TO WS-CL-NOM
           MOVE 'PIERRE' TO WS-CL-PRENOM
           MOVE 0000016 TO WS-CL-CODEAG
           MOVE WS-CLIENT TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'CLIENT: ' WS-CL-CODE ' ' WS-CL-NOM

           MOVE 72521 TO WS-CL-CODE
           MOVE 'POKAM' TO WS-CL-NOM
           MOVE 'JOEL' TO WS-CL-PRENOM
           MOVE 0000016 TO WS-CL-CODEAG
           MOVE WS-CLIENT TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'CLIENT: ' WS-CL-CODE ' ' WS-CL-NOM

      *----------------------------------------------------------------
      * RIB
      *----------------------------------------------------------------
           MOVE 00321 TO WS-RIB-CLT
           MOVE '12345678901234567890123' TO WS-RIB-CPTE
           MOVE 20250430 TO WS-RIB-DATE
           MOVE 54235.70 TO WS-RIB-SOLDE
           MOVE 'C' TO WS-RIB-SENS
           MOVE WS-RIB TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'RIB: ' WS-RIB-CLT ' SOLDE=' WS-RIB-SOLDE

           MOVE 01210 TO WS-RIB-CLT
           MOVE '13456789012345678901234' TO WS-RIB-CPTE
           MOVE 20250430 TO WS-RIB-DATE
           MOVE 122309.00 TO WS-RIB-SOLDE
           MOVE 'C' TO WS-RIB-SENS
           MOVE WS-RIB TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'RIB: ' WS-RIB-CLT ' SOLDE=' WS-RIB-SOLDE

      *----------------------------------------------------------------
      * MOUVEMENTS
      *----------------------------------------------------------------
           MOVE 01210 TO WS-MV-CLT
           MOVE 20250501 TO WS-MV-DATE
           MOVE 5623.00 TO WS-MV-MONT
           MOVE 'D' TO WS-MV-SENS
           MOVE WS-MVTC TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'MVTC: ' WS-MV-CLT ' ' WS-MV-DATE ' ' WS-MV-MONT

           MOVE 01210 TO WS-MV-CLT
           MOVE 20250502 TO WS-MV-DATE
           MOVE 7503.10 TO WS-MV-MONT
           MOVE 'D' TO WS-MV-SENS
           MOVE WS-MVTC TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'MVTC: ' WS-MV-CLT ' ' WS-MV-DATE ' ' WS-MV-MONT

           MOVE 01210 TO WS-MV-CLT
           MOVE 20250503 TO WS-MV-DATE
           MOVE 3215.00 TO WS-MV-MONT
           MOVE 'D' TO WS-MV-SENS
           MOVE WS-MVTC TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'MVTC: ' WS-MV-CLT ' ' WS-MV-DATE ' ' WS-MV-MONT

           MOVE 01210 TO WS-MV-CLT
           MOVE 20250504 TO WS-MV-DATE
           MOVE 8573.00 TO WS-MV-MONT
           MOVE 'D' TO WS-MV-SENS
           MOVE WS-MVTC TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'MVTC: ' WS-MV-CLT ' ' WS-MV-DATE ' ' WS-MV-MONT

           MOVE 01210 TO WS-MV-CLT
           MOVE 20250510 TO WS-MV-DATE
           MOVE 5623.00 TO WS-MV-MONT
           MOVE 'D' TO WS-MV-SENS
           MOVE WS-MVTC TO ENR-BUFFER
           WRITE ENR-BUFFER
           ADD 1 TO WS-CPT
           DISPLAY 'MVTC: ' WS-MV-CLT ' ' WS-MV-DATE ' ' WS-MV-MONT

           CLOSE FBUFFER

           DISPLAY ' '
           DISPLAY 'Total enregistrements ecrits : ' WS-CPT
           DISPLAY '=================================================='
           STOP RUN.
