      *================================================================*
      * Copybook : EMPLOYE.cpy
      * Description : Structure enregistrement fichier EMPLOYE
      * Fichier : FTEST.CICS.EMPLOYE (KSDS, LRECL=80, CLE=6)
      *================================================================*
       01  WS-EMPLOYE.
           05  ID-EMPL             PIC X(6).
           05  NAME-EMPL           PIC X(30).
           05  DEPT-EMPL           PIC X(10).
           05  SALAIRE-EMPL        PIC 9(7)V99.
           05  ETAT-CRED-EMPL      PIC X(1).
               88 EMPLOYE-A-CREDIT     VALUE 'Y'.
               88 EMPLOYE-SANS-CREDIT  VALUE 'N'.
           05  FILLER              PIC X(24).
