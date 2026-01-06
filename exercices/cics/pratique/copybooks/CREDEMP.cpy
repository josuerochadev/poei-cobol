      *================================================================*
      * Copybook : CREDEMP.cpy
      * Description : Structure enregistrement fichier CRE-EMP
      * Fichier : FTEST.CICS.CREDEMP (KSDS, LRECL=80, CLE=6)
      *================================================================*
       01  WS-CREDEMP.
           05  ID-EMPL             PIC X(6).
           05  LIB-CREDIT-EMPL     PIC X(20).
           05  MONTANT-CREDIT      PIC 9(9)V99.
           05  MONTANT-ECHEANCE    PIC 9(7)V99.
           05  RESTE-CREDIT        PIC 9(9)V99.
           05  FILLER              PIC X(23).
