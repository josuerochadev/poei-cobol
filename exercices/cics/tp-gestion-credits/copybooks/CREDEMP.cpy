      *================================================================*
      * Copybook : CREDEMP.cpy
      * Description : Structure enregistrement fichier CRE-EMP
      * Fichier : FTEST.CICS.CREDEMP (KSDS, LRECL=80, CLE=6)
      *================================================================*
       01  CREDIT-REC.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(9)V99.
           05  CRD-MONTANT-ECH     PIC 9(7)V99.
           05  CRD-RESTE           PIC 9(9)V99.
           05  FILLER              PIC X(23).
      *================================================================*
      * Longueur totale : 80 octets
      * Cle : CRD-ID-EMPL (positions 1-6)
      *================================================================*
