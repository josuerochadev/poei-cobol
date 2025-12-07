      ******************************************************************
      * Copybook : CREDEMP.cpy
      * Description : Structure enregistrement fichier CRE-EMP
      * Fichier VSAM : CREDEMP (KSDS)
      * Longueur : 39 octets
      ******************************************************************
       01  CREDIT-REC.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(7)V99 COMP-3.
           05  CRD-MONTANT-ECH     PIC 9(5)V99 COMP-3.
           05  CRD-RESTE           PIC 9(7)V99 COMP-3.
      ******************************************************************
      * Longueur totale : 6 + 20 + 5 + 4 + 5 = 40 octets
      * Note : COMP-3 9(7)V99 = 5 octets
      *        COMP-3 9(5)V99 = 4 octets
      ******************************************************************
