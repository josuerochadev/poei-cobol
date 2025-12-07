      ******************************************************************
      * Copybook : EMPLOYE.cpy
      * Description : Structure enregistrement fichier EMPLOYE
      * Fichier VSAM : EMPLOYE (KSDS)
      * Longueur : 52 octets
      ******************************************************************
       01  EMPLOYE-REC.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99 COMP-3.
           05  EMP-ETAT-CRED       PIC X(1).
               88  EMP-A-CREDIT    VALUE 'Y'.
               88  EMP-SANS-CREDIT VALUE 'N'.
      ******************************************************************
      * Longueur totale : 6 + 30 + 10 + 5 + 1 = 52 octets
      * Note : COMP-3 9(7)V99 = 5 octets (9 chiffres / 2 + 1)
      ******************************************************************
