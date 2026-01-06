      *================================================================*
      * Copybook : EMPLOYE.cpy                                         *
      * Description : Structure enregistrement EMPLOYE                 *
      * Fichier : EMPLOYE (KSDS, LRECL=80, CLE=6)                      *
      *================================================================*
       01  EMPLOYE-REC.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99.
           05  EMP-ETAT-CRED       PIC X(1).
               88  EMP-CREDIT-OUI  VALUE 'O'.
               88  EMP-CREDIT-NON  VALUE 'N'.
           05  EMP-FILLER          PIC X(24).
      *
      * Longueur totale : 80 octets
      * Cle primaire : EMP-ID (positions 1-6)
      * Index alternatif : EMP-NAME (positions 7-36)
      *
