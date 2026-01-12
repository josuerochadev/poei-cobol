      ******************************************************************
      * Copybook : CLIENT
      * Description : Structure enregistrement client bancaire
      * Fichier VSAM : CLIENT (KSDS)
      * Clé primaire : CLI-NUM (6 caractères)
      * Longueur enregistrement : 126 octets
      ******************************************************************
       01  CLIENT-REC.
           05  CLI-NUM             PIC X(6).
           05  CLI-NOM             PIC X(25).
           05  CLI-PRENOM          PIC X(20).
           05  CLI-ADRESSE         PIC X(30).
           05  CLI-VILLE           PIC X(20).
           05  CLI-CODEPOST        PIC X(5).
           05  CLI-TEL             PIC X(10).
           05  CLI-DATEOUV         PIC X(8).
           05  CLI-REGION          PIC X(2).
      ******************************************************************
