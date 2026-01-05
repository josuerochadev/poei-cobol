      ******************************************************************
      * Copybook : COMPTE
      * Description : Structure enregistrement compte bancaire
      * Fichier VSAM : COMPTE (KSDS)
      * Clé primaire : CPT-NUM (11 caractères)
      * Longueur enregistrement : 56 octets
      ******************************************************************
       01  COMPTE-REC.
           05  CPT-NUM             PIC X(11).
           05  CPT-CLIENT          PIC X(6).
           05  CPT-TYPE            PIC X(1).
               88  CPT-COURANT     VALUE 'C'.
               88  CPT-EPARGNE     VALUE 'E'.
               88  CPT-TITRE       VALUE 'T'.
           05  CPT-LIBELLE         PIC X(20).
           05  CPT-SOLDE           PIC S9(9)V99 COMP-3.
           05  CPT-DATEOUV         PIC X(8).
           05  CPT-DATEDER         PIC X(8).
      ******************************************************************
