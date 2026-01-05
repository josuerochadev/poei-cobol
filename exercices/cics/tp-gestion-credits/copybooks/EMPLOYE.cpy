      ******************************************************************
      * Copybook : EMPLOYE
      * Description : Structure enregistrement employé
      * Fichier VSAM : EMPLOYE (KSDS)
      * Clé primaire : EMP-ID (6 caractères)
      * Longueur enregistrement : 52 octets
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
      * Détail des champs :
      * -----------------------------------------------------------------
      * EMP-ID         : Identifiant unique employé (EMP001-EMP999)
      * EMP-NAME       : Nom complet de l'employé
      * EMP-DEPT       : Département (COMPTA, INFO, RH, etc.)
      * EMP-SALAIRE    : Salaire mensuel en format packed (COMP-3)
      * EMP-ETAT-CRED  : Indicateur crédit actif
      *                  'Y' = Crédit en cours
      *                  'N' = Pas de crédit
      ******************************************************************
