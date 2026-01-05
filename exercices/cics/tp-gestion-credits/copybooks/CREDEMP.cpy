      ******************************************************************
      * Copybook : CREDEMP
      * Description : Structure enregistrement crédit employé
      * Fichier VSAM : CRE-EMP (KSDS)
      * Clé primaire : CRD-ID-EMPL (6 caractères)
      * Longueur enregistrement : 41 octets
      ******************************************************************
       01  CREDIT-REC.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(7)V99 COMP-3.
           05  CRD-MONTANT-ECH     PIC 9(5)V99 COMP-3.
           05  CRD-RESTE           PIC 9(7)V99 COMP-3.
      ******************************************************************
      * Détail des champs :
      * -----------------------------------------------------------------
      * CRD-ID-EMPL      : Identifiant employé (clé étrangère vers EMPLOYE)
      * CRD-LIBELLE      : Type de crédit (PRET AUTO, PRET IMMO, etc.)
      * CRD-MONTANT-TOTAL: Montant initial du crédit (COMP-3)
      * CRD-MONTANT-ECH  : Montant de l'échéance mensuelle (COMP-3)
      * CRD-RESTE        : Reste à payer (COMP-3)
      ******************************************************************
      * Relation avec EMPLOYE :
      * -----------------------------------------------------------------
      * Un enregistrement CREDIT-REC existe uniquement si
      * EMP-ETAT-CRED = 'Y' dans l'enregistrement EMPLOYE correspondant.
      * La clé CRD-ID-EMPL doit correspondre à un EMP-ID existant.
      ******************************************************************
