      ******************************************************************
      * Copybook : MAPTEST.cpy
      * Description : Zone symbolique generee par BMS pour MAPTEST
      * Mapset     : TESTSET
      * Map        : MAPTEST
      *
      * Note : Ce copybook simule ce que genere l'assemblage BMS
      *        avec TYPE=DSECT. En environnement reel, ce fichier
      *        est produit automatiquement.
      ******************************************************************

      *-----------------------------------------------------------------
      * Structure d'entree (suffixe I) - Donnees recues de l'ecran
      *-----------------------------------------------------------------
       01  MAPTESTI.
           05  FILLER               PIC X(12).
      *--- Code employe (saisie)
           05  CODEEMPL             PIC S9(4) COMP.
           05  CODEEMPF             PIC X(1).
           05  FILLER REDEFINES CODEEMPF.
               10  CODEEMPA         PIC X(1).
           05  CODEEMPI             PIC X(6).
      *--- Nom employe (affichage)
           05  NOMEMPL              PIC S9(4) COMP.
           05  NOMEMPF              PIC X(1).
           05  FILLER REDEFINES NOMEMPF.
               10  NOMEMPA          PIC X(1).
           05  NOMEMPI              PIC X(30).
      *--- Departement (affichage)
           05  DEPTEMPL             PIC S9(4) COMP.
           05  DEPTEMPF             PIC X(1).
           05  FILLER REDEFINES DEPTEMPF.
               10  DEPTEMPA         PIC X(1).
           05  DEPTEMPI             PIC X(10).
      *--- Salaire (affichage)
           05  SALEMPL              PIC S9(4) COMP.
           05  SALEMPF              PIC X(1).
           05  FILLER REDEFINES SALEMPF.
               10  SALEMPA          PIC X(1).
           05  SALEMPI              PIC X(12).
      *--- Statut credit (affichage)
           05  STATEMPL             PIC S9(4) COMP.
           05  STATEMPF             PIC X(1).
           05  FILLER REDEFINES STATEMPF.
               10  STATEMPA         PIC X(1).
           05  STATEMPI             PIC X(15).
      *--- Message (affichage)
           05  MSGL                 PIC S9(4) COMP.
           05  MSGF                 PIC X(1).
           05  FILLER REDEFINES MSGF.
               10  MSGA             PIC X(1).
           05  MSGI                 PIC X(70).

      *-----------------------------------------------------------------
      * Structure de sortie (suffixe O) - Donnees envoyees a l'ecran
      *-----------------------------------------------------------------
       01  MAPTESTO REDEFINES MAPTESTI.
           05  FILLER               PIC X(12).
      *--- Code employe
           05  FILLER               PIC X(3).
           05  CODEEMPO             PIC X(6).
      *--- Nom employe
           05  FILLER               PIC X(3).
           05  NOMEMPO              PIC X(30).
      *--- Departement
           05  FILLER               PIC X(3).
           05  DEPTEMPO             PIC X(10).
      *--- Salaire
           05  FILLER               PIC X(3).
           05  SALEMPO              PIC X(12).
      *--- Statut credit
           05  FILLER               PIC X(3).
           05  STATEMPO             PIC X(15).
      *--- Message
           05  FILLER               PIC X(3).
           05  MSGO                 PIC X(70).
