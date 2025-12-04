       IDENTIFICATION DIVISION.
       PROGRAM-ID. C09CALREV.
       AUTHOR. ROCHA.
      ******************************************************************
      * SOUS-PROGRAMME : C09-CALREV
      * OBJET     : Calcul du revenu annuel
      *             Revenu = (12 x Salaire) + Primes
      * EXERCICE  : TP Chapitre IX - Programmes et Sous-programmes
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *----------------------------------------------------------------*
      * LINKAGE SECTION : Zone de réception des paramètres
      * Ces zones sont passées par le programme appelant (PERSREV)
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01 LK-SALAIRE              PIC 9(6).
       01 LK-PRIMES               PIC 9(6).
       01 LK-REVENU               PIC 9(8).

      ******************************************************************
       PROCEDURE DIVISION USING LK-SALAIRE
                                LK-PRIMES
                                LK-REVENU.
      ******************************************************************
      *----------------------------------------------------------------*
      * Calcul du revenu annuel
      * Formule : Revenu = (12 x Salaire mensuel) + Primes annuelles
      *----------------------------------------------------------------*
       0000-CALCUL-REVENU.
           COMPUTE LK-REVENU = (12 * LK-SALAIRE) + LK-PRIMES

      *    Retour au programme appelant
           GOBACK.

