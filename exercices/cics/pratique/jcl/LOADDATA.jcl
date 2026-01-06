//LOADDATA JOB (ACCT),'LOAD VSAM DATA',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : LOADDATA.jcl
//* Description : Chargement des donnees de test pour TP CICS
//*********************************************************************
//*
//*===================================================================*
//* STRUCTURE EMPLOYE (80 octets)
//*===================================================================*
//*   ID-EMPL        : X(6)     positions 1-6   (cle)
//*   NAME-EMPL      : X(30)    positions 7-36
//*   DEPT-EMPL      : X(10)    positions 37-46
//*   SALAIRE-EMPL   : 9(7)V99  positions 47-55
//*   ETAT-CRED-EMPL : X(1)     position 56     (Y=credit, N=pas credit)
//*   FILLER         : X(24)    positions 57-80
//*
//*===================================================================*
//* STRUCTURE CRE-EMP (80 octets)
//*===================================================================*
//*   ID-EMPL           : X(6)     positions 1-6   (cle)
//*   LIB-CREDIT-EMPL   : X(20)    positions 7-26
//*   MONTANT-CREDIT    : 9(9)V99  positions 27-37
//*   MONTANT-ECHEANCE  : 9(7)V99  positions 38-46
//*   RESTE-CREDIT      : 9(9)V99  positions 47-57
//*   FILLER            : X(23)    positions 58-80
//*
//*===================================================================*
//* DONNEES DE TEST :
//*   - 6 employes (EMP001-EMP006)
//*   - 4 credits (EMP001, EMP003, EMP004, EMP006)
//*   - EMP004 a un petit reste (250) pour tester le soldage
//*===================================================================*
//*
//*===================================================================*
//* ETAPE 1 : Chargement EMPLOYE (6 enregistrements)
//*===================================================================*
//LOADEMP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001DUPONT JEAN                    FINANCE   003500000Y
EMP002MARTIN MARIE                   RH        004200000N
EMP003DURAND PIERRE                  IT        005500000Y
EMP004BERNARD SOPHIE                 FINANCE   003800000Y
EMP005PETIT ALAIN                    IT        006200000N
EMP006MOREAU CLAIRE                  RH        004800000Y
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) -
        OUTDATASET(FTEST.CICS.EMPLOYE)
/*
//*
//*===================================================================*
//* ETAPE 2 : Chargement CRE-EMP (4 enregistrements)
//*===================================================================*
//LOADCRE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001CREDIT IMMOBILIER   0015000000000150000001200000
EMP003CREDIT AUTO         0002500000000025000000150000
EMP004CREDIT PERSO        0000050000000025000000025000
EMP006CREDIT TRAVAUX      0001200000000015000000090000
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) -
        OUTDATASET(FTEST.CICS.CREDEMP)
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
