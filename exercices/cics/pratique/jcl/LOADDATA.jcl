//LOADDATA JOB (ACCT),'LOAD VSAM DATA',CLASS=A,MSGCLASS=X
//*********************************************************************
//* JCL : LOADDATA.jcl
//* Description : Chargement des donnees de test dans EMPLOYE
//* Fichier : FTEST.CICS.EMPLOYE (KSDS, LRECL=80)
//*********************************************************************
//*
//* Structure enregistrement (80 octets) :
//*   EMP-ID        : X(6)     positions 1-6   (cle)
//*   EMP-NAME      : X(30)    positions 7-36
//*   EMP-DEPT      : X(10)    positions 37-46
//*   EMP-SALAIRE   : 9(7)V99  positions 47-55 (ex: 004500000 = 45000.00)
//*   EMP-ETAT-CRED : X(1)     position 56     (Y ou N)
//*   FILLER        : X(24)    positions 57-80
//*
//*********************************************************************
//*
//* ETAPE 1 : Chargement via REPRO avec donnees en ligne
//*
//LOAD     EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INFILE   DD *
EMP001DUPONT JEAN                    FINANCE   003500000Y
EMP002MARTIN MARIE                   RH        004200000N
EMP003DURAND PIERRE                  IT        005500000Y
EMP004BERNARD SOPHIE                 FINANCE   003800000N
EMP005PETIT ALAIN                    IT        006200000Y
/*
//SYSIN    DD *
  REPRO INFILE(INFILE) -
        OUTDATASET(FTEST.CICS.EMPLOYE)
/*
//*********************************************************************
//* FIN DU JOB
//*********************************************************************
