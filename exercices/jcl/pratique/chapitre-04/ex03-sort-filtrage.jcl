//FTESTEX3 JOB (ACCT),'EX03 SORT FILTRAGE',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* EXERCICE 3 : SORT - TRI ET FILTRAGE
//* ============================================================
//* Objectif : Maitriser les operations de tri, filtrage et
//*            reformatage avec SORT
//*
//* Prerequis : Executer ex01-iefbr14-iebgener.jcl d'abord
//*             pour creer FTEST.UTIL.DATA
//*
//* Format fichier entree (LRECL=50) :
//* - Pos 01-03 : Code client
//* - Pos 04-13 : Nom
//* - Pos 14-23 : Prenom
//* - Pos 24-33 : Ville
//* - Pos 34-38 : Code postal
//* - Pos 39-50 : Filler
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* STEP 0 : SUPPRESSION DES FICHIERS SORTIE
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.UTIL.TRIENOM
  DELETE FTEST.UTIL.TRIECP
  DELETE FTEST.UTIL.PARIS
  DELETE FTEST.UTIL.EXTRAIT
  SET MAXCC = 0
/*
//*
//* ----------------------------------------------------------
//* STEP 1 : TRI SIMPLE PAR NOM (POSITIONS 4-13)
//* ----------------------------------------------------------
//TRISIMPL EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//SORTOUT  DD DSN=FTEST.UTIL.TRIENOM,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* TRI PAR NOM EN ORDRE CROISSANT
  SORT FIELDS=(4,10,CH,A)
/*
//*
//* ----------------------------------------------------------
//* STEP 2 : TRI SUR DEUX CLES (CP DESC, NOM ASC)
//* ----------------------------------------------------------
//TRICP    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//SORTOUT  DD DSN=FTEST.UTIL.TRIECP,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* TRI PAR CODE POSTAL DESCENDANT PUIS NOM ASCENDANT
  SORT FIELDS=(34,5,CH,D,4,10,CH,A)
/*
//*
//* ----------------------------------------------------------
//* STEP 3 : FILTRAGE CLIENTS PARIS (CP COMMENCE PAR 75)
//* ----------------------------------------------------------
//FILTPARS EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//SORTOUT  DD DSN=FTEST.UTIL.PARIS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* FILTRER UNIQUEMENT LES CLIENTS PARISIENS (75xxx)
  INCLUDE COND=(34,2,CH,EQ,C'75')
  SORT FIELDS=(4,10,CH,A)
/*
//*
//* ----------------------------------------------------------
//* STEP 4 : EXTRACTION ET REFORMATAGE
//* ----------------------------------------------------------
//EXTRACT  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.UTIL.DATA,DISP=SHR
//SORTOUT  DD DSN=FTEST.UTIL.EXTRAIT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=30,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
* EXTRAIRE CODE(3) + NOM(10) + VILLE(10) + FILLER(7)
* TOTAL = 30 CARACTERES
  SORT FIELDS=COPY
  OUTREC FIELDS=(1,3,4,10,24,10,7X)
/*
//*
//* ----------------------------------------------------------
//* STEP 5 : AFFICHER LES RESULTATS
//* ----------------------------------------------------------
//PRINTNOM EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.UTIL.TRIENOM,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=51)
//*
//PRINTCP  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.UTIL.TRIECP,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=51)
//*
//PRINTPAR EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.UTIL.PARIS,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=51)
//*
//PRINTEXT EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.UTIL.EXTRAIT,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=31)
//
