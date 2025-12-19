//FTESTTP5 JOB (ACCT),'TP05 SORT',
//             CLASS=A,
//             MSGCLASS=X,
//             MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID
//*
//* ============================================================
//* TP 5 : MANIPULATION DE L'INSTRUCTION SORT
//* ============================================================
//* Objectif : Maitriser les operations de tri et filtrage
//*
//* Format du fichier d'entree (LRECL=60) :
//* - Pos 01-03 : Code client
//* - Pos 04-13 : Nom
//* - Pos 14-23 : Prenom
//* - Pos 24-28 : Code postal
//* - Pos 29-38 : Ville
//* - Pos 39-45 : Montant (PD 7 chiffres)
//* - Pos 46-60 : Filler
//*
//* Adaptation Hercules/TK4- :
//* - Remplacer FTEST par votre userid (ex: HERC01)
//* - Ajouter VOL=SER=PUB001,UNIT=3390
//* ============================================================
//*
//* ----------------------------------------------------------
//* ETAPE 0 : NETTOYAGE ET CREATION FICHIER SOURCE
//* ----------------------------------------------------------
//CLEANUP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE FTEST.TP05.INPUT
  DELETE FTEST.TP05.TRIENOM
  DELETE FTEST.TP05.TRIMULTI
  DELETE FTEST.TP05.PARIS
  DELETE FTEST.TP05.PROVINCE
  DELETE FTEST.TP05.EXTRAIT
  SET MAXCC = 0
/*
//*
//CREATE   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD *
001DUPONT    JEAN      75001PARIS     0100000XXXXXXXXXXXXXXX
002MARTIN    MARIE     69001LYON      0075000XXXXXXXXXXXXXXX
003DURAND    PIERRE    13001MARSEILLE 0150000XXXXXXXXXXXXXXX
004PETIT     SOPHIE    33001BORDEAUX  0050000XXXXXXXXXXXXXXX
005BERNARD   PAUL      75008PARIS     0200000XXXXXXXXXXXXXXX
006THOMAS    CLAIRE    69002LYON      0080000XXXXXXXXXXXXXXX
007ROBERT    LUC       44000NANTES    0025000XXXXXXXXXXXXXXX
008RICHARD   ANNE      75016PARIS     0175000XXXXXXXXXXXXXXX
009MOREAU    JACQUES   31000TOULOUSE  0090000XXXXXXXXXXXXXXX
010SIMON     MARIE     75005PARIS     0125000XXXXXXXXXXXXXXX
/*
//SYSUT2   DD DSN=FTEST.TP05.INPUT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=60,BLKSIZE=0),
//            UNIT=SYSDA
//*
//* ==========================================================
//* EXERCICE 1 : TRI SIMPLE SUR UNE CLE
//* ==========================================================
//* Trier par nom (pos 4-13) en ordre ascendant
//*
//TRIENOM  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.TP05.INPUT,DISP=SHR
//SORTOUT  DD DSN=FTEST.TP05.TRIENOM,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* TRI PAR NOM ASCENDANT
  SORT FIELDS=(4,10,CH,A)
/*
//*
//* ==========================================================
//* EXERCICE 2 : TRI SUR PLUSIEURS CLES
//* ==========================================================
//* Trier par ville (asc), puis montant (desc), puis nom (asc)
//*
//TRIMULTI EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.TP05.INPUT,DISP=SHR
//SORTOUT  DD DSN=FTEST.TP05.TRIMULTI,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* TRI PAR VILLE ASC, MONTANT DESC, NOM ASC
  SORT FIELDS=(29,10,CH,A,39,7,ZD,D,4,10,CH,A)
/*
//*
//* ==========================================================
//* EXERCICE 3 : FILTRAGE AVEC INCLUDE/OMIT
//* ==========================================================
//* Extraire uniquement les clients parisiens (CP 75xxx)
//*
//FILTPARS EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.TP05.INPUT,DISP=SHR
//SORTOUT  DD DSN=FTEST.TP05.PARIS,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* FILTRER CLIENTS PARIS (CP COMMENCE PAR 75)
  INCLUDE COND=(24,2,CH,EQ,C'75')
  SORT FIELDS=(39,7,ZD,D)
/*
//*
//* ==========================================================
//* EXERCICE 4 : REFORMATAGE AVEC OUTREC
//* ==========================================================
//* Extraire Code + Nom + Ville + Montant (format 35 car)
//*
//EXTRACT  EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.TP05.INPUT,DISP=SHR
//SORTOUT  DD DSN=FTEST.TP05.EXTRAIT,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(RECFM=FB,LRECL=35,BLKSIZE=0),
//            UNIT=SYSDA
//SYSIN    DD *
* EXTRAIRE ET REFORMATER
* Code(3) + Nom(10) + Ville(10) + Montant(7) + Separateur(5)
  SORT FIELDS=COPY
  OUTREC FIELDS=(1,3,C'-',4,10,C'-',29,10,C'-',39,7)
/*
//*
//* ==========================================================
//* EXERCICE 5 : SORTIES MULTIPLES AVEC OUTFIL
//* ==========================================================
//* Separer Paris des autres villes
//*
//SPLIT    EXEC PGM=SORT
//SYSOUT   DD SYSOUT=*
//SORTIN   DD DSN=FTEST.TP05.INPUT,DISP=SHR
//PARIS    DD DSN=FTEST.TP05.PARIS,DISP=MOD
//PROVINCE DD DSN=FTEST.TP05.PROVINCE,
//            DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=*.SORTIN,
//            UNIT=SYSDA
//SYSIN    DD *
* SEPARER PARIS ET PROVINCE
  SORT FIELDS=(4,10,CH,A)
  OUTFIL FNAMES=PARIS,INCLUDE=(24,2,CH,EQ,C'75')
  OUTFIL FNAMES=PROVINCE,SAVE
/*
//*
//* ==========================================================
//* AFFICHAGE DES RESULTATS
//* ==========================================================
//*
//PRINT1   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.TP05.TRIENOM,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=61)
//*
//PRINT2   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.TP05.EXTRAIT,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=36)
//*
//PRINT3   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=FTEST.TP05.PROVINCE,DISP=SHR
//SYSUT2   DD SYSOUT=*,DCB=(RECFM=FBA,LRECL=61)
//
