//PJ01CLT JOB PJ01CLT,'PJ01CLT',MSGLEVEL=(1,1),REGION=4M,
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*====================================================================*
//* PREMIERE PARTIE - CHARGEMENT DES DONNEES DE BASE                   *
//*                 - ET DES FICHIERS REFERENTIELS                     *
//* DEFINITION ET CREATION DU PS CLIENT                                *
//*====================================================================*
//* STRUCTURE ENREGISTREMENT CLIENT (80 octets):                       *
//*   POS 01-03 : NUMERO COMPTE      (3 NUM)                           *
//*   POS 04-05 : CODE REGION        (2 NUM)                           *
//*   POS 06-07 : NATURE COMPTE      (2 NUM)                           *
//*   POS 08-17 : NOM                (10 ALPHA)                        *
//*   POS 18-27 : PRENOM             (10 ALPHA)                        *
//*   POS 28-35 : DATE NAISSANCE     (8 NUM AAAAMMJJ)                  *
//*   POS 36    : SEXE               (1 ALPHA M/F)                     *
//*   POS 37-38 : ACTIVITE PROF      (2 NUM)                           *
//*   POS 39    : SITUATION          (1 ALPHA C/M/D/V)                 *
//*   POS 40-49 : ADRESSE            (10 ALPHA)                        *
//*   POS 50-59 : SOLDE              (10 NUM)                          *
//*   POS 60-61 : POSITION           (2 ALPHA DB/CR)                   *
//*   POS 62-80 : FILLER             (19 SPACES)                       *
//*====================================================================*
//ETAPE1 EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1 DD *
0010110MARTIN    JEAN      19850315M05CSTRASBRG  0000125000CR
0020120DURAND    MARIE     19900422F15MCOLMAR    0000045000DB
0030130BERNARD   PIERRE    19780910M25DMULHOUSE  0000230000CR
0040140PETIT     SOPHIE    19951205F35VMETZ      0000015000DB
0050150ROBERT    FRANCOIS  19820718M45CNANCY     0000180000CR
0060210RICHARD   ISABELLE  19880603F55MPARIS     0000320000DB
0070220MOREAU    PHILIPPE  19750225M65CNANTERRE  0000095000CR
0080230SIMON     NATHALIE  19920814F75DBOULOGNE  0000055000DB
0090240LAURENT   MICHEL    19680530M85VNEUILLY   0000410000CR
0100250LEFEVRE   CHRISTINE 19831112F95MCRETEIL   0000028000DB
0110310GARCIA    THOMAS    19870920M05CROUEN     0000175000CR
0120320MARTINEZ  JULIE     19940307F15DLEHAVRE   0000062000DB
0130330LOPEZ     ANTOINE   19790615M25MEVREUX    0000285000CR
0140340GONZALEZ  CAMILLE   19910428F35CDIEPPE    0000018000DB
0150350WILSON    DAVID     19860103M45VCAEN      0000145000CR
0160410MULLER    CLAIRE    19930819F55MRENNES    0000078000DB
0170420THOMAS    LUCAS     19800224M65CBREST     0000195000CR
0180430DUVAL     EMMA      19890711F75DLORIENT   0000042000DB
0190440ROUX      NICOLAS   19760918M85MVANNES    0000365000CR
0200450FAURE     JULIE     19840506F95CQUIMPER   0000088000DB
/*
//SYSUT2 DD DSN=ROCHA.FINANCE.CLIENT,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1)),UNIT=3390,VOL=SER=FDDBAS,
//           DCB=(LRECL=80,BLKSIZE=0,RECFM=FB,DSORG=PS)
//SYSIN DD DUMMY
/*
//*====================================================================*
//* DEFINITION DU FICHIER VSAM ESDS CLIENT                             *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
    DELETE ROCHA.FINANCE.CLIENT.ESDS PURGE
    DEFINE CLUSTER (NAME(ROCHA.FINANCE.CLIENT.ESDS)   -
                   TRACKS ( 1 1 )                  -
                   VOL (FDDBAS)                    -
                   CISZ (4096)                     -
                   RECORDSIZE (80 80)              -
                   SHAREOPTIONS(1,3)               -
                   NONINDEXED                      -
                   REUSE)                          -
           DATA (NAME(ROCHA.FINANCE.CLIENT.ESDS.DATA))
    IF LASTCC=0 THEN
       LISTCAT ALL LEVEL(ROCHA.FINANCE.CLIENT.ESDS)
/*
//*====================================================================*
//* CHARGEMENT DU FICHIER ESDS A PARTIR DU PS                          *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INPUT DD DSN=ROCHA.FINANCE.CLIENT,DISP=SHR
//OUTPUT DD DSN=ROCHA.FINANCE.CLIENT.ESDS,DISP=SHR
//SYSIN DD *
      REPRO  -
           INFILE(INPUT) -
           OUTFILE(OUTPUT)
/*
//*====================================================================*
//* PRINT VSAM FILE                                                    *
//*====================================================================*
//ETAPE4 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSOUT DD SYSOUT=*
//INDD DD DSN=ROCHA.FINANCE.CLIENT.ESDS,DISP=SHR
//SYSIN DD *
     PRINT -
     INFILE(INDD) -
     CHAR
/*
//
