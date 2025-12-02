//PJ01CLT JOB PJ01CLT,'PJ01CLT',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* PREMIERE PARTIE - CHARGEMENT DES DONNEES DE BASE                   *
//*                 - ET DES FICHIERS REFERENTIELS                     *
//* DEFINITION ET CREATION DU PS CLIENT                                *
//*====================================================================*
//ETAPE1 EXEC PGM=IEBGENER                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1 DD *                                                           
0010110NOMCLIENT1PRENOMCLT119901101M15C1230000000DB                     
0020110NOMCLIENT2PRENOMCLT219911202F15M4560000000CR                     
0030110NOMCLIENT3PRENOMCLT319921003M25D7890000000DB                     
0040110NOMCLIENT4PRENOMCLT419931104F25V9870000000CR                     
0050120NOMCLIENT5PRENOMCLT519901205M35C6540000000DB                     
0060220NOMCLIENT6PRENOMCLT619911006F35M3210000000CR      
0070220NOMCLIENT7PRENOMCLT719921107M45D7410000000DB                     
0080220NOMCLIENT8PRENOMCLT819931208F45V8520000000CR                     
0090230NOMCLIENT9PRENOMCLT919901009M55C9630000000DB                     
0100230NOMCLIEN10PRENOMCL1019911110F55M1470000000CR                     
0110330NOMCLIEN11PRENOMCL1119921211M65D2580000000DB                     
0120330NOMCLIEN12PRENOMCL1219931012F65V3690000000CR                     
0130340NOMCLIEN13PRENOMCL1319901113M75C7530000000DB                     
0140340NOMCLIEN14PRENOMCL1419911214F75M1590000000CR                     
0150340NOMCLIEN15PRENOMCL1519921015M85D7180000000DB                     
0160440NOMCLIEN16PRENOMCL1619931116F85V5230000000CR                     
0170450NOMCLIEN17PRENOMCL1719901217M95C6980000000DB                     
0180450NOMCLIEN18PRENOMCL1819911018F95M5210000000CR                     
0190450NOMCLIEN19PRENOMCL1919921119M05D4780000000DB                     
0200450NOMCLIEN20PRENOMCL2019931220F05V5230000000CR                     
/*                                                                      
//SYSUT2 DD DSN=ROCHA.FINANCE.CLIENTPS,DISP=(NEW,CATLG,DELETE),
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
    DELETE ROCHA.CLIENT.ESDS PURGE                                      
    DEFINE CLUSTER (NAME(ROCHA.CLIENT.ESDS)   -                         
                   TRACKS ( 1 1 )                  -                    
                   VOL (FDDBAS)                    -                    
                   CISZ (4096)                     -                    
                   RECORDSIZE (80 80)              -                    
                   SHAREOPTIONS(1,3)               -                    
                   NONINDEXED                      -                    
                   REUSE)                          -                    
           DATA (NAME(ROCHA.CLIENT.ESDS.DATA))                          
    IF LASTCC=0 THEN
       LISTCAT ALL LEVEL(ROCHA.CLIENT.ESDS)                             
/*                                                                      
//*====================================================================*
//* CHARGEMENT DU FICHIER ESDS A PARTIR DU PS                          *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//INPUT DD DSN=ROCHA.FINANCE.CLIENTPS,DISP=SHR                          
//OUTPUT DD DSN=ROCHA.CLIENT.ESDS,DISP=SHR                              
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
//INDD DD DSN=ROCHA.CLIENT.ESDS,DISP=SHR                                
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                                                                                           