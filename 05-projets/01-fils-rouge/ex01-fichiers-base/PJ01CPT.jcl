//PJ01CPT JOB PJ01CPT,'PJ01CPT',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* PREMIERE PARTIE - CHARGEMENT DES DONNEES DE BASE                   *
//*                 - ET DES FICHIERS REFERENTIELS                     *
//* DEFINITION ET CREATION DU PS NATURE COMPTE                         *
//*====================================================================*
//ETAPE1 EXEC PGM=IEBGENER                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1 DD *                                                           
10COMPTE-SALAIRE                                                        
20COMPTE-CHEQUE                                                         
30COMPTE-COMMERCIAL                                                     
40COMPTE-ENTREPRISE                                                     
50COMPTE-EPARGNE                                                        
/*        
//SYSUT2 DD DSN=ROCHA.FINANCE.COMPTEPS,DISP=(NEW,CATLG,DELETE),         
//          SPACE=(TRK,(1,1)),UNIT=3390,VOL=SER=FDDBAS,                 
//          DCB=(LRECL=80,BLKSIZE=0,RECFM=FB,DSORG=PS)                  
//SYSIN DD DUMMY                                                        
/*                                                                      
//*====================================================================*
//* DEFINITION DU FICHIER VSAM KSDS NATURE COMPTE                      *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
    DELETE ROCHA.COMPTE.KSDS PURGE                                      
    DEFINE CLUSTER (NAME(ROCHA.COMPTE.KSDS)        -                    
                   TRACKS ( 2 1 )                  -                    
                   VOL (FDDBAS)                    -                    
                   CISZ (4096)                     -                    
                   RECORDSIZE (80 80)              -                    
                   INDEXED                         -                    
                   KEYS(2,0)                       -                    
                   SHAREOPTIONS(1,3)               -                    
                   NOREUSE)                        -                    
           DATA (NAME(ROCHA.COMPTE.KSDS.DATA))     -                    
           INDEX (NAME(ROCHA.COMPTE.KSDS.INDEX))                        
    IF LASTCC=0 THEN
       LISTCAT ALL LEVEL(ROCHA.COMPTE.KSDS)                             
/*                                                                      
//*====================================================================*
//* CHARGEMENT DU FICHIER KSDS A PARTIR DU PS                          *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//INPUT DD DSN=ROCHA.FINANCE.COMPTEPS,DISP=SHR                          
//OUTPUT DD DSN=ROCHA.COMPTE.KSDS,DISP=SHR                              
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
//INDD DD DSN=ROCHA.COMPTE.KSDS,DISP=SHR                                
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                      

                                                              