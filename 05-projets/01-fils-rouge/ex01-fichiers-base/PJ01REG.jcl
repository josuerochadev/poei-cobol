//PJ01REG JOB PJ01REG,'PJ01REG',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* PREMIERE PARTIE - CHARGEMENT DES DONNEES DE BASE                   *
//*                 - ET DES FICHIERS REFERENTIELS                     *
//* DEFINITION ET CREATION DU PS REGION                                *
//*====================================================================*
//ETAPE1 EXEC PGM=IEBGENER                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1 DD *                                                           
01GRAND-EST                                                             
02ILE-DE-FRANCE                                                         
03NORMANDIE                                                             
04BRETAGNE                                                              
/*                                                                      
//SYSUT2 DD DSN=ROCHA.FINANCE.REGIONPS,DISP=(NEW,CATLG,DELETE),   
//          SPACE=(TRK,(1,1)),UNIT=3390,VOL=SER=FDDBAS,                 
//          DCB=(LRECL=80,BLKSIZE=0,RECFM=FB,DSORG=PS)                  
//SYSIN DD DUMMY                                                        
/*                                                                      
//*====================================================================*
//* DEFINITION DU FICHIER VSAM KSDS REGION                             *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
    DELETE ROCHA.REGION.KSDS PURGE                                      
    DEFINE CLUSTER (NAME(ROCHA.REGION.KSDS)        -                    
                   TRACKS ( 2 1 )                  -                    
                   VOL (FDDBAS)                    -                    
                   CISZ (4096)                     -                    
                   RECORDSIZE (80 80)              -    
                   INDEXED                         -                    
                   KEYS(2,0)                       -                    
                   SHAREOPTIONS(1,3)               -                    
                   NOREUSE)                        -                    
           DATA (NAME(ROCHA.REGION.KSDS.DATA))     -                    
           INDEX (NAME(ROCHA.REGION.KSDS.INDEX))                        
    IF LASTCC=0 THEN                                                    
       LISTCAT ALL LEVEL(ROCHA.REGION.KSDS)                             
/*                                                                      
//*====================================================================*
//* CHARGEMENT DU FICHIER KSDS A PARTIR DU PS                          *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//INPUT DD DSN=ROCHA.FINANCE.REGIONPS,DISP=SHR                          
//OUTPUT DD DSN=ROCHA.REGION.KSDS,DISP=SHR      
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
//INDD DD DSN=ROCHA.REGION.KSDS,DISP=SHR                                
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR 
/*        
//                                                               