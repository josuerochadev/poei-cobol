//PJ01PRO JOB PJ01PRO,'PJ01PRO',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* PREMIERE PARTIE - CHARGEMENT DES DONNEES DE BASE                   *
//*                 - ET DES FICHIERS REFERENTIELS                     *
//* DEFINITION ET CREATION DU PS PROFESSION                            *
//*====================================================================*
//ETAPE1 EXEC PGM=IEBGENER                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1 DD *                                                           
05FONCTIONNAIRE                                                         
15MEDICIN                                                               
25AVOCAT                                                                
35COMPTABLE                                                             
45FLEURISTE                                                             
55ARTISAN  
65ECRIVAIN                                                              
75CHANTEUR                                                              
85PROFESSEUR                                                            
95INFORMATICIEN                                                         
/*                                                                      
//SYSUT2 DD DSN=ROCHA.FINANCE.PROFESPS,DISP=(NEW,CATLG,DELETE),         
//          SPACE=(TRK,(1,1)),UNIT=3390,VOL=SER=FDDBAS,                 
//          DCB=(LRECL=80,BLKSIZE=0,RECFM=FB,DSORG=PS)                  
//SYSIN DD DUMMY                                                        
/*                                                                      
//*====================================================================*
//* DEFINITION DU FICHIER VSAM KSDS PROFESSION                         *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
    DELETE ROCHA.PROFES.KSDS PURGE                                      
    DEFINE CLUSTER (NAME(ROCHA.PROFES.KSDS)        -                    
                   TRACKS ( 2 1 )                  -                    
                   VOL (FDDBAS)                    -                    
                   CISZ (4096)                     -                    
                   RECORDSIZE (80 80)              -                    
                   INDEXED                         -                    
                   KEYS(2,0)                       -                    
                   SHAREOPTIONS(1,3)               -                    
                   NOREUSE)                        -                    
           DATA (NAME(ROCHA.PROFES.KSDS.DATA))     -                    
           INDEX (NAME(ROCHA.PROFES.KSDS.INDEX))                        
    IF LASTCC=0 THEN                                                    
       LISTCAT ALL LEVEL(ROCHA.PROFES.KSDS)                             
/*                                                                      
//*====================================================================*
//* CHARGEMENT DU FICHIER KSDS à PARTIR DU PS                          *
//*====================================================================*                                                             
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//INPUT DD DSN=ROCHA.FINANCE.PROFESPS,DISP=SHR                          
//OUTPUT DD DSN=ROCHA.PROFES.KSDS,DISP=SHR                              
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
//INDD DD DSN=ROCHA.PROFES.KSDS,DISP=SHR                                
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                                                