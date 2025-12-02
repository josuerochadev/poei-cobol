//PJ03CRED JOB PJ03CRED,'PJ03CRED',MSGLEVEL=(1,1),REGION=4M,            
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION D'UN DATA SET AVEC LES CLIENTS CREDITEURS               *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
     DELETE ROCHA.FINANCE.CRED PURGE                                    
     DEFINE CLUSTER (NAME(ROCHA.FINANCE.CRED) -                         
                    TRACKS ( 2  1 ) -                                   
                    VOL (FDDBAS) -                                      
                    CISZ (4096) -                                       
                    RECORDSIZE (80 80) -                                
                    SHAREOPTIONS(1,3) -                           
                    NONINDEXED -                                        
                    REUSE) -                                            
            DATA (NAME(ROCHA.FINANCE.CRED.DATA))                        
      IF LASTCC=0 THEN -                                                
            LISTCAT ALL LEVEL(ROCHA.FINANCE.CRED)                       
/*                                                                      
//*====================================================================*
//* TRI DU FICHIER AVEC SORT INCLUDE                                   *
//*====================================================================*
//ETAPE2 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.FINANCE.CLIENT,DISP=SHR                         
//SORTOUT DD DSN=ROCHA.FINANCE.CRED,DISP=SHR                            
//SYSIN DD *                                                            
         SORT FIELDS=COPY                                               
         INCLUDE COND=(60,2,CH,EQ,C'CR')     
/*                                                                      
//*====================================================================*
//* PRINT DU FICHIER                                              *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
//INDD DD DSN=ROCHA.FINANCE.CRED,DISP=SHR                               
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                      
                           

      