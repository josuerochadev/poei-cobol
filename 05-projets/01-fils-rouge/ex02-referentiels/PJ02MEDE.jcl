//PJ02MEDE JOB PJ02MEDE,'PJ02MEDE',MSGLEVEL=(1,1),REGION=4M,            
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION D'UN DATA SET AVEC LA PROFESSION 15 (MEDICIN)           *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
     DELETE ROCHA.FINANCE.MEDEC PURGE                                   
     DEFINE CLUSTER (NAME(ROCHA.FINANCE.MEDEC) -                        
                    TRACKS ( 2  1 ) -                                   
                    VOL (FDDBAS) -                                      
                    CISZ (4096) -                                       
                    RECORDSIZE (80 80) -                                
                    SHAREOPTIONS(1,3) -             
                    NONINDEXED -                                        
                    REUSE) -                                            
            DATA (NAME(ROCHA.FINANCE.MEDEC.DATA))                       
      IF LASTCC=0 THEN
            LISTCAT ALL LEVEL(ROCHA.FINANCE.MEDEC)                      
/*                                                                      
//*====================================================================*
//* TRI DU FICHIER AVEC SORT INCLUDE                                   *
//*====================================================================*
//ETAPE2 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.FINANCE.SOURCEPS,DISP=SHR                         
//SORTOUT DD DSN=ROCHA.FINANCE.MEDEC,DISP=SHR                           
//SYSIN DD *                                                            
         SORT FIELDS=COPY                                               
         INCLUDE COND=(37,2,FS,EQ,15)                                   
/*             
//*====================================================================*
//* PRINT VSAM FILE                                                    *
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
//INDD DD DSN=ROCHA.FINANCE.MEDEC,DISP=SHR                              
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                                                                               
                    