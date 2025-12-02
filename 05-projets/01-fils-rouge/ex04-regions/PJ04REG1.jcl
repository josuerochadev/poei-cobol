//PJ04REG1 JOB PJ04REG1,'PJ04REG1',MSGLEVEL=(1,1),REGION=4M,            
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION D'UN FICHIER PS AVEC LES CLIENTS DE LA REGION 1         *
//*====================================================================*
//ETAPE1 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.FINANCE.SOURCEPS,DISP=SHR                         
//SORTOUT DD DSN=ROCHA.FINANCE.REG1,                                    
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,1),                                            
//            VOL=SER=FDDBAS,                                           
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              
//SYSIN DD *                                                            
         SORT FIELDS=COPY                                                                                      
         INCLUDE COND=(4,2,FS,EQ,01)                                    
/*                                                                      
//*====================================================================*
//* PRINT DU FICHIER                                              *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
//INDD DD DSN=ROCHA.FINANCE.REG1,DISP=SHR                               
//SYSIN DD *                                                            
     PRINT -                                                            
     INFILE(INDD) -                                                     
     CHAR                                                               
/*                                                                      
//                                                                      