//PJ11MERG JOB PJ11MERG,'PJ11MERG',MSGLEVEL=(1,1),                      
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* FUSION DES DEUX DATASETS (CLIENTS COMPTABLES ET FONCTIONNAIRES)    *
//*====================================================================*
//ETAPE1 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN1 DD DSNAME=ROCHA.FINANCE.FONCT,DISP=SHR                        
//SORTIN2 DD DSNAME=ROCHA.FINANCE.COMPT,DISP=SHR                        
//SORTOUT DD DSNAME=ROCHA.FINANCE.FUSIO,                                
//           DISP=(NEW,CATLG,DELETE),SPACE=(TRK,1),VOL=SER=FDDBAS,      
//           DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)               
//SYSIN DD  *                                                           
         MERGE FIELDS=(37,2,FS,A)                                       
/*                                                                      
//