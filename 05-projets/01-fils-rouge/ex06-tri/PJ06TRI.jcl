//PJ06TRI JOB PJ06TRI,'PJ06TRI',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* EN UTILISANT L'UTILITAIRE SORT, TRIER PAR NUMERO COMPTE            *
//*====================================================================*
//ETAPE1 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.FINANCE.CLIENTPS,DISP=SHR                         
//SORTOUT DD DSN=ROCHA.FINANCE.CLIENT,                                  
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,1),                                            
//            VOL=SER=FDDBAS,                                           
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)              
//SYSIN DD *                                                            
         SORT FIELDS=(1,3,FS,D)                                         
/*  
//  

