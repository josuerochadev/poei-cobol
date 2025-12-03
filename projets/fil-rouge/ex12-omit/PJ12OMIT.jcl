//PJ12OMIT JOB PJ12OMIT,'PJ12OMIT',MSGLEVEL=(1,1),REGION=4M,            
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION D'UN NOUVEAU DATA SET CLIENT REDUIT                     *
//*====================================================================*
//ETAPE1 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=A                                                    
//SORTIN DD DSNAME=ROCHA.FINANCE.CLIENT,DISP=SHR                        
//SORTOUT DD DSNAME=ROCHA.FINANCE.REDUIT,VOL=SER=FDDBAS,                
//             DISP=(NEW,CATLG,DELETE),SPACE=(TRK,1),                   
//             DCB=(RECFM=FB,LRECL=80,BLKSIZE=800,DSORG=PS)             
//SYSIN DD  *                                                           
         SORT FIELDS=COPY                                               
         OUTREC FIELDS=(1:1,27,28:37,15)                                
/*                                                                      
//