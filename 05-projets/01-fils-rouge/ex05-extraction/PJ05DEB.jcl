//PJ05DEB JOB PJ05DEB,'PJ05DEB',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* LIST DU CONTENU AVEC IDCAMS DU DATASET CLIENT DEDITEURS            *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
//SYSIN DD *                                                            
     PRINT -                                                            
     INDATASET(ROCHA.FINANCE.DEB.DATA) -                                
     CHAR                                                               
/*                                                                      
//                                                                      