//PJ05CR JOB PJ05CR,'PJ05CR',MSGLEVEL=(1,1),REGION=4M,                  
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* LIST DU CONTENU AVEC IDCAMS DU DATASET CLIENT CREDITEURS           *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
//SYSIN DD *                                                            
     PRINT -                                                            
     INDATASET(ROCHA.FINANCE.CRED.DATA) -                               
     CHAR                                                               
/*                                                                      
//                                                                      