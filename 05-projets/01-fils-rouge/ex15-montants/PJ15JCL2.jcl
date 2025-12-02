//PJ15JCL2 JOB (COMPTE),'EXEC PJ15MONT',MSGLEVEL=(1,1),REGION=8M,       
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION DU PROGRAMME COBOL                                       *
//*====================================================================*
//RUN EXEC PGM=PJ15MONT                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//FDEBIT DD DSN=ROCHA.FINANCE.DEB,DISP=SHR                              
//FCREDIT DD DSN=ROCHA.FINANCE.CRED,DISP=SHR                            
//FEDITION DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
/*                                                                      
//                                                                      