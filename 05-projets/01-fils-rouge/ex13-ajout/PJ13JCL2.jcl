//PJ13JCL2 JOB (COMPTE),'EXEC PJ13AJOU',MSGLEVEL=(1,1),REGION=8M,       
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION DU PROGRAMME COBOL                                       *
//*====================================================================*
//RUN EXEC PGM=PJ13AJOU                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//FCLIENT DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR                     
//SYSOUT DD SYSOUT=*                                                    
/*                                                                      
//                                                                      