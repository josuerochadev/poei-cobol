//PJ16JCL2 JOB (COMPTE),'EXEC PJ16COND',MSGLEVEL=(1,1),REGION=8M,       
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION DU PROGRAMME COBOL                                       *
//*====================================================================*
//RUN EXEC PGM=PJ16COND                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//FCLIENT DD DSN=ROCHA.FINANCE.CLIENT,DISP=SHR                          
//FEDITION DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
/*                                                                      
//                                                                      