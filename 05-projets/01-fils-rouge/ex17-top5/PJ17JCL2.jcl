//PJ17JCL2 JOB (COMPTE),'EXEC TOP5',MSGLEVEL=(1,1),REGION=4M,           
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION DU PROGRAMME COBOL TOP 5                                 *
//*====================================================================*
//RUN EXEC PGM=PJ17TOP5                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//SORTWORK DD SPACE=(CYL,(5,5)),UNIT=SYSDA                              
//FCLIENT  DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR                    
//FEDITION DD SYSOUT=*                                                  
//SYSOUT DD SYSOUT=*                                                    
/*                                                                      
//                                                                      