//PJ19JCL2 JOB (COMPTE),'EXEC MOMT',MSGLEVEL=(1,1),REGION=4M,           
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                        
//*====================================================================*
//* EXECUTION - CALCUL MOUVEMENTS CLIENT                                
//*====================================================================*
//RUN      EXEC PGM=PJ19MOMT                                            
//STEPLIB  DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                           
//SORTWORK DD SPACE=(CYL,(5,5)),UNIT=SYSDA                              
//FMOUV    DD DSN=ROCHA.CLIENT.MOUV,DISP=SHR                            
//FEDITION DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
001                                                                     
/*                                                                      
//                                                                      