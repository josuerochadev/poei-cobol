//PJ20JCL2 JOB (COMPTE),'EXEC RELEVE',MSGLEVEL=(1,1),REGION=4M,         
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                        
//*====================================================================*
//* EXECUTION - RELEVE DE COMPTE CLIENT                                 
//*====================================================================*
//RUN      EXEC PGM=PJ20RELV                                            
//STEPLIB  DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                           
//SORTWORK DD SPACE=(CYL,(5,5)),UNIT=SYSDA                              
//FCLIENT  DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR                    
//FMOUV    DD DSN=ROCHA.CLIENT.MOUV,DISP=SHR                            
//FEDITION DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SYSIN    DD *                                                         
001                                                                     
/*                                                                      
//                                                                      