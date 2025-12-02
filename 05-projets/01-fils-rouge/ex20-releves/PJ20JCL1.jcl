//PJ20JCL1 JOB (COMPTE),'COMPIL RELEVE',MSGLEVEL=(1,1),REGION=4M,       
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                        
//*====================================================================*
//* COMPILATION                                                         
//*====================================================================*
//COMPIL   EXEC IGYWCL                                                  
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ20RELV),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ20RELV),DISP=SHR             
/*                                                                      
//                                                                      