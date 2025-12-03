//PJ19JCL1 JOB (COMPTE),'COMPIL MOMT',MSGLEVEL=(1,1),REGION=4M,         
//             MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                        
//*====================================================================*
//* COMPILATION                                                         
//*====================================================================*
//COMPIL   EXEC IGYWCL                                                  
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ19MOMT),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ19MOMT),DISP=SHR             
/*                                                                      
//                                                                      