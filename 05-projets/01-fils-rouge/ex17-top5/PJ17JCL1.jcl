//PJ17JCL1 JOB (COMPTE),'COMPIL TOP5',MSGLEVEL=(1,1),REGION=4M,         
//             NOTIFY=&SYSUID                                           
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPILATION PROGRAMME TOP 5                                         
//*====================================================================*
//COMPIL EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ17TOP5),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ17TOP5),DISP=SHR             
/*                                                                      
//                                                                      