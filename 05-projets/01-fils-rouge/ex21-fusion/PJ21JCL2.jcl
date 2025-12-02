//PJ21JCL2 JOB (COMPTE),'COMPIL MERGE',CLASS=A,MSGCLASS=X,              
//             NOTIFY=&SYSUID                                           
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPILATION PROGRAMME MERGE                                         
//*====================================================================*
//COMPIL EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ21MERG),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ21MERG),DISP=SHR             
/*                                                                      
//                                                                      