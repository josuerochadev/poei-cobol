//PJ15JCL1 JOB (COMPTE),'COMPIL PJ15MONT',MSGLEVEL=(1,1),REGION=8M,     
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPILATION DU PROGRAMME COBOL                                     *
//*====================================================================*
//ETAPE1 EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ15MONT),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ15MONT),DISP=SHR             
/*                                                                      
//                                                                      