//PJ13JCL1 JOB (COMPTE),'COMPIL PJ13AJOU',MSGLEVEL=(1,1),REGION=8M,     
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPILATION DU PROGRAMME COBOL                                     *
//*====================================================================*
//ETAPE1 EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ13AJOU),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ13AJOU),DISP=SHR             
/*                                                                      
//                                                                      
