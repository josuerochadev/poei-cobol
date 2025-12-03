//PJ16JCL1 JOB (COMPTE),'COMPIL PJ16COND',MSGLEVEL=(1,1),REGION=8M,     
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPILATION DU PROGRAMME COBOL                                     *
//*====================================================================*
//ETAPE1 EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ16COND),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ16COND),DISP=SHR             
/*                                                                      
//                                                                      