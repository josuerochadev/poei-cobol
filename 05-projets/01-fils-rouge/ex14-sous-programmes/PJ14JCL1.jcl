//PJ14JCL1 JOB (COMPTE),'COMPIL TABLES',MSGLEVEL=(1,1),REGION=8M,       
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL            *
//* COMPILATION DU SOUS PROGRAMME REGION                               *
//*====================================================================*
//REGION EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ14REG),DISP=SHR             
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ14REG),DISP=SHR              
//*====================================================================*
//* COMPILATION DU SOUS PROGRAMME COMPTE                               *
//*====================================================================*
//COMPTE EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ14CPT),DISP=SHR             
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ14CPT),DISP=SHR              
//*====================================================================*
//* COMPILATION DU SOUS PROGRAMME PROFESSION                           *
//*====================================================================*
//PROFES EXEC IGYWCL                                                    
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ14PRO),DISP=SHR             
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ14PRO),DISP=SHR              
//*====================================================================*
//* COMPILATION DU PROGRAMME PRINCIPAL                                 *
//*====================================================================*
//MAIN EXEC IGYWCL                                                      
//COBOL.SYSIN DD DSN=ROCHA.FINANCE.SOURCE(PJ14MAIN),DISP=SHR            
//LKED.SYSLMOD DD DSN=ROCHA.FINANCE.LOAD(PJ14MAIN),DISP=SHR             
//LKED.SYSLIB DD DSN=CEE.SCEELKED,DISP=SHR                              
//            DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                        
/*                                                                      
//                                                                      
