//PJ14JCL2 JOB (COMPTE),'EXEC TABLES',MSGLEVEL=(1,1),REGION=8M,         
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION PROGRAMME PRINCIPAL                                      *
//*====================================================================*
//RUN EXEC PGM=PJ14MAIN                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//FREGION DD DSN=ROCHA.REGION.KSDS,DISP=SHR                             
//FCOMPTE DD DSN=ROCHA.COMPTE.KSDS,DISP=SHR                             
//FPROFES DD DSN=ROCHA.PROFES.KSDS,DISP=SHR                             
//FEDITION DD SYSOUT=A                                                  
//SYSOUT DD SYSOUT=B                                                    
/*                                                                      
//                                                                      