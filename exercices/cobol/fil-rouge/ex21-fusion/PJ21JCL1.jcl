//PJ21JCL1 JOB (COMPTE),'SEPARATION MOIS',CLASS=A,MSGCLASS=X,           
//             NOTIFY=&SYSUID                                           
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL *           
//* COMPOSER DES DONNEES FICTIVES DE TROIS MOIS                         
//* DELETE DU FICHIER SI EXISTANT *                                     
//*====================================================================*
//ETAPE1 EXEC PGM=IEFBR14                                               
//DELJAN DD DSN=ROCHA.CLIENT.JANVIER,                                   
//           DISP=(MOD,DELETE,DELETE),                                  
//           SPACE=(TRK,(1,1))                                          
//DELFEV DD DSN=ROCHA.CLIENT.FEVRIER,                                   
//           DISP=(MOD,DELETE,DELETE),                                  
//           SPACE=(TRK,(1,1))                                          
//DELMAR DD DSN=ROCHA.CLIENT.MARS,                                      
//           DISP=(MOD,DELETE,DELETE),                                  
//           SPACE=(TRK,(1,1))                                          
//*=====================================================================
//* EXTRACTION DU MOIS DE JANVIER                                       
//*=====================================================================
//ETAPE2 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.CLIENT.MOUV,DISP=SHR                              
//SORTOUT DD DSN=ROCHA.CLIENT.JANVIER,                                  
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(1,1),RLSE),                                    
//           DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)                       
//SYSIN DD *                                                            
    SORT FIELDS=(1,3,CH,A)                                              
    INCLUDE COND=(35,2,CH,EQ,C'01')                                     
/*                                                                      
//*=====================================================================
//* EXTRACTION DU MOIS DE FEVRIER                                       
//*=====================================================================
//ETAPE3 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.CLIENT.MOUV,DISP=SHR                              
//SORTOUT DD DSN=ROCHA.CLIENT.FEVRIER,                                  
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(1,1),RLSE),                                    
//           DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)                       
//SYSIN DD *                                                            
    SORT FIELDS=(1,3,CH,A)                                              
    INCLUDE COND=(35,2,CH,EQ,C'02')                                     
/*                                                                      
//*=====================================================================
//* EXTRACTION DU MOIS DE MARS                                          
//*=====================================================================
//ETAPE4 EXEC PGM=SORT                                                  
//SYSOUT DD SYSOUT=*                                                    
//SORTIN DD DSN=ROCHA.CLIENT.MOUV,DISP=SHR                              
//SORTOUT DD DSN=ROCHA.CLIENT.MARS,                                     
//           DISP=(NEW,CATLG,DELETE),                                   
//           SPACE=(TRK,(1,1),RLSE),                                    
//           DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)                       
//SYSIN DD *                                                            
    SORT FIELDS=(1,3,CH,A)                                              
    INCLUDE COND=(35,2,CH,EQ,C'03')                                     
/*                                                                      
//                                                                      


