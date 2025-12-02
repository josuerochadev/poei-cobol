//PJ21JCL3 JOB (COMPTE),'EXEC MERGE',MSGLEVEL=(1,1),REGION=4M,          
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID,TYPRUN=HOLD               
//*====================================================================*
//* TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL            *
//* EXECUTION DU PROGRAMME COBOL FUSION MERGE                          *
//*====================================================================*
//RUN EXEC PGM=PJ21MERG                                                 
//STEPLIB DD DSN=ROCHA.FINANCE.LOAD,DISP=SHR                            
//SORTWORK DD SPACE=(CYL,(5,5)),UNIT=SYSDA                              
//FJANV    DD DSN=ROCHA.CLIENT.JANVIER,DISP=SHR                         
//FFEVR    DD DSN=ROCHA.CLIENT.FEVRIER,DISP=SHR                         
//FMARS    DD DSN=ROCHA.CLIENT.MARS,DISP=SHR                            
//FFUSION  DD DSN=ROCHA.CLIENT.FUSION,                                  
//            DISP=(NEW,CATLG,DELETE),                                  
//            SPACE=(TRK,(5,5),RLSE),                                   
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)                      
//SYSOUT DD SYSOUT=*   
/*                     
//                     
