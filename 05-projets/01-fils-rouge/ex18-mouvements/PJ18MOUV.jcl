//PJ18MOUV JOB (COMPTE),'CREATION MOUVEMENT',CLASS=A,MSGCLASS=X,        
//             NOTIFY=&SYSUID                                           
//*====================================================================*
//* TROISIEME PARTIE MàJ DES DONNEES ET PROGRAMMATION COBOL *           
//* DEFINITION D'UN DATA SET POUR GERER LES MOUVEMENTS DES CLIENTS     *
//* DELETE DU FICHIER SI EXISTANT *                                     
//*====================================================================*
//ETAPE1 EXEC PGM=IEFBR14                                               
//DELFILE DD DSN=ROCHA.CLIENT.MOUV,                                     
//           DISP=(MOD,DELETE,DELETE),                                  
//           SPACE=(TRK,(1,1))                                          
//ETAPE2 EXEC PGM=IEBGENER                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1 DD *                                                           
001VIREMENTSALAIRE150000CRVIR2025/04/15                                 
001PAIEMENTLOYER  005000DBCHQ2025/04/20                                 
001RETRAITESPECES 000500DBVIR2025/04/25                                 
002VIREMENTRECU   020000CRVIR2025/04/10                                 
002PAIEMENTFACTUR 003500DBCHQ2025/04/18                                 
003VERSEMENT      010000CRVER2025/04/05                                 
003ACHATINTERNET  000150DBCHQ2025/03/12                                 
004SALAIRE        180000CRVIR2025/03/15                                 
004COURSES        000250DBCHQ2025/03/22                                 
005VIREMENTRECU   005000CRVIR2025/03/08                                 
005PAIEMENTEDF    000120DBVIR2025/02/28                                 
006VERSEMENTESP   002000CRVER2025/02/03                                 
006RETRAITDAB     000300DBVIR2025/02/17                                 
007SALAIRE        140000CRVIR2025/02/15                                 
007LOYERJANVIER   008000DBCHQ2025/02/25                                 
008PRIMEJANVIER   050000CRVIR2025/01/20                                 
008ASSURANCEAUTO  001200DBVIR2025/01/30                                 
009VIREMENTFAMILLE025000CRVIR2025/01/12                                 
009TELEPHONE      000050DBCHQ2025/01/19                                 
010VERSEMENT      015000CRVER2025/01/07                                 
/*                                                                      
//SYSUT2 DD DSN=ROCHA.CLIENT.MOUV,                                      
//          DISP=(NEW,CATLG,DELETE),                                    
//          SPACE=(TRK,(1,1)),VOL=SER=FDDBAS,                           
//          DCB=(RECFM=FB,LRECL=80,BLKSIZE=8000)                        
//SYSIN DD DUMMY                                                        
/*                                                                      
//                                                                      