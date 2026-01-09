//PJ07TRI JOB PJ07TRI,'PJ07TRI',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION DU FICHIER VSAM KSDS AVEC LES CLIENTS TRIes PAR NUMERO  *
//* DE COMPTE, ORDRE DESCENDENTE                                       *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
    DELETE ROCHA.FINANCE.CLIENT.KSDS PURGE                              
    DEFINE CLUSTER (NAME(ROCHA.FINANCE.CLIENT.KSDS) -                   
                   TRACKS ( 2 1 )                  -                    
                   VOL (FDDBAS)                    -                    
                   CISZ (4096)                     -                    
                   RECORDSIZE (80 80)              -                    
                   INDEXED                         -                    
                   KEYS(3,0)                       -                    
                   SHAREOPTIONS(1,3)               -                    
                   NOREUSE)                        -                    
           DATA (NAME(ROCHA.FINANCE.CLIENT.KSDS.DATA)) -                
           INDEX (NAME(ROCHA.FINANCE.CLIENT.KSDS.INDEX))                
    IF LASTCC=0 THEN
       LISTCAT ALL LEVEL(ROCHA.FINANCE.CLIENT.KSDS)                     
/*                                                                      
//*====================================================================*
//* CHARGEMENT DU FICHIER KSDS A PARTIR DU PS                          *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSOUT DD SYSOUT=*                                                    
//SYSPRINT DD SYSOUT=*                                                  
//INPUT DD DSN=ROCHA.FINANCE.CLIENTPS,DISP=SHR                          
//OUTPUT DD DSN=ROCHA.FINANCE.CLIENT.KSDS,DISP=SHR                      
//SYSIN DD *                                                            
      REPRO  -                                                          
           INFILE(INPUT) -                                              
           OUTFILE(OUTPUT)                                              
/*                                                                      
//                                                                      