//PJ09AIX JOB PJ09AIX,'PJ09AIX',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION DE L'AIX (ACTIVITE PRO)                                 *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
   DELETE ROCHA.AIX.ACTPROF                                             
   DEFINE AIX -                                                         
         (NAME(ROCHA.AIX.ACTPROF) -                                     
         RELATE (ROCHA.FINANCE.CLIENT.KSDS) -                           
         TRACKS(1,1) -                                                  
         VOLUMES(FDDBAS) -                                              
         RECORDSIZE(80,80) -                                            
         SHAREOPTIONS(1,3) -                                            
         KEYS(2,36) -                                                   
         FREESPACE(10,10) -                                             
         NOREUSE -                                                      
         UPGRADE -                                                      
         NONUNIQUEKEY)                                                  
/*                                                                      
//*====================================================================*
//* DEFINE PATH DE L'AIX ACTIVITE PRO                                  *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
     DEFINE PATH -                                                      
         (NAME (ROCHA.PATH.AIX.ACTPROF) -                               
         PATHENTRY (ROCHA.AIX.ACTPROF))                                 
/*                                                                      
//*====================================================================*
//* DEFINE DE BUILDINDEX : REGION                                     * 
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
         BLDINDEX -                                                     
         INDATASET (ROCHA.FINANCE.CLIENT.KSDS) -                        
         OUTDATASET (ROCHA.AIX.ACTPROF) -                               
         NOSORTCALL                                                     
     IF LASTCC=0 THEN
        LISTCAT ENTRIES (ROCHA.AIX.ACTPROF) ALL                         
/*                                                                      
//                                                                      


