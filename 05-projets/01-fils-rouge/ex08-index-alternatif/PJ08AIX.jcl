//PJ08AIX JOB PJ08AIX,'PJ08AIX',MSGLEVEL=(1,1),REGION=4M,               
//          MSGCLASS=Y,CLASS=A,NOTIFY=&SYSUID                           
//*====================================================================*
//* DEUXIEME PARTIE - UTILISATION DES COMMANDES ET UTILITAIRES VSAM    *
//* DEFINITION DE L'AIX (REGION)                                       *
//*====================================================================*
//ETAPE1 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
   DELETE ROCHA.AIX.REGION                                              
   DEFINE AIX -                                                         
         (NAME(ROCHA.AIX.REGION) -                                      
         RELATE (ROCHA.FINANCE.CLIENT.KSDS) -                           
         TRACKS(1,1) -                                                  
         VOLUMES(FDDBAS) -                                              
         RECORDSIZE(80,80) -                 
         SHAREOPTIONS(1,3) -                                            
         KEYS(2,3) -                                                    
         FREESPACE(10,10) -                                             
         NOREUSE -                                                      
         UPGRADE -                                                      
         NONUNIQUEKEY)                                                  
//*====================================================================*
//* DEFINE PATH DE L'AIX REGION                                        *
//*====================================================================*
//ETAPE2 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
     DEFINE PATH -                                                      
         (NAME (ROCHA.PATH.AIX.REGION) -                                
         PATHENTRY (ROCHA.AIX.REGION))                                  
/*       
//*====================================================================*
//* DEFINE DE BUILDINDEX : REGION                                     * 
//*====================================================================*
//ETAPE3 EXEC PGM=IDCAMS                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN DD *                                                            
         BLDINDEX -                                                     
         INDATASET (ROCHA.FINANCE.CLIENT.KSDS) -                        
         OUTDATASET (ROCHA.AIX.REGION) -                                
         NOSORTCALL                                                     
     IF LASTCC=0 THEN
        LISTCAT ENTRIES (ROCHA.AIX.REGION) ALL                          
/*                                                                      
//                                                                                                                                                                