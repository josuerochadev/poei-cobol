       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ21MERG.                                            
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * FUSION DES MOUVEMENTS DE 3 MOIS AVEC MERGE INTERNE              
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-SORT ASSIGN TO SORTWORK.                           
                                                                        
            SELECT F-JANVIER ASSIGN TO FJANV                            
                FILE STATUS IS WS-FS-JANV.                              
                                                                        
            SELECT F-FEVRIER ASSIGN TO FFEVR                            
                FILE STATUS IS WS-FS-FEVR.                              
                                                                        
            SELECT F-MARS ASSIGN TO FMARS                               
                FILE STATUS IS WS-FS-MARS.                              
                                                                        
            SELECT F-FUSION ASSIGN TO FFUSION                           
                FILE STATUS IS WS-FS-FUSI.                              
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       SD F-SORT.                                                       
       01 ENR-SORT.                                                     
           05 SORT-NUM-COMPTE      PIC 9(03).                           
           05 SORT-LIBELLE         PIC X(15).                           
           05 SORT-MONTANT         PIC 9(06).                           
           05 SORT-SENS            PIC X(02).                           
           05 SORT-NATURE          PIC X(03).                           
           05 SORT-DATE            PIC X(10).                           
           05 FILLER               PIC X(41).                           
                                                                        
       FD F-JANVIER.                                                    
       01 ENR-JANVIER              PIC X(80).                           
                                                                        
       FD F-FEVRIER.                                                    
       01 ENR-FEVRIER              PIC X(80).                           
                                                                        
       FD F-MARS.                                                       
       01 ENR-MARS                 PIC X(80).                           
                                                                        
       FD F-FUSION.                                                     
       01 ENR-FUSION               PIC X(80).                           
                                                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-JANV               PIC X(02).                           
       01 WS-FS-FEVR               PIC X(02).                           
       01 WS-FS-MARS               PIC X(02).                           
       01 WS-FS-FUSI               PIC X(02).                           
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            DISPLAY 'DEBUT DE LA FUSION DES 3 MOIS'                     
                                                                        
            MERGE F-SORT                                                
                ON ASCENDING KEY SORT-NUM-COMPTE                        
                ON ASCENDING KEY SORT-DATE                              
                USING F-JANVIER F-FEVRIER F-MARS                        
                GIVING F-FUSION                                         
                                                                        
            DISPLAY 'FUSION TERMINEE AVEC SUCCES'                       
                                                                        
            STOP RUN.                                                   
                                                                        
