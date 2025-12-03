       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ14REG.                                             
      *================================================================*
      * TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL         
      * SOUS PROGRAMME - EDITION DE LA TABLE REGION                     
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-REGION ASSIGN TO FREGION                           
                ORGANIZATION IS INDEXED                                 
                ACCESS MODE IS SEQUENTIAL                               
                RECORD KEY IS REG-CODE                                  
                FILE STATUS IS WS-FS-REGION.                            
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDITION.                           
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-REGION.                                                     
       01 ENR-REGION.                                                   
           05 REG-CODE PIC 9(2).                                        
           05 REG-LIBELLE  PIC X(20).                                   
           05 FILLER PIC X(58).                                         
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION PIC X(80).                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-REGION PIC X(2).                                                       
       01 WS-FS-EDITION PIC X(2).                                       
       01 WS-EOF PIC 9(1) VALUE 0.                                      
           88 EOF VALUE 1.                                              
                                                                        
      * LIGNE DU TITRE                                                  
       01 WS-TITRE.                                                     
           05 FILLER PIC X(20) VALUE SPACES.                            
           05 FILLER PIC X(40)                                          
                     VALUE '*** TABLE DES REGIONS ***'.                 
           05 FILLER PIC X(20) VALUE SPACES.                            
                                                                        
      * LIGNE AVEC LES ENTETES                                          
       01 WS-ENTETE.                                                    
           05 FILLER PIC X(5) VALUE SPACES.                             
           05 FILLER PIC X(10) VALUE 'CODE'.                            
           05 FILLER PIC X(5) VALUE SPACES.                             
           05 FILLER PIC X(10) VALUE 'LIBELLE'.                         
           05 FILLER PIC X(5) VALUE SPACES.                             
                                                                        
      * LIGNE AVEC LES DETAILS                                          
       01 WS-LIGNE-DETAIL.                                              
           05 FILLER PIC X(5) VALUE SPACES.                             
           05 WS-DET-CODE PIC 9(2).                                     
           05 FILLER PIC X(5) VALUE SPACES.                             
           05 WS-DET-LIBELLE PIC X(20).                                 
           05 FILLER PIC X(5) VALUE SPACES.                             
                                                                        
       01 WS-LIGNE-VIDE PIC X(80) VALUE SPACES.                         
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            PERFORM INIT                                                
            PERFORM TRAITEMENT UNTIL EOF                                
            PERFORM FIN                                                 
            GOBACK.                                                     
                                                                        
       INIT.                                                            
            OPEN INPUT F-REGION                                         
            OPEN EXTEND F-EDITION                                       
                                                                        
      * SAUT DE PAGE ET TITRES                                          
            WRITE ENR-EDITION FROM WS-TITRE                             
                AFTER ADVANCING PAGE                                    
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                AFTER ADVANCING 1 LINE                                  
            WRITE ENR-EDITION FROM WS-ENTETE                            
                AFTER ADVANCING 1 LINE                                  
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                AFTER ADVANCING 1 LINE                                  
                                                                        
            PERFORM LIRE-REGION.                                        
                                                                        
        TRAITEMENT.                                                     
            MOVE REG-CODE TO WS-DET-CODE                                
            MOVE REG-LIBELLE TO WS-DET-LIBELLE                          
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                AFTER ADVANCING 1 LINE                                  
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                AFTER ADVANCING 1 LINE                                  
                                                                        
            PERFORM LIRE-REGION.                                        
                                                                        
        FIN.                                                            
            CLOSE F-REGION                                              
            CLOSE F-EDITION.                                            
                                                                        
        LIRE-REGION.                                                    
            READ F-REGION                                               
                AT END MOVE 1 TO WS-EOF                                 
            END-READ.                                                   
              
