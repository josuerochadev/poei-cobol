       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ14CPT.                                             
      *================================================================*
      * TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL         
      * SOUS PROGRAMME - EDITION DE LA TABLE NATURE COMPTE              
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-COMPTE ASSIGN TO FCOMPTE                           
                ORGANIZATION IS INDEXED                                 
                ACCESS MODE IS SEQUENTIAL                               
                RECORD KEY IS CPT-CODE                                  
                FILE STATUS IS WS-FS-COMPTE.                            
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDITION.                           
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-COMPTE.                                                     
       01 ENR-COMPTE.                                                   
           05 CPT-CODE PIC 9(2).                                        
           05 CPT-LIBELLE  PIC X(20).                                   
           05 FILLER PIC X(58).                                         
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION PIC X(80).                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-COMPTE PIC X(2).                                        
       01 WS-FS-EDITION PIC X(2).                                       
       01 WS-EOF PIC 9(1) VALUE 0.                                      
           88 EOF VALUE 1.                                              
                                                                        
      * LIGNE DU TITRE                                                  
       01 WS-TITRE.                                                     
           05 FILLER PIC X(15) VALUE SPACES.                            
           05 FILLER PIC X(50)                                          
                     VALUE '*** TABLE DES COMPTES ***'.                 
           05 FILLER PIC X(15) VALUE SPACES.                            
                                                                        
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
            OPEN INPUT F-COMPTE                                         
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
                                                                        
            PERFORM LIRE-COMPTE.                                        
                                                                        
        TRAITEMENT.                                                     
            MOVE CPT-CODE TO WS-DET-CODE                                
            MOVE CPT-LIBELLE TO WS-DET-LIBELLE                          
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                AFTER ADVANCING 1 LINE                                  
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                AFTER ADVANCING 1 LINE                                  
                                                                        
            PERFORM LIRE-COMPTE.                                        
                                                                        
        FIN.                                                            
            CLOSE F-COMPTE                                              
            CLOSE F-EDITION.                                            
                                                                        
        LIRE-COMPTE.                                                    
            READ F-COMPTE                                               
                AT END MOVE 1 TO WS-EOF                                 
            END-READ.                                                   
                                                                        
