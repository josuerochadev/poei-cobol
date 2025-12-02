       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ14PRO.                                             
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * SOUS PROGRAMME - EDITION DE LA TABLE ACTIVITE PROFISSIONNELLE   
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-PROFES ASSIGN TO FPROFES                           
                ORGANIZATION IS INDEXED                                 
                ACCESS MODE IS SEQUENTIAL                               
                RECORD KEY IS PRO-CODE                                  
                FILE STATUS IS WS-FS-PROFES.                            
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDITION.                           
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-PROFES.                                                     
       01 ENR-PROFES.                                                   
           05 PRO-CODE PIC 9(2).                                        
           05 PRO-LIBELLE  PIC X(20).                                   
           05 FILLER PIC X(58).                                         
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION PIC X(80).                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-PROFES PIC X(2).                                        
       01 WS-FS-EDITION PIC X(2).                                       
       01 WS-EOF PIC 9(1) VALUE 0.                                      
           88 EOF VALUE 1.                                              
                                                                        
      * LIGNE DU TITRE                                                  
       01 WS-TITRE.                                                     
           05 FILLER PIC X(15) VALUE SPACES.                            
           05 FILLER PIC X(50)                                          
                     VALUE '*** TABLE DES ACTIVITES PRO ***'.           
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
            OPEN INPUT F-PROFES                                         
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
                                                                        
            PERFORM LIRE-PROFES.                                        
                                                                        
        TRAITEMENT.                                                     
            MOVE PRO-CODE TO WS-DET-CODE                                
            MOVE PRO-LIBELLE TO WS-DET-LIBELLE                          
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                AFTER ADVANCING 1 LINE                                  
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                AFTER ADVANCING 1 LINE                                  
                                                                        
            PERFORM LIRE-PROFES.                                        
                                                                        
        FIN.                                                            
            CLOSE F-PROFES                                              
            CLOSE F-EDITION.                                            
                                                                        
        LIRE-PROFES.                                                    
            READ F-PROFES                                               
                AT END MOVE 1 TO WS-EOF                                 
            END-READ.                                                   
                                                                        
                                                  