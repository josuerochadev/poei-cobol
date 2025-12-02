       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ16COND.                                            
      *================================================================*
      * TROISIEME PARTIE MAJ DES DONNEES ET PROGRAMMATION COBOL         
      * CALCUL DEBITEURS/CREDITEURS PAR REGION AVEC NIVEAU 88           
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-CLIENT ASSIGN TO FCLIENT                           
                FILE STATUS IS WS-FS-CLI.                               
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDI.                               
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-CLIENT.                                                     
       01 ENR-DEBIT.                                                    
           05 CLI-NUM-COMPTE       PIC 9(03).                           
           05 CLI-CODE-REGION      PIC 9(02).                           
              88 REGION-GRAND-EST  VALUE 01.                            
              88 REGION-IDF        VALUE 02.                            
              88 REGION-NORMANDIE  VALUE 03.                            
              88 REGION-BRETAGNE   VALUE 04.                            
           05 CLI-NATURE-COMPTE    PIC 9(02).                           
           05 CLI-NOM              PIC X(10).                           
           05 CLI-PRENOM           PIC X(10).                           
           05 CLI-DATE-NAIS        PIC 9(08).                           
           05 CLI-SEXE             PIC X(01).                           
           05 CLI-ACTIVITE         PIC 9(02).                           
           05 CLI-SITUATION        PIC X(01).                           
           05 CLI-SOLDE            PIC 9(10).                           
           05 CLI-POSITION         PIC X(02).                           
              88 EST-DEBITEUR      VALUE 'DB'.                          
              88 EST-CREDITEUR     VALUE 'CR'.                          
           05 FILLER               PIC X(29).                           
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION              PIC X(80).                           
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-CLI                PIC X(02).                           
       01 WS-FS-EDI                PIC X(02).                           
                                                                        
       01 WS-EOF                   PIC 9(01) VALUE 0.                   
           88 EOF                  VALUE 1.                             
                                                                        
      * TOTAUX PAR REGION - DEBITEURS                                   
       01 WS-TOT-DEB-GDEST         PIC 9(15) VALUE 0.                   
       01 WS-TOT-DEB-IDF           PIC 9(15) VALUE 0.                   
       01 WS-TOT-DEB-NORM          PIC 9(15) VALUE 0.                   
       01 WS-TOT-DEB-BRET          PIC 9(15) VALUE 0.                   
                                                                        
      * TOTAUX PAR REGION - CREDITEURS                                  
       01 WS-TOT-CRE-GDEST         PIC 9(15) VALUE 0.                   
       01 WS-TOT-CRE-IDF           PIC 9(15) VALUE 0.                   
       01 WS-TOT-CRE-NORM          PIC 9(15) VALUE 0.                   
       01 WS-TOT-CRE-BRET          PIC 9(15) VALUE 0.                   
                                                                        
      * LIGNES D'EDITION                                                
       01 WS-LIGNE-SEP             PIC X(70) VALUE ALL '*'.             
                                                                        
       01 WS-LIGNE-TITRE.                                               
           05 FILLER               PIC X(05) VALUE '**** '.             
           05 FILLER               PIC X(40)                            
              VALUE 'TOTAUX PAR REGION - DEBITEURS/CREDITEURS'.         
           05 FILLER               PIC X(25) VALUE ' *****'.            
                                                                        
       01 WS-LIGNE-ENTETE.                                              
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 FILLER               PIC X(20) VALUE 'REGION'.            
           05 FILLER               PIC X(20) VALUE 'TOTAL DEBITEURS'.   
           05 FILLER               PIC X(20) VALUE 'TOTAL CREDITEURS'.  
           05 FILLER               PIC X(05) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-DETAIL.                                              
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 WS-DET-REGION        PIC X(20).                           
           05 WS-DET-TOT-DEB       PIC Z(14)9.                          
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 WS-DET-TOT-CRE       PIC Z(14)9.                          
           05 FILLER               PIC X(05) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-VIDE            PIC X(70) VALUE SPACES.              
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            PERFORM INIT                                                
            PERFORM TRAITEMENT UNTIL WS-EOF = 1                         
            PERFORM EDITER                                              
            PERFORM FIN                                                 
            STOP RUN.                                                   
                                                                        
       INIT.                                                            
            OPEN INPUT F-CLIENT                                         
            OPEN OUTPUT F-EDITION                                       
            PERFORM LIRE.                                               
                                                                        
       TRAITEMENT.                                                      
            DISPLAY 'LECTURE : ' CLI-NUM-COMPTE ' REG: ' CLI-CODE-REGION
            EVALUATE TRUE                                               
               WHEN REGION-GRAND-EST                                    
                  IF EST-DEBITEUR                                       
                    ADD CLI-SOLDE TO WS-TOT-DEB-GDEST                   
                END-IF                                                  
                IF EST-CREDITEUR                                        
                    ADD CLI-SOLDE TO WS-TOT-CRE-GDEST                   
                END-IF                                                  
             WHEN REGION-IDF                                            
                IF EST-DEBITEUR                                         
                    ADD CLI-SOLDE TO WS-TOT-DEB-IDF                     
                END-IF                                                  
                IF EST-CREDITEUR                                        
                    ADD CLI-SOLDE TO WS-TOT-CRE-IDF                     
                END-IF                                                  
             WHEN REGION-NORMANDIE                                      
                IF EST-DEBITEUR                                         
                    ADD CLI-SOLDE TO WS-TOT-DEB-NORM                    
                END-IF                                                  
                IF EST-CREDITEUR                                        
                    ADD CLI-SOLDE TO WS-TOT-CRE-NORM                    
                END-IF                                                  
             WHEN REGION-BRETAGNE                                       
                IF EST-DEBITEUR                                         
                    ADD CLI-SOLDE TO WS-TOT-DEB-BRET                    
                END-IF                                                  
                IF EST-CREDITEUR                                        
                    ADD CLI-SOLDE TO WS-TOT-CRE-BRET                    
                END-IF                                                  
            END-EVALUATE.                                               
                                                                        
            PERFORM LIRE.                                               
                                                                        
       EDITER.                                                          
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-TITRE                       
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-ENTETE                      
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                                                                        
      * REGION GRAND-EST                                                
            MOVE 'GRAND-EST'        TO WS-DET-REGION                    
            MOVE WS-TOT-DEB-GDEST   TO WS-DET-TOT-DEB                   
            MOVE WS-TOT-CRE-GDEST   TO WS-DET-TOT-CRE                   
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                                                                        
      * REGION ILE-DE-FRANCE                                            
            MOVE 'ILE-DE-FRANCE'    TO WS-DET-REGION                    
            MOVE WS-TOT-DEB-IDF     TO WS-DET-TOT-DEB                   
            MOVE WS-TOT-CRE-IDF     TO WS-DET-TOT-CRE                   
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                                                                        
      * REGION NORMANDIE                                                
            MOVE 'NORMANDIE'        TO WS-DET-REGION                    
            MOVE WS-TOT-DEB-NORM    TO WS-DET-TOT-DEB                   
            MOVE WS-TOT-CRE-NORM    TO WS-DET-TOT-CRE                   
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                      
                                                                        
      * REGION BRETAGNE                                                 
            MOVE 'BRETAGNE'         TO WS-DET-REGION                    
            MOVE WS-TOT-DEB-BRET    TO WS-DET-TOT-DEB                   
            MOVE WS-TOT-CRE-BRET    TO WS-DET-TOT-CRE                   
            WRITE ENR-EDITION FROM WS-LIGNE-DETAIL                                                                                  
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-SEP.                        
                                                                        
       FIN.                                                             
            CLOSE F-CLIENT                                              
            CLOSE F-EDITION.                                            
                                                                        
       LIRE.                                                            
            READ F-CLIENT                                               
              AT END MOVE 1 TO WS-EOF                                   
            END-READ.                                                                                                                           
