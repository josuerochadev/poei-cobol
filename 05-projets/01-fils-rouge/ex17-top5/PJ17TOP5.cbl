       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ17TOP5.                                            
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * LISTER LES 5 PREMIERS CLIENTS DEBITEURS (EN VALEUR)             
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-SORT ASSIGN TO SORTWORK.                           
                                                                        
            SELECT F-CLIENT ASSIGN TO FCLIENT                           
                ORGANIZATION IS INDEXED                                 
                ACCESS MODE IS SEQUENTIAL                               
                RECORD KEY IS CLI-NUM-COMPTE                            
                FILE STATUS IS WS-FS-CLI.                               
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDI.                               
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       SD F-SORT.                                                       
       01 ENR-SORT.                                                     
           05 SORT-NUM-COMPTE      PIC 9(03).                           
           05 SORT-CODE-REGION     PIC 9(02).                           
           05 SORT-NATURE-COMPTE   PIC 9(02).                           
           05 SORT-NOM             PIC X(10).                           
           05 SORT-PRENOM          PIC X(10).                           
           05 SORT-DATE-NAIS       PIC 9(08).                           
           05 SORT-SEXE            PIC X(01).                           
           05 SORT-ACTIVITE        PIC 9(02).                           
           05 SORT-SITUATION       PIC X(01).                           
           05 SORT-SOLDE           PIC 9(10).                           
           05 SORT-POSITION        PIC X(02).                           
           05 FILLER               PIC X(29).                           
                                                                        
       FD F-CLIENT.                                                     
       01 ENR-CLIENT               PIC X(80).                           
            05 CLI-NUM-COMPTE       PIC 9(03).                          
            05 FILLER               PIC X(77).                          
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION              PIC X(80).                           
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-CLI                PIC X(02).                           
       01 WS-FS-EDI                PIC X(02).                           
                                                                        
       01 WS-CPT                   PIC 9(01) VALUE 0.                   
       01 WS-EOF                   PIC 9(01) VALUE 0.                   
                                                                        
      * ZONE DE TRAVAIL                                                 
       01 WS-ENR.                                                       
           05 WS-NUM-COMPTE        PIC 9(03).                           
           05 WS-CODE-REGION       PIC 9(02).                           
           05 WS-NATURE-COMPTE     PIC 9(02).                           
           05 WS-NOM               PIC X(10).                           
           05 WS-PRENOM            PIC X(10).                           
           05 WS-DATE-NAIS         PIC 9(08).                           
           05 WS-SEXE              PIC X(01).                           
           05 WS-ACTIVITE          PIC 9(02).                           
           05 WS-SITUATION         PIC X(01).                           
           05 WS-SOLDE             PIC 9(10).                           
           05 WS-POSITION          PIC X(02).                           
           05 FILLER               PIC X(29).                           
                                                                        
      * LIGNES D'EDITION                                                
       01 WS-LIGNE-SEP             PIC X(70) VALUE ALL '*'.             
                                                                        
       01 WS-LIGNE-TITRE.                                               
           05 FILLER               PIC X(20) VALUE SPACES.              
           05 FILLER               PIC X(30)                            
              VALUE 'TOP 5 CLIENTS DEBITEURS'.                          
           05 FILLER               PIC X(20) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-ENTETE.                                              
           05 FILLER               PIC X(05) VALUE 'RANG '.             
           05 FILLER               PIC X(10) VALUE 'COMPTE    '.        
           05 FILLER               PIC X(15) VALUE 'NOM            '.   
           05 FILLER               PIC X(15) VALUE 'PRENOM         '.   
           05 FILLER               PIC X(15) VALUE 'SOLDE          '.   
           05 FILLER               PIC X(10) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-DETAIL.                                              
           05 FILLER               PIC X(02) VALUE SPACES.              
           05 WS-DET-RANG          PIC 9(01).                           
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 WS-DET-COMPTE        PIC 9(03).                           
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 WS-DET-NOM           PIC X(10).                           
           05 FILLER               PIC X(05) VALUE SPACES.              
           05 WS-DET-PRENOM        PIC X(10).                           
           05 FILLER               PIC X(03) VALUE SPACES.              
           05 WS-DET-SOLDE         PIC Z(09)9.                          
           05 FILLER               PIC X(16) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-VIDE            PIC X(70) VALUE SPACES.              
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            SORT F-SORT                                                 
               ON DESCENDING KEY SORT-SOLDE                             
               INPUT PROCEDURE IS SELECTION                             
               OUTPUT PROCEDURE IS EDITION                              
            STOP RUN.                                                   
                                                                        
       SELECTION.                                                       
            OPEN INPUT F-CLIENT                                         
            PERFORM UNTIL WS-EOF = 1                                    
               READ F-CLIENT INTO WS-ENR                                
                   AT END MOVE 1 TO WS-EOF                              
                   NOT AT END                                           
                       IF WS-POSITION = 'DB'                            
                           MOVE WS-ENR TO ENR-SORT                      
                           RELEASE ENR-SORT                             
                       END-IF                                           
               END-READ                                                 
            END-PERFORM                                                 
            CLOSE F-CLIENT.                                             
                                                                        
       EDITION.                                                         
            OPEN OUTPUT F-EDITION                                       
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-TITRE                       
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-ENTETE                      
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
                                                                        
            MOVE 0 TO WS-CPT    
                                        
            PERFORM UNTIL WS-CPT = 5                                    
                RETURN F-SORT INTO WS-ENR                               
                    AT END MOVE 5 TO WS-CPT                             
                    NOT AT END                                          
                        ADD 1 TO WS-CPT                                 
                        MOVE WS-CPT TO WS-DET-RANG                      
                        MOVE WS-NUM-COMPTE TO WS-DET-COMPTE             
                        MOVE WS-NOM TO WS-DET-NOM                       
                        MOVE WS-PRENOM TO WS-DET-PRENOM                 
                        MOVE WS-SOLDE TO WS-DET-SOLDE                   
                        WRITE ENR-EDITION FROM WS-LIGNE-DETAIL          
                END-RETURN                                              
            END-PERFORM       
                                          
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
                                                                        
            CLOSE F-EDITION.                                            

                      

