       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ15MONT.                                            
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * CALCUL MONTANT GENERAL ET MOYENNE DEBITEURS/CREDITEURS          
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-DEBIT ASSIGN TO AS-FDEBIT                          
                ORGANIZATION IS SEQUENTIAL                              
                ACCESS MODE IS SEQUENTIAL                               
                FILE STATUS IS WS-FS-DEB.                               
                                                                        
            SELECT F-CREDIT ASSIGN TO AS-FCREDIT                        
                ORGANIZATION IS SEQUENTIAL                              
                ACCESS MODE IS SEQUENTIAL                               
                FILE STATUS IS WS-FS-CRE.                               
                                                                        
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDI.                               
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-DEBIT.                                                      
       01 ENR-DEBIT.                                                    
           05 DEB-NUM-COMPTE       PIC 9(03).                           
           05 DEB-CODE-REGION      PIC 9(02).                           
           05 DEB-NATURE-COMPTE    PIC 9(02).                           
           05 DEB-NOM              PIC X(10).                           
           05 DEB-PRENOM           PIC X(10).                           
           05 DEB-DATE-NAIS        PIC 9(08).                           
           05 DEB-SEXE             PIC X(01).                           
           05 DEB-ACTIVITE         PIC 9(02).                           
           05 DEB-SITUATION        PIC X(01).                           
           05 DEB-SOLDE            PIC 9(10).                           
           05 DEB-POSITION         PIC X(02).                           
           05 FILLER               PIC X(29).                           
                                                                        
       FD F-CREDIT.                                                     
       01 ENR-CREDIT.                                                   
           05 CRE-NUM-COMPTE       PIC 9(03).                           
           05 CRE-CODE-REGION      PIC 9(02).                           
           05 CRE-NATURE-COMPTE    PIC 9(02).                           
           05 CRE-NOM              PIC X(10).                           
           05 CRE-PRENOM           PIC X(10).                           
           05 CRE-DATE-NAIS        PIC 9(08).                           
           05 CRE-SEXE             PIC X(01).                           
           05 CRE-ACTIVITE         PIC 9(02).                           
           05 CRE-SITUATION        PIC X(01).                           
           05 CRE-SOLDE            PIC 9(10).                           
           05 CRE-POSITION         PIC X(02).                           
           05 FILLER               PIC X(29).                           
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION              PIC X(60).                           
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-DEB                PIC X(02).                           
       01 WS-FS-CRE                PIC X(02).                           
       01 WS-FS-EDI                PIC X(02).                           
                                                                        
       01 WS-EOF-DEB               PIC 9(01).                           
           88 EOF-DEB              VALUE 1.                             
       01 WS-EOF-CRE               PIC 9(01).                           
           88 EOF-CRE              VALUE 1.                             
                                                                        
      * COMPTEURS ET TOTAUX                                             
       01 WS-CPT-DEB               PIC 9(05) VALUE 0.                   
       01 WS-CPT-CRE               PIC 9(05) VALUE 0.                   
       01 WS-TOT-DEB               PIC 9(15) VALUE 0.                   
       01 WS-TOT-CRE               PIC 9(15) VALUE 0.                   
       01 WS-MOY-DEB               PIC 9(12)V99 VALUE 0.                
       01 WS-MOY-CRE               PIC 9(12)V99 VALUE 0.                
                                                                        
      * LIGNES D'EDITION                                                
       01 WS-LIGNE-SEP             PIC X(60) VALUE ALL '*'.             
                                                                        
       01 WS-LIGNE-TOT-DEB.                                             
           05 FILLER               PIC X(02) VALUE '* '.                
           05 FILLER               PIC X(28)                            
              VALUE 'MONTANT GENERAL DEBITEURS : '.                     
           05 WS-ED-TOT-DEB        PIC Z(14)9.                          
           05 FILLER               PIC X(14) VALUE '             *'.    
                                                                        
       01 WS-LIGNE-TOT-CRE.                                             
           05 FILLER               PIC X(02) VALUE '* '.                
           05 FILLER               PIC X(28)                            
              VALUE 'MONTANT GENERAL CREDITEURS: '.                     
           05 WS-ED-TOT-CRE        PIC Z(14)9.                          
           05 FILLER               PIC X(14) VALUE '             *'.    
                                                                        
       01 WS-LIGNE-MOY-DEB.                                             
           05 FILLER               PIC X(02) VALUE '* '.                
           05 FILLER               PIC X(28)                            
              VALUE 'MONTANT MOYEN DEBITEURS: '.                        
           05 WS-ED-MOY-DEB        PIC Z(14)9.99.                       
           05 FILLER               PIC X(14) VALUE '             *'.    
                                                                        
       01 WS-LIGNE-MOY-CRE.                                             
           05 FILLER               PIC X(02) VALUE '* '.                
           05 FILLER               PIC X(28)                            
              VALUE 'MONTANT MOYEN CREDITEURS: '.                       
           05 WS-ED-MOY-CRE        PIC Z(14)9.99.                       
           05 FILLER               PIC X(14) VALUE '             *'.    
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
           PERFORM INIT                                                 
           PERFORM LIRE-DEB UNTIL EOF-DEB                               
           PERFORM LIRE-CRE UNTIL EOF-CRE                               
           PERFORM CALCULER                                             
           PERFORM EDITER                                               
           PERFORM FIN                                                  
           STOP RUN.                                                    
                                                                        
       INIT.                                                            
           OPEN INPUT F-DEBIT                                           
           OPEN INPUT F-CREDIT                                          
           OPEN OUTPUT F-EDITION.                                       
                                                                        
       LIRE-DEB.                                                        
           READ F-DEBIT                                                 
               AT END MOVE 1 TO WS-EOF-DEB                              
               NOT AT END                                               
                   ADD 1 TO WS-CPT-DEB                                  
                   ADD DEB-SOLDE TO WS-TOT-DEB                          
           END-READ.                                                    
                                                                        
       LIRE-CRE.                                                        
           READ F-CREDIT                                                
               AT END MOVE 1 TO WS-EOF-CRE                              
               NOT AT END                                               
                   ADD 1 TO WS-CPT-CRE                                  
                   ADD CRE-SOLDE TO WS-TOT-CRE                          
           END-READ.                                                    
                                                                        
       CALCULER.                                                        
           IF WS-CPT-DEB > 0                                            
               DIVIDE WS-TOT-DEB BY WS-CPT-DEB                          
                   GIVING WS-MOY-DEB ROUNDED                            
           END-IF.                                                      
                                                                        
           IF WS-CPT-CRE > 0                                            
               DIVIDE WS-TOT-CRE BY WS-CPT-CRE                          
                   GIVING WS-MOY-CRE ROUNDED                            
           END-IF.                                                      
                                                                        
       EDITER.                                                          
           MOVE WS-TOT-DEB TO WS-ED-TOT-DEB                             
           MOVE WS-TOT-CRE TO WS-ED-TOT-CRE                             
           MOVE WS-MOY-DEB TO WS-ED-MOY-DEB                             
           MOVE WS-MOY-CRE TO WS-ED-MOY-CRE                             
                                                                        
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-TOT-DEB                      
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-TOT-CRE                      
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-MOY-DEB                      
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-MOY-CRE                      
           WRITE ENR-EDITION FROM WS-LIGNE-SEP.                         
                                                                        
       FIN.                                                             
           CLOSE F-DEBIT                                                
           CLOSE F-CREDIT                                               
           CLOSE F-EDITION.                                             
                                                                        
