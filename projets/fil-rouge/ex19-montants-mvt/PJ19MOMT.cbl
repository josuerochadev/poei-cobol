       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ19MOMT.                                            
      *---------------------------------------------------------        
      * CALCUL MONTANT ET NOMBRE DE MOUVEMENTS D'UN CLIENT              
      * NUMERO DE COMPTE FOURNI PAR ACCEPT (IN-STREAM)                  
      *---------------------------------------------------------        
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
             SELECT F-SORT ASSIGN TO SORTWORK.                          
                                                                        
             SELECT F-MOUV ASSIGN TO FMOUV                              
               FILE STATUS IS WS-FS-MOUV.                               
                                                                        
             SELECT F-EDITION ASSIGN TO FEDITION                        
                FILE STATUS IS WS-FS-EDI.                               
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       SD F-SORT.                                                       
       01 ENR-SORT.                                                     
             05 SORT-NUM-COMPTE    PIC 9(03).                           
             05 SORT-LIBELLE       PIC X(15).                           
             05 SORT-MONTANT       PIC 9(06).                           
             05 SORT-SENS          PIC X(02).                           
             05 SORT-NATURE        PIC X(03).                           
             05 SORT-DATE          PIC X(10).                           
             05 FILLER             PIC X(41).                           
                                                                        
       FD F-MOUV.                                                       
       01 ENR-MOUV.                                                     
             05 MOUV-NUM-COMPTE    PIC 9(03).                           
             05 MOUV-LIBELLE       PIC X(15).                           
             05 MOUV-MONTANT       PIC 9(06).                           
             05 MOUV-SENS          PIC X(02).                           
             05 MOUV-NATURE        PIC X(03).                           
             05 MOUV-DATE          PIC X(10).                           
             05 FILLER             PIC X(41).                           
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION              PIC X(80).                           
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-MOUV               PIC X(02).                           
       01 WS-FS-EDI                PIC X(02).                           
                                                                        
       01 WS-EOF                   PIC 9(01) VALUE 0.                   
       01 WS-FIN-SORT              PIC 9(01) VALUE 0.                   
                                                                        
      * NUMERO DE COMPTE RECU PAR ACCEPT                                
       01 WS-NUM-RECHERCHE       PIC 9(03).                             
                                                                        
      * COMPTEURS ET TOTAUX                                             
       01 WS-CPT-MOUV              PIC 9(05) VALUE 0.                   
       01 WS-TOT-CREDIT            PIC 9(10) VALUE 0.                   
       01 WS-TOT-DEBIT             PIC 9(10) VALUE 0.                   
       01 WS-SOLDE                 PIC S9(10) VALUE 0.                  
                                                                        
      * ZONE DE TRAVAIL                                                 
       01 WS-ENR.                                                       
           05 WS-NUM-COMPTE        PIC 9(03).                           
           05 WS-LIBELLE           PIC X(15).                           
           05 WS-MONTANT           PIC 9(06).                           
           05 WS-SENS              PIC X(02).                           
           05 WS-NATURE            PIC X(03).                           
           05 WS-DATE              PIC X(10).                           
           05 FILLER               PIC X(41).                                                   
	                                                                        
      * LIGNES D'EDITION                                                
       01 WS-LIGNE-SEP             PIC X(60) VALUE ALL '*'.             
                                                                        
       01 WS-LIGNE-TITRE.                                               
             05 FILLER             PIC X(10) VALUE SPACES.              
             05 FILLER             PIC X(40)                            
                VALUE 'RESUME DES MOUVEMENTS CLIENT'.                   
             05 FILLER             PIC X(10) VALUE SPACES.              
             05 FILLER             PIC X(10) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-CLIENT.                                              
             05 FILLER             PIC X(20)                            
                VALUE '* NUMERO DE COMPTE :'.                           
             05 FILLER             PIC X(02) VALUE SPACES.              
             05 WS-ED-NUM          PIC 9(03).                           
             05 FILLER             PIC X(35) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-NBR.                                                 
             05 FILLER             PIC X(25)                            
                VALUE '* NOMBRE DE MOUVEMENTS : '.                      
             05 WS-ED-NBR          PIC Z(04)9.                          
             05 FILLER             PIC X(30) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-CREDIT.                                              
           05 FILLER               PIC X(25)                            
              VALUE '* TOTAL CREDITS :        '.                        
           05 WS-ED-CREDIT         PIC Z(09)9.                          
           05 FILLER               PIC X(25) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-DEBIT.                                               
           05 FILLER               PIC X(25)                            
              VALUE '* TOTAL DEBITS :         '.                        
           05 WS-ED-DEBIT          PIC Z(09)9.                          
           05 FILLER               PIC X(25) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-SOLDE.                                               
           05 FILLER               PIC X(25)                            
              VALUE '* SOLDE MOUVEMENTS :     '.                        
           05 WS-ED-SOLDE          PIC -(09)9.                          
           05 FILLER               PIC X(25) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-VIDE            PIC X(60) VALUE SPACES.              
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
             DISPLAY 'DEBUT DU PROGRAMME PJ19MOMT'                      
                                                                        
             ACCEPT WS-NUM-RECHERCHE FROM SYSIN                         
             DISPLAY 'CLIENT RECHERCHE : ' WS-NUM-RECHERCHE             
                                                                        
             SORT F-SORT                                                
                 ON ASCENDING KEY SORT-DATE                             
                 INPUT PROCEDURE IS SELECTION                           
                 OUTPUT PROCEDURE IS CALCUL                             
                                                                        
             PERFORM EDITION                                            
                                                                        
             DISPLAY 'FIN DU PROGRAMME'                                 
             STOP RUN.                                                  
                                                                        
       SELECTION.                                                       
             OPEN INPUT F-MOUV                                          
             MOVE 0 TO WS-EOF                                           
             PERFORM UNTIL WS-EOF = 1                                   
                 READ F-MOUV INTO WS-ENR                                
                     AT END MOVE 1 TO WS-EOF                            
                     NOT AT END                                         
                         IF WS-NUM-COMPTE = WS-NUM-RECHERCHE            
                             MOVE WS-ENR TO ENR-SORT                    
                             RELEASE ENR-SORT                           
                         END-IF                                         
                 END-READ                                               
             END-PERFORM                                                
             CLOSE F-MOUV.                                              
                                                                        
       CALCUL.                                                          
             MOVE 0 TO WS-FIN-SORT                                      
             PERFORM UNTIL WS-FIN-SORT = 1                              
                 RETURN F-SORT INTO WS-ENR                              
                     AT END MOVE 1 TO WS-FIN-SORT                       
                     NOT AT END                                         
                         ADD 1 TO WS-CPT-MOUV                           
                         IF WS-SENS = 'CR'                              
                             ADD WS-MONTANT TO WS-TOT-CREDIT            
                             ADD WS-MONTANT TO WS-SOLDE                 
                         ELSE                                           
                             ADD WS-MONTANT TO WS-TOT-DEBIT             
                             SUBTRACT WS-MONTANT FROM WS-SOLDE          
                         END-IF                                         
                 END-RETURN                                             
             END-PERFORM.                                               
                                                                        
       EDITION.                                                         
            OPEN OUTPUT F-EDITION                                       
                                                                        
             MOVE WS-NUM-RECHERCHE TO WS-ED-NUM                         
             MOVE WS-CPT-MOUV    TO WS-ED-NBR                           
             MOVE WS-TOT-CREDIT  TO WS-ED-CREDIT                        
             MOVE WS-TOT-DEBIT   TO WS-ED-DEBIT                         
             MOVE WS-SOLDE       TO WS-ED-SOLDE                         
                                                                        
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-TITRE                       
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-CLIENT                      
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-NBR                         
            WRITE ENR-EDITION FROM WS-LIGNE-CREDIT                      
            WRITE ENR-EDITION FROM WS-LIGNE-DEBIT                       
            WRITE ENR-EDITION FROM WS-LIGNE-SOLDE                       
            WRITE ENR-EDITION FROM WS-LIGNE-VIDE                        
            WRITE ENR-EDITION FROM WS-LIGNE-SEP                         
                                                                        
            CLOSE F-EDITION.                                            
                                                                        
