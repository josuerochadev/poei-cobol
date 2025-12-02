       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ20RELV.                                            
      *---------------------------------------------------------        
      * EDITION RELEVE DE COMPTE MOUVEMENTS CLIENT                      
      *---------------------------------------------------------        
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT F-SORT ASSIGN TO SORTWORK.                            
                                                                        
           SELECT F-CLIENT ASSIGN TO FCLIENT                            
               ORGANIZATION IS INDEXED                                  
               ACCESS MODE IS RANDOM                                    
               RECORD KEY IS CLI-NUM-COMPTE                             
               FILE STATUS IS WS-FS-CLI.                      
                                                                        
           SELECT F-MOUV ASSIGN TO FMOUV                                
               FILE STATUS IS WS-FS-MOUV.                               
                                                                        
           SELECT F-EDITION ASSIGN TO FEDITION                          
               FILE STATUS IS WS-FS-EDI.                                
                                                                        
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
                                                                        
       FD F-CLIENT.                                                     
       01 ENR-CLIENT.                                                   
           05 CLI-NUM-COMPTE       PIC 9(03).                           
           05 CLI-CODE-REGION      PIC 9(02).                           
           05 CLI-NATURE-COMPTE    PIC 9(02).                           
           05 CLI-NOM              PIC X(10).                           
           05 CLI-PRENOM           PIC X(10).                           
           05 CLI-DATE-NAIS        PIC 9(08).                           
           05 CLI-SEXE             PIC X(01).                           
           05 CLI-ACTIVITE         PIC 9(02).                           
           05 CLI-SITUATION        PIC X(01).                           
           05 CLI-SOLDE            PIC 9(10).                           
           05 CLI-POSITION         PIC X(02).                           
           05 FILLER               PIC X(29).                           
                                                                        
       FD F-MOUV.                                                       
       01 ENR-MOUV.                                                     
           05 MOUV-NUM-COMPTE      PIC 9(03).                           
           05 MOUV-LIBELLE         PIC X(15).                           
           05 MOUV-MONTANT         PIC 9(06).                           
           05 MOUV-SENS            PIC X(02).                           
           05 MOUV-NATURE          PIC X(03).                           
           05 MOUV-DATE            PIC X(10).                           
           05 FILLER               PIC X(41).                           
                                                                        
       FD F-EDITION.          
       01 ENR-EDITION              PIC X(80).                           
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-CLI                PIC X(02).                           
       01 WS-FS-MOUV               PIC X(02).                           
       01 WS-FS-EDI                PIC X(02).                           
                                                                        
       01 WS-EOF                   PIC 9(01) VALUE 0.                   
       01 WS-FIN-SORT              PIC 9(01) VALUE 0.                   
                                                                        
       01 WS-TOT-DEB-BRET          PIC 9(15) VALUE 0.                   
                                                                        
      * NUMERO DE COMPTE RECU PAR ACCEPT                                
       01 WS-NUM-RECHERCHE         PIC 9(03).                           
                                                                        
      * INFOS CLIENT                                                    
       01 WS-NOM-CLIENT            PIC X(10).                           
       01 WS-PRENOM-CLIENT         PIC X(10).                           
                                                                        
      * TOTAUX                                                          
       01 WS-TOT-CREDIT            PIC 9(10) VALUE 0.                   
       01 WS-TOT-DEBIT             PIC 9(10) VALUE 0.                   
      * DATE DU JOUR                                                    
       01 WS-DATE-JOUR.                                                 
           05 WS-ANNEE             PIC 9(04).                           
           05 WS-MOIS              PIC 9(02).                           
           05 WS-JOUR              PIC 9(02).                           
                                                                        
       01 WS-DATE-EDIT             PIC X(10).                           
                                                                        
      * ZONE DE TRAVAIL MOUVEMENT                                       
       01 WS-ENR.                                                       
           05 WS-NUM-COMPTE        PIC 9(03).                           
           05 WS-LIBELLE           PIC X(15).                           
           05 WS-MONTANT           PIC 9(06).                           
           05 WS-SENS              PIC X(02).                           
           05 WS-NATURE            PIC X(03).                           
           05 WS-DATE              PIC X(10).                           
           05 FILLER               PIC X(41).                           
           05 WS-DET-TOT-CRE       PIC Z(14)9.                          
           05 FILLER               PIC X(05) VALUE SPACES.              
                                                                        
      * LIGNES D'EDITION                                                
       01 WS-LIGNE-SEP.                                                 
           05 FILLER               PIC X(70) VALUE ALL '='.             
           05 FILLER               PIC X(10) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-TITRE.                                               
           05 FILLER               PIC X(20) VALUE SPACES.              
           05 FILLER               PIC X(30)                            
              VALUE '*** RELEVE DE COMPTE ***'.                         
           05 FILLER               PIC X(30) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-CLIENT.                                              
           05 FILLER               PIC X(13) VALUE 'NOM CLIENT : '.     
           05 WS-ED-NOM            PIC X(10).                           
           05 FILLER               PIC X(01) VALUE SPACE.               
           05 WS-ED-PRENOM         PIC X(10).                           
           05 FILLER               PIC X(15) VALUE SPACES.              
           05 FILLER               PIC X(11) VALUE 'N COMPTE : '.       
           05 WS-ED-COMPTE         PIC 9(03).                           
           05 FILLER               PIC X(17) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-ENTETE.                                              
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(14) VALUE ' DATE OPER.   '.    
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(17) VALUE ' LIBELLE        '.  
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(12) VALUE '   CREDIT   '.      
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(12) VALUE '   DEBIT    '.      
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(20) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-TIRET.                                               
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(14) VALUE ALL '-'.             
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(17) VALUE ALL '-'.             
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(12) VALUE ALL '-'.             
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(12) VALUE ALL '-'.             
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(20) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-DETAIL.                                              
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(02) VALUE SPACES.              
           05 WS-DET-DATE          PIC X(10).                           
	           05 FILLER               PIC X(02) VALUE SPACES.              
	           05 FILLER               PIC X(01) VALUE '*'.                 
	           05 FILLER               PIC X(01) VALUE SPACE.               
	           05 WS-DET-LIBELLE       PIC X(15).                           
	           05 FILLER               PIC X(01) VALUE '*'.                 
	           05 WS-DET-CREDIT        PIC Z(05)9.                          
	           05 FILLER               PIC X(06) VALUE SPACES.              
	           05 FILLER               PIC X(01) VALUE '*'.                 
	           05 WS-DET-DEBIT         PIC Z(05)9.                          
	           05 FILLER               PIC X(06) VALUE SPACES.              
	           05 FILLER               PIC X(01) VALUE '*'.                 
	           05 FILLER               PIC X(20) VALUE SPACES.              
	                                                                        
	       01 WS-LIGNE-TOTAL.                                               
	           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(14) VALUE ' TOTAUX       '.    
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(17) VALUE SPACES.              
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 WS-TOT-ED-CREDIT     PIC Z(05)9.                          
           05 FILLER               PIC X(06) VALUE SPACES.              
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 WS-TOT-ED-DEBIT      PIC Z(05)9.                          
           05 FILLER               PIC X(06) VALUE SPACES.              
           05 FILLER               PIC X(01) VALUE '*'.                 
           05 FILLER               PIC X(20) VALUE SPACES.              
                                                                        
                                                                        
       01 WS-LIGNE-DATE.                                                
           05 FILLER               PIC X(20) VALUE SPACES.              
           05 FILLER               PIC X(18) VALUE 'DATE DU RELEVE : '. 
           05 WS-ED-DATE-RELEVE    PIC X(10).                           
           05 FILLER               PIC X(32) VALUE SPACES.              
                                                                        
       01 WS-LIGNE-VIDE            PIC X(80) VALUE SPACES.              
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
           DISPLAY 'DEBUT DU PROGRAMME PJ20RELV'                        
                                                                        
           ACCEPT WS-NUM-RECHERCHE FROM SYSIN                           
           DISPLAY 'CLIENT RECHERCHE : ' WS-NUM-RECHERCHE               
                                                                        
           PERFORM LIRE-CLIENT                                          
           PERFORM GET-DATE                                             
                                                                        
           SORT F-SORT                                                  
               ON ASCENDING KEY SORT-DATE                               
               INPUT PROCEDURE IS SELECTION                             
               OUTPUT PROCEDURE IS EDITION                              
                                                                        
           DISPLAY 'FIN DU PROGRAMME'                                   
           STOP RUN.                                                    
                                                                        
       LIRE-CLIENT.                                                     
           OPEN INPUT F-CLIENT                                          
           MOVE WS-NUM-RECHERCHE TO CLI-NUM-COMPTE                      
           READ F-CLIENT                                                
               INVALID KEY                                              
                   MOVE 'INCONNU   ' TO WS-NOM-CLIENT                   
                   MOVE '          ' TO WS-PRENOM-CLIENT                
               NOT INVALID KEY                                          
                   MOVE CLI-NOM TO WS-NOM-CLIENT                        
                   MOVE CLI-PRENOM TO WS-PRENOM-CLIENT                  
           END-READ                                                     
           CLOSE F-CLIENT.                                              
                                                                        
       GET-DATE.                                                        
           ACCEPT WS-DATE-JOUR FROM DATE YYYYMMDD                       
           STRING WS-JOUR '/' WS-MOIS '/' WS-ANNEE                      
               DELIMITED BY SIZE INTO WS-DATE-EDIT.                     
                                                                        
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
                                                                        
       EDITION.                                                         
           OPEN OUTPUT F-EDITION                                        
                                                                        
           MOVE WS-NOM-CLIENT     TO WS-ED-NOM                          
           MOVE WS-PRENOM-CLIENT  TO WS-ED-PRENOM                       
           MOVE WS-NUM-RECHERCHE  TO WS-ED-COMPTE                       
           MOVE WS-DATE-EDIT      TO WS-ED-DATE-RELEVE                  
                                                                        
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-TITRE                        
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-VIDE                         
           WRITE ENR-EDITION FROM WS-LIGNE-CLIENT                       
           WRITE ENR-EDITION FROM WS-LIGNE-VIDE                         
           WRITE ENR-EDITION FROM WS-LIGNE-ENTETE                       
           WRITE ENR-EDITION FROM WS-LIGNE-TIRET     
                   
           MOVE 0 TO WS-FIN-SORT                                        
           PERFORM UNTIL WS-FIN-SORT = 1                                
               RETURN F-SORT INTO WS-ENR                                
                   AT END MOVE 1 TO WS-FIN-SORT                         
                   NOT AT END                                           
                       PERFORM ECRIRE-DETAIL                            
               END-RETURN                                               
           END-PERFORM                                                  
                                                                        
           WRITE ENR-EDITION FROM WS-LIGNE-TIRET   
                     
           MOVE WS-TOT-CREDIT TO WS-TOT-ED-CREDIT                       
           MOVE WS-TOT-DEBIT  TO WS-TOT-ED-DEBIT     
                   
           WRITE ENR-EDITION FROM WS-LIGNE-TOTAL                        
           WRITE ENR-EDITION FROM WS-LIGNE-SEP                          
           WRITE ENR-EDITION FROM WS-LIGNE-VIDE                         
           WRITE ENR-EDITION FROM WS-LIGNE-DATE                         
                                                                        
           CLOSE F-EDITION.                                             
                                                                        
       ECRIRE-DETAIL.                                                   
           MOVE WS-DATE    TO WS-DET-DATE                               
           MOVE WS-LIBELLE TO WS-DET-LIBELLE                            
           MOVE ZEROS      TO WS-DET-CREDIT                             
           MOVE ZEROS      TO WS-DET-DEBIT                              
                                                                        
           IF WS-SENS = 'CR'                                            
               MOVE WS-MONTANT TO WS-DET-CREDIT                         
               ADD WS-MONTANT TO WS-TOT-CREDIT                          
           ELSE                                                         
               MOVE WS-MONTANT TO WS-DET-DEBIT                          
               ADD WS-MONTANT TO WS-TOT-DEBIT                           
           END-IF                                                       
                                                                        
           WRITE ENR-EDITION FROM WS-LIGNE-DETAIL.                      
                                                                        
                                                                                                                  
                             