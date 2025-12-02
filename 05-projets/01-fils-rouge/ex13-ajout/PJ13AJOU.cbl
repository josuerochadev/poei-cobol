       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ13AJOU.                                            
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * AJOUT D'UN CLIENT DANS LE DATA SET CLIENT KSDS                  
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-CLIENT ASSIGN TO FCLIENT                           
                ORGANIZATION IS INDEXED                                 
                ACCESS MODE IS RANDOM                                   
                RECORD KEY IS CLI-NUM-COMPTE                            
                FILE STATUS IS WS-FS-CLIENT.                            
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-CLIENT.                                                     
       01 ENR-CLIENT.                                                   
         05 CLI-NUM-COMPTE PIC 9(3).                                    
         05 CLI-CODE-REGION PIC 9(2).                                   
         05 CLI-NATURE-COMPTE PIC 9(2).                                 
         05 CLI-NOM PIC X(10).                                          
         05 CLI-PRENOM PIC X(10).                                       
         05 CLI-DATE-NAISSANCE PIC 9(8).                                
         05 CLI-SEXE PIC X(1).                                          
         05 CLI-ACTIVITE PIC 9(2).                                      
         05 CLI-SITUATION PIC X(1).                                     
         05 CLI-SOLDE PIC 9(10).                                        
         05 CLI-POSITION PIC X(2).                                      
         05 FILLER PIC X(29).                                           
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-CLIENT PIC X(2).                                        
           88 FS-OK VALUE '00'.                                         
           88 FS-DUPKEY VALUE '22'.                                     
           88 FS-NOTFND VALUE '23'.                                     
                                                                        
      * NOUVEAU CLIENT à AJOUTER                                        
       01 WS-NOUVEAU-CLIENT.                                            
         05 WS-NUM-COMPTE PIC 9(3).                                     
         05 WS-CODE-REGION PIC 9(2).                                    
         05 WS-NATURE-COMPTE PIC 9(2).                                  
         05 WS-NOM PIC X(10).                                           
         05 WS-PRENOM PIC X(10).                                        
         05 WS-DATE-NAISSANCE PIC 9(8).                                 
         05 WS-SEXE PIC X(1).                                           
         05 WS-ACTIVITE PIC 9(2).                                       
         05 WS-SITUATION PIC X(1).                                      
         05 WS-SOLDE PIC 9(10).                                         
         05 WS-POSITION PIC X(2).                                       
                                                                        
       01 WS-MSG PIC X(50).                                             
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            PERFORM INIT                                                
            PERFORM AJOUTER-CLIENT                                      
            PERFORM FIN                                                 
            STOP RUN.                                                   
                                                                        
       INIT.                                                            
            OPEN I-O F-CLIENT                                           
            IF NOT FS-OK                                                
                DISPLAY 'ERREUR OVERTURE KSDS : ' WS-FS-CLIENT          
                STOP RUN                                                
            END-IF.                                                     
                                                                        
       AJOUTER-CLIENT.                                                  
      * INITIALISER LES DONNEES DU NOUVEAU CLIENT                       
            INITIALIZE WS-NOUVEAU-CLIENT                                
                                                                        
            MOVE 021 TO WS-NUM-COMPTE                                   
            MOVE 05 TO WS-CODE-REGION                                   
            MOVE 50 TO WS-NATURE-COMPTE                                 
            MOVE 'NOUVEAUNOM' TO WS-NOM                                 
            MOVE 'NOUVEAUPRN' TO WS-PRENOM                              
            MOVE 19990115 TO WS-DATE-NAISSANCE                          
            MOVE 'M' TO WS-SEXE                                         
            MOVE 10 TO WS-ACTIVITE                                      
            MOVE 'C' TO WS-SITUATION                                    
            MOVE 0000050000 TO WS-SOLDE                                 
            MOVE 'CR' TO WS-POSITION                                    
                                                                        
      * TRANSFERER VERS L'ENREGISTREMENT DU FICHIER                     
            MOVE WS-NUM-COMPTE TO CLI-NUM-COMPTE                        
            MOVE WS-CODE-REGION TO CLI-CODE-REGION                      
            MOVE WS-NATURE-COMPTE TO CLI-NATURE-COMPTE                  
            MOVE WS-NOM TO CLI-NOM                                      
            MOVE WS-PRENOM TO CLI-PRENOM                                
            MOVE WS-DATE-NAISSANCE TO CLI-DATE-NAISSANCE                
            MOVE WS-SEXE TO CLI-SEXE                                    
            MOVE WS-ACTIVITE TO CLI-ACTIVITE                            
            MOVE WS-SITUATION TO CLI-SITUATION                          
            MOVE WS-SOLDE TO CLI-SOLDE                                  
            MOVE WS-POSITION TO CLI-POSITION                            
                                                                        
      * ECRIRE DANS LE KSDS                                             
            WRITE ENR-CLIENT                                            
                                                                        
            EVALUATE TRUE                                               
              WHEN FS-OK                                                
                 DISPLAY 'CLIENT AJOUTé AVEC SUCCéS'                    
                 DISPLAY 'NUMERO COMPTE : ' CLI-NUM-COMPTE              
              WHEN FS-DUPKEY                                            
                 DISPLAY 'ERREUR : KEY DUPLIQUEE - CLIENT DEJA EXISTANT'
              WHEN OTHER                                                
                 DISPLAY 'ERREUR ECRITURE : ' WS-FS-CLIENT              
            END-EVALUATE.                                               
                                                                        
       FIN.                                                             
            CLOSE F-CLIENT.                                             

