       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PJ14MAIN.                                            
      *================================================================*
      * TROISIEME PARTIE MàJ DES DONNéES ET PROGRAMMATION COBOL         
      * PROGRAMME PRINCIPAL - EDITION DES 3 TABLES                      
      *================================================================*
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
            SELECT F-EDITION ASSIGN TO FEDITION                         
                FILE STATUS IS WS-FS-EDITION.                           
                                                                        
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
       FD F-EDITION.                                                    
       01 ENR-EDITION PIC X(80).                                        
                                                                        
       WORKING-STORAGE SECTION.                                         
       01 WS-FS-EDITION PIC X(2).                                       
                                                                        
       PROCEDURE DIVISION.                                              
                                                                        
       PRINCIPAL.                                                       
            OPEN OUTPUT F-EDITION                                       
            CLOSE F-EDITION                                             
                                                                        
            CALL 'PJ14REG'                                              
            CALL 'PJ14CPT'                                              
            CALL 'PJ14PRO'                                              
                                                                        
            STOP RUN.                                                   
