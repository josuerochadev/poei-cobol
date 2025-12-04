       IDENTIFICATION DIVISION.
       PROGRAM-ID. C05-EVALUATE.
       AUTHOR. ROCHA.
      *----------------------------------------------------------------
      * PROGRAMME : EVALUATE avec ALSO
      *             Evaluation notes examen et stage
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------
      * Variables de travail
      *----------------------------------------------------------------
       01  NOTE-EXAM       PIC 99 VALUE 0.
       01  NOTE-STAGE      PIC 99 VALUE 0.
       01  DECISION        PIC X(30) VALUE SPACES.

      *----------------------------------------------------------------
      * Variables d'edition
      *----------------------------------------------------------------
       01  WS-LIGNE-SEP    PIC X(50) VALUE ALL '-'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY '  EVALUATION DES RESULTATS EXAMEN + STAGE'
           DISPLAY '=================================================='
           DISPLAY ' '

           PERFORM 1000-TEST-CAS-1
           PERFORM 2000-TEST-CAS-2
           PERFORM 3000-TEST-CAS-3
           PERFORM 4000-TEST-CAS-4
           PERFORM 5000-TEST-CAS-5

           DISPLAY ' '
           DISPLAY '=================================================='
           STOP RUN.

      *----------------------------------------------------------------
      * Cas 1 : Recu (les deux notes >= 10)
      *----------------------------------------------------------------
       1000-TEST-CAS-1.
           MOVE 15 TO NOTE-EXAM
           MOVE 14 TO NOTE-STAGE
           PERFORM 9000-EVALUER
           DISPLAY 'Cas 1 - Exam=' NOTE-EXAM ' Stage=' NOTE-STAGE
               ' => ' DECISION.

      *----------------------------------------------------------------
      * Cas 2 : Rattrape par le stage
      *----------------------------------------------------------------
       2000-TEST-CAS-2.
           MOVE 9 TO NOTE-EXAM
           MOVE 16 TO NOTE-STAGE
           PERFORM 9000-EVALUER
           DISPLAY 'Cas 2 - Exam=' NOTE-EXAM ' Stage=' NOTE-STAGE
               ' => ' DECISION.

      *----------------------------------------------------------------
      * Cas 3 : Rattrape par les notes examen
      *----------------------------------------------------------------
       3000-TEST-CAS-3.
           MOVE 18 TO NOTE-EXAM
           MOVE 9 TO NOTE-STAGE
           PERFORM 9000-EVALUER
           DISPLAY 'Cas 3 - Exam=' NOTE-EXAM ' Stage=' NOTE-STAGE
               ' => ' DECISION.

      *----------------------------------------------------------------
      * Cas 4 : Elimine
      *----------------------------------------------------------------
       4000-TEST-CAS-4.
           MOVE 8 TO NOTE-EXAM
           MOVE 7 TO NOTE-STAGE
           PERFORM 9000-EVALUER
           DISPLAY 'Cas 4 - Exam=' NOTE-EXAM ' Stage=' NOTE-STAGE
               ' => ' DECISION.

      *----------------------------------------------------------------
      * Cas 5 : Limite (juste 10 aux deux)
      *----------------------------------------------------------------
       5000-TEST-CAS-5.
           MOVE 10 TO NOTE-EXAM
           MOVE 10 TO NOTE-STAGE
           PERFORM 9000-EVALUER
           DISPLAY 'Cas 5 - Exam=' NOTE-EXAM ' Stage=' NOTE-STAGE
               ' => ' DECISION.

      *----------------------------------------------------------------
      * Paragraphe d'evaluation avec EVALUATE ALSO
      *----------------------------------------------------------------
       9000-EVALUER.
           INITIALIZE DECISION

           EVALUATE NOTE-EXAM ALSO NOTE-STAGE
               WHEN 10 THRU 20 ALSO 10 THRU 20
                   MOVE 'RECU' TO DECISION
               WHEN 9 THRU 10 ALSO 12 THRU 20
                   MOVE 'RATTRAPE PAR LE STAGE' TO DECISION
               WHEN 14 THRU 20 ALSO 9 THRU 10
                   MOVE 'RATTRAPE PAR NOTES EXAM' TO DECISION
               WHEN OTHER
                   MOVE 'ELIMINE' TO DECISION
           END-EVALUATE.
