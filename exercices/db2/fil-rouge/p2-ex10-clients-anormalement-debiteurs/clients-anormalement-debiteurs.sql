-- P2 EXERCICE 10 : Clients anormalement debiteurs par profession

-- Clients debiteurs dont le solde < moyenne des debiteurs de leur profession
SELECT C.NUM_COMPTE, C.NOM_CLIENT, C.PREN_CLIENT,
       P.CODE_PROF, P.LIB_PROF,
       C.SOLDE
FROM CLIENT C
INNER JOIN PROFESSI P ON C.CODE_PROF = P.CODE_PROF
WHERE C.POS = 'DB'
  AND C.SOLDE < (SELECT AVG(C2.SOLDE)
                 FROM CLIENT C2
                 WHERE C2.POS = 'DB'
                   AND C2.CODE_PROF = C.CODE_PROF)
ORDER BY C.CODE_PROF, C.SOLDE;
