-- =================================================================
-- ACTIVITE 5 - OPERATEURS ENSEMBLISTES
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Maitriser UNION et UNION ALL
--   - Utiliser INTERSECT pour les intersections
--   - Appliquer EXCEPT (MINUS) pour les differences
--   - Combiner des ensembles de resultats
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Departement sans employe
-- -----------------------------------------------------------------
-- Concepts : EXCEPT
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT DEPT_NUM, DEPT_NOM
FROM DEPT
EXCEPT
SELECT D.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E, DEPT D
WHERE E.DEPT_NUM = D.DEPT_NUM;

-- Explication :
--   Tous les departements
--   MOINS ceux qui ont des employes
-- Resultat : EXPLOITATION (40)


-- -----------------------------------------------------------------
-- QUESTION 2
-- Postes attribues en S2 2021 ET S2 2025
-- -----------------------------------------------------------------
-- Concepts : INTERSECT
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT POSTE
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2021-07-01' AND '2021-12-31'
INTERSECT
SELECT POSTE
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2025-07-01' AND '2025-12-31';

-- Explication :
--   INTERSECT = lignes COMMUNES aux deux requetes
--   Postes presents dans les 2 periodes
-- Resultat : AGENT, ANALYSTE


-- -----------------------------------------------------------------
-- QUESTION 3
-- Postes des departements 10, 20, 30
-- -----------------------------------------------------------------
-- Concepts : UNION, ORDER BY
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 10
UNION
SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 20
UNION
SELECT POSTE, DEPT_NUM FROM EMPLOYEE WHERE DEPT_NUM = 30
ORDER BY DEPT_NUM;

-- Note :
--   UNION elimine les doublons
--   ORDER BY s'applique au resultat final
-- Resultat : 9 lignes (postes par departement)


-- -----------------------------------------------------------------
-- QUESTION 4
-- Departements sans ANALYSTE
-- -----------------------------------------------------------------
-- Concepts : EXCEPT
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT DEPT_NUM
FROM DEPT
EXCEPT
SELECT DEPT_NUM
FROM EMPLOYEE
WHERE POSTE = 'ANALYSTE';

-- Explication :
--   Tous les departements
--   MOINS ceux avec des analystes (dept 20)
-- Resultat : 10, 30, 40


-- -----------------------------------------------------------------
-- QUESTION 5
-- Difference symetrique : postes uniques a un departement
-- -----------------------------------------------------------------
-- Concepts : UNION + EXCEPT + INTERSECT
-- Difficulte : ★★★
-- -----------------------------------------------------------------

-- Postes dans 10 OU 20, mais PAS dans les deux
(
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 10
UNION
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 20
)
EXCEPT
(
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 10
INTERSECT
SELECT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 20
);

-- Explication :
--   (Postes du 10 UNION Postes du 20) = tous les postes
--   (Postes du 10 INTERSECT Postes du 20) = postes communs
--   Difference = postes uniquement dans l'un ou l'autre
-- Postes communs (exclus) : AGENT, DIRECTEUR
-- Resultat : ANALYSTE (dept 20), PDG (dept 10)


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- UNION       : A + B sans doublons
-- UNION ALL   : A + B avec doublons (plus rapide)
-- INTERSECT   : A et B (communs)
-- EXCEPT      : A - B (dans A mais pas dans B)
--
-- Regles :
--   - Meme nombre de colonnes
--   - Types compatibles
--   - ORDER BY uniquement a la fin
--
-- Diagramme :
--   A = {1,2,3}    B = {2,3,4}
--   UNION:     {1,2,3,4}
--   INTERSECT: {2,3}
--   A EXCEPT B: {1}
--   B EXCEPT A: {4}
-- =================================================================
