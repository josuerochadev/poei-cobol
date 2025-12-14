-- =================================================================
-- ACTIVITE 6 - FONCTIONS DE GROUPE
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Maitriser les fonctions d'agregation
--   - Utiliser GROUP BY pour regrouper les donnees
--   - Filtrer les groupes avec HAVING
--   - Combiner agregations et jointures
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Statistiques globales des salaires
-- -----------------------------------------------------------------
-- Concepts : MAX, MIN, SUM, AVG, ROUND, alias
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT ROUND(MAX(SAL), 0) AS "MAXIMUM",
       ROUND(MIN(SAL), 0) AS "MINIMUM",
       ROUND(SUM(SAL), 0) AS "SOMME",
       ROUND(AVG(SAL), 0) AS "MOYENNE"
FROM EMPLOYEE;

-- Resultat :
--   MAXIMUM: 5000
--   MINIMUM: 800
--   SOMME: 29025
--   MOYENNE: 2073


-- -----------------------------------------------------------------
-- QUESTION 2
-- Statistiques par poste
-- -----------------------------------------------------------------
-- Concepts : GROUP BY
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT POSTE,
       ROUND(MAX(SAL), 0) AS "MAXIMUM",
       ROUND(MIN(SAL), 0) AS "MINIMUM",
       ROUND(SUM(SAL), 0) AS "SOMME",
       ROUND(AVG(SAL), 0) AS "MOYENNE"
FROM EMPLOYEE
GROUP BY POSTE;

-- Resultat : 5 lignes (une par poste)
-- POSTE       MAXIMUM  MINIMUM  SOMME   MOYENNE
-- AGENT       1300     800      4150    1038
-- ANALYSTE    3000     3000     6000    3000
-- DIRECTEUR   2975     2450     8275    2758
-- PDG         5000     5000     5000    5000
-- VENDEUR     1600     1250     5600    1400


-- -----------------------------------------------------------------
-- QUESTION 3
-- Nombre de personnes par poste
-- -----------------------------------------------------------------
-- Concepts : COUNT(*), GROUP BY
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT POSTE,
       COUNT(*) AS "NOMBRE"
FROM EMPLOYEE
GROUP BY POSTE;

-- Resultat :
--   AGENT: 4, ANALYSTE: 2, DIRECTEUR: 3, PDG: 1, VENDEUR: 4


-- -----------------------------------------------------------------
-- QUESTION 4
-- Nombre de chefs (personnes ayant des subordonnes)
-- -----------------------------------------------------------------
-- Concepts : COUNT(DISTINCT)
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT COUNT(DISTINCT DIR) AS "NOMBRE DE CHEFS"
FROM EMPLOYEE;

-- Alternative avec WHERE pour eviter le warning SQLSTATE 01003 :
SELECT COUNT(DISTINCT DIR) AS "NOMBRE DE CHEFS"
FROM EMPLOYEE
WHERE DIR IS NOT NULL;

-- Note : COUNT(colonne) ignore les NULL automatiquement
-- Resultat : 6


-- -----------------------------------------------------------------
-- QUESTION 5
-- Difference entre salaire max et min
-- -----------------------------------------------------------------
-- Concepts : Expression avec fonctions d'agregation
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT MAX(SAL) - MIN(SAL) AS DIFFERENCE
FROM EMPLOYEE;

-- Resultat : 4200 (5000 - 800)


-- -----------------------------------------------------------------
-- QUESTION 6
-- Salaire minimum par directeur (avec filtres)
-- -----------------------------------------------------------------
-- Concepts : WHERE vs HAVING, GROUP BY, ORDER BY
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT DIR,
       MIN(SAL) AS MIN_SALAIRE
FROM EMPLOYEE
WHERE DIR IS NOT NULL          -- Filtre AVANT regroupement
GROUP BY DIR
HAVING MIN(SAL) > 1000         -- Filtre APRES regroupement
ORDER BY MIN_SALAIRE DESC;

-- IMPORTANT :
--   WHERE : filtre les LIGNES avant GROUP BY
--   HAVING : filtre les GROUPES apres GROUP BY
-- Resultat :
--   DIR   MIN_SALAIRE
--   7566  3000
--   7839  2450
--   7782  1300
--   7788  1100


-- -----------------------------------------------------------------
-- QUESTION 7
-- Statistiques par departement avec jointure
-- -----------------------------------------------------------------
-- Concepts : JOIN + GROUP BY + fonctions d'agregation
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT D.DEPT_NOM       AS "DEPARTEMENT",
       D.LOC            AS "LOCALISATION",
       COUNT(*)         AS "NOMBRE D'EMPLOYES",
       ROUND(AVG(E.SAL), 0) AS "SALAIRE"
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
GROUP BY D.DEPT_NOM, D.LOC;

-- Note : Le dept EXPLOITATION (40) n'apparait pas
--        car il n'a pas d'employes (INNER JOIN)
-- Resultat :
--   DEPARTEMENT   LOCALISATION  NB_EMP  SALAIRE
--   COMPTABILITE  MARSEILLE     3       2917
--   RECHERCHE     STRASBOURG    5       2175
--   VENTES        LYON          6       1567


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- Fonctions :
--   COUNT(*)   : Nombre de lignes
--   COUNT(col) : Nombre de valeurs non NULL
--   SUM(col)   : Somme
--   AVG(col)   : Moyenne
--   MIN(col)   : Minimum
--   MAX(col)   : Maximum
--
-- GROUP BY :
--   Regroupe les lignes par valeurs identiques
--   Toutes colonnes non agregees doivent etre dans GROUP BY
--
-- HAVING :
--   Filtre les GROUPES (apres GROUP BY)
--   WHERE filtre les LIGNES (avant GROUP BY)
--
-- Ordre d'execution :
--   FROM -> WHERE -> GROUP BY -> HAVING -> SELECT -> ORDER BY
--
-- NULL et agregations :
--   Les fonctions ignorent les NULL (sauf COUNT(*))
-- =================================================================
