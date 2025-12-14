-- =================================================================
-- ACTIVITE 4 - JOINTURES
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Maitriser les jointures internes (INNER JOIN)
--   - Utiliser les jointures externes (LEFT/RIGHT OUTER JOIN)
--   - Realiser des auto-jointures (self-join)
--   - Combiner plusieurs tables
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Nom, numero de departement et nom du departement
-- -----------------------------------------------------------------
-- Concepts : INNER JOIN, alias de tables
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, E.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM;

-- Resultat : 14 lignes avec nom employe + nom departement


-- -----------------------------------------------------------------
-- QUESTION 2
-- Postes uniques du departement 30
-- -----------------------------------------------------------------
-- Concepts : DISTINCT, WHERE
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT DISTINCT POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = 30;

-- Resultat : 3 postes (AGENT, DIRECTEUR, VENDEUR)


-- -----------------------------------------------------------------
-- QUESTION 3
-- Employes avec commission : nom, departement, localisation
-- -----------------------------------------------------------------
-- Concepts : JOIN + WHERE
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, D.DEPT_NOM, D.LOC
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE E.COMM IS NOT NULL;

-- Resultat : 4 lignes (vendeurs avec commission, tous a LYON)


-- -----------------------------------------------------------------
-- QUESTION 4
-- Employes dont le nom contient 'A'
-- -----------------------------------------------------------------
-- Concepts : JOIN + LIKE
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE E.EMP_NOM LIKE '%A%';

-- Resultat : 10 lignes


-- -----------------------------------------------------------------
-- QUESTION 5
-- Employes bases a STRASBOURG
-- -----------------------------------------------------------------
-- Concepts : JOIN avec filtre sur table jointe
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, E.POSTE, E.DEPT_NUM, D.DEPT_NOM
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
WHERE D.LOC = 'STRASBOURG';

-- Resultat : 5 lignes (departement RECHERCHE)


-- -----------------------------------------------------------------
-- QUESTION 6
-- Auto-jointure : employe et son directeur
-- -----------------------------------------------------------------
-- Concepts : Self-join, alias multiples
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM    AS "EMPLOYES",
       E.EMP_NUM    AS "N EMP.",
       M.EMP_NOM    AS "DIRECTEUR",
       M.EMP_NUM    AS "N DIR."
FROM EMPLOYEE E
JOIN EMPLOYEE M
ON E.DIR = M.EMP_NUM;

-- Explication :
--   E = l'employe
--   M = le manager (directeur)
--   On joint la table sur elle-meme
-- Note : HENRI n'apparait pas (pas de directeur)
-- Resultat : 13 lignes


-- -----------------------------------------------------------------
-- QUESTION 7
-- LEFT JOIN pour inclure les employes sans directeur
-- -----------------------------------------------------------------
-- Concepts : LEFT OUTER JOIN
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM    AS "EMPLOYES",
       E.EMP_NUM    AS "N EMP.",
       M.EMP_NOM    AS "DIRECTEUR",
       M.EMP_NUM    AS "N DIR."
FROM EMPLOYEE E
LEFT OUTER JOIN EMPLOYEE M
ON E.DIR = M.EMP_NUM;

-- LEFT JOIN : garde toutes les lignes de la table de gauche
-- HENRI apparait avec NULL pour DIRECTEUR
-- Resultat : 14 lignes


-- -----------------------------------------------------------------
-- QUESTION 8
-- Nombre de collegues par employe
-- -----------------------------------------------------------------
-- Concepts : Self-join + COUNT + GROUP BY
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.DEPT_NUM      AS "DEPARTEMENT",
       E.EMP_NOM       AS "EMPLOYE",
       COUNT(*)        AS NB_COLLEGUES
FROM EMPLOYEE E
JOIN EMPLOYEE C
ON E.DEPT_NUM = C.DEPT_NUM
WHERE E.EMP_NUM <> C.EMP_NUM
GROUP BY E.DEPT_NUM, E.EMP_NOM;

-- Explication :
--   Jointure sur meme departement
--   Exclusion de l'employe lui-meme
--   Comptage des collegues
-- Resultat : 14 lignes


-- -----------------------------------------------------------------
-- QUESTION 9
-- Jointure sur 3 tables : EMPLOYEE + DEPT + SAL_GRILLE
-- -----------------------------------------------------------------
-- Concepts : Multiple JOIN, BETWEEN dans ON
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, E.POSTE, D.DEPT_NOM,
       E.SAL,
       S.GRADE
FROM EMPLOYEE E
JOIN DEPT D
ON E.DEPT_NUM = D.DEPT_NUM
JOIN SAL_GRILLE S
ON E.SAL BETWEEN S.MIN_SAL AND S.MAX_SAL;

-- Explication :
--   1. EMPLOYEE + DEPT sur DEPT_NUM
--   2. EMPLOYEE + SAL_GRILLE sur plage de salaire
-- Resultat : 14 lignes avec grade calcule


-- -----------------------------------------------------------------
-- QUESTION 10
-- Employes arrives avant JEAN
-- -----------------------------------------------------------------
-- Concepts : Self-join avec comparaison de dates
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, E.DATE_EMB
FROM EMPLOYEE E
JOIN EMPLOYEE J
ON J.DATE_EMB > E.DATE_EMB
WHERE J.EMP_NOM = 'JEAN'
ORDER BY E.DATE_EMB;

-- Explication :
--   J = JEAN (reference)
--   E = autres employes
--   On garde E si sa date < date de JEAN
-- Resultat : 2 lignes (ARTHUR, PAUL)


-- -----------------------------------------------------------------
-- QUESTION 11
-- Employes embauches avant leur directeur
-- -----------------------------------------------------------------
-- Concepts : Self-join avec comparaison de dates
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM    AS "EMPLOYE",
       E.DATE_EMB   AS "DATE EMBAUCHE EMP.",
       D.EMP_NOM    AS "DIRECTEUR",
       D.DATE_EMB   AS "DATE EMBAUCHE DIR."
FROM EMPLOYEE E
JOIN EMPLOYEE D
ON E.DIR = D.EMP_NUM
WHERE E.DATE_EMB < D.DATE_EMB;

-- Resultat : 6 lignes (employes embauches avant leur chef)


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- INNER JOIN    : Correspondances uniquement
-- LEFT JOIN     : Toutes les lignes de gauche + correspondances
-- RIGHT JOIN    : Toutes les lignes de droite + correspondances
-- FULL JOIN     : Toutes les lignes des deux tables
-- Self-join     : Une table avec elle-meme (alias differents)
-- Multiple JOIN : Enchainer les jointures sur plusieurs tables
-- =================================================================
