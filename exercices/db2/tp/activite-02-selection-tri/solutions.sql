-- =================================================================
-- ACTIVITE 2 - SELECTION ET TRI DES LIGNES
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Filtrer les donnees avec WHERE
--   - Utiliser les operateurs de comparaison
--   - Maitriser BETWEEN, IN, LIKE, IS NULL
--   - Trier les resultats avec ORDER BY
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Employes gagnant plus de 2850
-- -----------------------------------------------------------------
-- Concepts : WHERE, operateur >
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > 2850;

-- Resultat : 4 lignes (CHARLES, ARSENE, HENRI, BASILE)


-- -----------------------------------------------------------------
-- QUESTION 2
-- Salaire NON compris entre 1500 et 2850
-- -----------------------------------------------------------------
-- Concepts : NOT BETWEEN
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL NOT BETWEEN 1500 AND 2850;

-- Resultat : 10 lignes (SAL < 1500 OU SAL > 2850)


-- -----------------------------------------------------------------
-- QUESTION 3
-- Embauches entre 20/02/2021 et 01/05/2021, tri par date
-- -----------------------------------------------------------------
-- Concepts : BETWEEN avec dates, ORDER BY
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, POSTE, DATE_EMB
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2021-02-20' AND '2021-05-01'
ORDER BY DATE_EMB;

-- Resultat : 4 lignes triees par date croissante


-- -----------------------------------------------------------------
-- QUESTION 4
-- Departements 10 et 30, tri alphabetique
-- -----------------------------------------------------------------
-- Concepts : IN, ORDER BY
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, DEPT_NUM
FROM EMPLOYEE
WHERE DEPT_NUM IN (10, 30)
ORDER BY EMP_NOM;

-- Resultat : 9 lignes triees alphabetiquement


-- -----------------------------------------------------------------
-- QUESTION 5
-- SAL > 1500 ET departement 10 ou 30, avec alias
-- -----------------------------------------------------------------
-- Concepts : AND, IN, alias de colonnes
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM "EMPLOYES", SAL "SALAIRE MENSUEL"
FROM EMPLOYEE
WHERE SAL > 1500
AND DEPT_NUM IN (10, 30);

-- Resultat : 4 lignes (PAUL, ALBERT, GASTON, HENRI)


-- -----------------------------------------------------------------
-- QUESTION 6
-- Employes entres en 2022
-- -----------------------------------------------------------------
-- Concepts : BETWEEN avec annee, ou fonction YEAR()
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

-- Methode 1 : BETWEEN
SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE DATE_EMB BETWEEN '2022-01-01' AND '2022-12-31';

-- Methode 2 : Fonction YEAR()
SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE YEAR(DATE_EMB) = 2022;

-- Resultat : 1 ligne (HECTOR)


-- -----------------------------------------------------------------
-- QUESTION 7
-- Employes sans manager (DIR IS NULL)
-- -----------------------------------------------------------------
-- Concepts : IS NULL
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, POSTE
FROM EMPLOYEE
WHERE DIR IS NULL;

-- IMPORTANT : Ne jamais utiliser = NULL ou <> NULL
-- Toujours utiliser IS NULL ou IS NOT NULL
-- Resultat : HENRI (PDG)


-- -----------------------------------------------------------------
-- QUESTION 8
-- Employes avec commission, tri decroissant
-- -----------------------------------------------------------------
-- Concepts : IS NOT NULL, ORDER BY DESC, tri multiple
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL, COMM
FROM EMPLOYEE
WHERE COMM IS NOT NULL
ORDER BY SAL DESC, COMM DESC;

-- Resultat : 4 lignes (vendeurs avec commission)


-- -----------------------------------------------------------------
-- QUESTION 9
-- 3eme lettre du nom = 'A'
-- -----------------------------------------------------------------
-- Concepts : LIKE avec joker _ (underscore)
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NOM LIKE '__A%';

-- Explication des jokers :
--   _ : exactement 1 caractere
--   % : 0 ou plusieurs caracteres
-- '__A%' = 2 caracteres + A + n'importe quoi
-- Resultat : JEAN, CHARLES


-- -----------------------------------------------------------------
-- QUESTION 10
-- Nom contient 2 'G' ET (dept 30 OU manager 7782)
-- -----------------------------------------------------------------
-- Concepts : LIKE avec %, AND, OR, parentheses
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NOM LIKE '%G%G%'
AND (DEPT_NUM = 30 OR DIR = 7782);

-- ATTENTION aux parentheses : AND est prioritaire sur OR
-- Sans parentheses, le resultat serait different
-- Resultat : GEORGES


-- -----------------------------------------------------------------
-- QUESTION 11
-- AGENT ou ANALYSTE avec salaire different de 1000, 3000, 5000
-- -----------------------------------------------------------------
-- Concepts : IN, NOT IN
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, POSTE, SAL
FROM EMPLOYEE
WHERE POSTE IN ('AGENT', 'ANALYSTE')
AND SAL NOT IN (1000, 3000, 5000);

-- Resultat : 4 lignes (ARTHUR, ANTOINE, FERNAND, HECTOR)


-- -----------------------------------------------------------------
-- QUESTION 12
-- Commission > 10% du salaire
-- -----------------------------------------------------------------
-- Concepts : Expressions arithmetiques dans WHERE
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL, COMM
FROM EMPLOYEE
WHERE COMM > SAL * 1.1;

-- SAL * 1.1 = salaire + 10%
-- Resultat : GEORGES (1250, 1400)


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- Comparaison   : =, <>, <, >, <=, >=
-- Logique       : AND, OR, NOT (attention parentheses!)
-- Plage         : BETWEEN min AND max (bornes incluses)
-- Liste         : IN (val1, val2, ...)
-- Motif         : LIKE ('_' = 1 car, '%' = 0+ car)
-- NULL          : IS NULL, IS NOT NULL (jamais = NULL)
-- Tri           : ORDER BY col [ASC|DESC]
-- =================================================================
