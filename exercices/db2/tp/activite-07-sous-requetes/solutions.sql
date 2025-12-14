-- =================================================================
-- ACTIVITE 7 - SOUS-REQUETES
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Maitriser les sous-requetes simples
--   - Utiliser les sous-requetes multi-colonnes
--   - Appliquer les sous-requetes correlees (synchronisees)
--   - Combiner avec IN, ANY, ALL, EXISTS
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Employes du meme departement que JEAN
-- -----------------------------------------------------------------
-- Concepts : Sous-requete scalaire
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, DATE_EMB
FROM EMPLOYEE
WHERE DEPT_NUM = (SELECT DEPT_NUM FROM EMPLOYEE WHERE EMP_NOM = 'JEAN')
AND EMP_NOM <> 'JEAN';

-- La sous-requete retourne le DEPT_NUM de JEAN (30)


-- -----------------------------------------------------------------
-- QUESTION 2
-- Employes gagnant plus que la moyenne
-- -----------------------------------------------------------------
-- Concepts : Sous-requete avec AVG
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NUM, EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT AVG(SAL) FROM EMPLOYEE)
ORDER BY SAL DESC;

-- La sous-requete calcule le salaire moyen (2073.21)


-- -----------------------------------------------------------------
-- QUESTION 3
-- Employes dans les departements ayant un employe avec 'T'
-- -----------------------------------------------------------------
-- Concepts : IN avec sous-requete
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NUM, EMP_NOM
FROM EMPLOYEE
WHERE DEPT_NUM IN (
    SELECT DEPT_NUM
    FROM EMPLOYEE
    WHERE EMP_NOM LIKE '%T%'
);

-- IN car la sous-requete peut retourner plusieurs valeurs


-- -----------------------------------------------------------------
-- QUESTION 4
-- Combinaison de deux conditions avec sous-requetes
-- -----------------------------------------------------------------
-- Concepts : Multiple sous-requetes avec AND
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NUM, EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > (SELECT AVG(SAL) FROM EMPLOYEE)
AND DEPT_NUM IN (
    SELECT DEPT_NUM
    FROM EMPLOYEE
    WHERE EMP_NOM LIKE '%T%'
);


-- -----------------------------------------------------------------
-- QUESTION 5
-- Employes du departement situe a STRASBOURG
-- -----------------------------------------------------------------
-- Concepts : Sous-requete sur autre table
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, DEPT_NUM, POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = (
    SELECT DEPT_NUM
    FROM DEPT
    WHERE LOC = 'STRASBOURG'
);

-- Alternative avec jointure :
-- SELECT E.EMP_NOM, E.DEPT_NUM, E.POSTE
-- FROM EMPLOYEE E JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM
-- WHERE D.LOC = 'STRASBOURG';


-- -----------------------------------------------------------------
-- QUESTION 6
-- Employes dont le directeur est ALBERT
-- -----------------------------------------------------------------
-- Concepts : Sous-requete pour trouver le matricule
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE DIR = (SELECT EMP_NUM FROM EMPLOYEE WHERE EMP_NOM = 'ALBERT');


-- -----------------------------------------------------------------
-- QUESTION 7
-- Employes du departement VENTES
-- -----------------------------------------------------------------
-- Concepts : Sous-requete sur DEPT_NOM
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT DEPT_NUM, EMP_NOM, POSTE
FROM EMPLOYEE
WHERE DEPT_NUM = (
    SELECT DEPT_NUM
    FROM DEPT
    WHERE DEPT_NOM = 'VENTES'
);


-- -----------------------------------------------------------------
-- QUESTION 8
-- Salaire superieur a TOUS les AGENTS
-- -----------------------------------------------------------------
-- Concepts : > ALL
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL
FROM EMPLOYEE
WHERE SAL > ALL (SELECT SAL FROM EMPLOYEE WHERE POSTE = 'AGENT')
ORDER BY SAL DESC;

-- > ALL = superieur a toutes les valeurs = > MAX
-- Salaires AGENTS : 800, 1100, 950, 1300 -> SAL > 1300

-- Alternative avec MAX :
-- WHERE SAL > (SELECT MAX(SAL) FROM EMPLOYEE WHERE POSTE = 'AGENT')


-- -----------------------------------------------------------------
-- QUESTION 9
-- Sous-requete multi-colonnes
-- -----------------------------------------------------------------
-- Concepts : IN avec tuple (col1, col2)
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NOM, DEPT_NUM, SAL
FROM EMPLOYEE
WHERE (DEPT_NUM, SAL) IN (
    SELECT DEPT_NUM, SAL
    FROM EMPLOYEE
    WHERE COMM IS NOT NULL
);

-- On compare un tuple avec une liste de tuples


-- -----------------------------------------------------------------
-- QUESTION 10
-- Correspondance salaire ET commission
-- -----------------------------------------------------------------
-- Concepts : Sous-requete multi-colonnes avec COALESCE
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NOM, D.DEPT_NOM, E.SAL
FROM EMPLOYEE E
JOIN DEPT D ON E.DEPT_NUM = D.DEPT_NUM
WHERE (E.SAL, COALESCE(E.COMM, 0)) IN (
    SELECT E2.SAL, COALESCE(E2.COMM, 0)
    FROM EMPLOYEE E2
    JOIN DEPT D2 ON E2.DEPT_NUM = D2.DEPT_NUM
    WHERE D2.LOC = 'STRASBOURG'
);

-- COALESCE gere les NULL dans les comparaisons


-- -----------------------------------------------------------------
-- QUESTION 11
-- Les 3 meilleurs salaires
-- -----------------------------------------------------------------
-- Concepts : Sous-requete correlee OU FETCH FIRST
-- Difficulte : ★★★
-- -----------------------------------------------------------------

-- Methode 1 : Sous-requete correlee
SELECT EMP_NOM, SAL
FROM EMPLOYEE E1
WHERE 3 > (
    SELECT COUNT(DISTINCT SAL)
    FROM EMPLOYEE E2
    WHERE E2.SAL > E1.SAL
)
ORDER BY SAL DESC;

-- Explication : On compte les salaires superieurs
--               Si < 3, on fait partie du top 3

-- Methode 2 : DB2 FETCH FIRST (plus simple)
SELECT EMP_NOM, SAL
FROM EMPLOYEE
ORDER BY SAL DESC
FETCH FIRST 3 ROWS ONLY;


-- -----------------------------------------------------------------
-- QUESTION 12
-- Employes qui ne sont pas responsables
-- -----------------------------------------------------------------
-- Concepts : NOT IN, NOT EXISTS
-- Difficulte : ★★★
-- -----------------------------------------------------------------

-- Methode 1 : NOT IN
SELECT EMP_NOM
FROM EMPLOYEE
WHERE EMP_NUM NOT IN (
    SELECT DISTINCT DIR
    FROM EMPLOYEE
    WHERE DIR IS NOT NULL
);

-- Methode 2 : NOT EXISTS
SELECT EMP_NOM
FROM EMPLOYEE E1
WHERE NOT EXISTS (
    SELECT 1
    FROM EMPLOYEE E2
    WHERE E2.DIR = E1.EMP_NUM
);


-- -----------------------------------------------------------------
-- QUESTION 13
-- Salaire superieur a la moyenne de son departement
-- -----------------------------------------------------------------
-- Concepts : Sous-requete correlee
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E.EMP_NUM, E.SAL, E.DEPT_NUM,
       (SELECT AVG(SAL) FROM EMPLOYEE WHERE DEPT_NUM = E.DEPT_NUM) AS SAL_MOY_DEPT
FROM EMPLOYEE E
WHERE E.SAL > (
    SELECT AVG(SAL)
    FROM EMPLOYEE
    WHERE DEPT_NUM = E.DEPT_NUM
)
ORDER BY SAL_MOY_DEPT;

-- La sous-requete fait reference a E.DEPT_NUM (correlation)
-- Re-executee pour chaque ligne de la requete principale


-- -----------------------------------------------------------------
-- QUESTION 14
-- Salaire inferieur a la moitie de la moyenne departementale
-- -----------------------------------------------------------------
-- Concepts : Sous-requete correlee avec expression
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT EMP_NOM, SAL, DEPT_NUM
FROM EMPLOYEE E
WHERE SAL < (
    SELECT AVG(SAL) / 2
    FROM EMPLOYEE
    WHERE DEPT_NUM = E.DEPT_NUM
);


-- -----------------------------------------------------------------
-- QUESTION 15
-- Collegues embauches apres avec meilleur salaire
-- -----------------------------------------------------------------
-- Concepts : EXISTS avec conditions multiples
-- Difficulte : ★★★
-- -----------------------------------------------------------------

SELECT E1.EMP_NOM, E1.DATE_EMB, E1.SAL, E1.DEPT_NUM
FROM EMPLOYEE E1
WHERE EXISTS (
    SELECT 1
    FROM EMPLOYEE E2
    WHERE E2.DEPT_NUM = E1.DEPT_NUM
    AND E2.EMP_NUM <> E1.EMP_NUM
    AND E2.DATE_EMB > E1.DATE_EMB
    AND E2.SAL > E1.SAL
);

-- EXISTS retourne TRUE si la sous-requete a au moins une ligne


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- Sous-requete simple (1 valeur) :
--   WHERE col = (SELECT ...)
--   Utiliser =, <, >, <=, >=
--
-- Sous-requete multi-valeurs :
--   WHERE col IN (SELECT ...)
--   WHERE col NOT IN (SELECT ...)
--
-- Sous-requete multi-colonnes :
--   WHERE (col1, col2) IN (SELECT col1, col2 FROM ...)
--
-- Operateurs ANY / ALL :
--   > ANY : superieur a au moins une valeur
--   > ALL : superieur a toutes les valeurs
--
-- EXISTS / NOT EXISTS :
--   WHERE EXISTS (SELECT 1 FROM ... WHERE ...)
--   Retourne TRUE si resultat non vide
--
-- Sous-requete correlee :
--   Reference une colonne de la requete externe
--   Re-executee pour chaque ligne
-- =================================================================
