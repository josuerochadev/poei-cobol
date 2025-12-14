-- =================================================================
-- ACTIVITE 1 - SELECT ELEMENTAIRE
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Objectifs :
--   - Maitriser la syntaxe de base du SELECT
--   - Utiliser les alias de colonnes
--   - Concatener des colonnes
--   - Eliminer les doublons avec DISTINCT
-- =================================================================


-- -----------------------------------------------------------------
-- QUESTION 1
-- Affichez toutes les donnees de la table EMPLOYEE.
-- -----------------------------------------------------------------
-- Concepts : SELECT *, FROM
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT * FROM EMPLOYEE;

-- Resultat attendu : 14 lignes affichees


-- -----------------------------------------------------------------
-- QUESTION 2
-- Affichez EMP_NUM, EMP_NOM, POSTE, DATE_EMB (matricule en premier)
-- -----------------------------------------------------------------
-- Concepts : Selection de colonnes specifiques, ordre des colonnes
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT EMP_NUM, EMP_NOM, POSTE, DATE_EMB
FROM EMPLOYEE;

-- Resultat attendu :
-- EMP_NUM  EMP_NOM    POSTE       DATE_EMB
-- -------  ---------  ---------   ----------
-- 7369     ARTHUR     AGENT       2020-12-17
-- 7499     PAUL       VENDEUR     2021-02-20
-- ...


-- -----------------------------------------------------------------
-- QUESTION 3
-- Affichez les differents types de poste (valeurs uniques)
-- -----------------------------------------------------------------
-- Concepts : DISTINCT pour eliminer les doublons
-- Difficulte : ★☆☆
-- -----------------------------------------------------------------

SELECT DISTINCT POSTE
FROM EMPLOYEE;

-- Resultat attendu : 5 postes distincts
-- POSTE
-- ---------
-- AGENT
-- ANALYSTE
-- DIRECTEUR
-- PDG
-- VENDEUR


-- -----------------------------------------------------------------
-- QUESTION 4
-- Utilisez des alias de colonnes personnalises
-- -----------------------------------------------------------------
-- Concepts : Alias de colonnes avec guillemets doubles
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NUM    "N° EMP.",
       EMP_NOM    "EMPLOYES",
       POSTE      POSTE,
       DATE_EMB   "DATE EMBAUCHE"
FROM EMPLOYEE;

-- Note : Les guillemets doubles permettent les espaces et
--        caracteres speciaux dans les alias


-- -----------------------------------------------------------------
-- QUESTION 5
-- Concatenez nom et poste avec virgule comme separateur
-- -----------------------------------------------------------------
-- Concepts : Operateur de concatenation ||
-- Difficulte : ★★☆
-- -----------------------------------------------------------------

SELECT EMP_NOM || ', ' || POSTE "EMPLOYES ET POSTES"
FROM EMPLOYEE;

-- Resultat attendu :
-- EMPLOYES ET POSTES
-- ------------------
-- ARTHUR, AGENT
-- PAUL, VENDEUR
-- JEAN, VENDEUR
-- ...


-- -----------------------------------------------------------------
-- QUESTION 6
-- Toutes les donnees dans une seule colonne
-- -----------------------------------------------------------------
-- Concepts : Concatenation multiple, CHAR(), COALESCE()
-- Difficulte : ★★★
-- -----------------------------------------------------------------

-- Version simple (probleme si valeurs NULL) :
SELECT CHAR(EMP_NUM) || ' ' || EMP_NOM || ' ' ||
       POSTE || ' ' || CHAR(DIR) || ' ' ||
       CHAR(DATE_EMB) || ' ' || CHAR(SAL) || ' ' ||
       CHAR(COMM) || ' ' ||
       CHAR(DEPT_NUM) AS "LISTE DES EMPLOYES"
FROM EMPLOYEE;

-- ATTENTION : Si une valeur est NULL (COMM ou DIR),
-- toute la ligne concatenee devient NULL !

-- Version amelioree avec COALESCE (gere les NULL) :
SELECT CHAR(EMP_NUM) || ', ' ||
       EMP_NOM || ', ' ||
       POSTE || ', ' ||
       COALESCE(CHAR(DIR), 'N/A') || ', ' ||
       CHAR(DATE_EMB) || ', ' ||
       CHAR(SAL) || ', ' ||
       COALESCE(CHAR(COMM), '0') || ', ' ||
       CHAR(DEPT_NUM) AS "LISTE DES EMPLOYES"
FROM EMPLOYEE;

-- Note : COALESCE(valeur, valeur_par_defaut) retourne la
--        premiere valeur non NULL


-- =================================================================
-- RESUME DES CONCEPTS
-- =================================================================
-- SELECT *           : Toutes les colonnes
-- SELECT col1, col2  : Colonnes specifiques
-- DISTINCT           : Elimine les doublons
-- "Alias"            : Alias avec espaces/speciaux
-- col1 || col2       : Concatenation
-- COALESCE(a, b)     : Retourne a si non NULL, sinon b
-- CHAR(num)          : Convertit un nombre en caractere
-- =================================================================
