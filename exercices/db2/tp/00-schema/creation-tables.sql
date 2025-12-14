-- ============================================================================
-- CREATION DES TABLES - Base de données Humania Services
-- Formation DB2/SQL - M2i Formation
-- ============================================================================

-- ============================================================================
-- TABLE DEPT (Départements)
-- ============================================================================

DROP TABLE DEPT;

CREATE TABLE DEPT (
    DEPT_NUM    SMALLINT    NOT NULL,
    DEPT_NOM    VARCHAR(14),
    LOC         VARCHAR(13),
    PRIMARY KEY (DEPT_NUM)
);

-- Insertion des données DEPT
INSERT INTO DEPT VALUES (10, 'COMPTABILITE', 'MARSEILLE');
INSERT INTO DEPT VALUES (20, 'RECHERCHE', 'STRASBOURG');
INSERT INTO DEPT VALUES (30, 'VENTES', 'LYON');
INSERT INTO DEPT VALUES (40, 'EXPLOITATION', 'PARIS');

-- Vérification
SELECT * FROM DEPT;

-- ============================================================================
-- TABLE EMPLOYEE (Employés)
-- ============================================================================

DROP TABLE EMPLOYEE;

CREATE TABLE EMPLOYEE (
    EMP_NUM     SMALLINT    NOT NULL,
    EMP_NOM     VARCHAR(10),
    POSTE       VARCHAR(9),
    DIR         SMALLINT,
    DATE_EMB    DATE,
    SAL         DECIMAL(7,2),
    COMM        DECIMAL(7,2),
    DEPT_NUM    SMALLINT    NOT NULL REFERENCES DEPT(DEPT_NUM),
    PRIMARY KEY (EMP_NUM)
);

-- Insertion des données EMPLOYEE
INSERT INTO EMPLOYEE VALUES
    (7369, 'ARTHUR', 'AGENT', 7902, DATE('2020-12-17'), 800.00, NULL, 20);
INSERT INTO EMPLOYEE VALUES
    (7499, 'PAUL', 'VENDEUR', 7698, DATE('2021-02-20'), 1600.00, 300.00, 30);
INSERT INTO EMPLOYEE VALUES
    (7521, 'JEAN', 'VENDEUR', 7698, DATE('2021-02-22'), 1250.00, 500.00, 30);
INSERT INTO EMPLOYEE VALUES
    (7566, 'CHARLES', 'DIRECTEUR', 7839, DATE('2021-04-02'), 2975.00, NULL, 20);
INSERT INTO EMPLOYEE VALUES
    (7654, 'GEORGES', 'VENDEUR', 7698, DATE('2021-09-28'), 1250.00, 1400.00, 30);
INSERT INTO EMPLOYEE VALUES
    (7698, 'ALBERT', 'DIRECTEUR', 7839, DATE('2021-05-01'), 2850.00, NULL, 30);
INSERT INTO EMPLOYEE VALUES
    (7782, 'GASTON', 'DIRECTEUR', 7839, DATE('2021-06-09'), 2450.00, NULL, 10);
INSERT INTO EMPLOYEE VALUES
    (7788, 'ARSENE', 'ANALYSTE', 7566, DATE('2025-07-13'), 3000.00, NULL, 20);
INSERT INTO EMPLOYEE VALUES
    (7839, 'HENRI', 'PDG', NULL, DATE('2021-11-17'), 5000.00, NULL, 10);
INSERT INTO EMPLOYEE VALUES
    (7844, 'JULES', 'VENDEUR', 7698, DATE('2021-09-08'), 1500.00, 0.00, 30);
INSERT INTO EMPLOYEE VALUES
    (7876, 'ANTOINE', 'AGENT', 7788, DATE('2025-07-13'), 1100.00, NULL, 20);
INSERT INTO EMPLOYEE VALUES
    (7900, 'FERNAND', 'AGENT', 7698, DATE('2021-12-03'), 950.00, NULL, 30);
INSERT INTO EMPLOYEE VALUES
    (7902, 'BASILE', 'ANALYSTE', 7566, DATE('2021-12-03'), 3000.00, NULL, 20);
INSERT INTO EMPLOYEE VALUES
    (7934, 'HECTOR', 'AGENT', 7782, DATE('2022-01-23'), 1300.00, NULL, 10);

-- Vérification
SELECT * FROM EMPLOYEE;

-- ============================================================================
-- TABLE SAL_GRILLE (Grille des salaires)
-- ============================================================================

DROP TABLE SAL_GRILLE;

CREATE TABLE SAL_GRILLE (
    GRADE       SMALLINT    NOT NULL,
    MIN_SAL     DECIMAL(7,2),
    MAX_SAL     DECIMAL(7,2),
    PRIMARY KEY (GRADE)
);

-- Insertion des données SAL_GRILLE
INSERT INTO SAL_GRILLE VALUES (1, 700.00, 1200.00);
INSERT INTO SAL_GRILLE VALUES (2, 1201.00, 1400.00);
INSERT INTO SAL_GRILLE VALUES (3, 1401.00, 2000.00);
INSERT INTO SAL_GRILLE VALUES (4, 2001.00, 3000.00);
INSERT INTO SAL_GRILLE VALUES (5, 3001.00, 9999.00);

-- Vérification
SELECT * FROM SAL_GRILLE;

-- ============================================================================
-- AFFICHAGE COMPLET DES TROIS TABLES
-- ============================================================================

SELECT * FROM DEPT;
SELECT * FROM EMPLOYEE;
SELECT * FROM SAL_GRILLE;
