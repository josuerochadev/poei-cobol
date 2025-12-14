-- =================================================================
-- EXERCICES DML - INSERT, UPDATE, DELETE
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Ces exercices pratiquent les commandes de manipulation de donnees
-- non couvertes dans les TP de la formatrice.
-- =================================================================


-- =================================================================
-- PREPARATION : Creer les tables de test
-- =================================================================

CREATE TABLE CLIENT (
    CLI_ID      INTEGER NOT NULL PRIMARY KEY,
    CLI_NOM     VARCHAR(50) NOT NULL,
    CLI_VILLE   VARCHAR(30),
    CLI_CREDIT  DECIMAL(10,2) DEFAULT 0
);

CREATE TABLE FACTURE (
    FAC_ID      INTEGER NOT NULL PRIMARY KEY,
    FAC_DATE    DATE DEFAULT CURRENT_DATE,
    CLI_ID      INTEGER NOT NULL,
    MONTANT     DECIMAL(10,2),
    STATUT      CHAR(1) DEFAULT 'N',  -- N=Nouvelle, P=Payee, A=Annulee
    FOREIGN KEY (CLI_ID) REFERENCES CLIENT(CLI_ID)
);


-- =================================================================
-- EXERCICE 1 : INSERT simple
-- =================================================================
-- Objectif : Inserer 3 clients :
--   1, 'DUPONT', 'PARIS', 5000
--   2, 'MARTIN', 'LYON', 3000
--   3, 'BERNARD', 'PARIS', 0
-- =================================================================

-- Votre solution :



-- Solution :
/*
INSERT INTO CLIENT (CLI_ID, CLI_NOM, CLI_VILLE, CLI_CREDIT)
VALUES (1, 'DUPONT', 'PARIS', 5000);

INSERT INTO CLIENT (CLI_ID, CLI_NOM, CLI_VILLE, CLI_CREDIT)
VALUES (2, 'MARTIN', 'LYON', 3000);

INSERT INTO CLIENT (CLI_ID, CLI_NOM, CLI_VILLE, CLI_CREDIT)
VALUES (3, 'BERNARD', 'PARIS', 0);
*/


-- =================================================================
-- EXERCICE 2 : INSERT avec valeurs par defaut
-- =================================================================
-- Objectif : Inserer une facture pour le client 1, montant 1500
--            en laissant FAC_DATE et STATUT prendre leurs defauts
-- =================================================================

-- Votre solution :



-- Solution :
/*
INSERT INTO FACTURE (FAC_ID, CLI_ID, MONTANT)
VALUES (100, 1, 1500);
*/


-- =================================================================
-- EXERCICE 3 : INSERT depuis un SELECT
-- =================================================================
-- Objectif : Creer une table CLIENT_PARIS et y copier les clients
--            dont la ville est PARIS
-- =================================================================

-- Votre solution :



-- Solution :
/*
CREATE TABLE CLIENT_PARIS (
    CLI_ID      INTEGER,
    CLI_NOM     VARCHAR(50),
    CLI_CREDIT  DECIMAL(10,2)
);

INSERT INTO CLIENT_PARIS (CLI_ID, CLI_NOM, CLI_CREDIT)
SELECT CLI_ID, CLI_NOM, CLI_CREDIT
FROM CLIENT
WHERE CLI_VILLE = 'PARIS';
*/


-- =================================================================
-- EXERCICE 4 : UPDATE simple
-- =================================================================
-- Objectif : Augmenter le credit du client 3 de 1000
-- =================================================================

-- Votre solution :



-- Solution :
/*
UPDATE CLIENT
SET CLI_CREDIT = CLI_CREDIT + 1000
WHERE CLI_ID = 3;
*/


-- =================================================================
-- EXERCICE 5 : UPDATE avec condition multiple
-- =================================================================
-- Objectif : Mettre le statut a 'P' (Payee) pour toutes les factures
--            dont le montant est inferieur a 2000
-- =================================================================

-- Votre solution :



-- Solution :
/*
UPDATE FACTURE
SET STATUT = 'P'
WHERE MONTANT < 2000;
*/


-- =================================================================
-- EXERCICE 6 : UPDATE avec sous-requete
-- =================================================================
-- Objectif : Augmenter de 10% le credit des clients ayant des
--            factures payees (STATUT = 'P')
-- =================================================================

-- Votre solution :



-- Solution :
/*
UPDATE CLIENT
SET CLI_CREDIT = CLI_CREDIT * 1.1
WHERE CLI_ID IN (
    SELECT CLI_ID
    FROM FACTURE
    WHERE STATUT = 'P'
);
*/


-- =================================================================
-- EXERCICE 7 : DELETE avec condition
-- =================================================================
-- Objectif : Supprimer toutes les factures annulees (STATUT = 'A')
-- =================================================================

-- Votre solution :



-- Solution :
/*
DELETE FROM FACTURE
WHERE STATUT = 'A';
*/


-- =================================================================
-- EXERCICE 8 : DELETE avec sous-requete
-- =================================================================
-- Objectif : Supprimer les clients n'ayant aucune facture
-- =================================================================

-- Votre solution :



-- Solution :
/*
DELETE FROM CLIENT
WHERE CLI_ID NOT IN (
    SELECT DISTINCT CLI_ID
    FROM FACTURE
);
*/


-- =================================================================
-- EXERCICE 9 : Transactions
-- =================================================================
-- Objectif : Dans une transaction :
--   1. Inserer une facture de 2000 pour le client 1
--   2. Reduire le credit du client 1 de 2000
--   3. Valider la transaction
-- =================================================================

-- Votre solution :



-- Solution :
/*
-- Debut de transaction (implicite ou BEGIN TRANSACTION selon SGBD)
INSERT INTO FACTURE (FAC_ID, CLI_ID, MONTANT, STATUT)
VALUES (101, 1, 2000, 'N');

UPDATE CLIENT
SET CLI_CREDIT = CLI_CREDIT - 2000
WHERE CLI_ID = 1;

COMMIT;  -- Valide la transaction
*/


-- =================================================================
-- EXERCICE 10 : Annulation de transaction
-- =================================================================
-- Objectif : Tenter une modification puis l'annuler
--   1. Supprimer tous les clients
--   2. Annuler (ROLLBACK)
-- =================================================================

-- Votre solution :



-- Solution :
/*
DELETE FROM CLIENT;  -- Attention : supprime tout !
-- Oups, erreur !
ROLLBACK;  -- Annule tout depuis le dernier COMMIT
*/


-- =================================================================
-- NETTOYAGE
-- =================================================================
/*
DROP TABLE CLIENT_PARIS;
DROP TABLE FACTURE;
DROP TABLE CLIENT;
*/


-- =================================================================
-- RESUME DES COMMANDES DML
-- =================================================================
-- INSERT INTO table (cols) VALUES (vals)   : Inserer une ligne
-- INSERT INTO table SELECT ...             : Inserer depuis SELECT
-- UPDATE table SET col=val WHERE ...       : Modifier des lignes
-- DELETE FROM table WHERE ...              : Supprimer des lignes
--
-- Transactions :
--   COMMIT    : Valide les modifications
--   ROLLBACK  : Annule les modifications
--   SAVEPOINT : Point de sauvegarde intermediaire
--
-- ATTENTION :
--   UPDATE/DELETE sans WHERE affecte TOUTES les lignes !
-- =================================================================
