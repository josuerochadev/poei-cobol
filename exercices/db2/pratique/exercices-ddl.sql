-- =================================================================
-- EXERCICES DDL - CREATE, ALTER, DROP
-- Formation DB2/SQL - M2i Formation
-- =================================================================
-- Ces exercices pratiquent les commandes de definition de donnees
-- non couvertes dans les TP de la formatrice.
-- =================================================================


-- =================================================================
-- EXERCICE 1 : Creer une table simple
-- =================================================================
-- Objectif : Creer la table PRODUIT avec les colonnes suivantes :
--   - PROD_ID    : Entier, cle primaire
--   - PROD_NOM   : Chaine de 50 caracteres, obligatoire
--   - PRIX       : Decimal (10,2)
--   - STOCK      : Entier, defaut 0
-- =================================================================

-- Votre solution :



-- Solution :
/*
CREATE TABLE PRODUIT (
    PROD_ID     INTEGER NOT NULL PRIMARY KEY,
    PROD_NOM    VARCHAR(50) NOT NULL,
    PRIX        DECIMAL(10,2),
    STOCK       INTEGER DEFAULT 0
);
*/


-- =================================================================
-- EXERCICE 2 : Creer une table avec cle etrangere
-- =================================================================
-- Objectif : Creer la table COMMANDE referencant PRODUIT :
--   - CMD_ID     : Entier, cle primaire
--   - CMD_DATE   : Date, defaut CURRENT_DATE
--   - PROD_ID    : Cle etrangere vers PRODUIT
--   - QUANTITE   : Entier, CHECK > 0
-- =================================================================

-- Votre solution :



-- Solution :
/*
CREATE TABLE COMMANDE (
    CMD_ID      INTEGER NOT NULL PRIMARY KEY,
    CMD_DATE    DATE DEFAULT CURRENT_DATE,
    PROD_ID     INTEGER NOT NULL,
    QUANTITE    INTEGER CHECK (QUANTITE > 0),
    FOREIGN KEY (PROD_ID) REFERENCES PRODUIT(PROD_ID)
);
*/


-- =================================================================
-- EXERCICE 3 : Ajouter une colonne
-- =================================================================
-- Objectif : Ajouter la colonne CATEGORIE (VARCHAR 30) a PRODUIT
-- =================================================================

-- Votre solution :



-- Solution :
/*
ALTER TABLE PRODUIT
ADD COLUMN CATEGORIE VARCHAR(30);
*/


-- =================================================================
-- EXERCICE 4 : Ajouter une contrainte
-- =================================================================
-- Objectif : Ajouter une contrainte CHECK sur PRIX (doit etre >= 0)
-- =================================================================

-- Votre solution :



-- Solution :
/*
ALTER TABLE PRODUIT
ADD CONSTRAINT CHK_PRIX CHECK (PRIX >= 0);
*/


-- =================================================================
-- EXERCICE 5 : Creer un index
-- =================================================================
-- Objectif : Creer un index sur PROD_NOM pour accelerer les recherches
-- =================================================================

-- Votre solution :



-- Solution :
/*
CREATE INDEX IDX_PROD_NOM ON PRODUIT(PROD_NOM);
*/


-- =================================================================
-- EXERCICE 6 : Creer une vue
-- =================================================================
-- Objectif : Creer une vue PRODUITS_EN_STOCK montrant uniquement
--            les produits avec STOCK > 0
-- =================================================================

-- Votre solution :



-- Solution :
/*
CREATE VIEW PRODUITS_EN_STOCK AS
SELECT PROD_ID, PROD_NOM, PRIX, STOCK
FROM PRODUIT
WHERE STOCK > 0;
*/


-- =================================================================
-- EXERCICE 7 : Supprimer des objets
-- =================================================================
-- Objectif : Supprimer la vue, l'index, puis les tables
--            (dans le bon ordre pour respecter les FK)
-- =================================================================

-- Votre solution :



-- Solution :
/*
DROP VIEW PRODUITS_EN_STOCK;
DROP INDEX IDX_PROD_NOM;
DROP TABLE COMMANDE;  -- D'abord la table avec FK
DROP TABLE PRODUIT;   -- Ensuite la table referencee
*/


-- =================================================================
-- RESUME DES COMMANDES DDL
-- =================================================================
-- CREATE TABLE   : Creer une table
-- ALTER TABLE    : Modifier une table (ADD, DROP, MODIFY)
-- DROP TABLE     : Supprimer une table
-- CREATE INDEX   : Creer un index
-- DROP INDEX     : Supprimer un index
-- CREATE VIEW    : Creer une vue
-- DROP VIEW      : Supprimer une vue
--
-- Contraintes :
--   PRIMARY KEY  : Cle primaire (unique + not null)
--   FOREIGN KEY  : Cle etrangere (integrite referentielle)
--   UNIQUE       : Valeurs uniques
--   NOT NULL     : Valeur obligatoire
--   CHECK        : Condition de validite
--   DEFAULT      : Valeur par defaut
-- =================================================================
