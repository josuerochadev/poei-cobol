# Chapitre I - Fondamentaux des Bases de Données

## I-1 : Qu'est-ce qu'une Base de Données ?

### Définition

Une **base de données** (BD) est un ensemble structuré de données organisées pour être facilement accessibles, gérées et mises à jour. Elle repose sur un logiciel spécialisé appelé **SGBD** (Système de Gestion de Bases de Données).

```
┌─────────────────────────────────────────────────────────────────┐
│                    BASE DE DONNÉES                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  MÉTADONNÉES (Dictionnaire de données)                  │   │
│   │  • Description des tables, colonnes, types              │   │
│   │  • Contraintes, relations entre tables                  │   │
│   │  • Index, vues, procédures stockées                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  DONNÉES                                                 │   │
│   │  • Enregistrements stockés dans des fichiers            │   │
│   │  • Organisés selon le schéma défini                     │   │
│   │  • Accessibles via requêtes SQL                         │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Les 5 caractéristiques clés

| Caractéristique | Description |
|-----------------|-------------|
| **Organisation** | Structure logique des données (tables, relations) |
| **Stockage** | Persistance sur support physique (disques) |
| **Partage** | Accès concurrent par plusieurs utilisateurs |
| **Confidentialité** | Contrôle d'accès et sécurité |
| **Performance** | Optimisation des temps de réponse |

### Fonctions d'une base de données

```
┌─────────────────────────────────────────────────────────────────┐
│               FONCTIONS D'UNE BASE DE DONNÉES                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. STOCKAGE de la description des objets (métadonnées)         │
│     • Structure des tables                                       │
│     • Types de données                                           │
│     • Contraintes d'intégrité                                   │
│                                                                  │
│  2. STOCKAGE des données dans des fichiers                      │
│     • Enregistrements organisés                                  │
│     • Index pour accès rapide                                    │
│                                                                  │
│  3. GESTION de l'accès aux données                              │
│     • Lecture (SELECT)                                           │
│     • Écriture (INSERT, UPDATE, DELETE)                         │
│                                                                  │
│  4. ASSURANCE de l'intégrité des données                        │
│     • Contraintes respectées                                     │
│     • Cohérence garantie                                         │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## I-2 : Avantages des Bases de Données

### Pourquoi utiliser une BD plutôt que des fichiers ?

```
┌─────────────────────────────────────────────────────────────────┐
│          FICHIERS TRADITIONNELS vs BASE DE DONNÉES              │
├───────────────────────────────┬─────────────────────────────────┤
│        FICHIERS               │      BASE DE DONNÉES            │
├───────────────────────────────┼─────────────────────────────────┤
│                               │                                 │
│  • Données dupliquées        │  • Données centralisées         │
│  • Incohérence possible      │  • Cohérence garantie           │
│  • Accès séquentiel          │  • Accès direct par clé         │
│  • Un programme = un fichier │  • Indépendance données/prog    │
│  • Sécurité limitée          │  • Contrôle d'accès fin         │
│  • Pas de partage simultané  │  • Accès concurrent             │
│                               │                                 │
└───────────────────────────────┴─────────────────────────────────┘
```

### Les 6 avantages principaux

| Avantage | Description |
|----------|-------------|
| **Disponibilité** | Consultation, saisie et mise à jour à tout moment |
| **Non-redondance** | Limitation de la duplication des données |
| **Accès concurrent** | Plusieurs utilisateurs travaillent simultanément |
| **Partage et sécurité** | Données partagées avec contrôle des droits |
| **Fiabilité** | Transactions, reprises sur pannes, sauvegardes |
| **Langage assertionnel** | SQL : décrire CE QUE l'on veut, pas COMMENT |

### Exemple concret : gestion des employés

**Sans base de données (fichiers) :**
```
┌──────────────────────────────────────────────────────────────────┐
│                                                                   │
│  PAIE.DAT                    RH.DAT                              │
│  ┌─────────────────────┐     ┌─────────────────────┐             │
│  │ EMP001 MARTIN 3500  │     │ EMP001 MARTIN COMPTA│             │
│  │ EMP002 DUPONT 4200  │     │ EMP002 DUPON  INFO  │  ◄─ Erreur! │
│  └─────────────────────┘     └─────────────────────┘             │
│                                                                   │
│  Problèmes :                                                      │
│  • Nom dupliqué dans 2 fichiers                                  │
│  • Incohérence (DUPONT vs DUPON)                                 │
│  • Mise à jour = modifier 2 fichiers                             │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

**Avec base de données :**
```
┌──────────────────────────────────────────────────────────────────┐
│                                                                   │
│  TABLE EMPLOYEE                                                   │
│  ┌─────────────────────────────────────────────┐                 │
│  │ EMP_NUM │ NOM    │ DEPT   │ SALAIRE │       │                 │
│  ├─────────┼────────┼────────┼─────────┤       │                 │
│  │ EMP001  │ MARTIN │ COMPTA │ 3500    │       │                 │
│  │ EMP002  │ DUPONT │ INFO   │ 4200    │       │                 │
│  └─────────────────────────────────────────────┘                 │
│                                                                   │
│  Avantages :                                                      │
│  • Donnée unique, pas de duplication                             │
│  • Cohérence garantie                                            │
│  • Une seule mise à jour suffit                                  │
│                                                                   │
└──────────────────────────────────────────────────────────────────┘
```

---

## I-3 : SGBD - Système de Gestion de Bases de Données

### Définition

Un **SGBD** (DataBase Management System - DBMS en anglais) est le logiciel qui permet de créer, gérer et interroger une base de données.

```
┌─────────────────────────────────────────────────────────────────┐
│                           SGBD                                    │
│                                                                  │
│   Interface entre les UTILISATEURS et les DONNÉES                │
│                                                                  │
│   ┌──────────────┐    ┌──────────────┐    ┌──────────────┐     │
│   │ Applications │    │     SGBD     │    │   Données    │     │
│   │    (SQL)     │───►│   (DB2,      │───►│  (Fichiers)  │     │
│   │              │    │   Oracle...) │    │              │     │
│   └──────────────┘    └──────────────┘    └──────────────┘     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Les 10 fonctionnalités principales du SGBD

```
┌─────────────────────────────────────────────────────────────────┐
│                  10 FONCTIONNALITÉS DU SGBD                      │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. PERSISTANCE DES DONNÉES                                      │
│     └── Stockage durable sur disque                             │
│                                                                  │
│  2. GESTION DU DISQUE ET OPTIMISATION                           │
│     └── Indexation, cache mémoire, placement physique           │
│                                                                  │
│  3. LDD - Langage de Description de Données                     │
│     └── CREATE, ALTER, DROP (structure)                         │
│                                                                  │
│  4. LMD - Langage de Manipulation de Données                    │
│     └── INSERT, UPDATE, DELETE (contenu)                        │
│                                                                  │
│  5. LID - Langage d'Interrogation de Données                    │
│     └── SELECT (lecture)                                        │
│                                                                  │
│  6. LCD - Langage de Contrôle des Données                       │
│     └── GRANT, REVOKE (droits d'accès)                         │
│                                                                  │
│  7. PARTAGE ET GESTION SIMULTANÉE                               │
│     └── Transactions, verrous, accès concurrent                 │
│                                                                  │
│  8. SÉCURITÉ ET CONFIDENTIALITÉ                                 │
│     └── Authentification, autorisation, audit                   │
│                                                                  │
│  9. PROPRIÉTÉS ACID                                             │
│     └── Atomicité, Cohérence, Isolation, Durabilité            │
│                                                                  │
│ 10. CONCURRENCE D'ACCÈS                                         │
│     └── Gestion des accès simultanés sans conflit              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Les sous-langages SQL

| Sous-langage | Signification | Commandes | Rôle |
|--------------|---------------|-----------|------|
| **DDL** | Data Definition Language | CREATE, ALTER, DROP | Définir la structure |
| **DML** | Data Manipulation Language | INSERT, UPDATE, DELETE | Modifier les données |
| **DQL** | Data Query Language | SELECT | Interroger les données |
| **DCL** | Data Control Language | GRANT, REVOKE | Gérer les droits |
| **TCL** | Transaction Control Language | COMMIT, ROLLBACK | Gérer les transactions |

### Principaux SGBD du marché

| SGBD | Éditeur | Environnement | Caractéristiques |
|------|---------|---------------|------------------|
| **DB2** | IBM | Mainframe z/OS, LUW | Référence mainframe, haute performance |
| **Oracle** | Oracle | Multi-plateforme | Leader du marché, très complet |
| **SQL Server** | Microsoft | Windows, Linux | Intégration écosystème Microsoft |
| **PostgreSQL** | Open Source | Multi-plateforme | Gratuit, très riche fonctionnellement |
| **MySQL** | Oracle | Multi-plateforme | Populaire pour le web |
| **IMS** | IBM | Mainframe z/OS | Base hiérarchique (non-relationnel) |

---

## I-4 : Propriétés ACID

### Définition

Les propriétés **ACID** garantissent la fiabilité des transactions dans un SGBD. Elles sont essentielles pour les systèmes critiques (banque, finance, santé).

### Les 4 propriétés

```
┌─────────────────────────────────────────────────────────────────┐
│                    PROPRIÉTÉS ACID                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  A - ATOMICITÉ                                          │    │
│  │  ─────────────                                          │    │
│  │  Une transaction s'exécute ENTIÈREMENT ou PAS DU TOUT   │    │
│  │                                                          │    │
│  │  Exemple : Virement de 100€                             │    │
│  │  • Débiter compte A de 100€   ─┐                        │    │
│  │  • Créditer compte B de 100€  ─┘ Les 2 ou aucun        │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  C - COHÉRENCE (Consistency)                            │    │
│  │  ───────────────                                        │    │
│  │  La base passe d'un état VALIDE à un autre état VALIDE  │    │
│  │                                                          │    │
│  │  Exemple : Contrainte "solde >= 0"                      │    │
│  │  • Avant transaction : solde A = 150€ (valide)         │    │
│  │  • Après transaction : solde A = 50€ (valide)          │    │
│  │  • Impossible d'avoir solde A = -50€ (invalide)        │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  I - ISOLATION                                          │    │
│  │  ───────────                                            │    │
│  │  Les transactions s'exécutent comme si elles étaient    │    │
│  │  SEULES (pas d'interférence)                           │    │
│  │                                                          │    │
│  │  Exemple : 2 transactions simultanées                   │    │
│  │  • T1 lit le solde = 1000€                             │    │
│  │  • T2 ne peut pas modifier tant que T1 n'est pas finie │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │  D - DURABILITÉ                                         │    │
│  │  ────────────                                           │    │
│  │  Une fois validée (COMMIT), la transaction est          │    │
│  │  DÉFINITIVEMENT enregistrée (même en cas de panne)      │    │
│  │                                                          │    │
│  │  Exemple : Après COMMIT                                 │    │
│  │  • Panne de courant → données préservées               │    │
│  │  • Crash système → données récupérables                │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Tableau récapitulatif ACID

| Propriété | Question clé | Garantie |
|-----------|--------------|----------|
| **Atomicité** | Tout ou rien ? | Transaction complète ou annulée |
| **Cohérence** | État valide ? | Contraintes toujours respectées |
| **Isolation** | Interférences ? | Transactions indépendantes |
| **Durabilité** | Persistance ? | Données sauvegardées définitivement |

### Exemple complet : virement bancaire

```
┌─────────────────────────────────────────────────────────────────┐
│             VIREMENT BANCAIRE - PROPRIÉTÉS ACID                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Scénario : Virer 500€ du compte A vers le compte B             │
│                                                                  │
│  État initial :                                                  │
│  • Compte A : 1000€                                             │
│  • Compte B : 200€                                              │
│                                                                  │
│  Transaction :                                                   │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  BEGIN TRANSACTION                                        │   │
│  │    1. UPDATE COMPTE SET SOLDE = SOLDE - 500               │   │
│  │       WHERE NUM_COMPTE = 'A'                              │   │
│  │    2. UPDATE COMPTE SET SOLDE = SOLDE + 500               │   │
│  │       WHERE NUM_COMPTE = 'B'                              │   │
│  │  COMMIT                                                   │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ATOMICITÉ : Les 2 UPDATE réussissent ou aucun                  │
│  COHÉRENCE : Solde A >= 0, somme totale constante (1200€)      │
│  ISOLATION : Autre transaction ne voit pas l'état intermédiaire│
│  DURABILITÉ : Après COMMIT, panne n'annule pas le virement     │
│                                                                  │
│  État final :                                                    │
│  • Compte A : 500€                                              │
│  • Compte B : 700€                                              │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Que se passe-t-il en cas d'échec ?

| Situation | Action du SGBD | Résultat |
|-----------|----------------|----------|
| Étape 1 réussit, étape 2 échoue | **ROLLBACK** automatique | Compte A retrouve 1000€ |
| Panne après COMMIT | **Recovery** au redémarrage | Données intègres |
| Panne avant COMMIT | **Rollback** au redémarrage | Retour état initial |

---

## I-5 : Transactions et Concurrence

### Qu'est-ce qu'une transaction ?

Une **transaction** est une unité logique de travail comprenant une ou plusieurs opérations SQL qui doivent être traitées comme un tout indivisible.

```
┌─────────────────────────────────────────────────────────────────┐
│                       TRANSACTION                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Début ──► Opérations SQL ──► Fin                               │
│                                                                  │
│  ┌────────────┐                     ┌────────────┐              │
│  │   BEGIN    │                     │  COMMIT    │              │
│  │ TRANSACTION│     ───────────►    │    ou      │              │
│  │            │                     │  ROLLBACK  │              │
│  └────────────┘                     └────────────┘              │
│                                                                  │
│  • COMMIT   : Valide toutes les modifications                   │
│  • ROLLBACK : Annule toutes les modifications                   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Gestion de la concurrence

```
┌─────────────────────────────────────────────────────────────────┐
│                  CONCURRENCE D'ACCÈS                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Problème : 2 utilisateurs modifient la même donnée             │
│                                                                  │
│  Temps    User A              User B                            │
│  ─────    ──────              ──────                            │
│  T1       READ solde=1000                                       │
│  T2                           READ solde=1000                   │
│  T3       solde=1000-100=900                                    │
│  T4                           solde=1000-200=800                │
│  T5       WRITE solde=900                                       │
│  T6                           WRITE solde=800  ◄── Problème!    │
│                                                                  │
│  Résultat attendu : 1000-100-200 = 700€                        │
│  Résultat obtenu : 800€ (User A écrasé!)                       │
│                                                                  │
│  Solution : VERROUS (locks)                                     │
│  ──────────────────────────                                     │
│  • Le SGBD verrouille la donnée pendant modification           │
│  • User B attend que User A ait terminé                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Types de verrous

| Type de verrou | Mode | Permet |
|----------------|------|--------|
| **Partagé** (Shared) | Lecture | Plusieurs lecteurs simultanés |
| **Exclusif** (Exclusive) | Écriture | Un seul utilisateur à la fois |

---

## I-6 : Modèles de données

### Évolution historique

```
┌─────────────────────────────────────────────────────────────────┐
│               ÉVOLUTION DES MODÈLES DE DONNÉES                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1960s ─────► 1970s ─────► 1980s ─────► 2000s+                 │
│                                                                  │
│  HIÉRARCHIQUE  RÉSEAU    RELATIONNEL    NoSQL                   │
│    (IMS)       (CODASYL)   (DB2)       (MongoDB...)             │
│                                                                  │
│  ┌───┐        ┌───┐       ┌─────┐      ┌─────────┐             │
│  │ A │        │ A │       │Table│      │Document │             │
│  ├───┴───┐    ├───┼───┐   ├─────┤      │ JSON    │             │
│  │B│ │C│ │    │B│ │C│ │   │Lignes│     │ Clé-Val │             │
│  └─┘ └─┘     └─┴─┴─┘     └─────┘      │ Graphes │             │
│                                        └─────────┘             │
│  Arbre       Graphe      Tables        Flexible                │
│              orienté                                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Comparaison des modèles

| Modèle | Structure | SGBD | Usage |
|--------|-----------|------|-------|
| **Hiérarchique** | Arbre (parent-enfants) | IMS | Mainframe, legacy |
| **Réseau** | Graphe | IDMS | Legacy |
| **Relationnel** | Tables avec relations | DB2, Oracle | Standard actuel |
| **Objet** | Classes et objets | ObjectDB | Applications OO |
| **NoSQL** | Documents, clé-valeur | MongoDB, Redis | Web, Big Data |

### Le modèle relationnel

Le **modèle relationnel** (Edgar F. Codd, 1970) est le standard dominant depuis les années 1980.

**Principes fondamentaux :**
- Données organisées en **tables** (relations)
- Chaque table a des **colonnes** (attributs) et des **lignes** (tuples)
- Les tables sont liées par des **clés** (primaires et étrangères)
- Manipulation via **SQL** (langage déclaratif)

```
┌─────────────────────────────────────────────────────────────────┐
│                   MODÈLE RELATIONNEL                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  TABLE EMPLOYEE                    TABLE DEPT                    │
│  ┌────────┬────────┬─────────┐    ┌─────────┬─────────────┐    │
│  │EMP_NUM │EMP_NOM │DEPT_NUM │    │DEPT_NUM │ DEPT_NOM    │    │
│  ├────────┼────────┼─────────┤    ├─────────┼─────────────┤    │
│  │ 7369   │ ARTHUR │   20    │────│   20    │ RECHERCHE   │    │
│  │ 7499   │ PAUL   │   30    │    │   30    │ VENTES      │    │
│  │ 7521   │ MARTIN │   30    │────│   30    │ VENTES      │    │
│  └────────┴────────┴─────────┘    └─────────┴─────────────┘    │
│                  │                      ▲                       │
│                  │                      │                       │
│                  └──────────────────────┘                       │
│                    Clé étrangère → Clé primaire                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────┐
│                    CHAPITRE I - RÉSUMÉ                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  I-1 BASE DE DONNÉES                                            │
│      • Ensemble structuré de données                            │
│      • 5 caractéristiques : Organisation, Stockage, Partage,    │
│        Confidentialité, Performance                              │
│                                                                  │
│  I-2 AVANTAGES                                                   │
│      • Disponibilité, non-redondance, accès concurrent          │
│      • Partage, fiabilité, langage SQL                          │
│                                                                  │
│  I-3 SGBD                                                        │
│      • Logiciel de gestion des BD                               │
│      • 10 fonctionnalités principales                           │
│      • Sous-langages : DDL, DML, DQL, DCL, TCL                  │
│                                                                  │
│  I-4 PROPRIÉTÉS ACID                                            │
│      • Atomicité : tout ou rien                                 │
│      • Cohérence : état valide à valide                         │
│      • Isolation : transactions indépendantes                   │
│      • Durabilité : persistance après COMMIT                    │
│                                                                  │
│  I-5 TRANSACTIONS                                                │
│      • Unité logique de travail                                 │
│      • COMMIT valide, ROLLBACK annule                           │
│      • Verrous pour gérer la concurrence                        │
│                                                                  │
│  I-6 MODÈLES DE DONNÉES                                         │
│      • Hiérarchique → Réseau → Relationnel → NoSQL             │
│      • Modèle relationnel = standard actuel                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Exercices

### Questions de compréhension

1. **Base de données**
   - Citez les 5 caractéristiques clés d'une base de données.
   - Quels sont les avantages d'une BD par rapport à des fichiers traditionnels ?

2. **SGBD**
   - Qu'est-ce qu'un SGBD ? Donnez 3 exemples.
   - Quels sont les 5 sous-langages de SQL ? Donnez une commande pour chacun.

3. **Propriétés ACID**
   - Expliquez chaque lettre de l'acronyme ACID.
   - Pourquoi l'atomicité est-elle cruciale pour un virement bancaire ?

4. **Transactions**
   - Quelle est la différence entre COMMIT et ROLLBACK ?
   - Comment le SGBD gère-t-il deux utilisateurs qui modifient la même donnée ?

### Exercice pratique

**Scénario : Réservation de place de cinéma**

Un système de réservation doit :
1. Vérifier que le siège est disponible
2. Marquer le siège comme réservé
3. Débiter le compte client
4. Générer le billet

**Questions :**
- Identifiez les propriétés ACID qui s'appliquent à ce scénario.
- Que se passe-t-il si l'étape 3 échoue après que le siège a été réservé ?
- Comment garantir qu'un siège ne soit pas réservé deux fois simultanément ?

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| - | [Chapitre II - Architecture DB2](02-architecture-db2.md) |

---
*Formation DB2/SQL - M2i Formation*
