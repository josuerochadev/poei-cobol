# Chapitre 03 - Manipulation IDCAMS

## Objectifs

- Copier des donnees avec REPRO
- Modifier des clusters avec ALTER
- Supprimer avec DELETE
- Verifier avec VERIFY
- Imprimer avec PRINT

---

## Exercice 1 : REPRO - Copie et Chargement

**Fichier :** `ex01-repro.jcl`

**Objectif :** Maitriser les differentes utilisations de REPRO

**Concepts :**
- Chargement depuis fichier sequentiel
- Copie VSAM vers VSAM
- Copie VSAM vers sequentiel
- Selection avec FROMKEY/TOKEY

---

## Exercice 2 : ALTER - Modification d'Attributs

**Fichier :** `ex02-alter.jcl`

**Objectif :** Modifier les attributs d'un cluster existant

**Concepts :**
- NEWNAME pour renommer
- FREESPACE pour modifier l'espace libre
- INHIBIT/UNINHIBIT pour lecture seule

---

## Exercice 3 : DELETE et VERIFY

**Fichier :** `ex03-delete-verify.jcl`

**Objectif :** Supprimer des clusters et verifier l'integrite

**Concepts :**
- DELETE avec PURGE
- DELETE avec ERASE
- VERIFY pour reparer

---

## Exercice 4 : PRINT - Affichage du Contenu

**Fichier :** `ex04-print.jcl`

**Objectif :** Afficher le contenu des fichiers VSAM

**Concepts :**
- CHARACTER, HEX, DUMP
- FROMKEY/TOKEY
- SKIP/COUNT

---

## Exercice Bonus : Workflow Complet

**Fichier :** `ex-bonus-workflow.jcl`

**Objectif :** Workflow complet de gestion VSAM

---

## Fichier de Nettoyage

**Fichier :** `cleanup.jcl`

---
*Exercices VSAM - M2i Formation*
