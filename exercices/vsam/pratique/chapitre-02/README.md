# Chapitre 02 - Index Alternatifs (AIX)

## Objectifs

- Creer des index alternatifs
- Definir des PATH
- Construire les index avec BLDINDEX
- Comprendre UPGRADE et NONUNIQUEKEY

---

## Exercice 1 : Creation d'un AIX sur KSDS

**Fichier :** `ex01-aix-ksds.jcl`

**Objectif :** Creer un index alternatif sur un cluster KSDS

**Concepts :**
- DEFINE ALTERNATEINDEX
- RELATE(cluster-base)
- UNIQUEKEY vs NONUNIQUEKEY
- UPGRADE

**Instructions :**
1. Creer un KSDS de base avec des donnees
2. Definir un AIX sur une cle secondaire
3. Definir le PATH
4. Construire l'index avec BLDINDEX

---

## Exercice 2 : AIX avec doublons

**Fichier :** `ex02-aix-nonunique.jcl`

**Objectif :** Creer un AIX autorisant les cles en double

**Concepts :**
- NONUNIQUEKEY
- Plusieurs enregistrements par cle AIX

**Instructions :**
1. Creer un KSDS clients
2. Creer un AIX sur la ville (doublons possibles)
3. Tester l'acces via le PATH

---

## Exercice 3 : AIX sur ESDS

**Fichier :** `ex03-aix-esds.jcl`

**Objectif :** Creer un index alternatif sur un cluster ESDS

**Concepts :**
- AIX sur ESDS
- Acces indexe a un fichier sequentiel

**Instructions :**
1. Creer un ESDS et le charger
2. Definir un AIX
3. Definir le PATH
4. Construire l'index

---

## Fichier de Nettoyage

**Fichier :** `cleanup.jcl`

---

## Points Cles a Retenir

| Option | Description |
|--------|-------------|
| RELATE | Cluster de base |
| UNIQUEKEY | Cle AIX unique |
| NONUNIQUEKEY | Doublons autorises |
| UPGRADE | Mise a jour auto |
| NOUPGRADE | Reconstruction manuelle |

---
*Exercices VSAM - M2i Formation*
