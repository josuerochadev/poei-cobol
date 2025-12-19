# Chapitre 01 - Definition de Clusters VSAM

## Objectifs

- Creer des clusters VSAM de differents types
- Comprendre les parametres DEFINE CLUSTER
- Maitriser IDCAMS pour la creation

---

## Exercice 1 : Creation d'un ESDS

**Fichier :** `ex01-esds.jcl`

**Objectif :** Creer un cluster ESDS (Entry Sequenced Data Set)

**Concepts :**
- NONINDEXED = type ESDS
- RECORDSIZE
- SHAREOPTIONS
- REUSE

**Instructions :**
1. Definir un cluster ESDS nomme `FTEST.VSAM.ESDS`
2. Taille : 1 track primaire, 1 track secondaire
3. Taille enregistrement : 80 octets (fixe)
4. SHAREOPTIONS(1,3)
5. Verifier avec LISTCAT

---

## Exercice 2 : Creation d'un KSDS

**Fichier :** `ex02-ksds.jcl`

**Objectif :** Creer un cluster KSDS (Key Sequenced Data Set)

**Concepts :**
- INDEXED = type KSDS
- KEYS(longueur offset)
- FREESPACE
- Composants DATA et INDEX

**Instructions :**
1. Definir un cluster KSDS nomme `FTEST.VSAM.KSDS`
2. Cle de 10 octets en position 0
3. Taille enregistrement : 100 octets (fixe)
4. FREESPACE(10 10)
5. Nommer explicitement DATA et INDEX

---

## Exercice 3 : Creation d'un RRDS

**Fichier :** `ex03-rrds.jcl`

**Objectif :** Creer un cluster RRDS (Relative Record Data Set)

**Concepts :**
- NUMBERED = type RRDS
- Slots de taille fixe
- Acces par RRN

**Instructions :**
1. Definir un cluster RRDS nomme `FTEST.VSAM.RRDS`
2. Taille enregistrement : 50 octets (fixe obligatoire)
3. SHAREOPTIONS(1,3)

---

## Exercice 4 : Creation d'un LDS

**Fichier :** `ex04-lds.jcl`

**Objectif :** Creer un cluster LDS (Linear Data Set)

**Concepts :**
- LINEAR = type LDS
- CI fixe de 4 Ko
- Pas de RECORDSIZE

**Instructions :**
1. Definir un cluster LDS nomme `FTEST.VSAM.LDS`
2. Allocation : 2 tracks primaire, 1 track secondaire
3. SHAREOPTIONS(1,3)

---

## Exercice Bonus : Workflow Complet

**Fichier :** `ex-bonus-all-types.jcl`

**Objectif :** Creer les 4 types de clusters en un seul job

**Instructions :**
1. Supprimer les clusters existants (ignorer erreur si inexistant)
2. Creer ESDS, KSDS, RRDS, LDS
3. Lister tous avec LISTCAT
4. Utiliser IF-THEN-ELSE pour gestion d'erreurs

---

## Fichier de Nettoyage

**Fichier :** `cleanup.jcl`

Supprime tous les clusters crees pendant les exercices.

---

## Points Cles a Retenir

| Type | Mot-cle | Index | Identification |
|------|---------|-------|----------------|
| ESDS | NONINDEXED | Non | RBA |
| KSDS | INDEXED | Oui | Cle |
| RRDS | NUMBERED | Non | RRN |
| LDS | LINEAR | Non | Offset 4K |

---
*Exercices VSAM - M2i Formation*
