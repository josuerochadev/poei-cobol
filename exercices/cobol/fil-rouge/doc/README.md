# Mini-Projet Informatique COBOL

**Duree:** 5 jours
**Date debut:** 25/11/2025
**Date fin:** 27/11/2025
**Travail:** Individuel

## Theme

Developpement d'un mini-projet COBOL sous environnement Z/OS pour le suivi clientele dans le secteur financier.

## Sujet

Une institution financiere propose de suivre les comptes de ces clients en chiffres et sur des periodes etalees dans le temps pour mener des operations commerciales et de marketing.

Le travail sera realise sur une plateforme informatique IBM Z/OS en se basant principalement sur l'utilisation des outils ISPF, JCL, VSAM et COBOL.

---

## Creation des Library

| Library | Description |
|---------|-------------|
| `Nom-candidat.FINANCE.SOURCE` | Programme et JCL |
| `Nom-candidat.FINANCE.LINK` | Programme Objet |
| `Nom-candidat.FINANCE.LOAD` | Programme executable |

---

## Structure des Fichiers

### Fichier Region (`Nom-candidat.FINANCE.REGION`)

| Champ | Type | Taille | Remarque |
|-------|------|--------|----------|
| Code region | Numerique | 2 | Cle unique |
| Id region | Caractere | 15 | |

**Exemple:**
```
01PARIS
02MARSEILLE
03LYON
04LILLE
```

### Fichier Nature Compte (`Nom-candidat.FINANCE.NATCOMPT`)

| Champ | Type | Taille | Remarque |
|-------|------|--------|----------|
| Code Compte | Numerique | 2 | Cle unique |
| Nature Compte | Caractere | 30 | |

**Exemple:**
```
20COMPTE EPARGNE
25COMPTE CHEQUE
30COMPTE COMMERCIAL
35COMPTE COMPAGNE AGRICOLE
40COMPTE CDI
```

### Fichier Activite Professionnelle (`Nom-candidat.FINANCE.PROFESSION`)

| Champ | Type | Taille | Remarque |
|-------|------|--------|----------|
| Code Profession | Numerique | 2 | Cle unique |
| Libelle Profession | Caractere | 20 | |

**Exemple:**
```
05MEDECIN
10INGENIEUR
15COMPTABLE
20COMMERCANT
25FONCTIONNAIRE
30PRIVEE
```

### Fichier CLIENT (`Nom-candidat.FINANCE.CLIENT`)

| Champ | Type | Taille | Remarque |
|-------|------|--------|----------|
| Numero de compte | Numerique | 3 | Cle unique |
| Code region | Numerique | 2 | |
| Nature compte | Numerique | 2 | |
| Nom client | Alphabetique | 10 | |
| Prenom client | Alphabetique | 10 | |
| Date naissance | Numerique | 8 | Format AAAAMMJJ |
| Sexe | Caractere | 1 | M ou F |
| Activite professionnelle | Numerique | 2 | |
| Situation sociale | Alphabetique | 1 | C, M, D ou V |
| Adresse | Caractere | 10 | |
| Solde | Numerique | 10 | |
| Position | Caractere | 2 | DB ou CR |

**Exemple:**
```
0010320NOMCLIENT1 PRNCLIENT1 19901102F15CADRCLIENT1    125000CR
0150225NOMCLIENT2 PRNCLIENT2 19950503M20CADCLIENT2      10000DB
```

---

## 1ere Partie : Chargement des donnees de base et des fichiers referentiels

### Exercice 01 - Creation des Data Sets
Proceder par la creation des Data Set donnees en utilisant aussi bien utilitaire IEBGENER et IDCAMS. Les PS sont crees au prealable, par la suite vous creez et chargez les Data Set VSAM a partir de ces PS.

Les Data Set a creer sont:
- a. Fichier Client
- b. Fichier Region
- c. Fichier Nature Compte
- d. Fichier Activite Professionnelle

---

## 2eme Partie : Utilisation des commandes et utilitaires VSAM

### Exercice 02 - Extraction par profession (SORT)
En utilisant l'utilitaire SORT, creer trois DATA SET VSAM contenant chacun les clients de profession COMPTABLE, FONCTIONNAIRE et MEDECIN:
- `Nom-candidat.FINANCE.CLIENT.COMPTABLE`
- `Nom-candidat.FINANCE.CLIENT.FONCTIONNAIRE`
- `Nom-candidat.FINANCE.CLIENT.MEDECIN`

### Exercice 03 - Separation CR/DB (SORT)
En utilisant l'utilitaire SORT, creer deux DATA SET VSAM contenant chacun les clients CR et les clients DB.

### Exercice 04 - Repartition par region (SORT)
En utilisant l'utilitaire SORT, creer une repartition par REGION dans quatre Data Set differents.

### Exercice 05 - Listing IDCAMS
En utilisant l'utilitaire IDCAMS, lister le contenu des deux Data Set DB et CR crees ci-dessus.

### Exercice 06 - Tri par numero de compte (SORT)
En utilisant l'utilitaire SORT, trier le Data Set `Nom-candidat.FINANCE.CLIENT` selon le numero de compte.

### Exercice 07 - Chargement KSDS
Charger le Data SET `Nom-candidat.FINANCE.CLIENT` deja trie dans un nouveau Data Set KSDS portant le nom `Nom-candidat.FINANCE.CLIENT.KSDS` et dont la KEYS est le Numero de compte.

### Exercice 08 - AIX Region (IDCAMS)
En utilisant l'utilitaire IDCAMS, creer un index secondaire AIX sur le Data Set CLIENT dont la KEYS secondaire est composee du code REGION.
- Nom: `Nom-candidat.AIX.REGION`

### Exercice 09 - AIX Activite Professionnelle (IDCAMS)
En utilisant l'utilitaire IDCAMS, creer un index secondaire AIX sur le Data Set CLIENT dont la KEYS secondaire est composee du code Activite Professionnelle.
- Nom: `Nom-candidat.AIX.ACTPROF`

### Exercice 10 - Edition par AIX (COBOL Write)
Editer le Data Set `Nom-candidat.FINANCE.CLIENT` en utilisant l'instruction Write selon l'ordre par REGION et par suite par ACTIVITE PROFESSIONNELLE. Cette edition sera realisee en faisant un saut de deux lignes en passant d'une REGION a une autre et de meme pour les ACTIVITE PROFESSIONNELLE.

### Exercice 11 - Fusion SORT
En utilisant l'utilitaire SORT, fusionner les deux Data Set `Nom-candidat.FINANCE.CLIENT.FONCTIONNAIRE` et `Nom-candidat.FINANCE.CLIENT.COMPTABLE` dans un Data Set nomme `Nom-candidat.FINANCE.CLIENT.FUSION`.

### Exercice 12 - Client reduit (SORT)
En utilisant l'utilitaire SORT, creer un nouveau Data Set CLIENT reduit (`Nom-candidat.CLIENT.REDUIT`), compose des champs suivants uniquement:

| Champ | Type | Taille |
|-------|------|--------|
| Numero de compte | Numerique | 3 |
| Code region | Numerique | 2 |
| Nature compte | Numerique | 2 |
| Nom client | Alphabetique | 10 |
| Prenom client | Alphabetique | 10 |
| Activite professionnelle | Numerique | 2 |
| Situation sociale | Alphabetique | 1 |
| Solde | Numerique | 10 |
| Position | Caractere | 2 |

---

## 3eme Partie : Mise a jour des donnees et programmation COBOL

### Exercice 13 - Ajout client (COBOL)
Ecrire un programme COBOL permettant d'ajouter un nouveau client dans le Data Set CLIENT (KSDS).

### Exercice 14 - Sous-programmes edition (COBOL)
Editer sous forme de tableau les trois Data Set REGION, ACTIVITE PROFESSIONNELLE et NATURE COMPTE sous forme de trois sous programmes COBOL separes.

Le contenu de chaque Data Set doit etre edite avec:
- Une interligne vide entre deux lignes
- Une page separee pour chaque Data Set
- Utiliser les parametres `ADVANCING PAGE` et `ADVANCING LINE`

### Exercice 15 - Montants et moyennes (COBOL)
Calculer le Montant General et la Moyenne des Comptes CLIENT Debiteurs et des Comptes CLIENT Crediteurs et editer le resultat sur un fichier Spool.

```
************************************************************
* Montant general Debiteurs     *                          *
************************************************************
* Montant general Crediteurs    *                          *
************************************************************
* Montant Debiteur Moyen        *                          *
************************************************************
* Montant Crediteur Moyen       *                          *
************************************************************
```

### Exercice 16 - Calcul par region avec niveau 88 (COBOL)
Calculer la valeur des Comptes Debiteurs et des Comptes Crediteurs par REGION en utilisant la variable conditionnelle (88). Les valeurs de cette variable seront les valeurs des codes regions.

### Exercice 17 - Top 5 debiteurs (COBOL)
Lister les cinq premiers Clients DEBITEURS (en valeurs) en se basant sur le Data Set cree precedemment (Exercice 07).

### Exercice 18 - Creation fichier mouvements
Creer un nouveau Data Set pour gerer les mouvements des Clients (`Nom-candidat.CLIENT.MOUV`):

| Champ | Type | Taille | Format |
|-------|------|--------|--------|
| Numero de compte | Numerique | 3 | |
| Libelle Mouvement | Alphabetique | 15 | |
| Montant-Mouvement | Numerique | 6 | |
| Sens Mouvement | Caractere | 2 | DB/CR |
| Nature Mouvement | Caractere | 3 | CHQ/VER/VIR |
| Date Mouvement | Date | 10 | AAAA/MM/JJ |

> Maximum 20 enregistrements, utiliser des numeros de compte definis dans le Data Set CLIENT.

### Exercice 19 - Calcul mouvements client (COBOL)
Ecrire un programme COBOL permettant de calculer le Montant des mouvements d'un CLIENT ainsi que le nombre de ces mouvements.

- Le programme recoit l'information du Client a traiter par l'instruction `ACCEPT`
- Le numero de compte sera fourni par une donnee In-stream
- Tri interne avec la condition de recuperer par le tri que les enregistrements relatifs au client mentionne

### Exercice 20 - Releve de compte (COBOL)
Editer un Releve de compte des mouvements d'un CLIENT selon ce modele:

```
*************************************************************
* Nom Client :                    Numero de compte :        *
*************************************************************
* Date operation * Libelle        * Credit    * Debit       *
*************************************************************
* ...            * ...            * ...       * ...         *
* ...            * ...            * ...       * ...         *
*************************************************************
*                                        DATE :             *
*************************************************************
```

### Exercice 21 - Fusion MERGE (COBOL)
Composer des donnees fictives de trois mois des mouvements clients tel que definis dans la question N 18 et fusionner les donnees des Data Set en utilisant l'instruction `MERGE` interne de COBOL.

---

## Correspondance Exercices / Dossiers

| Exercice | Dossier | Description |
|----------|---------|-------------|
| 01 | `ex01-fichiers-base` | Creation fichiers de base (Client) |
| 01 | `ex02-referentiels` | Creation referentiels (Region, Compte, Profession) |
| 02 | `ex03-sort-profession` | Extraction par profession |
| 03 | `ex04-sort-position` | Separation CR/DB |
| 04 | `ex05-sort-region` | Repartition par region |
| 05 | `ex06-idcams-print` | Listing IDCAMS |
| 06 | `ex07-sort-tri` | Tri par numero de compte |
| 07 | `ex07-sort-tri` | Chargement KSDS |
| 08 | `ex08-index-alternatif` | AIX Region |
| 09 | `ex09-index-alternatif-2` | AIX Activite Professionnelle |
| 10 | `ex10-editer-index-alternatif` | Edition par AIX |
| 11 | `ex11-fusion-sort` | Fusion SORT |
| 12 | `ex12-client-reduit` | Client reduit |
| 13 | `ex13-ajout` | Ajout client COBOL |
| 14 | `ex14-sous-programmes` | Sous-programmes edition |
| 15 | `ex15-montants` | Montants et moyennes |
| 16 | `ex16-conditions` | Calcul par region (niveau 88) |
| 17 | `ex17-top5` | Top 5 debiteurs |
| 18 | `ex18-mouvements` | Creation fichier mouvements |
| 19 | `ex19-montants-mvt` | Calcul mouvements client |
| 20 | `ex20-releves` | Releve de compte |
| 21 | `ex21-fusion` | Fusion MERGE COBOL |

---

**BON TRAVAIL**
