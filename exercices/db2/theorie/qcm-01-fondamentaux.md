# QCM 01 - Fondamentaux des Bases de Donnees

**Chapitre I** | 15 questions | Duree estimee : 10 minutes

---

## Question 1

Qu'est-ce qu'un SGBD ?

- A) Un langage de programmation
- B) Un systeme de gestion de bases de donnees
- C) Un type de fichier
- D) Un protocole reseau

<details>
<summary>Reponse</summary>

**B) Un systeme de gestion de bases de donnees**

Un SGBD (Systeme de Gestion de Bases de Donnees) est un logiciel qui permet de stocker, organiser, gerer et interroger des donnees de maniere structuree.

</details>

---

## Question 2

Que signifie l'acronyme ACID ?

- A) Access, Control, Integrity, Data
- B) Atomicity, Consistency, Isolation, Durability
- C) Application, Connection, Index, Database
- D) Automatic, Concurrent, Independent, Distributed

<details>
<summary>Reponse</summary>

**B) Atomicity, Consistency, Isolation, Durability**

ACID represente les 4 proprietes fondamentales des transactions :
- **Atomicite** : Une transaction est indivisible (tout ou rien)
- **Coherence** : La BD reste dans un etat valide
- **Isolation** : Les transactions concurrentes n'interferent pas
- **Durabilite** : Les modifications sont permanentes

</details>

---

## Question 3

Quel est l'avantage principal d'une base de donnees par rapport aux fichiers traditionnels ?

- A) Les fichiers sont plus rapides
- B) La centralisation et la coherence des donnees
- C) Les bases de donnees sont gratuites
- D) Les fichiers ne supportent pas le texte

<details>
<summary>Reponse</summary>

**B) La centralisation et la coherence des donnees**

Une BD centralise les donnees, elimine la redondance et garantit la coherence grace aux contraintes d'integrite.

</details>

---

## Question 4

Qu'est-ce que l'atomicite d'une transaction ?

- A) La transaction peut etre divisee en sous-parties
- B) La transaction est soit completement executee, soit completement annulee
- C) La transaction est executee en parallele
- D) La transaction est executee de maniere asynchrone

<details>
<summary>Reponse</summary>

**B) La transaction est soit completement executee, soit completement annulee**

L'atomicite garantit le principe du "tout ou rien" : si une partie echoue, toute la transaction est annulee (ROLLBACK).

</details>

---

## Question 5

Quelle propriete ACID garantit que les transactions concurrentes n'interferent pas entre elles ?

- A) Atomicite
- B) Coherence
- C) Isolation
- D) Durabilite

<details>
<summary>Reponse</summary>

**C) Isolation**

L'isolation garantit que les transactions s'executent comme si elles etaient seules, meme en cas d'acces concurrent.

</details>

---

## Question 6

Qu'est-ce qu'un langage assertionnel ?

- A) Un langage qui declare des assertions
- B) Un langage ou l'on decrit CE QUE l'on veut, pas COMMENT l'obtenir
- C) Un langage de programmation oriente objet
- D) Un langage de test automatise

<details>
<summary>Reponse</summary>

**B) Un langage ou l'on decrit CE QUE l'on veut, pas COMMENT l'obtenir**

SQL est un langage assertionnel (ou declaratif). On specifie le resultat souhaite, et le SGBD determine la methode optimale pour l'obtenir.

</details>

---

## Question 7

Quelle commande SQL est utilisee pour annuler une transaction ?

- A) CANCEL
- B) UNDO
- C) ROLLBACK
- D) REVERT

<details>
<summary>Reponse</summary>

**C) ROLLBACK**

ROLLBACK annule toutes les modifications depuis le dernier COMMIT ou le debut de la transaction.

</details>

---

## Question 8

Quelle commande SQL valide definitivement une transaction ?

- A) SAVE
- B) VALIDATE
- C) CONFIRM
- D) COMMIT

<details>
<summary>Reponse</summary>

**D) COMMIT**

COMMIT rend permanentes toutes les modifications effectuees dans la transaction courante.

</details>

---

## Question 9

Que sont les metadonnees dans une base de donnees ?

- A) Les donnees principales de l'application
- B) Les donnees qui decrivent la structure de la base
- C) Les donnees temporaires
- D) Les donnees chiffrees

<details>
<summary>Reponse</summary>

**B) Les donnees qui decrivent la structure de la base**

Les metadonnees (ou dictionnaire de donnees) contiennent la description des tables, colonnes, types, contraintes, index, etc.

</details>

---

## Question 10

Quel avantage offre l'acces concurrent dans une BD ?

- A) Les donnees sont dupliquees
- B) Plusieurs utilisateurs peuvent travailler simultanement
- C) Les requetes sont plus lentes
- D) Les transactions sont automatiques

<details>
<summary>Reponse</summary>

**B) Plusieurs utilisateurs peuvent travailler simultanement**

L'acces concurrent permet a plusieurs utilisateurs ou applications d'interroger et modifier les donnees en meme temps, tout en maintenant la coherence.

</details>

---

## Question 11

Quelle propriete ACID garantit que les donnees restent coherentes apres une panne ?

- A) Atomicite
- B) Coherence
- C) Isolation
- D) Durabilite

<details>
<summary>Reponse</summary>

**D) Durabilite**

La durabilite garantit qu'une fois une transaction validee (COMMIT), les modifications sont permanentes meme en cas de panne systeme.

</details>

---

## Question 12

Parmi les suivants, lequel N'EST PAS un avantage des bases de donnees ?

- A) Non-redondance des donnees
- B) Acces concurrent
- C) Complexite de mise en oeuvre reduite
- D) Controle de la securite

<details>
<summary>Reponse</summary>

**C) Complexite de mise en oeuvre reduite**

Au contraire, les SGBD sont plus complexes a mettre en oeuvre que de simples fichiers. C'est le prix a payer pour les nombreux avantages (coherence, securite, performance).

</details>

---

## Question 13

Dans quel cas utilise-t-on un SAVEPOINT ?

- A) Pour sauvegarder la base de donnees
- B) Pour creer un point de restauration intermediaire dans une transaction
- C) Pour exporter les donnees
- D) Pour creer une copie de la table

<details>
<summary>Reponse</summary>

**B) Pour creer un point de restauration intermediaire dans une transaction**

Un SAVEPOINT permet de faire un ROLLBACK partiel jusqu'a ce point, sans annuler toute la transaction.

</details>

---

## Question 14

Qu'est-ce que la coherence dans le contexte ACID ?

- A) Les donnees sont toujours les memes
- B) La base passe d'un etat valide a un autre etat valide
- C) Les donnees sont synchronisees entre serveurs
- D) Les requetes retournent toujours le meme resultat

<details>
<summary>Reponse</summary>

**B) La base passe d'un etat valide a un autre etat valide**

La coherence garantit que toutes les contraintes d'integrite sont respectees avant et apres chaque transaction.

</details>

---

## Question 15

Quel est le role principal du dictionnaire de donnees ?

- A) Stocker les mots de passe
- B) Decrire la structure et les metadonnees de la base
- C) Traduire les requetes SQL
- D) Optimiser les performances

<details>
<summary>Reponse</summary>

**B) Decrire la structure et les metadonnees de la base**

Le dictionnaire de donnees (catalogue systeme) contient toutes les informations sur les objets de la base : tables, colonnes, types, index, vues, contraintes, etc.

</details>

---

## Score

| Questions correctes | Appreciation |
|---------------------|--------------|
| 14-15 | Excellent |
| 11-13 | Tres bien |
| 8-10 | Bien |
| 5-7 | Moyen - Revoir le chapitre |
| 0-4 | Insuffisant - Reprendre le cours |

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [README](README.md) | [QCM 02 - Architecture DB2](qcm-02-architecture.md) |

---
*Formation DB2/SQL - M2i Formation*
