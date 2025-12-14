# Activite 7 - Sous-Requetes

## Objectifs

- Maitriser les sous-requetes simples (retournant une valeur)
- Utiliser les sous-requetes multi-colonnes
- Appliquer les sous-requetes synchronisees (correlees)
- Combiner sous-requetes avec IN, ANY, ALL, EXISTS

---

## Question 1

Creez une requete pour afficher le nom et la date d'embauche de tous les employes travaillant dans le **meme departement que 'JEAN'**, a l'exclusion de 'JEAN'.

---

## Question 2

Creez une requete pour afficher le matricule et le nom de tous les employes qui gagnent **plus que le salaire moyen**. Triez les resultats par ordre decroissant des salaires.

---

## Question 3

Ecrivez une requete pour afficher le matricule et le nom de tous les employes qui travaillent dans le **meme departement que tout employe dont le nom contient un 'T'**.

---

## Question 4

Modifiez la requete (3) afin d'afficher le matricule, le nom et le salaire de tous les employes qui **gagnent plus que le salaire moyen** ET qui **travaillent dans un departement avec tout employe dont le nom contient un 'T'**.

---

## Question 5

Affichez le nom, le numero de departement et le poste de tous les employes dont le **departement est situe a 'STRASBOURG'**.

---

## Question 6

Affichez le nom et le salaire de tous les employes dont le **directeur est 'ALBERT'**.

---

## Question 7

Affichez le numero de departement, le nom et le poste de tous les employes travaillant dans le **departement des ventes 'VENTES'**.

---

## Question 8

Creez une requete pour afficher les employes qui percoivent un salaire **superieur a TOUT employe dont le poste est 'AGENT'**. Triez le resultat par ordre decroissant des salaires.

---

## Question 9

Ecrivez une requete pour afficher le nom, le numero de departement et le salaire de tout employe dont le **numero de departement et le salaire correspondent** au numero de departement et au salaire d'un des employes **touchant une commission**.

---

## Question 10

Affichez le nom de l'employe, le nom du departement et le salaire de tout employe dont le **salaire et la commission sont tous les deux equivalents** au salaire et a la commission de n'importe quel employe base a **'STRASBOURG'**.

---

## Question 11

Ecrivez une requete pour afficher les **trois meilleurs salaires** dans la table EMPLOYEE. Affichez les noms des employes et leur salaire.

---

## Question 12

Recherchez tous les employes qui **ne sont pas des responsables** (n'ont pas de subordonnes).

---

## Question 13

Ecrivez une requete pour rechercher tous les employes dont le salaire est **superieur au salaire moyen de leur departement**. Affichez le numero de chaque employe, son salaire, son numero de departement et le salaire moyen du departement. Triez le resultat en fonction du salaire moyen.

---

## Question 14

Ecrivez une requete pour afficher les employes dont le salaire est **inferieur a la moitie du salaire moyen** de leur departement.

---

## Question 15

Ecrivez une requete pour afficher les employes ayant **un ou plusieurs collegues de leur departement** dont les dates d'embauche sont **posterieures** aux leurs ET dont les salaires sont **plus eleves** que les leurs.

---

## Solutions

Voir le fichier [solutions.sql](solutions.sql)

---

## Navigation

| Precedent | Suivant |
|-----------|---------|
| [Activite 6](../activite-06-fonctions-groupe/) | [README TP](../README.md) |

---
*Formation DB2/SQL - M2i Formation*
