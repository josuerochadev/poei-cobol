# QCM Chapitre IV - Commandes IDCAMS

*Questions du formateur - Certaines contiennent des erreurs intentionnelles a detecter*

---

## Question 1
IDCAMS est :

- a) Un langage de programmation
- b) L'utilitaire de gestion des fichiers VSAM
- c) Un editeur de texte
- d) Un compilateur COBOL

<details><summary>Reponse</summary>b) L'utilitaire (Access Method Services) pour gerer les fichiers VSAM</details>

---

## Question 2
La commande DEFINE CLUSTER cree :

- a) Uniquement le composant DATA
- b) Uniquement le composant INDEX
- c) Le cluster avec ses composants DATA et INDEX (si KSDS)
- d) Une copie d'un fichier existant

<details><summary>Reponse</summary>c) Le cluster complet avec ses composants</details>

---

## Question 3
**ERREUR A DETECTER** : Pour un ESDS, DEFINE CLUSTER cree automatiquement un composant INDEX.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Un ESDS n'a PAS de composant INDEX, seulement DATA (NONINDEXED)</details>

---

## Question 4
Le parametre INDEXED dans DEFINE CLUSTER cree :

- a) Un ESDS
- b) Un KSDS
- c) Un RRDS
- d) Un LDS

<details><summary>Reponse</summary>b) Un KSDS (Key Sequenced Data Set)</details>

---

## Question 5
Le parametre KEYS(10 0) signifie :

- a) 10 cles a la position 0
- b) Cle de 10 octets commencant a la position 0
- c) 10 enregistrements avec cle 0
- d) Position 10, longueur 0

<details><summary>Reponse</summary>b) Cle de longueur 10 octets, debutant a l'offset 0 de l'enregistrement</details>

---

## Question 6
FREESPACE(20 10) signifie :

- a) 20% d'espace libre par CI, 10% par CA
- b) 20 octets libres, 10 CI libres
- c) 20 CI libres, 10 CA libres
- d) 20% du disque, 10% de memoire

<details><summary>Reponse</summary>a) 20% d'espace libre reserve dans chaque CI, 10% de CI libres dans chaque CA</details>

---

## Question 7
La commande REPRO permet de :

- a) Supprimer un fichier
- b) Copier des donnees d'un fichier vers un autre
- c) Renommer un fichier
- d) Afficher les statistiques

<details><summary>Reponse</summary>b) Copier/charger des donnees entre fichiers</details>

---

## Question 8
**ERREUR A DETECTER** : La commande DELETE CLUSTER supprime uniquement le composant DATA, laissant l'INDEX intact.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - DELETE CLUSTER supprime le cluster COMPLET avec tous ses composants (DATA et INDEX)</details>

---

## Question 9
La commande VERIFY est utilisee pour :

- a) Verifier la syntaxe JCL
- b) Reparer les indicateurs de fin de fichier apres un ABEND
- c) Valider les donnees
- d) Comparer deux fichiers

<details><summary>Reponse</summary>b) Synchroniser les indicateurs du catalogue avec l'etat reel du fichier (apres une fin anormale)</details>

---

## Question 10
Quel code retour indique "fichier vide" ?

- a) 0
- b) 4
- c) 8
- d) 12

<details><summary>Reponse</summary>b) 4 - Warning, souvent "fichier vide" ou condition mineure</details>

---

## Exercice Pratique - Detecter les erreurs

Analysez cette commande DEFINE et identifiez les erreurs :

```
DEFINE CLUSTER ( -
  NAME(TEST.MON.FICHIER) -
  INDEXED -
  RECORDSIZE(100 100) -
  KEYS(10 5)) -
DATA (NAME(TEST.MON.FICHIER.DATA)) -
INDEX (NAME(TEST.MON.FICHIER.INDEX))
```

<details><summary>Reponse</summary>
Cette commande est syntaxiquement correcte. Points a verifier :
- INDEXED : cree un KSDS (correct)
- RECORDSIZE(100 100) : longueur moyenne = max = 100 (enregistrements fixes)
- KEYS(10 5) : cle de 10 octets a partir de la position 5
- Manque : VOLUMES, TRACKS/CYLINDERS pour l'allocation
</details>

---

*Total : 10 questions + 1 exercice pratique - Formation VSAM M2i*
