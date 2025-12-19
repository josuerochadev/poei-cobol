# QCM Chapitre VI - Algorithmes sur Fichiers

*Questions sur les algorithmes de traitement de fichiers sequentiels*

---

## Question 1
Quelle est la principale difference entre un tableau et un fichier ?

- a) Un fichier ne peut contenir que des nombres
- b) Un fichier est stocke sur disque, un tableau en memoire
- c) Un tableau peut etre trie, pas un fichier
- d) Un fichier est plus rapide d'acces

<details><summary>Reponse</summary>b) Un fichier est stocke sur disque, un tableau en memoire - Le fichier est persistant</details>

---

## Question 2
Dans le pattern "Read-Ahead", quand lit-on le premier enregistrement ?

- a) Dans la boucle
- b) Avant la boucle
- c) Apres la boucle
- d) Jamais

<details><summary>Reponse</summary>b) Avant la boucle - On lit d'abord, puis on traite dans la boucle</details>

---

## Question 3
Quel est le schema correct de lecture sequentielle ?

- a) Lire → Boucle(Traiter, Lire)
- b) Boucle(Lire, Traiter)
- c) Boucle(Traiter) → Lire
- d) Traiter → Boucle(Lire)

<details><summary>Reponse</summary>a) Lire → Boucle(Traiter, Lire) - Lecture anticipee (Read-Ahead)</details>

---

## Question 4
Qu'est-ce qu'une rupture de controle ?

- a) Une erreur de lecture
- b) Un changement de valeur de cle dans un fichier trie
- c) La fin du fichier
- d) Un enregistrement invalide

<details><summary>Reponse</summary>b) Un changement de valeur de cle dans un fichier trie - Permet de detecter les groupes</details>

---

## Question 5
A quoi sert la rupture de controle ?

- a) Trier le fichier
- b) Produire des sous-totaux par groupe
- c) Fusionner des fichiers
- d) Supprimer des doublons

<details><summary>Reponse</summary>b) Produire des sous-totaux par groupe - Etats avec totaux par categorie</details>

---

## Question 6
Pour une rupture de controle, le fichier doit etre :

- a) Vide
- b) Trie sur la cle de rupture
- c) En memoire
- d) Indexe

<details><summary>Reponse</summary>b) Trie sur la cle de rupture - Sinon les groupes ne sont pas contigus</details>

---

## Question 7
Dans une rupture a 2 niveaux (Region, Client), quel est le niveau majeur ?

- a) Client
- b) Region
- c) Les deux
- d) Aucun

<details><summary>Reponse</summary>b) Region - Le niveau majeur est le plus global (le premier dans l'ordre de tri)</details>

---

## Question 8
Que fait l'algorithme de fusion ?

- a) Combine deux fichiers tries en un fichier trie
- b) Trie un fichier
- c) Supprime les doublons
- d) Inverse l'ordre des enregistrements

<details><summary>Reponse</summary>a) Combine deux fichiers tries en un fichier trie - Merge de deux fichiers</details>

---

## Question 9
Quelle est la complexite de la fusion de deux fichiers de tailles n et m ?

- a) O(n × m)
- b) O(n + m)
- c) O(n²)
- d) O(log n)

<details><summary>Reponse</summary>b) O(n + m) - Chaque enregistrement est lu et ecrit une fois</details>

---

## Question 10
Pour fusionner deux fichiers, ils doivent etre :

- a) De meme taille
- b) Tries sur la meme cle
- c) Vides
- d) Indexes

<details><summary>Reponse</summary>b) Tries sur la meme cle - Prerequis pour l'algorithme de fusion</details>

---

## Question 11
Qu'est-ce que l'appareillement de fichiers ?

- a) Trier deux fichiers
- b) Comparer deux fichiers pour trouver correspondances et differences
- c) Concatener deux fichiers
- d) Dupliquer un fichier

<details><summary>Reponse</summary>b) Comparer deux fichiers pour trouver correspondances et differences - Matching</details>

---

## Question 12
Dans l'appareillement, si cle1 < cle2, cela signifie :

- a) Correspondance trouvee
- b) Enregistrement uniquement dans F1
- c) Enregistrement uniquement dans F2
- d) Erreur

<details><summary>Reponse</summary>b) Enregistrement uniquement dans F1 - La cle de F1 n'existe pas dans F2</details>

---

## Question 13
**ERREUR A DETECTER** : L'appareillement peut fonctionner sur des fichiers non tries.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Les fichiers doivent etre tries sur la meme cle</details>

---

## Question 14
Quel code represente generalement une suppression dans un fichier mouvement ?

- a) A
- b) M
- c) S
- d) D

<details><summary>Reponse</summary>c) S - Pour Suppression (A=Ajout, M=Modification)</details>

---

## Question 15
Dans la mise a jour maitre-mouvement, que fait-on si la cle maitre < cle mouvement ?

- a) Appliquer le mouvement
- b) Recopier le maitre tel quel
- c) Supprimer le maitre
- d) Signaler une erreur

<details><summary>Reponse</summary>b) Recopier le maitre tel quel - Pas de mouvement pour cet enregistrement</details>

---

## Question 16
Que fait l'eclatement de fichier ?

- a) Trie un fichier
- b) Repartit les enregistrements dans plusieurs fichiers selon un critere
- c) Fusionne plusieurs fichiers
- d) Compresse un fichier

<details><summary>Reponse</summary>b) Repartit les enregistrements dans plusieurs fichiers selon un critere - Split</details>

---

## Question 17
En COBOL, quelle clause detecte la fin de fichier ?

- a) END-OF-FILE
- b) AT END
- c) WHEN FINISHED
- d) FILE-END

<details><summary>Reponse</summary>b) AT END - Clause du READ pour detecter la fin</details>

---

## Question 18
Quelle instruction JCL permet de fusionner des fichiers ?

- a) SORT FIELDS
- b) MERGE FIELDS
- c) COMBINE FILES
- d) JOIN FIELDS

<details><summary>Reponse</summary>b) MERGE FIELDS - Operation de fusion en JCL SORT</details>

---

## Question 19
Dans l'algorithme de fusion, quand vide-t-on le reste d'un fichier ?

- a) Au debut
- b) Quand l'autre fichier est epuise
- c) Jamais
- d) A chaque enregistrement

<details><summary>Reponse</summary>b) Quand l'autre fichier est epuise - Il faut copier les enregistrements restants</details>

---

## Question 20
**ERREUR A DETECTER** : Dans la mise a jour, une modification sans enregistrement maitre correspondant est normale.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - C'est une erreur, on ne peut pas modifier ce qui n'existe pas</details>

---

## Question 21
Combien de fichiers sont ouverts simultanement pour une fusion simple ?

- a) 1
- b) 2
- c) 3
- d) 4

<details><summary>Reponse</summary>c) 3 - Deux fichiers en entree, un en sortie</details>

---

## Question 22
Quel est l'avantage du traitement sequentiel sur les gros fichiers ?

- a) Acces aleatoire
- b) Performance (lecture continue)
- c) Modification en place
- d) Pas besoin de tri

<details><summary>Reponse</summary>b) Performance (lecture continue) - Le disque lit des blocs consecutifs efficacement</details>

---

## Question 23
Dans une rupture de controle, quand affiche-t-on le sous-total ?

- a) A chaque enregistrement
- b) Quand la cle change
- c) En debut de fichier
- d) Jamais

<details><summary>Reponse</summary>b) Quand la cle change - C'est la "rupture" qui declenche l'affichage</details>

---

## Question 24
Quelle verification est importante avant une fusion ou un appareillement ?

- a) Taille des fichiers
- b) Fichiers tries sur la bonne cle
- c) Fichiers non vides
- d) Fichiers de meme format

<details><summary>Reponse</summary>b) Fichiers tries sur la bonne cle - Prerequis fondamental</details>

---

## Question 25
En COBOL, quelle section contient la definition des fichiers ?

- a) WORKING-STORAGE SECTION
- b) FILE SECTION
- c) LINKAGE SECTION
- d) LOCAL-STORAGE SECTION

<details><summary>Reponse</summary>b) FILE SECTION - Dans la DATA DIVISION</details>

---

## Question 26
Qu'est-ce que le FILE STATUS en COBOL ?

- a) La taille du fichier
- b) Un code retour indiquant le resultat de l'operation
- c) Le nom du fichier
- d) Le type de fichier

<details><summary>Reponse</summary>b) Un code retour indiquant le resultat de l'operation - 00 = succes</details>

---

## Question 27
**ERREUR A DETECTER** : On peut ajouter un enregistrement dont la cle existe deja dans le maitre.

- a) Vrai
- b) Faux

<details><summary>Reponse</summary>b) Faux - Un ajout (code A) sur une cle existante est une erreur (doublon)</details>

---

## Question 28
Dans l'eclatement, combien de passes sur le fichier d'entree ?

- a) Autant que de fichiers de sortie
- b) Une seule
- c) Deux
- d) Depend des donnees

<details><summary>Reponse</summary>b) Une seule - On lit chaque enregistrement une fois et on l'ecrit dans le bon fichier</details>

---

## Question 29
Quel type de traitement est typique en environnement batch mainframe ?

- a) Interactif
- b) Temps reel
- c) Sequentiel sur fichiers
- d) Graphique

<details><summary>Reponse</summary>c) Sequentiel sur fichiers - Traitements de masse planifies</details>

---

## Question 30
Quelle est la bonne pratique apres chaque operation fichier ?

- a) Trier le fichier
- b) Verifier le FILE STATUS
- c) Fermer le fichier
- d) Vider le buffer

<details><summary>Reponse</summary>b) Verifier le FILE STATUS - Pour detecter les erreurs</details>

---

*Total : 30 questions - Formation Algorithmique M2i*
