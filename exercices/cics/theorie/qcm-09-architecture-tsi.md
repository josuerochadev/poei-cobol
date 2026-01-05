# QCM 09 - Architecture Multicouches et Transactions TSI

## Questions (Chapitre IX)

### Question 1

Quelle est la différence principale entre un accesseur logique et un accesseur physique ?

- [ ] a) L'accesseur logique est plus rapide que l'accesseur physique
- [ ] b) L'accesseur logique permet d'accéder aux données indépendamment de leur implémentation physique
- [ ] c) L'accesseur physique est utilisé uniquement pour les bases de données SQL
- [ ] d) L'accesseur logique ne fonctionne qu'avec VSAM

<details>
<summary>Réponse</summary>

**b) L'accesseur logique permet d'accéder aux données indépendamment de leur implémentation physique**

L'accesseur logique utilise des modèles de données abstraits (READ, WRITE pour VSAM ou SELECT, INSERT pour SQL) indépendamment de leur implémentation en tables ou fichiers. L'accesseur physique interagit directement avec le stockage (disque, mémoire).

</details>

---

### Question 2

Qu'est-ce que la TSI (Transaction Status Information) ?

- [ ] a) Un composant officiel IBM pour la gestion des transactions
- [ ] b) Une zone de contrôle utilisée dans les applications transactionnelles pour tracer le statut des transactions
- [ ] c) Un protocole de communication réseau
- [ ] d) Un type de fichier VSAM

<details>
<summary>Réponse</summary>

**b) Une zone de contrôle utilisée dans les applications transactionnelles pour tracer le statut des transactions**

La TSI est une structure COBOL (COPY) initialisée au démarrage d'une transaction. Elle permet de tracer le statut, stocker les codes d'erreur, transporter des indicateurs de contexte et identifier l'environnement d'exécution. Ce n'est pas un composant officiel IBM mais une notion courante dans les architectures bancaires et Télécom.

</details>

---

### Question 3

Quel champ EIB permet de récupérer l'identifiant de la transaction en cours dans CICS ?

- [ ] a) EIBTRMID
- [ ] b) EIBTRNID
- [ ] c) EIBTASKN
- [ ] d) EIBDATE

<details>
<summary>Réponse</summary>

**b) EIBTRNID**

EIBTRNID contient l'identifiant de la transaction en cours. EIBTRMID contient l'identifiant du terminal, EIBTASKN contient le numéro de tâche, et EIBDATE contient la date.

</details>

---

### Question 4

Dans une architecture multicouches avec CICS, quel framework JavaScript pourrait être utilisé pour la couche présentation Web ?

- [ ] a) React, Angular ou Vue.js
- [ ] b) Uniquement React
- [ ] c) COBOL-WEB
- [ ] d) BMS uniquement

<details>
<summary>Réponse</summary>

**a) React, Angular ou Vue.js**

Pour créer une interface Web moderne qui interagit avec CICS, on peut utiliser des frameworks JavaScript comme React (Facebook), Angular (Google) ou Vue.js. Ces frameworks communiquent avec le mainframe via des APIs REST exposées par z/OS Connect ou CICS Transaction Gateway.

</details>

---

### Question 5

Quelle est la valeur DFHRESP correspondant à "enregistrement non trouvé" ?

- [ ] a) DFHRESP(NORMAL) = 00
- [ ] b) DFHRESP(DUPREC) = 14
- [ ] c) DFHRESP(NOTFND) = 13
- [ ] d) DFHRESP(LENGERR) = 22

<details>
<summary>Réponse</summary>

**c) DFHRESP(NOTFND) = 13**

NOTFND (code 13) indique que l'enregistrement demandé n'a pas été trouvé. NORMAL (00) = exécution normale, DUPREC (14) = enregistrement en double, LENGERR (22) = erreur de longueur.

</details>

---

### Question 6

Combien de conditions maximum peuvent être codées dans une instruction HANDLE CONDITION ?

- [ ] a) 8
- [ ] b) 10
- [ ] c) 12
- [ ] d) 16

<details>
<summary>Réponse</summary>

**c) 12**

Un maximum de 12 conditions peuvent être codées dans une seule instruction HANDLE CONDITION. Si plus de conditions doivent être gérées, il faut utiliser plusieurs instructions HANDLE CONDITION.

</details>

---

### Question 7

Quelle commande permet de désactiver temporairement la gestion des erreurs pour une commande CICS spécifique ?

- [ ] a) IGNORE CONDITION
- [ ] b) HANDLE ABEND
- [ ] c) NOHANDLE
- [ ] d) CANCEL

<details>
<summary>Réponse</summary>

**c) NOHANDLE**

L'option NOHANDLE désactive temporairement toutes les conditions HANDLE pour une commande CICS spécifique. Si une exception survient, le contrôle passe à l'instruction suivante. Note : l'utilisation de l'option RESP implique automatiquement NOHANDLE.

</details>

---

### Question 8

Quelle est la différence entre le mode conversationnel et le mode pseudo-conversationnel ?

- [ ] a) Pas de différence, ce sont des synonymes
- [ ] b) En mode conversationnel, le programme reste en mémoire pendant l'attente de l'utilisateur ; en pseudo-conversationnel, il est libéré
- [ ] c) Le mode pseudo-conversationnel n'utilise pas de terminal
- [ ] d) Le mode conversationnel est plus performant

<details>
<summary>Réponse</summary>

**b) En mode conversationnel, le programme reste en mémoire pendant l'attente de l'utilisateur ; en pseudo-conversationnel, il est libéré**

En mode conversationnel, le programme reste chargé en mémoire pendant le "think time" de l'utilisateur, ce qui consomme des ressources. En mode pseudo-conversationnel (recommandé), le programme se termine après avoir envoyé l'écran et spécifie la transaction à démarrer lorsque l'utilisateur répond. Les ressources sont ainsi libérées pour d'autres transactions.

</details>

---

### Question 9

Que signifie l'attribut MDT (Modified Data Tag) dans BMS ?

- [ ] a) Un indicateur pour les champs numériques
- [ ] b) Un bit indiquant si un champ a été modifié et doit être transmis lors du RECEIVE
- [ ] c) Un attribut de couleur pour les champs
- [ ] d) Un marqueur de fin de MAP

<details>
<summary>Réponse</summary>

**b) Un bit indiquant si un champ a été modifié et doit être transmis lors du RECEIVE**

Le MDT est un bit (8ème position) de l'octet d'attribut. Quand MDT=1 (ON), le champ sera transmis lors du RECEIVE MAP. Le MDT passe automatiquement à ON si l'utilisateur modifie le champ. L'option FRSET permet de remettre tous les MDT à zéro.

</details>

---

### Question 10

Quelle macro BMS définit les caractéristiques d'un champ individuel dans une MAP ?

- [ ] a) DFHMSD
- [ ] b) DFHMDI
- [ ] c) DFHMDF
- [ ] d) DFHAID

<details>
<summary>Réponse</summary>

**c) DFHMDF**

DFHMDF (Defined Field Hierarchy Map Definition Fields) définit un champ avec ses caractéristiques (position, longueur, attributs) dans une MAP. DFHMSD définit le MAPSET, DFHMDI définit la MAP, et DFHAID est le copybook des touches de fonction.

</details>

---

### Question 11

Quelle transaction CICS permet de gérer les ressources du système (terminaux, programmes, transactions) ?

- [ ] a) CEDF
- [ ] b) CECI
- [ ] c) CEMT
- [ ] d) CEDA

<details>
<summary>Réponse</summary>

**c) CEMT**

CEMT (Master Terminal) permet de gérer les ressources CICS : afficher l'état des tâches, activer/désactiver des programmes ou transactions, charger de nouvelles copies de programmes, mettre en/hors service des terminaux, et arrêter CICS (SHUTDOWN).

</details>

---

### Question 12

Quelle commande CEDA permet de rendre une définition disponible pour le système CICS actif ?

- [ ] a) CEDA DEFINE
- [ ] b) CEDA ADD
- [ ] c) CEDA INSTALL
- [ ] d) CEDA COPY

<details>
<summary>Réponse</summary>

**c) CEDA INSTALL**

CEDA INSTALL rend les définitions de ressources (programmes, transactions, MAPs) disponibles pour le système CICS en cours d'exécution. CEDA DEFINE crée les définitions sur le CSD, CEDA ADD ajoute un groupe à une liste, CEDA COPY copie une définition.

</details>

---

### Question 13

Quelle touche PF dans CEDF permet de basculer entre l'affichage alphanumérique et hexadécimal ?

- [ ] a) PF1
- [ ] b) PF2
- [ ] c) PF3
- [ ] d) PF4

<details>
<summary>Réponse</summary>

**b) PF2**

Dans CEDF, PF2 permet de basculer entre l'affichage alphanumérique et hexadécimal. PF1 affiche l'aide, PF3 termine la session EDF, PF4 affiche les champs EIB.

</details>

---

### Question 14

Combien de MAP peut contenir un MAPSET au maximum ?

- [ ] a) 100
- [ ] b) 255
- [ ] c) 512
- [ ] d) 1023

<details>
<summary>Réponse</summary>

**d) 1023**

CICS permet de définir jusqu'à 1023 MAP par MAPSET. Cette décomposition permet de mieux organiser l'emplacement du curseur et les différentes zones de l'écran.

</details>

---

### Question 15

Quelle est la séquence correcte pour effectuer une navigation (browse) dans un fichier VSAM sous CICS ?

- [ ] a) READ → READNEXT → ENDBR
- [ ] b) STARTBR → READNEXT/READPREV → ENDBR
- [ ] c) OPEN → READ → CLOSE
- [ ] d) STARTBR → READ → ENDBR

<details>
<summary>Réponse</summary>

**b) STARTBR → READNEXT/READPREV → ENDBR**

La navigation (browse) commence par STARTBR qui initialise le parcours, suivi de READNEXT ou READPREV pour lire les enregistrements séquentiellement, et se termine obligatoirement par ENDBR. RESETBR peut être utilisé pour repositionner sans fermer le browse.

</details>

---

### Question 16

Quelle option de la commande READ permet de verrouiller l'enregistrement pour une mise à jour ultérieure ?

- [ ] a) LOCK
- [ ] b) HOLD
- [ ] c) UPDATE
- [ ] d) EXCLUSIVE

<details>
<summary>Réponse</summary>

**c) UPDATE**

L'option UPDATE sur la commande READ verrouille l'enregistrement pour permettre une mise à jour (REWRITE) ou une suppression (DELETE) ultérieure. Ce verrou est maintenu jusqu'au SYNCPOINT, ROLLBACK, ou la prochaine opération.

</details>

---

### Question 17

Quelle commande CICS permet d'annuler toutes les modifications effectuées depuis le dernier point de synchronisation ?

- [ ] a) SYNCPOINT
- [ ] b) SYNCPOINT ROLLBACK
- [ ] c) ABEND CANCEL
- [ ] d) RETURN

<details>
<summary>Réponse</summary>

**b) SYNCPOINT ROLLBACK**

SYNCPOINT ROLLBACK annule toutes les modifications effectuées depuis le dernier point de synchronisation. SYNCPOINT valide les modifications. Ces commandes permettent de garantir les propriétés ACID des transactions.

</details>

---

### Question 18

Quelle est la particularité des programmes COBOL exécutés sous CICS concernant les fichiers ?

- [ ] a) Ils utilisent les verbes OPEN et CLOSE comme en batch
- [ ] b) Ils n'utilisent pas OPEN et CLOSE car CICS gère l'ouverture/fermeture des fichiers
- [ ] c) Ils doivent ouvrir les fichiers deux fois
- [ ] d) Ils ne peuvent pas accéder aux fichiers VSAM

<details>
<summary>Réponse</summary>

**b) Ils n'utilisent pas OPEN et CLOSE car CICS gère l'ouverture/fermeture des fichiers**

Contrairement aux programmes batch, les programmes COBOL sous CICS n'utilisent pas les instructions OPEN et CLOSE. L'ouverture et la fermeture des Data Sets est gérée par CICS. De plus, la section ENVIRONMENT DIVISION n'est pas nécessaire pour les déclarations de fichiers.

</details>

---

### Question 19

Lors d'une mise à jour pendant un browse, quelle est la séquence correcte ?

- [ ] a) READNEXT UPDATE → REWRITE
- [ ] b) ENDBR → READ UPDATE → REWRITE → STARTBR
- [ ] c) RESETBR → REWRITE
- [ ] d) STARTBR UPDATE → REWRITE → ENDBR

<details>
<summary>Réponse</summary>

**b) ENDBR → READ UPDATE → REWRITE → STARTBR**

Update et Browse sont des fonctions mutuellement exclusives. Pour mettre à jour un enregistrement pendant un browse, il faut : 1) ENDBR pour terminer le browse, 2) READ avec UPDATE pour verrouiller l'enregistrement, 3) REWRITE pour la mise à jour, 4) STARTBR pour reprendre la navigation.

</details>

---

### Question 20

Quel suffixe est utilisé pour les variables INPUT dans la MAP symbolique générée par BMS ?

- [ ] a) L (Length)
- [ ] b) A (Attribute)
- [ ] c) I (Input)
- [ ] d) F (Flag)

<details>
<summary>Réponse</summary>

**c) I (Input)**

Les variables de la MAP symbolique utilisent des suffixes : L (Length) pour la longueur, F (Flag) pour l'indicateur de modification, A (Attribute) pour l'attribut dynamique, I (Input) pour les données en entrée, O (Output) pour les données en sortie.

</details>

---

### Question 21

Quelle est la fonction de IBM MQ dans une architecture CICS ?

- [ ] a) Afficher les écrans 3270
- [ ] b) Permettre la communication asynchrone par messages entre les couches
- [ ] c) Compiler les programmes COBOL
- [ ] d) Gérer les transactions SQL

<details>
<summary>Réponse</summary>

**b) Permettre la communication asynchrone par messages entre les couches**

IBM MQ est un système de gestion de messages qui permet aux applications de communiquer de manière asynchrone. Dans une architecture CICS, il sert d'intermédiaire entre la couche présentation et la couche métier COBOL, permettant un découplage des systèmes et une gestion fiable des transactions.

</details>

---

### Question 22

Quelle commande permet de vérifier la syntaxe et d'exécuter interactivement des commandes CICS ?

- [ ] a) CEMT
- [ ] b) CEDA
- [ ] c) CECI
- [ ] d) CEDF

<details>
<summary>Réponse</summary>

**c) CECI**

CECI (Command Level Interpreter) permet de vérifier la syntaxe des commandes CICS et de les exécuter interactivement sur un écran 3270. Il est utilisé pour tester des commandes, créer des données de test, et déboguer des problèmes.

</details>

---

### Question 23

Quelle option de l'attribut ATTRB permet de positionner le curseur initialement sur un champ ?

- [ ] a) FSET
- [ ] b) DET
- [ ] c) IC
- [ ] d) BRT

<details>
<summary>Réponse</summary>

**c) IC**

IC (Initial Cursor) dans l'attribut ATTRB permet de définir la position initiale du curseur lors de l'envoi de la MAP. FSET active le MDT, DET rend le champ détectable, BRT affiche le champ en intensité brillante.

</details>

---

### Question 24

Quelle est la valeur du code RESP pour une erreur de fichier non ouvert ?

- [ ] a) 17 (IOERR)
- [ ] b) 18 (NOSPACE)
- [ ] c) 19 (NOTOPEN)
- [ ] d) 20 (ENDFILE)

<details>
<summary>Réponse</summary>

**c) 19 (NOTOPEN)**

NOTOPEN (code 19) indique que le fichier est fermé ou UNENABLED. IOERR (17) = erreur I/O, NOSPACE (18) = plus d'espace disponible, ENDFILE (20) = fin de fichier détectée.

</details>

---

### Question 25

Quelle est la contrainte importante lors de l'utilisation de REWRITE sur un fichier VSAM ?

- [ ] a) La longueur de l'enregistrement doit doubler
- [ ] b) Le champ de la clé ne doit pas changer
- [ ] c) Le fichier doit être fermé avant
- [ ] d) On ne peut réécrire qu'une seule fois

<details>
<summary>Réponse</summary>

**b) Le champ de la clé ne doit pas changer**

Lors d'un REWRITE sur un fichier VSAM, le champ de la clé primaire ne doit pas être modifié. De plus, REWRITE doit obligatoirement être précédé d'un READ avec l'option UPDATE pour verrouiller l'enregistrement.

</details>

---

## Barème de notation

| Score | Appréciation |
|-------|--------------|
| 23-25 | Excellent - Maîtrise complète |
| 18-22 | Très bien - Bonnes connaissances |
| 15-17 | Bien - Connaissances satisfaisantes |
| 10-14 | Passable - À approfondir |
| < 10 | Insuffisant - Révision nécessaire |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [QCM 06 - Couche Données](qcm-06-donnees.md) | - |

---
*Formation COBOL - Module CICS - QCM Chapitre IX*
