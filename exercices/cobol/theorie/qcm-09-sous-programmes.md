# QCM - Chapitre IX : Programmes et Sous-Programmes

## Instructions

- **25 questions** couvrant l'ensemble du chapitre
- Cochez la ou les bonnes réponses
- Certaines questions peuvent avoir plusieurs réponses correctes

---

## Section 1 : Concepts fondamentaux (Questions 1-5)

### Question 1
Comment appelle-t-on le programme qui exécute l'instruction CALL ?

- [ ] A) Programme APPELÉ
- [ ] B) Programme APPELANT
- [ ] C) Sous-programme
- [ ] D) Programme principal

<details>
<summary>Réponse</summary>

**B** - Le programme qui contient le verbe CALL est le programme **APPELANT**. Le programme appelé est le sous-programme.

</details>

---

### Question 2
Quels sont les avantages de la modularisation en COBOL ?

- [ ] A) Réutilisation du code
- [ ] B) Facilité de maintenance
- [ ] C) Programme plus rapide à l'exécution
- [ ] D) Possibilité de tester chaque module indépendamment

<details>
<summary>Réponse</summary>

**A, B, D** - La modularisation permet la réutilisation, facilite la maintenance et les tests. Elle n'améliore pas forcément les performances d'exécution.

</details>

---

### Question 3
Quelle est la différence entre un sous-programme interne et externe ?

- [ ] A) Interne = PERFORM, Externe = CALL
- [ ] B) Interne = même fichier source, Externe = compilé séparément
- [ ] C) Interne = plus rapide, Externe = plus lent
- [ ] D) Aucune différence

<details>
<summary>Réponse</summary>

**A et B** - Les sous-programmes internes sont des paragraphes exécutés par PERFORM. Les sous-programmes externes sont des programmes séparés appelés par CALL.

</details>

---

### Question 4
Un sous-programme COBOL peut-il être exécuté indépendamment ?

- [ ] A) Oui, comme tout programme
- [ ] B) Non, il doit être appelé par un autre programme
- [ ] C) Oui, mais seulement avec EXIT PROGRAM
- [ ] D) Oui, s'il contient STOP RUN

<details>
<summary>Réponse</summary>

**B** - Un sous-programme peut être compilé indépendamment mais ne peut pas être exécuté indépendamment. Il doit être appelé par un programme principal.

</details>

---

### Question 5
Que se passe-t-il pour le programme appelant lors d'un CALL ?

- [ ] A) Il continue son exécution en parallèle
- [ ] B) Son exécution s'arrête jusqu'au retour du sous-programme
- [ ] C) Il est terminé définitivement
- [ ] D) Il est rechargé en mémoire

<details>
<summary>Réponse</summary>

**B** - L'exécution du programme appelant s'arrête jusqu'à ce que le sous-programme termine et retourne le contrôle.

</details>

---

## Section 2 : Instruction CALL (Questions 6-10)

### Question 6
Quelle est la syntaxe correcte pour appeler un sous-programme avec paramètres ?

- [ ] A) `CALL 'SOUSPROG' PASSING WS-VAR1, WS-VAR2`
- [ ] B) `CALL 'SOUSPROG' USING WS-VAR1, WS-VAR2`
- [ ] C) `CALL 'SOUSPROG' WITH WS-VAR1, WS-VAR2`
- [ ] D) `CALL 'SOUSPROG' SEND WS-VAR1, WS-VAR2`

<details>
<summary>Réponse</summary>

**B** - La syntaxe correcte est `CALL 'nom' USING parametre-1, parametre-2 ...`

</details>

---

### Question 7
Quelle est la différence entre un appel statique et un appel dynamique ?

- [ ] A) Statique = nom en littéral, Dynamique = nom en variable
- [ ] B) Statique = plus rapide, Dynamique = plus lent
- [ ] C) Statique = CALL 'NOM', Dynamique = CALL WS-NOM
- [ ] D) Toutes ces réponses

<details>
<summary>Réponse</summary>

**D** - Appel statique : le nom est un littéral (`CALL 'NOM'`). Appel dynamique : le nom est dans une variable (`CALL WS-NOM`). L'appel statique est légèrement plus rapide.

</details>

---

### Question 8
Quelle clause gère l'erreur "programme non trouvé" lors d'un CALL ?

- [ ] A) INVALID KEY
- [ ] B) AT END
- [ ] C) ON EXCEPTION
- [ ] D) ON ERROR

<details>
<summary>Réponse</summary>

**C** - La clause `ON EXCEPTION` est déclenchée si le programme appelé n'est pas trouvé.

</details>

---

### Question 9
Comment passe-t-on des paramètres à un sous-programme par défaut ?

- [ ] A) BY VALUE
- [ ] B) BY CONTENT
- [ ] C) BY REFERENCE
- [ ] D) BY COPY

<details>
<summary>Réponse</summary>

**C** - Si la clause BY n'est pas spécifiée, les variables sont **toujours passées par référence** (BY REFERENCE).

</details>

---

### Question 10
Que permet ON OVERFLOW dans un CALL ?

- [ ] A) Gérer les erreurs de calcul
- [ ] B) Gérer le débordement mémoire
- [ ] C) Gérer les paramètres invalides
- [ ] D) Gérer les erreurs de fichier

<details>
<summary>Réponse</summary>

**B** - `ON OVERFLOW` gère les situations de débordement mémoire (rare en pratique).

</details>

---

## Section 3 : Modes de passage de paramètres (Questions 11-16)

### Question 11
Avec BY REFERENCE, les modifications du sous-programme sont-elles visibles dans l'appelant ?

- [ ] A) Non, jamais
- [ ] B) Oui, toujours
- [ ] C) Seulement si le sous-programme utilise RETURN
- [ ] D) Seulement avec EXIT PROGRAM

<details>
<summary>Réponse</summary>

**B** - Avec BY REFERENCE, le sous-programme reçoit l'adresse mémoire. Toute modification affecte directement la variable de l'appelant.

</details>

---

### Question 12
Avec BY CONTENT, que reçoit le sous-programme ?

- [ ] A) L'adresse mémoire de la variable
- [ ] B) Une copie de la valeur
- [ ] C) Un pointeur vers la variable
- [ ] D) La variable elle-même

<details>
<summary>Réponse</summary>

**B** - BY CONTENT passe une **copie** de la valeur. Les modifications dans le sous-programme n'affectent pas l'appelant.

</details>

---

### Question 13
Pour quel usage BY VALUE est-il principalement utilisé ?

- [ ] A) Paramètres d'entrée/sortie
- [ ] B) Paramètres d'entrée seule
- [ ] C) Interfaçage avec des programmes C
- [ ] D) Optimisation des performances

<details>
<summary>Réponse</summary>

**C** - BY VALUE suit les conventions C et est utilisé pour interfacer avec des programmes C ou des API système.

</details>

---

### Question 14
Quel mode de passage utiliser pour un paramètre qui doit être modifié par le sous-programme ?

- [ ] A) BY VALUE
- [ ] B) BY CONTENT
- [ ] C) BY REFERENCE
- [ ] D) BY OUTPUT

<details>
<summary>Réponse</summary>

**C** - BY REFERENCE est le seul mode où les modifications sont visibles dans l'appelant. Utilisé pour les paramètres entrée/sortie.

</details>

---

### Question 15
Peut-on combiner différents modes de passage dans un même CALL ?

- [ ] A) Non, un seul mode par CALL
- [ ] B) Oui, chaque paramètre peut avoir son mode
- [ ] C) Oui, mais seulement BY REFERENCE et BY CONTENT
- [ ] D) Non, c'est une erreur de syntaxe

<details>
<summary>Réponse</summary>

**B** - On peut combiner les modes : `CALL 'PGM' USING BY REFERENCE var1 BY CONTENT var2`.

</details>

---

### Question 16
Quel est l'équivalent de "passage par valeur" en termes de comportement pour protéger une variable ?

- [ ] A) BY REFERENCE
- [ ] B) BY CONTENT
- [ ] C) BY VALUE
- [ ] D) B et C

<details>
<summary>Réponse</summary>

**D** - BY CONTENT et BY VALUE protègent tous deux la variable de l'appelant contre les modifications.

</details>

---

## Section 4 : LINKAGE SECTION et Structure (Questions 17-21)

### Question 17
Dans quelle section déclare-t-on les paramètres reçus par un sous-programme ?

- [ ] A) WORKING-STORAGE SECTION
- [ ] B) FILE SECTION
- [ ] C) LINKAGE SECTION
- [ ] D) LOCAL-STORAGE SECTION

<details>
<summary>Réponse</summary>

**C** - Les paramètres reçus par un sous-programme sont déclarés dans la **LINKAGE SECTION**.

</details>

---

### Question 18
Les variables de la LINKAGE SECTION peuvent-elles avoir une clause VALUE ?

- [ ] A) Oui, obligatoirement
- [ ] B) Oui, optionnellement
- [ ] C) Non, c'est interdit
- [ ] D) Seulement pour les niveau 88

<details>
<summary>Réponse</summary>

**C** - Les éléments de la LINKAGE SECTION ne doivent pas avoir de clause VALUE.

</details>

---

### Question 19
L'ordre des paramètres dans PROCEDURE DIVISION USING doit-il correspondre à l'ordre du CALL ?

- [ ] A) Non, l'ordre n'a pas d'importance
- [ ] B) Oui, l'ordre doit être identique
- [ ] C) Non, COBOL les associe par nom
- [ ] D) Seulement pour BY REFERENCE

<details>
<summary>Réponse</summary>

**B** - L'ordre des paramètres dans PROCEDURE DIVISION USING doit être **identique** à l'ordre spécifié dans le CALL.

</details>

---

### Question 20
Quelle convention de nommage est recommandée pour les variables LINKAGE ?

- [ ] A) Préfixe WS-
- [ ] B) Préfixe LK- ou LS-
- [ ] C) Préfixe P-
- [ ] D) Aucune convention

<details>
<summary>Réponse</summary>

**B** - On utilise généralement le préfixe LK- (Linkage) ou LS- (Linkage Section) pour distinguer les paramètres des variables locales.

</details>

---

### Question 21
La LINKAGE SECTION peut-elle apparaître dans un programme principal ?

- [ ] A) Oui, toujours
- [ ] B) Non, uniquement dans les sous-programmes
- [ ] C) Oui, pour recevoir les paramètres JCL
- [ ] D) B et C

<details>
<summary>Réponse</summary>

**D** - La LINKAGE SECTION est normalement réservée aux sous-programmes, mais peut aussi être utilisée dans un programme principal pour recevoir des paramètres JCL (PARM).

</details>

---

## Section 5 : Instructions de retour et contrôle (Questions 22-25)

### Question 22
Quelle instruction utilise-t-on pour retourner au programme appelant ?

- [ ] A) RETURN
- [ ] B) EXIT PROGRAM
- [ ] C) STOP RUN
- [ ] D) GO BACK

<details>
<summary>Réponse</summary>

**B et D** - `EXIT PROGRAM` et `GOBACK` retournent au programme appelant. STOP RUN termine tout.

</details>

---

### Question 23
Que fait STOP RUN dans un sous-programme ?

- [ ] A) Retourne à l'appelant
- [ ] B) Termine le sous-programme uniquement
- [ ] C) Termine toute la chaîne d'appels
- [ ] D) Génère une erreur

<details>
<summary>Réponse</summary>

**C** - STOP RUN termine immédiatement **toute la chaîne d'appels**, y compris le programme principal. Ne jamais l'utiliser dans un sous-programme.

</details>

---

### Question 24
À quoi sert l'instruction CANCEL ?

- [ ] A) Annuler un CALL
- [ ] B) Libérer un sous-programme de la mémoire
- [ ] C) Terminer un sous-programme
- [ ] D) Supprimer un programme

<details>
<summary>Réponse</summary>

**B** - CANCEL libère un sous-programme de la mémoire. Au prochain CALL, il sera rechargé et réinitialisé.

</details>

---

### Question 25
Quelle est la différence entre EXIT PROGRAM et GOBACK ?

- [ ] A) Aucune différence
- [ ] B) EXIT PROGRAM est ignoré dans un programme principal, GOBACK le termine
- [ ] C) GOBACK est plus ancien
- [ ] D) EXIT PROGRAM ne fonctionne qu'avec BY REFERENCE

<details>
<summary>Réponse</summary>

**B** - Dans un sous-programme, les deux retournent à l'appelant. Mais dans un programme principal, EXIT PROGRAM est ignoré alors que GOBACK termine le programme.

</details>

---

## Résumé des scores

| Score | Niveau |
|-------|--------|
| 23-25 | Excellent - Maîtrise complète |
| 18-22 | Bien - Quelques révisions mineures |
| 13-17 | Moyen - Relire le chapitre |
| < 13 | Insuffisant - Revoir le cours en détail |

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [QCM Chapitre VIII](qcm-08-operations-es.md) | [QCM Chapitre X](qcm-10-traitement-fichiers.md) |

---
*Formation COBOL - M2i Formation*
