# Chapitre IX - Programmes et Sous-Programmes

Ce chapitre couvre la modularisation en COBOL : comment structurer un programme en unités indépendantes (sous-programmes) et comment les appeler depuis un programme principal.

---

## IX-1 Concepts fondamentaux

### Programme appelant vs Programme appelé

| Terme | Description |
|-------|-------------|
| **Programme APPELANT** | Programme principal qui fait appel à un sous-programme |
| **Programme APPELÉ (sous-programme)** | Module exécuté via une instruction CALL |

### Avantages de la modularisation

1. **Réutilisation** - Un même sous-programme peut être appelé par plusieurs programmes
2. **Maintenance** - Modifications localisées dans le sous-programme
3. **Lisibilité** - Programmes plus courts et plus clairs
4. **Tests** - Chaque module peut être testé indépendamment
5. **Travail d'équipe** - Plusieurs développeurs peuvent travailler en parallèle

### Types de sous-programmes

| Type | Description |
|------|-------------|
| **Interne** | Défini dans le même fichier source (NESTED PROGRAM) |
| **Externe** | Compilé séparément, lié à l'exécution |

---

## IX-2 L'instruction CALL

### Syntaxe générale

```cobol
       CALL 'nom-programme' [USING parametre-1 parametre-2 ...]
           [ON OVERFLOW instruction]
           [ON EXCEPTION instruction]
           [NOT ON EXCEPTION instruction]
       END-CALL
```

### Appel simple (sans paramètres)

```cobol
      * Appel statique (nom en littéral)
       CALL 'SOUSPROG'
       END-CALL

      * Appel dynamique (nom en variable)
       MOVE 'SOUSPROG' TO WS-NOM-PGM
       CALL WS-NOM-PGM
       END-CALL
```

### Appel avec paramètres

```cobol
       WORKING-STORAGE SECTION.
       01  WS-CODE-CLIENT    PIC 9(5).
       01  WS-NOM-CLIENT     PIC X(30).
       01  WS-SOLDE          PIC 9(7)V99.

       PROCEDURE DIVISION.
           MOVE 12345 TO WS-CODE-CLIENT
           CALL 'RECHERCH' USING WS-CODE-CLIENT
                                 WS-NOM-CLIENT
                                 WS-SOLDE
           END-CALL
           DISPLAY 'Client : ' WS-NOM-CLIENT
           DISPLAY 'Solde  : ' WS-SOLDE
```

### Gestion des erreurs d'appel

```cobol
      * ON EXCEPTION : si le programme n'est pas trouvé
       CALL 'SOUSPROG'
           ON EXCEPTION
               DISPLAY 'Erreur : Programme non trouve'
               MOVE 99 TO WS-CODE-RETOUR
           NOT ON EXCEPTION
               DISPLAY 'Appel reussi'
       END-CALL

      * ON OVERFLOW : débordement mémoire (rare)
       CALL 'GROSSPGM'
           ON OVERFLOW
               DISPLAY 'Memoire insuffisante'
       END-CALL
```

---

## IX-3 Modes de passage de paramètres

COBOL offre trois modes de passage de paramètres lors d'un CALL :

### BY REFERENCE (par défaut)

Le sous-programme reçoit **l'adresse mémoire** de la variable. Toute modification dans le sous-programme **affecte** la variable de l'appelant.

```cobol
      * Programme APPELANT
       WORKING-STORAGE SECTION.
       01  WS-COMPTEUR       PIC 9(5) VALUE 10.

       PROCEDURE DIVISION.
           DISPLAY 'Avant : ' WS-COMPTEUR
           CALL 'INCREMENTER' USING BY REFERENCE WS-COMPTEUR
           DISPLAY 'Apres : ' WS-COMPTEUR
           STOP RUN.

      *> Sortie :
      *> Avant : 00010
      *> Apres : 00011
```

```cobol
      * Sous-programme INCREMENTER
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INCREMENTER.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-COMPTEUR       PIC 9(5).

       PROCEDURE DIVISION USING LK-COMPTEUR.
           ADD 1 TO LK-COMPTEUR
           EXIT PROGRAM.
```

### BY CONTENT (par valeur copiée)

Le sous-programme reçoit une **copie** de la valeur. Les modifications dans le sous-programme **n'affectent PAS** la variable de l'appelant.

```cobol
      * Programme APPELANT
       WORKING-STORAGE SECTION.
       01  WS-TOTAL          PIC 9(7)V99 VALUE 1000.00.

       PROCEDURE DIVISION.
           DISPLAY 'Avant : ' WS-TOTAL
           CALL 'CALCULER' USING BY CONTENT WS-TOTAL
           DISPLAY 'Apres : ' WS-TOTAL
           STOP RUN.

      *> Sortie :
      *> Avant : 0001000.00
      *> Apres : 0001000.00   (inchangé)
```

### BY VALUE (pour appels vers C/autres langages)

Passage **par valeur** selon les conventions C. Utilisé principalement pour interfacer avec des programmes C ou des API système.

```cobol
      * Appel d'une fonction C
       CALL 'fonction_c' USING BY VALUE WS-ENTIER
                               BY VALUE WS-POINTEUR
```

### Tableau comparatif

| Mode | Reçoit | Modifications visibles | Usage |
|------|--------|------------------------|-------|
| **BY REFERENCE** | Adresse | ✅ Oui | Paramètres entrée/sortie |
| **BY CONTENT** | Copie | ❌ Non | Paramètres entrée seule |
| **BY VALUE** | Valeur (convention C) | ❌ Non | Interfaçage avec C |

### Combinaison des modes

```cobol
       CALL 'TRAITEMENT'
           USING BY REFERENCE WS-CODE-RETOUR
                 BY CONTENT   WS-DONNEES-ENTREE
                 BY REFERENCE WS-RESULTAT
       END-CALL
```

### Démonstration : BY REFERENCE vs BY CONTENT

**Programme Principal :**
```cobol
       WORKING-STORAGE SECTION.
       01 WS-NUMCARTE    PIC 9(10) VALUE 1111111111.
       01 WS-NAME-CLT    PIC X(15) VALUE 'JEAN'.

       PROCEDURE DIVISION.
           DISPLAY 'AVANT APPEL : '
           DISPLAY 'NUMERO : ' WS-NUMCARTE
           DISPLAY 'NOM    : ' WS-NAME-CLT

           CALL 'MODIFIER' USING WS-NUMCARTE, WS-NAME-CLT.

           DISPLAY 'APRES APPEL : '
           DISPLAY 'NUMERO : ' WS-NUMCARTE
           DISPLAY 'NOM    : ' WS-NAME-CLT
           STOP RUN.
```

**Sous-Programme MODIFIER :**
```cobol
       LINKAGE SECTION.
       01 LS-NUMCARTE    PIC 9(10).
       01 LS-NAME-CLT    PIC X(15).

       PROCEDURE DIVISION USING LS-NUMCARTE, LS-NAME-CLT.
           MOVE 2222222222   TO LS-NUMCARTE.
           MOVE 'JEAN-MICHEL' TO LS-NAME-CLT.
           EXIT PROGRAM.
```

**Résultat avec BY REFERENCE (par défaut) :**
```
AVANT APPEL :
NUMERO : 1111111111
NOM    : JEAN
APRES APPEL :
NUMERO : 2222222222    ← Valeur MODIFIÉE
NOM    : JEAN-MICHEL   ← Valeur MODIFIÉE
```

**Résultat avec BY CONTENT :**
```
AVANT APPEL :
NUMERO : 1111111111
NOM    : JEAN
APRES APPEL :
NUMERO : 1111111111    ← Valeur INCHANGÉE
NOM    : JEAN          ← Valeur INCHANGÉE
```

---

## IX-4 LINKAGE SECTION

La **LINKAGE SECTION** est la zone de données du sous-programme qui reçoit les paramètres passés par l'appelant.

### Syntaxe

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables locales au sous-programme
       01  WS-VARIABLE-LOCALE   PIC X(10).

       LINKAGE SECTION.
      * Paramètres reçus de l'appelant
       01  LK-PARAMETRE-1       PIC 9(5).
       01  LK-PARAMETRE-2       PIC X(30).
       01  LK-PARAMETRE-3       PIC 9(7)V99.
```

### Règles importantes

1. **Pas d'initialisation** - Les variables LINKAGE ne peuvent pas avoir de VALUE
2. **Ordre correspondant** - L'ordre dans LINKAGE doit correspondre à l'ordre du CALL
3. **Taille compatible** - Les tailles doivent correspondre entre appelant et appelé
4. **Convention LK-** - Préfixer avec LK- pour distinguer de WORKING-STORAGE

### Exemple complet

```cobol
      * Programme APPELANT
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINCIPAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-A    PIC 9(3) VALUE 100.
       01  WS-B    PIC 9(3) VALUE 50.
       01  WS-RES  PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
           CALL 'ADDITION' USING WS-A WS-B WS-RES
           DISPLAY 'Resultat : ' WS-RES
           STOP RUN.
```

```cobol
      * Sous-programme ADDITION
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDITION.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-NOMBRE1    PIC 9(3).
       01  LK-NOMBRE2    PIC 9(3).
       01  LK-RESULTAT   PIC 9(5).

       PROCEDURE DIVISION USING LK-NOMBRE1 LK-NOMBRE2 LK-RESULTAT.
           COMPUTE LK-RESULTAT = LK-NOMBRE1 + LK-NOMBRE2
           EXIT PROGRAM.
```

---

## IX-5 PROCEDURE DIVISION USING

Dans le sous-programme, la clause **USING** de la PROCEDURE DIVISION indique quels paramètres sont reçus.

### Syntaxe

```cobol
       PROCEDURE DIVISION USING parametre-1 [parametre-2 ...]
```

### Correspondance avec LINKAGE

```cobol
       LINKAGE SECTION.
       01  LK-PARAM-A    PIC 9(5).        *> 1er paramètre
       01  LK-PARAM-B    PIC X(30).       *> 2ème paramètre
       01  LK-PARAM-C    PIC 9(7)V99.     *> 3ème paramètre

       PROCEDURE DIVISION USING LK-PARAM-A
                                LK-PARAM-B
                                LK-PARAM-C.
```

### Avec mode explicite

```cobol
       PROCEDURE DIVISION
           USING BY REFERENCE LK-PARAM-A
                 BY VALUE     LK-PARAM-B.
```

---

## IX-6 Instructions de retour

### EXIT PROGRAM

Retourne le contrôle au programme appelant. **Doit être utilisé dans un sous-programme.**

```cobol
       PROCEDURE DIVISION USING LK-PARAM.
           IF LK-PARAM < 0
               MOVE 'E' TO LK-STATUS
               EXIT PROGRAM           *> Retour anticipé
           END-IF

           PERFORM TRAITEMENT
           MOVE 'S' TO LK-STATUS
           EXIT PROGRAM.              *> Retour normal
```

### GOBACK

Alternative plus moderne à EXIT PROGRAM. Fonctionne dans les programmes principaux ET les sous-programmes.

```cobol
      * Dans un sous-programme
       PROCEDURE DIVISION USING LK-PARAM.
           PERFORM TRAITEMENT
           GOBACK.                    *> Retourne à l'appelant

      * Dans un programme principal
       PROCEDURE DIVISION.
           PERFORM TRAITEMENT
           GOBACK.                    *> Termine le programme
```

### Comparaison EXIT PROGRAM vs GOBACK

| Instruction | Programme principal | Sous-programme |
|-------------|---------------------|----------------|
| **EXIT PROGRAM** | Ignoré (continue) | Retourne à l'appelant |
| **GOBACK** | Termine le programme | Retourne à l'appelant |
| **STOP RUN** | Termine tout | Termine tout (y compris l'appelant!) |

**Recommandation :** Utilisez **GOBACK** dans les sous-programmes, **STOP RUN** uniquement dans le programme principal.

### STOP RUN - Attention !

```cobol
      * DANGER : STOP RUN dans un sous-programme
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOUSPROG.

       PROCEDURE DIVISION.
           PERFORM TRAITEMENT
           STOP RUN.          *> TERMINE TOUT ! Y compris l'appelant !
```

**STOP RUN** termine immédiatement **toute la chaîne d'appels**, pas seulement le sous-programme actuel.

---

## IX-7 CANCEL - Libération de sous-programme

L'instruction **CANCEL** libère un sous-programme de la mémoire. Au prochain CALL, il sera rechargé et réinitialisé.

### Syntaxe

```cobol
       CANCEL 'nom-programme'
       CANCEL variable-nom-programme
```

### Utilité

```cobol
       WORKING-STORAGE SECTION.
       01  WS-COMPTEUR    PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
      * Premier appel : WS-COMPTEUR initialisé à 0
           CALL 'COMPTEUR' USING WS-COMPTEUR
           DISPLAY 'Premier appel : ' WS-COMPTEUR

      * Deuxième appel : garde l'état précédent
           CALL 'COMPTEUR' USING WS-COMPTEUR
           DISPLAY 'Deuxieme appel : ' WS-COMPTEUR

      * Libérer le sous-programme
           CANCEL 'COMPTEUR'

      * Troisième appel : réinitialisé !
           CALL 'COMPTEUR' USING WS-COMPTEUR
           DISPLAY 'Apres CANCEL : ' WS-COMPTEUR
           STOP RUN.
```

### Quand utiliser CANCEL ?

| Situation | Utiliser CANCEL |
|-----------|-----------------|
| Libérer de la mémoire | ✅ Oui |
| Réinitialiser un sous-programme | ✅ Oui |
| Appels fréquents au même sous-programme | ❌ Non (surcoût) |

---

## IX-8 Programmes imbriqués (NESTED)

Un programme peut contenir des sous-programmes **internes**.

### Syntaxe

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINCIPAL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESULTAT    PIC 9(5).

       PROCEDURE DIVISION.
           CALL 'CALCUL' USING WS-RESULTAT
           DISPLAY 'Resultat : ' WS-RESULTAT
           STOP RUN.

      *-------------------------------------------------
      * Sous-programme IMBRIQUE
      *-------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCUL.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-RES    PIC 9(5).

       PROCEDURE DIVISION USING LK-RES.
           COMPUTE LK-RES = 2 * 25
           EXIT PROGRAM.
       END PROGRAM CALCUL.

       END PROGRAM PRINCIPAL.
```

### Visibilité avec COMMON

Par défaut, un programme imbriqué n'est visible que par son parent. Avec **COMMON**, il devient visible par les "frères".

```cobol
       PROGRAM-ID. UTILITAIRE IS COMMON.
```

---

## IX-9 END PROGRAM

L'instruction **END PROGRAM** marque la fin du code source d'un programme.

### Syntaxe

```cobol
       END PROGRAM nom-programme.
```

### Obligatoire pour

- Programmes imbriqués (NESTED)
- Fichiers contenant plusieurs programmes

### Exemple avec plusieurs programmes

```cobol
      * Fichier contenant 3 programmes
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMME1.
       ...
       END PROGRAM PROGRAMME1.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMME2.
       ...
       END PROGRAM PROGRAMME2.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAMME3.
       ...
       END PROGRAM PROGRAMME3.
```

---

## IX-10 Passage de structures complexes

### Passage d'une structure (groupe)

```cobol
      * Programme APPELANT
       01  WS-CLIENT.
           05  WS-CODE      PIC 9(5).
           05  WS-NOM       PIC X(30).
           05  WS-SOLDE     PIC 9(7)V99.

       PROCEDURE DIVISION.
           MOVE 12345 TO WS-CODE
           MOVE 'DUPONT' TO WS-NOM
           MOVE 1500.00 TO WS-SOLDE
           CALL 'AFFICHER' USING WS-CLIENT
```

```cobol
      * Sous-programme AFFICHER
       LINKAGE SECTION.
       01  LK-CLIENT.
           05  LK-CODE      PIC 9(5).
           05  LK-NOM       PIC X(30).
           05  LK-SOLDE     PIC 9(7)V99.

       PROCEDURE DIVISION USING LK-CLIENT.
           DISPLAY 'Code   : ' LK-CODE
           DISPLAY 'Nom    : ' LK-NOM
           DISPLAY 'Solde  : ' LK-SOLDE
           EXIT PROGRAM.
```

### Passage de tableaux

```cobol
      * Programme APPELANT
       01  WS-TABLEAU.
           05  WS-ELEMENT   PIC 9(5) OCCURS 10 TIMES.

       PROCEDURE DIVISION.
           CALL 'TRIERTAB' USING WS-TABLEAU
```

```cobol
      * Sous-programme TRIERTAB
       LINKAGE SECTION.
       01  LK-TABLEAU.
           05  LK-ELEMENT   PIC 9(5) OCCURS 10 TIMES.

       PROCEDURE DIVISION USING LK-TABLEAU.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10
               DISPLAY 'Element ' WS-I ' : ' LK-ELEMENT(WS-I)
           END-PERFORM
           EXIT PROGRAM.
```

---

## IX-11 Code retour et communication

### Pattern avec code retour

```cobol
      * Programme APPELANT
       01  WS-CODE-RETOUR    PIC 99 VALUE 0.
           88  TRAITEMENT-OK VALUE 0.
           88  ERREUR-DONNEES VALUE 10.
           88  ERREUR-FICHIER VALUE 20.

       PROCEDURE DIVISION.
           CALL 'VALIDER' USING WS-DONNEES WS-CODE-RETOUR

           EVALUATE TRUE
               WHEN TRAITEMENT-OK
                   DISPLAY 'Traitement reussi'
               WHEN ERREUR-DONNEES
                   DISPLAY 'Erreur de donnees'
               WHEN ERREUR-FICHIER
                   DISPLAY 'Erreur fichier'
               WHEN OTHER
                   DISPLAY 'Erreur inconnue : ' WS-CODE-RETOUR
           END-EVALUATE
```

```cobol
      * Sous-programme VALIDER
       LINKAGE SECTION.
       01  LK-DONNEES        PIC X(100).
       01  LK-CODE-RETOUR    PIC 99.

       PROCEDURE DIVISION USING LK-DONNEES LK-CODE-RETOUR.
           MOVE 0 TO LK-CODE-RETOUR

           IF LK-DONNEES = SPACES
               MOVE 10 TO LK-CODE-RETOUR
               EXIT PROGRAM
           END-IF

           PERFORM TRAITEMENT
           EXIT PROGRAM.
```

---

## IX-12 Compilation et Édition de liens

### JCL de compilation standard (programme simple)

```jcl
//COMPPGM  JOB COMPIL,'COMPPGM',MSGLEVEL=(1,1),REGION=4M,
//         MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*========================================================
//* COMPILATION PROGRAMME SIMPLE
//*========================================================
//COMPIL   EXEC IGYWCL
//COBOL.SYSIN DD DSN=FTEST.COBOL.SOURCE(PGMMAIN),DISP=SHR
//LKED.SYSLMOD DD DSN=FTEST.COBOL.LOAD(PGMMAIN),DISP=SHR
/*
```

### JCL avec édition de liens (programme + sous-programme)

Pour lier un sous-programme au programme principal :

```jcl
//CLNKPGM  JOB COMPLNK,'COMPLNK',MSGLEVEL=(1,1),REGION=4M,
//         MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*========================================================
//* COMPILATION AVEC GENERATION OBJET
//*========================================================
//COMPIL   EXEC PGM=IGYCRCTL,PARM='OBJECT'
//STEPLIB  DD DSN=IGY410.SIGYCOMP,DISP=SHR
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSPRINT DD SYSOUT=*
//COBOL.SYSIN DD DSN=FTEST.COBOL.SOURCE(PGSECOND),DISP=SHR
//COBOL.SYSLIN DD DSN=FTEST.COBOL.LINK(PGSECOND),DISP=SHR
//*========================================================
//* ETAPE DE LINKAGE DES PROGRAMMES
//*========================================================
//LKED     EXEC PGM=IEWBLINK,COND=(8,LT,COMPIL),REGION=0M
//SYSLIB   DD DSN=CEE.SCEELKED,DISP=SHR
//         DD DSN=CEE.SCEELKEX,DISP=SHR
//         DD DSN=FTEST.COBOL.LINK,DISP=SHR
//SYSPRINT DD SYSOUT=*
//LKED.SYSLIN DD DSN=FTEST.COBOL.LINK(PGSECOND),DISP=SHR
//SYSLMOD  DD DSN=FTEST.COBOL.LOAD(PGSECOND),DISP=SHR
//SYSUT1   DD UNIT=SYSALLDA,SPACE=(CYL,(1,1))
/*
```

### JCL d'exécution avec passage de paramètres (BY VALUE)

```jcl
//JCLVAL   JOB JCLCAL,'JCLVAL',MSGLEVEL=(1,1),REGION=4M,
//         MSGCLASS=A,CLASS=A,NOTIFY=&SYSUID
//*========================================================
//* EXECUTION AVEC PARAMETRE PARM
//*========================================================
//JOBLIB   DD DSN=FTEST.COBOL.LOAD,DISP=SHR
//STEPVAL  EXEC PGM=VALSECND,PARM=(9999999999AAAAAAAAAAAAAAA)
//SYSOUT   DD SYSOUT=C
/*
```

---

## IX-13 Bonnes pratiques

### Conventions de nommage

```cobol
      * LINKAGE : préfixe LK-
       LINKAGE SECTION.
       01  LK-PARAMETRE-1    PIC X(10).

      * Variables locales : préfixe WS-
       WORKING-STORAGE SECTION.
       01  WS-VARIABLE       PIC 9(5).
```

### Documentation du sous-programme

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULER.
      *================================================================
      * SOUS-PROGRAMME : CALCULER
      *
      * DESCRIPTION : Calcule la TVA et le montant TTC
      *
      * PARAMETRES :
      *   - LK-MONTANT-HT  (IN)  : Montant hors taxes
      *   - LK-TAUX-TVA    (IN)  : Taux de TVA (ex: 20.00)
      *   - LK-MONTANT-TVA (OUT) : Montant de la TVA calculee
      *   - LK-MONTANT-TTC (OUT) : Montant TTC
      *   - LK-CODE-RETOUR (OUT) : 0=OK, 10=Erreur
      *
      * APPELE PAR : FACTURATION, DEVIS
      *================================================================
```

### Structure recommandée d'un sous-programme

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOUSPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Variables de travail locales
       01  WS-TEMP           PIC X(10).

       LINKAGE SECTION.
      * Paramètres d'entrée (IN)
       01  LK-PARAM-IN       PIC X(20).
      * Paramètres de sortie (OUT)
       01  LK-PARAM-OUT      PIC X(30).
      * Code retour (toujours en dernier)
       01  LK-CODE-RETOUR    PIC 99.

       PROCEDURE DIVISION USING LK-PARAM-IN
                                LK-PARAM-OUT
                                LK-CODE-RETOUR.
      * Initialisation
           MOVE 0 TO LK-CODE-RETOUR
           INITIALIZE LK-PARAM-OUT

      * Validation des entrées
           IF LK-PARAM-IN = SPACES
               MOVE 10 TO LK-CODE-RETOUR
               GOBACK
           END-IF

      * Traitement
           PERFORM TRAITEMENT-PRINCIPAL

      * Retour
           GOBACK.

       TRAITEMENT-PRINCIPAL.
           *> Logique métier ici
           CONTINUE.
```

---

## Tableau récapitulatif

| Instruction | Description | Usage |
|-------------|-------------|-------|
| **CALL** | Appelle un sous-programme | Programme appelant |
| **USING** | Passe des paramètres | CALL et PROCEDURE DIVISION |
| **BY REFERENCE** | Passage par adresse (modifiable) | Paramètres entrée/sortie |
| **BY CONTENT** | Passage par copie (non modifiable) | Paramètres entrée seule |
| **BY VALUE** | Passage par valeur (convention C) | Interfaçage C |
| **LINKAGE SECTION** | Déclare les paramètres reçus | Sous-programme |
| **EXIT PROGRAM** | Retourne à l'appelant | Sous-programme |
| **GOBACK** | Retourne (ou termine) | Les deux |
| **STOP RUN** | Termine tout | Programme principal uniquement |
| **CANCEL** | Libère un sous-programme | Programme appelant |
| **END PROGRAM** | Fin du code source | Programmes imbriqués |

---

## Exercices pratiques

### Exercice 1 : Calcul du Revenu Annuel (PERSREV/CALREV)

Développer deux programmes où l'un appelle l'autre.

**Programme Principal (PERSREV) :**

1. Lecture d'un Data Set ESDS `PERSONNEL.ESDS.REVENU` avec les champs suivants :

| Champ | Type | Taille |
|-------|------|--------|
| Matricule | Numérique | 6 caractères |
| Nom | Alphabétique | 15 caractères |
| Prénom | Alphabétique | 15 caractères |
| Salaire | Numérique | 6 caractères |
| Primes | Numérique | 6 caractères |
| Revenu annuel | Numérique | 8 caractères |
| Numéro S-Social | Numérique | 10 caractères |

2. Appel du sous-programme CALREV pour calculer le revenu annuel
3. Affectation du résultat dans le champ Revenu annuel
4. Édition de l'enregistrement

**Sous-Programme (CALREV) :**

Calcul : `Revenu annuel = (12 × Salaire) + Primes`

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALREV.

       DATA DIVISION.
       LINKAGE SECTION.
       01 LS-SALAIRE         PIC 9(6).
       01 LS-PRIMES          PIC 9(6).
       01 LS-REVENU          PIC 9(8).

       PROCEDURE DIVISION USING LS-SALAIRE, LS-PRIMES, LS-REVENU.
           COMPUTE LS-REVENU = (12 * LS-SALAIRE) + LS-PRIMES.
           EXIT PROGRAM.
```

### Exercice 2 : Sous-programme de calcul TVA
Créer un sous-programme qui calcule le montant TTC à partir du HT et du taux de TVA.

### Exercice 3 : Validation de données
Créer un sous-programme qui valide un numéro de compte bancaire (format IBAN).

### Exercice 4 : Gestion d'erreurs
Créer un programme principal qui appelle plusieurs sous-programmes avec gestion des codes retour.

### Exercice 5 : Passage de tableau
Créer un sous-programme qui trie un tableau de 10 éléments en ordre croissant.

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VIII - Opérations E/S](08-operations-es.md) | [Chapitre X - Traitement Fichiers](10-traitement-fichiers.md) |

---
*Formation COBOL - Module COBOL*
