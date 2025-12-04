# Chapitre VIII - Opérations d'E/S sur les Fichiers

Ce chapitre détaille toutes les opérations de lecture, écriture, modification et suppression sur les fichiers COBOL.

---

## VIII-1 OPEN (Ouverture)

L'instruction **OPEN** établit la connexion entre le programme et le fichier physique.

### Syntaxe

```cobol
       OPEN mode nom-fichier [nom-fichier-2 ...]
```

### Modes d'ouverture

| Mode | Description | Opérations autorisées |
|------|-------------|----------------------|
| `INPUT` | Lecture seule | READ |
| `OUTPUT` | Écriture seule (crée/écrase) | WRITE |
| `I-O` | Lecture + Écriture | READ, WRITE, REWRITE, DELETE |
| `EXTEND` | Ajout en fin de fichier | WRITE |

### Compatibilité Mode × Organisation

| Organisation | INPUT | OUTPUT | I-O | EXTEND |
|--------------|-------|--------|-----|--------|
| **SEQUENTIAL** | ✅ | ✅ | ✅* | ✅ |
| **INDEXED** | ✅ | ✅ | ✅ | ✅ |
| **RELATIVE** | ✅ | ✅ | ✅ | ✅ |

*\* SEQUENTIAL en I-O : REWRITE possible mais pas DELETE*

### Exemples

```cobol
      * Ouverture simple
       OPEN INPUT FICHIER-CLIENTS

      * Ouverture multiple (même mode)
       OPEN INPUT FICHIER-CLIENTS FICHIER-PRODUITS

      * Ouverture multiple (modes différents)
       OPEN INPUT  FICHIER-ENTREE
            OUTPUT FICHIER-SORTIE
            I-O    FICHIER-MAJ

      * Avec contrôle FILE STATUS
       OPEN INPUT FICHIER-CLIENTS
       IF WS-STATUS NOT = '00'
           DISPLAY 'Erreur ouverture : ' WS-STATUS
           STOP RUN
       END-IF
```

### FILE STATUS à l'ouverture

| Code | Signification |
|------|---------------|
| `00` | Ouverture réussie |
| `05` | Fichier optionnel non présent (créé) |
| `35` | Fichier non trouvé (INPUT/I-O) |
| `37` | Mode d'ouverture incompatible |
| `39` | Conflit d'attributs fichier |
| `41` | Fichier déjà ouvert |

### Bonnes pratiques

```cobol
      * TOUJOURS vérifier le FILE STATUS après OPEN
       OPEN INPUT F-CLIENTS
       EVALUATE WS-STATUS
           WHEN '00'
               CONTINUE
           WHEN '35'
               DISPLAY 'Fichier introuvable'
               STOP RUN
           WHEN '41'
               DISPLAY 'Fichier deja ouvert'
           WHEN OTHER
               DISPLAY 'Erreur ouverture : ' WS-STATUS
               STOP RUN
       END-EVALUATE
```

---

## VIII-2 READ (Lecture)

L'instruction **READ** transfère un enregistrement du fichier vers la zone FD.

### Syntaxe générale

```cobol
       READ nom-fichier [NEXT|PREVIOUS] [INTO zone-ws]
           [KEY IS nom-clé]
           [AT END instruction]
           [NOT AT END instruction]
           [INVALID KEY instruction]
           [NOT INVALID KEY instruction]
       END-READ
```

### READ selon l'organisation et le mode d'accès

#### 1. Fichier SEQUENTIAL (ou accès SEQUENTIAL)

```cobol
      * Lecture séquentielle simple
       READ FICHIER-SEQ
           AT END SET FIN-FICHIER TO TRUE
       END-READ

      * Avec INTO (copie dans WORKING-STORAGE)
       READ FICHIER-SEQ INTO WS-ENREGISTREMENT
           AT END SET FIN-FICHIER TO TRUE
       END-READ

      * Avec NOT AT END
       READ FICHIER-SEQ
           AT END
               SET FIN-FICHIER TO TRUE
           NOT AT END
               PERFORM TRAITER-ENREGISTREMENT
       END-READ
```

#### 2. Fichier INDEXED - Accès RANDOM

```cobol
      * Lecture directe par clé primaire
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX
           INVALID KEY DISPLAY 'Client non trouve'
           NOT INVALID KEY DISPLAY 'Trouve : ' CLI-NOM
       END-READ

      * Lecture par clé explicite
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
       END-READ

      * Lecture par clé secondaire (ALTERNATE KEY)
       MOVE 'DUPONT' TO CLI-NOM
       READ FICHIER-IDX KEY IS CLI-NOM
           INVALID KEY DISPLAY 'Nom non trouve'
       END-READ
```

#### 3. Fichier INDEXED - Accès DYNAMIC

```cobol
      * Lecture directe
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
       END-READ

      * Lecture séquentielle (après positionnement)
       READ FICHIER-IDX NEXT
           AT END SET FIN-FICHIER TO TRUE
       END-READ

      * Lecture en arrière (si supporté)
       READ FICHIER-IDX PREVIOUS
           AT END SET DEBUT-FICHIER TO TRUE
       END-READ
```

#### 4. Fichier RELATIVE

```cobol
      * Accès RANDOM - par numéro d'enregistrement
       MOVE 5 TO WS-RRN
       READ FICHIER-REL
           INVALID KEY DISPLAY 'Position vide ou inexistante'
       END-READ

      * Accès SEQUENTIAL
       READ FICHIER-REL NEXT
           AT END SET FIN-FICHIER TO TRUE
       END-READ
```

### Clause INTO

Copie l'enregistrement dans une zone WORKING-STORAGE (recommandé).

```cobol
       FILE SECTION.
       FD  FICHIER-CLIENTS.
       01  ENR-CLIENT          PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-CLIENT.
           05  WS-CODE         PIC 9(4).
           05  WS-NOM          PIC X(30).
           05  FILLER          PIC X(46).

       PROCEDURE DIVISION.
      * Sans INTO : données dans ENR-CLIENT (FD)
       READ FICHIER-CLIENTS
           AT END SET FIN-FICHIER TO TRUE
       END-READ

      * Avec INTO : copie dans WS-CLIENT
       READ FICHIER-CLIENTS INTO WS-CLIENT
           AT END SET FIN-FICHIER TO TRUE
       END-READ
```

**Avantage INTO :** La zone FD peut être écrasée au prochain READ, la copie dans WS est préservée.

### Pattern classique de lecture séquentielle (Read-Ahead)

```cobol
       OPEN INPUT FICHIER-CLIENTS

       READ FICHIER-CLIENTS INTO WS-CLIENT
           AT END SET FIN-FICHIER TO TRUE
       END-READ

       PERFORM UNTIL FIN-FICHIER
           PERFORM TRAITER-CLIENT
           READ FICHIER-CLIENTS INTO WS-CLIENT
               AT END SET FIN-FICHIER TO TRUE
           END-READ
       END-PERFORM

       CLOSE FICHIER-CLIENTS
```

### FILE STATUS après READ

| Code | Signification |
|------|---------------|
| `00` | Lecture réussie |
| `10` | Fin de fichier (AT END) |
| `21` | Erreur de séquence de clé |
| `22` | Clé en double trouvée |
| `23` | Enregistrement non trouvé (INVALID KEY) |
| `46` | READ sans positionnement préalable |

### Résumé READ par contexte

| Contexte | Syntaxe | Clause |
|----------|---------|--------|
| Séquentiel | `READ fichier` | AT END |
| Séquentiel suivant | `READ fichier NEXT` | AT END |
| Direct par clé | `READ fichier KEY IS cle` | INVALID KEY |
| Direct (clé implicite) | `READ fichier` | INVALID KEY |
| Relatif direct | `READ fichier` (RRN positionné) | INVALID KEY |

---

## VIII-3 WRITE (Écriture)

L'instruction **WRITE** transfère un enregistrement de la mémoire vers le fichier.

### Syntaxe générale

```cobol
       WRITE nom-enregistrement [FROM zone-ws]
           [INVALID KEY instruction]
           [NOT INVALID KEY instruction]
       END-WRITE
```

**Important :** On écrit l'**enregistrement** (niveau 01 du FD), pas le fichier !

### WRITE selon l'organisation

#### 1. Fichier SEQUENTIAL

```cobol
      * Ouvert en OUTPUT : création/écrasement
       OPEN OUTPUT FICHIER-SEQ

       MOVE '0001' TO CLI-CODE
       MOVE 'DUPONT' TO CLI-NOM
       WRITE ENR-CLIENT

       CLOSE FICHIER-SEQ

      * Ouvert en EXTEND : ajout en fin
       OPEN EXTEND FICHIER-SEQ

       MOVE '0099' TO CLI-CODE
       MOVE 'NOUVEAU' TO CLI-NOM
       WRITE ENR-CLIENT

       CLOSE FICHIER-SEQ
```

**Note :** Pas de clause INVALID KEY pour SEQUENTIAL (pas de clé).

#### 2. Fichier INDEXED

```cobol
       OPEN OUTPUT FICHIER-IDX

      * Écriture simple
       MOVE '0001' TO CLI-CODE
       MOVE 'DUPONT' TO CLI-NOM
       WRITE ENR-CLIENT
           INVALID KEY DISPLAY 'Cle en double : ' CLI-CODE
           NOT INVALID KEY DISPLAY 'Client ajoute'
       END-WRITE

      * Avec FROM (depuis WORKING-STORAGE)
       MOVE '0002' TO WS-CODE
       MOVE 'MARTIN' TO WS-NOM
       WRITE ENR-CLIENT FROM WS-CLIENT
           INVALID KEY DISPLAY 'Erreur ecriture'
       END-WRITE

       CLOSE FICHIER-IDX
```

**INVALID KEY se produit si :**
- La clé primaire existe déjà (doublon)
- Une clé secondaire UNIQUE existe déjà

#### 3. Fichier RELATIVE

```cobol
       OPEN OUTPUT FICHIER-REL

      * Écriture à une position spécifique
       MOVE 5 TO WS-RRN
       MOVE 'ENREGISTREMENT 5' TO ENR-RELATIF
       WRITE ENR-RELATIF
           INVALID KEY DISPLAY 'Position deja occupee'
       END-WRITE

      * Écriture séquentielle (position auto-incrémentée)
       WRITE ENR-RELATIF

       CLOSE FICHIER-REL
```

### Clause FROM

Copie depuis WORKING-STORAGE avant écriture (recommandé).

```cobol
       WORKING-STORAGE SECTION.
       01  WS-CLIENT.
           05  WS-CODE         PIC 9(4).
           05  WS-NOM          PIC X(30).
           05  WS-SOLDE        PIC 9(7)V99.

       PROCEDURE DIVISION.
      * Préparer les données en WORKING-STORAGE
       MOVE '0001' TO WS-CODE
       MOVE 'DUPONT' TO WS-NOM
       MOVE 1500.00 TO WS-SOLDE

      * Écrire avec FROM
       WRITE ENR-CLIENT FROM WS-CLIENT
           INVALID KEY DISPLAY 'Erreur'
       END-WRITE
```

### WRITE pour fichiers d'impression

```cobol
       FD  FICHIER-ETAT
           RECORDING MODE IS F
           RECORD CONTAINS 132 CHARACTERS.
       01  LIGNE-ETAT          PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-LIGNE-TITRE.
           05  FILLER          PIC X(50) VALUE SPACES.
           05  FILLER          PIC X(30) VALUE 'LISTE DES CLIENTS'.
           05  FILLER          PIC X(52) VALUE SPACES.

       01  WS-LIGNE-DETAIL.
           05  FILLER          PIC X(5) VALUE SPACES.
           05  WS-DET-CODE     PIC 9(4).
           05  FILLER          PIC X(5) VALUE SPACES.
           05  WS-DET-NOM      PIC X(30).
           05  FILLER          PIC X(88) VALUE SPACES.

       PROCEDURE DIVISION.
           OPEN OUTPUT FICHIER-ETAT

      * Écrire titre
           WRITE LIGNE-ETAT FROM WS-LIGNE-TITRE

      * Écrire détails
           MOVE '0001' TO WS-DET-CODE
           MOVE 'DUPONT' TO WS-DET-NOM
           WRITE LIGNE-ETAT FROM WS-LIGNE-DETAIL

           CLOSE FICHIER-ETAT
```

### Contrôle de saut de page (impression)

```cobol
       WRITE LIGNE-ETAT FROM WS-LIGNE-TITRE
           AFTER ADVANCING PAGE

       WRITE LIGNE-ETAT FROM WS-LIGNE-DETAIL
           AFTER ADVANCING 1 LINE

       WRITE LIGNE-ETAT FROM WS-LIGNE-DETAIL
           AFTER ADVANCING 2 LINES

       WRITE LIGNE-ETAT FROM WS-LIGNE-BLANC
           BEFORE ADVANCING 3 LINES
```

| Clause | Effet |
|--------|-------|
| `AFTER ADVANCING PAGE` | Saut de page avant écriture |
| `AFTER ADVANCING n LINES` | n sauts de ligne avant écriture |
| `BEFORE ADVANCING n LINES` | n sauts de ligne après écriture |

### FILE STATUS après WRITE

| Code | Signification |
|------|---------------|
| `00` | Écriture réussie |
| `21` | Erreur de séquence (clé non croissante en OUTPUT) |
| `22` | Clé en double (INVALID KEY) |
| `24` | Dépassement limite fichier |
| `48` | WRITE sans OPEN OUTPUT/EXTEND/I-O |

### Résumé WRITE par organisation

| Organisation | Mode OPEN | INVALID KEY | Notes |
|--------------|-----------|-------------|-------|
| SEQUENTIAL | OUTPUT | Non | Écrit en séquence |
| SEQUENTIAL | EXTEND | Non | Ajoute en fin |
| INDEXED | OUTPUT | Oui | Clé doit être croissante |
| INDEXED | I-O | Oui | Insertion n'importe où |
| RELATIVE | OUTPUT | Oui | À la position RRN |
| RELATIVE | I-O | Oui | À la position RRN |

---

## VIII-4 REWRITE (Modification)

L'instruction **REWRITE** remplace un enregistrement existant par un nouveau contenu.

### Syntaxe générale

```cobol
       REWRITE nom-enregistrement [FROM zone-ws]
           [INVALID KEY instruction]
           [NOT INVALID KEY instruction]
       END-REWRITE
```

### Règle fondamentale

**Un REWRITE doit TOUJOURS être précédé d'un READ réussi sur le même enregistrement !**

```cobol
      * CORRECT
       READ FICHIER-CLIENTS KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
           NOT INVALID KEY
               MOVE 'NOUVEAU NOM' TO CLI-NOM
               REWRITE ENR-CLIENT
       END-READ

      * INCORRECT - pas de READ préalable
       MOVE '0003' TO CLI-CODE
       MOVE 'NOUVEAU NOM' TO CLI-NOM
       REWRITE ENR-CLIENT        *> ERREUR !
```

### REWRITE selon l'organisation

#### 1. Fichier SEQUENTIAL

```cobol
       OPEN I-O FICHIER-SEQ

       READ FICHIER-SEQ
           AT END SET FIN-FICHIER TO TRUE
           NOT AT END
               MOVE 'MODIFIE' TO CLI-NOM
               REWRITE ENR-CLIENT
       END-READ

       CLOSE FICHIER-SEQ
```

**Contraintes SEQUENTIAL :**
- Fichier doit être ouvert en **I-O**
- L'enregistrement réécrit doit avoir la **même taille** que l'original
- On ne peut **pas changer la position** dans le fichier

#### 2. Fichier INDEXED

```cobol
       OPEN I-O FICHIER-IDX

      * Modification via accès direct
       MOVE '0003' TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE
           INVALID KEY
               DISPLAY 'Client non trouve'
           NOT INVALID KEY
               MOVE 'NOUVEAU NOM' TO CLI-NOM
               ADD 100 TO CLI-SOLDE
               REWRITE ENR-CLIENT
                   INVALID KEY DISPLAY 'Erreur modification'
                   NOT INVALID KEY DISPLAY 'Client modifie'
               END-REWRITE
       END-READ

       CLOSE FICHIER-IDX
```

**Règles INDEXED :**
- La **clé primaire ne peut PAS être modifiée** (sinon INVALID KEY)
- Les **clés secondaires peuvent être modifiées**
- Les autres champs peuvent être modifiés librement

#### 3. Fichier RELATIVE

```cobol
       OPEN I-O FICHIER-REL

      * Modification par position
       MOVE 5 TO WS-RRN
       READ FICHIER-REL
           INVALID KEY
               DISPLAY 'Position vide'
           NOT INVALID KEY
               MOVE 'DONNEES MODIFIEES' TO ENR-RELATIF
               REWRITE ENR-RELATIF
                   INVALID KEY DISPLAY 'Erreur'
               END-REWRITE
       END-READ

       CLOSE FICHIER-REL
```

### Clause FROM

```cobol
       WORKING-STORAGE SECTION.
       01  WS-CLIENT.
           05  WS-CODE         PIC 9(4).
           05  WS-NOM          PIC X(30).
           05  WS-SOLDE        PIC 9(7)V99.

       PROCEDURE DIVISION.
      * Lire l'enregistrement
       READ FICHIER-IDX INTO WS-CLIENT KEY IS CLI-CODE
           INVALID KEY DISPLAY 'Non trouve'
           NOT INVALID KEY
      * Modifier en WORKING-STORAGE
               ADD 500 TO WS-SOLDE
               MOVE 'NOM MODIFIE' TO WS-NOM
      * Réécrire avec FROM
               REWRITE ENR-CLIENT FROM WS-CLIENT
                   INVALID KEY DISPLAY 'Erreur'
               END-REWRITE
       END-READ
```

### Pattern de mise à jour en masse

```cobol
       OPEN I-O FICHIER-CLIENTS

       READ FICHIER-CLIENTS
           AT END SET FIN-FICHIER TO TRUE
       END-READ

       PERFORM UNTIL FIN-FICHIER
      * Appliquer une augmentation de 10%
           IF CLI-SOLDE > 0
               COMPUTE CLI-SOLDE = CLI-SOLDE * 1.10
               REWRITE ENR-CLIENT
                   INVALID KEY DISPLAY 'Erreur sur ' CLI-CODE
               END-REWRITE
           END-IF

           READ FICHIER-CLIENTS
               AT END SET FIN-FICHIER TO TRUE
           END-READ
       END-PERFORM

       CLOSE FICHIER-CLIENTS
```

### FILE STATUS après REWRITE

| Code | Signification |
|------|---------------|
| `00` | Modification réussie |
| `21` | Erreur de séquence de clé |
| `22` | Clé secondaire en double |
| `43` | Pas de READ préalable |
| `44` | Taille enregistrement différente (SEQUENTIAL) |
| `49` | REWRITE sans OPEN I-O |

### Erreurs courantes

```cobol
      * ERREUR 1 : Modifier la clé primaire
       READ FICHIER-IDX KEY IS CLI-CODE
       MOVE '9999' TO CLI-CODE        *> INTERDIT !
       REWRITE ENR-CLIENT             *> INVALID KEY

      * ERREUR 2 : REWRITE sans READ
       MOVE '0003' TO CLI-CODE
       REWRITE ENR-CLIENT             *> FILE STATUS 43

      * ERREUR 3 : Fichier ouvert en INPUT
       OPEN INPUT FICHIER-IDX
       READ FICHIER-IDX
       REWRITE ENR-CLIENT             *> FILE STATUS 49

      * ERREUR 4 : Changer la taille (SEQUENTIAL)
       READ FICHIER-SEQ               *> Enr de 80 octets
       MOVE ALL '*' TO ENR-CLIENT(1:100)  *> 100 octets
       REWRITE ENR-CLIENT             *> FILE STATUS 44
```

### Résumé REWRITE

| Organisation | Prérequis | Clé primaire | Clé secondaire |
|--------------|-----------|--------------|----------------|
| SEQUENTIAL | READ préalable, I-O, même taille | N/A | N/A |
| INDEXED | READ préalable, I-O | Non modifiable | Modifiable |
| RELATIVE | READ préalable, I-O | N/A | N/A |

---

## VIII-5 DELETE (Suppression)

L'instruction **DELETE** supprime un enregistrement du fichier.

### Syntaxe générale

```cobol
       DELETE nom-fichier [RECORD]
           [INVALID KEY instruction]
           [NOT INVALID KEY instruction]
       END-DELETE
```

**Note :** On supprime depuis le **fichier** (pas l'enregistrement comme WRITE/REWRITE).

### Disponibilité

**DELETE n'est PAS disponible pour les fichiers SEQUENTIAL !**

| Organisation | DELETE disponible |
|--------------|-------------------|
| SEQUENTIAL | ❌ Non |
| INDEXED | ✅ Oui |
| RELATIVE | ✅ Oui |

Pour supprimer dans un fichier SEQUENTIAL, il faut recopier le fichier en excluant les enregistrements à supprimer.

### DELETE selon le mode d'accès

#### 1. Accès SEQUENTIAL (fichiers INDEXED/RELATIVE)

Le DELETE supprime l'enregistrement **lu précédemment**.

```cobol
       SELECT FICHIER-IDX
           ASSIGN TO 'CLIENTS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS CLI-CODE
           FILE STATUS IS WS-STATUS.

       PROCEDURE DIVISION.
           OPEN I-O FICHIER-IDX

      * Lire puis supprimer
           READ FICHIER-IDX
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END
                   IF CLI-SOLDE = 0
                       DELETE FICHIER-IDX
                           INVALID KEY DISPLAY 'Erreur suppression'
                       END-DELETE
                   END-IF
           END-READ

           CLOSE FICHIER-IDX
```

#### 2. Accès RANDOM (fichiers INDEXED/RELATIVE)

Le DELETE supprime l'enregistrement correspondant à la **clé positionnée**.

```cobol
       SELECT FICHIER-IDX
           ASSIGN TO 'CLIENTS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CLI-CODE
           FILE STATUS IS WS-STATUS.

       PROCEDURE DIVISION.
           OPEN I-O FICHIER-IDX

      * Suppression directe par clé (pas de READ nécessaire)
           MOVE '0003' TO CLI-CODE
           DELETE FICHIER-IDX
               INVALID KEY DISPLAY 'Client non trouve'
               NOT INVALID KEY DISPLAY 'Client supprime'
           END-DELETE

           CLOSE FICHIER-IDX
```

**En mode RANDOM, pas besoin de READ préalable !**

#### 3. Accès DYNAMIC

Les deux méthodes sont possibles.

```cobol
       SELECT FICHIER-IDX
           ASSIGN TO 'CLIENTS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLI-CODE
           FILE STATUS IS WS-STATUS.

       PROCEDURE DIVISION.
           OPEN I-O FICHIER-IDX

      * Méthode 1 : Suppression directe (comme RANDOM)
           MOVE '0003' TO CLI-CODE
           DELETE FICHIER-IDX
               INVALID KEY DISPLAY 'Non trouve'
           END-DELETE

      * Méthode 2 : Après lecture (comme SEQUENTIAL)
           MOVE '0005' TO CLI-CODE
           READ FICHIER-IDX KEY IS CLI-CODE
               INVALID KEY DISPLAY 'Non trouve'
               NOT INVALID KEY
                   DELETE FICHIER-IDX
                   END-DELETE
           END-READ

           CLOSE FICHIER-IDX
```

### Fichier RELATIVE

```cobol
       SELECT FICHIER-REL
           ASSIGN TO 'DATA.REL'
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS RANDOM
           RELATIVE KEY IS WS-RRN
           FILE STATUS IS WS-STATUS.

       WORKING-STORAGE SECTION.
       01  WS-RRN    PIC 9(6).

       PROCEDURE DIVISION.
           OPEN I-O FICHIER-REL

      * Supprimer l'enregistrement en position 5
           MOVE 5 TO WS-RRN
           DELETE FICHIER-REL
               INVALID KEY DISPLAY 'Position vide ou inexistante'
               NOT INVALID KEY DISPLAY 'Enregistrement supprime'
           END-DELETE

           CLOSE FICHIER-REL
```

**Note :** La position devient "vide" (trou) mais reste dans le fichier.

### Pattern : Suppression avec confirmation

```cobol
       DISPLAY 'Code client a supprimer : '
       ACCEPT WS-CODE-SUPPR

       MOVE WS-CODE-SUPPR TO CLI-CODE
       READ FICHIER-IDX KEY IS CLI-CODE
           INVALID KEY
               DISPLAY 'Client inexistant'
           NOT INVALID KEY
               DISPLAY 'Client : ' CLI-NOM
               DISPLAY 'Confirmer suppression ? (O/N)'
               ACCEPT WS-CONFIRM
               IF WS-CONFIRM = 'O'
                   DELETE FICHIER-IDX
                       INVALID KEY DISPLAY 'Erreur'
                       NOT INVALID KEY DISPLAY 'Supprime !'
                   END-DELETE
               ELSE
                   DISPLAY 'Suppression annulee'
               END-IF
       END-READ
```

### Pattern : Suppression en masse (purge)

```cobol
      * Supprimer tous les clients avec solde = 0
       OPEN I-O FICHIER-IDX

       MOVE LOW-VALUES TO CLI-CODE
       START FICHIER-IDX KEY > CLI-CODE
           INVALID KEY SET FIN-FICHIER TO TRUE
       END-START

       PERFORM UNTIL FIN-FICHIER
           READ FICHIER-IDX NEXT
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END
                   IF CLI-SOLDE = 0
                       DELETE FICHIER-IDX
                           INVALID KEY CONTINUE
                       END-DELETE
                       ADD 1 TO WS-CPT-SUPPR
                   END-IF
           END-READ
       END-PERFORM

       DISPLAY 'Enregistrements supprimes : ' WS-CPT-SUPPR
       CLOSE FICHIER-IDX
```

### FILE STATUS après DELETE

| Code | Signification |
|------|---------------|
| `00` | Suppression réussie |
| `23` | Enregistrement non trouvé (INVALID KEY) |
| `43` | Pas de READ préalable (accès SEQUENTIAL) |
| `49` | DELETE sans OPEN I-O |

### Résumé DELETE

| Mode d'accès | READ préalable | Clé à positionner |
|--------------|----------------|-------------------|
| SEQUENTIAL | ✅ Obligatoire | Non (dernier lu) |
| RANDOM | ❌ Non nécessaire | ✅ Oui |
| DYNAMIC | Optionnel | Si pas de READ |

| Organisation | DELETE possible |
|--------------|-----------------|
| SEQUENTIAL | ❌ Non |
| INDEXED | ✅ Oui |
| RELATIVE | ✅ Oui |

---

## VIII-6 START (Positionnement)

L'instruction **START** positionne le pointeur de lecture sur un enregistrement spécifique **sans le lire**. Elle prépare une lecture séquentielle à partir d'une position donnée.

### Syntaxe générale

```cobol
       START nom-fichier KEY condition nom-clé
           [INVALID KEY instruction]
           [NOT INVALID KEY instruction]
       END-START
```

### Conditions de positionnement

| Condition | Syntaxe | Signification |
|-----------|---------|---------------|
| Égal | `KEY =` ou `KEY IS EQUAL TO` | Premier enregistrement avec clé = valeur |
| Supérieur | `KEY >` ou `KEY IS GREATER THAN` | Premier enregistrement avec clé > valeur |
| Supérieur ou égal | `KEY >=` ou `KEY IS NOT LESS THAN` | Premier avec clé >= valeur |
| Inférieur | `KEY <` ou `KEY IS LESS THAN` | Premier avec clé < valeur |
| Inférieur ou égal | `KEY <=` ou `KEY IS NOT GREATER THAN` | Premier avec clé <= valeur |

### Disponibilité

| Organisation | START disponible |
|--------------|------------------|
| SEQUENTIAL | ❌ Non |
| INDEXED | ✅ Oui |
| RELATIVE | ✅ Oui |

| Mode d'accès | START disponible |
|--------------|------------------|
| SEQUENTIAL | ❌ Non (INDEXED/RELATIVE) |
| RANDOM | ❌ Non (pas utile) |
| DYNAMIC | ✅ Oui |

### Exemples avec fichier INDEXED

#### 1. Positionnement exact

```cobol
       MOVE '0050' TO CLI-CODE
       START FICHIER-IDX KEY = CLI-CODE
           INVALID KEY
               DISPLAY 'Client 0050 non trouve'
           NOT INVALID KEY
               DISPLAY 'Positionne sur client 0050'
               READ FICHIER-IDX NEXT
                   AT END DISPLAY 'Fin'
                   NOT AT END DISPLAY CLI-CODE ' ' CLI-NOM
               END-READ
       END-START
```

#### 2. Positionnement supérieur ou égal (le plus courant)

```cobol
      * Lire tous les clients à partir du code 0100
       MOVE '0100' TO CLI-CODE
       START FICHIER-IDX KEY >= CLI-CODE
           INVALID KEY
               DISPLAY 'Aucun client >= 0100'
               SET FIN-FICHIER TO TRUE
       END-START

       PERFORM UNTIL FIN-FICHIER
           READ FICHIER-IDX NEXT
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END DISPLAY CLI-CODE ' ' CLI-NOM
           END-READ
       END-PERFORM
```

#### 3. Positionnement au début du fichier

```cobol
      * LOW-VALUES = plus petite valeur possible
       MOVE LOW-VALUES TO CLI-CODE
       START FICHIER-IDX KEY >= CLI-CODE
           INVALID KEY DISPLAY 'Fichier vide'
       END-START

      * Ou simplement
       MOVE SPACES TO CLI-CODE        *> Pour clé alphanumérique
       MOVE ZEROS TO CLI-CODE         *> Pour clé numérique
       START FICHIER-IDX KEY >= CLI-CODE
```

#### 4. Positionnement à la fin (parcours inverse)

```cobol
      * HIGH-VALUES = plus grande valeur possible
       MOVE HIGH-VALUES TO CLI-CODE
       START FICHIER-IDX KEY <= CLI-CODE
           INVALID KEY DISPLAY 'Fichier vide'
       END-START

      * Lecture en arrière (si supporté)
       PERFORM UNTIL DEBUT-FICHIER
           READ FICHIER-IDX PREVIOUS
               AT END SET DEBUT-FICHIER TO TRUE
               NOT AT END DISPLAY CLI-CODE ' ' CLI-NOM
           END-READ
       END-PERFORM
```

#### 5. Recherche dans une plage

```cobol
      * Lire les clients de 0100 à 0199
       MOVE '0100' TO CLI-CODE
       START FICHIER-IDX KEY >= CLI-CODE
           INVALID KEY
               DISPLAY 'Aucun client dans la plage'
               SET FIN-RECHERCHE TO TRUE
       END-START

       PERFORM UNTIL FIN-RECHERCHE OR FIN-FICHIER
           READ FICHIER-IDX NEXT
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END
                   IF CLI-CODE >= '0200'
                       SET FIN-RECHERCHE TO TRUE
                   ELSE
                       DISPLAY CLI-CODE ' ' CLI-NOM
                   END-IF
           END-READ
       END-PERFORM
```

### START avec clé secondaire

```cobol
       SELECT FICHIER-IDX
           ASSIGN TO 'CLIENTS.DAT'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS CLI-CODE
           ALTERNATE RECORD KEY IS CLI-NOM WITH DUPLICATES
           FILE STATUS IS WS-STATUS.

       PROCEDURE DIVISION.
      * Positionner sur la clé secondaire
       MOVE 'DUPONT' TO CLI-NOM
       START FICHIER-IDX KEY >= CLI-NOM
           INVALID KEY DISPLAY 'Aucun nom >= DUPONT'
       END-START

      * Lecture séquentielle par nom
       PERFORM UNTIL FIN-FICHIER
           READ FICHIER-IDX NEXT
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END DISPLAY CLI-NOM ' - ' CLI-CODE
           END-READ
       END-PERFORM
```

### START avec fichier RELATIVE

```cobol
       SELECT FICHIER-REL
           ASSIGN TO 'DATA.REL'
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS DYNAMIC
           RELATIVE KEY IS WS-RRN
           FILE STATUS IS WS-STATUS.

       WORKING-STORAGE SECTION.
       01  WS-RRN    PIC 9(6).

       PROCEDURE DIVISION.
      * Positionner à partir de la position 100
       MOVE 100 TO WS-RRN
       START FICHIER-REL KEY >= WS-RRN
           INVALID KEY DISPLAY 'Aucun enregistrement >= 100'
       END-START

      * Lecture séquentielle
       PERFORM UNTIL FIN-FICHIER
           READ FICHIER-REL NEXT
               AT END SET FIN-FICHIER TO TRUE
               NOT AT END DISPLAY 'Position ' WS-RRN ' : ' ENR-RELATIF
           END-READ
       END-PERFORM
```

### FILE STATUS après START

| Code | Signification |
|------|---------------|
| `00` | Positionnement réussi |
| `23` | Aucun enregistrement ne correspond (INVALID KEY) |
| `46` | START sans OPEN INPUT ou I-O |

### Points importants

1. **START ne lit pas** - il positionne seulement le pointeur
2. **READ NEXT** doit suivre pour lire l'enregistrement
3. **INVALID KEY** si aucun enregistrement ne satisfait la condition
4. Utilisé principalement en mode **DYNAMIC**
5. Permet de combiner accès direct (positionnement) et séquentiel (lecture)

### Résumé START

| Aspect | Valeur |
|--------|--------|
| Fichiers | INDEXED, RELATIVE |
| Mode d'accès | DYNAMIC (principalement) |
| Opérateurs | =, >, >=, <, <= |
| Clause erreur | INVALID KEY |
| Suivi par | READ NEXT |

---

## VIII-7 CLOSE (Fermeture)

L'instruction **CLOSE** termine la connexion entre le programme et le fichier, libère les ressources et assure l'intégrité des données.

### Syntaxe générale

```cobol
       CLOSE nom-fichier [WITH LOCK]
             [nom-fichier-2 ...]
```

### Exemples de base

```cobol
      * Fermeture simple
       CLOSE FICHIER-CLIENTS

      * Fermeture multiple
       CLOSE FICHIER-CLIENTS
             FICHIER-PRODUITS
             FICHIER-COMMANDES

      * Fermeture avec verrouillage
       CLOSE FICHIER-CLIENTS WITH LOCK
```

### Pourquoi CLOSE est important ?

| Raison | Explication |
|--------|-------------|
| **Vidage des buffers** | Les données en mémoire sont écrites sur disque |
| **Libération ressources** | Le système libère les verrous et descripteurs |
| **Intégrité des données** | Garantit que toutes les écritures sont finalisées |
| **Réouverture possible** | Le fichier peut être réouvert dans un autre mode |

### WITH LOCK

Empêche la réouverture du fichier dans le même programme.

```cobol
      * Sans LOCK : réouverture possible
       CLOSE FICHIER-CLIENTS
       OPEN INPUT FICHIER-CLIENTS    *> OK

      * Avec LOCK : réouverture interdite
       CLOSE FICHIER-CLIENTS WITH LOCK
       OPEN INPUT FICHIER-CLIENTS    *> ERREUR (FILE STATUS 38)
```

**Usage :** Sécurité pour éviter une réouverture accidentelle.

### Pattern standard de gestion de fichier

```cobol
       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INITIALISATION
           PERFORM 2000-TRAITEMENT
           PERFORM 3000-FINALISATION
           STOP RUN.

       1000-INITIALISATION.
           OPEN INPUT FICHIER-ENTREE
           IF WS-STATUS-ENT NOT = '00'
               DISPLAY 'Erreur ouverture entree : ' WS-STATUS-ENT
               STOP RUN
           END-IF

           OPEN OUTPUT FICHIER-SORTIE
           IF WS-STATUS-SOR NOT = '00'
               DISPLAY 'Erreur ouverture sortie : ' WS-STATUS-SOR
               CLOSE FICHIER-ENTREE
               STOP RUN
           END-IF.

       2000-TRAITEMENT.
           PERFORM UNTIL FIN-FICHIER
               READ FICHIER-ENTREE
                   AT END SET FIN-FICHIER TO TRUE
                   NOT AT END PERFORM 2100-TRAITER-ENR
               END-READ
           END-PERFORM.

       2100-TRAITER-ENR.
           *> Traitement...
           WRITE ENR-SORTIE.

       3000-FINALISATION.
           CLOSE FICHIER-ENTREE
           CLOSE FICHIER-SORTIE
           DISPLAY 'Traitement termine'.
```

### CLOSE et FILE STATUS

| Code | Signification |
|------|---------------|
| `00` | Fermeture réussie |
| `38` | Fichier fermé avec LOCK, réouverture interdite |
| `42` | Fichier non ouvert (CLOSE sur fichier fermé) |
| `44` | Erreur d'écriture au vidage des buffers |

### Bonnes pratiques

#### 1. Toujours fermer les fichiers ouverts

```cobol
      * MAUVAIS : oubli de fermeture
       OPEN INPUT FICHIER-CLIENTS
       PERFORM TRAITEMENT
       STOP RUN.                      *> Fichier non fermé !

      * BON : fermeture explicite
       OPEN INPUT FICHIER-CLIENTS
       PERFORM TRAITEMENT
       CLOSE FICHIER-CLIENTS
       STOP RUN.
```

#### 2. Fermer avant STOP RUN ou GOBACK

```cobol
       CLOSE FICHIER-ENTREE
             FICHIER-SORTIE
             FICHIER-ERREUR
       STOP RUN.
```

#### 3. Fermer en cas d'erreur

```cobol
       READ FICHIER-CLIENTS
           AT END SET FIN-FICHIER TO TRUE
           NOT AT END
               IF CLI-CODE = SPACES
                   DISPLAY 'Erreur donnees'
                   CLOSE FICHIER-CLIENTS
                   STOP RUN
               END-IF
       END-READ
```

#### 4. Utiliser un paragraphe de fermeture

```cobol
       9000-FERMER-FICHIERS.
           IF FICHIER-ENTREE-OUVERT
               CLOSE FICHIER-ENTREE
           END-IF
           IF FICHIER-SORTIE-OUVERT
               CLOSE FICHIER-SORTIE
           END-IF.
```

### Réouverture de fichier

```cobol
      * Premier traitement : lecture
       OPEN INPUT FICHIER-CLIENTS
       PERFORM LIRE-TOUS-CLIENTS
       CLOSE FICHIER-CLIENTS

      * Deuxième traitement : mise à jour
       OPEN I-O FICHIER-CLIENTS
       PERFORM METTRE-A-JOUR-CLIENTS
       CLOSE FICHIER-CLIENTS

      * Troisième traitement : ajouts
       OPEN EXTEND FICHIER-CLIENTS
       PERFORM AJOUTER-NOUVEAUX-CLIENTS
       CLOSE FICHIER-CLIENTS
```

---

## Tableau récapitulatif des opérations E/S

| Opération | Fichier | OPEN requis | Clause erreur |
|-----------|---------|-------------|---------------|
| **OPEN** | Tous | - | FILE STATUS |
| **READ** | Tous | INPUT, I-O | AT END, INVALID KEY |
| **WRITE** | Tous | OUTPUT, EXTEND, I-O | INVALID KEY |
| **REWRITE** | Tous | I-O | INVALID KEY |
| **DELETE** | INDEXED, RELATIVE | I-O | INVALID KEY |
| **START** | INDEXED, RELATIVE | INPUT, I-O | INVALID KEY |
| **CLOSE** | Tous | Fichier ouvert | FILE STATUS |

---

## Tableau des FILE STATUS

| Code | Catégorie | Signification |
|------|-----------|---------------|
| `00` | Succès | Opération réussie |
| `02` | Succès | Clé secondaire en double (OK) |
| `04` | Succès | Longueur enregistrement incorrecte |
| `05` | Succès | Fichier optionnel non présent |
| `10` | Fin | Fin de fichier (AT END) |
| `21` | Erreur clé | Erreur de séquence de clé |
| `22` | Erreur clé | Clé en double |
| `23` | Erreur clé | Enregistrement non trouvé |
| `24` | Erreur | Dépassement limite fichier |
| `35` | Erreur | Fichier non trouvé |
| `37` | Erreur | Mode ouverture incompatible |
| `38` | Erreur | Fichier fermé avec LOCK |
| `39` | Erreur | Conflit d'attributs |
| `41` | Erreur | Fichier déjà ouvert |
| `42` | Erreur | Fichier non ouvert |
| `43` | Erreur | Pas de READ préalable |
| `44` | Erreur | Taille enregistrement différente |
| `46` | Erreur | READ/START sans positionnement |
| `48` | Erreur | WRITE sans OPEN correct |
| `49` | Erreur | REWRITE/DELETE sans OPEN I-O |
