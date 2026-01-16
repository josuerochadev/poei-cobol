# Partie 3 : Opérations Avancées

[< Partie 2c : Suppression](05-partie-2c-suppression.md) | [Sommaire](00-introduction.md) | [Conclusion >](07-conclusion.md)

---

Cette section couvre les exercices 16 à 19 : création de clients génériques, navigation VSAM avec STARTBR/READNEXT/ENDBR, et statistiques par région.

## Commandes CICS pour navigation VSAM

| Commande | Usage | Description |
|----------|-------|-------------|
| **STARTBR** | Positionner le curseur | Démarre un parcours à partir d'une clé partielle |
| **READNEXT** | Lire l'enregistrement suivant | Lit séquentiellement après STARTBR |
| **ENDBR** | Terminer le parcours | Libère les ressources du browse |

Ces commandes permettent de parcourir un fichier VSAM de manière séquentielle, contrairement aux commandes READ/WRITE/REWRITE/DELETE qui travaillent sur un enregistrement spécifique.

---

## Exercice 16 : Création de clients génériques

### Énoncé

Sachant que le CODE CLIENT est sur six caractères, créer cinq CLIENT avec une partie de leur code générique commençant par '111...', de même '444...' et '777...'.

### Mon travail

> **Note** : J'ai anticipé une partie de cet exercice lors des phases précédentes du projet.

**Clients déjà existants :**

| Préfixe | Source | Contexte |
|---------|--------|----------|
| **222xxx** | LOADVSAM.jcl (Ex 1) | Pré-chargés pour les tests READNEXT |
| **111xxx** | Transaction AJOU (Ex 7-8) | Créés lors des tests et debug de la fonction WRITE |

En lisant l'ensemble du projet avant de commencer, j'ai identifié le besoin de clients avec des clés génériques pour les exercices de navigation VSAM. J'ai donc :

1. **Pré-chargé les clients 222xxx** dans le JCL de chargement initial (LOADVSAM.jcl) pour avoir des données de test dès le départ

2. **Créé les clients 111xxx** lors des tests de la transaction AJOU (exercices 7-8), ce qui m'a permis de valider la fonction d'ajout tout en préparant les données pour cet exercice

**Clients à créer pour compléter :**

Les clients 444xxx et 777xxx peuvent être créés via la transaction AJOU si nécessaire pour des tests supplémentaires.

### Résolution

**Clients 222xxx (pré-chargés via LOADVSAM.jcl) :**

```
222001 - LEROY Michel (Paris, Créditeur)
222002 - ROUX Nathalie (Marseille, Débiteur)
222003 - DAVID François (Lyon, Créditeur)
222004 - BERTRAND Isabelle (Lille, Débiteur)
222005 - MOREL Philippe (Paris, Créditeur)
```

**Clients 111xxx (créés via AJOU lors des tests) :**

Plusieurs clients ont été créés lors du debug de la transaction AJOU, avec des clés commençant par 111.

**Création de clients supplémentaires (optionnel) :**

Pour créer les clients 444xxx et 777xxx, utiliser la transaction AJOU :

```
AJOU
-> Saisir numéro 444001, remplir les champs, valider
-> Répéter pour 444002, 444003, etc.
```

### Vérification

Pour vérifier les clients existants avec un préfixe donné, utiliser la transaction AFFI :

```
AFFI
-> Saisir 222001 -> Client affiché
-> Saisir 111001 -> Client affiché (si créé)
```

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt3ex16-1 : Transaction AFFI - Affichage client 222001
2. pt3ex16-2 : Transaction AFFI - Affichage client 111001
3. pt3ex16-3 : Transaction AJOU - Création d'un client 444001 (optionnel)
-->

---

## Exercice 17 : Suppression par code générique (STARTBR)

### Énoncé

En utilisant les commandes adéquates, supprimer les CLIENT dont le code générique est '111...'.

### Mon travail

J'ai créé un programme qui va plus loin que l'énoncé initial en permettant :
- La suppression par **préfixe variable** (1 à 5 caractères) : supprime tous les clients correspondants
- La suppression par **clé complète** (6 caractères) : supprime un seul client spécifique

**Principe de la navigation VSAM :**

```
STARTBR (111000, GTEQ)     READNEXT           READNEXT           READNEXT
        │                      │                  │                  │
        ▼                      ▼                  ▼                  ▼
   ┌─────────┐            ┌─────────┐        ┌─────────┐        ┌─────────┐
   │ 111001  │ ────────►  │ 111002  │ ────►  │ 111003  │ ────►  │ 222001  │
   │ DELETE  │            │ DELETE  │        │ DELETE  │        │ STOP!   │
   └─────────┘            └─────────┘        └─────────┘        └─────────┘
                                                               Clé != 111
```

**Mode pseudo-conversationnel à 2 phases :**

```
┌─────────────────────────────────────────────────────────────┐
│  PHASE 1 : COMPTAGE                                         │
│  ─────────────────                                          │
│  1. Saisie préfixe (1-5 car) ou clé complète (6 car)       │
│  2. STARTBR/READNEXT pour compter les clients              │
│  3. Affichage : "X client(s) trouvé(s)"                    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  PHASE 2 : CONFIRMATION                                     │
│  ──────────────────────                                     │
│  4. L'utilisateur répond O ou N                            │
│  5. Si O : STARTBR/READNEXT + DELETE pour chaque client    │
│  6. Si N : Retour phase 1                                  │
└─────────────────────────────────────────────────────────────┘
```

**Choix technique important :**

Le champ PREFIXE est défini en `PIC X` (et non `PIC 9` avec NUM) pour éviter la justification à droite des valeurs numériques. Ainsi, si l'utilisateur saisit `1`, la valeur reste `1_____` (avec espaces) et non `_____1`.

### Résolution

**MAP BMS : CLIDEL.bms**

```
***********************************************************************
*  MAPSET : CLIDEL - Suppression Générique Client
*  Transaction : DELG
*  Fil Rouge CICS - Exercice 17
***********************************************************************
CLIDEL   DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
***********************************************************************
MAPDEL   DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE OU CLE COMPLETE
*----------------------------------------------------------------------
         DFHMDF POS=(5,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='PREFIXE OU CLE COMPLETE :'
PREFIXE  DFHMDF POS=(5,28),LENGTH=6,ATTRB=(UNPROT,IC)
         DFHMDF POS=(5,35),LENGTH=1,ATTRB=ASKIP
         DFHMDF POS=(5,37),LENGTH=30,ATTRB=ASKIP,                       X
               INITIAL='(1 a 6 caracteres)'
*----------------------------------------------------------------------
* ZONE D'INFORMATION - NOMBRE DE CLIENTS
*----------------------------------------------------------------------
         DFHMDF POS=(8,2),LENGTH=25,ATTRB=ASKIP,                        X
               INITIAL='CLIENTS CORRESPONDANTS  :'
NBCLI    DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE DE CONFIRMATION
*----------------------------------------------------------------------
         DFHMDF POS=(10,2),LENGTH=25,ATTRB=ASKIP,                       X
               INITIAL='CONFIRMER (O/N)         :'
CONFIRM  DFHMDF POS=(10,28),LENGTH=1,ATTRB=(UNPROT,IC)
*----------------------------------------------------------------------
* ZONE MESSAGE
*----------------------------------------------------------------------
MSG      DFHMDF POS=(15,13),LENGTH=60,ATTRB=(ASKIP,BRT)
```

**Programme : PRGDELG.cbl** (Suppression Générique)

```cobol
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase, le préfixe et le compte entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-COMPTAGE   VALUE '1'.
              88 PHASE-CONFIRM    VALUE '2'.
           05 WS-PREFIXE-SAVED    PIC X(06) VALUE SPACES.
           05 WS-LONGUEUR-SAVED   PIC 9(01) VALUE 0.
           05 WS-NBCLI-SAVED      PIC 9(05) VALUE 0.

      *-----------------------------------------------------------------
       3100-PARCOURIR-FICHIER.
      *-----------------------------------------------------------------
      * Parcours du fichier pour compter les clients correspondants
      *-----------------------------------------------------------------
           MOVE 0 TO WS-COMPTEUR
           MOVE 'N' TO WS-FIN-BROWSE

      *    Construction de la clé de début (préfixe complété par des 0)
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT

      *    Positionnement sur le premier client >= préfixe
           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

      *    Boucle de lecture
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR) NOT =
                       WS-PREFIXE(1:WS-LONGUEUR)
      *                Clé ne correspond plus au préfixe
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Client correspondant trouvé
                       ADD 1 TO WS-COMPTEUR
               END-EVALUATE
           END-PERFORM

      *    Fermeture du browse
           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.

      *-----------------------------------------------------------------
       4100-SUPPRIMER-CLIENTS.
      *-----------------------------------------------------------------
      * Suppression effective de tous les clients correspondants
      * Utilise STARTBR/READNEXT puis DELETE pour chaque client
      *-----------------------------------------------------------------
           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

      *    Boucle de lecture et suppression
           PERFORM UNTIL FIN-BROWSE
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR) NOT =
                       WS-PREFIXE(1:WS-LONGUEUR)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Suppression du client courant
                       EXEC CICS DELETE
                           FILE('FCLIENT')
                           RIDFLD(WS-CLE-COURANTE)
                           RESP(WS-RESP)
                       END-EXEC
                       IF WS-RESP = DFHRESP(NORMAL)
                           ADD 1 TO WS-COMPTEUR-SUP
                       END-IF
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.
```

**JCL d'assemblage BMS : ASMDEL.jcl**

```jcl
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIDEL',RMODE=24
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIDEL),DISP=SHR
```

**JCL de compilation COBOL : CMPDELG.jcl**

```jcl
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGDELG),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGDELG(R)
/*
```

**Définition de la transaction DELG :**

```
CEDA DEFINE MAPSET(CLIDEL) GROUP(CLIGROUP)

CEDA DEFINE PROGRAM(PRGDELG) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA DEFINE TRANSACTION(DELG) GROUP(CLIGROUP)
     PROGRAM(PRGDELG)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt3ex17-1 : Écran CLIDEL - Saisie préfixe "111"
2. pt3ex17-2 : Résultat comptage "5 client(s) trouvé(s)"
3. pt3ex17-3 : Confirmation "O" et résultat suppression
4. pt3ex17-4 : Saisie clé complète "222001" pour suppression unique
-->

---

## Exercice 18 : Lecture successive (READNEXT, ENDBR)

### Énoncé

Faire une lecture successive des CLIENT dont le code générique est '222...' en utilisant la commande READNEXT et ENDBR.

### Mon travail

Ce programme illustre le parcours séquentiel d'un fichier VSAM avec positionnement générique :

1. **STARTBR** avec GTEQ pour se positionner sur le premier '222xxx'
2. **READNEXT** en boucle pour lire les suivants
3. **Arrêt** quand le code ne commence plus par '222'
4. **ENDBR** pour terminer le browse et libérer les ressources

**Schéma du parcours :**

```
┌─────────────────────────────────────────────────────────────┐
│  STARTBR('222000', GTEQ)                                    │
│  ─────────────────────────                                  │
│  Positionne le curseur sur le premier enregistrement        │
│  dont la clé est >= '222000'                                │
│  Résultat : curseur sur 222001                              │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  READNEXT (boucle)                                          │
│  ─────────────────                                          │
│  222001 → Afficher    (clé commence par '222')              │
│  222002 → Afficher    (clé commence par '222')              │
│  222003 → Afficher    (clé commence par '222')              │
│  222004 → Afficher    (clé commence par '222')              │
│  222005 → Afficher    (clé commence par '222')              │
│  444001 → STOP        (clé ne commence plus par '222')      │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  ENDBR                                                      │
│  ─────                                                      │
│  Libère les ressources du browse                            │
└─────────────────────────────────────────────────────────────┘
```

### Résolution

**Programme : PRGLGEN.cbl** (Liste Générique)

```cobol
       2000-LISTER-GENERIQUE.
           MOVE '222000' TO WS-CLE-DEBUT
           MOVE 0 TO WS-COMPTEUR

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:3) NOT = '222'
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       PERFORM 3000-AFFICHER-LIGNE
                       ADD 1 TO WS-COMPTEUR
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

           MOVE WS-COMPTEUR TO MSGO
           STRING 'TOTAL CLIENTS 222XXX : ' WS-COMPTEUR
               DELIMITED BY SIZE INTO MSGO.
```

### Captures d'écran

<!-- ![pt3ex18-1](images-pt3/pt3ex18-1.png) -->

---

## Exercice 19 : Statistiques par région

### Énoncé

Élaborer une transaction permettant de calculer pour une REGION le nombre de CLIENT, la somme des montants des CLIENT Débiteurs et leur nombre et la somme des montants des CLIENT Créditeurs et leur nombre. Cette transaction aura en entrée le code REGION et affichera les quatre informations spécifiées ci-dessus.

### Mon travail

Cette transaction effectue un parcours complet du fichier pour calculer les statistiques d'une région donnée :
- Nombre total de clients de la région
- Nombre et somme des clients débiteurs (DB)
- Nombre et somme des clients créditeurs (CR)

J'utilise STARTBR/READNEXT pour parcourir tout le fichier et je filtre sur le code région.

**Algorithme :**

```
1. Saisie du code région (01, 02, 03 ou 04)
2. STARTBR depuis le début du fichier (LOW-VALUES)
3. Pour chaque enregistrement (READNEXT) :
   - Si code région correspond :
     - Incrémenter compteur total
     - Si position = 'DB' : compteur débiteurs + montant
     - Si position = 'CR' : compteur créditeurs + montant
4. ENDBR
5. Afficher les résultats
```

### Résolution

**MAP BMS : CLISTAT.bms**

```
* Zone de saisie code région
         DFHMDF POS=(3,2),LENGTH=15,                                   X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(3,18),LENGTH=2,                                   X
               ATTRB=(UNPROT,NUM,IC)

* Zones d'affichage des statistiques
         DFHMDF POS=(6,2),LENGTH=25,                                   X
               INITIAL='NOMBRE TOTAL CLIENTS    :'
NBTOT    DFHMDF POS=(6,28),LENGTH=5,ATTRB=(ASKIP)

         DFHMDF POS=(8,2),LENGTH=25,                                   X
               INITIAL='CLIENTS DEBITEURS       :'
NBDB     DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP)
MTDB     DFHMDF POS=(8,35),LENGTH=12,ATTRB=(ASKIP)

         DFHMDF POS=(10,2),LENGTH=25,                                  X
               INITIAL='CLIENTS CREDITEURS      :'
NBCR     DFHMDF POS=(10,28),LENGTH=5,ATTRB=(ASKIP)
MTCR     DFHMDF POS=(10,35),LENGTH=12,ATTRB=(ASKIP)
```

**Programme : PRGSTAT.cbl**

```cobol
       WORKING-STORAGE SECTION.
       01 WS-STATS.
          05 WS-NB-TOTAL         PIC 9(05) VALUE 0.
          05 WS-NB-DEBITEURS     PIC 9(05) VALUE 0.
          05 WS-MT-DEBITEURS     PIC 9(10) VALUE 0.
          05 WS-NB-CREDITEURS    PIC 9(05) VALUE 0.
          05 WS-MT-CREDITEURS    PIC 9(10) VALUE 0.

       2000-CALCULER-STATS.
           INITIALIZE WS-STATS
           MOVE CODREGI TO WS-CODE-REGION

      * Vérification région existante
           IF WS-CODE-REGION NOT = '01' AND '02' AND '03' AND '04'
               MOVE 'REGION INEXISTANTE, SAISIR CODE REGION' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Parcours du fichier
           MOVE LOW-VALUES TO WS-CLE-DEBUT
           EXEC CICS STARTBR FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE
                   IF CLI-CODREG = WS-CODE-REGION
                       ADD 1 TO WS-NB-TOTAL
                       IF CLI-POSITION = 'DB'
                           ADD 1 TO WS-NB-DEBITEURS
                           ADD CLI-SOLDE TO WS-MT-DEBITEURS
                       ELSE
                           ADD 1 TO WS-NB-CREDITEURS
                           ADD CLI-SOLDE TO WS-MT-CREDITEURS
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC
           PERFORM 3000-AFFICHER-RESULTATS.

       3000-AFFICHER-RESULTATS.
           MOVE WS-NB-TOTAL      TO NBTOTO
           MOVE WS-NB-DEBITEURS  TO NBDBO
           MOVE WS-MT-DEBITEURS  TO MTDBO
           MOVE WS-NB-CREDITEURS TO NBCRO
           MOVE WS-MT-CREDITEURS TO MTCRO
           MOVE 'STATISTIQUES CALCULEES' TO MSGO.
```

**Transaction :**

```
CEDA DEFINE TRANSACTION(STAT) GROUP(CLIGROUP)
     PROGRAM(PRGSTAT)

CEDA INSTALL TRANSACTION(STAT) GROUP(CLIGROUP)
```

**Résultats attendus (avec les données initiales) :**

| Région | Total | Débiteurs | Montant DB | Créditeurs | Montant CR |
|--------|-------|-----------|------------|------------|------------|
| 01 Paris | 5 | 1 | 80 000 | 4 | 871 000 |
| 02 Marseille | 4 | 2 | 77 000 | 2 | 395 000 |
| 03 Lyon | 3 | 1 | 12 000 | 2 | 598 000 |
| 04 Lille | 3 | 2 | 118 000 | 1 | 180 000 |

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt3ex19-1 : Écran CLISTAT - Saisie code région 01
2. pt3ex19-2 : Résultats statistiques pour Paris
3. pt3ex19-3 : Résultats statistiques pour une autre région
-->

---

[< Partie 2c : Suppression](05-partie-2c-suppression.md) | [Sommaire](00-introduction.md) | [Conclusion >](07-conclusion.md)
