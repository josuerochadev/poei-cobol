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
│  PHASE 2 : CONFIRMATION ET SUPPRESSION                      │
│  ─────────────────────────────────────                      │
│  4. L'utilisateur répond O ou N                            │
│  5. Si N : Retour phase 1                                  │
│  6. Si O : Suppression en 2 étapes (évite deadlock)        │
│     a) STARTBR/READNEXT → collecter clés en table (max 100)│
│     b) ENDBR (fermer browse)                               │
│     c) Pour chaque clé : DELETE RIDFLD                     │
└─────────────────────────────────────────────────────────────┘
```

**Point technique - Éviter le deadlock CICS :**

On ne peut pas faire `DELETE RIDFLD` pendant un browse actif (STARTBR/READNEXT) car cela provoque un deadlock : le browse tient un verrou lecture, le DELETE demande un verrou exclusif sur le même enregistrement → CICS freeze.

**Solution adoptée :** Collecter les clés dans une table (max 100), fermer le browse avec ENDBR, puis supprimer chaque clé. Si plus de 100 clients correspondent, l'utilisateur doit relancer la transaction.

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
CONFIRM  DFHMDF POS=(10,28),LENGTH=1,ATTRB=UNPROT
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
      * TABLE DES CLES A SUPPRIMER (max 100 clients)
      *-----------------------------------------------------------------
       01  WS-TABLE-CLES.
           05 WS-NB-CLES          PIC 9(03) VALUE 0.
           05 WS-CLES OCCURS 100 TIMES.
              10 WS-CLE-SUP       PIC X(06).
       01  WS-IDX-SUP             PIC 9(03) VALUE 0.

      *-----------------------------------------------------------------
       4100-SUPPRIMER-CLIENTS.
      *-----------------------------------------------------------------
      * Suppression en 2 étapes pour éviter le deadlock
      * Étape 1 : Collecter les clés pendant le browse
      * Étape 2 : Supprimer après ENDBR
      *-----------------------------------------------------------------
           MOVE 0 TO WS-NB-CLES

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

      *    ETAPE 1 : Collecter les clés (sans DELETE)
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
                   WHEN WS-NB-CLES >= 100
      *                Table pleine
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
      *                Stocker la clé dans la table
                       ADD 1 TO WS-NB-CLES
                       MOVE WS-CLE-COURANTE TO WS-CLE-SUP(WS-NB-CLES)
               END-EVALUATE
           END-PERFORM

      *    Fermer le browse AVANT les suppressions
           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

      *    ETAPE 2 : Supprimer chaque clé collectée
           PERFORM VARYING WS-IDX-SUP FROM 1 BY 1
               UNTIL WS-IDX-SUP > WS-NB-CLES
               MOVE WS-CLE-SUP(WS-IDX-SUP) TO WS-CLE-COURANTE
               EXEC CICS DELETE
                   FILE('FCLIENT')
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC
               IF WS-RESP = DFHRESP(NORMAL)
                   ADD 1 TO WS-COMPTEUR-SUP
               END-IF
           END-PERFORM.
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

## Exercice 18 : Liste générique paginée (READNEXT, ENDBR)

### Énoncé

Faire une lecture successive des CLIENT dont le code générique est '222...' en utilisant la commande READNEXT et ENDBR.

### Mon travail

J'ai étendu l'énoncé pour créer un programme complet de **liste paginée** :

- Saisie d'un préfixe générique (1 à 6 caractères)
- Affichage de **10 clients par page**
- Navigation avec **PF7** (page précédente) et **PF8** (page suivante)
- Affichage du compteur total et du numéro de page

**Nouveau mapset BMS requis :** CLILIST avec 10 lignes répétitives pour afficher les clients.

**Mode pseudo-conversationnel avec pagination :**

```
┌─────────────────────────────────────────────────────────────┐
│  COMMAREA (sauvegarde entre passages)                       │
│  ─────────────────────────────────────                      │
│  - Préfixe saisi et sa longueur                            │
│  - Dernière clé affichée (pour navigation)                 │
│  - Numéro de page courante                                 │
│  - Total clients trouvés et nombre de pages                │
└─────────────────────────────────────────────────────────────┘
```

**Algorithme de pagination :**

```
┌─────────────────────────────────────────────────────────────┐
│  ENTER : Nouvelle recherche                                 │
│  ───────────────────────────                                │
│  1. Compter tous les clients correspondants                │
│  2. Calculer le nombre de pages (total / 10)               │
│  3. Afficher la première page                              │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  PF8 : Page suivante                                        │
│  ───────────────────                                        │
│  1. STARTBR au début du préfixe                            │
│  2. READNEXT pour sauter (page - 1) × 10 enregistrements   │
│  3. READNEXT × 10 pour remplir l'écran                     │
│  4. ENDBR                                                  │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  PF7 : Page précédente                                      │
│  ─────────────────────                                      │
│  Même principe avec page - 1                               │
└─────────────────────────────────────────────────────────────┘
```

**Gestion du cas "aucun client trouvé" :**

Quand le préfixe saisi ne correspond à aucun client, le programme :
1. Affiche le message "AUCUN CLIENT TROUVE - SAISIR AUTRE PREFIXE"
2. Réinitialise la COMMAREA pour permettre une nouvelle recherche
3. Reste en mode pseudo-conversationnel (la transaction ne se termine pas)

### Résolution

**MAP BMS : CLILIST.bms**

```
CLILIST  DFHMSD TYPE=&SYSPARM,MODE=INOUT,LANG=COBOL,                   X
               STORAGE=AUTO,CTRL=(FREEKB,FRSET),TIOAPFX=YES
MAPLGEN  DFHMDI SIZE=(24,80),LINE=1,COLUMN=1
*----------------------------------------------------------------------
* ZONE DE SAISIE - PREFIXE
*----------------------------------------------------------------------
         DFHMDF POS=(3,2),LENGTH=10,ATTRB=ASKIP,INITIAL='PREFIXE :'
PREFIXE  DFHMDF POS=(3,13),LENGTH=6,ATTRB=(UNPROT,IC)
*----------------------------------------------------------------------
* EN-TETE DES COLONNES
*----------------------------------------------------------------------
         DFHMDF POS=(5,1),LENGTH=50,ATTRB=(ASKIP,BRT),                  X
               INITIAL='NUMCPT RG NOM        PRENOM     SOLDE      POS'
*----------------------------------------------------------------------
* LIGNES 1 à 10 (structure répétitive)
*----------------------------------------------------------------------
L1NUM    DFHMDF POS=(7,1),LENGTH=6,ATTRB=ASKIP
L1REG    DFHMDF POS=(7,8),LENGTH=2,ATTRB=ASKIP
L1NOM    DFHMDF POS=(7,11),LENGTH=10,ATTRB=ASKIP
L1PRE    DFHMDF POS=(7,22),LENGTH=10,ATTRB=ASKIP
L1SOL    DFHMDF POS=(7,33),LENGTH=10,ATTRB=ASKIP
L1POS    DFHMDF POS=(7,44),LENGTH=2,ATTRB=ASKIP
* ... (L2 à L10 sur les lignes 8 à 16)
*----------------------------------------------------------------------
* ZONE INFORMATIONS PAGINATION
*----------------------------------------------------------------------
         DFHMDF POS=(18,2),LENGTH=6,ATTRB=ASKIP,INITIAL='PAGE :'
PAGNUM   DFHMDF POS=(18,9),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,13),LENGTH=1,ATTRB=ASKIP,INITIAL='/'
PAGTOT   DFHMDF POS=(18,15),LENGTH=3,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(18,22),LENGTH=7,ATTRB=ASKIP,INITIAL='TOTAL :'
CLITOT   DFHMDF POS=(18,30),LENGTH=5,ATTRB=(ASKIP,BRT)
*----------------------------------------------------------------------
* ZONE MESSAGE ET TOUCHES FONCTION
*----------------------------------------------------------------------
MSG      DFHMDF POS=(20,13),LENGTH=60,ATTRB=(ASKIP,BRT)
         DFHMDF POS=(23,2),LENGTH=60,ATTRB=ASKIP,                       X
               INITIAL='ENTER=Chercher  PF7=Prec  PF8=Suiv  PF3=Quitter'
```

**Programme : PRGLGEN.cbl** (Liste Générique Paginée)

```cobol
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PREFIXE-SAVED   PIC X(06) VALUE SPACES.
           05 WS-LONGUEUR-SAVED  PIC 9(01) VALUE 0.
           05 WS-DERNIERE-CLE    PIC X(06) VALUE SPACES.
           05 WS-PAGE-COURANTE   PIC 9(03) VALUE 0.
           05 WS-TOTAL-CLIENTS   PIC 9(05) VALUE 0.
           05 WS-TOTAL-PAGES     PIC 9(03) VALUE 0.
           05 WS-FIN-FICHIER     PIC X(01) VALUE 'N'.

      *-----------------------------------------------------------------
       3100-COMPTER-TOTAL.
      *-----------------------------------------------------------------
      * Compte le nombre total de clients correspondant au préfixe
      *-----------------------------------------------------------------
           MOVE 0 TO WS-TOTAL-CLIENTS
           MOVE SPACES TO WS-CLE-DEBUT
           MOVE WS-PREFIXE(1:WS-LONGUEUR) TO WS-CLE-DEBUT
      *    Compléter avec des zéros
           ...
           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           MOVE WS-CLE-DEBUT TO WS-CLE-COURANTE

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
                   WHEN WS-CLE-COURANTE(1:WS-LONGUEUR-SAVED) NOT =
                       WS-PREFIXE-SAVED(1:WS-LONGUEUR-SAVED)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       ADD 1 TO WS-TOTAL-CLIENTS
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.

      *-----------------------------------------------------------------
       6000-AFFICHER-PAGE.
      *-----------------------------------------------------------------
      * Affiche la page courante (10 clients)
      *-----------------------------------------------------------------
           EXEC CICS STARTBR ... END-EXEC

      *    Sauter les enregistrements des pages précédentes
           COMPUTE WS-COMPTEUR = (WS-PAGE-COURANTE - 1) * 10
           PERFORM WS-COMPTEUR TIMES
               EXEC CICS READNEXT ... END-EXEC
           END-PERFORM

      *    Lire les 10 clients de cette page
           PERFORM UNTIL FIN-BROWSE OR WS-LIGNE-COURANTE >= 10
               EXEC CICS READNEXT ... END-EXEC
               ...
               MOVE CLI-NUMCPT TO WS-CLI-NUM(WS-LIGNE-COURANTE)
               MOVE CLI-NOM TO WS-CLI-NOM(WS-LIGNE-COURANTE)
               ...
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

      *    Transférer vers la MAP
           MOVE WS-CLI-NUM(1) TO L1NUMO
           ...
           MOVE WS-PAGE-COURANTE TO PAGNUMO
           MOVE WS-TOTAL-PAGES TO PAGTOTO
           MOVE WS-TOTAL-CLIENTS TO CLITOTO

           EXEC CICS SEND MAP('MAPLGEN') MAPSET('CLILIST') ERASE
           END-EXEC.
```

**JCL d'assemblage BMS : ASMLIST.jcl**

```jcl
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLILIST',RMODE=24
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLILIST),DISP=SHR
```

**JCL de compilation COBOL : CMPLGEN.jcl**

```jcl
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(PRGLGEN),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME PRGLGEN(R)
/*
```

**Définition de la transaction LGEN :**

```
CEDA DEFINE MAPSET(CLILIST) GROUP(CLIGROUP)

CEDA DEFINE PROGRAM(PRGLGEN) GROUP(CLIGROUP)
     LANGUAGE(COBOL)

CEDA DEFINE TRANSACTION(LGEN) GROUP(CLIGROUP)
     PROGRAM(PRGLGEN)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt3ex18-1 : Écran MAPLGEN - Saisie préfixe "1"
2. pt3ex18-2 : Liste des 10 premiers clients (page 1/3)
3. pt3ex18-3 : Navigation PF8 - Page 2/3
4. pt3ex18-4 : Message "AUCUN CLIENT TROUVE" avec préfixe "9"
-->

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
