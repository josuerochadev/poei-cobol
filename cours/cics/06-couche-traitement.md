# Chapitre VI - Couche de Traitement

## VI-1 Rôle de la couche de traitement

### Position dans l'architecture

La **couche de traitement** (aussi appelée **couche métier** ou **Business Logic Layer**) se situe au cœur de l'application :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DE TRAITEMENT                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │                  COUCHE PRÉSENTATION                         │    │
│     │                  (Écrans BMS)                                │    │
│     └───────────────────────────┬─────────────────────────────────┘    │
│                                 │                                       │
│                          COMMAREA / TSI                                 │
│                                 │                                       │
│                                 ▼                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │              COUCHE DE TRAITEMENT (ce chapitre)                  │   │
│  │                                                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │  RESPONSABILITÉS :                                       │   │   │
│  │  │  • Logique métier et règles de gestion                   │   │   │
│  │  │  • Validation fonctionnelle                              │   │   │
│  │  │  • Orchestration des opérations                          │   │   │
│  │  │  • Calculs métier                                        │   │   │
│  │  │  • Gestion des transactions (SYNCPOINT)                  │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                                                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │  COMMANDES CICS UTILISÉES :                              │   │   │
│  │  │  • READ    : Lecture d'enregistrements                   │   │   │
│  │  │  • WRITE   : Création d'enregistrements                  │   │   │
│  │  │  • REWRITE : Mise à jour d'enregistrements               │   │   │
│  │  │  • DELETE  : Suppression d'enregistrements               │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                 │                                       │
│                          COMMAREA / TSI                                 │
│                                 │                                       │
│                                 ▼                                       │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │                  COUCHE DONNÉES                              │    │
│     │                  (Fichiers VSAM, DB2)                        │    │
│     └─────────────────────────────────────────────────────────────┘    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Remarque importante sur l'architecture

Dans une **architecture multicouches stricte**, les commandes d'accès aux données (READ, WRITE, etc.) appartiendraient à la **couche données**. Cependant, pour des raisons **pédagogiques** et de **simplicité**, nous les étudions ici dans la couche de traitement, ce qui correspond à une approche courante dans les applications CICS traditionnelles.

```
┌─────────────────────────────────────────────────────────────────────────┐
│  APPROCHE TRADITIONNELLE          vs    APPROCHE STRICTE                │
│  (étudiée ici)                          (architecturée)                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌──────────────┐                    ┌──────────────┐                   │
│  │ Présentation │                    │ Présentation │                   │
│  └──────┬───────┘                    └──────┬───────┘                   │
│         │                                   │                            │
│         ▼                                   ▼                            │
│  ┌──────────────┐                    ┌──────────────┐                   │
│  │ Traitement   │                    │ Traitement   │                   │
│  │ + Accès Data │ ◄── Ici           │ (Logique)    │                   │
│  │ READ, WRITE  │                    └──────┬───────┘                   │
│  └──────────────┘                           │                            │
│         │                                   ▼                            │
│         │                            ┌──────────────┐                   │
│         │                            │   Données    │                   │
│         │                            │ READ, WRITE  │                   │
│         ▼                            └──────┬───────┘                   │
│  ┌──────────────┐                           │                            │
│  │ Fichiers     │                    ┌──────▼───────┐                   │
│  │ VSAM / DB2   │                    │ Fichiers     │                   │
│  └──────────────┘                    │ VSAM / DB2   │                   │
│                                      └──────────────┘                   │
│                                                                          │
│  Avantages :                         Avantages :                        │
│  • Plus simple                       • Meilleure séparation             │
│  • Moins de programmes               • Plus testable                    │
│  • Apprentissage facile              • Plus évolutif                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## VI-2 La commande CICS READ

### Syntaxe générale

La commande **READ** permet de lire un enregistrement dans un fichier VSAM :

```cobol
       EXEC CICS
           READ FILE('nom-fichier')
                INTO(zone-réception)
                RIDFLD(zone-clé)
                [LENGTH(zone-longueur)]
                [KEYLENGTH(longueur-clé)]
                [GENERIC]
                [GTEQ | EQUAL]
                [UPDATE]
                [RESP(zone-resp)]
                [RESP2(zone-resp2)]
       END-EXEC
```

### Options de la commande READ

| Option | Description |
|--------|-------------|
| **FILE** | Nom du fichier défini dans la FCT (max 8 caractères) |
| **INTO** | Zone de réception de l'enregistrement |
| **RIDFLD** | Zone contenant la clé de recherche |
| **LENGTH** | Longueur de l'enregistrement (retournée après lecture) |
| **KEYLENGTH** | Longueur de la clé (pour recherche générique) |
| **GENERIC** | Recherche partielle sur début de clé |
| **GTEQ** | Greater Than or Equal - premier enregistrement ≥ clé |
| **EQUAL** | Égalité stricte (par défaut) |
| **UPDATE** | Verrouillage pour mise à jour |
| **RESP** | Code retour de l'opération |

### Codes retour (RESP) pour READ

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | NORMAL | Lecture réussie |
| 12 | FILENOTFOUND | Fichier non défini dans FCT |
| 13 | NOTFND | Enregistrement non trouvé |
| 14 | DUPREC | Clé en double (index alternatif) |
| 19 | NOTOPEN | Fichier non ouvert |
| 22 | DISABLED | Fichier désactivé |
| 32 | ILLOGIC | Erreur VSAM interne |

### Exemple 1 : Lecture simple

```cobol
      ******************************************************************
      * Exemple READ simple - Recherche d'un client par numéro
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-CLIENT-KEY           PIC X(8).
       01  WS-CLIENT-REC.
           05  CLI-NUM             PIC X(8).
           05  CLI-NOM             PIC X(30).
           05  CLI-PRENOM          PIC X(20).
           05  CLI-SOLDE           PIC S9(9)V99 COMP-3.
       01  WS-RESP                 PIC S9(8) COMP.

       PROCEDURE DIVISION.

       1000-LIRE-CLIENT.

           MOVE '12345678' TO WS-CLIENT-KEY

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   DISPLAY 'Client trouvé: ' CLI-NOM
               WHEN DFHRESP(NOTFND)
                   DISPLAY 'Client non trouvé'
               WHEN OTHER
                   DISPLAY 'Erreur lecture: ' WS-RESP
           END-EVALUATE.
```

### Exemple 2 : Lecture pour mise à jour (UPDATE)

```cobol
      ******************************************************************
      * READ avec UPDATE - Verrouillage pour modification
      ******************************************************************
       1100-LIRE-POUR-MAJ.

      *─── Lecture avec verrouillage ──────────────────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               EVALUATE WS-RESP
                   WHEN DFHRESP(NOTFND)
                       MOVE 'Client non trouvé' TO WS-MESSAGE
                   WHEN DFHRESP(RECORDBUSY)
                       MOVE 'Enregistrement verrouillé' TO WS-MESSAGE
                   WHEN OTHER
                       MOVE 'Erreur lecture' TO WS-MESSAGE
               END-EVALUATE
               GO TO 1100-EXIT
           END-IF

      *─── L'enregistrement est maintenant verrouillé ─────────────────
      *─── Il faut faire REWRITE ou UNLOCK avant la fin ───────────────
           PERFORM 1200-MODIFIER-CLIENT.

       1100-EXIT.
           EXIT.
```

### Exemple 3 : Lecture générique (début de clé)

```cobol
      ******************************************************************
      * READ GENERIC - Recherche sur début de clé
      ******************************************************************
       1200-LIRE-GENERIQUE.

      *─── Recherche des clients dont le numéro commence par '123' ────
           MOVE '123' TO WS-CLIENT-KEY

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    KEYLENGTH(3)
                    GENERIC
                    RESP(WS-RESP)
           END-EXEC

      *─── Retourne le premier client avec clé >= '123' ───────────────
           IF WS-RESP = DFHRESP(NORMAL)
               DISPLAY 'Premier client trouvé: ' CLI-NUM
           END-IF.
```

### Exemple 4 : Lecture avec GTEQ

```cobol
      ******************************************************************
      * READ avec GTEQ - Premier enregistrement >= clé
      ******************************************************************
       1300-LIRE-GTEQ.

      *─── Recherche du premier client >= '12300000' ──────────────────
           MOVE '12300000' TO WS-CLIENT-KEY

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    GTEQ
                    RESP(WS-RESP)
           END-EXEC

      *─── La clé retournée peut être différente de la clé recherchée ─
           IF WS-RESP = DFHRESP(NORMAL)
               DISPLAY 'Clé recherchée : ' WS-CLIENT-KEY
               DISPLAY 'Clé trouvée    : ' CLI-NUM
           END-IF.
```

## VI-3 La commande CICS WRITE

### Syntaxe générale

La commande **WRITE** permet de créer un nouvel enregistrement :

```cobol
       EXEC CICS
           WRITE FILE('nom-fichier')
                 FROM(zone-données)
                 RIDFLD(zone-clé)
                 [LENGTH(longueur)]
                 [KEYLENGTH(longueur-clé)]
                 [RESP(zone-resp)]
                 [RESP2(zone-resp2)]
       END-EXEC
```

### Options de la commande WRITE

| Option | Description |
|--------|-------------|
| **FILE** | Nom du fichier VSAM |
| **FROM** | Zone contenant l'enregistrement à écrire |
| **RIDFLD** | Zone contenant la clé |
| **LENGTH** | Longueur de l'enregistrement (obligatoire pour ESDS) |
| **RESP** | Code retour |

### Codes retour (RESP) pour WRITE

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | NORMAL | Écriture réussie |
| 14 | DUPREC | Clé déjà existante |
| 18 | NOSPACE | Plus d'espace dans le fichier |
| 19 | NOTOPEN | Fichier non ouvert |
| 22 | DISABLED | Fichier désactivé |

### Exemple 1 : WRITE simple (sans lecture préalable)

```cobol
      ******************************************************************
      * WRITE simple - Création d'un nouveau client
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-NEW-CLIENT.
           05  CLI-NUM             PIC X(8).
           05  CLI-NOM             PIC X(30).
           05  CLI-PRENOM          PIC X(20).
           05  CLI-ADRESSE         PIC X(50).
           05  CLI-CODE-POSTAL     PIC X(5).
           05  CLI-VILLE           PIC X(30).
           05  CLI-SOLDE           PIC S9(9)V99 COMP-3.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-MESSAGE              PIC X(78).

       PROCEDURE DIVISION.

       2000-CREER-CLIENT.

      *─── Préparation des données ────────────────────────────────────
           INITIALIZE WS-NEW-CLIENT
           MOVE '99990001'          TO CLI-NUM
           MOVE 'DUPONT'            TO CLI-NOM
           MOVE 'Jean'              TO CLI-PRENOM
           MOVE '10 rue de Paris'   TO CLI-ADRESSE
           MOVE '75001'             TO CLI-CODE-POSTAL
           MOVE 'PARIS'             TO CLI-VILLE
           MOVE 1000.00             TO CLI-SOLDE

      *─── Écriture dans le fichier ───────────────────────────────────
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-NEW-CLIENT)
                     RIDFLD(CLI-NUM)
                     RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Client créé avec succès' TO WS-MESSAGE
               WHEN DFHRESP(DUPREC)
                   MOVE 'Ce numéro client existe déjà' TO WS-MESSAGE
               WHEN DFHRESP(NOSPACE)
                   MOVE 'Fichier plein - Contactez admin' TO WS-MESSAGE
               WHEN OTHER
                   STRING 'Erreur création: RESP=' WS-RESP
                          DELIMITED SIZE INTO WS-MESSAGE
           END-EVALUATE.
```

### Exemple 2 : WRITE précédé d'un contrôle READ

```cobol
      ******************************************************************
      * WRITE avec contrôle préalable - Bonne pratique
      ******************************************************************
       2100-CREER-CLIENT-CONTROLE.

      *─── Vérifier que le client n'existe pas déjà ───────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-BUFFER)
                    RIDFLD(CLI-NUM)
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
      *─────── Le client existe déjà ──────────────────────────────────
               MOVE 'Ce client existe déjà dans la base'
                   TO WS-MESSAGE
               GO TO 2100-EXIT
           END-IF

           IF WS-RESP NOT = DFHRESP(NOTFND)
      *─────── Erreur autre que "non trouvé" ──────────────────────────
               MOVE 'Erreur lors de la vérification' TO WS-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Le client n'existe pas, on peut le créer ───────────────────
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-NEW-CLIENT)
                     RIDFLD(CLI-NUM)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Client créé avec succès' TO WS-MESSAGE
           ELSE
               MOVE 'Erreur lors de la création' TO WS-MESSAGE
           END-IF.

       2100-EXIT.
           EXIT.
```

### Exemple 3 : WRITE avec génération de clé

```cobol
      ******************************************************************
      * WRITE avec génération automatique de numéro client
      ******************************************************************
       01  WS-COMPTEUR-REC.
           05  CPT-ID              PIC X(8) VALUE 'NUMCLI'.
           05  CPT-VALEUR          PIC 9(8) VALUE 0.

       2200-CREER-AVEC-NUMERO-AUTO.

      *─── Lire le compteur de numéros clients ────────────────────────
           MOVE 'NUMCLI' TO CPT-ID

           EXEC CICS
               READ FILE('COMPTEUR')
                    INTO(WS-COMPTEUR-REC)
                    RIDFLD(CPT-ID)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Erreur lecture compteur' TO WS-MESSAGE
               GO TO 2200-EXIT
           END-IF

      *─── Incrémenter le compteur ────────────────────────────────────
           ADD 1 TO CPT-VALEUR
           MOVE CPT-VALEUR TO CLI-NUM

      *─── Mettre à jour le compteur ──────────────────────────────────
           EXEC CICS
               REWRITE FILE('COMPTEUR')
                       FROM(WS-COMPTEUR-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Erreur MAJ compteur' TO WS-MESSAGE
               GO TO 2200-EXIT
           END-IF

      *─── Créer le nouveau client ────────────────────────────────────
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-NEW-CLIENT)
                     RIDFLD(CLI-NUM)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               STRING 'Client créé avec numéro: ' CLI-NUM
                      DELIMITED SIZE INTO WS-MESSAGE
           ELSE
               MOVE 'Erreur création client' TO WS-MESSAGE
           END-IF.

       2200-EXIT.
           EXIT.
```

## VI-4 La commande CICS REWRITE

### Syntaxe générale

La commande **REWRITE** permet de mettre à jour un enregistrement **préalablement lu avec UPDATE** :

```cobol
       EXEC CICS
           REWRITE FILE('nom-fichier')
                   FROM(zone-données)
                   [LENGTH(longueur)]
                   [RESP(zone-resp)]
                   [RESP2(zone-resp2)]
       END-EXEC
```

### Règle importante

```
┌─────────────────────────────────────────────────────────────────────────┐
│  ATTENTION : REWRITE nécessite un READ UPDATE préalable !               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  SÉQUENCE OBLIGATOIRE :                                                 │
│                                                                          │
│  1. READ FILE('XXX') ... UPDATE    ◄── Verrouille l'enregistrement     │
│                                                                          │
│  2. Modifications des données en mémoire                                │
│                                                                          │
│  3. REWRITE FILE('XXX') ...        ◄── Écrit et déverrouille           │
│                                                                          │
│  OU                                                                      │
│                                                                          │
│  3. UNLOCK FILE('XXX')             ◄── Annule sans modifier            │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Codes retour (RESP) pour REWRITE

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | NORMAL | Mise à jour réussie |
| 16 | INVREQ | Pas de READ UPDATE préalable |
| 18 | NOSPACE | Plus d'espace (si longueur augmentée) |
| 19 | NOTOPEN | Fichier non ouvert |

### Exemple 1 : REWRITE simple

```cobol
      ******************************************************************
      * REWRITE - Mise à jour du solde client
      ******************************************************************
       3000-METTRE-A-JOUR-SOLDE.

      *─── Lecture avec verrouillage ──────────────────────────────────
           MOVE WS-NUM-CLIENT TO WS-CLIENT-KEY

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Client non trouvé' TO WS-MESSAGE
               GO TO 3000-EXIT
           END-IF

      *─── Modification du solde ──────────────────────────────────────
           ADD WS-MONTANT-CREDIT TO CLI-SOLDE

      *─── Réécriture de l'enregistrement ─────────────────────────────
           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Solde mis à jour avec succès' TO WS-MESSAGE
           ELSE
               MOVE 'Erreur lors de la mise à jour' TO WS-MESSAGE
           END-IF.

       3000-EXIT.
           EXIT.
```

### Exemple 2 : REWRITE avec annulation possible

```cobol
      ******************************************************************
      * REWRITE avec possibilité d'annulation (UNLOCK)
      ******************************************************************
       3100-MAJ-AVEC-CONFIRMATION.

      *─── Lecture pour mise à jour ───────────────────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               GO TO 3100-EXIT
           END-IF

      *─── Calcul du nouveau solde ────────────────────────────────────
           COMPUTE WS-NOUVEAU-SOLDE = CLI-SOLDE - WS-MONTANT-DEBIT

      *─── Vérification de la règle métier ────────────────────────────
           IF WS-NOUVEAU-SOLDE < WS-DECOUVERT-AUTORISE

      *─────── Refus de l'opération - déverrouillage sans modification
               EXEC CICS
                   UNLOCK FILE('CLIENTS')
               END-EXEC
               MOVE 'Opération refusée: dépassement découvert'
                   TO WS-MESSAGE
               GO TO 3100-EXIT
           END-IF

      *─── Validation de la modification ──────────────────────────────
           MOVE WS-NOUVEAU-SOLDE TO CLI-SOLDE

           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Débit effectué avec succès' TO WS-MESSAGE
           END-IF.

       3100-EXIT.
           EXIT.
```

### Exemple 3 : REWRITE - Mise à jour complète d'un client

```cobol
      ******************************************************************
      * REWRITE - Modification des informations client
      ******************************************************************
       3200-MODIFIER-CLIENT-COMPLET.

      *─── Lecture de l'enregistrement existant ───────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               EVALUATE WS-RESP
                   WHEN DFHRESP(NOTFND)
                       MOVE 'Client inexistant' TO WS-MESSAGE
                   WHEN DFHRESP(RECORDBUSY)
                       MOVE 'Client en cours de modification'
                           TO WS-MESSAGE
                   WHEN OTHER
                       STRING 'Erreur READ: ' WS-RESP
                              DELIMITED SIZE INTO WS-MESSAGE
               END-EVALUATE
               GO TO 3200-EXIT
           END-IF

      *─── Application des modifications ──────────────────────────────
      *─── (données reçues de la couche présentation) ─────────────────
           IF WS-MODIF-NOM NOT = SPACES
               MOVE WS-MODIF-NOM TO CLI-NOM
           END-IF

           IF WS-MODIF-ADRESSE NOT = SPACES
               MOVE WS-MODIF-ADRESSE TO CLI-ADRESSE
           END-IF

           IF WS-MODIF-VILLE NOT = SPACES
               MOVE WS-MODIF-VILLE TO CLI-VILLE
           END-IF

      *─── Réécriture ─────────────────────────────────────────────────
           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Modifications enregistrées' TO WS-MESSAGE
           ELSE
               STRING 'Erreur REWRITE: ' WS-RESP
                      DELIMITED SIZE INTO WS-MESSAGE
           END-IF.

       3200-EXIT.
           EXIT.
```

## VI-5 La commande CICS DELETE

### Syntaxe générale

La commande **DELETE** permet de supprimer un enregistrement :

```cobol
       EXEC CICS
           DELETE FILE('nom-fichier')
                  RIDFLD(zone-clé)
                  [KEYLENGTH(longueur-clé)]
                  [GENERIC]
                  [NUMREC(zone-compteur)]
                  [RESP(zone-resp)]
                  [RESP2(zone-resp2)]
       END-EXEC
```

### Deux méthodes de suppression

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    MÉTHODES DE SUPPRESSION                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  MÉTHODE 1 : DELETE direct (sans READ préalable)                        │
│  ──────────────────────────────────────────────────────────────────     │
│                                                                          │
│     EXEC CICS                                                           │
│         DELETE FILE('CLIENTS')                                          │
│                RIDFLD(WS-CLIENT-KEY)                                    │
│     END-EXEC                                                            │
│                                                                          │
│     → Supprime directement par la clé                                   │
│     → Simple mais pas de vérification préalable                         │
│                                                                          │
│  MÉTHODE 2 : READ UPDATE + DELETE (après vérification)                  │
│  ──────────────────────────────────────────────────────────────────     │
│                                                                          │
│     EXEC CICS                                                           │
│         READ FILE('CLIENTS')                                            │
│              INTO(WS-CLIENT-REC)                                        │
│              RIDFLD(WS-CLIENT-KEY)                                      │
│              UPDATE                                                     │
│     END-EXEC                                                            │
│                                                                          │
│     ... vérifications ...                                               │
│                                                                          │
│     EXEC CICS                                                           │
│         DELETE FILE('CLIENTS')                                          │
│     END-EXEC                                                            │
│                                                                          │
│     → Permet de vérifier avant suppression                              │
│     → RIDFLD non nécessaire (utilise l'enregistrement verrouillé)       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Codes retour (RESP) pour DELETE

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | NORMAL | Suppression réussie |
| 13 | NOTFND | Enregistrement non trouvé |
| 19 | NOTOPEN | Fichier non ouvert |
| 22 | DISABLED | Fichier désactivé |

### Exemple 1 : DELETE direct

```cobol
      ******************************************************************
      * DELETE direct - Suppression simple par clé
      ******************************************************************
       4000-SUPPRIMER-CLIENT-DIRECT.

           MOVE WS-NUM-CLIENT TO WS-CLIENT-KEY

           EXEC CICS
               DELETE FILE('CLIENTS')
                      RIDFLD(WS-CLIENT-KEY)
                      RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 'Client supprimé' TO WS-MESSAGE
               WHEN DFHRESP(NOTFND)
                   MOVE 'Client non trouvé' TO WS-MESSAGE
               WHEN OTHER
                   STRING 'Erreur suppression: ' WS-RESP
                          DELIMITED SIZE INTO WS-MESSAGE
           END-EVALUATE.
```

### Exemple 2 : DELETE avec vérification préalable

```cobol
      ******************************************************************
      * DELETE avec contrôle - Vérification avant suppression
      ******************************************************************
       4100-SUPPRIMER-CLIENT-CONTROLE.

      *─── Lecture pour vérification ──────────────────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(WS-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Client non trouvé' TO WS-MESSAGE
               GO TO 4100-EXIT
           END-IF

      *─── Vérification règle métier : pas de solde ───────────────────
           IF CLI-SOLDE NOT = 0
               EXEC CICS
                   UNLOCK FILE('CLIENTS')
               END-EXEC
               MOVE 'Impossible: le client a un solde non nul'
                   TO WS-MESSAGE
               GO TO 4100-EXIT
           END-IF

      *─── Vérification règle métier : pas de comptes actifs ──────────
           PERFORM 4110-VERIFIER-COMPTES-ACTIFS
           IF WS-COMPTES-ACTIFS > 0
               EXEC CICS
                   UNLOCK FILE('CLIENTS')
               END-EXEC
               MOVE 'Impossible: des comptes sont encore actifs'
                   TO WS-MESSAGE
               GO TO 4100-EXIT
           END-IF

      *─── Suppression validée ────────────────────────────────────────
           EXEC CICS
               DELETE FILE('CLIENTS')
                      RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'Client supprimé avec succès' TO WS-MESSAGE
      *─────── Journaliser la suppression ─────────────────────────────
               PERFORM 4120-JOURNALISER-SUPPRESSION
           ELSE
               MOVE 'Erreur lors de la suppression' TO WS-MESSAGE
           END-IF.

       4100-EXIT.
           EXIT.
```

### Exemple 3 : DELETE générique (multiple)

```cobol
      ******************************************************************
      * DELETE GENERIC - Suppression de plusieurs enregistrements
      ******************************************************************
       01  WS-NB-SUPPRIMES         PIC 9(5) COMP.

       4200-SUPPRIMER-GROUPE.

      *─── Supprimer tous les clients dont la clé commence par '999' ──
           MOVE '999' TO WS-CLIENT-KEY

           EXEC CICS
               DELETE FILE('CLIENTS')
                      RIDFLD(WS-CLIENT-KEY)
                      KEYLENGTH(3)
                      GENERIC
                      NUMREC(WS-NB-SUPPRIMES)
                      RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               STRING WS-NB-SUPPRIMES ' enregistrements supprimés'
                      DELIMITED SIZE INTO WS-MESSAGE
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 'Aucun enregistrement à supprimer'
                       TO WS-MESSAGE
               ELSE
                   MOVE 'Erreur suppression' TO WS-MESSAGE
               END-IF
           END-IF.
```

## VI-6 Programme complet de traitement

### Exemple : Service de gestion des clients

```cobol
      ******************************************************************
      * Programme : CLNTSRV - Service de traitement clients
      * Couche   : Traitement (Business Logic)
      * Appel    : Via LINK depuis la couche présentation
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTSRV.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-VARIABLES.
           05  WS-RESP             PIC S9(8) COMP.
           05  WS-RESP2            PIC S9(8) COMP.

      *─── Structure enregistrement client ────────────────────────────
       01  WS-CLIENT-REC.
           05  CLI-NUM             PIC X(8).
           05  CLI-NOM             PIC X(30).
           05  CLI-PRENOM          PIC X(20).
           05  CLI-ADRESSE         PIC X(50).
           05  CLI-CODE-POSTAL     PIC X(5).
           05  CLI-VILLE           PIC X(30).
           05  CLI-SOLDE           PIC S9(9)V99 COMP-3.
           05  CLI-DATE-CREATION   PIC X(10).
           05  CLI-STATUT          PIC X(1).
               88 CLI-ACTIF        VALUE 'A'.
               88 CLI-INACTIF      VALUE 'I'.
               88 CLI-SUSPENDU     VALUE 'S'.

      *─── Structure COMMAREA d'échange ───────────────────────────────
       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88 CA-CONSULTER     VALUE 'C'.
               88 CA-CREER         VALUE 'N'.
               88 CA-MODIFIER      VALUE 'M'.
               88 CA-SUPPRIMER     VALUE 'S'.
           05  CA-CODE-RETOUR      PIC 9(2).
               88 CA-OK            VALUE 00.
               88 CA-NOTFND        VALUE 13.
               88 CA-DUPREC        VALUE 14.
               88 CA-ERREUR        VALUE 99.
           05  CA-MESSAGE          PIC X(78).
           05  CA-CLIENT-KEY       PIC X(8).
           05  CA-CLIENT-DATA.
               10  CA-NOM          PIC X(30).
               10  CA-PRENOM       PIC X(20).
               10  CA-ADRESSE      PIC X(50).
               10  CA-CP           PIC X(5).
               10  CA-VILLE        PIC X(30).
               10  CA-SOLDE        PIC S9(9)V99.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(225).

       PROCEDURE DIVISION.

      ******************************************************************
      * 0000-PRINCIPAL : Dispatcher des actions
      ******************************************************************
       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR
           MOVE SPACES TO CA-MESSAGE

           EVALUATE TRUE
               WHEN CA-CONSULTER
                   PERFORM 1000-CONSULTER-CLIENT
               WHEN CA-CREER
                   PERFORM 2000-CREER-CLIENT
               WHEN CA-MODIFIER
                   PERFORM 3000-MODIFIER-CLIENT
               WHEN CA-SUPPRIMER
                   PERFORM 4000-SUPPRIMER-CLIENT
               WHEN OTHER
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Action non reconnue' TO CA-MESSAGE
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA

           EXEC CICS
               RETURN
           END-EXEC.

      ******************************************************************
      * 1000-CONSULTER-CLIENT : Lecture d'un client
      ******************************************************************
       1000-CONSULTER-CLIENT.

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CA-CLIENT-KEY)
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 00 TO CA-CODE-RETOUR
               MOVE CLI-NOM     TO CA-NOM
               MOVE CLI-PRENOM  TO CA-PRENOM
               MOVE CLI-ADRESSE TO CA-ADRESSE
               MOVE CLI-CODE-POSTAL TO CA-CP
               MOVE CLI-VILLE   TO CA-VILLE
               MOVE CLI-SOLDE   TO CA-SOLDE
               MOVE 'Client trouvé' TO CA-MESSAGE
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Client non trouvé' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   STRING 'Erreur lecture: ' WS-RESP
                          DELIMITED SIZE INTO CA-MESSAGE
               END-IF
           END-IF.

      ******************************************************************
      * 2000-CREER-CLIENT : Création d'un nouveau client
      ******************************************************************
       2000-CREER-CLIENT.

      *─── Vérification : le client ne doit pas exister ───────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CA-CLIENT-KEY)
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 14 TO CA-CODE-RETOUR
               MOVE 'Ce numéro client existe déjà' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           IF WS-RESP NOT = DFHRESP(NOTFND)
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur vérification' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

      *─── Préparation de l'enregistrement ────────────────────────────
           INITIALIZE WS-CLIENT-REC
           MOVE CA-CLIENT-KEY TO CLI-NUM
           MOVE CA-NOM        TO CLI-NOM
           MOVE CA-PRENOM     TO CLI-PRENOM
           MOVE CA-ADRESSE    TO CLI-ADRESSE
           MOVE CA-CP         TO CLI-CODE-POSTAL
           MOVE CA-VILLE      TO CLI-VILLE
           MOVE 0             TO CLI-SOLDE
           SET CLI-ACTIF      TO TRUE

      *─── Récupération de la date du jour ────────────────────────────
           EXEC CICS
               ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS
               FORMATTIME ABSTIME(WS-ABSTIME)
                          YYYYMMDD(CLI-DATE-CREATION)
                          DATESEP('-')
           END-EXEC

      *─── Écriture ───────────────────────────────────────────────────
           EXEC CICS
               WRITE FILE('CLIENTS')
                     FROM(WS-CLIENT-REC)
                     RIDFLD(CLI-NUM)
                     RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 00 TO CA-CODE-RETOUR
               MOVE 'Client créé avec succès' TO CA-MESSAGE
           ELSE
               MOVE 99 TO CA-CODE-RETOUR
               STRING 'Erreur création: ' WS-RESP
                      DELIMITED SIZE INTO CA-MESSAGE
           END-IF.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 3000-MODIFIER-CLIENT : Mise à jour d'un client
      ******************************************************************
       3000-MODIFIER-CLIENT.

      *─── Lecture avec verrouillage ──────────────────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CA-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               IF WS-RESP = DFHRESP(NOTFND)
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Client non trouvé' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture' TO CA-MESSAGE
               END-IF
               GO TO 3000-EXIT
           END-IF

      *─── Application des modifications ──────────────────────────────
           IF CA-NOM NOT = SPACES
               MOVE CA-NOM TO CLI-NOM
           END-IF
           IF CA-PRENOM NOT = SPACES
               MOVE CA-PRENOM TO CLI-PRENOM
           END-IF
           IF CA-ADRESSE NOT = SPACES
               MOVE CA-ADRESSE TO CLI-ADRESSE
           END-IF
           IF CA-CP NOT = SPACES
               MOVE CA-CP TO CLI-CODE-POSTAL
           END-IF
           IF CA-VILLE NOT = SPACES
               MOVE CA-VILLE TO CLI-VILLE
           END-IF

      *─── Réécriture ─────────────────────────────────────────────────
           EXEC CICS
               REWRITE FILE('CLIENTS')
                       FROM(WS-CLIENT-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 00 TO CA-CODE-RETOUR
               MOVE 'Client modifié avec succès' TO CA-MESSAGE
           ELSE
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur modification' TO CA-MESSAGE
           END-IF.

       3000-EXIT.
           EXIT.

      ******************************************************************
      * 4000-SUPPRIMER-CLIENT : Suppression d'un client
      ******************************************************************
       4000-SUPPRIMER-CLIENT.

      *─── Lecture pour vérification ──────────────────────────────────
           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-CLIENT-REC)
                    RIDFLD(CA-CLIENT-KEY)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 13 TO CA-CODE-RETOUR
               MOVE 'Client non trouvé' TO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF

      *─── Règle métier : vérifier le solde ───────────────────────────
           IF CLI-SOLDE NOT = 0
               EXEC CICS
                   UNLOCK FILE('CLIENTS')
               END-EXEC
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Suppression impossible: solde non nul'
                   TO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF

      *─── Suppression ────────────────────────────────────────────────
           EXEC CICS
               DELETE FILE('CLIENTS')
                      RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 00 TO CA-CODE-RETOUR
               MOVE 'Client supprimé avec succès' TO CA-MESSAGE
           ELSE
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur suppression' TO CA-MESSAGE
           END-IF.

       4000-EXIT.
           EXIT.
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE VI - RÉSUMÉ                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  VI-1 RÔLE DE LA COUCHE TRAITEMENT                                      │
│       • Logique métier et règles de gestion                             │
│       • Orchestration des opérations CRUD                               │
│       • Position centrale entre Présentation et Données                 │
│                                                                          │
│  VI-2 COMMANDE READ                                                      │
│       • Syntaxe : READ FILE(...) INTO(...) RIDFLD(...)                  │
│       • Options : UPDATE, GENERIC, GTEQ, EQUAL                          │
│       • Codes : NORMAL(0), NOTFND(13), DISABLED(22)                     │
│       • UPDATE nécessaire avant REWRITE ou DELETE                       │
│                                                                          │
│  VI-3 COMMANDE WRITE                                                     │
│       • Syntaxe : WRITE FILE(...) FROM(...) RIDFLD(...)                 │
│       • Crée un nouvel enregistrement                                   │
│       • Codes : NORMAL(0), DUPREC(14), NOSPACE(18)                      │
│       • Bonne pratique : vérifier avec READ avant WRITE                 │
│                                                                          │
│  VI-4 COMMANDE REWRITE                                                   │
│       • Syntaxe : REWRITE FILE(...) FROM(...)                           │
│       • OBLIGATOIRE : READ UPDATE préalable                             │
│       • Codes : NORMAL(0), INVREQ(16)                                   │
│       • Alternative : UNLOCK pour annuler sans modifier                 │
│                                                                          │
│  VI-5 COMMANDE DELETE                                                    │
│       • Deux méthodes : direct ou après READ UPDATE                     │
│       • Option GENERIC pour suppression multiple                        │
│       • NUMREC retourne le nombre d'enregistrements supprimés           │
│       • Codes : NORMAL(0), NOTFND(13)                                   │
│                                                                          │
│  VI-6 BONNES PRATIQUES                                                   │
│       • Toujours vérifier RESP après chaque commande                    │
│       • Utiliser READ UPDATE avant REWRITE/DELETE                       │
│       • Appliquer les règles métier avant les opérations               │
│       • Prévoir UNLOCK en cas d'annulation                              │
│       • Journaliser les opérations sensibles                            │
│                                                                          │
│  TABLEAU RÉCAPITULATIF                                                   │
│  ┌──────────┬─────────────────────────────────────────────────────┐    │
│  │ Commande │ Usage                                                │    │
│  ├──────────┼─────────────────────────────────────────────────────┤    │
│  │ READ     │ Lecture (consultation ou préparation MAJ)           │    │
│  │ WRITE    │ Création d'un nouvel enregistrement                 │    │
│  │ REWRITE  │ Mise à jour (après READ UPDATE)                     │    │
│  │ DELETE   │ Suppression (directe ou après READ UPDATE)          │    │
│  │ UNLOCK   │ Annulation du verrouillage READ UPDATE              │    │
│  └──────────┴─────────────────────────────────────────────────────┘    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Couche de Présentation](05-couche-presentation.md) | [Chapitre VII - Couche des Données](07-couche-donnees.md) |

---
*Formation COBOL - Module CICS*
