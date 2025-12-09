# Chapitre VII - Couche des Données

## VII-1 Rôle principal

### Position dans l'architecture

La **couche des données** (aussi appelée **Data Access Layer** ou **DAL**) est responsable de toutes les interactions avec les sources de données :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    COUCHE DES DONNÉES                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│     ┌─────────────────────────────────────────────────────────────┐    │
│     │                  COUCHE TRAITEMENT                           │    │
│     │                  (Logique métier)                            │    │
│     └───────────────────────────┬─────────────────────────────────┘    │
│                                 │                                       │
│                          COMMAREA / TSI                                 │
│                                 │                                       │
│                                 ▼                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │               COUCHE DES DONNÉES (ce chapitre)                   │   │
│  │                                                                  │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │  RESPONSABILITÉS :                                       │   │   │
│  │  │  • Accès aux fichiers VSAM (KSDS, ESDS, RRDS)           │   │   │
│  │  │  • Requêtes SQL vers DB2                                 │   │   │
│  │  │  • Accès aux bases IMS (DL/I)                           │   │   │
│  │  │  • Gestion des erreurs techniques                        │   │   │
│  │  │  • Transformation des données                            │   │   │
│  │  │  • Abstraction des sources de données                    │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                 │                                       │
│                                 ▼                                       │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                   SOURCES DE DONNÉES                             │   │
│  │                                                                  │   │
│  │   ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐      │   │
│  │   │   VSAM   │  │   DB2    │  │   IMS    │  │   MQ     │      │   │
│  │   │   KSDS   │  │   SQL    │  │   DL/I   │  │ Series   │      │   │
│  │   │   ESDS   │  │          │  │          │  │          │      │   │
│  │   │   RRDS   │  │          │  │          │  │          │      │   │
│  │   └──────────┘  └──────────┘  └──────────┘  └──────────┘      │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Responsabilités détaillées

```
┌─────────────────────────────────────────────────────────────────────────┐
│               RESPONSABILITÉS DE LA COUCHE DONNÉES                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. OPÉRATIONS CRUD                                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Create : Création d'enregistrements                          │   │
│  │  • Read   : Lecture (unitaire et parcours)                      │   │
│  │  • Update : Mise à jour                                         │   │
│  │  • Delete : Suppression                                         │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. GESTION DES ERREURS TECHNIQUES                                      │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Fichier non trouvé (FILENOTFOUND)                            │   │
│  │  • Fichier désactivé (DISABLED)                                 │   │
│  │  • Erreurs I/O (IOERR)                                          │   │
│  │  • Espace disque plein (NOSPACE)                                │   │
│  │  • Enregistrement verrouillé (RECORDBUSY)                       │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  3. TRANSFORMATION DES DONNÉES                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Format fichier → Format programme                            │   │
│  │  • Conversion de types                                          │   │
│  │  • Décompression/Compression                                    │   │
│  │  • Gestion des zones COMP-3                                     │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  4. ABSTRACTION                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Masquer la complexité des accès                              │   │
│  │  • Interface uniforme pour la couche traitement                 │   │
│  │  • Faciliter les changements de source de données               │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Ce que la couche données NE FAIT PAS

| Responsabilité | Appartient à |
|----------------|--------------|
| Règles métier | Couche Traitement |
| Validation fonctionnelle | Couche Traitement |
| Affichage / Saisie | Couche Présentation |
| Gestion des écrans | Couche Présentation |
| Contrôles utilisateur | Couche Présentation |

## VII-2 Interactions avec les autres couches

### Vue d'ensemble des flux

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    FLUX D'INTERACTIONS                                   │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  COUCHE TRAITEMENT                    COUCHE DONNÉES                    │
│  ─────────────────                    ──────────────                    │
│                                                                          │
│  ┌─────────────────┐                  ┌─────────────────┐              │
│  │  CLNTSRV        │  ── LINK ──────► │  CLNTDAO        │              │
│  │  (Service)      │  ◄─ RETURN ───── │  (Data Access)  │              │
│  └─────────────────┘                  └─────────────────┘              │
│         │                                     │                         │
│         │                                     │                         │
│         │  COMMAREA:                          │  COMMANDES CICS:        │
│         │  - Action demandée                  │  - READ FILE            │
│         │  - Clé de recherche                 │  - WRITE FILE           │
│         │  - Données à traiter                │  - REWRITE FILE         │
│         │                                     │  - DELETE FILE          │
│         │                                     │  - STARTBR/READNEXT     │
│         │                                     │                         │
│         │  RETOUR:                            │  RETOUR:                │
│         │  - Code retour                      │  - RESP/RESP2           │
│         │  - Message                          │  - Données lues         │
│         │  - Données résultat                 │  - Nombre traités       │
│         │                                     │                         │
│         ▼                                     ▼                         │
│                                                                          │
│       RÈGLES MÉTIER                      FICHIERS VSAM                  │
│       CALCULS                            BASES DB2                      │
│       VALIDATIONS                        BASES IMS                      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Communication avec la couche traitement

#### Structure COMMAREA standardisée

```cobol
      ******************************************************************
      * Copybook : DAOCOPY - Structure COMMAREA couche données
      ******************************************************************
       01  DAO-COMMAREA.
      *─── EN-TÊTE DE REQUÊTE ─────────────────────────────────────────
          05  DAO-HEADER.
              10  DAO-SERVICE          PIC X(8).
              10  DAO-ACTION           PIC X(4).
                  88  DAO-READ         VALUE 'READ'.
                  88  DAO-WRIT         VALUE 'WRIT'.
                  88  DAO-UPDT         VALUE 'UPDT'.
                  88  DAO-DELT         VALUE 'DELT'.
                  88  DAO-BRWS         VALUE 'BRWS'.
                  88  DAO-NEXT         VALUE 'NEXT'.
                  88  DAO-PREV         VALUE 'PREV'.
                  88  DAO-FRST         VALUE 'FRST'.
                  88  DAO-LAST         VALUE 'LAST'.
              10  DAO-KEY              PIC X(50).
              10  DAO-KEY-LENGTH       PIC 9(4) COMP.

      *─── RÉPONSE ────────────────────────────────────────────────────
          05  DAO-RESPONSE.
              10  DAO-RESP-CODE        PIC 9(4).
                  88  DAO-OK           VALUE 0.
                  88  DAO-NOT-FOUND    VALUE 13.
                  88  DAO-DUPLICATE    VALUE 14.
                  88  DAO-NO-SPACE     VALUE 18.
                  88  DAO-DISABLED     VALUE 22.
                  88  DAO-ERROR        VALUE 9999.
              10  DAO-RESP2-CODE       PIC 9(4).
              10  DAO-MESSAGE          PIC X(78).
              10  DAO-NB-RECORDS       PIC 9(5) COMP.

      *─── ZONE DONNÉES ───────────────────────────────────────────────
          05  DAO-DATA                 PIC X(500).
```

#### Exemple d'appel depuis la couche traitement

```cobol
      ******************************************************************
      * Appel de la couche données depuis le traitement
      ******************************************************************
       PROCEDURE DIVISION.

       1000-CONSULTER-CLIENT.
      *─── Préparation de la requête ──────────────────────────────────
           INITIALIZE DAO-COMMAREA
           MOVE 'CLIENTDA' TO DAO-SERVICE
           SET DAO-READ TO TRUE
           MOVE WS-NUM-CLIENT TO DAO-KEY
           MOVE 8 TO DAO-KEY-LENGTH

      *─── Appel de la couche données ─────────────────────────────────
           EXEC CICS
               LINK PROGRAM('CLNTDAO')
                    COMMAREA(DAO-COMMAREA)
                    LENGTH(LENGTH OF DAO-COMMAREA)
           END-EXEC

      *─── Analyse de la réponse ──────────────────────────────────────
           IF DAO-OK
               MOVE DAO-DATA TO WS-CLIENT-REC
               PERFORM 1100-APPLIQUER-REGLES-METIER
           ELSE
               IF DAO-NOT-FOUND
                   MOVE 'Client non trouvé' TO WS-MESSAGE
               ELSE
                   MOVE DAO-MESSAGE TO WS-MESSAGE
               END-IF
           END-IF.
```

### Gestion des transactions

#### Concept de transaction CICS

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    GESTION DES TRANSACTIONS                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Une transaction CICS est une UNITÉ DE TRAVAIL atomique :               │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                                                                  │   │
│  │  Début transaction                                               │   │
│  │         │                                                        │   │
│  │         ▼                                                        │   │
│  │  ┌──────────────┐                                               │   │
│  │  │  READ UPDATE │  ◄── Lecture compte source                    │   │
│  │  └──────────────┘                                               │   │
│  │         │                                                        │   │
│  │         ▼                                                        │   │
│  │  ┌──────────────┐                                               │   │
│  │  │   REWRITE    │  ◄── Débit compte source                      │   │
│  │  └──────────────┘                                               │   │
│  │         │                                                        │   │
│  │         ▼                                                        │   │
│  │  ┌──────────────┐                                               │   │
│  │  │  READ UPDATE │  ◄── Lecture compte destination               │   │
│  │  └──────────────┘                                               │   │
│  │         │                                                        │   │
│  │         ▼                                                        │   │
│  │  ┌──────────────┐                                               │   │
│  │  │   REWRITE    │  ◄── Crédit compte destination                │   │
│  │  └──────────────┘                                               │   │
│  │         │                                                        │   │
│  │         ▼                                                        │   │
│  │  ┌──────────────┐                                               │   │
│  │  │  SYNCPOINT   │  ◄── Validation de toutes les opérations     │   │
│  │  └──────────────┘       OU                                      │   │
│  │  ┌──────────────┐                                               │   │
│  │  │  SYNCPOINT   │  ◄── Annulation en cas d'erreur              │   │
│  │  │  ROLLBACK    │                                               │   │
│  │  └──────────────┘                                               │   │
│  │                                                                  │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  PROPRIÉTÉS ACID :                                                       │
│  • Atomicité   : Tout ou rien                                           │
│  • Cohérence   : État valide avant/après                                │
│  • Isolation   : Invisibilité des modifications en cours                │
│  • Durabilité  : Persistance après validation                           │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Commandes SYNCPOINT

```cobol
      ******************************************************************
      * Gestion des transactions - SYNCPOINT et ROLLBACK
      ******************************************************************

      *─── SYNCPOINT : Valider les modifications ──────────────────────
           EXEC CICS
               SYNCPOINT
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               DISPLAY 'Transaction validée'
           ELSE
               DISPLAY 'Erreur validation: ' WS-RESP
           END-IF

      *─── SYNCPOINT ROLLBACK : Annuler les modifications ─────────────
           EXEC CICS
               SYNCPOINT ROLLBACK
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               DISPLAY 'Transaction annulée'
           END-IF
```

#### Exemple de transaction complète

```cobol
      ******************************************************************
      * Programme : VIRMDAO - Virement entre comptes (couche données)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VIRMDAO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-COMPTE-REC.
           05  CPT-NUMERO          PIC X(12).
           05  CPT-SOLDE           PIC S9(11)V99 COMP-3.
           05  CPT-STATUT          PIC X(1).

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-ERREUR-DETECTEE      PIC X VALUE 'N'.
           88 ERREUR-DETECTEE      VALUE 'O'.
           88 PAS-ERREUR           VALUE 'N'.

       01  WS-COMMAREA.
           05  VIR-CPT-SOURCE      PIC X(12).
           05  VIR-CPT-DEST        PIC X(12).
           05  VIR-MONTANT         PIC S9(11)V99.
           05  VIR-RESP-CODE       PIC 9(4).
           05  VIR-MESSAGE         PIC X(78).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(120).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           MOVE DFHCOMMAREA TO WS-COMMAREA
           SET PAS-ERREUR TO TRUE

      *─── Étape 1 : Débiter le compte source ─────────────────────────
           PERFORM 1000-DEBITER-SOURCE

           IF ERREUR-DETECTEE
               PERFORM 9000-ROLLBACK
               GO TO 0000-FIN
           END-IF

      *─── Étape 2 : Créditer le compte destination ───────────────────
           PERFORM 2000-CREDITER-DESTINATION

           IF ERREUR-DETECTEE
               PERFORM 9000-ROLLBACK
               GO TO 0000-FIN
           END-IF

      *─── Étape 3 : Valider la transaction ───────────────────────────
           PERFORM 3000-VALIDER-TRANSACTION.

       0000-FIN.
           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-DEBITER-SOURCE : Débit du compte source
      ******************************************************************
       1000-DEBITER-SOURCE.

           MOVE VIR-CPT-SOURCE TO CPT-NUMERO

           EXEC CICS
               READ FILE('COMPTES')
                    INTO(WS-COMPTE-REC)
                    RIDFLD(CPT-NUMERO)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET ERREUR-DETECTEE TO TRUE
               MOVE 13 TO VIR-RESP-CODE
               MOVE 'Compte source non trouvé' TO VIR-MESSAGE
               GO TO 1000-EXIT
           END-IF

      *─── Vérification du solde (couche données : pas de règle) ──────
      *─── Note: la vérification métier devrait être en couche trait.
           SUBTRACT VIR-MONTANT FROM CPT-SOLDE

           EXEC CICS
               REWRITE FILE('COMPTES')
                       FROM(WS-COMPTE-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET ERREUR-DETECTEE TO TRUE
               MOVE 99 TO VIR-RESP-CODE
               MOVE 'Erreur débit compte source' TO VIR-MESSAGE
           END-IF.

       1000-EXIT.
           EXIT.

      ******************************************************************
      * 2000-CREDITER-DESTINATION : Crédit du compte destination
      ******************************************************************
       2000-CREDITER-DESTINATION.

           MOVE VIR-CPT-DEST TO CPT-NUMERO

           EXEC CICS
               READ FILE('COMPTES')
                    INTO(WS-COMPTE-REC)
                    RIDFLD(CPT-NUMERO)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET ERREUR-DETECTEE TO TRUE
               MOVE 13 TO VIR-RESP-CODE
               MOVE 'Compte destination non trouvé' TO VIR-MESSAGE
               GO TO 2000-EXIT
           END-IF

           ADD VIR-MONTANT TO CPT-SOLDE

           EXEC CICS
               REWRITE FILE('COMPTES')
                       FROM(WS-COMPTE-REC)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               SET ERREUR-DETECTEE TO TRUE
               MOVE 99 TO VIR-RESP-CODE
               MOVE 'Erreur crédit compte destination' TO VIR-MESSAGE
           END-IF.

       2000-EXIT.
           EXIT.

      ******************************************************************
      * 3000-VALIDER-TRANSACTION : Commit
      ******************************************************************
       3000-VALIDER-TRANSACTION.

           EXEC CICS
               SYNCPOINT
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 0 TO VIR-RESP-CODE
               MOVE 'Virement effectué avec succès' TO VIR-MESSAGE
           ELSE
               MOVE 99 TO VIR-RESP-CODE
               MOVE 'Erreur validation transaction' TO VIR-MESSAGE
           END-IF.

      ******************************************************************
      * 9000-ROLLBACK : Annulation en cas d'erreur
      ******************************************************************
       9000-ROLLBACK.

           EXEC CICS
               SYNCPOINT ROLLBACK
           END-EXEC.
```

### Abstraction de la base de données

#### Principe de l'abstraction

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ABSTRACTION DES SOURCES DE DONNÉES                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  AVANTAGE : La couche traitement ne connaît pas la source réelle        │
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  COUCHE TRAITEMENT                                               │   │
│  │                                                                  │   │
│  │  CALL 'CLNTDAO' USING DAO-COMMAREA                              │   │
│  │  (Même appel quelle que soit la source)                         │   │
│  │                                                                  │   │
│  └───────────────────────────┬─────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  COUCHE DONNÉES (DAO)                                            │   │
│  │                                                                  │   │
│  │  EVALUATE WS-SOURCE-TYPE                                        │   │
│  │      WHEN 'VSAM'                                                │   │
│  │          PERFORM ACCES-VSAM                                     │   │
│  │      WHEN 'DB2'                                                 │   │
│  │          PERFORM ACCES-DB2                                      │   │
│  │      WHEN 'IMS'                                                 │   │
│  │          PERFORM ACCES-IMS                                      │   │
│  │  END-EVALUATE                                                   │   │
│  │                                                                  │   │
│  └─────┬─────────────────────┬─────────────────────┬───────────────┘   │
│        │                     │                     │                    │
│        ▼                     ▼                     ▼                    │
│   ┌─────────┐           ┌─────────┐           ┌─────────┐              │
│   │  VSAM   │           │   DB2   │           │   IMS   │              │
│   │  KSDS   │           │         │           │   DL/I  │              │
│   └─────────┘           └─────────┘           └─────────┘              │
│                                                                          │
│  BÉNÉFICES :                                                            │
│  • Changement de source sans modifier la couche traitement             │
│  • Tests unitaires facilités (mock du DAO)                             │
│  • Migration progressive possible (VSAM → DB2)                         │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

#### Exemple de DAO multi-sources

```cobol
      ******************************************************************
      * Programme : CLNTDAO - Data Access Object pour les clients
      * Support  : VSAM, DB2 (configurable)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLNTDAO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CONFIG.
           05  WS-SOURCE-TYPE      PIC X(4) VALUE 'VSAM'.
               88 SOURCE-VSAM      VALUE 'VSAM'.
               88 SOURCE-DB2       VALUE 'DB2 '.

      *─── Structure VSAM ─────────────────────────────────────────────
       01  WS-VSAM-REC.
           05  VSAM-NUM            PIC X(8).
           05  VSAM-NOM            PIC X(30).
           05  VSAM-SOLDE          PIC S9(9)V99 COMP-3.

      *─── Structure DB2 ──────────────────────────────────────────────
           EXEC SQL INCLUDE SQLCA END-EXEC.

       01  DB2-CLIENT.
           05  DB2-NUM             PIC X(8).
           05  DB2-NOM             PIC X(30).
           05  DB2-SOLDE           PIC S9(9)V99.

       01  WS-RESP                 PIC S9(8) COMP.

           COPY DAOCOPY.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(650).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           MOVE DFHCOMMAREA TO DAO-COMMAREA

           EVALUATE TRUE
               WHEN DAO-READ
                   PERFORM 1000-LIRE
               WHEN DAO-WRIT
                   PERFORM 2000-ECRIRE
               WHEN DAO-UPDT
                   PERFORM 3000-METTRE-A-JOUR
               WHEN DAO-DELT
                   PERFORM 4000-SUPPRIMER
           END-EVALUATE

           MOVE DAO-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-LIRE : Lecture selon la source configurée
      ******************************************************************
       1000-LIRE.

           EVALUATE TRUE
               WHEN SOURCE-VSAM
                   PERFORM 1100-LIRE-VSAM
               WHEN SOURCE-DB2
                   PERFORM 1200-LIRE-DB2
           END-EVALUATE.

       1100-LIRE-VSAM.
      *─── Lecture VSAM ───────────────────────────────────────────────
           MOVE DAO-KEY TO VSAM-NUM

           EXEC CICS
               READ FILE('CLIENTS')
                    INTO(WS-VSAM-REC)
                    RIDFLD(VSAM-NUM)
                    RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               SET DAO-OK TO TRUE
               PERFORM 1110-MAPPER-VSAM-VERS-DAO
           ELSE
               IF WS-RESP = DFHRESP(NOTFND)
                   SET DAO-NOT-FOUND TO TRUE
                   MOVE 'Client non trouvé (VSAM)' TO DAO-MESSAGE
               ELSE
                   SET DAO-ERROR TO TRUE
                   STRING 'Erreur VSAM: ' WS-RESP
                          DELIMITED SIZE INTO DAO-MESSAGE
               END-IF
           END-IF.

       1110-MAPPER-VSAM-VERS-DAO.
      *─── Transformation format VSAM → format DAO ────────────────────
           MOVE VSAM-NUM   TO DAO-DATA(1:8)
           MOVE VSAM-NOM   TO DAO-DATA(9:30)
           MOVE VSAM-SOLDE TO DAO-DATA(39:8).

       1200-LIRE-DB2.
      *─── Lecture DB2 ────────────────────────────────────────────────
           MOVE DAO-KEY TO DB2-NUM

           EXEC SQL
               SELECT CLI_NUM, CLI_NOM, CLI_SOLDE
               INTO :DB2-NUM, :DB2-NOM, :DB2-SOLDE
               FROM CLIENTS
               WHERE CLI_NUM = :DB2-NUM
           END-EXEC

           EVALUATE SQLCODE
               WHEN 0
                   SET DAO-OK TO TRUE
                   PERFORM 1210-MAPPER-DB2-VERS-DAO
               WHEN 100
                   SET DAO-NOT-FOUND TO TRUE
                   MOVE 'Client non trouvé (DB2)' TO DAO-MESSAGE
               WHEN OTHER
                   SET DAO-ERROR TO TRUE
                   STRING 'Erreur DB2 SQLCODE=' SQLCODE
                          DELIMITED SIZE INTO DAO-MESSAGE
           END-EVALUATE.

       1210-MAPPER-DB2-VERS-DAO.
      *─── Transformation format DB2 → format DAO ─────────────────────
           MOVE DB2-NUM   TO DAO-DATA(1:8)
           MOVE DB2-NOM   TO DAO-DATA(9:30)
           MOVE DB2-SOLDE TO DAO-DATA(39:8).

      ******************************************************************
      * Autres opérations (2000, 3000, 4000) suivent le même modèle
      ******************************************************************
```

## VII-3 Sécurité dans la couche données

### Aspects de sécurité

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    SÉCURITÉ DANS LA COUCHE DONNÉES                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. CONTRÔLE D'ACCÈS AUX RESSOURCES                                     │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • RACF/ACF2/Top Secret : Autorisation sur les fichiers        │   │
│  │  • CICS Security : Vérification à l'exécution                  │   │
│  │  • Profils utilisateur : Droits READ, UPDATE, DELETE           │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  2. PROTECTION DES DONNÉES SENSIBLES                                    │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Masquage des données sensibles (numéros de carte)           │   │
│  │  • Chiffrement des données au repos                             │   │
│  │  • Journalisation des accès                                     │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  3. INTÉGRITÉ DES DONNÉES                                               │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Verrouillage pendant mise à jour (READ UPDATE)              │   │
│  │  • Gestion des conflits d'accès (RECORDBUSY)                   │   │
│  │  • Transactions atomiques (SYNCPOINT)                          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
│  4. AUDIT ET TRAÇABILITÉ                                                │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  • Journal des opérations (TD Queue)                            │   │
│  │  • Horodatage des modifications                                 │   │
│  │  • Identification de l'utilisateur (EIBTRMID, USERID)          │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Exemple de journalisation

```cobol
      ******************************************************************
      * Journalisation des opérations sensibles
      ******************************************************************
       01  WS-JOURNAL-REC.
           05  JRN-TIMESTAMP       PIC X(26).
           05  JRN-USERID          PIC X(8).
           05  JRN-TERMINAL        PIC X(4).
           05  JRN-TRANSACTION     PIC X(4).
           05  JRN-OPERATION       PIC X(10).
           05  JRN-TABLE           PIC X(8).
           05  JRN-CLE             PIC X(20).
           05  JRN-RESULTAT        PIC 9(4).

       5000-JOURNALISER.

      *─── Récupération des informations de contexte ──────────────────
           EXEC CICS
               ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS
               FORMATTIME ABSTIME(WS-ABSTIME)
                          YYYYMMDD(WS-DATE)
                          TIME(WS-TIME)
                          DATESEP('-')
                          TIMESEP(':')
           END-EXEC

           STRING WS-DATE ' ' WS-TIME
                  DELIMITED SIZE INTO JRN-TIMESTAMP

           EXEC CICS
               ASSIGN USERID(JRN-USERID)
           END-EXEC

           MOVE EIBTRMID TO JRN-TERMINAL
           MOVE EIBTRNID TO JRN-TRANSACTION
           MOVE WS-OPERATION TO JRN-OPERATION
           MOVE 'CLIENTS' TO JRN-TABLE
           MOVE DAO-KEY TO JRN-CLE
           MOVE DAO-RESP-CODE TO JRN-RESULTAT

      *─── Écriture dans la queue de journalisation ───────────────────
           EXEC CICS
               WRITEQ TD QUEUE('AUDT')
                         FROM(WS-JOURNAL-REC)
                         LENGTH(LENGTH OF WS-JOURNAL-REC)
           END-EXEC.
```

### Gestion des erreurs d'accès

```cobol
      ******************************************************************
      * Gestion centralisée des erreurs d'accès
      ******************************************************************
       6000-GERER-ERREUR-ACCES.

           EVALUATE WS-RESP
               WHEN DFHRESP(NOTAUTH)
                   SET DAO-ERROR TO TRUE
                   MOVE 'Accès non autorisé au fichier'
                       TO DAO-MESSAGE
                   PERFORM 5000-JOURNALISER

               WHEN DFHRESP(DISABLED)
                   SET DAO-DISABLED TO TRUE
                   MOVE 'Fichier temporairement indisponible'
                       TO DAO-MESSAGE

               WHEN DFHRESP(NOTOPEN)
                   SET DAO-ERROR TO TRUE
                   MOVE 'Fichier non ouvert - Contactez admin'
                       TO DAO-MESSAGE

               WHEN DFHRESP(RECORDBUSY)
                   SET DAO-ERROR TO TRUE
                   MOVE 'Enregistrement en cours de modification'
                       TO DAO-MESSAGE

               WHEN DFHRESP(IOERR)
                   SET DAO-ERROR TO TRUE
                   MOVE 'Erreur I/O - Contactez support technique'
                       TO DAO-MESSAGE
                   PERFORM 5000-JOURNALISER

               WHEN OTHER
                   SET DAO-ERROR TO TRUE
                   STRING 'Erreur inattendue: RESP=' WS-RESP
                          ' RESP2=' WS-RESP2
                          DELIMITED SIZE INTO DAO-MESSAGE
                   PERFORM 5000-JOURNALISER
           END-EVALUATE.
```

## VII-4 Parcours de fichiers (Browse)

### Commandes de parcours VSAM

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    PARCOURS DE FICHIERS (BROWSE)                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  SÉQUENCE DE PARCOURS :                                                 │
│                                                                          │
│  1. STARTBR ─────► Initialise le parcours                              │
│        │                                                                 │
│        ▼                                                                 │
│  2. READNEXT ────► Lecture suivante                                     │
│        │           (répéter jusqu'à fin de fichier)                     │
│        │                                                                 │
│        ├──────────► READPREV possible (sens inverse)                    │
│        │                                                                 │
│        ▼                                                                 │
│  3. ENDBR ───────► Termine le parcours                                  │
│                    (obligatoire !)                                      │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Exemple de parcours

```cobol
      ******************************************************************
      * Parcours de fichier VSAM - Liste des clients
      ******************************************************************
       01  WS-CLIENT-TABLE.
           05  WS-NB-CLIENTS       PIC 9(4) COMP VALUE 0.
           05  WS-CLIENT-ENTRY OCCURS 100 TIMES.
               10  WS-ENT-NUM      PIC X(8).
               10  WS-ENT-NOM      PIC X(30).
               10  WS-ENT-SOLDE    PIC S9(9)V99.

       01  WS-FIN-FICHIER          PIC X VALUE 'N'.
           88 FIN-FICHIER          VALUE 'O'.
           88 PAS-FIN              VALUE 'N'.

       01  WS-START-KEY            PIC X(8).

       7000-LISTER-CLIENTS.

           SET PAS-FIN TO TRUE
           MOVE 0 TO WS-NB-CLIENTS
           MOVE DAO-KEY TO WS-START-KEY

      *─── Initialisation du parcours ─────────────────────────────────
           EXEC CICS
               STARTBR FILE('CLIENTS')
                       RIDFLD(WS-START-KEY)
                       GTEQ
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               IF WS-RESP = DFHRESP(NOTFND)
                   SET DAO-NOT-FOUND TO TRUE
                   MOVE 'Aucun client trouvé' TO DAO-MESSAGE
               ELSE
                   SET DAO-ERROR TO TRUE
                   MOVE 'Erreur initialisation parcours' TO DAO-MESSAGE
               END-IF
               GO TO 7000-EXIT
           END-IF

      *─── Boucle de lecture ──────────────────────────────────────────
           PERFORM UNTIL FIN-FICHIER OR WS-NB-CLIENTS >= 100

               EXEC CICS
                   READNEXT FILE('CLIENTS')
                            INTO(WS-VSAM-REC)
                            RIDFLD(WS-START-KEY)
                            RESP(WS-RESP)
               END-EXEC

               EVALUATE WS-RESP
                   WHEN DFHRESP(NORMAL)
                       ADD 1 TO WS-NB-CLIENTS
                       MOVE VSAM-NUM TO
                           WS-ENT-NUM(WS-NB-CLIENTS)
                       MOVE VSAM-NOM TO
                           WS-ENT-NOM(WS-NB-CLIENTS)
                       MOVE VSAM-SOLDE TO
                           WS-ENT-SOLDE(WS-NB-CLIENTS)
                   WHEN DFHRESP(ENDFILE)
                       SET FIN-FICHIER TO TRUE
                   WHEN OTHER
                       SET FIN-FICHIER TO TRUE
                       SET DAO-ERROR TO TRUE
               END-EVALUATE

           END-PERFORM

      *─── Fin du parcours (OBLIGATOIRE) ──────────────────────────────
           EXEC CICS
               ENDBR FILE('CLIENTS')
           END-EXEC

           IF WS-NB-CLIENTS > 0
               SET DAO-OK TO TRUE
               MOVE WS-NB-CLIENTS TO DAO-NB-RECORDS
               MOVE WS-CLIENT-TABLE TO DAO-DATA
           ELSE
               SET DAO-NOT-FOUND TO TRUE
               MOVE 'Aucun client dans la sélection' TO DAO-MESSAGE
           END-IF.

       7000-EXIT.
           EXIT.
```

## VII-5 Schéma récapitulatif de l'architecture CICS-VSAM

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE CICS-VSAM                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │              COUCHE PRÉSENTATION (Front-End)                       │  │
│  │  ┌─────────┐   ┌─────────┐   ┌─────────────────────────────────┐  │  │
│  │  │   MAP   │   │ MAPSET  │   │  Transaction CICS (TR01)        │  │  │
│  │  │ (Écran) │   │         │   │  Interface utilisateur          │  │  │
│  │  └─────────┘   └─────────┘   └─────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                              │                                          │
│                      COMMAREA / TSI                                     │
│                              │                                          │
│                              ▼                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │              COUCHE TRAITEMENT (Back-End)                          │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │           Programme COBOL-CICS                               │  │  │
│  │  │                                                              │  │  │
│  │  │  ┌──────────┐ ┌──────────┐ ┌─────────┐ ┌─────────────────┐  │  │  │
│  │  │  │   READ   │ │  WRITE   │ │ REWRITE │ │     DELETE      │  │  │  │
│  │  │  │          │ │          │ │         │ │                 │  │  │  │
│  │  │  │ Lecture  │ │ Écriture │ │  Mise   │ │   Suppression   │  │  │  │
│  │  │  │          │ │          │ │ à jour  │ │                 │  │  │  │
│  │  │  └──────────┘ └──────────┘ └─────────┘ └─────────────────┘  │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                              │                                          │
│                      COMMAREA / TSI                                     │
│                              │                                          │
│                              ▼                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │              COUCHE DONNÉES (Data Layer)                           │  │
│  │  ┌─────────────────────────────────────────────────────────────┐  │  │
│  │  │                    FICHIERS VSAM                             │  │  │
│  │  │                                                              │  │  │
│  │  │  ┌─────────┐  ┌─────────┐  ┌─────────────────────────────┐  │  │  │
│  │  │  │  KSDS   │  │  ESDS   │  │           RRDS              │  │  │  │
│  │  │  │ (Clé)   │  │ (Séq.)  │  │        (Relatif)            │  │  │  │
│  │  │  └─────────┘  └─────────┘  └─────────────────────────────┘  │  │  │
│  │  │                                                              │  │  │
│  │  │       Intégrité │ Cohérence │ Sécurité │ Durabilité         │  │  │
│  │  └─────────────────────────────────────────────────────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────┘  │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## VII-6 Tableau récapitulatif des commandes CICS

| Commande | Syntaxe minimale | Description |
|----------|------------------|-------------|
| **READ** | `EXEC CICS READ FILE('name') INTO(data) RIDFLD(key) END-EXEC` | Lecture d'un enregistrement |
| **WRITE** | `EXEC CICS WRITE FILE('name') FROM(data) RIDFLD(key) END-EXEC` | Écriture d'un nouvel enregistrement |
| **REWRITE** | `EXEC CICS REWRITE FILE('name') FROM(data) END-EXEC` | Mise à jour (après READ UPDATE) |
| **DELETE** | `EXEC CICS DELETE FILE('name') END-EXEC` | Suppression (après READ UPDATE) |
| **STARTBR** | `EXEC CICS STARTBR FILE('name') RIDFLD(key) END-EXEC` | Début de parcours séquentiel |
| **READNEXT** | `EXEC CICS READNEXT FILE('name') INTO(data) RIDFLD(key) END-EXEC` | Lecture suivante |
| **READPREV** | `EXEC CICS READPREV FILE('name') INTO(data) RIDFLD(key) END-EXEC` | Lecture précédente |
| **ENDBR** | `EXEC CICS ENDBR FILE('name') END-EXEC` | Fin de parcours |
| **UNLOCK** | `EXEC CICS UNLOCK FILE('name') END-EXEC` | Libérer verrou sans modification |
| **SYNCPOINT** | `EXEC CICS SYNCPOINT END-EXEC` | Valider les modifications |
| **SYNCPOINT ROLLBACK** | `EXEC CICS SYNCPOINT ROLLBACK END-EXEC` | Annuler les modifications |

### Codes de retour (RESP) principaux

| Code DFHRESP | Valeur | Signification |
|--------------|--------|---------------|
| `NORMAL` | 0 | Opération réussie |
| `NOTFND` | 13 | Enregistrement non trouvé |
| `DUPREC` | 14 | Clé en double |
| `INVREQ` | 16 | Requête invalide |
| `NOSPACE` | 18 | Plus d'espace |
| `NOTOPEN` | 19 | Fichier non ouvert |
| `DISABLED` | 22 | Fichier désactivé |
| `IOERR` | 17 | Erreur d'entrée/sortie |
| `ENDFILE` | 20 | Fin de fichier (browse) |
| `FILENOTFOUND` | 12 | Fichier non trouvé |

---

## Conclusion

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE VII - CONCLUSION                             │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  La couche des données est ESSENTIELLE pour :                           │
│                                                                          │
│  ✓ ABSTRACTION                                                           │
│    • Masquer la complexité des accès aux sources de données            │
│    • Permettre le changement de technologie (VSAM → DB2)               │
│    • Fournir une interface uniforme à la couche traitement             │
│                                                                          │
│  ✓ FIABILITÉ                                                             │
│    • Gestion centralisée des erreurs                                    │
│    • Transactions atomiques (SYNCPOINT)                                 │
│    • Intégrité des données garantie                                     │
│                                                                          │
│  ✓ SÉCURITÉ                                                              │
│    • Contrôle d'accès aux ressources                                    │
│    • Journalisation des opérations                                      │
│    • Protection des données sensibles                                   │
│                                                                          │
│  ✓ MAINTENABILITÉ                                                        │
│    • Code d'accès données isolé                                         │
│    • Tests unitaires facilités                                          │
│    • Évolutions localisées                                              │
│                                                                          │
│  L'architecture multicouches avec une couche données bien conçue        │
│  est la base d'applications mainframe robustes et évolutives.           │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE VII - RÉSUMÉ                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  VII-1 RÔLE PRINCIPAL                                                    │
│        • Accès aux sources de données (VSAM, DB2, IMS)                 │
│        • Opérations CRUD                                                │
│        • Gestion des erreurs techniques                                 │
│        • Transformation des formats de données                          │
│                                                                          │
│  VII-2 INTERACTIONS                                                      │
│        • Communication via COMMAREA standardisée                        │
│        • Appel par LINK depuis la couche traitement                    │
│        • Retour de codes et messages                                    │
│                                                                          │
│  GESTION DES TRANSACTIONS                                                │
│        • SYNCPOINT : validation des modifications                       │
│        • SYNCPOINT ROLLBACK : annulation                                │
│        • Propriétés ACID respectées                                     │
│                                                                          │
│  ABSTRACTION                                                             │
│        • DAO (Data Access Object) pattern                               │
│        • Interface uniforme multi-sources                               │
│        • Migration facilitée (VSAM → DB2)                               │
│                                                                          │
│  VII-3 SÉCURITÉ                                                          │
│        • Contrôle d'accès (RACF)                                        │
│        • Journalisation (TD Queue)                                      │
│        • Verrouillage (READ UPDATE)                                     │
│        • Gestion des erreurs d'accès                                    │
│                                                                          │
│  VII-4 PARCOURS DE FICHIERS                                              │
│        • STARTBR : initialisation                                       │
│        • READNEXT/READPREV : lecture séquentielle                       │
│        • ENDBR : terminaison (obligatoire !)                            │
│                                                                          │
│  BONNES PRATIQUES                                                        │
│        • Pas de logique métier dans la couche données                  │
│        • Interface claire et documentée                                 │
│        • Journalisation des opérations sensibles                        │
│        • Toujours fermer les browse (ENDBR)                             │
│        • Gérer tous les codes retour                                    │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI - Couche de Traitement](06-couche-traitement.md) | [Chapitre VIII - Travaux Pratiques](08-travaux-pratiques.md) |

---
*Formation COBOL - Module CICS*
