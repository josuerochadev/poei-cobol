# Chapitre VIII - Travaux Pratiques

## VIII-1 Présentation de l'application

### Objectif

Ce chapitre présente un **exemple pratique complet** mettant en œuvre l'architecture multicouches CICS avec des données stockées dans des fichiers VSAM. L'application gère les **crédits des employés** d'une entreprise.

### Contexte fonctionnel

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    APPLICATION GESTION DES CRÉDITS                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  CONTEXTE :                                                              │
│  ─────────                                                               │
│  Une entreprise propose des crédits à ses employés. L'application       │
│  permet de :                                                             │
│  • Consulter les informations d'un employé                              │
│  • Vérifier s'il a contracté un crédit                                  │
│  • Afficher les détails du crédit                                       │
│  • Mettre à jour le reste à payer après une échéance                    │
│                                                                          │
│  RÈGLES MÉTIER :                                                         │
│  ───────────────                                                         │
│  1. Un employé peut avoir un crédit (Etat-CRED-EMPL = 'Y') ou non ('N') │
│  2. Si crédit soldé (RESTE-CREDIT = 0), passer Etat-CRED-EMPL à 'N'    │
│  3. Le montant de l'échéance est fixe pour chaque crédit               │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Structures de données

L'application utilise deux fichiers VSAM de type KSDS :

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    STRUCTURES VSAM                                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  FICHIER EMPLOYE (KSDS)                 FICHIER CRE-EMP (KSDS)          │
│  ──────────────────────                 ────────────────────────         │
│                                                                          │
│  ┌─────────────────────────┐            ┌─────────────────────────┐    │
│  │ ID-EMPL        X(6)     │ ◄── Clé    │ ID-EMPL        X(6)     │ ◄──│
│  ├─────────────────────────┤            ├─────────────────────────┤    │
│  │ NAME-EMPL      X(30)    │            │ LIB-CREDIT     X(20)    │    │
│  │ DEPT-EMPL      X(10)    │            │ MONTANT-TOTAL  9(7)V99  │    │
│  │ SALAIRE-EMPL   9(7)V99  │            │ MONTANT-ECH    9(5)V99  │    │
│  │ ETAT-CRED-EMPL X(1)     │            │ RESTE-CREDIT   9(7)V99  │    │
│  │   'Y' = Crédit actif    │            └─────────────────────────┘    │
│  │   'N' = Pas de crédit   │                                           │
│  └─────────────────────────┘                                           │
│                                                                          │
│  Relation : ID-EMPL (EMPLOYE) ──────────► ID-EMPL (CRE-EMP)            │
│             Si ETAT-CRED-EMPL = 'Y'                                     │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Données de test

**Fichier EMPLOYE (6 enregistrements) :**

| ID-EMPL | NAME-EMPL | DEPT-EMPL | SALAIRE | ETAT-CRED |
|---------|-----------|-----------|---------|-----------|
| EMP001 | MARTIN JEAN | COMPTA | 3500.00 | Y |
| EMP002 | DUPONT MARIE | INFO | 4200.00 | N |
| EMP003 | DURAND PIERRE | RH | 3800.00 | Y |
| EMP004 | LEROY SOPHIE | COMPTA | 3200.00 | Y |
| EMP005 | MOREAU PAUL | INFO | 4500.00 | N |
| EMP006 | SIMON ANNE | RH | 3600.00 | Y |

**Fichier CRE-EMP (4 enregistrements) :**

| ID-EMPL | LIB-CREDIT | MONTANT-TOTAL | MONTANT-ECH | RESTE |
|---------|------------|---------------|-------------|-------|
| EMP001 | PRET AUTO | 15000.00 | 450.00 | 12600.00 |
| EMP003 | PRET IMMO | 50000.00 | 800.00 | 48400.00 |
| EMP004 | PRET PERSO | 5000.00 | 250.00 | 250.00 |
| EMP006 | PRET ETUDES | 8000.00 | 200.00 | 6800.00 |

## VIII-2 Architecture de l'application

### Vue d'ensemble

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    ARCHITECTURE 3 TIERS                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                        TERMINAL 3270                             │   │
│  │  ┌─────────────────────────────────────────────────────────┐   │   │
│  │  │  GESTION DES CREDITS EMPLOYES              CRED         │   │   │
│  │  │─────────────────────────────────────────────────────────│   │   │
│  │  │  ID Employé : [______]                                   │   │   │
│  │  │                                                          │   │   │
│  │  │  Nom        : XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX            │   │   │
│  │  │  Département: XXXXXXXXXX     Salaire: ZZZ,ZZZ.99        │   │   │
│  │  │                                                          │   │   │
│  │  │  ═══════════════ INFORMATIONS CREDIT ═══════════════   │   │   │
│  │  │  Libellé    : XXXXXXXXXXXXXXXXXXXX                      │   │   │
│  │  │  Montant    : ZZZ,ZZZ.99     Échéance: ZZ,ZZZ.99       │   │   │
│  │  │  Reste      : ZZZ,ZZZ.99                                │   │   │
│  │  │─────────────────────────────────────────────────────────│   │   │
│  │  │  PF3=Quitter  PF5=Payer échéance  ENTER=Rechercher      │   │   │
│  │  └─────────────────────────────────────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                    │                                    │
│                                    ▼                                    │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  COUCHE PRÉSENTATION - Programme CREDPRES                       │   │
│  │  Transaction : CRED                                              │   │
│  │  ────────────────────────────────────────────────────────────── │   │
│  │  • Affichage écran BMS (CREDSET/CREDMAP)                        │   │
│  │  • Réception saisie utilisateur                                  │   │
│  │  • Validation format ID employé                                  │   │
│  │  • Gestion touches fonction (ENTER, PF3, PF5)                   │   │
│  └───────────────────────────┬─────────────────────────────────────┘   │
│                              │ COMMAREA                                 │
│                              ▼                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  COUCHE TRAITEMENT - Programme CREDTRT                          │   │
│  │  ────────────────────────────────────────────────────────────── │   │
│  │  • Lecture employé (appel CREDDAO)                              │   │
│  │  • Vérification état crédit (ETAT-CRED-EMPL = 'Y' ?)           │   │
│  │  • Lecture crédit si existant (appel CREDDAO)                   │   │
│  │  • Calcul nouveau reste après paiement                          │   │
│  │  • Mise à jour état crédit si soldé                             │   │
│  └───────────────────────────┬─────────────────────────────────────┘   │
│                              │ COMMAREA                                 │
│                              ▼                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │  COUCHE DONNÉES - Programme CREDDAO                             │   │
│  │  ────────────────────────────────────────────────────────────── │   │
│  │  • READ/REWRITE fichier EMPLOYE                                 │   │
│  │  • READ/REWRITE fichier CRE-EMP                                 │   │
│  │  • Gestion des erreurs VSAM                                     │   │
│  │  • Transformation des formats                                    │   │
│  └───────────────────────────┬─────────────────────────────────────┘   │
│                              │                                          │
│                              ▼                                          │
│  ┌────────────────────┐  ┌────────────────────┐                        │
│  │  VSAM EMPLOYE      │  │  VSAM CRE-EMP      │                        │
│  │  (KSDS)            │  │  (KSDS)            │                        │
│  └────────────────────┘  └────────────────────┘                        │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Flux de traitement

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    DÉROULEMENT DE L'APPLICATION                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ÉTAPE 1 : Consultation employé                                         │
│  ───────────────────────────────                                        │
│                                                                          │
│  Utilisateur                Présentation              Traitement         │
│      │                          │                         │              │
│      │  Saisit "EMP001"         │                         │              │
│      │  + ENTER                 │                         │              │
│      │ ────────────────────────►│                         │              │
│      │                          │  Valide format          │              │
│      │                          │  LINK CREDTRT           │              │
│      │                          │ ────────────────────────►│             │
│      │                          │                         │              │
│      │                          │                    ┌────┴────┐        │
│      │                          │                    │ CREDDAO │        │
│      │                          │                    │ READ    │        │
│      │                          │                    │ EMPLOYE │        │
│      │                          │                    └────┬────┘        │
│      │                          │                         │              │
│      │                          │  Si ETAT-CRED = 'Y'     │              │
│      │                          │                    ┌────┴────┐        │
│      │                          │                    │ CREDDAO │        │
│      │                          │                    │ READ    │        │
│      │                          │                    │ CRE-EMP │        │
│      │                          │                    └────┬────┘        │
│      │                          │                         │              │
│      │                          │ ◄────────────────────────│             │
│      │                          │  Données employé+crédit  │              │
│      │                          │                         │              │
│      │  Affichage écran         │                         │              │
│      │ ◄────────────────────────│                         │              │
│      │                          │                         │              │
│                                                                          │
│  ÉTAPE 2 : Paiement échéance (PF5)                                      │
│  ─────────────────────────────────                                      │
│                                                                          │
│      │  Appuie PF5              │                         │              │
│      │ ────────────────────────►│                         │              │
│      │                          │  LINK CREDTRT           │              │
│      │                          │  Action = 'P'           │              │
│      │                          │ ────────────────────────►│             │
│      │                          │                         │              │
│      │                          │  Calcul :               │              │
│      │                          │  RESTE = RESTE - ECH    │              │
│      │                          │                         │              │
│      │                          │  Si RESTE = 0 :         │              │
│      │                          │  ETAT-CRED = 'N'        │              │
│      │                          │                         │              │
│      │                          │                    ┌────┴────┐        │
│      │                          │                    │ CREDDAO │        │
│      │                          │                    │ REWRITE │        │
│      │                          │                    │ CRE-EMP │        │
│      │                          │                    │ EMPLOYE │        │
│      │                          │                    └────┬────┘        │
│      │                          │                         │              │
│      │  Affichage nouveau reste │                         │              │
│      │ ◄────────────────────────│                         │              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## VIII-3 Implémentation détaillée

### Copybooks

Les structures de données sont définies dans des copybooks réutilisables :

**EMPLOYE.cpy :**
```cobol
      ******************************************************************
      * Copybook : EMPLOYE - Structure enregistrement employé
      ******************************************************************
       01  EMPLOYE-REC.
           05  EMP-ID              PIC X(6).
           05  EMP-NAME            PIC X(30).
           05  EMP-DEPT            PIC X(10).
           05  EMP-SALAIRE         PIC 9(7)V99 COMP-3.
           05  EMP-ETAT-CRED       PIC X(1).
               88  EMP-A-CREDIT    VALUE 'Y'.
               88  EMP-SANS-CREDIT VALUE 'N'.
```

**CREDEMP.cpy :**
```cobol
      ******************************************************************
      * Copybook : CREDEMP - Structure enregistrement crédit
      ******************************************************************
       01  CREDIT-REC.
           05  CRD-ID-EMPL         PIC X(6).
           05  CRD-LIBELLE         PIC X(20).
           05  CRD-MONTANT-TOTAL   PIC 9(7)V99 COMP-3.
           05  CRD-MONTANT-ECH     PIC 9(5)V99 COMP-3.
           05  CRD-RESTE           PIC 9(7)V99 COMP-3.
```

### Programme Présentation (CREDPRES)

```cobol
      ******************************************************************
      * Programme : CREDPRES - Couche Présentation
      * Transaction : CRED
      * Fonction : Interface utilisateur gestion crédits
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDPRES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY DFHAID.
           COPY DFHBMSCA.
           COPY CREDSET.

       01  WS-RESP                 PIC S9(8) COMP.

      *─── COMMAREA pour échange avec couche traitement ───────────────
       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-PAYER        VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
               88  CA-OK           VALUE 00.
               88  CA-NOTFND       VALUE 13.
               88  CA-PAS-CREDIT   VALUE 20.
               88  CA-ERREUR       VALUE 99.
           05  CA-MESSAGE          PIC X(60).
           05  CA-ID-EMPL          PIC X(6).
           05  CA-EMPLOYE-DATA.
               10  CA-NAME         PIC X(30).
               10  CA-DEPT         PIC X(10).
               10  CA-SALAIRE      PIC 9(7)V99.
               10  CA-ETAT-CRED    PIC X(1).
           05  CA-CREDIT-DATA.
               10  CA-LIBELLE      PIC X(20).
               10  CA-MONTANT-TOT  PIC 9(7)V99.
               10  CA-MONTANT-ECH  PIC 9(5)V99.
               10  CA-RESTE        PIC 9(7)V99.

       01  WS-ETAT                 PIC X(1) VALUE 'I'.
           88  PREMIER-PASSAGE     VALUE 'I'.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           IF EIBCALEN = 0
               SET PREMIER-PASSAGE TO TRUE
               PERFORM 1000-AFFICHER-ECRAN-VIDE
           ELSE
               MOVE DFHCOMMAREA TO WS-COMMAREA
               PERFORM 2000-TRAITER-SAISIE
           END-IF

           EXEC CICS
               RETURN TRANSID('CRED')
                      COMMAREA(WS-COMMAREA)
                      LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.

       1000-AFFICHER-ECRAN-VIDE.

           INITIALIZE CREDMAPO
           MOVE 'Entrez un ID employé (EMP001-EMP006)'
               TO MSGO

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    ERASE
           END-EXEC.

       2000-TRAITER-SAISIE.

           EXEC CICS
               RECEIVE MAP('CREDMAP')
                       MAPSET('CREDSET')
                       INTO(CREDMAPI)
                       RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               PERFORM 1000-AFFICHER-ECRAN-VIDE
               GO TO 2000-EXIT
           END-IF

           EVALUATE EIBAID
               WHEN DFHENTER
                   PERFORM 2100-RECHERCHER-EMPLOYE
               WHEN DFHPF5
                   PERFORM 2200-PAYER-ECHEANCE
               WHEN DFHPF3
                   PERFORM 9000-QUITTER
               WHEN OTHER
                   MOVE 'Touche non autorisée' TO MSGO
                   PERFORM 3000-AFFICHER-MESSAGE
           END-EVALUATE.

       2000-EXIT.
           EXIT.

       2100-RECHERCHER-EMPLOYE.

      *─── Validation format ──────────────────────────────────────────
           IF IDEMPLL = 0 OR IDEMPLI = SPACES
               MOVE 'ID employé obligatoire' TO MSGO
               MOVE DFHBMDAR TO IDEMPLA
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2100-EXIT
           END-IF

      *─── Appel couche traitement ────────────────────────────────────
           INITIALIZE WS-COMMAREA
           SET CA-CONSULTER TO TRUE
           MOVE IDEMPLI TO CA-ID-EMPL

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

      *─── Affichage résultat ─────────────────────────────────────────
           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2100-EXIT.
           EXIT.

       2110-AFFICHER-RESULTAT.

           MOVE CA-ID-EMPL     TO IDEMPLO
           MOVE CA-NAME        TO NOMO
           MOVE CA-DEPT        TO DEPTO
           MOVE CA-SALAIRE     TO SALO

           IF CA-ETAT-CRED = 'Y'
               MOVE CA-LIBELLE     TO LIBCREDO
               MOVE CA-MONTANT-TOT TO MTTOTALO
               MOVE CA-MONTANT-ECH TO MTECHO
               MOVE CA-RESTE       TO RESTEO
               MOVE 'Crédit actif - PF5 pour payer une échéance'
                   TO MSGO
           ELSE
               MOVE SPACES TO LIBCREDO
               MOVE 0 TO MTTOTALO MTECHO RESTEO
               MOVE 'Cet employé n''a pas de crédit en cours'
                   TO MSGO
           END-IF

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    ERASE
           END-EXEC.

       2200-PAYER-ECHEANCE.

           IF CA-ETAT-CRED NOT = 'Y'
               MOVE 'Aucun crédit à payer pour cet employé'
                   TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
               GO TO 2200-EXIT
           END-IF

           SET CA-PAYER TO TRUE

           EXEC CICS
               LINK PROGRAM('CREDTRT')
                    COMMAREA(WS-COMMAREA)
           END-EXEC

           IF CA-OK
               PERFORM 2110-AFFICHER-RESULTAT
           ELSE
               MOVE CA-MESSAGE TO MSGO
               PERFORM 3000-AFFICHER-MESSAGE
           END-IF.

       2200-EXIT.
           EXIT.

       3000-AFFICHER-MESSAGE.

           EXEC CICS
               SEND MAP('CREDMAP')
                    MAPSET('CREDSET')
                    FROM(CREDMAPO)
                    DATAONLY
           END-EXEC.

       9000-QUITTER.

           EXEC CICS
               SEND TEXT FROM('Fin de transaction CRED. Au revoir.')
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
```

### Programme Traitement (CREDTRT)

```cobol
      ******************************************************************
      * Programme : CREDTRT - Couche Traitement
      * Fonction : Logique métier gestion des crédits
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDTRT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY EMPLOYE.
           COPY CREDEMP.

       01  WS-DAO-COMMAREA.
           05  DAO-ACTION          PIC X(4).
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

       01  WS-COMMAREA.
           05  CA-ACTION           PIC X(1).
               88  CA-CONSULTER    VALUE 'C'.
               88  CA-PAYER        VALUE 'P'.
           05  CA-CODE-RETOUR      PIC 9(2).
           05  CA-MESSAGE          PIC X(60).
           05  CA-ID-EMPL          PIC X(6).
           05  CA-EMPLOYE-DATA.
               10  CA-NAME         PIC X(30).
               10  CA-DEPT         PIC X(10).
               10  CA-SALAIRE      PIC 9(7)V99.
               10  CA-ETAT-CRED    PIC X(1).
           05  CA-CREDIT-DATA.
               10  CA-LIBELLE      PIC X(20).
               10  CA-MONTANT-TOT  PIC 9(7)V99.
               10  CA-MONTANT-ECH  PIC 9(5)V99.
               10  CA-RESTE        PIC 9(7)V99.

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(150).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 00 TO CA-CODE-RETOUR

           EVALUATE TRUE
               WHEN CA-CONSULTER
                   PERFORM 1000-CONSULTER-EMPLOYE
               WHEN CA-PAYER
                   PERFORM 2000-PAYER-ECHEANCE
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-CONSULTER-EMPLOYE : Récupération données employé + crédit
      ******************************************************************
       1000-CONSULTER-EMPLOYE.

      *─── Lecture employé ────────────────────────────────────────────
           INITIALIZE WS-DAO-COMMAREA
           MOVE 'READ' TO DAO-ACTION
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               IF DAO-RESP = 13
                   MOVE 13 TO CA-CODE-RETOUR
                   MOVE 'Employé non trouvé' TO CA-MESSAGE
               ELSE
                   MOVE 99 TO CA-CODE-RETOUR
                   MOVE 'Erreur lecture employé' TO CA-MESSAGE
               END-IF
               GO TO 1000-EXIT
           END-IF

      *─── Transfert données employé ──────────────────────────────────
           MOVE DAO-DATA TO EMPLOYE-REC
           MOVE EMP-NAME TO CA-NAME
           MOVE EMP-DEPT TO CA-DEPT
           MOVE EMP-SALAIRE TO CA-SALAIRE
           MOVE EMP-ETAT-CRED TO CA-ETAT-CRED

      *─── Si crédit actif, lire les détails ──────────────────────────
           IF EMP-A-CREDIT
               PERFORM 1100-LIRE-CREDIT
           ELSE
               INITIALIZE CA-CREDIT-DATA
               MOVE 00 TO CA-CODE-RETOUR
           END-IF.

       1000-EXIT.
           EXIT.

       1100-LIRE-CREDIT.

           MOVE 'READ' TO DAO-ACTION
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP = 0
               MOVE DAO-DATA TO CREDIT-REC
               MOVE CRD-LIBELLE TO CA-LIBELLE
               MOVE CRD-MONTANT-TOTAL TO CA-MONTANT-TOT
               MOVE CRD-MONTANT-ECH TO CA-MONTANT-ECH
               MOVE CRD-RESTE TO CA-RESTE
               MOVE 00 TO CA-CODE-RETOUR
           ELSE
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture crédit' TO CA-MESSAGE
           END-IF.

      ******************************************************************
      * 2000-PAYER-ECHEANCE : Traitement paiement d'une échéance
      ******************************************************************
       2000-PAYER-ECHEANCE.

      *─── Lecture crédit avec verrouillage ───────────────────────────
           MOVE 'UPDT' TO DAO-ACTION
           MOVE 'CREDEMP' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur lecture crédit pour MAJ' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

           MOVE DAO-DATA TO CREDIT-REC

      *─── Calcul du nouveau reste ────────────────────────────────────
           SUBTRACT CRD-MONTANT-ECH FROM CRD-RESTE

           IF CRD-RESTE < 0
               MOVE 0 TO CRD-RESTE
           END-IF

      *─── Mise à jour crédit ─────────────────────────────────────────
           MOVE 'REWT' TO DAO-ACTION
           MOVE CREDIT-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               MOVE 99 TO CA-CODE-RETOUR
               MOVE 'Erreur mise à jour crédit' TO CA-MESSAGE
               GO TO 2000-EXIT
           END-IF

      *─── Si crédit soldé, mettre à jour employé ─────────────────────
           IF CRD-RESTE = 0
               PERFORM 2100-SOLDER-CREDIT
           END-IF

      *─── Mise à jour COMMAREA retour ────────────────────────────────
           MOVE CRD-RESTE TO CA-RESTE
           MOVE 00 TO CA-CODE-RETOUR
           IF CRD-RESTE = 0
               MOVE 'N' TO CA-ETAT-CRED
               MOVE 'Crédit soldé ! Félicitations !' TO CA-MESSAGE
           ELSE
               MOVE 'Échéance payée avec succès' TO CA-MESSAGE
           END-IF.

       2000-EXIT.
           EXIT.

       2100-SOLDER-CREDIT.

      *─── Lecture employé pour MAJ ───────────────────────────────────
           MOVE 'UPDT' TO DAO-ACTION
           MOVE 'EMPLOYE' TO DAO-FICHIER
           MOVE CA-ID-EMPL TO DAO-CLE

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC

           IF DAO-RESP NOT = 0
               GO TO 2100-EXIT
           END-IF

           MOVE DAO-DATA TO EMPLOYE-REC
           SET EMP-SANS-CREDIT TO TRUE

      *─── Réécriture employé ─────────────────────────────────────────
           MOVE 'REWT' TO DAO-ACTION
           MOVE EMPLOYE-REC TO DAO-DATA

           EXEC CICS
               LINK PROGRAM('CREDDAO')
                    COMMAREA(WS-DAO-COMMAREA)
           END-EXEC.

       2100-EXIT.
           EXIT.
```

### Programme Données (CREDDAO)

```cobol
      ******************************************************************
      * Programme : CREDDAO - Couche Données (Data Access Object)
      * Fonction : Accès aux fichiers VSAM EMPLOYE et CRE-EMP
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREDDAO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-RESP                 PIC S9(8) COMP.
       01  WS-FICHIER              PIC X(8).
       01  WS-DATA-BUFFER          PIC X(100).

       01  WS-COMMAREA.
           05  DAO-ACTION          PIC X(4).
               88  DAO-READ        VALUE 'READ'.
               88  DAO-READ-UPD    VALUE 'UPDT'.
               88  DAO-REWRITE     VALUE 'REWT'.
               88  DAO-WRITE       VALUE 'WRIT'.
               88  DAO-DELETE      VALUE 'DELT'.
           05  DAO-FICHIER         PIC X(8).
           05  DAO-CLE             PIC X(6).
           05  DAO-RESP            PIC 9(4).
           05  DAO-DATA            PIC X(100).

       LINKAGE SECTION.
       01  DFHCOMMAREA             PIC X(122).

       PROCEDURE DIVISION.

       0000-PRINCIPAL.

           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE 0 TO DAO-RESP

           EVALUATE TRUE
               WHEN DAO-READ
                   PERFORM 1000-LIRE
               WHEN DAO-READ-UPD
                   PERFORM 1100-LIRE-POUR-MAJ
               WHEN DAO-REWRITE
                   PERFORM 2000-REECRIRE
               WHEN DAO-WRITE
                   PERFORM 3000-ECRIRE
               WHEN DAO-DELETE
                   PERFORM 4000-SUPPRIMER
           END-EVALUATE

           MOVE WS-COMMAREA TO DFHCOMMAREA
           EXEC CICS RETURN END-EXEC.

      ******************************************************************
      * 1000-LIRE : Lecture simple
      ******************************************************************
       1000-LIRE.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 1100-LIRE-POUR-MAJ : Lecture avec verrouillage
      ******************************************************************
       1100-LIRE-POUR-MAJ.

           EXEC CICS
               READ FILE(DAO-FICHIER)
                    INTO(WS-DATA-BUFFER)
                    RIDFLD(DAO-CLE)
                    UPDATE
                    RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 2000-REECRIRE : Mise à jour après READ UPDATE
      ******************************************************************
       2000-REECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               REWRITE FILE(DAO-FICHIER)
                       FROM(WS-DATA-BUFFER)
                       RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 3000-ECRIRE : Création nouvel enregistrement
      ******************************************************************
       3000-ECRIRE.

           MOVE DAO-DATA TO WS-DATA-BUFFER

           EXEC CICS
               WRITE FILE(DAO-FICHIER)
                     FROM(WS-DATA-BUFFER)
                     RIDFLD(DAO-CLE)
                     RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 4000-SUPPRIMER : Suppression
      ******************************************************************
       4000-SUPPRIMER.

           EXEC CICS
               DELETE FILE(DAO-FICHIER)
                      RIDFLD(DAO-CLE)
                      RESP(WS-RESP)
           END-EXEC

           PERFORM 9000-TRAITER-RESP.

      ******************************************************************
      * 9000-TRAITER-RESP : Gestion code retour
      ******************************************************************
       9000-TRAITER-RESP.

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE 0 TO DAO-RESP
                   MOVE WS-DATA-BUFFER TO DAO-DATA
               WHEN DFHRESP(NOTFND)
                   MOVE 13 TO DAO-RESP
               WHEN DFHRESP(DUPREC)
                   MOVE 14 TO DAO-RESP
               WHEN DFHRESP(NOSPACE)
                   MOVE 18 TO DAO-RESP
               WHEN DFHRESP(DISABLED)
                   MOVE 22 TO DAO-RESP
               WHEN OTHER
                   MOVE 99 TO DAO-RESP
           END-EVALUATE.
```

## VIII-4 Fichiers sources

Les fichiers sources complets de cet exercice sont disponibles dans le dossier :

```
exercices/cics/tp-gestion-credits/
├── README.md           # Énoncé et instructions
├── copybooks/
│   ├── EMPLOYE.cpy     # Structure employé
│   └── CREDEMP.cpy     # Structure crédit
├── jcl/
│   ├── DEFVSAM.jcl     # Définition fichiers VSAM
│   └── LOADDATA.jcl    # Chargement données test
├── data/
│   ├── EMPLOYE.dat     # Données test employés
│   └── CREDEMP.dat     # Données test crédits
├── bms/
│   └── CREDSET.bms     # Définition écran
└── cobol/
    ├── CREDPRES.cbl    # Couche Présentation
    ├── CREDTRT.cbl     # Couche Traitement
    └── CREDDAO.cbl     # Couche Données
```

## Résumé du chapitre

```
┌─────────────────────────────────────────────────────────────────────────┐
│                    CHAPITRE VIII - RÉSUMÉ                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  CET EXERCICE PRATIQUE ILLUSTRE :                                       │
│                                                                          │
│  1. ARCHITECTURE MULTICOUCHES                                           │
│     • Séparation claire Présentation / Traitement / Données            │
│     • Communication par COMMAREA entre couches                          │
│     • Chaque couche a une responsabilité unique                        │
│                                                                          │
│  2. STRUCTURES VSAM                                                      │
│     • Deux fichiers KSDS avec relation logique                         │
│     • Formats COMP-3 pour les montants                                 │
│     • Clé commune ID-EMPL                                              │
│                                                                          │
│  3. COMMANDES CICS                                                       │
│     • READ / READ UPDATE pour lecture                                   │
│     • REWRITE pour mise à jour                                         │
│     • LINK pour appels inter-programmes                                │
│     • SEND MAP / RECEIVE MAP pour écrans                               │
│                                                                          │
│  4. LOGIQUE MÉTIER                                                       │
│     • Vérification état crédit                                         │
│     • Calcul nouveau reste après paiement                              │
│     • Mise à jour cohérente des deux fichiers                          │
│                                                                          │
│  5. BONNES PRATIQUES                                                     │
│     • Copybooks pour réutilisation                                     │
│     • Gestion des codes retour                                         │
│     • Messages utilisateur explicites                                  │
│     • Mode pseudo-conversationnel                                       │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VII - Couche des Données](07-couche-donnees.md) | [Chapitre IX - Architecture et Transactions TSI](09-architecture-transactions-tsi.md) |

---
*Formation COBOL - Module CICS*
