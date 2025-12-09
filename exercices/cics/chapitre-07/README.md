# Exercices CICS - Chapitre VII

## Thème : Couche de Données - Browse VSAM et Transactions

Ce TP met en pratique le parcours séquentiel de fichiers VSAM et la gestion des transactions avec SYNCPOINT/ROLLBACK.

## Objectifs

- Maîtriser les commandes STARTBR, READNEXT, READPREV, ENDBR
- Implémenter un affichage paginé (page up/page down)
- Comprendre SYNCPOINT et ROLLBACK
- Gérer les points de sauvegarde (SYNCPOINT)

## Fichiers

```
chapitre-07/
├── bms/
│   └── BROWSSET.bms      # Mapset pour affichage liste
├── cobol/
│   ├── PROGBRWS.cbl      # Exercice 1 : Browse simple
│   ├── PROGPAGE.cbl      # Exercice 2 : Affichage paginé
│   └── PROGVIR.cbl       # Exercice 3 : Virement avec SYNCPOINT
├── copybooks/
│   └── MAPBROWS.cpy      # Zone symbolique
├── jcl/
│   └── DEFVSAM.jcl       # Définition fichiers VSAM
└── README.md
```

## Structure de données

### Fichier EMPLOYE (VSAM KSDS) - réutilisé

| Champ | PIC | Longueur | Description |
|-------|-----|----------|-------------|
| EMP-ID | X(6) | 6 | Clé primaire |
| EMP-NAME | X(30) | 30 | Nom de l'employé |
| EMP-DEPT | X(10) | 10 | Département |
| EMP-SALAIRE | 9(7)V99 | 9 | Salaire |
| EMP-ETAT-CRED | X(1) | 1 | Y=Crédit, N=Sans crédit |
| FILLER | X(24) | 24 | Réservé |

### Fichier COMPTE (VSAM KSDS) - pour exercice 3

| Champ | PIC | Longueur | Description |
|-------|-----|----------|-------------|
| CPT-NUM | X(10) | 10 | Numéro de compte |
| CPT-NOM | X(30) | 30 | Titulaire |
| CPT-SOLDE | S9(9)V99 | 11 | Solde |
| FILLER | X(29) | 29 | Réservé |

**Longueur totale** : 80 octets

---

## Exercice 1 : Browse simple (PROGBRWS)

### Objectif

Parcourir un fichier VSAM du début à la fin et afficher tous les enregistrements.

### Transaction : `BRWS`

### Schéma du parcours

```
┌─────────────────────────────────────────────────────────────────────────┐
│  PARCOURS SÉQUENTIEL VSAM                                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  1. STARTBR FILE('XXX') RIDFLD(key)    ◄── Ouvre le curseur             │
│                                                                          │
│  2. READNEXT/READPREV FILE('XXX')       ◄── Lit l'enregistrement        │
│     INTO(ws-area) RIDFLD(key)               suivant/précédent           │
│                                                                          │
│  3. Répéter jusqu'à ENDFILE                                             │
│                                                                          │
│  4. ENDBR FILE('XXX')                   ◄── Ferme le curseur            │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Syntaxe

```cobol
      *    Initialisation du browse
           MOVE LOW-VALUES TO WS-START-KEY.

           EXEC CICS STARTBR
               FILE('EMPLOYE')
               RIDFLD(WS-START-KEY)
               RESP(WS-RESP)
           END-EXEC.

      *    Boucle de lecture
           PERFORM UNTIL WS-RESP = DFHRESP(ENDFILE)
               EXEC CICS READNEXT
                   FILE('EMPLOYE')
                   INTO(WS-EMPLOYE)
                   RIDFLD(WS-START-KEY)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(NORMAL)
                   PERFORM TRAITER-ENREGISTREMENT
               END-IF
           END-PERFORM.

      *    Fermeture du browse
           EXEC CICS ENDBR
               FILE('EMPLOYE')
           END-EXEC.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | Lecture réussie |
| 20 | DFHRESP(ENDFILE) | Fin de fichier atteinte |
| 13 | DFHRESP(NOTFND) | Clé de départ non trouvée |

### Programme fourni

Le programme `PROGBRWS.cbl` :
1. Ouvre un browse avec STARTBR (clé LOW-VALUES)
2. Lit tous les enregistrements avec READNEXT
3. Affiche chaque enregistrement sur le terminal
4. Ferme avec ENDBR

### Test

1. Lancer la transaction : `BRWS`
2. Observer la liste des employés affichés
3. Message final : `FIN DU PARCOURS - XX ENREGISTREMENTS`

---

## Exercice 2 : Affichage paginé (PROGPAGE)

### Objectif

Implémenter un affichage paginé avec navigation PF7 (page précédente) et PF8 (page suivante).

### Transaction : `PAGE`

### Spécifications écran

```
┌─────────────────────────────────────────────────────────────────────────┐
│  Ligne 1  : LISTE DES EMPLOYES                          Page: 01        │
│  Ligne 2  : ═══════════════════════════════════════════════════════════ │
│                                                                          │
│  Ligne 4  : ID     NOM                           DEPT       SALAIRE     │
│  Ligne 5  : ────── ────────────────────────────  ────────── ──────────  │
│  Ligne 6  : EMP001 DUPONT JEAN                   COMPTABIL  045000.00   │
│  Ligne 7  : EMP002 MARTIN MARIE                  INFORMATI  052000.00   │
│  ...                                                                     │
│  Ligne 15 : EMP010 ROBERT PIERRE                 VENTES     038000.00   │
│                                                                          │
│  Ligne 24 : PF7=Page préc   PF8=Page suiv   PF3=Quitter                 │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Logique de pagination

```
┌─────────────────────────────────────────────────────────────────────────┐
│  GESTION DE LA PAGINATION                                               │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  COMMAREA contient :                                                    │
│  • Première clé de la page courante (TOP-KEY)                          │
│  • Dernière clé de la page courante (BOT-KEY)                          │
│  • Numéro de page                                                       │
│                                                                          │
│  PF8 (Page suivante) :                                                  │
│  1. STARTBR à BOT-KEY                                                   │
│  2. READNEXT pour passer l'enregistrement courant                      │
│  3. Lire les 10 suivants                                               │
│                                                                          │
│  PF7 (Page précédente) :                                                │
│  1. STARTBR à TOP-KEY                                                   │
│  2. READPREV 10 fois                                                    │
│  3. Inverser l'ordre pour affichage                                     │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Structure COMMAREA

```cobol
       01  WS-COMMAREA.
           05  CA-TOP-KEY             PIC X(6).
           05  CA-BOT-KEY             PIC X(6).
           05  CA-PAGE-NUM            PIC 9(3).
           05  CA-TOTAL-REC           PIC 9(5).
```

### Syntaxe READPREV

```cobol
      *    Lecture en arrière
           EXEC CICS READPREV
               FILE('EMPLOYE')
               INTO(WS-EMPLOYE)
               RIDFLD(WS-KEY)
               RESP(WS-RESP)
           END-EXEC.
```

### Programme fourni

Le programme `PROGPAGE.cbl` :
1. Premier affichage : 10 premiers enregistrements
2. PF8 : Affiche les 10 suivants
3. PF7 : Affiche les 10 précédents
4. PF3 : Quitte le programme

### Test

1. Lancer la transaction : `PAGE`
2. Appuyer sur PF8 pour page suivante
3. Appuyer sur PF7 pour page précédente
4. Appuyer sur PF3 pour quitter

---

## Exercice 3 : Virement avec SYNCPOINT (PROGVIR)

### Objectif

Implémenter un virement entre deux comptes avec gestion transactionnelle.

### Transaction : `VIR`

### Principe du SYNCPOINT

```
┌─────────────────────────────────────────────────────────────────────────┐
│  GESTION TRANSACTIONNELLE CICS                                          │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │  EXEC CICS SYNCPOINT                                              │ │
│  │  ─────────────────────                                            │ │
│  │  • Valide toutes les modifications depuis le dernier SYNCPOINT   │ │
│  │  • Crée un point de reprise                                      │ │
│  │  • Les données sont définitivement écrites                       │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │  EXEC CICS SYNCPOINT ROLLBACK                                     │ │
│  │  ───────────────────────────────                                  │ │
│  │  • Annule toutes les modifications depuis le dernier SYNCPOINT   │ │
│  │  • Restaure l'état précédent des fichiers                        │ │
│  │  • À utiliser en cas d'erreur                                    │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Scénario de virement

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        VIREMENT BANCAIRE                                 │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ÉTAPE 1 : Débiter le compte source                                     │
│  ─────────────────────────────────────                                  │
│  • READ UPDATE compte source                                            │
│  • Vérifier solde suffisant                                             │
│  • REWRITE avec nouveau solde                                           │
│                                                                          │
│  ÉTAPE 2 : Créditer le compte destination                               │
│  ───────────────────────────────────────────                            │
│  • READ UPDATE compte destination                                       │
│  • REWRITE avec nouveau solde                                           │
│                                                                          │
│  SI SUCCÈS : SYNCPOINT (valider)                                        │
│  SI ERREUR : SYNCPOINT ROLLBACK (annuler)                               │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

### Syntaxe

```cobol
      *    Début de la transaction
           PERFORM DEBITER-COMPTE-SOURCE.

           IF WS-DEBIT-OK
               PERFORM CREDITER-COMPTE-DEST

               IF WS-CREDIT-OK
      *            Validation de la transaction
                   EXEC CICS SYNCPOINT
                       RESP(WS-RESP)
                   END-EXEC
                   MOVE 'VIREMENT EFFECTUE' TO WS-MESSAGE
               ELSE
      *            Annulation en cas d'erreur
                   EXEC CICS SYNCPOINT ROLLBACK
                       RESP(WS-RESP)
                   END-EXEC
                   MOVE 'ERREUR CREDIT - ANNULATION' TO WS-MESSAGE
               END-IF
           ELSE
               MOVE 'SOLDE INSUFFISANT' TO WS-MESSAGE
           END-IF.
```

### Codes retour

| Code | Constante | Signification |
|------|-----------|---------------|
| 0 | DFHRESP(NORMAL) | SYNCPOINT réussi |
| 82 | DFHRESP(ROLLEDBACK) | Rollback automatique |

### Programme fourni

Le programme `PROGVIR.cbl` :
1. Demande compte source, compte destination, montant
2. Vérifie que le compte source a un solde suffisant
3. Débite le compte source
4. Crédite le compte destination
5. Si succès : SYNCPOINT
6. Si erreur : SYNCPOINT ROLLBACK

### Test

1. Lancer la transaction : `VIR`
2. Saisir compte source : `CPT0000001`
3. Saisir compte destination : `CPT0000002`
4. Saisir montant : `1000.00`
5. Message attendu : `VIREMENT EFFECTUE - NOUVEAU SOLDE: 4000.00`

### Test d'erreur

1. Lancer avec un montant supérieur au solde
2. Message attendu : `SOLDE INSUFFISANT - OPERATION ANNULEE`

---

## Tableau récapitulatif des commandes Browse

| Commande | Usage | Conditions |
|----------|-------|------------|
| **STARTBR** | Ouvrir un curseur de parcours | NOTFND si clé inexacte (sans GTEQ) |
| **READNEXT** | Lire l'enregistrement suivant | ENDFILE en fin de fichier |
| **READPREV** | Lire l'enregistrement précédent | ENDFILE en début de fichier |
| **RESETBR** | Repositionner le curseur | - |
| **ENDBR** | Fermer le curseur | Obligatoire après STARTBR |

## Options importantes

| Option | Commande | Description |
|--------|----------|-------------|
| **GTEQ** | STARTBR | Positionne sur clé >= à la clé fournie |
| **GENERIC** | STARTBR | Recherche générique sur début de clé |
| **KEYLENGTH** | STARTBR | Longueur de clé pour recherche générique |
| **UPDATE** | READNEXT | Verrouille pour modification |

---

## Exercice 4 (Avancé) : Browse avec filtre

### Objectif

Parcourir les employés d'un département spécifique.

### Spécifications

- Transaction : `DEPT`
- Saisir un code département
- Afficher tous les employés de ce département
- Utiliser un browse complet avec filtrage côté programme

### Pseudo-code

```cobol
       PERFORM UNTIL WS-END-FILE
           EXEC CICS READNEXT ... END-EXEC
           IF EMP-DEPT = WS-DEPT-RECHERCHE
               PERFORM AFFICHER-EMPLOYE
           END-IF
       END-PERFORM.
```

---

## Exercice 5 (Avancé) : Index alternatif

### Objectif

Utiliser un AIX (Alternate Index) pour parcourir par département.

### Concept

```
┌─────────────────────────────────────────────────────────────────────────┐
│  FICHIER VSAM AVEC INDEX ALTERNATIF                                     │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  Fichier principal (EMPLOYE)        Index alternatif (EMPDEPT)          │
│  ─────────────────────────────      ──────────────────────────          │
│  Clé primaire : EMP-ID              Clé AIX : EMP-DEPT                  │
│                                                                          │
│  ┌──────────────────────┐           ┌──────────────────────┐            │
│  │ EMP001 │ COMPTABIL   │ ◄──────── │ COMPTABIL │ EMP001   │            │
│  │ EMP002 │ INFORMATI   │           │ COMPTABIL │ EMP005   │            │
│  │ EMP003 │ VENTES      │           │ INFORMATI │ EMP002   │            │
│  │ EMP004 │ VENTES      │           │ VENTES    │ EMP003   │            │
│  │ EMP005 │ COMPTABIL   │           │ VENTES    │ EMP004   │            │
│  └──────────────────────┘           └──────────────────────┘            │
│                                                                          │
│  STARTBR FILE('EMPDEPT') RIDFLD('COMPTABIL') GENERIC                   │
│  → Retourne EMP001, EMP005                                              │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Prérequis

1. **Fichiers VSAM définis et chargés**
   - EMPLOYE (réutilisé du chapitre 06)
   - COMPTE (nouveau pour exercice 3)

2. **Définitions CICS** :
   ```
   CEDA DEF FILE(EMPLOYE) GROUP(TESTGRP) ...
   CEDA DEF FILE(COMPTE) GROUP(TESTGRP) ...
   CEDA DEF TRANSACTION(BRWS) GROUP(TESTGRP) PROGRAM(PROGBRWS)
   CEDA DEF TRANSACTION(PAGE) GROUP(TESTGRP) PROGRAM(PROGPAGE)
   CEDA DEF TRANSACTION(VIR) GROUP(TESTGRP) PROGRAM(PROGVIR)
   ```

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre VI - Couche Traitement](../chapitre-06/) | [TP Gestion des Crédits](../tp-gestion-credits/) |

---
*Formation COBOL - Module CICS - TP Chapitre VII*
