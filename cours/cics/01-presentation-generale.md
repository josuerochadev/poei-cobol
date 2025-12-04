# Chapitre I - Présentation générale de CICS

## 1. Introduction

### 1.1 Qu'est-ce que CICS ?

**CICS** (Customer Information Control System) est un **moniteur transactionnel** (ou TP Monitor - Transaction Processing Monitor) développé par IBM depuis 1968. C'est l'un des logiciels les plus utilisés au monde pour le traitement des transactions en temps réel sur les systèmes mainframe.

### 1.2 Définition d'une transaction

Une **transaction** est une unité de travail logique comprenant :
- Une ou plusieurs opérations
- Un début et une fin clairement définis
- Les propriétés ACID (Atomicité, Cohérence, Isolation, Durabilité)

```
┌─────────────────────────────────────────────────────────────┐
│                    TRANSACTION CICS                          │
├─────────────────────────────────────────────────────────────┤
│  Début ──► Lecture données ──► Traitement ──► Écriture ──► Fin │
│                                                              │
│  • Atomique : tout ou rien                                   │
│  • Cohérente : état valide avant/après                       │
│  • Isolée : invisible aux autres transactions                │
│  • Durable : modifications permanentes                       │
└─────────────────────────────────────────────────────────────┘
```

## 2. Historique

| Année | Événement |
|-------|-----------|
| 1968 | Première version de CICS |
| 1970s | Adoption massive dans le secteur bancaire |
| 1980s | Support de DB2 et nouvelles fonctionnalités |
| 1990s | CICS Transaction Server (CTS) |
| 2000s | Support Java, Web Services |
| 2010s | CICS TS V5.x, Cloud, API modernes |
| 2020s | CICS TS V6.x, conteneurs, DevOps |

## 3. Positionnement de CICS

### 3.1 Architecture globale

```
┌─────────────────────────────────────────────────────────────────┐
│                         UTILISATEURS                             │
│         (Terminaux 3270, Web, Mobile, API)                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                            CICS                                  │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ Gestion des │  │ Gestion des │  │ Gestion des │              │
│  │ Transactions│  │   Écrans    │  │  Programmes │              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ Gestion des │  │  Sécurité   │  │ Récupération│              │
│  │  Ressources │  │   (RACF)    │  │   (Recovery)│              │
│  └─────────────┘  └─────────────┘  └─────────────┘              │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      RESSOURCES DONNÉES                          │
│    ┌────────┐    ┌────────┐    ┌────────┐    ┌────────┐        │
│    │  VSAM  │    │  DB2   │    │  IMS   │    │  MQ    │        │
│    └────────┘    └────────┘    └────────┘    └────────┘        │
└─────────────────────────────────────────────────────────────────┘
```

### 3.2 CICS vs Batch

| Caractéristique | Traitement Batch | CICS (Temps réel) |
|-----------------|------------------|-------------------|
| **Mode** | Différé | Immédiat |
| **Interaction** | Aucune | Conversationnel |
| **Temps de réponse** | Minutes/Heures | Millisecondes |
| **Volume par transaction** | Élevé | Faible |
| **Nombre de transactions** | Peu | Très nombreuses |
| **Exemple** | Édition relevés bancaires | Consultation solde |

## 4. Domaines d'utilisation

CICS est omniprésent dans les secteurs critiques :

### 4.1 Secteur bancaire
- Consultation de comptes
- Virements en temps réel
- Opérations au guichet
- Distributeurs automatiques (ATM)

### 4.2 Assurances
- Gestion des contrats
- Déclaration de sinistres
- Calcul de cotisations

### 4.3 Transport et réservation
- Réservation de billets
- Gestion des vols/trains
- Check-in

### 4.4 Distribution
- Gestion des stocks
- Points de vente
- Commandes en ligne

## 5. Caractéristiques principales

### 5.1 Haute disponibilité
- Fonctionne 24h/24, 7j/7
- Reprise automatique après incident
- Pas d'interruption de service

### 5.2 Performance
- Milliers de transactions par seconde
- Temps de réponse en millisecondes
- Gestion optimisée de la mémoire

### 5.3 Scalabilité
- Support de milliers d'utilisateurs simultanés
- Répartition de charge (Sysplex)
- Croissance sans refonte

### 5.4 Sécurité
- Intégration avec RACF/ACF2/Top Secret
- Contrôle d'accès aux transactions
- Audit des opérations

## 6. CICS et COBOL

### 6.1 Programme COBOL-CICS

Un programme CICS est un programme COBOL standard enrichi de commandes CICS :

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXEMPLE1.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE        PIC X(50).

       PROCEDURE DIVISION.
           MOVE 'BIENVENUE DANS CICS' TO WS-MESSAGE

           EXEC CICS
               SEND TEXT FROM(WS-MESSAGE)
                    LENGTH(50)
                    ERASE
           END-EXEC

           EXEC CICS
               RETURN
           END-EXEC.
```

### 6.2 Structure d'une commande CICS

```cobol
       EXEC CICS
           commande [option1(valeur1)]
                    [option2(valeur2)]
                    ...
       END-EXEC
```

**Points clés :**
- Commence par `EXEC CICS`
- Se termine par `END-EXEC`
- La commande et ses options sont en majuscules
- Les valeurs peuvent être des littéraux ou des variables COBOL

## 7. Vocabulaire essentiel

| Terme | Définition |
|-------|------------|
| **Transaction** | Unité de travail identifiée par un code de 4 caractères (ex: MENU) |
| **Programme** | Programme COBOL exécuté par une transaction |
| **Région CICS** | Instance CICS en cours d'exécution |
| **Terminal** | Point d'entrée utilisateur (écran 3270) |
| **Task** | Instance d'exécution d'une transaction |
| **EIB** | Execute Interface Block - zone de communication |
| **COMMAREA** | Zone de passage de données entre programmes |
| **BMS** | Basic Mapping Support - gestion des écrans |

## 8. Résumé

```
┌─────────────────────────────────────────────────────────────────┐
│                    CICS EN RÉSUMÉ                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  • Moniteur transactionnel IBM depuis 1968                       │
│  • Traitement en temps réel (vs Batch)                          │
│  • Propriétés ACID des transactions                              │
│  • Intégré avec COBOL via EXEC CICS ... END-EXEC                │
│  • Accès aux données : VSAM, DB2, IMS                           │
│  • Secteurs : Banque, Assurance, Transport                       │
│  • Haute disponibilité et performance                            │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

**Navigation**
- [Suivant : Organisation du système →](02-organisation-systeme.md)
- [Retour au sommaire](README.md)
