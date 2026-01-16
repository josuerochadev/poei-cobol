# Conclusion et Annexes

[< Partie 3 : Avancées](06-partie-3-avancees.md) | [Retour à l'introduction](00-introduction.md)

---

## Bilan du projet

### Difficultés rencontrées et solutions

| Problème | Cause | Solution |
|----------|-------|----------|
| Erreur VSAM 108 au chargement | Longueur incorrecte des enregistrements | Le DD * du JCL lit en LRECL=80 par défaut. Définir RECORDSIZE(80 80) et utiliser un FILLER de 16 octets |
| Volume non spécifié (TK4-) | Paramètre manquant | Ajouter VOLUMES(FDDBAS) dans la définition du cluster VSAM |
| Fichier VSAM vide après REPRO | LRECL incompatible | Passer à 80 octets pour tous les enregistrements |
| Données effacées après mise à jour | Bug fusion des modifications | Ajouter clause ELSE pour préserver les champs non modifiés dans le paragraphe 4050-FUSIONNER-MODIFICATIONS |
| Justification à droite des clés | Attribut NUM sur champ préfixe | Utiliser PIC X sans NUM pour les champs de clé partielle |

### Compétences mises en œuvre

**VSAM et CICS :**

- Définition de fichiers VSAM KSDS (IDCAMS)
- Intégration fichiers dans CICS (FCT - File Control Table)
- Commandes CICS : READ, WRITE, REWRITE, DELETE
- Navigation VSAM : STARTBR, READNEXT, ENDBR

**BMS et Écrans :**

- Conception d'écrans BMS (Basic Mapping Support)
- Gestion des attributs (ASKIP, UNPROT, BRT)
- Commandes SEND MAP, RECEIVE MAP

**Programmation COBOL-CICS :**

- Mode pseudo-conversationnel (RETURN TRANSID, COMMAREA)
- LINKAGE SECTION pour DFHCOMMAREA
- Validation et contrôle des données saisies
- Gestion des erreurs (RESP, DFHRESP)

**Administration :**

- Définition de transactions (CEDA DEFINE)
- Installation de ressources (CEDA INSTALL)
- Débogage avec CEDF

---

## Référence des Commandes CICS

### Opérations sur enregistrements

| Commande | Usage | Prérequis |
|----------|-------|-----------|
| **READ** | Lecture directe par clé | Aucun |
| **READ UPDATE** | Lecture avec verrouillage | Pour REWRITE |
| **WRITE** | Ajout nouvel enregistrement | Le client ne doit PAS exister |
| **REWRITE** | Mise à jour enregistrement | READ UPDATE obligatoire dans même UOW |
| **DELETE** | Suppression enregistrement | Aucun |

### Navigation VSAM (Browse)

| Commande | Usage |
|----------|-------|
| **STARTBR** | Positionner le curseur sur une clé (partielle) avec GTEQ |
| **READNEXT** | Lire l'enregistrement suivant |
| **ENDBR** | Terminer le parcours et libérer les ressources |

### Écrans BMS

| Commande | Usage |
|----------|-------|
| **SEND MAP** | Afficher un écran à l'utilisateur |
| **RECEIVE MAP** | Recevoir les données saisies |
| **RETURN TRANSID** | Retour pseudo-conversationnel |
| **RETURN** | Fin de transaction |

---

## Annexes

### Liste des programmes COBOL-CICS

| Programme | Transaction | Commandes | Description |
|-----------|-------------|-----------|-------------|
| PRGCLIA | AFFI | READ | Affichage d'un client |
| PRGAJT | AJOU | WRITE | Ajout d'un nouveau client |
| PRGMAJ | MAJO | READ UPDATE, REWRITE | Mise à jour d'un client |
| PRGSUP | SUPP | READ, DELETE | Suppression d'un client (avec affichage) |
| PRGDELG | DELG | STARTBR, READNEXT, DELETE | Suppression générique par préfixe |

### Liste des MAPs BMS

| Mapset | Map | Programme | Description |
|--------|-----|-----------|-------------|
| CLIAFF | MAPAFF | PRGCLIA | Écran d'affichage client |
| CLIAJT | MAPAJT | PRGAJT | Écran d'ajout client |
| CLIMAJ | MAPMAJ | PRGMAJ | Écran de mise à jour |
| CLISUP | MAPSUP | PRGSUP | Écran de suppression |
| CLIDEL | MAPDEL | PRGDELG | Écran de suppression générique |

### Liste des transactions CICS

| Code | Programme | Description |
|------|-----------|-------------|
| AFFI | PRGCLIA | Affichage client |
| AJOU | PRGAJT | Ajout client |
| MAJO | PRGMAJ | Mise à jour client |
| SUPP | PRGSUP | Suppression client |
| DELG | PRGDELG | Suppression générique |

### Structure du fichier CLIENT (80 octets)

| Position | Champ | Type | Longueur | Description |
|----------|-------|------|----------|-------------|
| 01-06 | NUMCPT | NUM | 6 | Numéro compte (clé) |
| 07-08 | CODREG | NUM | 2 | Code région (01-04) |
| 09-10 | NATCPT | NUM | 2 | Nature compte |
| 11-20 | NOM | ALPHA | 10 | Nom client |
| 21-30 | PRENOM | ALPHA | 10 | Prénom client |
| 31-38 | DATNAISS | NUM | 8 | Date naissance (AAAAMMJJ) |
| 39 | SEXE | ALPHA | 1 | Sexe (M/F) |
| 40-41 | ACTPRO | NUM | 2 | Activité professionnelle |
| 42 | SITSO | ALPHA | 1 | Situation sociale (C/M/D/V) |
| 43-52 | ADRESSE | ALPHA | 10 | Adresse |
| 53-62 | SOLDE | NUM | 10 | Solde |
| 63-64 | POSITION | ALPHA | 2 | Position (DB/CR) |
| 65-80 | FILLER | - | 16 | Réserve |

### Messages d'erreur standards

| Message | Contexte |
|---------|----------|
| ENREGISTREMENT EN DOUBLE | Ajout d'un client existant |
| ZONE NUMERIQUE, RESAISIR CE CHAMP | Champ numérique invalide |
| REGION INEXISTANTE, SAISIR CODE REGION | Code région invalide |
| CLIENT INEXISTANT | Recherche sans résultat |
| SUPPRESSION EFFECTUEE | Confirmation suppression |
| MISE A JOUR EFFECTUEE | Confirmation mise à jour |

---

## Conclusion

Ce projet m'a permis de mettre en pratique l'ensemble des compétences acquises durant la formation POEI Mainframe COBOL pour le volet CICS. À travers les différentes parties du projet, j'ai pu :

- **Maîtriser VSAM sous CICS** : Définition de fichiers KSDS, intégration dans la FCT (File Control Table), et gestion des opérations de lecture, écriture, mise à jour et suppression.

- **Développer des écrans BMS** : Conception de MAPs avec gestion des attributs (couleurs, protection), zones de saisie et d'affichage, messages d'erreur.

- **Programmer en COBOL-CICS** : Utilisation des commandes CICS (SEND/RECEIVE MAP, READ, WRITE, REWRITE, DELETE), gestion pseudo-conversationnelle avec RETURN TRANSID, et navigation VSAM avec STARTBR/READNEXT/ENDBR.

- **Administrer les transactions** : Définition via CEDA, installation de groupes, tests avec CEDF.

Le projet couvre un cas concret de gestion clientèle dans le secteur financier, avec **5 programmes COBOL-CICS**, **5 MAPs BMS** et **5 transactions**. Les principales difficultés rencontrées (gestion des attributs BMS, validation des données, navigation VSAM, fusion des modifications) m'ont permis de développer une approche méthodique de résolution de problèmes.

Cette expérience constitue une base solide pour aborder des projets mainframe transactionnels en entreprise.

---

*Rapport réalisé par Josué ROCHA - Formation POEI Mainframe COBOL - M2i Formation, Strasbourg - Janvier 2026*

---

[< Partie 3 : Avancées](06-partie-3-avancees.md) | [Retour à l'introduction](00-introduction.md)
