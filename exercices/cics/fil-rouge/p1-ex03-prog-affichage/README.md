# Exercice 3 : Programme COBOL-CICS d'affichage

## Objectif

Creer le programme necessaire pour l'affichage des donnees pour un code CLIENT saisi. Il doit permettre une saisie multiple de code CLIENT jusqu'a fin de saisie d'affichage de la part de l'utilisateur. De meme, il faut accompagner chaque anomalie ou action par un message d'information ou d'avertissement.

## Structure du programme

### Mode pseudo-conversationnel

Le programme utilise le mode pseudo-conversationnel CICS :
- **Premier passage** (EIBCALEN = 0) : Affiche l'ecran vide
- **Passages suivants** : Traite la saisie et affiche le resultat
- **RETURN TRANSID** : Revient au programme apres chaque interaction

### Flux du programme

```
DEBUT
  |
  v
EIBCALEN = 0 ? --OUI--> Afficher ecran vide
  |
  NON
  |
  v
PF3 ? --OUI--> Fin programme
  |
  NON
  |
  v
CLEAR ? --OUI--> Afficher ecran vide
  |
  NON
  |
  v
Recevoir MAP
  |
  v
Numero saisi ? --NON--> Message erreur
  |
  OUI
  |
  v
Lire VSAM FCLIENT
  |
  v
Trouve ? --NON--> Message "CLIENT INEXISTANT"
  |
  OUI
  |
  v
Afficher donnees client
  |
  v
RETURN TRANSID('AFFI')
```

## Commandes CICS utilisees

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'ecran a l'utilisateur |
| RECEIVE MAP | Recevoir les donnees saisies |
| READ FILE | Lire un enregistrement VSAM par cle |
| RETURN TRANSID | Retour pseudo-conversationnel |
| RETURN | Fin de transaction |
| SEND TEXT | Afficher un message simple |

## Variables EIB utilisees

| Variable | Description |
|----------|-------------|
| EIBCALEN | Longueur COMMAREA (0 = premier passage) |
| EIBAID | Touche fonction appuyee |
| DFHPF3 | Constante pour PF3 |
| DFHCLEAR | Constante pour CLEAR |
| DFHRESP(NORMAL) | Lecture OK |
| DFHRESP(NOTFND) | Enregistrement non trouve |

## Structure de l'enregistrement CLIENT

```cobol
01  ENR-CLIENT.
    05 CLI-NUMCPT           PIC X(06).    Numero compte (cle)
    05 CLI-CODREG           PIC X(02).    Code region
    05 CLI-NATCPT           PIC X(02).    Nature compte
    05 CLI-NOM              PIC X(10).    Nom
    05 CLI-PRENOM           PIC X(10).    Prenom
    05 CLI-DATNAISS         PIC X(08).    Date naissance
    05 CLI-SEXE             PIC X(01).    Sexe (M/F)
    05 CLI-ACTPRO           PIC X(02).    Activite professionnelle
    05 CLI-SITSO            PIC X(01).    Situation sociale
    05 CLI-ADRESSE          PIC X(10).    Adresse
    05 CLI-SOLDE            PIC X(10).    Solde
    05 CLI-POSITION         PIC X(02).    Position (DB/CR)
    05 FILLER               PIC X(16).    Reserve
```

## Compilation

### JCL de compilation : CMPCLAF.jcl

```jcl
//ROCHA04 JOB (ACCT),'COMPILE CLIAFF',CLASS=A,MSGCLASS=X,
//             MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*****************************************************************
//* COMPILATION DU PROGRAMME COBOL-CICS CLIAFF
//*****************************************************************
//PROCMAN  JCLLIB ORDER=(DFH510.CICS.SDFHPROC,ROCHA.CICS.SOURCE,
//          ROCHA.CICS.LINK,ROCHA.CICS.LOAD)
//*
//COMPIL   EXEC PROC=DFHYITVL,
//          INDEX='DFH510.CICS',
//          PROGLIB='ROCHA.CICS.LOAD',
//          AD370HLQ='IGY420',
//          DSCTLIB='ROCHA.CICS.LINK',
//          LE370HLQ='CEE'
//TRN.SYSIN DD DSN=ROCHA.CICS.SOURCE(CLIAFF),DISP=SHR
//LKED.SYSIN DD *
     INCLUDE SYSLIB(DFHELII)
     NAME CLIAFF(R)
/*
```

### Parametres de la procedure DFHYITVL

| Parametre | Valeur | Description |
|-----------|--------|-------------|
| INDEX | DFH510.CICS | Prefixe des libraries CICS |
| PROGLIB | ROCHA.CICS.LOAD | Destination du load module |
| AD370HLQ | IGY420 | Prefixe compilateur COBOL |
| DSCTLIB | ROCHA.CICS.LINK | Library des copybooks (DSECT BMS) |
| LE370HLQ | CEE | Language Environment |

## Fichiers

- `CLIAFF.cbl` : Source COBOL-CICS
- `CMPCLAF.jcl` : JCL de compilation

## Messages du programme

| Message | Contexte |
|---------|----------|
| SAISIR LE NUMERO DE COMPTE ET APPUYER SUR ENTREE | Premier affichage |
| VEUILLEZ SAISIR UN NUMERO DE COMPTE | Numero non saisi |
| CLIENT INEXISTANT - VERIFIEZ LE NUMERO | Client non trouve |
| CLIENT TROUVE - PF3=QUITTER OU NOUVELLE RECHERCHE | Affichage reussi |
| ERREUR LECTURE FICHIER - CONTACTEZ SUPPORT | Erreur VSAM |
| TRANSACTION AFFI TERMINEE - AU REVOIR | Fin (PF3) |

## Test

Apres compilation et definition de la transaction :

1. Taper `AFFI` pour lancer la transaction
2. Saisir un numero de compte (ex: 000001)
3. Appuyer sur ENTREE
4. Verifier l'affichage des donnees
5. PF3 pour quitter
