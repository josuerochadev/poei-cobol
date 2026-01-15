# Exercice 10 : Programme de mise a jour (REWRITE)

## Objectif

Creer le programme COBOL-CICS PRGMAJ pour mettre a jour un client existant dans le fichier VSAM FCLIENT. Le programme utilise la commande REWRITE et gere un mode pseudo-conversationnel a 3 phases.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `PRGMAJ.cbl` | Programme COBOL-CICS de mise a jour |
| `CMPMAJ.jcl` | JCL de compilation |

## Difference WRITE vs REWRITE

| Aspect | WRITE (Ajout) | REWRITE (Mise a jour) |
|--------|---------------|----------------------|
| Client | Ne doit PAS exister | DOIT exister |
| Cle | Nouvelle | Existante (non modifiable) |
| Prerequis | Aucun | READ UPDATE obligatoire |
| Erreur typique | DUPREC (doublon) | NOTFND (inexistant) |

## Mode pseudo-conversationnel a 3 phases

```
+------------------+     +-------------------+     +-------------------+
|   PHASE 1        |     |   PHASE 2         |     |   PHASE 3         |
|   RECHERCHE      | --> |   AFFICHAGE       | --> |   VALIDATION      |
+------------------+     +-------------------+     +-------------------+
|                  |     |                   |     |                   |
| NUMCPT: UNPROT   |     | NUMCPT: ASKIP     |     | NUMCPT: ASKIP     |
| Autres: vides    |     | Autres: remplis   |     | Autres: modifies  |
|                  |     |                   |     |                   |
| Action: Saisie   |     | Action: READ      |     | Action: REWRITE   |
| du numero        |     | + Affichage       |     | apres validation  |
+------------------+     +-------------------+     +-------------------+
```

## Concept cle : READ UPDATE + REWRITE

En CICS, la commande REWRITE necessite un READ UPDATE prealable dans la meme unite de travail (UOW). Cependant, en mode pseudo-conversationnel, chaque interaction utilisateur termine la tache CICS.

**Solution implementee :**

```cobol
* Dans 4300-ECRIRE-MODIFICATION :
* 1. Relecture avec UPDATE (verrouillage)
EXEC CICS READ
    FILE('FCLIENT')
    INTO(ENR-CLIENT)
    RIDFLD(CLI-NUMCPT)
    UPDATE              <-- Verrouille l'enregistrement
    RESP(WS-RESP)
END-EXEC

* 2. Application des modifications
MOVE WS-NOM TO CLI-NOM
...

* 3. REWRITE (mise a jour effective)
EXEC CICS REWRITE
    FILE('FCLIENT')
    FROM(ENR-CLIENT)
    RESP(WS-RESP)
END-EXEC
```

## Structure de la COMMAREA

La COMMAREA sauvegarde l'etat entre les passages :

```cobol
01  WS-COMMAREA.
    05 WS-PHASE            PIC X(01) VALUE '1'.
       88 PHASE-RECHERCHE  VALUE '1'.
       88 PHASE-AFFICHAGE  VALUE '2'.
       88 PHASE-VALIDATION VALUE '3'.
    05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.
```

- `WS-PHASE` : Indique la phase en cours
- `WS-NUMCPT-SAVED` : Conserve le numero de compte (car NUMCPT sera en ASKIP donc non transmis)

## Gestion dynamique des attributs

```cobol
* Apres lecture du client, proteger NUMCPT
MOVE DFHBMASK TO NUMCPTA

* DFHBMASK = X'20' = ASKIP (protege, intensite normale)
* Defini dans le copybook DFHBMSCA
```

## Utilisation

### 1. Copier le source COBOL dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member PRGMAJ
Copier le contenu de PRGMAJ.cbl
```

### 2. Soumettre le JCL de compilation

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CMPMAJ (copier CMPMAJ.jcl)
SUB (submit)
```

### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre PRGMAJ present dans ROCHA.CICS.LOAD

### 4. Definir le programme dans CICS

```
CEDA DEFINE PROGRAM(PRGMAJ) GROUP(CLIGROUP) LANGUAGE(COBOL)
CEDA INSTALL PROGRAM(PRGMAJ) GROUP(CLIGROUP)
```

## Verification

```
CEMT INQ PROGRAM(PRGMAJ)
```

Resultat attendu : `Prog(PRGMAJ) Cob Ena`

## Points importants

1. **COPY DFHBMSCA** : Ajoute pour avoir acces aux constantes d'attribut (DFHBMASK, etc.)

2. **Sauvegarde du NUMCPT** : Le numero est sauvegarde dans WS-NUMCPT-SAVED car une fois en ASKIP, il n'est plus transmis par le terminal

3. **READ sans UPDATE en phase 2** : La premiere lecture (affichage) n'utilise pas UPDATE car le verrouillage ne persiste pas entre les passages pseudo-conversationnels

4. **READ UPDATE + REWRITE atomique** : Les deux commandes sont executees dans le meme paragraphe pour garantir l'atomicite

5. **Retour en phase 1** : Apres une mise a jour reussie, le programme revient en phase RECHERCHE pour permettre la modification d'un autre client
