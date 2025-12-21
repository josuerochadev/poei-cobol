# Chapitre VI - Algorithmes sur Fichiers

## 1. Introduction

### Contexte mainframe

Dans l'environnement mainframe COBOL, les traitements batch manipulent principalement des **fichiers** :
- Fichiers sequentiels (PS)
- Fichiers indexes (VSAM KSDS, ESDS)
- Fichiers relatifs (VSAM RRDS)

### Difference avec les tableaux

| Aspect | Tableau | Fichier |
|--------|---------|---------|
| Stockage | Memoire (RAM) | Disque |
| Taille | Limitee | Tres grande |
| Acces | Direct O(1) | Sequentiel ou indexe |
| Persistance | Volatile | Permanente |

### Operations de base sur les fichiers

```
Ouvrir(F)           // Ouverture du fichier
Fermer(F)           // Fermeture du fichier
Lire(F, enreg)      // Lecture d'un enregistrement
Ecrire(F, enreg)    // Ecriture d'un enregistrement
FinDeFichier(F)     // Test de fin de fichier
```

---

## 2. Schema de lecture sequentielle

### Principe

Parcourir tous les enregistrements d'un fichier du debut a la fin.

### Algorithme standard

```
Procedure Lecture_Sequentielle(F : Fichier)
Var enreg : Enregistrement
Debut
    Ouvrir(F)
    Lire(F, enreg)

    Tant que NON FinDeFichier(F) faire
        Traiter(enreg)
        Lire(F, enreg)
    Fait

    Fermer(F)
Fin
```

### Pattern "Read-Ahead"

La lecture se fait **avant** le traitement (lecture anticipee) :
1. Lire le premier enregistrement
2. Boucle : traiter, puis lire le suivant
3. Sortir quand fin de fichier

### Exemple : Calcul d'un total

```
Fonction Total_Montants(F : Fichier) : reel
Var enreg : Enregistrement
    total : reel
Debut
    total := 0
    Ouvrir(F)
    Lire(F, enreg)

    Tant que NON FinDeFichier(F) faire
        total := total + enreg.montant
        Lire(F, enreg)
    Fait

    Fermer(F)
    Retourner total
Fin
```

---

## 3. Rupture de controle (Control Break)

### Principe

La **rupture de controle** permet de detecter les changements de valeur d'une cle dans un fichier trie, afin de produire des sous-totaux et totaux.

### Cas d'usage

- Etats recapitulatifs (par departement, par client, par mois...)
- Sous-totaux et totaux generaux
- Rapports hierarchiques

### Niveaux de rupture

```
Fichier trie par : Region, Departement, Client

Niveau 1 (mineur)  : Rupture sur Client
Niveau 2 (moyen)   : Rupture sur Departement
Niveau 3 (majeur)  : Rupture sur Region
```

### Algorithme a un niveau de rupture

```
Procedure Etat_Avec_Rupture(F : Fichier)
Var enreg : Enregistrement
    cle_precedente : TypeCle
    sous_total : reel
    total_general : reel
Debut
    total_general := 0
    Ouvrir(F)
    Lire(F, enreg)

    Tant que NON FinDeFichier(F) faire
        // Debut d'un nouveau groupe
        cle_precedente := enreg.cle
        sous_total := 0

        // Traitement du groupe
        Tant que NON FinDeFichier(F) ET enreg.cle = cle_precedente faire
            sous_total := sous_total + enreg.montant
            Lire(F, enreg)
        Fait

        // Rupture : afficher le sous-total
        Ecrire("Sous-total ", cle_precedente, " : ", sous_total)
        total_general := total_general + sous_total
    Fait

    // Total general
    Ecrire("Total general : ", total_general)
    Fermer(F)
Fin
```

### Algorithme a deux niveaux de rupture

```
Procedure Etat_Deux_Niveaux(F : Fichier)
Var enreg : Enregistrement
    cle_majeure_prec, cle_mineure_prec : TypeCle
    total_mineur, total_majeur, total_general : reel
Debut
    total_general := 0
    Ouvrir(F)
    Lire(F, enreg)

    Tant que NON FinDeFichier(F) faire
        // Nouveau groupe majeur
        cle_majeure_prec := enreg.cle_majeure
        total_majeur := 0

        Tant que NON FinDeFichier(F) ET enreg.cle_majeure = cle_majeure_prec faire
            // Nouveau groupe mineur
            cle_mineure_prec := enreg.cle_mineure
            total_mineur := 0

            Tant que NON FinDeFichier(F)
                   ET enreg.cle_majeure = cle_majeure_prec
                   ET enreg.cle_mineure = cle_mineure_prec faire
                total_mineur := total_mineur + enreg.montant
                Lire(F, enreg)
            Fait

            // Rupture mineure
            Ecrire("  Sous-total ", cle_mineure_prec, " : ", total_mineur)
            total_majeur := total_majeur + total_mineur
        Fait

        // Rupture majeure
        Ecrire("Total ", cle_majeure_prec, " : ", total_majeur)
        total_general := total_general + total_majeur
    Fait

    Ecrire("TOTAL GENERAL : ", total_general)
    Fermer(F)
Fin
```

---

## 4. Fusion de fichiers (Merge)

### Principe

La **fusion** combine deux fichiers **tries** en un seul fichier trie.

### Prerequis

- Les deux fichiers d'entree doivent etre tries sur la meme cle
- Le fichier de sortie sera trie sur cette cle

### Algorithme de fusion

```
Procedure Fusion(F1, F2 : Fichier, var Resultat : Fichier)
Var enreg1, enreg2 : Enregistrement
    fin1, fin2 : booleen
Debut
    Ouvrir(F1)
    Ouvrir(F2)
    Ouvrir(Resultat, ecriture)

    Lire(F1, enreg1)
    Lire(F2, enreg2)
    fin1 := FinDeFichier(F1)
    fin2 := FinDeFichier(F2)

    // Fusion tant que les deux fichiers ont des donnees
    Tant que NON fin1 ET NON fin2 faire
        Si enreg1.cle <= enreg2.cle Alors
            Ecrire(Resultat, enreg1)
            Lire(F1, enreg1)
            fin1 := FinDeFichier(F1)
        Sinon
            Ecrire(Resultat, enreg2)
            Lire(F2, enreg2)
            fin2 := FinDeFichier(F2)
        Fsi
    Fait

    // Vider le reste de F1
    Tant que NON fin1 faire
        Ecrire(Resultat, enreg1)
        Lire(F1, enreg1)
        fin1 := FinDeFichier(F1)
    Fait

    // Vider le reste de F2
    Tant que NON fin2 faire
        Ecrire(Resultat, enreg2)
        Lire(F2, enreg2)
        fin2 := FinDeFichier(F2)
    Fait

    Fermer(F1)
    Fermer(F2)
    Fermer(Resultat)
Fin
```

### Complexite

O(n + m) ou n et m sont les tailles des deux fichiers.

---

## 5. Appareillement de fichiers (Matching)

### Principe

L'**appareillement** compare deux fichiers tries pour detecter :
- Les enregistrements **communs** (meme cle dans les deux fichiers)
- Les enregistrements **uniquement dans F1**
- Les enregistrements **uniquement dans F2**

### Cas d'usage

- Rapprochement bancaire
- Mise a jour de fichiers maitre
- Detection d'anomalies

### Algorithme d'appareillement

```
Procedure Appareillement(F1, F2 : Fichier)
Var enreg1, enreg2 : Enregistrement
    fin1, fin2 : booleen
Debut
    Ouvrir(F1)
    Ouvrir(F2)

    Lire(F1, enreg1)
    Lire(F2, enreg2)
    fin1 := FinDeFichier(F1)
    fin2 := FinDeFichier(F2)

    Tant que NON fin1 ET NON fin2 faire
        Si enreg1.cle = enreg2.cle Alors
            // Correspondance trouvee
            Traiter_Correspondance(enreg1, enreg2)
            Lire(F1, enreg1)
            Lire(F2, enreg2)
            fin1 := FinDeFichier(F1)
            fin2 := FinDeFichier(F2)

        SinonSi enreg1.cle < enreg2.cle Alors
            // Enregistrement uniquement dans F1
            Traiter_Absent_F2(enreg1)
            Lire(F1, enreg1)
            fin1 := FinDeFichier(F1)

        Sinon
            // Enregistrement uniquement dans F2
            Traiter_Absent_F1(enreg2)
            Lire(F2, enreg2)
            fin2 := FinDeFichier(F2)
        Fsi
    Fait

    // Traiter les restes
    Tant que NON fin1 faire
        Traiter_Absent_F2(enreg1)
        Lire(F1, enreg1)
        fin1 := FinDeFichier(F1)
    Fait

    Tant que NON fin2 faire
        Traiter_Absent_F1(enreg2)
        Lire(F2, enreg2)
        fin2 := FinDeFichier(F2)
    Fait

    Fermer(F1)
    Fermer(F2)
Fin
```

---

## 6. Mise a jour de fichier maitre

### Principe

Appliquer les mouvements (ajouts, modifications, suppressions) d'un fichier mouvement sur un fichier maitre.

### Types de mouvements

| Code | Action |
|------|--------|
| A | Ajout (creation) |
| M | Modification |
| S | Suppression |

### Algorithme de mise a jour

```
Procedure Mise_A_Jour(Maitre, Mouvement : Fichier, var Nouveau_Maitre : Fichier)
Var enreg_m, enreg_v : Enregistrement
    fin_m, fin_v : booleen
Debut
    Ouvrir(Maitre)
    Ouvrir(Mouvement)
    Ouvrir(Nouveau_Maitre, ecriture)

    Lire(Maitre, enreg_m)
    Lire(Mouvement, enreg_v)
    fin_m := FinDeFichier(Maitre)
    fin_v := FinDeFichier(Mouvement)

    Tant que NON fin_m ET NON fin_v faire
        Si enreg_m.cle < enreg_v.cle Alors
            // Maitre sans mouvement : recopier
            Ecrire(Nouveau_Maitre, enreg_m)
            Lire(Maitre, enreg_m)
            fin_m := FinDeFichier(Maitre)

        SinonSi enreg_m.cle = enreg_v.cle Alors
            // Appliquer le mouvement
            Selon enreg_v.code faire
                'M' : // Modification
                    Appliquer_Modif(enreg_m, enreg_v)
                    Ecrire(Nouveau_Maitre, enreg_m)
                'S' : // Suppression
                    // Ne pas ecrire (supprimer)
                Autre :
                    Erreur("Code invalide")
            Fselon
            Lire(Maitre, enreg_m)
            Lire(Mouvement, enreg_v)
            fin_m := FinDeFichier(Maitre)
            fin_v := FinDeFichier(Mouvement)

        Sinon // enreg_m.cle > enreg_v.cle
            // Mouvement sans maitre
            Si enreg_v.code = 'A' Alors
                // Ajout
                Ecrire(Nouveau_Maitre, enreg_v)
            Sinon
                Erreur("Modification/Suppression sans maitre")
            Fsi
            Lire(Mouvement, enreg_v)
            fin_v := FinDeFichier(Mouvement)
        Fsi
    Fait

    // Recopier le reste du maitre
    Tant que NON fin_m faire
        Ecrire(Nouveau_Maitre, enreg_m)
        Lire(Maitre, enreg_m)
        fin_m := FinDeFichier(Maitre)
    Fait

    // Traiter les ajouts restants
    Tant que NON fin_v faire
        Si enreg_v.code = 'A' Alors
            Ecrire(Nouveau_Maitre, enreg_v)
        Sinon
            Erreur("Mouvement sans maitre")
        Fsi
        Lire(Mouvement, enreg_v)
        fin_v := FinDeFichier(Mouvement)
    Fait

    Fermer(Maitre)
    Fermer(Mouvement)
    Fermer(Nouveau_Maitre)
Fin
```

---

## 7. Eclatement de fichier

### Principe

L'**eclatement** consiste a repartir les enregistrements d'un fichier en plusieurs fichiers de sortie selon un critere.

### Algorithme

```
Procedure Eclater(Entree : Fichier, var S1, S2, S3 : Fichier)
Var enreg : Enregistrement
Debut
    Ouvrir(Entree)
    Ouvrir(S1, ecriture)
    Ouvrir(S2, ecriture)
    Ouvrir(S3, ecriture)

    Lire(Entree, enreg)

    Tant que NON FinDeFichier(Entree) faire
        Selon enreg.type faire
            'A' : Ecrire(S1, enreg)
            'B' : Ecrire(S2, enreg)
            Autre : Ecrire(S3, enreg)
        Fselon
        Lire(Entree, enreg)
    Fait

    Fermer(Entree)
    Fermer(S1)
    Fermer(S2)
    Fermer(S3)
Fin
```

---

## 8. Correspondance avec COBOL/JCL

### Operations COBOL

| Algorithmique | COBOL |
|---------------|-------|
| `Ouvrir(F)` | `OPEN INPUT F` / `OPEN OUTPUT F` |
| `Fermer(F)` | `CLOSE F` |
| `Lire(F, enreg)` | `READ F INTO enreg` |
| `Ecrire(F, enreg)` | `WRITE enreg FROM ...` |
| `FinDeFichier(F)` | `AT END` ou `FILE STATUS` |

### Pattern COBOL de lecture

```cobol
PROCEDURE DIVISION.
    OPEN INPUT FICHIER-ENTREE.
    PERFORM LIRE-ENREG.

    PERFORM UNTIL FIN-FICHIER = 'O'
        PERFORM TRAITER-ENREG
        PERFORM LIRE-ENREG
    END-PERFORM.

    CLOSE FICHIER-ENTREE.
    STOP RUN.

LIRE-ENREG.
    READ FICHIER-ENTREE INTO WS-ENREG
        AT END MOVE 'O' TO FIN-FICHIER
    END-READ.
```

### JCL pour fusion

```jcl
//FUSION   EXEC PGM=SORT
//SORTIN01 DD DSN=FICHIER1,DISP=SHR
//SORTIN02 DD DSN=FICHIER2,DISP=SHR
//SORTOUT  DD DSN=FICHIER.FUSIONNE,DISP=(NEW,CATLG)
//SYSIN    DD *
  MERGE FIELDS=(1,10,CH,A)
/*
```

---

## 9. Bonnes pratiques

### Gestion des erreurs

```
Ouvrir(F)
Si Erreur Alors
    Afficher("Erreur ouverture fichier")
    Arreter
Fsi
```

### Verification des fichiers

- Toujours verifier que le fichier est trie avant fusion/appareillement
- Verifier le FILE STATUS apres chaque operation
- Gerer les cas d'erreur (fichier vide, enregistrement invalide)

### Performance

- Minimiser les ouvertures/fermetures de fichiers
- Utiliser le SORT JCL pour les gros volumes
- Preferer le traitement sequentiel quand possible

---

## 10. Exercices pratiques

Les exercices de ce chapitre sont disponibles dans :
- [Exercices theoriques (QCM)](../../exercices/algorithmique/theorie/qcm-06-fichiers.md)
- [Exercices pratiques](../../exercices/algorithmique/pratique/chapitre-06/)

---

## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre V - Complexité Algorithmique](05-complexite-algorithmique.md) | [Chapitre VII - Modularité](07-modularite.md) |

---
*Formation POEI Developpeur COBOL Grand Systeme - M2i Formation*
