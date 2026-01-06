# TODO - Points d'Amélioration du Repository

## Priorité Haute

- [x] **Navigation VSAM** : Ajouter section Navigation aux 7 fichiers `cours/vsam/*.md` ✅
- [x] **Navigation Algorithmique** : Ajouter section Navigation aux 9 fichiers `cours/algorithmique/*.md` ✅
- [x] **Lien cassé JCL** : Corriger `cours/jcl/README.md:16` - pointer vers `../../exercices/jcl/tp/README.md` ✅

## Priorité Moyenne

- [x] **Accents JCL** : Harmoniser les accents dans `cours/jcl/*.md` (chapitres) ✅
- [x] **Accents VSAM** : Harmoniser les accents dans `cours/vsam/*.md` ✅
- [x] **Accents Algorithmique** : Harmoniser les accents dans `cours/algorithmique/*.md` ✅ (déjà correct)
- [x] **Accents exercices JCL** : Harmoniser `exercices/jcl/README.md` ✅
- [x] **README exercices/jcl/theorie/** : Ajouter un README ✅
- [x] **README exercices/cics/theorie/** : Ajouter un README ✅
- [x] **README exercices/algorithmique/theorie/** : Ajouter un README ✅
- [x] **README exemples/** : Ajouter un README pour documenter hello-world.cbl ✅
- [x] **README utils/** : Ajouter un README ✅

## Priorité Basse

- [x] **Activité DB2 manquante** : Vérifié - activite-03 (fonctions scalaires) intentionnellement non incluse, documenté dans README ✅
- [x] **Navigation README zos-tso** : Ajouter navigation inter-modules à `cours/zos-tso/README.md` ✅
- [x] **Navigation README jcl** : Ajouter navigation inter-modules à `cours/jcl/README.md` ✅
- [x] **Navigation README vsam** : Ajouter navigation inter-modules à `cours/vsam/README.md` ✅
- [x] **Navigation README algorithmique** : Ajouter navigation inter-modules à `cours/algorithmique/README.md` ✅
- [x] **Navigation README cobol** : Ajouter navigation inter-modules à `cours/cobol/README.md` ✅
- [x] **Navigation README cics** : Ajouter navigation inter-modules à `cours/cics/README.md` ✅
- [x] **Navigation README db2** : Navigation inter-modules à `cours/db2/README.md` ✅
- [x] **Gitignore binaires** : Ajouter les exécutables compilés au `.gitignore` ✅

---

## Format de Navigation à utiliser

### Pour les chapitres (exemple VSAM)

```markdown
## Navigation

| Précédent | Suivant |
|-----------|---------|
| [Chapitre X - Titre](XX-fichier.md) | [Chapitre Y - Titre](YY-fichier.md) |

---
*Formation VSAM - M2i Formation*
```

### Pour les READMEs de modules

```markdown
## Navigation

| Module précédent | Module suivant |
|------------------|----------------|
| [Nom Module](../module/README.md) | [Nom Module](../module/README.md) |

---
*Formation COBOL - M2i Formation*
```

---
*Mis à jour le 2025-12-21*
