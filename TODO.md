# TODO - Points d'Amélioration du Repository

## Priorité Haute

- [x] **Navigation VSAM** : Ajouter section Navigation aux 7 fichiers `cours/vsam/*.md` ✅
- [x] **Navigation Algorithmique** : Ajouter section Navigation aux 9 fichiers `cours/algorithmique/*.md` ✅
- [x] **Lien cassé JCL** : Corriger `cours/jcl/README.md:16` - pointer vers `../../exercices/jcl/tp/README.md` ✅

## Priorité Moyenne

- [ ] **Accents JCL** : Harmoniser les accents dans `cours/jcl/*.md` (chapitres)
- [ ] **Accents VSAM** : Harmoniser les accents dans `cours/vsam/*.md`
- [ ] **Accents Algorithmique** : Harmoniser les accents dans `cours/algorithmique/*.md`
- [ ] **Accents exercices JCL** : Harmoniser `exercices/jcl/README.md`
- [x] **README exercices/jcl/theorie/** : Ajouter un README ✅
- [x] **README exercices/cics/theorie/** : Ajouter un README ✅
- [x] **README exercices/algorithmique/theorie/** : Ajouter un README ✅
- [x] **README exemples/** : Ajouter un README pour documenter hello-world.cbl ✅
- [x] **README utils/** : Ajouter un README ✅

## Priorité Basse

- [ ] **Activité DB2 manquante** : Vérifier si activite-03 doit exister dans `exercices/db2/tp/` ou renommer les autres
- [ ] **Navigation README zos-tso** : Ajouter navigation inter-modules à `cours/zos-tso/README.md`
- [ ] **Navigation README jcl** : Ajouter navigation inter-modules à `cours/jcl/README.md`
- [ ] **Navigation README vsam** : Ajouter navigation inter-modules à `cours/vsam/README.md`
- [ ] **Navigation README algorithmique** : Ajouter navigation inter-modules à `cours/algorithmique/README.md`
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
