# Vérifications de vertebralis_notebook.qmd
vertebralis <- parse_rmd("../../vertebralis_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes vertebralis_notebook est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("vertebralis_notebook.qmd"))
  # La version compilée HTML du carnet de notes vertebralis_notebook est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("vertebralis_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document vertebralis_notebook est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Croissance du foraminifère sans son hôte",
    "Vérification des conditions d'application du test",
    "Comparaisons multiples en absence de l'hôte",
    "Autre version du premier test",
    "Croissance du foraminifère en présence de l'algue rouge",
    "Vérification des conditions d'application du test en présence de l'hôte",
    "Autre version du test en présence de l'hôte", "Discussion et conclusions",
    "Références")
    %in% (rmd_node_sections(vertebralis) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes vertebralis_notebook ne sont pas
  # toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "desccomment", "filter1", "table1",
    "plot1", "desc1comment", "test1choice", "test1hypo", "anova1",
    "test1comment", "normal1", "homos1", "test1appli", "multcomp1",
    "test1multicomment", "test1bchoice", "test1bhypo", "kruskal1",
    "multcompkw1", "test1bcomment", "filter2", "table2", "plot2",
    "desc2comment", "test2choice", "test2hypo", "anova2", "test2comment",
    "normal2", "homos2", "test2appli", "test2bchoice", "test2bhypo", "kruskal2",
    "test2bcomment") %in% rmd_node_label(vertebralis)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent dans
  # vertebralis_notebook.qmd
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(vertebralis))))
  # Un ou plusieurs labels de chunks sont dupliqués dans vertebralis_notebook.qmd
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété dans vertebralis_ca ?", {
  expect_true(vertebralis[[1]]$author != "___")
  expect_true(!grepl("__", vertebralis[[1]]$author))
  expect_true(grepl("^[^_]....+", vertebralis[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", vertebralis[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", vertebralis[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import', 'desccomment' : importation des données `growth.rds` depuis `data`", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `growth` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct.

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `growth` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `growth` est incorrect
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("desccomment"))
  # Les commentaires sur les données sont(partiellement) fausse dans le chunk
  # 'descscomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'filter1', 'table1', 'plot1', 'desc1comment' : description des données sans hôte", {
  expect_true(is_identical_to_ref("filter1", "names"))
  # Les colonnes dans le tableau `growth1` filtré ne sont pas celles attendues
  # Votre jeu de données filtré n'est pas correct.

  expect_true(is_identical_to_ref("filter1", "classes"))
  # La nature des variables (classe) dans le tableau `growth1` est incorrecte
  # Vérifiez le chunk `filter1`.

  expect_true(is_identical_to_ref("filter1", "nrow"))
  # Le nombre de lignes dans le tableau `growth1` est incorrect
  # Vérifiez le chunk `filter1`.

  expect_true(is_identical_to_ref("table1", "names"))
  # Les colonnes dans le tableau `growth1_tab` ne sont pas celles attendues
  # Votre tableau n'est pas correct.

  expect_true(is_identical_to_ref("table1", "classes"))
  # La nature des variables (classe) dans le tableau `growth1_tab` est incorrecte
  # Vérifiez le chunk `table1`.

  expect_true(is_identical_to_ref("table1", "nrow"))
  # Le nombre de lignes dans le tableau `growth1_tab` est incorrect
  # Vérifiez le chunk `table1`.

  expect_true(is_identical_to_ref("plot1"))
  # Le graphique produit par le chunk 'plot1' n'est pas celui attendu.
  # Vérifiez le graphique réalisé qui doit être un nuage de points "jitter" avec
  # des boites de dispersion parallèles superposées ainsi que la moyenne en
  # rouge.

  expect_true(is_identical_to_ref("desc1comment"))
  # Les commentaires sur les données sont(partiellement) fausse dans le chunk
  # 'desc1comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'test1choice', 'test1hypo', 'anova1', 'test1comment' : Premier test sans l'hôte", {
  expect_true(is_identical_to_ref("test1choice"))
  # La spécification du premier test est incorrecte dans le chunk 'test1choice'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("test1hypo"))
  # La spécification des hypothèses du premier test est incorrecte dans le chunk
  # 'test1hypo'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_equal_to_ref("anova1"))
  # Le test dans le chunk 'anova1' n'est pas celui attendu.
  # Vérifiez votre code dans le chunk 'anova1'.

  expect_true(is_identical_to_ref("test1comment"))
  # L'interprétation du premier test est (partiellement) fausse dans le chunk
  # 'test1comment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'normal1', 'homos1', 'test1appli' : Vérification des conditions du premier test sans l'hôte", {
  expect_true(is_identical_to_ref("normal1"))
  # Le graphique est incorrect dans le chunk 'normal1'
  # Vous devez réaliser un graphique quantile-quantile relatif à votre ANOVA

  expect_true(is_equal_to_ref("homos1"))
  # Votre test d'homoscédasticité est incorrect dans le chunk 'homos1'
  # Vous devez effectuer un test de Bartlett avec les mêmes données que pour le
  # test principal.

  expect_true(is_identical_to_ref("test1appli"))
  # L'interprétation des conditions d'application du premier test est
  # (partiellement) fausse dans le chunk 'test1appli'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'multcomp1', 'test1multicomment' : Comparaisons multiples du premier test sans l'hôte", {
  expect_true(is_equal_to_ref("multcomp1"))
  # L'analyse post hoc ou le graphique est incorrect dans le chunk 'multcomp1'
  # Vérifiez votre code dans ce chunk.

  expect_true(is_identical_to_ref("test1multicomment"))
  # L'interprétation des tests post hoc du premier test est (partiellement)
  # fausse dans le chunk 'test1multicomment'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'test1bchoice', 'test1bhypo', 'kruskal1', 'multcompkw1' 'test1bcomment' : test alternatif sans l'hôte", {
  expect_true(is_identical_to_ref("test1bchoice"))
  # La spécification du premier test est incorrecte dans le chunk 'test1bchoice'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("test1bhypo"))
  # La spécification des hypothèses du premier test est incorrecte dans le chunk
  # 'test1bhypo'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_equal_to_ref("kruskal1"))
  # Le test dans le chunk 'kruskal1' n'est pas celui attendu.
  # Vérifiez votre code dans le chunk 'kruskal1'.

  expect_true(is_equal_to_ref("multcompkw1"))
  # Les comparaisons multiples du test alternatif dans le chunk 'multcompkw1'
  # ne sont pas celles attendues.
  # Vérifiez votre code dans le chunk 'multcompkw1'.

  expect_true(is_identical_to_ref("test1bcomment"))
  # L'interprétation du test alternatif est (partiellement) fausse dans le chunk
  # 'test1bcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'filter2', 'table2', 'plot2', 'desc2comment' : description des données avec hôte", {
  expect_true(is_identical_to_ref("filter2", "names"))
  # Les colonnes dans le tableau `growth2` filtré ne sont pas celles attendues
  # Votre jeu de données filtré n'est pas correct.

  expect_true(is_identical_to_ref("filter2", "classes"))
  # La nature des variables (classe) dans le tableau `growth2` est incorrecte
  # Vérifiez le chunk `filter2`.

  expect_true(is_identical_to_ref("filter2", "nrow"))
  # Le nombre de lignes dans le tableau `growth2` est incorrect
  # Vérifiez le chunk `filter2`.

  expect_true(is_identical_to_ref("table2", "names"))
  # Les colonnes dans le tableau `growth2_tab` ne sont pas celles attendues
  # Votre tableau n'est pas correct.

  expect_true(is_identical_to_ref("table2", "classes"))
  # La nature des variables (classe) dans le tableau `growth2_tab` est incorrecte
  # Vérifiez le chunk `table2`.

  expect_true(is_identical_to_ref("table2", "nrow"))
  # Le nombre de lignes dans le tableau `growth2_tab` est incorrect
  # Vérifiez le chunk `table2`.

  expect_true(is_identical_to_ref("plot2"))
  # Le graphique produit par le chunk 'plot2' n'est pas celui attendu.
  # Vérifiez le graphique réalisé qui doit être un nuage de points "jitter" avec
  # des boites de dispersion parallèles superposées ainsi que la moyenne en
  # rouge.

  expect_true(is_identical_to_ref("desc2comment"))
  # Les commentaires sur les données sont(partiellement) fausse dans le chunk
  # 'desc2comment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'test2choice', 'test2hypo', 'anova2', 'test2comment' : Test avec l'hôte", {
  expect_true(is_identical_to_ref("test2choice"))
  # La spécification du test avec hôte est incorrecte dans le chunk 'test2choice'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("test2hypo"))
  # La spécification des hypothèses du test avec hôte est incorrecte dans le
  # chunk 'test2hypo'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_equal_to_ref("anova2"))
  # Le test dans le chunk 'anova2' n'est pas celui attendu.
  # Vérifiez votre code dans le chunk 'anova2'.

  expect_true(is_identical_to_ref("test2comment"))
  # L'interprétation du test avec hôte est (partiellement) fausse dans le chunk
  # 'test2comment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'normal2', 'homos2', 'test2appli' : Vérification des conditions du test avec l'hôte", {
  expect_true(is_identical_to_ref("normal2"))
  # Le graphique est incorrect dans le chunk 'normal2'
  # Vous devez réaliser un graphique quantile-quantile relatif à votre ANOVA.

  expect_true(is_equal_to_ref("homos2"))
  # Votre test d'homoscédasticité est incorrect dans le chunk 'homos2'
  # Vous devez effectuer un test de Bartlett avec les mêmes données que pour le
  # test principal.

  expect_true(is_identical_to_ref("test2appli"))
  # L'interprétation des conditions d'application du test avec hôte est
  # (partiellement) fausse dans le chunk 'test2appli'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'test2bhoice', 'test2bhypo', 'kruskal2', 'test2bcomment' : test alternatif avec l'hôte", {
  expect_true(is_identical_to_ref("test2bchoice"))
  # La spécification du test alternatif avec l'hôte est incorrecte dans le chunk
  # 'test2bchoice'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("test2bhypo"))
  # La spécification des hypothèses du test alternatif avec l'hôte est
  # incorrecte dans le chunk 'test2bhypo'
  # Vous devez cochez les phrases correctes d'un 'x' entre les crochets
  # [] -> [x]. Ensuite, vous devez recompiler la version HTML du bloc-notes
  # (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_equal_to_ref("kruskal2"))
  # Le test dans le chunk 'kruskal2' n'est pas celui attendu.
  # Vérifiez votre code dans le chunk 'kruskal2'.

  expect_true(is_identical_to_ref("test2bcomment"))
  # L'interprétation du test alternatif avec hôte est (partiellement) fausse
  # dans le chunk 'test2bcomment'
  # Vous devez cochez les phrases qui décrivent le test d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(vertebralis, by_section("Discussion et conclusions")) |>
      as_document() |> grepl("...Votre discussion ici...", x = _,
        fixed = TRUE) |> any()))
  # La discussion et les conclusions ne sont pas faites
  # Remplacez "...Votre discussion ici..." par vos phrases de commentaires
  # libres (à noter que le contenu de cette section n'est pas évalué
  # automatiquement, mais il le sera par vos enseignants).
})
