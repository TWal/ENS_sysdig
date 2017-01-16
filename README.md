# Projet de système digital.

## Simulateur

Le simulateur est dans un dossier séparé `git submodule --init` pour le créer et `git submodule update` pour le mettre à jour.

Son repo est à l'adresse : [https://git.eleves.ens.fr/perami/NetListCppCompiler](https://git.eleves.ens.fr/perami/NetListCppCompiler)

Son readme est bien plus détaillé.

## Assembleur

L'assembleur est dans le dossier `asm`. Il comporte un Makefile et est compilable avec `make`.
Ses options sont `-o` suivi d'un nom de fichier pour préciser la destination,
`-v` pour un mode verbeux et un fichier sans option pour l'entrée :
`asm code.s - o exec` pour assembler `code.s` dans `exec`.

## Processeur

Le processeur est compilable avec `make`, il sera crée dans le fichier `proco`, la netlist étant dans `proco.net`.
Ce makefile ne vérifie que les fichier source on été mis à jour seulement l'éxécutable. 
Penser à mettre à jour le simulateur avan de faire make.

Le processeur est opérationel à l'exception des instruction `gpu`

Pour plus détail sur la compilation du Haskell :

Compiler : `cabal build`

Lancer normalement : `cabal run`

Configurer pour afficher les stacktrace : `cabal configure --enable-profiling`

Lancer avec les stacktraces : `./run.sh`

## Horloge

L'horloge se situe dans `clock\clock.m4`. Le Makefile permet de l'assembler, mais elle n'est pas fonctionnelle pour l'instant.



