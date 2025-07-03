 ![](https://github.com/ledemocrate/mouvement_financier/blob/main/mouvements_financiers_animation_adaptive_flux_final.gif)

# Simulation et Visualisation des Flux Financiers entre Agents

Ce projet vise à simuler et à visualiser les mouvements financiers au sein d'un réseau d'agents. Il explore la dynamique des soldes d'agents et les interactions de paiement au fil du temps.

## Aperçu du Projet

L'objectif principal de cette simulation est de modéliser un écosystème financier simplifié où des "agents" effectuent des transactions monétaires les uns avec les autres. Le projet intègre également des mécanismes d'injection et de retrait de fonds dans le système, simulant ainsi des flux externes qui influencent la liquidité globale.

## Fonctionnalités Clés

* **Génération de Transactions**: Création aléatoire de paiements entre agents, d'injections de fonds dans le système et de retraits de fonds hors du système.
* **Agents "Hub"**: Intégration de la notion d'agents "centraux" ou "hubs" qui peuvent avoir des comportements de transaction différents (par exemple, plus susceptibles de recevoir des paiements).
* **Flux Externes Adaptatifs**: Les injections et retraits sont modélisés de manière à s'adapter aux soldes actuels des agents, permettant de simuler des dynamiques de soutien ou de prélèvement ciblées.
* **Suivi des Soldes**: Calcul et mise à jour des soldes de chaque agent à travers le temps.
* **Visualisation Animée**: Utilisation de `gganimate` pour créer une animation visuelle du réseau, montrant l'évolution des soldes des agents (taille et couleur des nœuds) et l'intensité des flux financiers (épaisseur et opacité des arêtes) sur des intervalles de temps définis.

## Technologies Utilisées

* **R**: Langage de programmation principal pour la logique de simulation et la manipulation des données.
* **`dplyr`**: Pour la manipulation et l'agrégation des données.
* **`igraph`**: Pour la création et la manipulation de structures de graphes/réseaux.
* **`ggplot2`**: Pour la création de visualisations statiques.
* **`gganimate`**: Extension de `ggplot2` pour transformer les visualisations statiques en animations.
* **`lubridate`**: Pour la gestion et la manipulation des dates et des intervalles de temps.

## Comment Utiliser (Vaguement)

Le script principal (`.R`) contient toute la logique de génération des données, de calcul des états du réseau et de création de l'animation.

1.  **Charger les librairies nécessaires.**
2.  **Définir les paramètres de la simulation** (nombre d'agents, nombre de transactions, intervalle de temps pour l'animation, paramètres des agents "hub" et des flux externes).
3.  **Exécuter la génération des stocks initiaux et des transactions.**
4.  **Exécuter la boucle de calcul des états du réseau** par intervalle de temps pour préparer les données d'animation.
5.  **Exécuter le code de `ggplot2` et `gganimate`** pour générer et sauvegarder l'animation (généralement au format GIF).

**Note:** Les chemins de fichiers ou les spécificités d'exécution détaillées ne sont pas inclus ici, car ce README est une présentation générale. Référez-vous au code source pour les détails d'implémentation.

---

**Développé par:** goldentzgrahamz a.k.a coachdagrahamz a.k.a ledemocrate
**Date:** 25 Juillet 2025
