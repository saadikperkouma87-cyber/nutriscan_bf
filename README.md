# NutriScan-BF

Analyse statistique des déterminants de la malnutrition chronique (retard de croissance) chez les enfants de moins de cinq ans au Burkina Faso, et application web interactive qui valorise les résultats.

Projet réalisé lors d'un stage à la Direction Générale des Études et des Statistiques Sectorielles (DGESS) du Ministère de la Santé, Ouagadougou (4 mai – 4 juillet 2026), dans le cadre de la Licence en Ingénierie Statistique-Économie, Université Thomas Sankara.

## Contexte

Selon l'Enquête Démographique et de Santé 2021 (INSD & ICF, 2022), le retard de croissance touche 22,6 % des enfants de moins de cinq ans au Burkina Faso.

**Question de recherche :** quels sont les déterminants socioéconomiques, démographiques et biologiques du retard de croissance chez les enfants de moins de cinq ans en 2021, et quelle est leur contribution relative dans un cadre multivarié tenant compte du plan de sondage ?

## Données

- Source : EDSBF-V 2021 (Enquête Démographique et de Santé du Burkina Faso), INSD & ICF.
- Sous-échantillon extrait : 12 343 enfants.
- Après nettoyage et recodage : 5 731 enfants avec une mesure anthropométrique valide (prévalence estimée : 21,27 %).
- Modèle final estimé sur 5 655 observations (après exclusion des valeurs manquantes sur les variables explicatives).

Les données brutes ne sont **pas** incluses dans ce dépôt. Elles s'obtiennent sur le site du programme DHS (dhsprogram.com), après demande d'accès.

## Méthode

- Nettoyage et recodage des variables.
- Régression logistique avec prise en compte du plan de sondage complexe de l'EDS (package `survey` de R).
- Qualité du modèle : test de Hosmer-Lemeshow (p = 0,532) et AUC = 0,641 (IC 95 % bootstrap : [0,627 ; 0,660]).

## Principaux résultats

Sept facteurs sont significativement associés au retard de croissance :

| Facteur | Odds ratio | Référence |
|---|---|---|
| Sexe féminin | 0,73 | Masculin |
| Milieu rural | 1,45 | Urbain |
| Mère avec instruction primaire | 0,71 | Aucune instruction |
| Quintile de richesse le plus élevé | 0,62 | Quintile le plus pauvre |
| Âge de l'enfant | 1,006 par mois | – |
| Gémellité | 2,66 | Non jumeau |
| Intervalle inter-génésique < 24 mois | 1,57 | ≥ 36 mois |

L'intervalle de 24 à 35 mois (OR = 1,41) et le premier-né (OR = 1,30) sont aussi significatifs par rapport à un intervalle d'au moins 36 mois.

Le modèle est bien calibré, avec une capacité de discrimination modérée (AUC = 0,641).

## Application web NutriScan-BF

Application interactive développée en Python avec Streamlit, qui propose deux modules :

1. **Dépistage postnatal**
2. **Prédiction prénatale**

[Ajoute ici 1 ou 2 captures d'écran de l'application.]

### Lancer l'application

```bash
pip install -r requirements.txt
streamlit run [nom_du_fichier_principal].py
```

## Structure du dépôt

```
nutriscan_bf/
├── nutriscan_bf.py   # application Streamlit
├── requirements.txt
├── analyse_R/                      # code R de l'analyse statistique
└── README.md
```

## Limites

- L'AUC de 0,641 indique une capacité de discrimination modérée : l'application est un outil de démonstration issu d'un travail académique, **pas un outil de diagnostic médical**.
- Les résultats décrivent des associations statistiques, pas des relations de cause à effet.

## Auteur

Mouhamad Saadik PERKOUMA
Université Thomas Sankara, Ouagadougou, Burkina Faso
LinkedIn : https://www.linkedin.com/in/mouhamad-saadik-perkouma-0a359a2b8/

Maître de stage : M. Adama BANDE, DGESS/MS
Tuteur universitaire : Dr Alex Félix SANOGO, UTS

## Référence

INSD & ICF (2022). Enquête Démographique et de Santé du Burkina Faso 2021.
