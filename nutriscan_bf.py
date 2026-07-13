import streamlit as st
import math

# ================================================
# CONFIGURATION
# ================================================
st.set_page_config(
    page_title="NutriScan-BF — DGESS",
    page_icon="🏥",
    layout="wide",
    initial_sidebar_state="collapsed"
)

# ================================================
# SESSION STATE INITIALISATION
# ================================================
for key, default in {
    "page"        : "accueil",
    "mode"        : None,
    "resultats"   : {},
    "form_data"   : {},
}.items():
    if key not in st.session_state:
        st.session_state[key] = default


def aller_a(page):
    st.session_state.page = page
    st.rerun()


# ================================================
# CSS GLOBAL
# ================================================
st.markdown("""
<style>
/* ---- RESET & BASE ---- */
.block-container { padding: 1.2rem 2rem 2rem; max-width: 1100px; }

/* ---- EN-TÊTE ---- */
.entete {
    background: linear-gradient(135deg, #1F5C99 0%, #2E86C1 100%);
    padding: 24px 36px 20px;
    border-radius: 14px;
    margin-bottom: 28px;
    text-align: center;
    box-shadow: 0 4px 18px rgba(31,92,153,0.22);
}
.entete h1 {
    color: white; margin: 0;
    font-size: 2.2rem; letter-spacing: 1.5px;
}
.entete .sous-titre {
    color: #D6E4F0; font-size: 1rem; margin-top: 6px;
    font-style: italic;
}
.entete .institution {
    color: #AED6F1; font-size: 0.85rem; margin-top: 6px; line-height: 1.6;
}

/* ---- BOUTONS D'ACCUEIL ---- */
.btn-accueil-vert {
    display: block; width: 100%;
    background: linear-gradient(135deg, #2A9D8F, #20857A);
    color: white !important; border: none; border-radius: 14px;
    padding: 36px 24px; text-align: center; cursor: pointer;
    font-size: 1.35rem; font-weight: 700;
    box-shadow: 0 6px 20px rgba(42,157,143,0.3);
    transition: transform 0.15s, box-shadow 0.15s;
    text-decoration: none;
}
.btn-accueil-orange {
    display: block; width: 100%;
    background: linear-gradient(135deg, #F4A261, #E07A3A);
    color: white !important; border: none; border-radius: 14px;
    padding: 36px 24px; text-align: center; cursor: pointer;
    font-size: 1.35rem; font-weight: 700;
    box-shadow: 0 6px 20px rgba(244,162,97,0.3);
    transition: transform 0.15s, box-shadow 0.15s;
    text-decoration: none;
}
.btn-sous { font-size: 0.88rem; font-weight: 400;
            margin-top: 8px; opacity: 0.92; display: block; }

/* ---- SECTIONS DE FORMULAIRE ---- */
.sec-vert {
    background: #E8F8F5; border-left: 5px solid #2A9D8F;
    padding: 8px 14px; border-radius: 6px; font-weight: 600;
    color: #1B7168; margin: 18px 0 8px; font-size: 0.95rem;
}
.sec-orange {
    background: #FEF5E7; border-left: 5px solid #F4A261;
    padding: 8px 14px; border-radius: 6px; font-weight: 600;
    color: #9C5A1D; margin: 18px 0 8px; font-size: 0.95rem;
}
.sec-bleu {
    background: #EBF5FB; border-left: 5px solid #1F5C99;
    padding: 8px 14px; border-radius: 6px; font-weight: 600;
    color: #1A4F7A; margin: 18px 0 8px; font-size: 0.95rem;
}

/* ---- RÉSULTATS ---- */
.proba-display {
    text-align: center; font-size: 4.2rem;
    font-weight: 800; line-height: 1; margin: 10px 0 4px;
}
.niveau-display {
    text-align: center; font-size: 1.2rem;
    font-weight: 700; margin-bottom: 8px;
}
.risque-faible {
    background: #D5F5E3; border-left: 5px solid #1E8449;
    padding: 16px 20px; border-radius: 10px; margin: 10px 0;
}
.risque-modere {
    background: #FEF9E7; border-left: 5px solid #D4AC0D;
    padding: 16px 20px; border-radius: 10px; margin: 10px 0;
}
.risque-eleve {
    background: #FDEDEC; border-left: 5px solid #C0392B;
    padding: 16px 20px; border-radius: 10px; margin: 10px 0;
}
.facteur-r {
    background: #FDEDEC; border-left: 3px solid #C0392B;
    border-radius: 6px; padding: 6px 12px; margin: 3px 0;
    font-size: 0.88rem; color: #922B21;
}
.facteur-p {
    background: #D5F5E3; border-left: 3px solid #27AE60;
    border-radius: 6px; padding: 6px 12px; margin: 3px 0;
    font-size: 0.88rem; color: #1E8449;
}
.contexte-box {
    background: #F4F6F7; border-radius: 8px;
    padding: 12px 16px; margin-top: 6px;
    font-size: 0.85rem; color: #555;
    border: 1px solid #E5E8E8;
}
.footer-note {
    background: #EBF5FB; border-radius: 8px;
    padding: 12px 16px; font-size: 0.82rem;
    color: #555; margin-top: 14px;
    border: 1px solid #D6EAF8;
}
.avert-pre {
    background: #FEF5E7; border-radius: 8px;
    padding: 12px 16px; font-size: 0.85rem;
    color: #784212; margin-bottom: 14px;
    border-left: 4px solid #F4A261;
}
.badge-date {
    background: rgba(255,255,255,0.18); border-radius: 20px;
    padding: 3px 14px; font-size: 0.78rem;
    color: #D6E4F0; display: inline-block; margin-top: 6px;
}

/* ---- PRINT ---- */
@media print {
    .no-print { display: none !important; }
    .block-container { padding: 0 !important; }
    .entete { border-radius: 0 !important; margin-bottom: 16px !important; }
    .stButton { display: none !important; }
}
</style>
""", unsafe_allow_html=True)


# ================================================
# COMPOSANT : EN-TÊTE
# ================================================
def entete():
    st.markdown("""
    <div class="entete">
        <h1>🏥 NutriScan-BF</h1>
        <div class="sous-titre">Outil de dépistage et de prédiction du retard de croissance</div>
        <div class="institution">
            Direction Générale des Études et des Statistiques Sectorielles (DGESS)<br>
            Ministère de la Santé — Burkina Faso<br>
            <span style="font-size:0.9rem;opacity:0.8;">
                Développé dans le cadre d'un stage — Université Thomas Sankara, 2026
            </span>
        </div>
    </div>
    """, unsafe_allow_html=True)


# ================================================
# COEFFICIENTS DU MODÈLE
# Régression logistique avec plan de sondage (svyglm)
# EDS Burkina Faso 2021 — N = 5 655 enfants
# Variables significatives uniquement (p < 0,05)
# Note : mere_malnutrie et region exclus du calcul
#   (non significatifs, p > 0,05) → présentés en contexte
# ================================================
COEF = {
    "intercept"              : -1.623330,
    "sexe_feminin"           : -0.311461,
    "milieu_rural"           :  0.371277,
    "instruction_primaire"   : -0.341359,
    "instruction_secondaire" : -0.131170,
    "richesse_pauvre"        :  0.005990,
    "richesse_moyen"         : -0.224319,
    "richesse_riche"         : -0.227450,
    "richesse_tres_riche"    : -0.486683,
    "age_enfant"             :  0.005502,
    "jumeau"                 :  0.979918,
    "intervalle_premier_ne"  :  0.265266,
    "intervalle_24_35"       :  0.345255,
    "intervalle_moins_24"    :  0.451071,
}

REGIONS = [
    "Boucle du Mouhoun",
    "Cascades", "Centre", "Centre-Est", "Centre-Nord",
    "Centre-Ouest", "Centre-Sud", "Est", "Hauts-Bassins",
    "Nord", "Plateau-Central", "Sahel", "Sud-Ouest",
]


# ================================================
# CALCUL DE PROBABILITÉ
# ================================================
def calculer_proba(sexe, age, milieu, instruction,
                   richesse, jumeau, intervalle):
    logit = COEF["intercept"]

    if sexe == "Féminin":
        logit += COEF["sexe_feminin"]
    if milieu == "Rural":
        logit += COEF["milieu_rural"]
    if instruction == "Primaire":
        logit += COEF["instruction_primaire"]
    elif instruction == "Secondaire ou plus":
        logit += COEF["instruction_secondaire"]

    map_richesse = {
        "Pauvre"     : COEF["richesse_pauvre"],
        "Moyen"      : COEF["richesse_moyen"],
        "Riche"      : COEF["richesse_riche"],
        "Très riche" : COEF["richesse_tres_riche"],
    }
    logit += map_richesse.get(richesse, 0.0)
    logit += COEF["age_enfant"] * age

    if jumeau:
        logit += COEF["jumeau"]

    map_int = {
        "Premier-né"       : COEF["intervalle_premier_ne"],
        "24-35 mois"       : COEF["intervalle_24_35"],
        "Moins de 24 mois" : COEF["intervalle_moins_24"],
    }
    logit += map_int.get(intervalle, 0.0)

    return 1.0 / (1.0 + math.exp(-logit))


# ================================================
# PAGE 1 — ACCUEIL
# ================================================
def page_accueil():
    entete()

    st.markdown("""
    <p style="text-align:center;color:#555;font-size:1.05rem;margin-bottom:28px;">
        Sélectionnez le type d'évaluation à réaliser
    </p>
    """, unsafe_allow_html=True)

    c1, c2 = st.columns(2, gap="large")

    with c1:
        st.markdown("""
        <div class="btn-accueil-vert">
            🔍 Dépistage postnatal
            <span class="btn-sous">
                Évaluer le risque actuel de retard de croissance<br>
                chez un enfant déjà né (0 à 59 mois)
            </span>
        </div>
        """, unsafe_allow_html=True)
        st.markdown("<div style='height:10px'></div>", unsafe_allow_html=True)
        if st.button("Accéder au dépistage →",
                     use_container_width=True, key="btn_post",
                     type="primary"):
            st.session_state.mode = "postnatal"
            aller_a("formulaire")

    with c2:
        st.markdown("""
        <div class="btn-accueil-orange">
            🤰 Prédiction prénatale
            <span class="btn-sous">
                Estimer le risque de retard de croissance<br>
                pour l'enfant à naître, avant la naissance
            </span>
        </div>
        """, unsafe_allow_html=True)
        st.markdown("<div style='height:10px'></div>", unsafe_allow_html=True)
        if st.button("Accéder à la prédiction →",
                     use_container_width=True, key="btn_pre"):
            st.session_state.mode = "prenatal"
            aller_a("formulaire")

    st.divider()
    st.markdown("""
    <div style="text-align:center;font-size:0.82rem;color:#888;margin-top:10px;">
        ℹ️ Cet outil est destiné aux professionnels de santé. 
        Il ne remplace pas la mesure anthropométrique (HAZ).<br>
        Basé sur le modèle logistique EDS Burkina Faso 2021 — 
        AUC = 0,641 | Hosmer-Lemeshow p = 0,532
    </div>
    """, unsafe_allow_html=True)


# ================================================
# PAGE 2 — FORMULAIRE
# ================================================
def page_formulaire():
    entete()

    mode = st.session_state.mode
    couleur_sec = "sec-vert" if mode == "postnatal" else "sec-orange"
    titre_page  = ("🔍 Dépistage postnatal"
                   if mode == "postnatal"
                   else "🤰 Prédiction prénatale")

    # Fil d'Ariane + Retour
    col_titre, col_retour = st.columns([5, 1])
    with col_titre:
        st.markdown(f"#### {titre_page}")
    with col_retour:
        if st.button("← Accueil", key="retour_form"):
            aller_a("accueil")

    st.divider()

    # -----------------------------------------------
    # FORMULAIRE POSTNATAL
    # -----------------------------------------------
    if mode == "postnatal":

        with st.form("form_postnatal"):

            st.markdown('<div class="sec-vert">👶 Caractéristiques de l\'enfant</div>',
                        unsafe_allow_html=True)
            c1, c2 = st.columns(2)
            with c1:
                sexe = st.radio("Sexe", ["Masculin", "Féminin"],
                                horizontal=True)
                jumeau = st.checkbox("Naissance multiple (jumeau ou plus)")
            with c2:
                age = st.slider("Âge en mois", 0, 59, 24,
                                help="Âge de l'enfant en mois")

            st.markdown('<div class="sec-vert">👩 Caractéristiques de la mère</div>',
                        unsafe_allow_html=True)
            c1, c2 = st.columns(2)
            with c1:
                instruction = st.selectbox(
                    "Niveau d'instruction",
                    ["Aucune", "Primaire", "Secondaire ou plus"]
                )
            with c2:
                intervalle = st.selectbox(
                    "Intervalle depuis la naissance précédente",
                    ["36 mois et plus (référence)",
                     "24-35 mois",
                     "Moins de 24 mois",
                     "Premier-né (aucune naissance précédente)"]
                )

            st.markdown('<div class="sec-vert">🏠 Caractéristiques du ménage</div>',
                        unsafe_allow_html=True)
            c1, c2, c3 = st.columns(3)
            with c1:
                milieu = st.radio("Milieu", ["Urbain", "Rural"],
                                  horizontal=True)
            with c2:
                richesse = st.selectbox(
                    "Quintile de richesse",
                    ["Très pauvre", "Pauvre", "Moyen",
                     "Riche", "Très riche"]
                )
            with c3:
                pass  # espace

            # Variables contextuelles (n'entrent pas dans le calcul)
            with st.expander("🔍 Informations contextuelles (facultatif)"):
                st.caption(
                    "Ces informations enrichissent le dossier de l'enfant "
                    "mais ne modifient pas le calcul de la probabilité."
                )
                c1, c2 = st.columns(2)
                with c1:
                    region = st.selectbox("Région administrative", REGIONS)
                    mere_malnutrie = st.checkbox(
                        "Mère malnutrie (IMC < 18,5 kg/m²)"
                    )
                with c2:
                    eau = st.selectbox(
                        "Accès à l'eau potable",
                        ["Améliorée (robinet, forage protégé…)",
                         "Non améliorée (puits non protégé, surface…)"]
                    )
                    nb_enf = st.selectbox(
                        "Nb enfants < 5 ans dans le ménage",
                        ["1 enfant", "2 enfants",
                         "3 enfants et plus"]
                    )
                c1, c2 = st.columns(2)
                with c1:
                    act_mere = st.selectbox(
                        "Activité économique de la mère",
                        ["Agriculture", "Commerce",
                         "Autres activités", "Sans activité"]
                    )
                with c2:
                    rang = st.selectbox(
                        "Rang de naissance",
                        ["Rang 1-2", "Rang 3-4",
                         "Rang 5 et plus"]
                    )

            soumis = st.form_submit_button(
                "📊 Calculer le risque →",
                use_container_width=True,
                type="primary"
            )

        if soumis:
            map_int = {
                "36 mois et plus (référence)"               : "36 mois et plus",
                "24-35 mois"                                : "24-35 mois",
                "Moins de 24 mois"                          : "Moins de 24 mois",
                "Premier-né (aucune naissance précédente)"  : "Premier-né",
            }
            intervalle_c = map_int.get(intervalle, "36 mois et plus")

            proba = calculer_proba(
                sexe, age, milieu, instruction,
                richesse, jumeau, intervalle_c
            )

            st.session_state.resultats = {
                "proba"         : proba,
                "sexe"          : sexe,
                "age"           : age,
                "milieu"        : milieu,
                "instruction"   : instruction,
                "richesse"      : richesse,
                "jumeau"        : jumeau,
                "intervalle"    : intervalle_c,
                # contextuelles
                "region"        : region,
                "mere_malnutrie": mere_malnutrie,
                "eau"           : eau,
                "nb_enf"        : nb_enf,
                "act_mere"      : act_mere,
                "rang"          : rang,
            }
            aller_a("resultats")

    # -----------------------------------------------
    # FORMULAIRE PRÉNATAL
    # -----------------------------------------------
    else:

        st.markdown("""
        <div class="avert-pre">
            ⚠️ <strong>Note méthodologique :</strong>
            Cet outil prédit le risque de retard de croissance pour un enfant
            à naître en utilisant les variables du modèle logistique connues
            avant la naissance. L'âge de l'enfant (inconnu) est fixé à 0 mois.
            La précision est par nature inférieure au dépistage postnatal.
        </div>
        """, unsafe_allow_html=True)

        # Confirmation obligatoire avant affichage du formulaire
        confirmee = st.checkbox(
            "✅ Je confirme être en présence d'une femme enceinte "
            "ou souhaitant évaluer le risque pour une prochaine grossesse",
            key="confirm_grossesse"
        )

        if not confirmee:
            st.info(
                "👆 Veuillez cocher la case ci-dessus pour accéder "
                "au formulaire de prédiction prénatale."
            )
            return

        with st.form("form_prenatal"):

            st.markdown('<div class="sec-orange">🤰 Informations sur la grossesse</div>',
                        unsafe_allow_html=True)
            c1, c2 = st.columns(2)
            with c1:
                jumeau_p = st.checkbox(
                    "Grossesse multiple attendue (jumeaux)",
                    help="Cochez si l'échographie a confirmé une grossesse multiple"
                )
            with c2:
                intervalle_choix = st.selectbox(
                    "Intervalle depuis la dernière naissance",
                    ["Première grossesse (aucune naissance précédente)",
                     "36 mois et plus (favorable)",
                     "24-35 mois",
                     "Moins de 24 mois (court — risque accru)"],
                    help=(
                        "Sélectionnez 'Première grossesse' si la femme "
                        "n'a jamais accouché auparavant."
                    )
                )
            cpn = st.selectbox(
                "Nombre de consultations prénatales (CPN) effectuées",
                ["Aucune CPN", "1 à 3 CPN",
                 "4 CPN et plus (recommandation OMS)"],
                help="Information contextuelle — ne modifie pas le calcul"
            )

            st.markdown('<div class="sec-orange">👩 Caractéristiques de la mère</div>',
                        unsafe_allow_html=True)
            c1, c2, c3 = st.columns(3)
            with c1:
                instruction_p = st.selectbox(
                    "Niveau d'instruction",
                    ["Aucune", "Primaire", "Secondaire ou plus"],
                    key="instr_pre"
                )
            with c2:
                age_mere = st.selectbox(
                    "Tranche d'âge de la mère",
                    ["Moins de 20 ans", "20-34 ans", "35 ans et plus"],
                    index=1,
                    help="Information contextuelle — ne modifie pas le calcul"
                )
            with c3:
                mere_malnutrie_p = st.checkbox(
                    "Mère malnutrie (IMC < 18,5 kg/m²)",
                    key="mal_pre",
                    help=(
                        "Information contextuelle — "
                        "non intégré au calcul de probabilité"
                    )
                )

            st.markdown('<div class="sec-orange">🏠 Caractéristiques du ménage</div>',
                        unsafe_allow_html=True)
            c1, c2, c3 = st.columns(3)
            with c1:
                milieu_p = st.radio("Milieu", ["Urbain", "Rural"],
                                    horizontal=True, key="mil_pre")
            with c2:
                richesse_p = st.selectbox(
                    "Quintile de richesse",
                    ["Très pauvre", "Pauvre", "Moyen",
                     "Riche", "Très riche"],
                    key="ric_pre"
                )
            with c3:
                region_p = st.selectbox(
                    "Région administrative",
                    REGIONS, key="reg_pre",
                    help="Information contextuelle — ne modifie pas le calcul"
                )

            eau_p = st.selectbox(
                "Accès à l'eau potable",
                ["Améliorée (robinet, forage protégé…)",
                 "Non améliorée (puits non protégé, eau de surface…)"],
                key="eau_pre",
                help="Information contextuelle — ne modifie pas le calcul"
            )

            soumis_p = st.form_submit_button(
                "🔮 Prédire le risque prénatal →",
                use_container_width=True,
                type="primary"
            )

        if soumis_p:
            map_int_p = {
                "Première grossesse (aucune naissance précédente)" : "Premier-né",
                "36 mois et plus (favorable)"                      : "36 mois et plus",
                "24-35 mois"                                       : "24-35 mois",
                "Moins de 24 mois (court — risque accru)"          : "Moins de 24 mois",
            }
            intervalle_pc = map_int_p.get(
                intervalle_choix, "36 mois et plus"
            )

            # Sexe inconnu → référence Masculin | Âge = 0
            proba_p = calculer_proba(
                "Masculin", 0,
                milieu_p, instruction_p, richesse_p,
                jumeau_p, intervalle_pc
            )

            st.session_state.resultats = {
                "proba"            : proba_p,
                "sexe"             : "Inconnu (référence : Masculin)",
                "age"              : "Non applicable",
                "milieu"           : milieu_p,
                "instruction"      : instruction_p,
                "richesse"         : richesse_p,
                "jumeau"           : jumeau_p,
                "intervalle"       : intervalle_pc,
                "region"           : region_p,
                "mere_malnutrie"   : mere_malnutrie_p,
                "eau"              : eau_p,
                "cpn"              : cpn,
                "age_mere"         : age_mere,
            }
            aller_a("resultats")


# ================================================
# PAGE 3 — RÉSULTATS
# ================================================
def page_resultats():
    mode = st.session_state.mode
    d    = st.session_state.resultats
    if not d:
        aller_a("accueil")
        return

    # En-tête (visible aussi à l'impression)
    entete()

    proba = d["proba"]
    pct   = proba * 100

    # Couleur et niveau
    if pct < 15:
        couleur, niveau, icone = "#1E8449", "RISQUE FAIBLE",  "🟢"
    elif pct < 25:
        couleur, niveau, icone = "#D4AC0D", "RISQUE MODÉRÉ", "🟠"
    else:
        couleur, niveau, icone = "#C0392B", "RISQUE ÉLEVÉ",  "🔴"

    # Bouton retour (masqué à l'impression)
    st.markdown('<div class="no-print">', unsafe_allow_html=True)
    cr, ct = st.columns([1, 6])
    with cr:
        if st.button("← Modifier", key="retour_res"):
            aller_a("formulaire")
    with ct:
        titre = ("📋 Résultat du dépistage postnatal"
                 if mode == "postnatal"
                 else "📋 Résultat de la prédiction prénatale")
        st.markdown(f"#### {titre}")
    st.markdown('</div>', unsafe_allow_html=True)

    st.divider()

    # ---- PROBABILITÉ ----
    col_prob, col_rec = st.columns([1, 1], gap="large")

    with col_prob:
        st.markdown(f"""
        <div style="text-align:center;padding:20px;background:#FAFAFA;
             border-radius:14px;border:1px solid #E5E8E8;">
            <div style="font-size:0.9rem;color:#777;margin-bottom:4px;">
                Probabilité estimée de retard de croissance
            </div>
            <div class="proba-display" style="color:{couleur};">{pct:.1f} %</div>
            <div class="niveau-display" style="color:{couleur};">{icone} {niveau}</div>
        </div>
        """, unsafe_allow_html=True)

        st.progress(min(pct / 100, 1.0))

        st.caption(
            f"Prévalence nationale de référence (EDS 2021) : **22,6 %**  "
            f"|  Seuil modéré : 15 %  |  Seuil élevé : 25 %  \n"
            f"*Prévalence estimée sur le sous-échantillon analytique "
            f"de cette étude : 21,27 %*"
        )

    with col_rec:
        if mode == "postnatal":
            if pct < 15:
                st.markdown("""
                <div class="risque-faible">
                    <strong>✅ Risque faible</strong><br><br>
                    • Maintenir les bonnes pratiques nutritionnelles<br>
                    • Poursuivre le suivi de croissance au calendrier habituel<br>
                    • Encourager l'allaitement et la diversification alimentaire
                </div>""", unsafe_allow_html=True)
            elif pct < 25:
                st.markdown("""
                <div class="risque-modere">
                    <strong>⚠️ Risque modéré</strong><br><br>
                    • Renforcer le suivi nutritionnel et l'éducation de la mère<br>
                    • Vérifier la diversité alimentaire et les pratiques d'allaitement<br>
                    • Prévoir un contrôle anthropométrique dans 4 semaines<br>
                    • Sensibiliser sur les pratiques ANJE
                </div>""", unsafe_allow_html=True)
            else:
                st.markdown("""
                <div class="risque-eleve">
                    <strong>🚨 Risque élevé — Action requise</strong><br><br>
                    • Orienter vers une consultation nutritionnelle spécialisée<br>
                    • Procéder à la mesure HAZ immédiate si disponible<br>
                    • Notifier le responsable de la formation sanitaire<br>
                    • Mettre en place un suivi rapproché (toutes les 2 semaines)
                </div>""", unsafe_allow_html=True)

        else:  # mode == "prenatal"
            if pct < 15:
                st.markdown("""
                <div class="risque-faible">
                    <strong>✅ Risque prénatal faible</strong><br><br>
                    • Maintenir un suivi prénatal régulier (4 CPN minimum)<br>
                    • Veiller à une alimentation variée et équilibrée<br>
                      pendant la grossesse<br>
                    • Poursuivre la supplémentation en fer et acide folique<br>
                    • Préparer les conditions d'un allaitement exclusif<br>
                      dès la naissance
                </div>""", unsafe_allow_html=True)
            elif pct < 25:
                st.markdown("""
                <div class="risque-modere">
                    <strong>⚠️ Risque prénatal modéré</strong><br><br>
                    • Intensifier le suivi prénatal et l'éducation nutritionnelle<br>
                      de la mère lors de chaque CPN<br>
                    • Assurer une supplémentation systématique en fer,
                      acide folique et micronutriments multiples<br>
                    • Surveiller la prise de poids maternelle<br>
                    • Planifier un suivi nutritionnel postnatal renforcé<br>
                      dès la naissance de l'enfant
                </div>""", unsafe_allow_html=True)
            else:
                st.markdown("""
                <div class="risque-eleve">
                    <strong>🚨 Risque prénatal élevé — Intervention urgente</strong><br><br>
                    • Référer la mère à une consultation prénatale spécialisée<br>
                      en nutrition maternelle<br>
                    • Mettre en place une supplémentation nutritionnelle renforcée<br>
                    • Prévoir un suivi postnatal rapproché dès la naissance<br>
                      (mesure HAZ à 0, 3 et 6 mois)<br>
                    • Notifier le responsable de la formation sanitaire<br>
                      pour une prise en charge coordonnée
                </div>""", unsafe_allow_html=True)

    st.divider()

    # ---- FACTEURS ----
    st.markdown("#### 🔎 Facteurs identifiés")
    col_r, col_p = st.columns(2)

    facteurs_risque    = []
    facteurs_protec    = []

    sexe_v = d.get("sexe", "")
    if mode == "postnatal":
        if "Masculin" in sexe_v:
            facteurs_risque.append(("Sexe masculin", "OR = 1,37 vs les filles"))
        elif "Féminin" in sexe_v:
            facteurs_protec.append(("Sexe féminin", "OR = 0,73 — facteur protecteur"))

    if d.get("jumeau"):
        facteurs_risque.append(("Naissance multiple (jumeau)",
                                 "OR = 2,66 — facteur le plus puissant du modèle"))
    else:
        facteurs_protec.append(("Naissance unique (singleton)",
                                 "Risque bien inférieur aux naissances multiples"))

    int_v = d.get("intervalle", "")
    if int_v == "Moins de 24 mois":
        facteurs_risque.append(("Intervalle inter-génésique < 24 mois",
                                 "OR = 1,57"))
    elif int_v == "24-35 mois":
        facteurs_risque.append(("Intervalle inter-génésique 24-35 mois",
                                 "OR = 1,41"))
    elif int_v == "Premier-né":
        facteurs_risque.append(("Premier-né", "OR = 1,30"))
    else:
        facteurs_protec.append(("Intervalle ≥ 36 mois",
                                 "Groupe de référence — risque le plus faible"))

    if d.get("milieu") == "Rural":
        facteurs_risque.append(("Milieu rural",
                                 "OR = 1,45 vs milieu urbain"))
    else:
        facteurs_protec.append(("Milieu urbain",
                                 "Prévalence 13,8 % vs 24,3 % en rural"))

    if d.get("richesse") in ["Très pauvre", "Pauvre"]:
        facteurs_risque.append(
            (f"Ménage {d['richesse'].lower()}",
             "Risque élevé par rapport au quintile très riche"))
    elif d.get("richesse") == "Très riche":
        facteurs_protec.append(("Ménage très riche",
                                 "OR = 0,62 — facteur protecteur fort"))

    if d.get("instruction") == "Aucune":
        facteurs_risque.append(("Mère sans instruction",
                                 "Groupe de référence — risque le plus élevé"))
    else:
        facteurs_protec.append(
            (f"Mère instruite ({d.get('instruction', '')})",
             "OR = 0,71 pour le niveau primaire"))

    age_v = d.get("age")
    if mode == "postnatal" and isinstance(age_v, int) and age_v >= 24:
        facteurs_risque.append((f"Âge : {age_v} mois",
                                "Le risque s'accumule entre 24 et 47 mois"))

    with col_r:
        st.markdown("**🔴 Facteurs de risque**")
        if facteurs_risque:
            for label, detail in facteurs_risque:
                st.markdown(
                    f'<div class="facteur-r">⚠ <strong>{label}</strong>'
                    f'<br><span style="font-size:0.82rem;">{detail}</span></div>',
                    unsafe_allow_html=True
                )
        else:
            st.success("Aucun facteur de risque majeur identifié")

    with col_p:
        st.markdown("**🟢 Facteurs protecteurs**")
        if facteurs_protec:
            for label, detail in facteurs_protec:
                st.markdown(
                    f'<div class="facteur-p">✓ <strong>{label}</strong>'
                    f'<br><span style="font-size:0.82rem;">{detail}</span></div>',
                    unsafe_allow_html=True
                )
        else:
            st.warning("Aucun facteur protecteur majeur identifié")

    st.divider()

    # ---- INFORMATIONS CONTEXTUELLES ----
    st.markdown("#### 📋 Informations contextuelles")
    ctx_items = []

    if d.get("region"):
        ctx_items.append(f"<strong>Région :</strong> {d['region']}")
    if d.get("mere_malnutrie") is not None:
        ctx_items.append(
            f"<strong>Mère malnutrie (IMC &lt; 18,5) :</strong> "
            f"{'Oui' if d['mere_malnutrie'] else 'Non'}"
        )
    if d.get("eau"):
        ctx_items.append(f"<strong>Eau potable :</strong> {d['eau']}")
    if d.get("nb_enf"):
        ctx_items.append(f"<strong>Enfants &lt; 5 ans :</strong> {d['nb_enf']}")
    if d.get("act_mere"):
        ctx_items.append(f"<strong>Activité mère :</strong> {d['act_mere']}")
    if d.get("rang"):
        ctx_items.append(f"<strong>Rang de naissance :</strong> {d['rang']}")
    if d.get("cpn"):
        ctx_items.append(f"<strong>CPN effectuées :</strong> {d['cpn']}")
    if d.get("age_mere"):
        ctx_items.append(f"<strong>Âge de la mère :</strong> {d['age_mere']}")

    if ctx_items:
        st.markdown(
            '<div class="contexte-box">'
            + "<br>".join(ctx_items)
            + "<br><br><em>Ces variables ne sont pas intégrées au calcul "
              "de la probabilité (non significatives dans le modèle "
              "ou informations contextuelles uniquement).</em>"
            + "</div>",
            unsafe_allow_html=True
        )
    else:
        st.caption("Aucune information contextuelle saisie.")

    # ---- RECOMMANDATIONS SPÉCIFIQUES PRÉNATALES ----
    if mode == "prenatal":
        st.markdown("#### 📌 Recommandations prénatales spécifiques")
        recos = []
        if d.get("intervalle") == "Moins de 24 mois":
            recos.append("Intervalle court — renforcer la planification familiale "
                         "et le suivi nutritionnel de la mère.")
        if d.get("mere_malnutrie"):
            recos.append("Supplémenter la mère en fer, acide folique "
                         "et micronutriments multiples dès maintenant.")
        if d.get("instruction") == "Aucune":
            recos.append("Renforcer l'éducation nutritionnelle lors de chaque CPN.")
        if d.get("cpn") in ["Aucune CPN", "1 à 3 CPN"]:
            recos.append("Encourager le recours aux CPN — objectif : "
                         "4 visites minimum selon les recommandations OMS.")
        if d.get("milieu") == "Rural":
            recos.append("Orienter vers la formation sanitaire la plus proche "
                         "pour le suivi prénatal.")
        if d.get("jumeau"):
            recos.append("Grossesse multiple — suivi postnatal renforcé "
                         "dès la naissance (risque de stunting très élevé).")
        if d.get("region") == "Sahel":
            recos.append("Zone à haute prévalence — assurer un suivi postnatal "
                         "rapproché dès la naissance.")
        if not recos:
            recos.append("Maintenir le suivi prénatal habituel "
                         "et les bonnes pratiques nutritionnelles.")

        for i, r in enumerate(recos, 1):
            st.markdown(f"**{i}.** {r}")

    # ---- NOTE DE BAS ----
    if mode == "postnatal":
        note_modele = (
            "Modèle postnatal — Variables significatives : sexe, âge, milieu, "
            "instruction maternelle, quintile de richesse, gémellité, "
            "intervalle inter-génésique."
        )
    else:
        note_modele = (
            "Modèle prénatal — Sous-ensemble des variables significatives du "
            "modèle postnatal, limitées aux informations disponibles avant la "
            "naissance (instruction, milieu, richesse, gémellité, intervalle). "
            "Âge de l'enfant fixé à 0 mois (inconnu)."
        )

    st.markdown(f"""
    <div class="footer-note">
        ℹ️ <strong>Outil d'aide à la décision — ne remplace pas la mesure
        anthropométrique (HAZ).</strong><br>
        {note_modele}<br>
        Source : Régression logistique avec plan de sondage (svyglm) —
        EDS Burkina Faso 2021.<br>
        AUC = 0,641 | Hosmer-Lemeshow p = 0,532 | N = 5 655 enfants.<br>
        <em>Note : les variables région et statut nutritionnel de la mère
        figurent dans le modèle estimé mais ne sont pas statistiquement
        significatives (p &gt; 0,05) ; elles sont présentées à titre
        contextuel uniquement.</em>
    </div>
    """, unsafe_allow_html=True)

    # ---- IMPRESSION ----
    st.markdown('<div class="no-print">', unsafe_allow_html=True)
    st.markdown("---")
    st.info(
        "🖨️ **Imprimer ou enregistrer en PDF :** "
        "utilisez `Ctrl+P` dans votre navigateur "
        "et sélectionnez *Enregistrer en PDF* comme imprimante. "
        "Les boutons de navigation seront masqués à l'impression."
    )
    if st.button("← Effectuer un nouveau dépistage",
                 key="nouveau", use_container_width=True):
        st.session_state.resultats = {}
        aller_a("formulaire")
    st.markdown('</div>', unsafe_allow_html=True)


# ================================================
# ROUTEUR PRINCIPAL
# ================================================
page = st.session_state.page

if page == "accueil":
    page_accueil()
elif page == "formulaire":
    page_formulaire()
elif page == "resultats":
    page_resultats()
else:
    aller_a("accueil")

