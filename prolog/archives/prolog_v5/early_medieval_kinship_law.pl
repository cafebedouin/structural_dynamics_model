% ============================================================================
% CONSTRAINT STORY: early_medieval_kinship_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_early_medieval_kinship_law, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: early_medieval_kinship_law
 *   human_readable: Early Medieval Kinship Law as Coordination and Extraction
 *   domain: social/legal/institutional
 *
 * SUMMARY:
 *   Early medieval kinship law (6th–10th centuries) across Germanic successor
 *   kingdoms combines genuine coordination functions with asymmetric
 *   extraction. The constraint solved legitimate collective action problems —
 *   property inheritance clarity, lineage stability, prevention of civil war
 *   over succession — while simultaneously concentrating control over women,
 *   wealth, and inheritance in male household heads. The law exhibits all six
 *   DR types depending on observer position. Women under guardianship see a
 *   snare (complete incapacity, no exit). Younger sons see tangled rope
 *   (clear succession rules prevent chaos but concentrate property
 *   asymmetrically). Male household heads see rope (coordinating property and
 *   authority). The Church sees a scaffold emerging gradually through reform
 *   (consent requirements, dower rights, convent alternatives). By the 10th
 *   century, written law codes appear increasingly ceremonial (piton). A
 *   civilizational observer risks naturalizing the constraint as inherent to
 *   agricultural societies. The declining extractiveness over the interval
 *   reflects the gradual introduction of Church-backed reforms that improved
 *   women's and younger sons' exit options, although suppression remains high
 *   throughout (guardianship rules persist in modified form through the
 *   entire period).
 *
 * KEY AGENTS:
 *   - Male Household Heads: Primary beneficiary (institutional/arbitrage) — control property, inheritance, marriage alliances; flexible interpretation of law; high exit optionality
 *   - Women Under Guardianship: Primary victim (powerless/trapped) — complete legal incapacity; identity fused with household role; exit requires abandoning social protection
 *   - Younger Sons: Secondary victim (moderate/constrained) — constrained by primogeniture but have defined status pathways (military, ecclesiastical, negotiated support claims)
 *   - Aristocratic Lineages: Beneficiary collective (institutional/arbitrage) — kinship law protects lineage integrity and property consolidation against fragmentation
 *   - Church Institutional Reformers: Organized agents (organized/constrained) — gradually introduce consent, dower, and convent pathways; building parallel institutions with sunset logic
 *   - Barbarian Law Codifiers: Institutional actors (institutional/arbitrage) — maintain written codes; codes become increasingly performative by 10th century (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent features of pre-state societies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(early_medieval_kinship_law, 0.52).
domain_priors:suppression_score(early_medieval_kinship_law, 0.68).
domain_priors:theater_ratio(early_medieval_kinship_law, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(early_medieval_kinship_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(early_medieval_kinship_law, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(early_medieval_kinship_law, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(early_medieval_kinship_law, tangled_rope).
narrative_ontology:human_readable(early_medieval_kinship_law, "Early Medieval Kinship Law as Coordination and Extraction").
narrative_ontology:topic_domain(early_medieval_kinship_law, "social/legal/institutional").

domain_priors:requires_active_enforcement(early_medieval_kinship_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(early_medieval_kinship_law, male_household_heads).
narrative_ontology:constraint_beneficiary(early_medieval_kinship_law, aristocratic_lineages).
narrative_ontology:constraint_victim(early_medieval_kinship_law, women_under_guardianship).
narrative_ontology:constraint_victim(early_medieval_kinship_law, younger_sons).
narrative_ontology:constraint_victim(early_medieval_kinship_law, illegitimate_heirs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMAN UNDER GUARDIANSHIP (SNARE) — Complete legal incapacity. Cannot own property, cannot testify, cannot initiate divorce, cannot select marriage partner. No appeal mechanism exists; guardianship extends through marriage to the husband. Theater is low (enforcement is blunt and material), but suppression is maximal and extraction is severe. Exit is impossible without abandoning family protection and social identity.
constraint_indexing:constraint_classification(early_medieval_kinship_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: YOUNGER SON (TANGLED ROPE) — Constrained by primogeniture rules that concentrate inheritance but also enjoys genuine coordination benefits: clear succession prevents civil war, kinship law provides structural claims on elder brother's support in distress, monastic placement offers alternative status pathway. Extraction is real (elder captures disproportionate wealth) but not total — younger sons occupy defined social positions and have some recourse claims.
constraint_indexing:constraint_classification(early_medieval_kinship_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALE HOUSEHOLD HEAD (ROPE) — Primary beneficiary. Controls property, manages marriage alliances, makes inheritance decisions. Experiences kinship law as a coordination mechanism that clarifies his authority and protects his lineage integrity. Benefits from clear rules governing wives' property devolution and children's status. Net beneficiary with high exit optionality (can relocate, can exploit ambiguities in law, can negotiate with other households).
constraint_indexing:constraint_classification(early_medieval_kinship_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHURCH AS INSTITUTIONAL REFORMER (SCAFFOLD) — Organized agents (bishops, councils, monastic scholars) gradually introduced consent requirements for marriage, expanded women's property rights through dower law, and created alternative status pathways (convent entrance). These reforms show sunset logic: they build parallel institutions (ecclesiastical courts, monastic communities) that partially bypass the traditional household authority. Theater is moderate (doctrinal authority has performative content) but the constraint is genuinely being reformed, not just reified.
constraint_indexing:constraint_classification(early_medieval_kinship_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: BARBARIAN LAW CODE AS RITUAL (PITON) — By the 10th century, written kinship codes (Lex Salica, Lex Burgundionum) are increasingly ceremonial rather than functional. Actual social practice diverges from written law; enforcement depends on ad-hoc negotiation among lords rather than genuine application of codified rules. Theater is high (public recitation, symbolic authority) but functional constraint is degrading as feudal relationships supersede lineal kinship rules. The code persists through institutional inertia.
constraint_indexing:constraint_classification(early_medieval_kinship_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, paternal control of kinship rules appears as an inevitable feature of pre-literate societies lacking centralized enforcement: without written records and state capacity, kinship authority must reside in household heads to guarantee lineage clarity and property transmission. This perspective naturalizes the constraint as inherent to agricultural societies. However, the structural data contradicts the mountain classification — the church's reforms and the variance across Germanic codes reveal kinship law as contingent institutional arrangements, not natural law.
constraint_indexing:constraint_classification(early_medieval_kinship_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(early_medieval_kinship_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(early_medieval_kinship_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(early_medieval_kinship_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(early_medieval_kinship_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(early_medieval_kinship_law, TR),
    TR >= 0.70.

:- end_tests(early_medieval_kinship_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from women and younger sons in favor of male household heads and aristocratic lineages. The core extraction mechanisms are property concentration (women cannot own or inherit independently; primogeniture concentrates land in eldest sons) and authority asymmetry (women have no legal personality; younger sons have no inheritance claims). However, extraction is not maximal because genuine coordination functions exist: clear succession rules prevent civil war and lineage fragmentation, which would harm all parties including the constrained groups. The value declines from 0.65 to 0.52 across the interval as Church reforms expand women's property control and create alternative status pathways. Suppression (0.68): High. Material barriers to exit are severe: women cannot leave marriages or guardianship without abandoning all social support and becoming destitute; younger sons cannot claim inheritance or property without violating lineage rules; illegitimate heirs have no recourse. Enforcement is through household authority (blunt, material) and social exclusion (identity-level). However, suppression is not absolute because convent entrance and military service offer limited alternative pathways, and informal negotiation sometimes overrides written rules. Theater ratio (0.58): Moderate. Early codes (6th–7th century) have low theater: enforcement is direct and material. But by the 9th–10th century, theater rises as written law becomes increasingly disconnected from feudal practice — codes are ritually recited but enforcement depends on aristocratic negotiation rather than legal principle. The interval captures this transition.
 *
 * PERSPECTIVAL GAP:
 *   The woman under guardianship sees a snare (complete extraction, no coordination benefit, no exit). The male household head sees a rope (coordination benefit, authority clarity, property security). The younger son sees tangled rope (mixed coordination and extraction). The Church sees a scaffold (temporary constraint with sunset through gradual reform). The analytical natural law observer sees a mountain (inherent to societies without state capacity) — but this is a false summit contradicted by evidence of contingency and reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (male household heads, aristocratic lineages) experience low directionality (d ≈ 0.1–0.2) because they benefit and have arbitrage exit options. Victims (women, younger sons) experience high directionality (d ≈ 0.75–0.95) because they bear costs and have trapped/constrained exit options. The Church as organized reform agents experience moderate directionality (d ≈ 0.45) because they are constrained by existing authority but can intentionally lower extraction through institutional change. The synthetic directionality drives chi: beneficiaries experience negative or near-zero chi; victims experience high chi; reformers experience moderate chi.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA CONSTRAINT DECOMPOSITION: The constraint decomposes into two structurally distinct stories: (1) kinship_law_property_coordination — genuine coordination function managing property transmission and succession (ε ≈ 0.25, Rope), and (2) kinship_law_patriarchal_extraction — asymmetric control over women and inheritance (ε ≈ 0.65, Snare). The tangled rope classification at moderate-high ε (0.52) reflects these two mechanisms operating together. The mandatrophy is resolved by showing that the classification is robust: the constraint exhibits both genuine coordination (prevents lineage fragmentation, clarifies succession) and asymmetric extraction (concentrates property in male hands, removes women's legal personality) from the same structural data. The perspectival variance arises from different agents' structural positions, not from measurement ambiguity. Women see pure extraction (snare) because they receive zero coordination benefit while bearing full extraction cost. Male household heads see pure coordination (rope) because they receive both coordination benefit and extraction benefit. The Church's scaffold perspective is empirically supported — Church reforms (consent requirements, dower rights, convent pathways) are demonstrably creating alternative structures that reduce women's suppression over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_threshold_ambiguity,
    'Did early medieval consent requirements for marriage (introduced by the Church from the 6th century onward) represent genuine reform or performative legitimation of existing patriarchal practice?',
    'Historical analysis of marriage contracts, trial records, and annulment petitions; identification of cases where consent was actually invoked to dissolve unwanted marriages vs cases where consent requirement was merely ceremonial.',
    'If genuine: the church''s reforms are real scaffolding reducing women''s extraction; constraint should show declining suppression over time. If performative: consent is a theater gate without material impact; constraint remains snare from women''s perspective throughout the period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_threshold_ambiguity, empirical, 'Whether marriage consent requirements reduced patriarchal extraction or remained performative').

omega_variable(
    property_control_boundary,
    'To what degree did dower rights and widow succession create genuine property control opportunities for women vs remaining purely formal titles that husbands could effectively override?',
    'Charting actual property transactions, inheritance disputes, and widow autonomy across documented cases; distinguishing between nominal and effectual property control.',
    'If dower rights were genuine: women''s exit options improve from trapped to constrained; classification shifts from snare to tangled rope. If dower was formal cover: women remain trapped despite legal text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_control_boundary, empirical, 'Actual vs formal property control for women in early medieval law').

omega_variable(
    legitimacy_status_fluidity,
    'Were illegitimate heirs'' legal disabilities immutable or could they be overcome through Church recognition, aristocratic negotiation, or normalization within lineages?',
    'Genealogical and documentary evidence of illegitimate lineage members'' actual social standing, property control, and inheritance outcomes across generations.',
    'If rigid: illegitimate sons face permanent snare from property exclusion. If fluid: status is constrained rather than trapped; extraction is moderate not severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_status_fluidity, empirical, 'Fluidity vs rigidity of illegitimacy status in medieval kinship law').

omega_variable(
    regional_legal_variance,
    'Do regional variations in kinship codes (Salian law, Burgundian law, Ostrogothic edicts) represent structurally distinct constraints or surface variations on a uniform underlying mechanism?',
    'Comparative legal analysis of kinship provisions; identification of codes that genuinely reduce women''s suppression or expand younger sons'' exit options vs codes that merely reword the same restrictions.',
    'If structurally distinct: multiple constraint stories should be written (one per legal tradition). If uniform mechanism: single story adequately models the constraint across the region.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_legal_variance, empirical, 'Whether regional legal codes represent distinct or uniform constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(early_medieval_kinship_law, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emkl_tr_t0, early_medieval_kinship_law, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emkl_tr_t250, early_medieval_kinship_law, theater_ratio, 250, 0.48).
narrative_ontology:measurement(emkl_tr_t500, early_medieval_kinship_law, theater_ratio, 500, 0.58).
narrative_ontology:measurement(emkl_tr_t100, early_medieval_kinship_law, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(emkl_be_t0, early_medieval_kinship_law, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(emkl_be_t250, early_medieval_kinship_law, base_extractiveness, 250, 0.58).
narrative_ontology:measurement(emkl_be_t500, early_medieval_kinship_law, base_extractiveness, 500, 0.52).
narrative_ontology:measurement(emkl_be_t100, early_medieval_kinship_law, base_extractiveness, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(early_medieval_kinship_law, resource_allocation).
narrative_ontology:affects_constraint(early_medieval_kinship_law, feudal_succession_hierarchy).
narrative_ontology:affects_constraint(early_medieval_kinship_law, ecclesiastical_reform_pressure).
narrative_ontology:affects_constraint(early_medieval_kinship_law, women_property_rights_medieval).

% DUAL FORMULATION NOTE:
% Early medieval kinship law decomposes into property coordination (ε ≈ 0.25, Rope) and patriarchal extraction (ε ≈ 0.65, Snare). The tangled rope classification emerges from genuine coordination of inheritance alongside asymmetric concentration of control. The two mechanisms are structurally distinct but institutionally entangled — separating them would require counterfactual analysis of succession systems that managed property transmission without gendered control asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(early_medieval_kinship_law, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
