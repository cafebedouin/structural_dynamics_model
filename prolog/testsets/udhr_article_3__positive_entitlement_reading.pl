% ============================================================================
% CONSTRAINT STORY: udhr_article_3__positive_entitlement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__positive_entitlement_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_article_3__positive_entitlement_reading
 *   human_readable: UDHR Article 3: Positive Entitlement Reading (State Provision of Material Conditions)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   Article 3 of the Universal Declaration of Human Rights states 'Everyone
 *   has the right to life, liberty and security of person.' This constraint
 *   story instantiates the POSITIVE ENTITLEMENT READING: the interpretation
 *   that Article 3 obligates states to provide material conditions —
 *   healthcare, housing, food security, welfare payments — necessary for life
 *   and security. This is one of three structurally distinct readings of the
 *   same kernel. The positive entitlement reading emerged from post-colonial
 *   and socialist interpretations emphasizing state responsibility for basic
 *   welfare, materialized in the International Covenant on Economic, Social
 *   and Cultural Rights (ICESCR), and is central to social-democratic welfare
 *   states. The competing sibling readings — negative liberty reading
 *   (Article 3 merely prohibits state violence and deprivation) and
 *   procedural hybrid reading (Article 3 guarantees procedural access to
 *   protection, neutral about whether protection is positive provision or
 *   negative restraint) — are held by different constituencies and have
 *   different structural implications. This story models ONLY the positive
 *   entitlement reading as a clean, ε-invariant constraint. The 58%
 *   extractiveness reflects that state provision of welfare requires
 *   redistribution that burdens property holders and constrains expression
 *   (hate speech prohibition often justified by dignity/security grounds),
 *   while simultaneously providing genuine coordination function (pooling
 *   healthcare risk, universal housing preventing homelessness). The 65%
 *   suppression reflects that beneficiaries face bureaucratic gatekeeping,
 *   means-testing surveillance, work requirements, and dignity-stripping
 *   conditionality; while property holders face confiscatory taxation
 *   justified by international human rights obligation. The extraction
 *   accumulates over the 50-unit interval as welfare states deepen provision
 *   (extractiveness: 0.32 → 0.58), enforcement mechanisms intensify
 *   (suppression: 0.42 → 0.65), and monitoring bodies proliferate (theater:
 *   0.35 → 0.48). The measurement trajectory shows the positive entitlement
 *   reading becoming progressively more extractive and more suppressive —
 *   beneficiaries face more bureaucratic overhead, property holders face
 *   greater redistribution pressure, and the institutional machinery
 *   maintains legitimacy through reporting compliance rather than behavioral
 *   change.
 *
 * KEY AGENTS:
 *   - Vulnerable populations (destitute, homeless, chronically ill): Primary beneficiary of positive provision; faces maximum suppression via means-testing and behavioral conditionality (powerless/trapped)
 *   - Precariat (working poor, unemployed, chronically underemployed): Secondary beneficiary; constrained by eligibility requirements and stigma (moderate/constrained)
 *   - Welfare state apparatus (social agencies, healthcare bureaucracies, housing authorities): Institutional implementer; experiences dual mandate of provision and enforcement (institutional/constrained)
 *   - Property holders (business owners, affluent individuals, investors): Primary victim of redistributive taxation; exit available via capital flight and regulatory arbitrage (powerful/arbitrage)
 *   - Expression communities (speech-oriented groups, civil liberties coalitions): Secondary victim if Article 3 is interpreted to justify hate speech restrictions on dignity/security grounds (organized/mobile)
 *   - International human rights monitoring bodies (UN committees, regional courts, NGOs): Institutional overseer; maintains legitimacy through reporting rather than enforcement (institutional/arbitrage)
 *   - Analytical observer (constitutional scholar, political philosopher): Sees the positive reading as solving a genuine coordination problem around survival security (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, 0.58).
domain_priors:suppression_score(udhr_article_3__positive_entitlement_reading, 0.65).
domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(udhr_article_3__positive_entitlement_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__positive_entitlement_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__positive_entitlement_reading, "UDHR Article 3: Positive Entitlement Reading (State Provision of Material Conditions)").
narrative_ontology:topic_domain(udhr_article_3__positive_entitlement_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__positive_entitlement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__positive_entitlement_reading, 'c2b9087a-fe38-4df9-933c-736af957e63e').
narrative_ontology:cs_kernel_codification('c2b9087a-fe38-4df9-933c-736af957e63e', fixed_text).
narrative_ontology:cs_authority_grounding('c2b9087a-fe38-4df9-933c-736af957e63e', extraction).
narrative_ontology:cs_interpretation_layer_present('c2b9087a-fe38-4df9-933c-736af957e63e').
narrative_ontology:cs_reading_relation('c2b9087a-fe38-4df9-933c-736af957e63e', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2b9087a-fe38-4df9-933c-736af957e63e', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('c2b9087a-fe38-4df9-933c-736af957e63e', foundational, material_provision_necessary_for_life_security).
narrative_ontology:cs_axiom_status(material_provision_necessary_for_life_security, holdable).
narrative_ontology:cs_axiom_grounding('c2b9087a-fe38-4df9-933c-736af957e63e', material_provision_necessary_for_life_security, empirically_contingent).
narrative_ontology:cs_axiom('c2b9087a-fe38-4df9-933c-736af957e63e', foundational, state_capacity_obligates_redistribution).
narrative_ontology:cs_axiom_status(state_capacity_obligates_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('c2b9087a-fe38-4df9-933c-736af957e63e', state_capacity_obligates_redistribution, deontological).
narrative_ontology:cs_reference_frame('c2b9087a-fe38-4df9-933c-736af957e63e', universal_material_provision_framework).
narrative_ontology:cs_drift_state('c2b9087a-fe38-4df9-933c-736af957e63e', contemporary_post_pandemic_fiscal_constraint, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c2b9087a-fe38-4df9-933c-736af957e63e', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__positive_entitlement_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, marginalized_groups).
narrative_ontology:constraint_beneficiary(udhr_article_3__positive_entitlement_reading, welfare_beneficiaries).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, property_holders).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, expression_communities).
narrative_ontology:constraint_victim(udhr_article_3__positive_entitlement_reading, taxpaying_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESTITUTE (SNARE) — Person without housing, healthcare, or subsistence income faces the entitlement reading as both salvation and trap. They cannot exit a territorial state that has ratified Article 3; they are locked into the status of perpetual claimant. The state's enforcement machinery (means-testing, bureaucracy, shame rituals) becomes coercive surveillance. Experienced as pure extraction of dignity in exchange for survival provision. No alternative.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIAT (TANGLED ROPE) — Working poor, unemployed, chronically ill: benefit from guaranteed healthcare and housing floors but constrained by means-testing, work requirements, and stigma. The entitlement reading provides genuine coordination function (pooled healthcare risk, public housing preventing homelessness) AND asymmetric extraction (surveillance, bureaucratic overhead, behavioral conditioning through conditionality). Significant exit cost to migrate to low-welfare jurisdictions.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE STATE APPARATUS (TANGLED ROPE) — Government agencies charged with implementing Article 3 entitlements experience a dual mandate: genuine coordination (distributing healthcare and housing) AND coercive enforcement (benefit gatekeeping, fraud investigation, conditionality enforcement). The apparatus sees itself as solving coordination problems but observers see extraction machinery. Constrained by international human rights law and fiscal pressure; cannot easily exit from the Article 3 commitment.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PROPERTY AND EXPRESSION COMMUNITIES (SNARE) — Property holders, speech communities, commercial actors see Article 3's positive reading as enabling state wealth redistribution and hate speech restrictions justified by dignity and survival rights. They perceive extraction: taxation, property seizure for housing programs, speech regulation. However, organized communities (business federations, civil liberties unions) have mobile exit options (regulatory arbitrage, relocation, capital flight) and can mobilize countervailing power. The snare is not maximal because organization provides agency.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMAN RIGHTS MONITORING BODIES (PITON) — UN committees, regional courts, NGO monitors tasked with monitoring Article 3 compliance engage in substantial theater: issuing reports, scheduling reviews, naming violations. The functional monitoring power is limited (enforcement relies on state cooperation); the institutional structure persists through legitimacy narratives rather than enforcement capacity. Arbitrage available: states can ignore findings or withdraw from protocols. Theater ratio high; effective enforcement low.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational view, Article 3's positive reading solves a genuine coordination problem: how do societies ensure survival security and basic healthcare for all members without creating a commons tragedy? The entitlement reading instantiates a coordination mechanism (pooled risk, universal entitlements). The extraction observed from other perspectives is experienced here as coordination cost. This perspective emphasizes the functional cooperation required to deliver benefits at scale.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: WEALTHY CONSTITUENCIES (ROPE) — Affluent agents experience Article 3's entitlement reading as a coordination mechanism: pooled public healthcare prevents catastrophic pandemics; subsidized housing prevents urban decay and crime; welfare floors prevent social collapse and political instability. They see the extraction (progressive taxation) as a cost of social stability and can arbitrage between high-tax and low-tax jurisdictions. Experienced as rope because the coordination function is genuine and the exit option is real.
constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__positive_entitlement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(udhr_article_3__positive_entitlement_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__positive_entitlement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(udhr_article_3__positive_entitlement_reading, TR),
    TR >= 0.70.

:- end_tests(udhr_article_3__positive_entitlement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58, rising to 0.58 by endpoint): The positive entitlement reading obligates states to provide material welfare, which requires significant wealth redistribution. The extractiveness is moderate-high because (1) genuine coordination function exists (healthcare pooling, housing security), (2) property redistribution is not total wealth destruction but sustainable transfer, and (3) vulnerable beneficiaries gain net benefit while property holders lose marginal income. Initial measurement (0.32) reflects early post-UDHR era when positive entitlements were aspirational and not enforced; modern implementation (0.58) reflects deepened welfare states where provision is systematic and funded by progressive taxation. Not higher (≥0.70) because the positive reading cannot impose total wealth destruction and maintain legitimacy; states must preserve property incentives or face capital flight. Not lower (≤0.40) because genuine extraction occurs: progressive taxation reaches effective marginal rates ≥40% in welfare states; property owners experience material loss even if not catastrophic. SUPPRESSION (0.65, rising from 0.42): The constraint's enforceability depends on bureaucratic machinery that functions coercively. Beneficiaries face means-testing surveillance (what income/assets disqualify you?), work requirements (you must participate in labor activation to receive benefits), and behavioral conditionality (benefits depend on compliance with parenting/health/education norms). This is genuine suppression: the alternative to compliance is destitution. Property holders face confiscatory taxation legitimized by international human rights obligation — tax evasion is framed as violation of human rights law, not mere financial crime. The suppression trajectory (0.42→0.65) reflects intensifying enforcement: early welfare states had weaker monitoring and higher non-compliance; modern welfare states deploy means-testing databases, cross-agency verification, and fraud investigation machinery. THEATER RATIO (0.48, rising from 0.35): The coordination function is genuine (healthcare and housing are actually provided), so theater is moderate rather than high. However, international monitoring bodies (UN committees, regional courts) maintain legitimacy through reporting compliance without strong enforcement capacity. States submit reports; committees issue findings; findings are non-binding unless domestic courts adopt them. The theater rises as monitoring proliferates but enforcement capacity does not. The positive reading generates less theater than the negative liberty reading would, because the positive reading obligates affirmative action (provide healthcare, build housing) that is either done or not done; there is less room for performative compliance than with negative obligations (refrain from violence, respect freedom).
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits dramatic perspectival divergence across power levels and exit options. The destitute person sees SNARE (pure extraction of dignity for survival provision; no exit; maximum suppression). The precariat sees TANGLED ROPE (genuine coordination function in healthcare pooling and housing security, but asymmetric extraction through bureaucratic overhead and work requirements; constrained exit). The welfare state apparatus sees TANGLED ROPE (implementing both a coordination function and a control mechanism; constrained by law and fiscal pressure). Organized property communities see SNARE (extraction via progressive taxation and potential hate speech restrictions; but mobile exit via capital arbitrage means the snare is not maximal). International monitors see PITON (maintaining institutional legitimacy through reporting rather than enforcement; high theater). The analytical observer sees ROPE (genuine coordination function around survival security; extraction is coordination cost). Wealthy arbitrageurs see ROPE (pool risk, avoid social instability, arbitrage between tax regimes). The perspectival gap is driven by: (1) structural position (beneficiary vs victim), (2) exit capacity (trapped vs arbitrage), and (3) whether the agent experiences the extraction as legitimate cost or illegitimate taking. The destitute cannot exit and do not control the frame ('survival provision' vs 'dignity extraction'), so they experience maximum extraction. Wealthy arbitrageurs can reframe ('social stability' vs 'wealth redistribution') and exit (capital flight), so they experience coordination. The same structural constraint — state-mandated welfare provision — appears as snare, tangled rope, piton, and rope depending on the observer's position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural relationships: beneficiary status, victim status, and exit options. Vulnerable populations are beneficiaries of welfare provision but lack exit options (trapped), yielding high d toward the constraint, high f(d), and high experienced extraction. Precariat are beneficiaries constrained by eligibility barriers (constrained exit), yielding moderate-high d. Property holders are victims with arbitrage exit (capital flight, regulatory arbitrage), yielding lower d than victims with constrained exit. The analytical observer is structurally symmetric (both benefits from social stability and contributes to it via intellectual legitimation), yielding d≈0.50. The positive entitlement reading creates an asymmetry: beneficiaries (vulnerable populations) are typically trapped or constrained; victims (property holders) are typically mobile or have arbitrage. This is the structural signature of a tangled rope that leans toward snare for the powerless and toward rope for the powerful. The direction of extraction is unambiguous: from property holders toward vulnerable populations, coordinated through state machinery that compresses both beneficiary and victim agency. The effective extraction (χ) is scaled not just by base extraction (ε) but by power and exit options: chi_destitute > chi_precariat > chi_property_holder, because f(d) is sigmoid with inflection around d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The positive entitlement reading is a genuine tangled rope, not a misclassified snare or rope. The mandatrophy is resolved by recognizing that Article 3's positive interpretation serves BOTH coordination and extraction functions simultaneously. Coordination function: pooling healthcare risk, universal housing preventing catastrophic poverty, welfare floors preventing social collapse. Extraction function: progressive taxation burdens property holders, means-testing surveillance burdens beneficiaries, behavioral conditionality restricts freedom. The classification avoids the mandatrophy by acknowledging both: the constraint is NOT pure coordination (rope) because genuine asymmetric extraction occurs; it is NOT pure extraction (snare) because genuine coordination function exists. The positive reading's higher extractiveness (vs. negative liberty reading at lower ε) reflects that positive entitlements require affirmative state action and resource transfer, not mere restraint. The beneficiary/victim structure confirms tangled rope: beneficiaries (vulnerable populations) exist as required by rope gate; victims (property holders, expression communities) exist as required for snare/tangled-rope gate. The requires_active_enforcement flag is true: states must actively implement welfare programs, not merely refrain from deprivation. This is why the positive entitlement reading is tangled rope, not rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_vs_negative_liberty_kernel,
    'Does Article 3 establish a positive entitlement to state provision, or does it merely prohibit state deprivation of life and security?',
    'Textual analysis of Article 3 (''the right to life, liberty and security of person'') against negotiating history, travaux préparatoires, and subsequent interpretation by courts and monitoring bodies. Examine: (a) whether state inaction constitutes violation; (b) whether minimum threshold for state provision is specified; (c) whether resource constraints excuse non-compliance.',
    'If positive entitlement: extractiveness ≥ 0.50 (states must actively redistribute), beneficiaries = vulnerable groups, victims = property/expression rights holders. If negative liberty only: extractiveness ≤ 0.30 (states merely refrain from deprivation), beneficiaries = broad population, victims = states violating the principle. This is the primary semantic fork between the positive entitlement reading (this file) and the negative liberty reading (sibling constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positive_vs_negative_liberty_kernel, conceptual, 'Whether Article 3 obligates positive state provision or only prohibits state deprivation').

omega_variable(
    resource_constraint_binding,
    'Does ''to the maximum of available resources'' qualification (implicit in Article 3 interpretation via Article 2) create an escape valve that prevents the positive entitlement reading from fully obligating redistribution?',
    'Comparison of state compliance patterns: nations with high GDP per capita vs low; examination of whether wealthy states consistently achieve higher Article 3 provision levels; analysis of whether ''maximum available resources'' is interpreted as genuine constraint or performative excuse.',
    'If ''resources'' is binding constraint: extractiveness drops to 0.42 (states have legitimate excuse for inadequate provision); the constraint becomes a ''best efforts'' coordination mechanism rather than hard obligation. If ''resources'' is performative excuse: extractiveness holds at 0.58+ (states redistribute selectively); the positive entitlement reading is enforceable and extractive for property holders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_constraint_binding, empirical, 'Whether resource constraints genuinely bind state Article 3 obligations').

omega_variable(
    enforcement_legitimacy_mechanism,
    'What legitimizes enforcement of Article 3 positive entitlements: democratic consent, human rights moral authority, or something else? Does enforcement mechanism itself constitute extraction?',
    'Survey of enforcement vectors: court-mandated welfare expansion, international pressure, public shaming, progressive taxation. Examine whether populations subject to enforcement perceive legitimacy vs coercion; whether enforcement mechanisms become extraction tools (means-testing surveillance, behavioral conditioning, fraud investigation).',
    'If enforcement is legitimized by democratic consent: suppression stays ≤ 0.50; the constraint is seen as enforceable without excessive coercion. If enforcement relies on international authority or moral claim alone: suppression rises to ≥ 0.65; populations see external imposition, and enforcement machinery becomes extraction mechanism. Theater ratio may rise if enforcement becomes performative (reporting compliance without behavioral change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_mechanism, preference, 'What legitimizes enforcement of Article 3 positive entitlements').

omega_variable(
    wealth_destruction_vs_redistribution,
    'Does Article 3 positive reading require wealth destruction (taxation for universal provision) or wealth redistribution (progressive taxation + public provision)? The difference determines whether property rights are the victim or merely constrained.',
    'Economic analysis of welfare expansion pathways: nations that expanded provision via growth (rising tide) vs those that expanded via redistribution (zero-sum). Measurement of deadweight loss in taxation and provision mechanisms.',
    'If redistribution (sustainable): extractiveness ≤ 0.48; property holders lose marginal income but keep core assets; constraint is tangled rope (coordination + extraction). If destruction (unsustainable): extractiveness ≥ 0.65; property rights are victims; constraint approaches snare. This affects how property holders are classified in base_properties.victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wealth_destruction_vs_redistribution, empirical, 'Whether Article 3 requires wealth destruction or redistribution').

omega_variable(
    identity_lock_welfare_bureaucracy,
    'Does prolonged welfare provision create identity lock for beneficiaries — internalized status as ''welfare recipient'' that persists after material exit conditions are met?',
    'Longitudinal study of welfare exits: comparison of employment and income trajectories for individuals who exit welfare vs continue; measurement of self-reported identity shifts post-exit; analysis of whether bureaucratic categorization persists as self-concept after material dependency ends.',
    'If identity lock occurs: exit_options for beneficiaries should be classified as identity_locked (not just constrained or mobile) even after material barriers disappear; the extraction mechanism becomes cognitive/internalized rather than structural. This affects perspectival gap: some beneficiaries see themselves as permanently dependent even when exit is materially possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_welfare_bureaucracy, empirical, 'Whether welfare provision creates identity lock in beneficiaries').

omega_variable(
    sibling_reading_foreclosure,
    'Does the positive entitlement reading logically foreclose the negative liberty reading within a single coherent human rights framework, or do they represent genuinely coexistent positions held by different constituencies?',
    'Jurisprudential analysis: examine whether courts, human rights bodies, and state constitutions hold both readings simultaneously or oscillate between them. Identify which framework elements (dignity, survival, freedom from state violence) ground each reading''s legitimacy claim.',
    'If foreclosed: this reading and the negative liberty reading are mutually exclusive; only one can be law in any jurisdiction at any moment. If coexistent: the readings coexist in the contested political space; different parties hold different readings; no single framework resolution exists. This determines cs_structure.reading_relations: whether ''forecloses'' vs ''coexists_with''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether positive entitlement reading logically forecloses negative liberty reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__positive_entitlement_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr3pe_theater_t0, udhr_article_3__positive_entitlement_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(udhr3pe_theater_t25, udhr_article_3__positive_entitlement_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(udhr3pe_theater_t50, udhr_article_3__positive_entitlement_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(udhr3pe_extractiveness_t0, udhr_article_3__positive_entitlement_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(udhr3pe_extractiveness_t25, udhr_article_3__positive_entitlement_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(udhr3pe_extractiveness_t50, udhr_article_3__positive_entitlement_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(udhr3pe_suppression_t0, udhr_article_3__positive_entitlement_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(udhr3pe_suppression_t25, udhr_article_3__positive_entitlement_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement(udhr3pe_suppression_t50, udhr_article_3__positive_entitlement_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__positive_entitlement_reading, resource_allocation).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, welfare_state_implementation_mechanisms).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, progressive_taxation_incidence).
narrative_ontology:affects_constraint(udhr_article_3__positive_entitlement_reading, hate_speech_restriction_justification).

% DUAL FORMULATION NOTE:
% Article 3 UDHR contains a contested kernel that decomposes into three structurally distinct constraint stories: positive_entitlement_reading (this file, ε=0.58), negative_liberty_reading (sibling, ε≤0.35), and procedural_hybrid_reading (sibling, ε≈0.40). Each reading has different beneficiaries, victims, and extractiveness values. The readings coexist in the contested political space; different constituencies hold different readings; no single framework resolution exists at the UDHR level. Individual state constitutions typically instantiate one reading or a hybrid. The network edges show the positive entitlement reading's structural effects: it creates pressure toward welfare state implementation (affects_constraints: welfare_state_implementation), justifies progressive taxation incidence analysis, and justifies hate speech restrictions on dignity/security grounds. The sibling readings would have different network effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__positive_entitlement_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
