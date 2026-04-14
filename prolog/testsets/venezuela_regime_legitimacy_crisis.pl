% ============================================================================
% CONSTRAINT STORY: venezuela_regime_legitimacy_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venezuela_regime_legitimacy_crisis, []).

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
 *   constraint_id: venezuela_regime_legitimacy_crisis
 *   human_readable: Venezuela Regime Legitimacy Crisis
 *   domain: political_economy/state_capacity
 *
 * SUMMARY:
 *   The Venezuelan regime legitimacy crisis represents a state-level
 *   extraction constraint where the regime's capacity to govern through
 *   democratic institutions or social contract has collapsed, replaced by a
 *   system sustained primarily through military coercion and suppression of
 *   alternatives. Beginning approximately 2013 with Hugo Chávez's death and
 *   accelerating through 2018-2020 under Nicolás Maduro, the constraint
 *   exhibits the full spectrum of DR classification depending on observer
 *   position. The regime maintains electoral theater (fraudulent but
 *   continuously held elections) while suppressing opposition, controlling
 *   food distribution, persecuting civil society, and imposing capital
 *   controls that prevent citizens from accessing resources or exiting. The
 *   military security apparatus benefits disproportionately through budget
 *   prioritization and smuggling opportunities while the general population
 *   experiences hyperinflation, medicine shortages, and violence. Over 7
 *   million have emigrated (20% of population), creating a transnational
 *   diaspora coordinating remittances and political opposition. International
 *   sanctions since 2017 create external pressure for democratic transition.
 *   The constraint's extractiveness has accumulated over the measurement
 *   interval (0.35 → 0.68) while theater ratio has increased (0.40 → 0.80),
 *   indicating that the regime is investing more in legitimacy performance
 *   while actual state capacity degrades — a classic piton pattern layered
 *   over an underlying snare structure.
 *
 * KEY AGENTS:
 *   - General Population: Primary victim (powerless/trapped) — 28 million citizens experiencing economic collapse, food insecurity, medical system failure, and violence with no exit mechanism
 *   - Military Security Apparatus: Primary beneficiary (institutional/arbitrage) — receives disproportionate budget allocation, smuggling opportunities, and institutional power in exchange for regime loyalty
 *   - Political Opposition: Secondary victim (moderate/constrained) — face imprisonment, harassment, electoral fraud, and exclusion from power despite significant electoral support
 *   - International Diaspora: Secondary beneficiary/victim (organized/mobile) — benefit from exit mobility but experience family separation, property seizure, and informal taxation through remittance controls
 *   - Regime Electoral Theater: Institutional constraint (institutional/constrained) — maintains performative democratic legitimacy through rigged elections; persists through inertia rather than function
 *   - International Community: External enforcer (organized/constrained) — coordinates sanctions pressure with implicit sunset clause upon democratic transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_regime_legitimacy_crisis, 0.68).
domain_priors:suppression_score(venezuela_regime_legitimacy_crisis, 0.75).
domain_priors:theater_ratio(venezuela_regime_legitimacy_crisis, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_regime_legitimacy_crisis, extractiveness, 0.68).
narrative_ontology:constraint_metric(venezuela_regime_legitimacy_crisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(venezuela_regime_legitimacy_crisis, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venezuela_regime_legitimacy_crisis, snare).
narrative_ontology:human_readable(venezuela_regime_legitimacy_crisis, "Venezuela Regime Legitimacy Crisis").
narrative_ontology:topic_domain(venezuela_regime_legitimacy_crisis, "political_economy/state_capacity").

domain_priors:requires_active_enforcement(venezuela_regime_legitimacy_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venezuela_regime_legitimacy_crisis, military_security_apparatus).
narrative_ontology:constraint_beneficiary(venezuela_regime_legitimacy_crisis, regime_inner_circle).
narrative_ontology:constraint_victim(venezuela_regime_legitimacy_crisis, general_population).
narrative_ontology:constraint_victim(venezuela_regime_legitimacy_crisis, political_opposition).
narrative_ontology:constraint_victim(venezuela_regime_legitimacy_crisis, civil_society).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL POPULATION (SNARE) — Trapped by economic collapse, capital controls, and absence of exit mechanisms. Citizens cannot leave without regime permission; cannot organize political alternatives without state violence; cannot access goods without participating in regime-controlled distribution. Suppression is total: hyperinflation, food scarcity, medical collapse, and violence eliminate alternatives. The constraint extracts not just material resources but all freedom of movement and political voice. Experienced as pure coercive extraction with zero coordination benefit.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL OPPOSITION (SNARE) — Constrained but not trapped — opposition parties can technically organize, but face severe costs: imprisonment, asset seizure, harassment, electoral fraud, and exclusion from power even if elected. Exit option exists (emigration, underground activism) but at extreme cost (family separation, loss of status, risk of violent suppression). Experiences extraction as coercive denial of legitimate power claims backed by periodic electoral support. No genuine coordination benefit; the state tolerates opposition only to maintain theater of legitimacy.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY SECURITY APPARATUS (TANGLED ROPE) — Primary beneficiary and active enforcer. Experiences genuine coordination: the regime requires military loyalty to maintain order and extract resources; the military requires state resources to maintain institutional capacity. This is mutually beneficial coordination at the institutional level. However, the constraint also exhibits extraction: the military's loyalty is purchased through disproportionate budget allocation, smuggling opportunities, and exemption from economic hardship that affects civilians. The military benefits from the constraint (resources, power, reduced competition from civilian institutions) while civilians bear the full cost. Coordination + asymmetric extraction = Tangled Rope. Exit option is arbitrage — the military could withdraw support and trigger regime change, but the opportunity cost of losing institutional position and resource flows is prohibitive.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME ELECTORAL THEATER (PITON) — The regime maintains elections and opposition participation despite widespread fraud and predetermined outcomes. Elections serve as legitimation ritual rather than mechanism for power transfer. The constraint persists through institutional inertia — the regime cannot simply abandon electoral facade without explicitly rejecting democratic pretense, which would trigger international isolation. Theater ratio is high (0.80): the performative legitimacy ritual requires ongoing cost and effort but produces no genuine power-transfer function. The original election mechanism has atrophied; only the theatrical performance remains. Classified as Piton because function has degraded while constraint persists through inertia.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL DIASPORA AND EXILE NETWORKS (TANGLED ROPE) — Over 7 million Venezuelans have emigrated (roughly 20% of population). Diaspora communities coordinate remittances, political organization, and advocacy. They have mobile exit options (many have obtained residency/citizenship elsewhere). However, they experience extraction: families left behind remain trapped; diaspora members face separation costs, loss of property, and social stigma as 'traitors' in regime discourse. The constraint coordinates diaspora survival (remittances sustain families) while extracting migration tax through family separation and property seizure. Beneficiary status is ambiguous — diaspora benefits from exit mobility but bears costs of separation. Victim status is clear for families left behind. Classified as Tangled Rope because the remittance coordination is genuine but layered with extraction through property seizure and informal taxation of diaspora resources.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL COMMUNITY AND SANCTIONS REGIME (SCAFFOLD) — Over 60 countries (US, EU, Canada, Australia, regional partners) have imposed targeted sanctions on regime figures and sectors. These sanctions coordinate international pressure for democratic transition with an implicit sunset clause: sanctions will lift upon credible democratic reforms. Low effective extraction because the sanctions regime has defined exit pathways (elections, power transfer, institutional reform) and enforcement is external rather than internalized. The constraint functions as temporary coordination to enforce pressure rather than permanent extraction. However, sanctions create collateral civilian harm (medical imports blocked, fuel scarcity) that complicate the scaffold classification. Primary structural function is transitional with defined sunset conditions.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, the constraint exhibits characteristics of a pure extraction regime sustained by violence and suppression with minimal coordination benefit beyond regime reproduction. The state's claim to coordinate national development or democratic governance has collapsed; the regime's only functional role is extracting resources from the population and directing them toward the security apparatus. No universalizable coordination function remains. The regime legitimacy constraint is a degraded political authority that persists through coercion rather than social contract. Theater ratio indicates that legitimacy claims (electoral fairness, constituent service, national development) are purely performative — the actual function is coercive extraction. This perspective identifies the constraint as pure snare, not tangled rope, because the military coordination is not fundamentally about national governance but about mutual survival of an extractive apparatus.
constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_regime_legitimacy_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venezuela_regime_legitimacy_crisis, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venezuela_regime_legitimacy_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venezuela_regime_legitimacy_crisis, TR),
    TR >= 0.70.

:- end_tests(venezuela_regime_legitimacy_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The regime extracts material resources from the population through inflation tax (currency debasement forces savings devaluation), capital controls (preventing capital flight), and direct confiscation through price controls and nationalization. More fundamentally, it extracts all political voice, mobility, and autonomy — citizens cannot exit, organize opposition, or access basic goods without state permission. The trajectory from 0.35 to 0.68 over 15 years reflects accumulating extraction as state capacity collapsed and the regime shifted from attempted governance to pure resource extraction. Suppression (0.75): Very high. Multiple suppression mechanisms operate simultaneously: (1) Material: food scarcity, medicine shortage, hyperinflation make survival dependent on regime distribution; (2) Legal: emigration restrictions, capital controls, asset freezes eliminate exit options; (3) Violent: military repression of protests, imprisonment of opposition, extrajudicial killings eliminate political alternatives; (4) Informational: state control of broadcast media, censorship of internet and social media, restriction of independent journalism prevent reality-testing. Theater ratio (0.80): Very high and increasing. Elections occur regularly (theater) but lack any power-transfer function (degraded capability). Opposition campaigns occur (theater) but face systematic fraud and suppression (degraded function). The regime claims to pursue democratic socialism, constituent power, and national development (theater) while presiding over state collapse (degraded function). The theater has intensified as actual legitimacy eroded — more elaborate staging of elections, more frequent speeches, more emphasis on ceremonial functions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across the observation positions. The general population sees pure extraction (Snare) with no coordination benefit — the state provides no services, no security, no development, only suppression. The military sees genuine coordination (Tangled Rope) — their institutional survival is mutually dependent on regime survival; they get resources and institutional power in exchange for suppressing opposition and maintaining order. The political opposition sees extraction denied through fraud (Snare) — they have electoral support but are systematically prevented from exercising power. The diaspora sees a mixed constraint (Tangled Rope) — they have escaped to safety (mobile exit) but carry their families and assets as hostages (constrained by family separation and property seizure). The international community sees a temporary problem with a sunset (Scaffold) — sanctions create pressure toward elections and power transfer with defined exit conditions. The regime's electoral theater sees its own degradation (Piton) — the performative legitimacy ritual persists through institutional inertia despite losing functional capacity. The analytical observer sees pure extraction (Snare) — the regime has no universalizable coordination function remaining; it exists only to extract resources for the security apparatus. The perspectival gap between the military (Tangled Rope — genuine mutual benefit) and the general population (Snare — pure extraction) is the critical diagnostic gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position: beneficiary status (does the constraint flow resources toward this agent or extract from them?) combined with exit options (can they escape without catastrophic cost?). The military has high beneficiary status (budget allocation, smuggling opportunities, institutional power) plus arbitrage exit option (could withdraw support and trigger regime change, but opportunity cost of losing position is prohibitive) → low d → negative f(d) → experiences extraction flowing TOWARD them (they are the net recipients). The general population has pure victim status (economic collapse, suppression, denial of services) plus trapped exit option (cannot leave) → high d → high f(d) → experiences maximum extraction flowing AWAY from them (they bear the full cost). The opposition has victim status (fraud, imprisonment, exclusion) plus constrained exit option (can emigrate at extreme cost) → high d but slightly lower than trapped → moderate-high f(d). The diaspora has ambiguous status: beneficiary (escaped to safety) but victim (family separation, property seizure) combined with mobile exit (already exited) → moderate d → moderate f(d) → experiences moderate extraction layered over escape. The international sanctioner has no beneficiary/victim status within Venezuela (external actor) but has analytical exit option → analytical d ≈ 0.73 → standard analytical extraction formula applies.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint would be the claim that the regime legitimacy crisis represents a Rope (pure coordination) or that the constraint is natural law (Mountain). The empirical structural data prevents both. The regime cannot be classified as pure coordination because there are explicit victims (general population experiencing hyperinflation, food shortage, medical collapse) and explicit beneficiaries (military security apparatus receiving disproportionate resources). The coordination function (maintaining order) exists but is not the constraint's primary purpose — the primary purpose is extracting resources from the population for the regime. The constraint is not a mountain because it is clearly historically contingent: it emerged after specific political choices (Chávez policies, Maduro's authoritarian turn) and could be reversed through regime change or democratic transition. The actual classification (Snare with Tangled Rope elements at the military level) correctly reflects that this is an extractive hierarchy sustained by coercion, not a coordination mechanism. The mandatrophy is resolved by recognizing the perspectival multiplicity: the constraint IS coordination from the military perspective (Tangled Rope) AND pure extraction from the civilian perspective (Snare), and both are structurally true. The constraint cannot be unified into a single type because the structural relationships are fundamentally asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinated_military_loyalty_versus_institutional_capture,
    'Does the military''s support for the regime reflect genuine coordination around state capacity, or is it pure institutional capture driven by resource extraction?',
    'Analysis of military budget allocation trends, comparison to pre-crisis baseline, examination of smuggling networks and illicit enrichment channels, surveys of military defection rates and stated rationales',
    'If coordination: military perspective is correctly classified as Tangled Rope with genuine mutual benefit. If capture: military perspective should be reclassified as Snare with military as secondary victim of institutional lock-in. Changes classification of 2+ perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinated_military_loyalty_versus_institutional_capture, empirical, 'Whether military support reflects coordination or institutional capture').

omega_variable(
    diaspora_remittance_extraction_rate,
    'What percentage of diaspora remittances are effectively taxed through informal channels, property seizure, and travel restrictions on family members?',
    'Quantitative analysis of remittance flows vs declared income/assets; cross-country comparison of similar diaspora remittance patterns; interviews with diaspora members on taxation mechanisms',
    'If high extraction (>30% effective tax): diaspora perspective is pure victim despite mobility, reclassifies to Snare. If low extraction (<10%): confirms Tangled Rope classification with asymmetric but limited extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diaspora_remittance_extraction_rate, empirical, 'Effective extraction rate on diaspora remittances').

omega_variable(
    international_sanctions_collateral_harm_versus_targeted_pressure,
    'Do international sanctions'' collateral effects on civilian population (medical imports, fuel scarcity) exceed the pressure effect on regime decision-making?',
    'Cost-benefit analysis: comparative quantification of civilian harm attributable to sanctions vs regime policy; counterfactual analysis of regime behavior absent sanctions; temporal correlation of sanctions intensity with regime concessions',
    'If collateral harm dominates: sanctions regime reclassifies from Scaffold to Snare, losing legitimacy. If targeted pressure dominates: Scaffold classification confirmed with sunset mechanism intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_sanctions_collateral_harm_versus_targeted_pressure, empirical, 'Relative magnitude of sanctions collateral harm vs pressure effectiveness').

omega_variable(
    electoral_fraud_detection_capacity_versus_acceptance_of_legitimacy_theater,
    'Is the population''s continued participation in elections despite widespread fraud an indicator of identity-locked belief in democratic process, or rational strategic compliance?',
    'Exit survey data on fraud awareness, voting intention transparency, comparison to abstention rates pre/post major fraud revelations, analysis of opposition turnout patterns in contested elections',
    'If identity-locked: population experiences constraint through internalized legitimacy beliefs, not just external coercion. Reclassifies powerless perspective from pure Snare to partly Mountain (unchangeable from within identity frame). If strategic compliance: confirms pure Snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electoral_fraud_detection_capacity_versus_acceptance_of_legitimacy_theater, conceptual, 'Whether population participation in fraudulent elections reflects identity-lock or strategic compliance').

omega_variable(
    regime_regime_transition_pathway_feasibility,
    'Is the constraint structure fundamentally unstable (regime collapse imminent) or stable (extractive equilibrium sustainable indefinitely)?',
    'Analysis of regime revenue sources sustainability, military defection thresholds, elite consolidation vs fragmentation, regional/international intervention likelihood, demographic trends in opposition organization',
    'If unstable: constraint is temporary and approaching phase transition, reclassifies to Scaffold. If stable: constraint is entrenched snare, potentially degrading to Piton over generational timescale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_regime_transition_pathway_feasibility, empirical, 'Regime stability and transition pathway feasibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venezuela_regime_legitimacy_crisis, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(venez_tr_t0, venezuela_regime_legitimacy_crisis, theater_ratio, 0, 0.4).
narrative_ontology:measurement(venez_tr_t5, venezuela_regime_legitimacy_crisis, theater_ratio, 5, 0.65).
narrative_ontology:measurement(venez_tr_t10, venezuela_regime_legitimacy_crisis, theater_ratio, 10, 0.8).
narrative_ontology:measurement(venez_tr_t15, venezuela_regime_legitimacy_crisis, theater_ratio, 15, 0.82).

% Extraction over time
narrative_ontology:measurement(venez_be_t0, venezuela_regime_legitimacy_crisis, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(venez_be_t5, venezuela_regime_legitimacy_crisis, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(venez_be_t10, venezuela_regime_legitimacy_crisis, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(venez_be_t15, venezuela_regime_legitimacy_crisis, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venezuela_regime_legitimacy_crisis, enforcement_mechanism).
narrative_ontology:affects_constraint(venezuela_regime_legitimacy_crisis, latin_american_authoritarian_diffusion).
narrative_ontology:affects_constraint(venezuela_regime_legitimacy_crisis, petro_state_resource_curse).
narrative_ontology:affects_constraint(venezuela_regime_legitimacy_crisis, diaspora_remittance_dependency).

% DUAL FORMULATION NOTE:
% The Venezuela regime legitimacy crisis decomposes into three structurally distinct constraints with different ε values: (1) Military Institutional Capture (ε=0.55, Tangled Rope) — the coordination-extraction hybrid at the military-regime level; (2) Civilian Suppression Apparatus (ε=0.78, Snare) — pure coercive extraction from the general population; (3) Electoral Theater Persistence (ε=0.42, Piton) — performative legitimacy ritual with degraded function. These are linked constraints within a constraint family: the military capture enables the suppression apparatus (military enforces civilian extraction); the electoral theater legitimates the entire structure. The measured values (extractiveness 0.68, suppression 0.75) represent a weighted aggregate across these components. Separate constraint stories with individual ε values would enable more precise analysis of transition pathways: military defection triggers cascade through entire structure; electoral theater collapse precedes or follows suppression apparatus failure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(venezuela_regime_legitimacy_crisis, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
