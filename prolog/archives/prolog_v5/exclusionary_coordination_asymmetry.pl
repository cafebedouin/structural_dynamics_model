% ============================================================================
% CONSTRAINT STORY: exclusionary_coordination_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exclusionary_coordination_asymmetry, []).

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
 *   constraint_id: exclusionary_coordination_asymmetry
 *   human_readable: Exclusionary Coordination Asymmetry
 *   domain: authority_agency/coordination_systems
 *
 * SUMMARY:
 *   Exclusionary coordination asymmetry describes the structural pattern
 *   where coordination systems that genuinely solve collective action
 *   problems simultaneously create extraction burden through asymmetric
 *   access. Professional guilds coordinate quality standards and information
 *   flow while extracting rent through credentialing barriers. Social
 *   networks coordinate resource allocation and opportunity access while
 *   extracting through membership exclusion. Financial systems coordinate
 *   capital allocation while extracting through access fees and information
 *   asymmetry. The constraint exhibits irreducible indexical variance:
 *   insiders experience genuine coordination value (the system works for
 *   them), while outsiders experience genuine extraction burden (the system
 *   works against them). Both experiences are structurally valid, not
 *   perceptual artifacts. The constraint is downstream of environmental
 *   instability: when environments are unstable, coordination systems that
 *   can adapt quickly provide genuine value, but the mechanisms that enable
 *   rapid coordination (tight networks, shared context, trust) are the same
 *   mechanisms that create exclusionary barriers. The theater ratio (0.58)
 *   reflects increasing performative justification of exclusion (meritocracy
 *   narratives, quality standards rhetoric) as the coordination function
 *   becomes more established and the extraction more entrenched.
 *
 * KEY AGENTS:
 *   - Powerful Insiders: Primary beneficiary (institutional/arbitrage) — experience pure coordination value, can exit to alternative systems, capture positional rent from gatekeeping
 *   - Powerless Outsiders: Primary victim (powerless/trapped) — experience pure extraction burden, cannot access coordination benefits, cannot exit system that structures their exclusion
 *   - Gatekeepers: Secondary beneficiary (powerful/arbitrage) — control access mechanisms, experience coordination value plus positional rent, can arbitrage between systems
 *   - Aspirants: Mixed position (moderate/constrained) — partial access to coordination value, bear high entry costs, see path to insider status but face real barriers
 *   - Reform Coalition: Organized agents (organized/mobile) — recognize both coordination value and extraction burden, work to expand access while preserving function
 *   - Excluded Groups: Collective victim (powerless/trapped) — abstract collective that bears systemic exclusion costs, cannot organize effectively due to coordination deficit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exclusionary_coordination_asymmetry, 0.48).
domain_priors:suppression_score(exclusionary_coordination_asymmetry, 0.62).
domain_priors:theater_ratio(exclusionary_coordination_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exclusionary_coordination_asymmetry, extractiveness, 0.48).
narrative_ontology:constraint_metric(exclusionary_coordination_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(exclusionary_coordination_asymmetry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exclusionary_coordination_asymmetry, tangled_rope).
narrative_ontology:human_readable(exclusionary_coordination_asymmetry, "Exclusionary Coordination Asymmetry").
narrative_ontology:topic_domain(exclusionary_coordination_asymmetry, "authority_agency/coordination_systems").

domain_priors:requires_active_enforcement(exclusionary_coordination_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exclusionary_coordination_asymmetry, powerful_insiders).
narrative_ontology:constraint_beneficiary(exclusionary_coordination_asymmetry, gatekeepers).
narrative_ontology:constraint_victim(exclusionary_coordination_asymmetry, powerless_outsiders).
narrative_ontology:constraint_victim(exclusionary_coordination_asymmetry, excluded_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED OUTSIDER (SNARE) — Trapped by lack of access to coordination mechanisms that structure opportunity. Experiences pure extraction: bears costs of exclusion (lost economic opportunity, social isolation, inability to coordinate with others) with no coordination benefit. Cannot exit the system that structures their exclusion.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ASPIRANT (TANGLED ROPE) — Constrained by high entry costs and gatekeeping but can see path to insider status. Experiences both coordination value (partial access to networks, information, resources) and extraction burden (fees, credentialing requirements, social capital investment). Mixed experience reflects genuine hybrid structure.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSIDER INSTITUTION (ROPE) — Benefits from coordination system that structures access. Experiences constraint as pure coordination: the system enables efficient resource allocation, information flow, and collective action among members. Extraction runs toward this agent. Can exit to alternative coordination systems if needed.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (TANGLED ROPE) — Organized agents working to expand access see both coordination value (the system does solve real collective action problems) and extraction burden (exclusionary mechanisms concentrate benefits). Mobile enough to build alternative coordination pathways but recognize the existing system's genuine function.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE GATEKEEPER (ROPE) — Powerful insider who controls access mechanisms. Experiences constraint as coordination: managing entry maintains system quality and enables efficient operation. Benefits from both coordination value and positional rent. Can arbitrage between coordination systems.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, sees irreducible hybrid structure. The coordination system genuinely solves collective action problems (information asymmetry, quality signaling, resource pooling) AND creates asymmetric extraction through exclusionary access. Both functions are structural, not perspectival artifacts. The indexical variance is the phenomenon.
constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exclusionary_coordination_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exclusionary_coordination_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exclusionary_coordination_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exclusionary_coordination_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The coordination system genuinely provides value to insiders (information flow, resource pooling, quality signaling, collective action capacity) but this value is purchased through exclusion that imposes costs on outsiders (lost opportunity, social isolation, inability to coordinate). The extraction is not pure rent-seeking — the coordination function is real — but the asymmetric access creates genuine extraction burden. The value has increased over the interval as the system matured and exclusionary mechanisms became more entrenched. Suppression (0.62): High. Significant barriers to entry include credentialing requirements, social capital investment, information asymmetry, network effects, and often internalized legitimacy beliefs (outsiders believe exclusion is justified). But suppression is not total — some agents can and do gain access, and reform coalitions are building alternative pathways. Theater ratio (0.58): Moderate-high. Increasing performative justification of exclusion through meritocracy narratives, quality standards rhetoric, and naturalization of access barriers as necessary coordination costs. The theater has increased as the coordination function became established and the extraction more visible — more justification needed to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates irreducible indexical variance where the classification legitimately differs by structural position. Powerful insiders genuinely experience Rope (pure coordination) because the system works for them — they access information, resources, and collective action capacity with minimal friction. Powerless outsiders genuinely experience Snare (pure extraction) because the system works against them — they bear exclusion costs with no coordination benefit. Aspirants and reform coalitions experience Tangled Rope (hybrid) because they have partial access and can see both functions. The analytical observer also sees Tangled Rope because from civilizational perspective both the coordination function and the extraction burden are structural features, not perspectival artifacts. The gap is not a measurement error or false consciousness — it is the phenomenon. The coordination system genuinely solves collective action problems AND genuinely creates asymmetric extraction. Insiders are not wrong to see coordination value; outsiders are not wrong to see extraction burden. The presheaf over observation sites captures this irreducible variance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the coordination system's access boundary. Powerful insiders are beneficiaries with arbitrage exit — they experience low d (near 0.15) producing low or negative chi (pure coordination value). Powerless outsiders are victims with trapped exit — they experience high d (near 0.95) producing maximum chi (pure extraction burden). Gatekeepers are beneficiaries with arbitrage exit but also control the access mechanism — they experience very low d (near 0.05) producing negative chi (they extract rent from the system). Aspirants are partial victims with constrained exit — they experience moderate-high d (near 0.65) producing moderate chi (mixed coordination value and extraction burden). Reform coalitions are organized agents with mobile exit — they experience moderate d (near 0.50) producing balanced chi (see both functions clearly). The analytical observer uses analytical exit and sees the irreducible hybrid structure — moderate d (near 0.55) producing moderate chi (both coordination and extraction are structural).
 *
 * MANDATROPHY ANALYSIS:
 *   INDEXICAL VARIANCE EXEMPLAR: This constraint resolves mandatrophy by demonstrating that Tangled Rope classification at the analytical level can coexist with Rope classification from beneficiary perspectives and Snare classification from victim perspectives when the structural asymmetry is genuine. The mandatrophy question 'Is this coordination or extraction?' has no single answer — it depends on where you stand. The analytical observer sees Tangled Rope because both functions are structural. The insider sees Rope because they experience genuine coordination value. The outsider sees Snare because they experience genuine extraction burden. All three classifications are correct from their respective positions. The constraint is not mislabeled — the indexical variance IS the constraint. The system coordinates insiders AND extracts from outsiders as a single structural mechanism. Attempting to resolve this to a single type would erase the structural asymmetry that is the constraint's defining feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'What level of exclusionary access is structurally necessary for coordination function vs extractive rent-seeking?',
    'Comparative analysis of coordination systems with varying access barriers; measurement of coordination efficiency vs exclusion costs across different threshold levels',
    'If low threshold sufficient: most exclusion is extractive overhead (Snare from more perspectives). If high threshold necessary: exclusion is coordination cost (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Threshold distinguishing necessary coordination barriers from extractive exclusion').

omega_variable(
    alternative_coordination_viability,
    'Can alternative coordination mechanisms achieve equivalent function without exclusionary access?',
    'Empirical testing of open-access coordination systems (open-source communities, distributed networks, public goods provision) against exclusionary systems on coordination efficiency metrics',
    'If viable alternatives exist: exclusionary systems are extractive choice (Snare). If no viable alternatives: exclusionary access is structural necessity (Rope/Mountain).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether non-exclusionary coordination can match exclusionary system efficiency').

omega_variable(
    insider_naturalization_mechanism,
    'Do insiders genuinely experience only coordination value, or do they naturalize extraction they don''t personally bear?',
    'Longitudinal tracking of agents across insider/outsider transitions; measurement of classification shift as structural position changes; analysis of insider discourse for naturalization patterns',
    'If insiders naturalize: their Rope classification is false consciousness (should be Tangled Rope with awareness gap). If insiders genuinely experience pure coordination: the indexical variance is structural, not perceptual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insider_naturalization_mechanism, conceptual, 'Whether insider Rope classification reflects genuine experience or naturalized extraction').

omega_variable(
    suppression_mechanism_decomposition,
    'Is suppression primarily structural (material barriers to entry) or internalized (outsiders believe exclusion is legitimate)?',
    'Measurement of suppression persistence after barrier removal; analysis of outsider discourse for legitimacy beliefs vs structural constraint recognition; comparison of suppression levels across different exclusionary systems with similar material barriers but different legitimacy narratives',
    'If primarily structural: reducing material barriers reduces suppression. If primarily internalized: barrier removal insufficient without legitimacy challenge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_decomposition, empirical, 'Decomposition of suppression into structural vs internalized components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exclusionary_coordination_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eca_tr_t0, exclusionary_coordination_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eca_tr_t3, exclusionary_coordination_asymmetry, theater_ratio, 3, 0.42).
narrative_ontology:measurement(eca_tr_t6, exclusionary_coordination_asymmetry, theater_ratio, 6, 0.5).
narrative_ontology:measurement(eca_tr_t9, exclusionary_coordination_asymmetry, theater_ratio, 9, 0.55).
narrative_ontology:measurement(eca_tr_t12, exclusionary_coordination_asymmetry, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(eca_be_t0, exclusionary_coordination_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eca_be_t3, exclusionary_coordination_asymmetry, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(eca_be_t6, exclusionary_coordination_asymmetry, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(eca_be_t9, exclusionary_coordination_asymmetry, base_extractiveness, 9, 0.46).
narrative_ontology:measurement(eca_be_t12, exclusionary_coordination_asymmetry, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(eca_be_t15, exclusionary_coordination_asymmetry, base_extractiveness, 15, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exclusionary_coordination_asymmetry, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of environmental_instability_as_constraint. When environments are unstable, coordination systems that can adapt quickly provide genuine value, but the mechanisms that enable rapid coordination (tight networks, shared context, trust) are the same mechanisms that create exclusionary barriers. The upstream mountain (environmental instability) creates selection pressure for coordination systems, but does not determine whether those systems will be exclusionary. The exclusionary asymmetry is a contingent institutional response to the mountain constraint, not an inevitable consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exclusionary_coordination_asymmetry, powerful, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
