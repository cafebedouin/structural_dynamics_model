% ============================================================================
% CONSTRAINT STORY: hybrid_legitimation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hybrid_legitimation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Through Symbolic Authority Transfer
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The hybrid legitimation mechanism describes a state formation process
 *   where new cultural norms achieve legitimacy through the combination of
 *   symbolic authority transfer (the emperor's personal example or
 *   endorsement) and institutional incentives (tax benefits, legal
 *   recognition, access to state resources). This reading sits between two
 *   alternatives: the endogenous climb reading (norms emerge from grassroots
 *   demand and are rationalized post-hoc by elites) and the exogenous
 *   override reading (norms are imposed coercively and legitimacy claims are
 *   pure theater). The hybrid reading claims that BOTH symbolic authority and
 *   material incentives are structurally necessary: symbolic authority
 *   provides initial legitimacy that reduces resistance, while institutional
 *   incentives build habitual compliance across stratified populations. The
 *   mechanism produces asymmetric extraction: early adopter elites capture
 *   benefits (imperial favor, state offices, resource access) during the
 *   transition period, while traditional practice communities and non-elite
 *   populations bear compliance costs without voice in the norm-setting
 *   process. The constraint exhibits moderate enforcement costs (neither the
 *   low costs of grassroots climb nor the high costs of pure coercion) and
 *   stratified adoption patterns (elites first, masses later). Theater ratio
 *   rises over the interval as imperial charisma atrophies but the ritual of
 *   imperial endorsement persists through institutional inertia.
 *
 * KEY AGENTS:
 *   - Imperial Court: Primary agenda-setter (institutional/arbitrage) — emperor's symbolic authority provides initial legitimacy; captures state-building benefits
 *   - Early Adopter Elites: Primary beneficiaries (institutional/arbitrage) — gain imperial favor, state offices, and resource access by aligning with new norms
 *   - Institutional Intermediaries: Organized agents (organized/mobile) — provincial administrators, ritual specialists, educational institutions that mediate between imperial authority and local populations; see mechanism as transitional scaffold
 *   - Traditional Practice Communities: Primary victims (powerless/trapped) — economic dependency on elite patronage; no exit from imperial authority; bear costs of norm transition without voice
 *   - Non-Elite Populations: Secondary victims (moderate/constrained) — constrained by institutional incentives but also benefit from coordination function; mixed experience
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (standardization) and asymmetric extraction (elite capture during transition)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_legitimation_reading, 0.35).
domain_priors:suppression_score(hybrid_legitimation_reading, 0.45).
domain_priors:theater_ratio(hybrid_legitimation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_legitimation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hybrid_legitimation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_legitimation_reading, "Hybrid Legitimation Through Symbolic Authority Transfer").
narrative_ontology:topic_domain(hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_legitimation_reading, '82f9169b-2305-4cfd-a840-707f8644f6bc').
narrative_ontology:cs_kernel_codification('82f9169b-2305-4cfd-a840-707f8644f6bc', distributed).
narrative_ontology:cs_authority_grounding('82f9169b-2305-4cfd-a840-707f8644f6bc', lineage).
narrative_ontology:cs_interpretation_layer_present('82f9169b-2305-4cfd-a840-707f8644f6bc').
narrative_ontology:cs_reading_relation('82f9169b-2305-4cfd-a840-707f8644f6bc', hybrid_legitimation_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('82f9169b-2305-4cfd-a840-707f8644f6bc', hybrid_legitimation_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('82f9169b-2305-4cfd-a840-707f8644f6bc', foundational, charismatic_authority_transfers_legitimacy).
narrative_ontology:cs_axiom_status(charismatic_authority_transfers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('82f9169b-2305-4cfd-a840-707f8644f6bc', charismatic_authority_transfers_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('82f9169b-2305-4cfd-a840-707f8644f6bc', secondary, institutional_incentives_build_habitual_compliance).
narrative_ontology:cs_axiom_status(institutional_incentives_build_habitual_compliance, holdable).
narrative_ontology:cs_axiom_grounding('82f9169b-2305-4cfd-a840-707f8644f6bc', institutional_incentives_build_habitual_compliance, instrumental).
narrative_ontology:cs_reference_frame('82f9169b-2305-4cfd-a840-707f8644f6bc', imperial_charismatic_authority).
narrative_ontology:cs_drift_state('82f9169b-2305-4cfd-a840-707f8644f6bc', late_dynastic_period, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('82f9169b-2305-4cfd-a840-707f8644f6bc', '').
narrative_ontology:cs_kernel_id(hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, imperial_court).
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, early_adopter_elites).
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, institutional_intermediaries).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, traditional_practice_communities).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, non_elite_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, non_elite_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The imperial court sets new cultural norms through the emperor's personal example and formal endorsement. The court captures state-building benefits: centralized authority, expanded administrative reach, and legitimacy for resource extraction. The emperor's charisma provides initial legitimacy that reduces resistance costs. The court has arbitrage-grade exit: can maintain traditional practices in private domains while publicly endorsing new norms.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, imperial_court, agenda_setter,
    institutional, immediate, arbitrage, national).

% Provincial elites, merchant families, and scholar-officials who adopt new norms early to gain imperial favor. They capture state offices, tax benefits, legal recognition, and access to imperial patronage networks. They have arbitrage-grade exit: can adopt new norms in public contexts while maintaining traditional practices in private domains. Net beneficiaries during the transition period.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, early_adopter_elites, beneficiary,
    institutional, immediate, arbitrage, national).

% Provincial administrators, ritual specialists, educational institutions, and religious organizations that mediate between imperial authority and local populations. They benefit from expanded administrative roles and institutional resources. They see the hybrid mechanism as transitional: symbolic authority provides initial legitimacy while institutional incentives build habitual compliance across generations. Mobile exit: can shift between imperial service and local patronage networks.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, institutional_intermediaries, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hybrid_legitimation_reading, institutional_intermediaries, agenda_setter).

% Artisan guilds, local religious communities, and kinship networks whose traditional practices are displaced by new norms. They are economically dependent on elite patronage and institutional access. Resistance means exclusion from state resources, legal recognition, and elite networks. Trapped: no exit from imperial authority, no voice in norm-setting process. Bear compliance costs without capturing benefits.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, traditional_practice_communities, payer,
    powerless, biographical, trapped, regional).

% Peasant farmers, urban laborers, and small merchants who face institutional incentives to adopt new norms: tax benefits for compliance, legal recognition for participation in state-mediated exchange, ritual participation requirements for community standing. They bear compliance costs (learning new practices, abandoning traditional ones) but also benefit from coordination function: standardized norms reduce transaction costs and enable broader participation in markets and state institutions. Constrained exit: high costs to resist but not impossible.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, non_elite_populations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hybrid_legitimation_reading, non_elite_populations, beneficiary).

% The analytical observer sees the hybrid mechanism from a civilizational perspective: genuine coordination function (standardization reduces transaction costs) coexists with asymmetric extraction (elites capture benefits during transition, non-elites bear costs without voice). The mechanism is neither pure climb (grassroots demand) nor pure override (coercion) but a structured asymmetry where symbolic authority and material incentives jointly produce stratified adoption.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardization of cultural norms across diverse regional and social populations to enable state-mediated exchange, legal recognition, and ritual participation. The coordination problem is real: without shared norms, transaction costs are high and state administrative capacity is limited.
% TRANSFER_FUNCTION: Imperial favor, state offices, tax benefits, legal recognition, and access to patronage networks flow from the imperial court to early adopter elites. Compliance costs (learning new practices, abandoning traditional ones, exclusion risk) flow from non-elite populations and traditional practice communities to the state. Symbolic legitimacy flows from the emperor's charisma to the new norms.
% ABSENT_VOICES: Traditional practice communities whose norms are displaced have no voice in the norm-setting process. They are excluded from the imperial court and from elite networks. Their absence is structural: the hybrid mechanism operates through elite adoption first, and traditional communities are brought into compliance through institutional incentives and exclusion threats rather than through participation in norm design. This is commentary-grade evidence that the coordination function is asymmetric, not a classification override.
% DISAPPEARANCE_RATIONALE: If the hybrid legitimation mechanism disappeared overnight — if the emperor withdrew symbolic endorsement and institutional incentives were removed — the new norms would not persist on their own. Early adopter elites would revert to traditional practices or adopt alternative norms that maximize their access to resources. Non-elite populations would abandon compliance once the material benefits and exclusion threats were removed. The mechanism is not self-sustaining until the norms are internalized across generations (the scaffold sunset). The world rearranges because the arrangements (elite adoption, institutional incentives, stratified compliance) depend on the constraint's active operation.
% FOUNDING_PROBLEM: Fragmented cultural practices across diverse regional and social populations created high transaction costs for state-mediated exchange and limited the state's administrative reach. The founding problem was genuine: without shared norms, the state could not effectively tax, conscript, adjudicate disputes, or coordinate large-scale projects. The hybrid mechanism was built to solve this coordination problem by leveraging imperial charisma to reduce resistance costs while using institutional incentives to build habitual compliance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (fragmented cultural practices limiting state capacity) is corroborated by historical records of administrative expansion, tax collection efficiency, and legal standardization following norm adoption. Corroborating sources include: provincial administrative reports (outside the imperial court), merchant guild records (outside the beneficiary set), and archaeological evidence of material culture standardization. The problem's live status is corroborated by ongoing state efforts to maintain norm compliance through institutional incentives and by reversion to traditional practices in regions where state capacity weakens.
narrative_ontology:disappearance_verdict(hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hybrid_legitimation_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL PRACTICE COMMUNITIES (SNARE) — Trapped by economic dependency on elite patronage and institutional access. The new norms arrive as fait accompli backed by imperial authority; resistance means exclusion from state resources and elite networks. Maximum experienced extraction — no exit, no voice in the transition.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-ELITE POPULATIONS (TANGLED ROPE) — Constrained by institutional incentives (tax benefits, legal recognition, ritual participation requirements) but also benefit from the coordination function: standardized norms reduce transaction costs and enable broader participation in state-mediated exchange. Mixed experience — some agency, some extraction.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY ADOPTER ELITES (ROPE) — Arbitrage-grade exit: can adopt new norms to gain imperial favor or maintain traditional practices in private domains. Experience the constraint as coordination: aligning with imperial authority opens access to state resources, offices, and legitimacy. Net beneficiaries — extraction flows toward them.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL INTERMEDIARIES (SCAFFOLD) — Organized agents (provincial administrators, ritual specialists, educational institutions) see the hybrid mechanism as transitional: symbolic authority transfer provides initial legitimacy while institutional incentives build habitual compliance. Sunset logic: once the new norms are internalized across generations, neither imperial charisma nor material incentives are needed — the norms become self-sustaining practice. Estimated sunset: 2-3 generations for full internalization.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: IMPERIAL COURT LATE PERIOD (PITON) — From a civilizational perspective, the imperial court's symbolic authority has atrophied: the emperor's example no longer carries transformative legitimacy, but the ritual of imperial endorsement persists through institutional inertia. The court maintains the performance of norm-setting authority despite diminished functional capacity to drive cultural change. Theater ratio reflects this degradation.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the hybrid mechanism exhibits both genuine coordination (standardization reduces transaction costs, enables state-mediated exchange) and asymmetric extraction (elites capture benefits during transition, non-elites bear compliance costs without voice). The mechanism is neither pure climb (grassroots demand) nor pure override (coercive imposition) but a structured asymmetry where symbolic authority and material incentives jointly produce stratified adoption.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_legitimation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_legitimation_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Early adopter elites capture significant benefits (imperial favor, state offices, resource access) during the transition period, and traditional practice communities bear compliance costs without voice. But the extraction is not as severe as pure override (0.60+) because the coordination function is real: standardized norms reduce transaction costs and enable broader participation in state-mediated exchange. The moderate value reflects that the career and resource asymmetry, while real, is partly offset by genuine coordination gains. Suppression (0.45): Moderate. Institutional incentives create significant pressure to adopt new norms (tax benefits, legal recognition, ritual participation requirements), and traditional practice communities face exclusion from state resources if they resist. But suppression is not total: some communities maintain traditional practices in private domains, and the mechanism accommodates gradual adoption rather than demanding immediate compliance. The moderate value reflects that alternatives are constrained but not eliminated. Theater ratio (0.58): Moderate-high. The ritual of imperial endorsement carries genuine legitimacy in the early period (t=0: 0.30) when imperial charisma is strong, but atrophies over time as the emperor's example loses transformative force. By the late period (t=50: 0.58), the performance of imperial norm-setting authority persists through institutional inertia despite diminished functional capacity. The rising trajectory models the degradation from functional symbolic authority to performative ritual.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism — symbolic authority transfer combined with institutional incentives — produces radically different experiences depending on the observer's position. Early adopter elites see coordination (Rope): aligning with imperial authority solves the problem of accessing state resources and legitimacy. Institutional intermediaries see a transitional scaffold: the hybrid mechanism builds compliance that will eventually become self-sustaining. Traditional practice communities see pure extraction (Snare): the new norms arrive as fait accompli with no exit and no voice. Non-elite populations see mixed coordination and extraction (Tangled Rope): institutional incentives both enable and constrain. The late-period imperial court sees its own degraded ritual (Piton): symbolic authority has atrophied but the performance persists. The analytical observer sees tangled rope: the mechanism has genuine coordination function but produces asymmetric extraction during the transition. The perspectival gap reveals that 'legitimacy' is not a property of the norms themselves but of the structural relationship between the norm-setting authority and the populations subject to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position — their power level, exit options, and relationship to the extraction flow. The engine computes d from beneficiary/victim declarations and exit options, then applies the sigmoid f(d) to produce experienced extractiveness chi. Early adopter elites are beneficiaries with arbitrage exit → low d → low/negative chi (they experience the constraint as coordination). Traditional practice communities are victims with trapped exit → high d → high chi (they experience maximum extraction). Non-elite populations are victims with constrained exit → moderate d → moderate chi (mixed experience). Institutional intermediaries are beneficiaries with mobile exit → low d → low chi (they see the mechanism as transitional coordination). The piton classification for the late-period imperial court derives from the theater gate (rising theater_ratio crosses the piton threshold) rather than from high chi. The analytical observer sees tangled rope: genuine coordination function (standardization) coexists with asymmetric extraction (elite capture).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that hybrid legitimation is neither pure coordination (Rope) nor pure extraction (Snare) but a structured asymmetry (Tangled Rope from the analytical perspective). The coordination function is real: standardized norms reduce transaction costs and enable state-mediated exchange. But the extraction is also real: elites capture benefits during the transition period while non-elites bear compliance costs without voice. The mechanism is not a false summit (naturalized extraction) because the coordination function persists even after accounting for the extraction. It is not a pure snare because the institutional incentives provide genuine benefits to non-elite populations (legal recognition, ritual participation, access to state resources). The tangled rope classification captures the structural reality: coordination and extraction are inseparable in this mechanism, and the perspectival gap (Rope from beneficiaries, Snare from victims, Tangled Rope from analytical observer) is the measurement the framework exists to take.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a hybrid legitimation mechanism (symbolic authority + institutional incentives), an endogenous climb (grassroots demand rationalized post-hoc), or an exogenous override (coercion masked by ritual)?',
    'Historical evidence of adoption sequence (elite-first vs mass-first), enforcement costs (low = climb, high = override, moderate = hybrid), and resistance patterns (suppressed vs accommodated). Comparative analysis across multiple state formation episodes.',
    'If endogenous climb: extractiveness lower, beneficiaries include mass populations. If exogenous override: extractiveness higher, suppression higher, victims include elites. If hybrid: extractiveness and suppression moderate, stratified beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Which reading of the imposition mechanism kernel is structurally correct').

omega_variable(
    charisma_durability_threshold,
    'How many generations can symbolic authority transfer sustain legitimacy before institutional incentives must carry the full load?',
    'Longitudinal analysis of norm persistence across dynastic transitions, succession crises, and periods of weak imperial authority. Measurement of compliance rates when charismatic authority is absent but institutional incentives remain vs when both are absent.',
    'If threshold < 1 generation: scaffold sunset arrives quickly, hybrid mechanism is transitional. If threshold > 3 generations: symbolic authority has durable structural force, mechanism is stable tangled rope rather than scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charisma_durability_threshold, empirical, 'Durability of charismatic authority in hybrid legitimation').

omega_variable(
    elite_adoption_sufficiency,
    'Is elite adoption alone sufficient to establish new norms as legitimate, or does mass internalization require independent mechanisms?',
    'Comparison of norm persistence in regions with high elite adoption but low mass compliance vs regions with stratified adoption across social strata. Analysis of norm reversion rates after elite defection or dynastic collapse.',
    'If elite adoption sufficient: the hybrid mechanism is primarily extraction (elites capture legitimacy, masses comply under duress). If mass internalization required: the mechanism has genuine coordination function (institutional incentives enable mass participation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_adoption_sufficiency, empirical, 'Whether elite adoption alone establishes norm legitimacy').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the emperor''s symbolic authority (lineage-grounded), or the institutional incentive structure (extraction-grounded)?',
    'Identify which element persists when the other is removed: if symbolic authority collapses but institutional incentives sustain compliance, the kernel is the incentive structure. If incentives are withdrawn but imperial example sustains adoption, the kernel is symbolic authority. Historical natural experiments: succession crises (test symbolic authority), fiscal collapse (test institutional incentives).',
    'If kernel = symbolic authority: cs_structure.authority_grounding should be lineage, and drift_state tracks erosion of imperial charisma. If kernel = institutional incentives: authority_grounding should be extraction, and drift_state tracks fiscal capacity or administrative reach. Current framing (lineage) assumes symbolic authority is primary; alternative framing would reclassify as extraction-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which structural element constitutes the kernel in hybrid legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_legitimation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_leg_theater_early, hybrid_legitimation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hybrid_leg_theater_mid, hybrid_legitimation_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(hybrid_leg_theater_late, hybrid_legitimation_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(hybrid_leg_extract_early, hybrid_legitimation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hybrid_leg_extract_mid, hybrid_legitimation_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(hybrid_leg_extract_late, hybrid_legitimation_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_leg_suppress_early, hybrid_legitimation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hybrid_leg_suppress_mid, hybrid_legitimation_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(hybrid_leg_suppress_late, hybrid_legitimation_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_legitimation_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The hybrid legitimation mechanism is one reading of a contested kernel. The endogenous_climb_reading and exogenous_override_reading are sibling constraints (separate JSON files) that model alternative structural interpretations of the same historical episodes. Each reading has its own extractiveness value, beneficiary structure, and enforcement cost profile. The readings are linked via cs_structure.reading_relations rather than network.affects_constraints because they are alternative framings of the same kernel, not causally dependent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
