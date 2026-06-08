% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous Override: State-Imposed Norms via Coercive Authority
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   The exogenous override reading models norm imposition as a top-down
 *   coercive process: the state uses its monopoly on violence to impose
 *   behavioral rules that lack prior cultural legitimacy. This is the
 *   canonical state formation story — Weber's rational-legal authority backed
 *   by legitimate violence, Scott's high modernism making society legible,
 *   Foucault's disciplinary power. The constraint is extractive because
 *   compliance is coerced rather than voluntary: subjects conform to avoid
 *   punishment, not because they endorse the norms. Suppression is high
 *   because the state must actively monitor and enforce — compliance is
 *   conditional on the credible threat of violence. The theater ratio rises
 *   over time as enforcement becomes ritualized: the initial imposition
 *   requires genuine violence (low theater), but as the system matures, much
 *   enforcement becomes performative (checkpoints, inspections, paperwork)
 *   while actual violence is reserved for serious defiance. This reading is
 *   one of three sibling readings of the imposition_mechanism_kernel. The
 *   endogenous_climb_reading models norms emerging from within the culture
 *   and climbing to state codification. The hybrid_legitimation_reading
 *   models a mixed process where state coercion and cultural acceptance
 *   co-evolve. The three readings share a kernel (the mechanism by which
 *   norms become authoritative) but differ in their authority grounding,
 *   enforcement requirements, and extractiveness profiles.
 *
 * KEY AGENTS:
 *   - State Apparatus: Primary beneficiary (institutional/arbitrage) — gains administrative capacity, tax revenue, and monopoly on legitimate violence
 *   - Aligned Elite Factions: Secondary beneficiary (powerful/mobile) — gain state backing for property rights and contracts; bear conformity costs
 *   - Subject Populations: Primary victim (powerless/trapped) — bear compliance costs under threat of punishment; no exit except emigration or rebellion
 *   - Displaced Cultural Authorities: Secondary victim (moderate/constrained) — traditional leaders whose authority is superseded; some adapt, others are suppressed
 *   - Reformist Coalition: Organized agents (organized/constrained) — see coercion as transitional scaffold to build modern institutions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and genuine extraction; classifies as Tangled Rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.68).
domain_priors:suppression_score(exogenous_override_reading, 0.82).
domain_priors:theater_ratio(exogenous_override_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, snare).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous Override: State-Imposed Norms via Coercive Authority").
narrative_ontology:topic_domain(exogenous_override_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, 'acc6f0e3-5cf6-42b4-8a42-734a5a555709').
narrative_ontology:cs_kernel_codification('acc6f0e3-5cf6-42b4-8a42-734a5a555709', formalized).
narrative_ontology:cs_authority_grounding('acc6f0e3-5cf6-42b4-8a42-734a5a555709', extraction).
narrative_ontology:cs_interpretation_layer_present('acc6f0e3-5cf6-42b4-8a42-734a5a555709').
narrative_ontology:cs_reading_relation('acc6f0e3-5cf6-42b4-8a42-734a5a555709', exogenous_override_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('acc6f0e3-5cf6-42b4-8a42-734a5a555709', exogenous_override_reading__hybrid_legitimation_reading, influences).
narrative_ontology:cs_axiom('acc6f0e3-5cf6-42b4-8a42-734a5a555709', foundational, violence_monopoly_grounds_authority).
narrative_ontology:cs_axiom_status(violence_monopoly_grounds_authority, holdable).
narrative_ontology:cs_axiom_grounding('acc6f0e3-5cf6-42b4-8a42-734a5a555709', violence_monopoly_grounds_authority, conventional).
narrative_ontology:cs_axiom('acc6f0e3-5cf6-42b4-8a42-734a5a555709', secondary, legitimacy_follows_compliance).
narrative_ontology:cs_axiom_status(legitimacy_follows_compliance, holdable).
narrative_ontology:cs_axiom_grounding('acc6f0e3-5cf6-42b4-8a42-734a5a555709', legitimacy_follows_compliance, empirically_contingent).
narrative_ontology:cs_reference_frame('acc6f0e3-5cf6-42b4-8a42-734a5a555709', pre_state_customary_order).
narrative_ontology:cs_drift_state('acc6f0e3-5cf6-42b4-8a42-734a5a555709', post_imposition_generation_two, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('acc6f0e3-5cf6-42b4-8a42-734a5a555709', '').
narrative_ontology:cs_kernel_id(exogenous_override_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, state_apparatus).
narrative_ontology:constraint_beneficiary(exogenous_override_reading, aligned_elite_factions).
narrative_ontology:constraint_victim(exogenous_override_reading, subject_populations).
narrative_ontology:constraint_victim(exogenous_override_reading, displaced_cultural_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(exogenous_override_reading, aligned_elite_factions).
narrative_ontology:constraint_victim(exogenous_override_reading, reformist_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The state apparatus sets and enforces the norms through its monopoly on violence. It gains administrative capacity, tax revenue, and the ability to conscript labor. It can revise norms, delegate enforcement, or withdraw from peripheral regions. The state experiences the constraint as a coordination mechanism that solves the problem of legal predictability across diverse populations.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Local elites who align with state norms to preserve their position. They benefit from state enforcement of property rights and contracts, but they also bear costs: must conform to state norms that may conflict with local custom, must pay taxes, must suppress local rivals who resist. They can relocate capital or shift allegiance if the state weakens.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, aligned_elite_factions, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(exogenous_override_reading, aligned_elite_factions, payer).

% The subject population bears compliance costs under threat of punishment. They must conform to norms they did not endorse and may not understand. Exit options are geographic (emigration, which most cannot afford) or violent (rebellion, which invites suppression). Compliance is coerced rather than internalized.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, subject_populations, payer,
    powerless, biographical, trapped, national).

% Traditional religious leaders, tribal elders, or customary law practitioners whose authority is superseded by state imposition. Some can adapt by aligning with the state; others retreat to informal influence. The state's monopoly on violence forecloses their traditional adjudication mechanisms and extracts their legitimacy capital.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, displaced_cultural_authorities, payer,
    moderate, biographical, constrained, regional).

% Organized groups (modernizing bureaucrats, nationalist movements, legal reformers) who see state imposition as a temporary mechanism to break entrenched traditional structures. They support coercive enforcement now to build institutional infrastructure that will make coercion unnecessary later. They bear costs (must support enforcement) but expect future benefits (modern institutions).
narrative_ontology:constraint_stakeholder(exogenous_override_reading, reformist_coalition, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(exogenous_override_reading, reformist_coalition, payer).

% The analytical observer sees both the coordination function (legal uniformity, contract enforcement) and the extraction function (coerced compliance, suppression of alternatives). Neither function can be reduced to the other. The observer is neither collecting nor paying, but recognizing the structural irreducibility of the Tangled Rope.
narrative_ontology:constraint_stakeholder(exogenous_override_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state solves the genuine problem of legal predictability and administrative legibility across diverse populations with incompatible customary systems. Uniform norms enable contract enforcement, taxation, conscription, and resource allocation at scale.
% TRANSFER_FUNCTION: The state extracts compliance (behavioral conformity, tax revenue, conscripted labor) from subject populations through threat of punishment. It transfers administrative capacity and enforcement backing to aligned elite factions. It suppresses the authority of displaced cultural leaders.
% ABSENT_VOICES: Peripheral populations outside the state's effective reach, stateless peoples, and traditional authorities who were never consulted in the norm-setting process. They would object to the imposition but were not in the room when the norms were codified. Their absence is structural: the state's monopoly on violence makes their consent unnecessary.
% DISAPPEARANCE_RATIONALE: If the state's coercive enforcement disappeared overnight, subject populations would revert to customary norms, aligned elites would lose state backing for their property claims, and displaced cultural authorities would reassert traditional adjudication mechanisms. The administrative infrastructure (courts, police, bureaucracy) depends on the state's monopoly on violence. Without it, the uniform norms would fragment back into local customary systems.
% FOUNDING_PROBLEM: The founding problem was the administrative chaos and military vulnerability of fragmented customary systems. Pre-state societies had incompatible local norms, no mechanism for large-scale resource mobilization, and no unified legal framework for contract enforcement or property rights. The state was built to solve this coordination problem through coercive imposition of uniform norms.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and aligned elite factions attest that the founding problem (administrative fragmentation) is still live and that state coercion remains necessary. Displaced cultural authorities and some subject populations attest that the founding problem is overstated or manufactured — that customary systems provided adequate coordination and that state imposition serves extraction rather than coordination. Historical sociologists (analytical observers) note that the founding problem was real in some cases (e.g., post-conquest state formation) but manufactured in others (e.g., colonial 'pacification' campaigns that destroyed functional customary systems to justify state imposition).
narrative_ontology:disappearance_verdict(exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(exogenous_override_reading, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Trapped by state monitoring and enforcement apparatus. Compliance is coerced rather than internalized. Exit options are geographic (emigration, which most cannot afford) or violent (rebellion, which invites suppression). The norm is experienced as pure extraction — behavioral conformity is extracted through threat of punishment, with no genuine coordination benefit from the subject's perspective.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISPLACED CULTURAL AUTHORITY (SNARE) — Traditional religious leaders, tribal elders, or customary law practitioners whose authority is superseded by state imposition. Constrained rather than fully trapped — some can adapt by aligning with the state, others retreat to informal influence. But the constraint suppresses their institutional role and extracts their legitimacy capital. The state's monopoly on violence forecloses their traditional adjudication mechanisms.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE APPARATUS (ROPE) — The state experiences the constraint as coordination: imposing uniform norms solves the genuine problem of legal predictability and administrative legibility across diverse populations. From this perspective, the coercive mechanism is a necessary coordination tool, not extraction. The state has full exit options (can revise norms, delegate enforcement, or withdraw from peripheral regions) and benefits from the arrangement through enhanced administrative capacity and tax collection.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALIGNED ELITE FACTION (TANGLED ROPE) — Local elites who align with state norms to preserve their own position. They benefit from state backing (coordination function: their property rights and contracts are now state-enforced) but also bear costs (must conform to state norms that may conflict with local custom, must pay taxes, must suppress local rivals who resist). Mixed experience: genuine coordination benefit plus asymmetric extraction. Mobile exit options: can relocate capital or shift allegiance if the state weakens.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMIST COALITION (SCAFFOLD) — Organized groups (modernizing bureaucrats, nationalist movements, legal reformers) who see state imposition as a temporary mechanism to break entrenched traditional structures. The coercion is justified as transitional: once the new norms are internalized across a generation, enforcement can be relaxed. This perspective sees a sunset — the goal is to reach a state where compliance is habitual rather than coerced. The scaffold logic: we use the state's monopoly on violence now to build the institutional infrastructure that will make violence unnecessary later.
constraint_indexing:constraint_classification(exogenous_override_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, state-imposed norms exhibit both coordination and extraction. The coordination function is real: legal uniformity, contract enforcement, and administrative legibility solve genuine collective action problems that fragmented customary systems could not. But the extraction is also real: the state suppresses alternative authority structures, extracts compliance through threat rather than consent, and benefits asymmetrically (state capacity increases while subject populations bear conformity costs). The analytical classification is Tangled Rope because both functions are structurally present and neither can be reduced to the other.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state extracts compliance through coercion rather than consent. Subjects bear behavioral conformity costs, monitoring costs (self-censorship, performance of loyalty), and direct punishment costs (fines, imprisonment, violence) when they fail to comply. The extraction is not total (0.68 rather than 0.85+) because some subjects do benefit from legal predictability and contract enforcement — the coordination function is real even when coerced. The extractiveness declines slightly over the 50-year interval (0.75 → 0.65) as partial internalization occurs and some subjects begin to see the norms as legitimate rather than purely imposed. Suppression (0.82): Very high. The state must actively monitor compliance and credibly threaten punishment. Exit options are severely constrained: geographic exit (emigration) is expensive and often blocked; voice (political opposition) is suppressed; loyalty (internalization) is coerced rather than voluntary. Suppression declines modestly over time (0.90 → 0.78) as habitual compliance reduces the need for active enforcement, but remains high because the legitimacy deficit persists — compliance is still conditional on state capacity to punish. Theater ratio (0.45): Moderate and rising. Initial imposition requires genuine violence (low theater at t=0: 0.35). As the system matures, enforcement becomes increasingly ritualized: checkpoints, inspections, loyalty oaths, and paperwork perform state authority without requiring actual violence in most cases. The theater ratio rises to 0.50 by t=50 as the coercive apparatus becomes institutionalized and much enforcement becomes performative. But theater remains below piton threshold (0.60+) because the underlying violence capacity is real and periodically demonstrated.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The state apparatus sees coordination (Rope) — uniform norms solve genuine administrative problems. The subject population sees extraction (Snare) — compliance is coerced with no genuine benefit. The aligned elite faction sees mixed coordination and extraction (Tangled Rope) — they benefit from state backing but bear conformity costs. The reformist coalition sees a temporary scaffold — coercion now, internalization later. The analytical observer sees an irreducible Tangled Rope — both coordination and extraction are structurally present. The gap reveals that 'legitimacy' is not a property of the norm itself but of the observer's structural position: those who benefit see legitimacy; those who are coerced do not. The state's monopoly on violence does not create legitimacy — it overrides the need for legitimacy by making non-compliance too costly.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the primary beneficiary: it collects administrative capacity, tax revenue, and the ability to conscript labor and soldiers. Directionality for the state is near zero (full beneficiary). Aligned elite factions are secondary beneficiaries: they gain state enforcement of their property rights and contracts, but they also bear costs (taxes, conformity to state norms that may conflict with local custom). Directionality for aligned elites is moderate (mixed beneficiary/payer). Subject populations are the primary victims: they bear compliance costs under threat of punishment with minimal benefit. Directionality for subjects is near 1.0 (full target). Displaced cultural authorities are secondary victims: their institutional role is suppressed and their legitimacy capital is extracted. Directionality for displaced authorities is high (0.7-0.8). The reformist coalition has moderate directionality (0.4-0.5): they bear costs now (must support coercive enforcement) but expect future benefits (modern institutional infrastructure). The analytical observer has neutral directionality (0.5): neither collecting nor paying, but recognizing both functions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the state's coordination function (legal uniformity, contract enforcement) and its extraction function (coerced compliance, suppression of alternatives) are structurally inseparable in the exogenous override reading. The state is not 'merely coordinating' — it is coordinating through coercion, and the coercion extracts compliance costs from subjects who do not endorse the norms. But the state is also not 'purely extracting' — the legal predictability and administrative capacity it provides are genuine coordination benefits, even when coercively imposed. The Tangled Rope classification at the analytical level captures this irreducibility: you cannot have the coordination without the extraction in this reading. The scaffold perspective (reformist coalition) offers a potential resolution: if the coercion is genuinely transitional and internalization occurs, the extraction mechanism sunsets and the constraint becomes a Rope. But the omega variable (internalization_timeline) leaves this unresolved: does internalization always occur, or does the extraction mechanism persist indefinitely?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_timeline,
    'How many generations does it take for coercively imposed norms to become internalized as legitimate, and does internalization always occur?',
    'Historical case studies comparing compliance patterns across generations; measurement of enforcement intensity required to maintain compliance over time; survey data on perceived legitimacy of state norms in post-colonial or post-authoritarian contexts.',
    'If internalization occurs within 2-3 generations and is robust: scaffold perspective is vindicated — coercion is genuinely transitional. If internalization is incomplete or requires continuous enforcement: snare perspective is vindicated — the extraction mechanism is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_timeline, empirical, 'Timeline and completeness of norm internalization after coercive imposition').

omega_variable(
    coordination_vs_legibility_extraction,
    'Is the state''s coordination function (legal predictability, contract enforcement) separable from its legibility extraction (surveillance, taxation, conscription), or are they structurally coupled?',
    'Comparative analysis of states with high coordination capacity but low extraction (if any exist) vs states with high extraction and low coordination. Historical cases of state collapse: does coordination infrastructure survive when extraction mechanisms fail?',
    'If separable: the coordination function could be preserved while reducing extraction, supporting a rope-toward-scaffold trajectory. If structurally coupled: the tangled rope is irreducible — you cannot have the coordination without the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_legibility_extraction, conceptual, 'Whether state coordination and legibility extraction are structurally separable').

omega_variable(
    alternative_reading_kernel_ambiguity,
    'Is this constraint (exogenous override via state coercion) one reading of a contested kernel about norm imposition mechanisms, or is it a distinct constraint from endogenous cultural climb?',
    'Kernel identity test: do the sibling readings (endogenous climb, hybrid legitimation) share a stable base extractiveness value when measured against the same observable (e.g., compliance cost borne by subjects), or do they produce different epsilon values? If epsilon varies across readings, they are distinct constraints and should not be modeled as kernel readings.',
    'If same epsilon across readings: kernel framing is correct, and the readings differ only in authority grounding and legitimacy source. If different epsilon: the readings are distinct constraints (state coercion has higher epsilon than cultural diffusion), and the kernel framing collapses — decompose into separate constraint families.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_kernel_ambiguity, conceptual, 'Whether exogenous override and endogenous climb are readings of one kernel or distinct constraints').

omega_variable(
    violence_monopoly_necessity,
    'Is the state''s monopoly on violence a necessary condition for norm imposition, or is it sufficient? Can norms be exogenously imposed without violence (e.g., through economic dependency, information control, or social ostracism)?',
    'Case studies of norm imposition in contexts with weak state violence capacity but strong alternative coercion mechanisms (colonial indirect rule, corporate company towns, religious excommunication). Measurement: correlation between enforcement violence intensity and compliance rates.',
    'If violence monopoly is necessary: the constraint is correctly scoped to state formation contexts. If sufficient but not necessary: the constraint is broader (any coercive authority can impose norms exogenously), and the state-specific framing is too narrow. If neither necessary nor sufficient: the constraint''s causal mechanism is misspecified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_monopoly_necessity, empirical, 'Whether violence monopoly is necessary, sufficient, both, or neither for exogenous norm imposition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_theater_t0, exogenous_override_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exog_theater_t10, exogenous_override_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(exog_theater_t25, exogenous_override_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(exog_theater_t50, exogenous_override_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(exog_extract_t0, exogenous_override_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(exog_extract_t10, exogenous_override_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(exog_extract_t25, exogenous_override_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(exog_extract_t50, exogenous_override_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(exog_suppress_t0, exogenous_override_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(exog_suppress_t10, exogenous_override_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(exog_suppress_t25, exogenous_override_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(exog_suppress_t50, exogenous_override_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% The exogenous override reading is one of three sibling readings of the imposition_mechanism_kernel. The three readings share a kernel (norm imposition mechanism) but differ in authority grounding, enforcement requirements, and extractiveness. If the epsilon-invariance test reveals that the readings have substantially different epsilon values when measured against the same observable, the kernel framing collapses and they should be decomposed into separate constraint families rather than modeled as readings of a single kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
