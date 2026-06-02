% ============================================================================
% CONSTRAINT STORY: selective_retention_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_selective_retention_necessity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: selective_retention_necessity
 *   human_readable: Selective Retention Necessity in Thinned Social Environments
 *   domain: social_philosophy/trust_theory/relational_ethics
 *
 * SUMMARY:
 *   Under conditions of structural thinning — where categorical trust
 *   structures (extended family, stable institutions, rooted communities)
 *   have eroded — the fundamental unit of trust shifts from category to
 *   specific individual. Selective retention is the mechanism by which agents
 *   allocate scarce attention and binding commitment to particular people,
 *   built one at a time through accumulated observation of reliability across
 *   repeated interactions. This constraint exhibits the core tangled rope
 *   structure: it solves a genuine coordination problem (how to allocate
 *   trust when categorical heuristics have failed) while simultaneously
 *   creating asymmetric extraction (those without access to stable
 *   observation conditions cannot build the relational infrastructure that
 *   selective retention provides). The constraint's extractiveness has
 *   increased over the 30-year interval as thinning has accelerated:
 *   geographic mobility, economic precarity, and institutional fragmentation
 *   have reduced the proportion of the population with access to the stable
 *   social contexts required for trust accumulation. Theater ratio has also
 *   increased as performative trust signals (social media connection counts,
 *   professional networking rituals, surface-level relationship maintenance)
 *   have proliferated without corresponding depth of observation.
 *
 * KEY AGENTS:
 *   - Isolated Individual Without Observation Access: Primary victim (powerless/trapped) — lacks stable social context for trust accumulation; must navigate high-stakes decisions without relational infrastructure
 *   - Selective Retainer: Mixed position (moderate/constrained) — benefits from coordination function but bears cognitive load and opportunity cost of individuated relationship maintenance
 *   - Successfully Retained Individual: Primary beneficiary (institutional/arbitrage) — accumulated trust capital enables access to opportunities and reciprocal trust across networks
 *   - Intentional Community Builder: Organized agent (organized/mobile) — attempts to rebuild categorical trust structures; sees selective retention as both necessary filter and extractive barrier
 *   - Reputation System Designer: Institutional actor (institutional/mobile) — building technological substitutes for personal observation; sees selective retention as temporary problem with technological sunset
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes irreducible hybrid of genuine coordination and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(selective_retention_necessity, 0.58).
domain_priors:suppression_score(selective_retention_necessity, 0.62).
domain_priors:theater_ratio(selective_retention_necessity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(selective_retention_necessity, extractiveness, 0.58).
narrative_ontology:constraint_metric(selective_retention_necessity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(selective_retention_necessity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(selective_retention_necessity, tangled_rope).
narrative_ontology:human_readable(selective_retention_necessity, "Selective Retention Necessity in Thinned Social Environments").
narrative_ontology:topic_domain(selective_retention_necessity, "social_philosophy/trust_theory/relational_ethics").

domain_priors:requires_active_enforcement(selective_retention_necessity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(selective_retention_necessity, those_with_observation_access).
narrative_ontology:constraint_beneficiary(selective_retention_necessity, successfully_retained_individuals).
narrative_ontology:constraint_victim(selective_retention_necessity, those_without_observation_conditions).
narrative_ontology:constraint_victim(selective_retention_necessity, categorically_excluded_groups).
narrative_ontology:constraint_victim(selective_retention_necessity, late_arrivals_to_thinned_networks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISOLATED INDIVIDUAL (SNARE) — Lacks the stable social context required to accumulate trust observations. Geographic mobility, economic precarity, or institutional exclusion prevent the repeated interactions necessary to build specific trusted relationships. Cannot exit the necessity of trust-building but lacks access to the mechanism. Experiences maximum extraction: must navigate high-stakes decisions without the relational infrastructure that selective retention provides to others.
constraint_indexing:constraint_classification(selective_retention_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SELECTIVE RETAINER (TANGLED ROPE) — Has sufficient stability to accumulate trust observations and build a network of specifically trusted individuals. Benefits from the coordination function: selective retention solves the genuine problem of allocating scarce attention and binding commitment under conditions where categorical trust (family, institution, community) has thinned. But also bears extraction: the necessity of constant evaluation, the cognitive load of maintaining individuated relationships, the risk of misjudgment, and the opportunity cost of relationships not pursued. Mixed experience: genuine coordination benefit with embedded extraction.
constraint_indexing:constraint_classification(selective_retention_necessity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUCCESSFULLY RETAINED INDIVIDUAL (ROPE) — Has been selected into multiple trust networks through demonstrated reliability. Experiences the constraint as coordination: their accumulated reputation enables access to opportunities, resources, and reciprocal trust. Net beneficiary: the selective retention mechanism runs toward them, not away from them. Can arbitrage between networks due to portable trust capital.
constraint_indexing:constraint_classification(selective_retention_necessity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTENTIONAL COMMUNITY BUILDER (TANGLED ROPE) — Organized agents attempting to rebuild categorical trust structures (cohousing, mutual aid networks, professional guilds, religious communities) see selective retention as both necessary coordination and extractive barrier. They benefit from the mechanism when recruiting members (selective retention filters for commitment) but also recognize it as extraction when it excludes those who lack observation access. Mobile exit: can shift between community-building strategies, but cannot escape the underlying necessity of trust-building under thinned conditions.
constraint_indexing:constraint_classification(selective_retention_necessity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REPUTATION SYSTEM DESIGNER (SCAFFOLD) — Institutional actors building technological or institutional reputation systems (credit scores, professional licensing, platform ratings, blockchain identity) see selective retention necessity as a temporary coordination problem with a sunset: formalized reputation mechanisms can substitute for accumulated personal observation, reducing the extraction on those without observation access. The sunset logic: as reputation becomes portable and verifiable through third-party systems, the necessity of building trust through direct repeated interaction declines. Estimated sunset: 20-40 years for mature reputation infrastructure to reduce selective retention extraction.
constraint_indexing:constraint_classification(selective_retention_necessity, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, selective retention is both a genuine coordination mechanism (solving the problem of trust allocation under thinned conditions) and an extractive barrier (concentrating relational capital among those with observation access while excluding those without). The analytical view recognizes the structural necessity (you cannot trust everyone equally when categorical structures have thinned) while also identifying the asymmetric extraction (those without stable social contexts bear disproportionate costs). Tangled Rope classification reflects the irreducible hybrid: this is not a false summit naturalizing contingent extraction, nor is it pure coordination — it is genuinely both.
constraint_indexing:constraint_classification(selective_retention_necessity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(selective_retention_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(selective_retention_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(selective_retention_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(selective_retention_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(selective_retention_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from those without observation access — they bear the costs of trust necessity without access to the mechanism. But extraction is not maximal because selective retention does solve a genuine coordination problem: under thinned conditions, you genuinely cannot trust everyone equally, and individuated trust-building is a real solution. The extraction comes from the asymmetric distribution of observation access, not from the mechanism itself being purely extractive. Suppression (0.62): Moderate-high. Significant barriers prevent those without observation access from building trusted relationships: geographic mobility disrupts repeated interaction, economic precarity prevents stable social participation, institutional exclusion blocks access to trust-building contexts, and late arrival to established networks faces high entry costs. But suppression is not total — some agents can and do build trust networks despite barriers, and intentional community-building creates alternative pathways. Theater ratio (0.48): Moderate. Performative trust signals have proliferated (social media connections, networking events, surface-level relationship rituals) but have not fully displaced genuine observation-based trust. The theater has increased over the interval as digital connection has created the appearance of relationship depth without corresponding observation accumulation.
 *
 * PERSPECTIVAL GAP:
 *   The isolated individual sees pure extraction (Snare) — they bear the necessity of trust-building without access to the mechanism. The selective retainer sees mixed coordination and extraction (Tangled Rope) — genuine problem-solving with embedded costs. The successfully retained individual sees coordination (Rope) — their accumulated reputation enables access. The intentional community builder sees both coordination (when recruiting committed members) and extraction (when excluding those without observation access). The reputation system designer sees a temporary problem with a technological sunset (Scaffold) — formalized reputation can substitute for personal observation. The analytical observer sees an irreducible hybrid (Tangled Rope) — genuinely both coordination and extraction, not a false summit naturalizing contingent extraction. The perspectival gap reveals that the constraint's classification depends on the agent's structural position: access to observation conditions, accumulated trust capital, and exit options determine whether the constraint appears as coordination, extraction, or hybrid.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those with observation access (stable social contexts enabling repeated interaction) and those who have been successfully retained (accumulated trust capital). Victims are those without observation access (isolated by mobility, precarity, or exclusion) and those categorically excluded from observation opportunities (late arrivals to established networks, members of stigmatized groups facing higher observation thresholds). The selective retainer occupies a mixed position: benefits from the coordination function but bears the extraction of constant evaluation and opportunity cost. The successfully retained individual is a net beneficiary: the mechanism runs toward them. The isolated individual is a net victim: the mechanism runs away from them. Directionality is derived from these structural relationships combined with exit options: trapped agents with no observation access experience maximum extraction; mobile agents with observation access experience low extraction; constrained agents experience moderate extraction with mixed benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that selective retention is genuinely both coordination and extraction, not one masquerading as the other. It is NOT a false summit (a snare claiming to be a mountain) — the coordination function is real: under thinned conditions, individuated trust-building does solve the problem of allocating scarce attention and binding commitment. It is NOT pure coordination (a rope) — the extraction is real: those without observation access bear disproportionate costs and cannot access the relational infrastructure the mechanism provides. The tangled rope classification captures the irreducible hybrid: you cannot remove the extraction without destroying the coordination (if everyone could be trusted equally, selective retention would be unnecessary), and you cannot remove the coordination without abandoning the problem (under thinned conditions, some mechanism for trust allocation is necessary). The mandatrophy is resolved by recognizing that the constraint's dual nature is structural, not rhetorical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observation_threshold_sufficiency,
    'What quantity and quality of repeated interaction constitutes sufficient observation to justify selective retention of a specific trusted individual?',
    'Empirical analysis of trust calibration accuracy: correlation between observation duration/depth and subsequent reliability; identification of minimum viable observation conditions',
    'If threshold is low (few interactions suffice): selective retention is efficient coordination with minimal extraction. If threshold is high (extensive observation required): extraction on those without observation access is severe and the mechanism becomes exclusionary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observation_threshold_sufficiency, empirical, 'Minimum observation threshold for justified selective retention').

omega_variable(
    reputation_system_substitutability,
    'Can formalized reputation systems (credit scores, platform ratings, professional credentials) actually substitute for accumulated personal observation in high-stakes trust decisions?',
    'Comparative analysis of trust outcomes: decisions based on personal observation vs decisions based on formalized reputation metrics; identification of domains where substitution succeeds vs fails',
    'If substitutable: scaffold perspective confirmed — technological sunset is real and extraction declines as reputation infrastructure matures. If non-substitutable: selective retention necessity persists regardless of technological development, and the scaffold perspective is aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reputation_system_substitutability, empirical, 'Whether reputation systems can substitute for personal observation').

omega_variable(
    categorical_trust_recovery_possibility,
    'Is the thinning of categorical trust structures (family, institution, community) a reversible historical contingency or an irreversible structural shift?',
    'Historical analysis of trust structure evolution; identification of conditions under which categorical trust has been rebuilt after thinning; assessment of whether contemporary thinning differs structurally from historical precedents',
    'If reversible: selective retention is a temporary adaptation to contingent social conditions, and intentional community-building can restore categorical trust. If irreversible: selective retention is the permanent coordination mechanism for post-categorical social environments, and extraction on those without observation access is structural rather than contingent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_trust_recovery_possibility, conceptual, 'Whether categorical trust thinning is reversible').

omega_variable(
    misjudgment_asymmetry,
    'Does selective retention produce asymmetric costs for misjudgment — higher costs for false negatives (excluding trustworthy individuals) vs false positives (retaining untrustworthy individuals)?',
    'Analysis of trust decision error costs across different social positions; identification of whether exclusion errors or inclusion errors dominate harm; assessment of whether error asymmetry varies by power position',
    'If false negatives dominate: selective retention is more extractive than coordination (excludes more trustworthy people than it protects against untrustworthy ones). If false positives dominate: selective retention is more coordination than extraction (protects against harm more than it excludes benefit). If asymmetry varies by position: extraction is concentrated on specific groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(misjudgment_asymmetry, empirical, 'Asymmetry of misjudgment costs in selective retention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(selective_retention_necessity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(selret_theater_t0, selective_retention_necessity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(selret_theater_t15, selective_retention_necessity, theater_ratio, 15, 0.42).
narrative_ontology:measurement(selret_theater_t30, selective_retention_necessity, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(selret_extract_t0, selective_retention_necessity, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(selret_extract_t15, selective_retention_necessity, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(selret_extract_t30, selective_retention_necessity, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(selective_retention_necessity, attachment_coordination).

% DUAL FORMULATION NOTE:
% Selective retention necessity is downstream of structural_thinning_convergence (the erosion of categorical trust structures creates the necessity for individuated trust-building) and epistemic_dataset_construction (the accumulated observations that enable selective retention are themselves a form of dataset construction under resource constraints). The upstream constraints have their own extractiveness values reflecting the structural and epistemic dynamics; selective retention has its own extractiveness reflecting the asymmetric distribution of observation access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
