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
 *   constraint_id: hybrid_legitimation_reading
 *   human_readable: Hybrid Legitimation Through Symbolic Authority Transfer and Institutional Incentives
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint models the mechanism by which new norms achieve
 *   legitimacy through a hybrid process combining symbolic authority transfer
 *   (the emperor's example) with institutional incentives (career
 *   advancement, resource access, status for adopters). The constraint is one
 *   reading of a contested kernel about how norms are imposed or adopted in
 *   state formation. The hybrid reading occupies the middle position between
 *   two sibling readings: the endogenous_climb_reading (populations
 *   voluntarily adopt because norms solve their problems) and the
 *   exogenous_override_reading (imperial coercion overrides local
 *   preferences). The hybrid reading asserts that both mechanisms are
 *   necessary and structurally entangled: the imperial example provides
 *   legitimacy that makes adoption seem voluntary, while institutional
 *   incentives ensure that elites adopt first, creating a cascade that
 *   pressures non-adopters. The constraint exhibits stratified adoption
 *   (elites first, masses later) and moderate enforcement costs (suppression
 *   requirement declines over time as adoption spreads). The theater ratio
 *   rises over the interval as the legitimacy apparatus becomes increasingly
 *   performative — the original coordination function is achieved, but the
 *   ceremonial machinery persists through institutional inertia.
 *
 * KEY AGENTS:
 *   - Imperial Center: Primary beneficiary (institutional/arbitrage) — provides symbolic authority frame; captures coordination benefits with minimal enforcement cost
 *   - Elite Adopters: Secondary beneficiary and enforcer (organized/constrained) — benefit from early adoption and institutional incentives; enforce adoption downward through their positions
 *   - Traditional Authority Holders: Primary victim (moderate/constrained) — lose autonomy and status as imperial authority displaces traditional legitimacy; constrained by institutional incentives to adopt
 *   - Non-Adopting Populations: Secondary victim (powerless/trapped) — face coercive pressure to adopt without voice or exit; trapped in the constraint's spatial scope
 *   - Legitimacy Apparatus: Institutional actor (institutional/arbitrage) — maintains symbolic authority transfer through court rituals and ceremonial performances; becomes increasingly performative over time
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the hybrid mechanism as a universal law of state formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hybrid_legitimation_reading, 0.48).
domain_priors:suppression_score(hybrid_legitimation_reading, 0.42).
domain_priors:theater_ratio(hybrid_legitimation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hybrid_legitimation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hybrid_legitimation_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hybrid_legitimation_reading, tangled_rope).
narrative_ontology:human_readable(hybrid_legitimation_reading, "Hybrid Legitimation Through Symbolic Authority Transfer and Institutional Incentives").
narrative_ontology:topic_domain(hybrid_legitimation_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(hybrid_legitimation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hybrid_legitimation_reading, 'bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9').
narrative_ontology:cs_kernel_codification('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', distributed).
narrative_ontology:cs_authority_grounding('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', extraction).
narrative_ontology:cs_interpretation_layer_present('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9').
narrative_ontology:cs_reading_relation('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', hybrid_legitimation_reading__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', hybrid_legitimation_reading__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', foundational, symbolic_authority_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(symbolic_authority_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', symbolic_authority_necessary_for_legitimacy, conventional).
narrative_ontology:cs_axiom('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', foundational, institutional_incentives_drive_elite_adoption).
narrative_ontology:cs_axiom_status(institutional_incentives_drive_elite_adoption, holdable).
narrative_ontology:cs_axiom_grounding('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', institutional_incentives_drive_elite_adoption, empirically_contingent).
narrative_ontology:cs_reference_frame('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', imperial_authority_legitimacy_frame).
narrative_ontology:cs_drift_state('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bd912190-7c01-4e7b-a7f2-e8ccba2ce0e9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(hybrid_legitimation_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, imperial_center).
narrative_ontology:constraint_beneficiary(hybrid_legitimation_reading, elite_adopters).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, non_adopting_populations).
narrative_ontology:constraint_victim(hybrid_legitimation_reading, traditional_authority_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provides the symbolic authority frame through imperial example and ceremonial performance. Sets the agenda for norm adoption across the realm. Captures coordination benefits with minimal enforcement cost. Can exit the constraint by withdrawing imperial authority, but chooses to maintain it because it solves the problem of norm diffusion.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, imperial_center, agenda_setter,
    institutional, immediate, arbitrage, global).

% Adopt the new norms early and enforce adoption downward through their institutional positions. Benefit from status, resource access, and career advancement tied to adoption. Constrained by dependence on imperial favor and loss of traditional autonomy. Enforce the constraint on non-adopters through institutional incentives and coercive pressure.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, elite_adopters, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hybrid_legitimation_reading, elite_adopters, beneficiary).

% Lose status and autonomy as imperial authority displaces traditional legitimacy. Face institutional incentives to adopt new norms (career advancement, resource access) that require abandoning traditional authority. Constrained by the need to maintain position in the new institutional order. Bear costs of norm transition without capturing benefits.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, traditional_authority_holders, payer,
    moderate, generational, constrained, regional).

% Face coercive pressure to adopt new norms without meaningful voice or exit. Trapped in the constraint's spatial scope with no alternative institutional order to join. Experience the imperial example as a legitimacy cascade that marginalizes resistance. Bear the costs of norm transition without capturing benefits.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, non_adopting_populations, payer,
    powerless, biographical, trapped, regional).

% Maintains the symbolic authority transfer through court rituals, official histories, and ceremonial performances. Provides the institutional machinery that makes the imperial example credible and diffuses it across the realm. Becomes increasingly performative over time as the original coordination function is achieved but the apparatus persists through institutional inertia.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, legitimacy_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Observes the constraint from a civilizational perspective and risks naturalizing the hybrid mechanism as a universal law of state formation. Sees norm diffusion through symbolic authority as an inherent feature of large-scale societies rather than a contingent institutional arrangement. Subject to the oracle gap: the analytical position's native instruments cannot detect the structure that cross-position analysis reveals.
narrative_ontology:constraint_stakeholder(hybrid_legitimation_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Diffuse new norms across heterogeneous populations in a large-scale state without requiring direct coercion of every individual. The imperial example provides a legitimacy frame that makes adoption seem voluntary, while institutional incentives ensure elite adoption and cascade pressure on non-adopters.
% TRANSFER_FUNCTION: Status, resources, and career advancement flow from non-adopters and traditional authority holders to elite adopters and the imperial center. Legitimacy flows from the imperial center to elites to populations. Autonomy flows away from traditional authority holders toward the imperial center.
% ABSENT_VOICES: Populations that would resist norm adoption if they had voice and exit options. Marginalized traditional authorities whose legitimacy is displaced. Alternative institutional orders that might solve the coordination problem without imperial authority. These voices are excluded from the agenda-setting process and their absence enables the constraint to persist.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight, the imperial authority structure would collapse and populations would revert to traditional norms or develop alternative coordination mechanisms. The new norms depend on the imperial example and institutional incentives for their persistence — they are not self-sustaining. The coordination problem (diffusing norms across heterogeneous populations) would remain, but the solution would change.
% FOUNDING_PROBLEM: Large-scale states require mechanisms to coordinate behavior across heterogeneous populations with different local traditions and interests. Direct coercion is costly and unstable. The founding problem is: how can a center impose new norms without bearing the full cost of enforcement?
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical analysis of state formation across multiple societies shows that norm diffusion through symbolic authority is a recurring solution to this problem. Anthropological and historical scholarship documents the role of imperial example and institutional incentives in norm adoption. However, alternative mechanisms (grassroots adoption, coercive imposition, institutional evolution) also exist, suggesting the founding problem is live but not uniquely solved by the hybrid mechanism.
narrative_ontology:disappearance_verdict(hybrid_legitimation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hybrid_legitimation_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-ADOPTING POPULATIONS (SNARE) — Powerless agents trapped in the constraint's spatial scope. Face coercive pressure to adopt new norms without meaningful exit or voice in the adoption process. The imperial example creates a legitimacy cascade that marginalizes resistance. Maximum experienced extraction — no structural agency.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRADITIONAL AUTHORITY HOLDERS (TANGLED ROPE) — Moderate power agents constrained by the institutional incentive structure. Experience both coordination (the new norms do solve collective action problems) and extraction (their traditional authority is displaced by imperial charisma). Career and status incentives push toward adoption despite loss of autonomy. Significant but not maximal extraction.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMPERIAL CENTER (ROPE) — Institutional actor with arbitrage options. Experiences the constraint as pure coordination: the symbolic authority transfer solves the problem of norm diffusion across heterogeneous populations. The imperial example provides a low-cost coordination mechanism. Net beneficiary with genuine coordination function — extraction runs toward this agent.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ELITE ADOPTERS (TANGLED ROPE) — Organized agents with constrained exit. Benefit from early adoption (status, institutional position, resource access) while also bearing costs (loss of traditional autonomy, dependence on imperial favor). The institutional incentives create a coalition that enforces adoption downward. Moderate extraction with genuine coordination function.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMACY APPARATUS (PITON) — The institutional machinery that maintains the symbolic authority transfer (court rituals, official histories, ceremonial performances) becomes increasingly performative over time. The original coordination function (norm diffusion) is achieved; the apparatus persists through inertia and theatrical maintenance of imperial charisma. Theater ratio rises as the apparatus becomes decoupled from actual norm enforcement.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, norm diffusion through symbolic authority is a universal feature of state formation: all large-scale societies require mechanisms to coordinate behavior across heterogeneous populations. This perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will identify this as naturalization of a hybrid extraction-coordination mechanism.
constraint_indexing:constraint_classification(hybrid_legitimation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hybrid_legitimation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hybrid_legitimation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hybrid_legitimation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hybrid_legitimation_reading, TR),
    TR >= 0.70.

:- end_tests(hybrid_legitimation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from non-adopters and traditional authority holders while providing genuine coordination benefits to the imperial center and elite adopters. The extraction is not maximal because the coordination function is real — the new norms do solve collective action problems — but it is substantial because the benefits are asymmetrically distributed. The measurement trajectory shows rising extractiveness (0.35 → 0.52) as the constraint matures and the imperial center consolidates control. Suppression (0.42): Moderate. Initial suppression is high (0.55) because non-adopters face coercive pressure, but suppression declines (0.38) as adoption spreads and becomes normalized. This declining trajectory reflects the hybrid mechanism: coercion is front-loaded, but as elites adopt and create institutional incentives, the constraint becomes self-enforcing through career incentives rather than direct coercion. Theater ratio (0.58): Moderate-high and rising. The legitimacy apparatus begins with genuine coordination function (theater ratio 0.42) but becomes increasingly performative (0.65) as the constraint matures. The ceremonial machinery persists through institutional inertia even after the original coordination problem is solved.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon appears as pure coordination (Rope from the imperial center's perspective), mixed coordination-extraction (Tangled Rope from elite and traditional authority perspectives), pure extraction (Snare from non-adopters' perspective), and degraded ritual (Piton from the legitimacy apparatus's perspective). The perspectival gap reveals the hybrid mechanism: the imperial center genuinely solves a coordination problem (how to diffuse norms across heterogeneous populations), but the solution asymmetrically benefits elites and extracts from non-adopters. The analytical observer risks collapsing this perspectival diversity into a false summit (natural law of state formation), but the structural data reveals a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the extraction flow. The imperial center benefits from the constraint (d ≈ 0.2, low extraction) because they provide the legitimacy frame and capture coordination benefits. Elite adopters benefit from early adoption but are constrained by dependence on imperial favor (d ≈ 0.4, moderate extraction). Traditional authority holders lose status and autonomy (d ≈ 0.6, high extraction). Non-adopters face coercive pressure with no exit (d ≈ 0.85, maximum extraction). The engine computes effective extraction (χ) from these d values modulated by power level and exit options: powerless trapped agents experience maximum χ; institutional arbitrage agents experience minimum χ. The hybrid classification depends on the genuine mixing of coordination and extraction mechanisms — if either mechanism dominated, the constraint would reclassify toward pure Rope or pure Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves mandatrophy by asserting that the constraint's mandate (diffuse new norms across heterogeneous populations) remains live and functional throughout the interval. The constraint does not outlive its function — the legitimacy apparatus persists because norm diffusion remains an ongoing problem in state formation. However, the theater ratio rises over time, indicating that the apparatus becomes increasingly performative as the original coordination problem is solved. This is not mandatrophy (mandate outliving function) but rather institutional inertia (apparatus persisting beyond strict necessity). The piton perspective captures this: the ceremonial machinery is maintained through institutional momentum, not because it is strictly necessary for coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_coercive_boundary,
    'What proportion of norm adoption is driven by genuine persuasion through imperial example versus coercive pressure from institutional incentives?',
    'Historical analysis of adoption patterns: do populations adopt norms when imperial authority is distant/weak, or only when enforcement capacity is present? Comparative analysis across regions with varying enforcement intensity.',
    'If primarily symbolic: constraint reclassifies toward Rope (pure coordination). If primarily coercive: constraint reclassifies toward Snare (pure extraction). The hybrid classification depends on genuine mixing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_coercive_boundary, empirical, 'Boundary between symbolic persuasion and coercive pressure in norm adoption').

omega_variable(
    elite_capture_vs_genuine_coordination,
    'Do the institutional incentives that drive elite adoption actually solve a genuine collective action problem, or do they primarily serve elite interests while creating the appearance of coordination?',
    'Comparative analysis of outcomes: do populations that adopt the new norms experience improved coordination on public goods, security, or resource allocation? Or do outcomes primarily benefit elites with minimal spillover to non-adopters?',
    'If genuine coordination: Tangled Rope classification confirmed (mixed coordination and extraction). If primarily elite capture: constraint reclassifies toward Snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_vs_genuine_coordination, empirical, 'Whether institutional incentives solve genuine collective action problems or primarily serve elite interests').

omega_variable(
    reading_contest_endogenous_vs_hybrid,
    'Is this constraint better understood as endogenous norm climb (populations voluntarily adopt because the norms solve their problems) or as hybrid legitimation (imperial authority provides the legitimacy frame while institutional incentives drive adoption)?',
    'This is the core committer-axis question: the three readings (endogenous_climb_reading, exogenous_override_reading, hybrid_legitimation_reading) represent genuinely different structural claims about the same historical phenomenon. Resolution requires examining whether populations would adopt the norms absent imperial authority, and whether they would resist absent institutional incentives.',
    'If endogenous climb dominates: reclassify to endogenous_climb_reading (Rope from most perspectives). If exogenous override dominates: reclassify to exogenous_override_reading (Snare from most perspectives). The hybrid reading is the middle position: both mechanisms are necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_endogenous_vs_hybrid, conceptual, 'Kernel reading contest: endogenous climb vs. hybrid legitimation vs. exogenous override').

omega_variable(
    false_summit_naturalization,
    'Is the analytical observer''s mountain classification a genuine natural law of state formation, or a false summit that naturalizes a contingent institutional arrangement?',
    'Comparative historical analysis: do all large-scale societies use symbolic authority transfer combined with institutional incentives? Or are there alternative mechanisms for norm diffusion that achieve similar coordination without this specific hybrid structure?',
    'If universal: mountain classification is legitimate. If contingent: false summit detector fires, reclassifying to tangled_rope or snare depending on extraction magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether norm diffusion through symbolic authority is a universal natural law or a contingent institutional arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hybrid_legitimation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_legit_tr_t0, hybrid_legitimation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hybrid_legit_tr_t10, hybrid_legitimation_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(hybrid_legit_tr_t20, hybrid_legitimation_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(hybrid_legit_tr_t30, hybrid_legitimation_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(hybrid_legit_be_t0, hybrid_legitimation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hybrid_legit_be_t10, hybrid_legitimation_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hybrid_legit_be_t20, hybrid_legitimation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(hybrid_legit_be_t30, hybrid_legitimation_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_legit_su_t0, hybrid_legitimation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hybrid_legit_su_t10, hybrid_legitimation_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(hybrid_legit_su_t20, hybrid_legitimation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(hybrid_legit_su_t30, hybrid_legitimation_reading, suppression_requirement, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hybrid_legitimation_reading, identity_coordination).
narrative_ontology:affects_constraint(hybrid_legitimation_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(hybrid_legitimation_reading, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The imposition_mechanism_kernel has three structurally distinct readings with different ε values and classification profiles. The hybrid_legitimation_reading (this constraint) asserts that both symbolic authority transfer and institutional incentives are necessary. The endogenous_climb_reading emphasizes voluntary adoption driven by genuine coordination benefits. The exogenous_override_reading emphasizes coercive imposition. These are not three perspectives on one constraint but three different constraints instantiating different readings of the same kernel. Each has its own ε, its own beneficiary/victim structure, and its own classification profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
