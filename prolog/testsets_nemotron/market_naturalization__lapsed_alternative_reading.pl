% ============================================================================
% CONSTRAINT STORY: market_naturalization__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__lapsed_alternative_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__lapsed_alternative_reading
 *   human_readable: Market Dominance as Lapsed Closure (No Active Maintenance)
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'lapsed_alternative_reading' of
 *   the market_naturalization kernel: the claim that market dominance
 *   persists not because it is actively defended by identifiable
 *   beneficiaries, but because the alternatives that would contest it have
 *   atrophied through non-use. The arrangement presents as a Mountain — high
 *   accessibility collapse (0.87), near-zero resistance (0.06), low
 *   extractiveness (0.12) and suppression (0.18) — but the Mountain claim
 *   rests on the absence of live alternatives rather than a natural law. The
 *   structural delta from sibling readings is precise:
 *   beneficiary_maintained_reading declares active beneficiaries (incumbent
 *   capital holders) and enforcement; hybrid_reading declares both lapsed and
 *   active elements; THIS reading declares neither active beneficiaries nor
 *   active enforcement, only the ghost of a coordination function that once
 *   justified the closure. The kernel contest is whether dominance is a
 *   Mountain (lapsed_alternative), a Snare (beneficiary_maintained), or a
 *   Tangled Rope (hybrid). This story authors the first reading cleanly, per
 *   ε-invariance: one reading, one constraint, one ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__lapsed_alternative_reading, 0.12).
domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, 0.18).
domain_priors:theater_ratio(market_naturalization__lapsed_alternative_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_naturalization__lapsed_alternative_reading, "Market Dominance as Lapsed Closure (No Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__lapsed_alternative_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__lapsed_alternative_reading, 'a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0').
narrative_ontology:cs_kernel_codification('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', distributed).
narrative_ontology:cs_authority_grounding('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', diffuse_epistemic).
narrative_ontology:cs_reading_relation('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', foundational, dominance_requires_no_active_defense).
narrative_ontology:cs_axiom_status(dominance_requires_no_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', dominance_requires_no_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', foundational, alternatives_atrophied_not_suppressed).
narrative_ontology:cs_axiom_status(alternatives_atrophied_not_suppressed, holdable).
narrative_ontology:cs_axiom_grounding('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', alternatives_atrophied_not_suppressed, empirically_contingent).
narrative_ontology:cs_reference_frame('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', competitive_market_equilibrium).
narrative_ontology:cs_drift_state('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', contemporary_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0badfc5-62bc-4a05-a6e5-928ee8fdc7f0', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(market_naturalization__lapsed_alternative_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, market_outcomes_reflect_competitive_process_not_structural_closure).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, dominance_atrophies_without_active_defense).
narrative_ontology:constraint_vindicates(market_naturalization__lapsed_alternative_reading, coordination_costs_explain_apparent_extraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Observes the market structure from outside any participant seat. Sees dominance as a stable pattern with no active enforcement machinery, no identifiable beneficiary class collecting rents, and no victim class bearing asymmetric costs. The constraint appears as a landscape feature — high accessibility collapse because no alternative coordination mechanism is instantiated, low resistance because no organized challenge exists.
narrative_ontology:constraint_stakeholder(market_naturalization__lapsed_alternative_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__lapsed_alternative_reading, diffuse).
narrative_ontology:fixing_cost_class(market_naturalization__lapsed_alternative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex production and exchange across strangers without central planning: prices, property, and competition solve the information and incentive problems that would otherwise require a planner.
% TRANSFER_FUNCTION: Moves coordination surplus to no identifiable seat — the arrangement extracts only the marginal cost of maintaining the market infrastructure (contract enforcement, standard weights and measures, dispute resolution), approximately 0.12 of transaction value, which is diffuse overhead rather than concentrated rent.
% ABSENT_VOICES: Would-be alternative coordinators (cooperative enterprises, public utility providers, mutual aid networks) are absent not because they are excluded but because they never formed at scale. The constraint did not suppress them; the historical path selected the dominant form and the alternatives atrophied through non-instantiation. They would object to the claim that dominance is natural, but they are not in the room because they were never built.
% DISAPPEARANCE_RATIONALE: If the market dominance structure vanished overnight, the world would rearrange violently: supply chains would fracture, price signals would disappear, property claims would become unenforceable. The coordination function is real and load-bearing. But the SPECIFIC dominance configuration (this firm, this market share, this pricing power) is not the coordination function — it is a historical accident that filled the coordination niche. The function would reconstitute, possibly in a different dominance configuration, possibly in a more distributed form. The verdict is world_rearranges because the constraint (market coordination) is real, even if the reading (this particular dominance is a lapsed closure) claims the current form is contingent.
% FOUNDING_PROBLEM: How to coordinate complex production and exchange among strangers without a central planner who knows all local conditions.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the entire tradition of political economy from Smith through Hayek to modern mechanism design — not by the beneficiaries of any particular dominance configuration. The problem (coordination without omniscience) is live; the specific closure (this dominance structure) is a historical solution that may have lapsed.
narrative_ontology:disappearance_verdict(market_naturalization__lapsed_alternative_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__lapsed_alternative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__lapsed_alternative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_naturalization__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__lapsed_alternative_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_naturalization__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_naturalization__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_naturalization__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_naturalization__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics describe a constraint that operates with minimal extraction and suppression, meeting almost no resistance because alternatives are not merely suppressed but absent — they atrophied. The high accessibility_collapse (0.87) reflects that once the dominance structure is understood, no alternative coordination mechanism appears viable within the current frame. The low theater_ratio (0.08) indicates little performative maintenance: the constraint persists because nobody bothers to challenge it, not because anyone performs its defense. The measurement series on a shared grid (0, 25, 50, 75, 100) shows stability: extractiveness and suppression are flat, theater is flat. This is not a constraint that is degrading or intensifying; it is a constraint that has settled into a stable, low-cost equilibrium because the contestation machinery was never built.
 *
 * PERSPECTIVAL GAP:
 *   The analytical seat sees a stable, low-extraction Mountain. A payer seat (if one existed) would experience the constraint as a coordination cost with no exit. A beneficiary seat (if one existed) would experience it as a natural subsidy. The divergence is not between existing seats but between the current reading (no seats declared) and the sibling readings (which declare seats). The engine computes per-seat classification from the structural data authored; where the sibling readings author beneficiaries and victims, their per-seat classifications will diverge from this reading's uniform near-symmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because this reading asserts no identifiable agent class structurally benefits from or is extracted by the constraint. The coordination function (if any) is diffuse and historical; the current arrangement simply persists. All agents — consumers, workers, firms, regulators — face the same structural condition: a market structure with no live outside option. Directionality is near-symmetric (d ≈ 0.5) for all seats because the constraint neither subsidizes nor extracts from any identifiable group; it merely IS the landscape. If the Mountain claim holds, this is correct. If FSM triggers (beneficiaries exist but were not declared), the engine will reclassify.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coordinating complex production and exchange without central planning — was solved by the market closure. That problem is LIVE (markets still coordinate), but the SPECIFIC CLOSURE (dominance structures) is not necessary for the solution. The mandate has atrophied: the coordination function persists, but the particular dominance configuration that claims to embody it is a lapsed closure, not a live coordination mechanism. The constraint is a piton candidate masquerading as a Mountain: the coordination function is real (markets coordinate), but the dominance structure is the atrophied remainder of a historical closure that no longer serves the function. The low theater_ratio means the masquerade is not actively performed; it is the default assumption.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint a reading of the market_naturalization kernel (lapsed_alternative_reading), distinct from beneficiary_maintained_reading and hybrid_reading?',
    'Structural comparison of beneficiary/victim declarations and enforcement requirements across the three readings; if beneficiary_maintained_reading declares active beneficiaries and enforcement while this reading declares none, the readings are structurally distinct constraints sharing a kernel label.',
    'If confirmed as a distinct reading, this constraint''s low extraction and zero beneficiaries are an authored fact about THIS reading, not a hedge across the kernel. The sibling readings instantiate separate constraints with their own ε values and stakeholder surfaces.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'This constraint is one reading of the market_naturalization kernel; its ε and beneficiary structure are reading-indexed, not kernel-averaged.').

omega_variable(
    naturalness_vs_lapsed_construction,
    'Is the apparent naturalness of market dominance a genuine Mountain (would persist without human action) or a lapsed construction whose alternatives atrophied through non-use (a false summit candidate)?',
    'Historical counterfactual: if active alternatives were reintroduced (antitrust enforcement, public options, cooperative structures), would dominance persist without active defense? If dominance collapses without maintenance, it is a lapsed construction (piton or scaffold), not a Mountain.',
    'If lapsed construction, the Mountain claim fails and the constraint reclassifies as piton (theatrical maintenance of atrophied function) or scaffold (transitional support never sunsetted). The distinction turns on whether identifiable agents benefit from the appearance of naturalness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_lapsed_construction, empirical, 'Whether the constraint''s Mountain profile reflects genuine natural law or a constructed arrangement whose alternatives were allowed to atrophy.').

omega_variable(
    coordination_cost_vs_extraction_boundary,
    'Are the measured low extraction (0.12) and low suppression (0.18) genuinely mere coordination costs, or do they conceal extraction that appears as coordination because no alternatives exist to reveal it?',
    'Introduce a live alternative (e.g., a public utility option, a cooperative platform, enforced interoperability) and measure whether the constraint''s extractiveness rises when participants have a comparison point. If extraction rises, the low baseline was an artifact of no outside option.',
    'If extraction is an artifact of no alternatives, the Mountain claim is a false summit (FSM candidate) and the constraint reclassifies as snare or tangled_rope when alternatives exist. The current metrics reflect a measurement condition, not a structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction_boundary, empirical, 'Whether low extractiveness is structural or an artifact of absent alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__lapsed_alternative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_naturalization_lapsed_tr_t0, market_naturalization__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(market_naturalization_lapsed_tr_t25, market_naturalization__lapsed_alternative_reading, theater_ratio, 25, 0.06).
narrative_ontology:measurement(market_naturalization_lapsed_tr_t50, market_naturalization__lapsed_alternative_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(market_naturalization_lapsed_tr_t75, market_naturalization__lapsed_alternative_reading, theater_ratio, 75, 0.08).
narrative_ontology:measurement(market_naturalization_lapsed_tr_t100, market_naturalization__lapsed_alternative_reading, theater_ratio, 100, 0.08).

% Extraction over time
narrative_ontology:measurement(market_naturalization_lapsed_be_t0, market_naturalization__lapsed_alternative_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(market_naturalization_lapsed_be_t25, market_naturalization__lapsed_alternative_reading, base_extractiveness, 25, 0.11).
narrative_ontology:measurement(market_naturalization_lapsed_be_t50, market_naturalization__lapsed_alternative_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(market_naturalization_lapsed_be_t75, market_naturalization__lapsed_alternative_reading, base_extractiveness, 75, 0.12).
narrative_ontology:measurement(market_naturalization_lapsed_be_t100, market_naturalization__lapsed_alternative_reading, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(market_naturalization_lapsed_su_t0, market_naturalization__lapsed_alternative_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(market_naturalization_lapsed_su_t25, market_naturalization__lapsed_alternative_reading, suppression_requirement, 25, 0.16).
narrative_ontology:measurement(market_naturalization_lapsed_su_t50, market_naturalization__lapsed_alternative_reading, suppression_requirement, 50, 0.17).
narrative_ontology:measurement(market_naturalization_lapsed_su_t75, market_naturalization__lapsed_alternative_reading, suppression_requirement, 75, 0.18).
narrative_ontology:measurement(market_naturalization_lapsed_su_t100, market_naturalization__lapsed_alternative_reading, suppression_requirement, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__lapsed_alternative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__lapsed_alternative_reading, 0.1).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__lapsed_alternative_reading, market_naturalization__hybrid_reading).

% DUAL FORMULATION NOTE:
% market_naturalization kernel decomposes into three constraint stories: lapsed_alternative_reading (this file, Mountain-claimed, ε=0.12), beneficiary_maintained_reading (Snare-claimed, ε≈0.65), hybrid_reading (Tangled_Rope-claimed, ε≈0.35). The upstream claim (lapsed alternative) is often cited as evidence for the downstream claims (active maintenance, hybrid). This story's low ε and zero beneficiaries are the baseline from which the other readings deviate by declaring active structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
