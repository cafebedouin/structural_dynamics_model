% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__contraction_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Total War Reachability Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint describes the 'contraction reading' of the total war
 *   reachability boundary, asserting that nuclear weapons have fundamentally
 *   and permanently removed winnable total war from the feasible set of
 *   strategic options. It is a Mountain because its persistence is
 *   independent of human will or enforcement; it is a physical/logical limit
 *   imposed by the destructive power of nuclear arsenals. No actor benefits
 *   from its operation, as even the 'victor' in a total nuclear exchange
 *   would face catastrophic consequences. The victim set is universal,
 *   encompassing all of humanity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.0).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.95).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Total War Reachability Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, '3a063f2c-aeee-420b-8da2-4cc4b73ab09c').
narrative_ontology:cs_kernel_codification('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', implicit).
narrative_ontology:cs_authority_grounding('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', self_enforcing).
narrative_ontology:cs_reading_relation('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', foundational, mutual_assured_destruction_is_absolute).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', mutual_assured_destruction_is_absolute, empirically_contingent).
narrative_ontology:cs_axiom('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', foundational, winnable_total_war_is_a_logical_contradiction).
narrative_ontology:cs_axiom_status(winnable_total_war_is_a_logical_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', winnable_total_war_is_a_logical_contradiction, deontological).
narrative_ontology:cs_reference_frame('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', pre_nuclear_strategic_calculus).
narrative_ontology:cs_drift_state('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a063f2c-aeee-420b-8da2-4cc4b73ab09c', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, human_species).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, nuclear_powers).
narrative_ontology:constraint_victim(total_war_reachability_boundary__contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the means to initiate total war but are also the primary agents whose actions are constrained by its unreachability. They bear the cost of maintaining deterrence and the existential risk of its failure, without any prospect of 'winning' a total war.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, nuclear_powers, payer,
    institutional, generational, trapped, global).

% Are subject to the existential risk of total war, even if they cannot initiate it. Their strategic options are constrained by the nuclear overhang, forcing reliance on alliances or non-proliferation efforts.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, non_nuclear_states, payer,
    moderate, generational, constrained, global).

% The ultimate victim of total war, facing extinction or civilizational collapse. This entity bears the universal cost of the constraint without any agency to alter it.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, human_species, payer,
    powerless, civilizational, trapped, universal).

% Analyze the implications of nuclear weapons for international relations, developing concepts like Mutually Assured Destruction (MAD) and the 'long peace.' They observe the constraint's operation and its effects on state behavior.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__contraction_reading, strategic_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint implicitly coordinates nuclear powers to avoid total war by making it unwinnable, thereby preventing species-level catastrophe. It establishes a shared understanding of an uncrossable boundary.
% TRANSFER_FUNCTION: Transfers the concept of 'winnable total war' from the realm of strategic possibility to the realm of physical impossibility, effectively transferring existential risk from a contingent outcome to a fixed, albeit terrifying, boundary condition.
% ABSENT_VOICES: Past military strategists who conceived of total war as a viable, if costly, option would object to its categorical unreachability. Future generations, if total war were to occur, would be absent entirely.
% DISAPPEARANCE_RATIONALE: If the unreachability of total war vanished (e.g., through a technological breakthrough rendering nuclear weapons obsolete or ineffective), the entire global strategic landscape would fundamentally rearrange. States would re-evaluate military spending, alliances, and conflict resolution, potentially returning to a pre-nuclear strategic calculus.
% FOUNDING_PROBLEM: The problem of preventing global catastrophe once humanity developed weapons capable of species-level destruction.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, corroborated by ongoing nuclear proliferation concerns, the continuous maintenance of nuclear arsenals, and the persistent academic and policy discourse around deterrence theory. International bodies and non-proliferation treaties also attest to the ongoing nature of this problem.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_reachability_boundary__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.0 because no party collects rents from the unreachability of total war; rather, all parties are constrained by it. Suppression is 0.95 because the physical reality of nuclear destruction effectively suppresses any rational attempt to initiate total war. Accessibility collapse is 0.98, reflecting the near-total closure of the 'winnable total war' option. Resistance is 0.05, as there is no meaningful resistance to this physical reality, only attempts to manage its implications (e.g., arms control). Theater ratio is 0.0, as there is no performative aspect to this fundamental physical limit.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of all actors, the constraint is a Mountain. There is no perspectival gap regarding its fundamental nature, only regarding the implications for policy and strategy. The 'contraction reading' asserts this as an objective, shared reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear powers and non-nuclear states are both 'payers' in the sense that they bear the existential risk and the constraints on their strategic choices. The human species is the ultimate 'payer' as the universal victim of any failure of this boundary. There are no beneficiaries, as no actor can 'win' a total nuclear war. The constraint subsidizes no one; it imposes a universal cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_reversibility,
    'Is the unreachability of total war a permanent physical boundary, or could future technological developments (e.g., effective missile defense, new weapon types) render it reachable again?',
    'Emergence of new military technologies that fundamentally alter the dynamics of nuclear deterrence, or a shift in the scientific consensus on the feasibility of such technologies.',
    'If reversible, the constraint would reclassify from Mountain to a Piton (atrophied capability) or Rope (contingent coordination), as its persistence would depend on technological stasis or active maintenance, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reversibility, empirical, 'Whether the physical boundary is truly immutable or technologically contingent.').

omega_variable(
    rationality_assumption,
    'Does the ''unreachability'' of total war depend on the assumption of rational actors, and if so, how would a breakdown of rationality affect the constraint?',
    'Empirical observation of state actors consistently making irrational decisions that risk total war, or a shift in the conceptual understanding of rationality in strategic studies.',
    'If the constraint''s force relies on rationality, and rationality breaks down, the ''unreachability'' might become a ''dropping_reading'' (lower probability but still reachable) or even a Snare (if irrational actors impose costs on others by risking total war).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rationality_assumption, conceptual, 'The role of actor rationality in maintaining the unreachability boundary.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''contraction_reading'' asserts total war is physically impossible, how do other readings (dropping, contingent) maintain their coherence?',
    'Analysis of the underlying axioms and reference frames of the sibling readings to identify the precise point of conceptual divergence.',
    'If the ''contraction_reading'' is correct, the other readings are either based on flawed premises or describe different, less fundamental constraints. This omega documents the conceptual contest itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'The conceptual basis for divergence among readings of the total war reachability kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement(tota_tr_t1989, total_war_reachability_boundary__contraction_reading, theater_ratio, 1989, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_reachability_boundary__contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement(tota_be_t1989, total_war_reachability_boundary__contraction_reading, base_extractiveness, 1989, 0.0).
narrative_ontology:measurement(tota_be_t2024, total_war_reachability_boundary__contraction_reading, base_extractiveness, 2024, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1962, 0.9).
narrative_ontology:measurement(tota_su_t1989, total_war_reachability_boundary__contraction_reading, suppression_requirement, 1989, 0.95).
narrative_ontology:measurement(tota_su_t2024, total_war_reachability_boundary__contraction_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, nuclear_non_proliferation_treaty).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, arms_control_regimes).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_reachability_boundary' kernel. The 'contraction_reading' asserts total war is physically impossible, influencing the 'dropping_reading' (lower probability) and 'contingent_reachability_reading' (technology-dependent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
