% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_reachability_boundary__contraction_reading
 *   human_readable: Nuclear-Constrained Total War Impossibility Boundary (Contraction Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the contraction reading of the
 *   total_war_reachability_boundary kernel: the claim that nuclear weapons
 *   permanently removed winnable total war from the feasible set, producing a
 *   mountain-like structural boundary. No actor benefits from extraction
 *   because the boundary is not enforced or administered; breach imposes
 *   universal species-level costs. This reading is contested by the dropping
 *   reading (total war remains reachable but deterred, a rope) and the
 *   contingent reachability reading (current contraction is a reversible
 *   piton). These are not the same constraint viewed from different angles;
 *   they assign different Îµ values and structural types to the same
 *   historical proposition.
 *
 * KEY AGENTS:
 *   - nuclear_armed_states: Structural subjects â bound by the logic of assured destruction, neither extracting nor being extracted from by the boundary
 *   - human_species: Universal risk-bearer â carries the extinction downside if the boundary is breached
 *   - strategic_studies_community: Analytical observer â produces and disputes competing readings of the boundary's nature
 *   - non_nuclear_weapon_states: Excluded from the deterrence logic but subject to systemic fallout and secondary effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__contraction_reading, 0.01).
domain_priors:suppression_score(total_war_reachability_boundary__contraction_reading, 0.01).
domain_priors:theater_ratio(total_war_reachability_boundary__contraction_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, extractiveness, 0.01).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, accessibility_collapse, 0.94).
narrative_ontology:constraint_metric(total_war_reachability_boundary__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__contraction_reading, mountain).
narrative_ontology:human_readable(total_war_reachability_boundary__contraction_reading, "Nuclear-Constrained Total War Impossibility Boundary (Contraction Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__contraction_reading, "international_relations/strategic_studies").

domain_priors:emerges_naturally(total_war_reachability_boundary__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__contraction_reading, 'aaae4bfe-3f25-4427-9786-3538ab2d3b19').
narrative_ontology:cs_kernel_codification('aaae4bfe-3f25-4427-9786-3538ab2d3b19', distributed).
narrative_ontology:cs_authority_grounding('aaae4bfe-3f25-4427-9786-3538ab2d3b19', distributed).
narrative_ontology:cs_reading_relation('aaae4bfe-3f25-4427-9786-3538ab2d3b19', total_war_reachability_boundary__dropping_reading, forecloses).
narrative_ontology:cs_reading_relation('aaae4bfe-3f25-4427-9786-3538ab2d3b19', total_war_reachability_boundary__contingent_reachability_reading, forecloses).
narrative_ontology:cs_axiom('aaae4bfe-3f25-4427-9786-3538ab2d3b19', foundational, mutual_assured_destruction_excludes_total_war).
narrative_ontology:cs_axiom_status(mutual_assured_destruction_excludes_total_war, holdable).
narrative_ontology:cs_axiom_grounding('aaae4bfe-3f25-4427-9786-3538ab2d3b19', mutual_assured_destruction_excludes_total_war, empirically_contingent).
narrative_ontology:cs_axiom('aaae4bfe-3f25-4427-9786-3538ab2d3b19', foundational, strategic_impossibility_independent_of_will).
narrative_ontology:cs_axiom_status(strategic_impossibility_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('aaae4bfe-3f25-4427-9786-3538ab2d3b19', strategic_impossibility_independent_of_will, empirically_contingent).
narrative_ontology:cs_reference_frame('aaae4bfe-3f25-4427-9786-3538ab2d3b19', post_nuclear_revolution_boundary).
narrative_ontology:cs_drift_state('aaae4bfe-3f25-4427-9786-3538ab2d3b19', contemporary_tech_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aaae4bfe-3f25-4427-9786-3538ab2d3b19', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None â the constraint does not coordinate agents through an arrangement. It operates as a structural boundary that removes winnable total war from the feasible set by rendering it self-annihilating for all parties.
% TRANSFER_FUNCTION: No transfer â the boundary extracts no resources and moves no value between agents. Any breach imposes a symmetric, universal extinction cost rather than a directional extraction.
% ABSENT_VOICES: Military strategists who treat total war as a recoverable strategic option, states pursuing decisive strategic advantage through disarming counterforce, and abolitionist movements that reject the nuclear equilibrium entirely are structurally backgrounded by the prevailing analytical frame.
% DISAPPEARANCE_RATIONALE: If the boundary vanished and total war became strategically feasible again, the entire architecture of extended deterrence, alliance guarantees, and great-power crisis management would collapse. States would reorganize around preemption, arms racing, and alliance abandonment.
% FOUNDING_PROBLEM: The problem of catastrophic interstate total war among industrialized great powers, escalated to potentially species-ending scale by nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the World Wars corroborate the severity of the founding problem. The record of no nuclear-armed total war since 1945 corroborates the boundary's effect. However, the causal attribution to structural impossibility rather than deterrence, normative restraint, or chance is contested within the strategic studies community itself. Peace researchers and non-nuclear states dispute the 'dead' status, citing ongoing nuclear risk.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_reachability_boundary__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__contraction_reading, 0.01, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is authored at 0.01 because a genuine structural impossibility extracts nothing; there is no rent, no tribute, and no coordination surplus captured by any seat. Suppression is 0.01 because the boundary requires no active enforcement â it is sustained by the physical and strategic logic of assured destruction, not by coercion. Theater ratio is 0.01 because the constraint is not performative; strategic debates about its nature are analytical, not theatrical maintenance. Accessibility collapse is 0.94 because once the logic of second-strike survivability is understood, alternatives to total-war impossibility collapse (the only alternative is extinction). Resistance is 0.02 because no agent can resist the strategic physics of mutual annihilation.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer seat (strategic theorists) and the state seats should compute similarly as mountains because the boundary applies universally. However, non-nuclear states may compute a slightly different type if they experience the boundary as an imposed exclusion from security decision-making rather than a symmetric structural feature.
 *
 * DIRECTIONALITY LOGIC:
 *   There are no beneficiaries or victims in the extractive sense. All agents sit at approximately symmetric directionality (d â 0.5) because the boundary imposes no directional transfer. Nuclear-armed states are subjects of the boundary, not its targets. The human species carries the universal extinction externality, but this is not extraction â it is a symmetric cost of breach.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents misreading MAD as a rope that requires active maintenance or as a snare that extracts from someone. The contraction reading identifies a genuine structural boundary that needs no enforcement, no beneficiary, and no theatrical upkeep. If the discipline of strategic studies debates its nature endlessly, that debate is external to the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Is the total war reachability boundary a permanent structural feature (mountain), a stable coordination equilibrium (rope), or a temporarily atrophied capability (piton)?',
    'Technological shocks that undermine assured destruction (e.g., effective missile defense, disarming cyber strikes, revolutionary delivery systems) would falsify the mountain reading and support contingent reachability; sustained nuclear peace maintained by mutual adjustment without structural impossibility would support the rope reading.',
    'Determines whether the constraint is modeled as zero-extraction natural law or as an actively maintained arrangement with potential for distributional consequences and reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Which reading of the total war reachability kernel is structurally true').

omega_variable(
    mad_robustness_under_tech_change,
    'Does emerging technology (hypersonic delivery, missile defense, AI-enabled command-and-control disruption) threaten the empirical premise that assured destruction is unavoidable?',
    'Empirical assessment of second-strike survivability under technological stress; wargame and simulation data; analysis of command-and-control vulnerability.',
    'If MAD is empirically undermined, the mountain reading collapses toward the contingent reachability reading (piton) or the dropping reading (rope), raising extractiveness as states compete to restore strategic advantage and the boundary becomes actively contested rather than structurally given.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mad_robustness_under_tech_change, empirical, 'Whether the foundational empirical premise of the contraction reading remains valid').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__contraction_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrbc_tr_t0, total_war_reachability_boundary__contraction_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(twrbc_tr_t39, total_war_reachability_boundary__contraction_reading, theater_ratio, 39, 0.01).
narrative_ontology:measurement(twrbc_tr_t78, total_war_reachability_boundary__contraction_reading, theater_ratio, 78, 0.01).

% Extraction over time
narrative_ontology:measurement(twrbc_be_t0, total_war_reachability_boundary__contraction_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(twrbc_be_t39, total_war_reachability_boundary__contraction_reading, base_extractiveness, 39, 0.01).
narrative_ontology:measurement(twrbc_be_t78, total_war_reachability_boundary__contraction_reading, base_extractiveness, 78, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_reachability_boundary__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__dropping_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__contraction_reading, total_war_reachability_boundary__contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_reachability_boundary kernel. The kernel decomposes into three structurally distinct constraints: the contraction reading (mountain), the dropping reading (rope), and the contingent reachability reading (piton). Each reading assigns a different Îµ and type to the same natural-language concept; they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
