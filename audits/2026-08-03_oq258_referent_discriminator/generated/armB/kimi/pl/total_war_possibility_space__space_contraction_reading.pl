% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Possibility Space — Space Contraction Reading
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story instantiates the space_contraction_reading of the
 *   total_war_possibility_space kernel. The reading asserts that nuclear
 *   weapons did not merely raise the cost or violate the taboo of total war,
 *   but removed it from the strategically thinkable altogether, producing
 *   categorical impossibility. The constraint is treated as a structural
 *   feature of the nuclear-strategic environment: once mutual annihilation
 *   capability exists, total war ceases to be a coherent strategic option and
 *   becomes, in Thomas Schelling's terms, outside the bargaining space. The
 *   predicted structural delta includes the institutional atrophy of general
 *   staffs, the disappearance of mobilization doctrine, and the migration of
 *   strategic studies to sub-nuclear and limited-war domains. Because this
 *   reading treats the constraint as a material-strategic boundary rather
 *   than a normative or equilibrium arrangement, it is classified as a
 *   mountain with negligible extraction, suppression, and resistance. The
 *   claim/metric independence principle is observed: the metrics are authored
 *   descriptively low to reflect the reading's own assertion of categorical
 *   unthinkability, not tuned to force a mountain output.
 *
 * KEY AGENTS:
 *   - conventional_general_staffs: Institutional target — institutionally atrophied by the removal of their core planning function; no extraction, but functional extinction.
 *   - nuclear_strategists: Analytical observer — the community that asserts and maintains the space-contraction reading; benefits discursively but does not extract via the constraint itself.
 *   - great_power_polities: Diffuse incidental beneficiary — total war no longer threatens annihilation, though no party collects from the constraint's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.04).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.08).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Possibility Space — Space Contraction Reading").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, '050f4d92-ee1e-484c-a8e7-4a1420df3119').
narrative_ontology:cs_kernel_codification('050f4d92-ee1e-484c-a8e7-4a1420df3119', distributed).
narrative_ontology:cs_authority_grounding('050f4d92-ee1e-484c-a8e7-4a1420df3119', expertise).
narrative_ontology:cs_interpretation_layer_present('050f4d92-ee1e-484c-a8e7-4a1420df3119').
narrative_ontology:cs_reading_relation('050f4d92-ee1e-484c-a8e7-4a1420df3119', total_war_possibility_space__deterrence_equilibrium_reading, forecloses).
narrative_ontology:cs_reading_relation('050f4d92-ee1e-484c-a8e7-4a1420df3119', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('050f4d92-ee1e-484c-a8e7-4a1420df3119', foundational, total_war_categorically_impossible).
narrative_ontology:cs_axiom_status(total_war_categorically_impossible, holdable).
narrative_ontology:cs_axiom_grounding('050f4d92-ee1e-484c-a8e7-4a1420df3119', total_war_categorically_impossible, empirically_contingent).
narrative_ontology:cs_axiom('050f4d92-ee1e-484c-a8e7-4a1420df3119', foundational, material_capability_determines_strategic_space).
narrative_ontology:cs_axiom_status(material_capability_determines_strategic_space, holdable).
narrative_ontology:cs_axiom_grounding('050f4d92-ee1e-484c-a8e7-4a1420df3119', material_capability_determines_strategic_space, empirically_contingent).
narrative_ontology:cs_reference_frame('050f4d92-ee1e-484c-a8e7-4a1420df3119', nuclear_total_war_unthinkability).
narrative_ontology:cs_drift_state('050f4d92-ee1e-484c-a8e7-4a1420df3119', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('050f4d92-ee1e-484c-a8e7-4a1420df3119', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

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
% COORDINATION_FUNCTION: None — the constraint operates as a structural boundary condition rather than an arrangement between agents. It removes a strategic option rather than solving a collective action problem.
% TRANSFER_FUNCTION: None — the constraint does not move resources, status, or risk between agents. It eliminates a class of strategic behavior from the possibility space.
% ABSENT_VOICES: Continental-war general staffs and mass-mobilization planners, now institutionally atrophied or reassigned to sub-nuclear and stabilization roles; they would contest the claim that total war is categorically impossible, but their institutional weight and access to strategic discourse have dissolved.
% DISAPPEARANCE_RATIONALE: If total war returned to the strategically thinkable, general staffs would rebuild mobilization apparatus, alliance structures would shift from extended deterrence to war-fighting coalitions, defense procurement would reorient toward mass conventional forces, and strategic studies would recentre great-power total war.
% FOUNDING_PROBLEM: How industrial great powers could pursue national survival without catastrophic self-destruction under conditions of absolute weaponry.
% FOUNDING_PROBLEM_CORROBORATION: Institutional historians and military sociologists outside the nuclear-strategy community attest to the disappearance of mass-mobilization doctrine and general staff war-gaming for existential conflict; the strategic studies community corroborates the shift but is professionally invested in the post-total-war analytical paradigm.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.04, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.04) because the constraint does not transfer resources; it removes a strategic possibility. Suppression is low (0.08) because no coercion is required to maintain unthinkability — the constraint operates as a structural boundary. Theater ratio is minimal (0.05) because performative maintenance is unnecessary; the reality is self-enforcing. Accessibility collapse is very high (0.92) because once nuclear deterrence is understood, total war as a strategic option collapses entirely. Resistance is negligible (0.04) because no agent can strategically resist the logic of mutual annihilation. The metrics are flat across the interval because the constraint, as a mountain, is stable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear strategists and post-Cold War planners, the constraint is an objective strategic ceiling. From the perspective of displaced conventional general staffs, the same phenomenon reads as institutional extinction and the loss of a professional worldview. The engine computes per-seat directionality from structural position: the general staff seat, if modeled, would sit at high directionality as a target of institutional obsolescence, but because the constraint does not extract (epsilon approximately 0), effective extraction chi remains negligible regardless of seat. This is the hallmark of a genuine mountain: structural position does not translate into extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiary derives rent from this constraint's operation. Nuclear powers benefit from the stability but do not collect from it. Conventional military institutions bear the cost of functional displacement but are not farmed by an extracting agent. Directionality is therefore diffuse and low-magnitude across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The space_contraction reading avoids mandatrophy mislabeling by distinguishing the constraint from deterrence equilibrium. A deterrence equilibrium is a rope or tangled rope (actively maintained by mutual arrangement), whereas this reading asserts that total war has become genuinely unthinkable — the coordination story is not cover for extraction, because there is no coordination story. The constraint persists without enforcement, agenda-setters, or beneficiaries. If institutional atrophy were mistaken for the constraint itself, one might classify it as a piton; here the atrophy is a downstream effect of the mountain, not the constraint's primary identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_limit_vs_constructed_doctrine,
    'Is the unthinkability of total war a material-strategic mountain arising from nuclear physics, or a constructed doctrine within the strategic studies field that benefits from treating professional consensus as natural law?',
    'Comparative analysis of strategic planning documents across nuclear and non-nuclear great powers; if non-nuclear powers also abandon total-war planning, the constraint is constructed doctrine rather than material limit.',
    'If constructed, the constraint''s classification shifts from mountain to snare or tangled rope (institutional extraction via false naturalization); if material, mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_limit_vs_constructed_doctrine, conceptual, 'Ambiguity between material strategic limit and constructed disciplinary doctrine').

omega_variable(
    institutional_atrophy_as_cause_or_effect,
    'Does the atrophy of total-war planning institutions prove that total war has become strategically impossible, or does the institutional atrophy itself produce the appearance of impossibility by erasing the capacity to imagine and execute total war?',
    'Historical counterfactual analysis: can institutional capacity for total war be rebuilt under changed technological or political conditions? If rebuildable, the constraint is reversible and not a mountain.',
    'If atrophy causes unthinkability, the constraint is a piton (degraded institution) or scaffold (transitional atrophy) rather than a mountain; if atrophy follows genuine impossibility, mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_as_cause_or_effect, empirical, 'Direction of causality between institutional atrophy and strategic unthinkability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twp_spaccon_tr_t0, total_war_possibility_space__space_contraction_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(twp_spaccon_tr_t16, total_war_possibility_space__space_contraction_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(twp_spaccon_tr_t32, total_war_possibility_space__space_contraction_reading, theater_ratio, 32, 0.05).
narrative_ontology:measurement(twp_spaccon_tr_t48, total_war_possibility_space__space_contraction_reading, theater_ratio, 48, 0.05).
narrative_ontology:measurement(twp_spaccon_tr_t64, total_war_possibility_space__space_contraction_reading, theater_ratio, 64, 0.05).
narrative_ontology:measurement(twp_spaccon_tr_t80, total_war_possibility_space__space_contraction_reading, theater_ratio, 80, 0.05).

% Extraction over time
narrative_ontology:measurement(twp_spaccon_be_t0, total_war_possibility_space__space_contraction_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(twp_spaccon_be_t16, total_war_possibility_space__space_contraction_reading, base_extractiveness, 16, 0.04).
narrative_ontology:measurement(twp_spaccon_be_t32, total_war_possibility_space__space_contraction_reading, base_extractiveness, 32, 0.04).
narrative_ontology:measurement(twp_spaccon_be_t48, total_war_possibility_space__space_contraction_reading, base_extractiveness, 48, 0.04).
narrative_ontology:measurement(twp_spaccon_be_t64, total_war_possibility_space__space_contraction_reading, base_extractiveness, 64, 0.04).
narrative_ontology:measurement(twp_spaccon_be_t80, total_war_possibility_space__space_contraction_reading, base_extractiveness, 80, 0.04).

% Suppression requirement over time
narrative_ontology:measurement(twp_spaccon_su_t0, total_war_possibility_space__space_contraction_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(twp_spaccon_su_t16, total_war_possibility_space__space_contraction_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement(twp_spaccon_su_t32, total_war_possibility_space__space_contraction_reading, suppression_requirement, 32, 0.08).
narrative_ontology:measurement(twp_spaccon_su_t48, total_war_possibility_space__space_contraction_reading, suppression_requirement, 48, 0.08).
narrative_ontology:measurement(twp_spaccon_su_t64, total_war_possibility_space__space_contraction_reading, suppression_requirement, 64, 0.08).
narrative_ontology:measurement(twp_spaccon_su_t80, total_war_possibility_space__space_contraction_reading, suppression_requirement, 80, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the total_war_possibility_space kernel; sibling readings include deterrence_equilibrium_reading and nuclear_taboo_reading. The epsilon-invariance principle requires separate stories because each reading posits a different structural mechanism (material impossibility, deterred equilibrium, normative taboo) with different epsilon values and different stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
