% ============================================================================
% CONSTRAINT STORY: nuclear_impossibility_kernel__structural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nuclear_impossibility_kernel__structural_contraction_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nuclear_impossibility_kernel__structural_contraction_reading
 *   human_readable: Nuclear Impossibility: Structural Contraction of War
 *   domain: strategic_studies/international_relations
 *
 * SUMMARY:
 *   This constraint describes the 'structural contraction' reading of the
 *   nuclear impossibility kernel: the physical reality of mutual assured
 *   destruction (MAD) means that no rational path to victory exists in a
 *   large-scale nuclear conflict. This is treated as an irreducible
 *   physical/logical limit, a Mountain, that fundamentally alters the
 *   strategic landscape. While the impossibility itself does not 'operate' in
 *   an extractive sense, its existence creates a deterrence regime from which
 *   nuclear states benefit by avoiding conventional great-power war,
 *   triggering False Summit Mountain (FSM) analysis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, 0.05).
domain_priors:theater_ratio(nuclear_impossibility_kernel__structural_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nuclear_impossibility_kernel__structural_contraction_reading, mountain).
narrative_ontology:human_readable(nuclear_impossibility_kernel__structural_contraction_reading, "Nuclear Impossibility: Structural Contraction of War").
narrative_ontology:topic_domain(nuclear_impossibility_kernel__structural_contraction_reading, "strategic_studies/international_relations").

domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nuclear_impossibility_kernel__structural_contraction_reading, 'c4dd0207-4e4e-4828-8069-d215812fc82a').
narrative_ontology:cs_kernel_codification('c4dd0207-4e4e-4828-8069-d215812fc82a', implicit).
narrative_ontology:cs_authority_grounding('c4dd0207-4e4e-4828-8069-d215812fc82a', self_enforcing).
narrative_ontology:cs_reading_relation('c4dd0207-4e4e-4828-8069-d215812fc82a', nuclear_impossibility_kernel__rational_dropout_reading, forecloses).
narrative_ontology:cs_reading_relation('c4dd0207-4e4e-4828-8069-d215812fc82a', nuclear_impossibility_kernel__credibility_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('c4dd0207-4e4e-4828-8069-d215812fc82a', foundational, mutual_annihilation_guaranteed).
narrative_ontology:cs_axiom_status(mutual_annihilation_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('c4dd0207-4e4e-4828-8069-d215812fc82a', mutual_annihilation_guaranteed, empirically_contingent).
narrative_ontology:cs_axiom('c4dd0207-4e4e-4828-8069-d215812fc82a', foundational, victory_is_impossible).
narrative_ontology:cs_axiom_status(victory_is_impossible, holdable).
narrative_ontology:cs_axiom_grounding('c4dd0207-4e4e-4828-8069-d215812fc82a', victory_is_impossible, deontological).
narrative_ontology:cs_reference_frame('c4dd0207-4e4e-4828-8069-d215812fc82a', post_nuclear_era_strategic_reality).
narrative_ontology:cs_drift_state('c4dd0207-4e4e-4828-8069-d215812fc82a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c4dd0207-4e4e-4828-8069-d215812fc82a', '').
narrative_ontology:cs_kernel_id(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states possess nuclear weapons and are therefore subject to the impossibility of rational victory in large-scale conflict. They benefit from the resulting strategic stability and deterrence of conventional great-power war, but bear the cost of maintaining arsenals and the existential risk.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_states, beneficiary,
    institutional, civilizational, trapped, global).

% These states do not possess nuclear weapons but are profoundly affected by the global nuclear reality. They are constrained by the actions of nuclear states and bear the risk of nuclear conflict without the direct deterrence benefits of possession. Their strategic options are shaped by this impossibility.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, non_nuclear_states, payer,
    organized, generational, constrained, global).

% These actors study and interpret the implications of nuclear weapons for international relations and strategic theory. They analyze the structural constraints and paradoxes created by nuclear weapons, seeking to understand and model their effects on state behavior.
narrative_ontology:constraint_stakeholder(nuclear_impossibility_kernel__structural_contraction_reading, strategic_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None directly; the constraint is a structural limit on strategic action. The deterrence regime that arises from this impossibility, however, coordinates state behavior by making large-scale war irrational.
% TRANSFER_FUNCTION: None directly; it prevents the transfer of 'victory' in large-scale conflict from one state to another, effectively removing it from the strategic calculus.
% ABSENT_VOICES: None in a literal sense; the physical reality of mutual annihilation is universally acknowledged by rational actors. However, historical military doctrines that assumed conventional victory in great-power conflict are now structurally absent from rational strategic planning.
% DISAPPEARANCE_RATIONALE: If the physical impossibility of rational victory in nuclear war vanished overnight (e.g., through a perfect defense or disarmament), the strategic calculus for large-scale conflict would fundamentally change. 'Victory' would become a conceivable outcome again, destabilizing the current deterrence regime and potentially leading to a return of great-power conventional warfare.
% FOUNDING_PROBLEM: The existential threat of large-scale conventional warfare between great powers, which historically led to devastating conflicts and global instability.
% FOUNDING_PROBLEM_CORROBORATION: Strategic analysts, historians, and policymakers across nuclear and non-nuclear states corroborate that the threat of large-scale conventional war remains, and nuclear deterrence, rooted in this impossibility, has fundamentally altered its dynamics by making such wars irrational.
narrative_ontology:disappearance_verdict(nuclear_impossibility_kernel__structural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(nuclear_impossibility_kernel__structural_contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nuclear_impossibility_kernel__structural_contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(nuclear_impossibility_kernel__structural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nuclear_impossibility_kernel__structural_contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(nuclear_impossibility_kernel__structural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(nuclear_impossibility_kernel__structural_contraction_reading),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(nuclear_impossibility_kernel__structural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(nuclear_impossibility_kernel__structural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Mountain because it represents an unchangeable physical and logical limit imposed by nuclear weapons. Extractiveness, suppression, and theater ratio are all very low, reflecting that it is a fundamental reality rather than an actively enforced or performative construct. Accessibility collapse is high (0.95) because the alternative of 'winning a nuclear war' is structurally foreclosed. Resistance is low (0.05) as attempts to circumvent this fundamental impossibility are futile. The metrics are stable over time because the underlying physical reality has not changed since 1945.
 *
 * PERSPECTIVAL GAP:
 *   For this reading, there is no significant perspectival gap among rational actors regarding the core impossibility. While different actors may interpret its implications or the resulting deterrence strategies differently, the fundamental structural limit it imposes is widely acknowledged. The FSM trigger arises from the benefit of avoiding large-scale conventional war, which is a consequence of living under this Mountain, rather than an extraction from its 'operation'.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear states are identified as beneficiaries because the impossibility of victory in nuclear war creates a powerful deterrent against large-scale conventional conflict, which benefits them by preserving their security and preventing devastating wars. Non-nuclear states are identified as payers because they bear the risks and consequences of this nuclear reality without the direct deterrence benefits of possession. No party is a 'victim' in the sense of being actively extracted from by the impossibility itself, but rather constrained by its fundamental nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_natural_vs_constructed_impossibility,
    'Is the ''impossibility of victory'' a pure physical and logical limit, or is its framing as such also a constructed constraint that benefits nuclear states by stabilizing deterrence and preventing challenges to their strategic position?',
    'Analysis of historical and contemporary strategic discourse: if the ''impossibility'' is selectively emphasized or de-emphasized to serve political ends, it suggests a constructed element. Examination of ''limited nuclear war'' doctrines: if these are seriously pursued, it challenges the absolute nature of the impossibility.',
    'If a significant constructed element is found, the constraint would lean more towards a Tangled Rope or Snare, where the ''natural'' framing serves an extractive function for nuclear states, rather than a pure Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_natural_vs_constructed_impossibility, conceptual, 'Ambiguity between a pure physical limit and a strategically constructed framing of impossibility.').

omega_variable(
    impossibility_vs_cost_prohibition,
    'Does nuclear war truly present a ''physical impossibility'' of victory, or does it merely make the costs of victory so ''prohibitive'' that it becomes irrational, as argued by the ''rational_dropout_reading''?',
    'Detailed modeling of post-exchange scenarios and recovery capabilities: if any scenario allows for a meaningful ''victory'' state, it supports the ''rational_dropout'' view. Conversely, if all paths lead to societal collapse, it supports ''structural_contraction''.',
    'If the ''rational_dropout_reading'' is validated, this constraint''s classification would shift from Mountain to a more constructed type (e.g., Rope or Tangled Rope), as the constraint would then be based on a cost-benefit calculation rather than an absolute physical limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(impossibility_vs_cost_prohibition, empirical, 'Distinction between absolute impossibility and prohibitive cost as the core constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nuclear_impossibility_kernel__structural_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nucl_tr_t1945, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(nucl_tr_t1960, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1960, 0.0).
narrative_ontology:measurement(nucl_tr_t1980, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(nucl_tr_t2000, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(nucl_tr_t2024, nuclear_impossibility_kernel__structural_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(nucl_be_t1945, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(nucl_be_t1960, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(nucl_be_t1980, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 1980, 0.05).
narrative_ontology:measurement(nucl_be_t2000, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(nucl_be_t2024, nuclear_impossibility_kernel__structural_contraction_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(nucl_su_t1945, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1945, 0.05).
narrative_ontology:measurement(nucl_su_t1960, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1960, 0.05).
narrative_ontology:measurement(nucl_su_t1980, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 1980, 0.05).
narrative_ontology:measurement(nucl_su_t2000, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(nucl_su_t2024, nuclear_impossibility_kernel__structural_contraction_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nuclear_impossibility_kernel__structural_contraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__rational_dropout_reading).
narrative_ontology:affects_constraint(nuclear_impossibility_kernel__structural_contraction_reading, nuclear_impossibility_kernel__credibility_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'nuclear_impossibility_kernel', focusing on the structural contraction of war. It is linked to sibling readings that emphasize different aspects of the nuclear reality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
