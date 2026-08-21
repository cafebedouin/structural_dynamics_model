% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.9).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '072514a4-1a5f-4f8c-b216-76760e4b6ebd').
narrative_ontology:cs_kernel_codification('072514a4-1a5f-4f8c-b216-76760e4b6ebd', fixed_text).
narrative_ontology:cs_authority_grounding('072514a4-1a5f-4f8c-b216-76760e4b6ebd', extraction).
narrative_ontology:cs_interpretation_layer_present('072514a4-1a5f-4f8c-b216-76760e4b6ebd').
narrative_ontology:cs_reading_relation('072514a4-1a5f-4f8c-b216-76760e4b6ebd', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('072514a4-1a5f-4f8c-b216-76760e4b6ebd', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('072514a4-1a5f-4f8c-b216-76760e4b6ebd', foundational, majoritarian_rule_is_democratic_norm).
narrative_ontology:cs_axiom_status(majoritarian_rule_is_democratic_norm, holdable).
narrative_ontology:cs_axiom_grounding('072514a4-1a5f-4f8c-b216-76760e4b6ebd', majoritarian_rule_is_democratic_norm, deontological).
narrative_ontology:cs_axiom('072514a4-1a5f-4f8c-b216-76760e4b6ebd', foundational, entrenched_privilege_is_unjust).
narrative_ontology:cs_axiom_status(entrenched_privilege_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('072514a4-1a5f-4f8c-b216-76760e4b6ebd', entrenched_privilege_is_unjust, deontological).
narrative_ontology:cs_reference_frame('072514a4-1a5f-4f8c-b216-76760e4b6ebd', constitutional_stability_as_blocking_tool).
narrative_ontology:cs_drift_state('072514a4-1a5f-4f8c-b216-76760e4b6ebd', contemporary_political_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('072514a4-1a5f-4f8c-b216-76760e4b6ebd', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, disenfranchised_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the political, economic, or social groups whose historical advantages are codified in the status quo. They actively use the supermajority threshold to block any reforms that would challenge their position, effectively converting historical privilege into a permanent veto power.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Groups or industries that benefit from existing laws and policies. They rely on the supermajority threshold to prevent majoritarian efforts to change regulations, redistribute resources, or alter economic structures that favor them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    powerful, biographical, constrained, national).

% The majority of the population whose collective will, expressed through democratic processes, is consistently blocked by the supermajority requirement. They bear the costs of unmet policy needs, delayed reforms, and a political system unresponsive to their demands.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, constrained, national).

% Marginalized communities and populations who suffer most directly from the inability to enact reforms. Their interests are often systematically excluded by the entrenched status quo, and they have the fewest options for exit or resistance within the existing system.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, disenfranchised_groups, payer,
    powerless, generational, trapped, local).

% Academics who study the effects of constitutional design, including supermajority rules, on democratic responsiveness, stability, and equity. They analyze empirical data and theoretical models to understand the constraint's operation.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, political_scientists_observers, observer,
    analytical, biographical, analytical, universal).

% Legal experts who interpret constitutional texts and debate the normative implications of supermajority clauses. They engage in discourse about whether such thresholds serve their intended purpose or have become tools for entrenchment.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The supermajority threshold nominally coordinates constitutional stability by requiring broad consensus for fundamental changes, aiming to protect against transient majoritarian impulses and ensure enduring legitimacy for amendments.
% TRANSFER_FUNCTION: This constraint transfers effective political power and the ability to shape policy outcomes from contemporary majorities to entrenched minorities and beneficiaries of the status quo, by granting them a veto over change.
% ABSENT_VOICES: Future generations, whose interests are not represented in the current political structure, and those who are systematically excluded from the political process by the very structures the supermajority protects, are absent. They would advocate for a more adaptable and responsive constitutional framework.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, it would fundamentally alter the balance of power. Majoritarian reforms, currently blocked, would likely pass, leading to significant shifts in policy, resource distribution, and potentially a more responsive political system. The political landscape would reorganize around a more direct form of majoritarian rule.
% FOUNDING_PROBLEM: The constraint was originally designed to prevent hasty or tyrannical majoritarian changes to fundamental law, ensuring that constitutional amendments reflect a deep, persistent, and broad democratic consensus rather than transient passions or narrow interests.
% FOUNDING_PROBLEM_CORROBORATION: While proponents of the supermajority threshold still claim it protects against tyranny, political scientists, historians, and contemporary majoritarian movements widely attest that the original problem is largely solved or that the mechanism has overshot its purpose, now primarily serving to entrench historical privilege and block necessary reforms. Legislative hearing testimony and independent academic analyses support this shifted-function reading.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_vs_veto_ambiguity,
    'Is the supermajority threshold primarily functioning as a genuine safeguard for broad consensus, or as a tool for a blocking minority to exercise a permanent veto?',
    'Empirical analysis of legislative outcomes over time: if a significant number of reforms with clear majoritarian support are consistently blocked by a small minority, it supports the veto reading. If only truly divisive or radical proposals are blocked, it supports the consensus safeguard reading.',
    'If resolved as a veto, the constraint''s effective extractiveness and suppression are higher, reinforcing its ''snare'' classification. If resolved as a consensus safeguard, it would lean towards a ''tangled_rope'' or even ''rope'' classification, with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_vs_veto_ambiguity, empirical, 'Distinguishing between consensus-building and minority veto power.').

omega_variable(
    framers_intent_vs_contemporary_effect,
    'To what extent does the contemporary effect of the supermajority threshold align with the original intent of its framers, and how much has its function drifted?',
    'Historical research into constitutional debates and political theory of the founding era, compared with modern political science analysis of legislative gridlock and policy outcomes.',
    'If a significant divergence is found, it strengthens the argument for mandatrophy and the ''snare'' classification, indicating the constraint persists for reasons other than its original justification. If alignment is strong, it would challenge the ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framers_intent_vs_contemporary_effect, conceptual, 'Assessing the drift between original intent and current function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t1980, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(supe_tr_t1988, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1988, 0.3).
narrative_ontology:measurement(supe_tr_t1996, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(supe_tr_t2004, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2004, 0.4).
narrative_ontology:measurement(supe_tr_t2012, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2012, 0.43).
narrative_ontology:measurement(supe_tr_t2020, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(supe_be_t1980, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(supe_be_t1988, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1988, 0.72).
narrative_ontology:measurement(supe_be_t1996, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 1996, 0.78).
narrative_ontology:measurement(supe_be_t2004, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2004, 0.81).
narrative_ontology:measurement(supe_be_t2012, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2012, 0.83).
narrative_ontology:measurement(supe_be_t2020, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t1980, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(supe_su_t1988, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1988, 0.77).
narrative_ontology:measurement(supe_su_t1996, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 1996, 0.83).
narrative_ontology:measurement(supe_su_t2004, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2004, 0.86).
narrative_ontology:measurement(supe_su_t2012, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(supe_su_t2020, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
