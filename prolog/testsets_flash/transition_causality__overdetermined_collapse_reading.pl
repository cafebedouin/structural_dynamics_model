% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Overdetermined Collapse of Fixed Exchange Rate Regime
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint describes the collapse of a fixed exchange rate regime
 *   (e.g., Bretton Woods) as structurally inevitable, driven by multiple
 *   reinforcing contradictions, with the Triffin Dilemma acting as a
 *   Mountain-like constraint. This 'overdetermined collapse' reading
 *   emphasizes that no policy choices could have averted the transition, and
 *   counterfactual viability of the regime was near-zero. The victims are all
 *   actors constrained by the fixed-rate regime, as they are forced to adapt
 *   to the inevitable transition.
 *
 * KEY AGENTS:
 *   - fixed_exchange_rate_regime_participants: Primary victims (powerless/trapped) – forced to adapt to the inevitable collapse.
 *   - academic_theorists_of_structural_inevitability: Primary beneficiaries (analytical/arbitrage) – their theories are vindicated by the collapse.
 *   - policy_makers_of_the_fixed_regime: Payer/excluded (institutional/constrained) – their efforts to maintain the regime are ultimately futile against structural forces.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.1).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.9).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Overdetermined Collapse of Fixed Exchange Rate Regime").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '2f825d89-853e-4b82-93fa-928f711f0424').
narrative_ontology:cs_kernel_codification('2f825d89-853e-4b82-93fa-928f711f0424', implicit).
narrative_ontology:cs_authority_grounding('2f825d89-853e-4b82-93fa-928f711f0424', diffuse_epistemic).
narrative_ontology:cs_reading_relation('2f825d89-853e-4b82-93fa-928f711f0424', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('2f825d89-853e-4b82-93fa-928f711f0424', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('2f825d89-853e-4b82-93fa-928f711f0424', foundational, structural_contradictions_are_determinative).
narrative_ontology:cs_axiom_status(structural_contradictions_are_determinative, holdable).
narrative_ontology:cs_axiom_grounding('2f825d89-853e-4b82-93fa-928f711f0424', structural_contradictions_are_determinative, empirically_contingent).
narrative_ontology:cs_axiom('2f825d89-853e-4b82-93fa-928f711f0424', foundational, counterfactual_viability_is_zero).
narrative_ontology:cs_axiom_status(counterfactual_viability_is_zero, holdable).
narrative_ontology:cs_axiom_grounding('2f825d89-853e-4b82-93fa-928f711f0424', counterfactual_viability_is_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('2f825d89-853e-4b82-93fa-928f711f0424', inherent_systemic_contradictions).
narrative_ontology:cs_drift_state('2f825d89-853e-4b82-93fa-928f711f0424', contemporary_economic_analysis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2f825d89-853e-4b82-93fa-928f711f0424', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, academic_theorists_of_structural_inevitability).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_regime_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, policy_makers_of_the_fixed_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All economic actors (governments, corporations, individuals) operating within the fixed exchange rate system, who are forced to adapt to its inevitable collapse and the resulting economic instability. They have no collective means to avert the structural contradictions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_exchange_rate_regime_participants, payer,
    powerless, immediate, trapped, global).

% Economists and political scientists whose theories predict and explain the structural inevitability of such collapses. The actual collapse vindicates their analytical frameworks and enhances their academic standing.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, academic_theorists_of_structural_inevitability, beneficiary,
    analytical, generational, analytical, global).

% Government officials and central bankers tasked with maintaining the fixed exchange rate regime. Despite their efforts, they are ultimately unable to overcome the inherent structural contradictions, bearing the political and economic costs of the collapse.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, policy_makers_of_the_fixed_regime, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, policy_makers_of_the_fixed_regime, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint itself does not solve a coordination problem; rather, it describes the breakdown of a prior coordination mechanism (the fixed exchange rate regime) due to inherent structural contradictions.
% TRANSFER_FUNCTION: The constraint transfers the costs of systemic instability and forced adaptation from the unsustainable structure to all participants within the fixed exchange rate regime.
% ABSENT_VOICES: Advocates for radical, non-mainstream international monetary reforms who were dismissed during the regime's operation would argue that the collapse was predictable and avoidable with different systemic choices, but their voices were excluded from mainstream policy discourse.
% DISAPPEARANCE_RATIONALE: If the 'inevitability' of the collapse vanished, it would imply that the structural contradictions were not truly insurmountable. However, this reading asserts the inevitability as a fundamental truth of the system's dynamics, meaning its 'disappearance' would fundamentally alter the understanding of international finance itself, rather than just a specific event.
% FOUNDING_PROBLEM: The fixed exchange rate regime was built to solve the problem of international monetary instability and provide a stable framework for global trade and investment after World War II.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought and international relations, as well as later generations of economists, corroborate that the founding problem of post-war instability was addressed, but the solution itself contained the seeds of its own destruction (e.g., the Triffin Dilemma), leading to the problem being 'dead' in its original form but replaced by new, structurally induced problems. This is attested by independent academic analysis, not just beneficiaries of the new floating regime.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_unchanged).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because this reading asserts the inevitability of the collapse, driven by fundamental contradictions like the Triffin Dilemma. Extractiveness is low (0.1) because it's not an active extraction by an agent, but a systemic cost of an unsustainable structure. Suppression is high (0.9) because the structural forces make alternatives nearly impossible, effectively 'suppressing' any attempts to maintain the old regime. Theater ratio is low (0.05) as there's little performative maintenance; the system is genuinely breaking down. Accessibility collapse is high (0.95) as the structural contradictions leave no viable alternatives for the regime's continuation. Resistance is low (0.05) because, from this perspective, resistance is futile against the inevitable.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'fixed_exchange_rate_regime_participants' and 'policy_makers_of_the_fixed_regime', the constraint is a force majeure, an external inevitability. For 'academic_theorists_of_structural_inevitability', it's a vindication of their models. The engine's classification will reflect this divergence, with victims experiencing it as a Mountain (unchangeable limit) and beneficiaries as a confirmation of their analytical framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'academic_theorists_of_structural_inevitability' are beneficiaries (d=0.0) as the collapse validates their theories. The 'fixed_exchange_rate_regime_participants' are victims (d=1.0) as they bear the costs of the inevitable transition. 'Policy_makers_of_the_fixed_regime' are also victims (d=0.9) as their efforts to prevent the collapse are overridden by structural forces.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the collapse as a Snare (pure extraction by an agent) or a Rope (coordination problem with a solution). By framing it as a Mountain, it highlights the systemic, unavoidable nature of the transition, where no single agent is 'extracting' but rather the system itself is unsustainable. The concept of mandatrophy doesn't fully apply here, as the 'mandate' of the fixed regime was inherently contradictory and thus destined to fail, rather than atrophying after fulfilling its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_contingent_causality,
    'Is the collapse of the fixed exchange rate regime a structural inevitability (as this reading claims) or a contingent outcome of policy choices?',
    'Counterfactual historical analysis: if all plausible policy alternatives are shown to lead to the same outcome, it strengthens the inevitability claim. If viable alternative paths are identified, it weakens it.',
    'If resolved as contingent, the constraint shifts from a Mountain (Triffin Dilemma) to a Snare or Tangled Rope, reflecting policy choices rather than natural law. If resolved as inevitable, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_contingent_causality, conceptual, 'Ambiguity between structural inevitability and contingent policy choice in regime transitions.').

omega_variable(
    triffin_dilemma_natural_law_or_construct,
    'Is the Triffin Dilemma a genuine natural law of international finance, or a constructed constraint arising from specific institutional arrangements?',
    'Theoretical analysis of alternative international monetary architectures: if the dilemma persists across all stable architectures, it suggests natural law; if it disappears with different designs, it suggests a construct.',
    'If a construct, the ''mountain'' aspect of the Triffin Dilemma is a false summit, and the constraint''s classification would shift to a Tangled Rope or Snare, with identifiable beneficiaries of the specific institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_dilemma_natural_law_or_construct, conceptual, 'Whether the Triffin Dilemma is an inherent feature of international finance or a product of specific design choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tran_tr_t5, transition_causality__overdetermined_collapse_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(tran_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tran_be_t5, transition_causality__overdetermined_collapse_reading, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(tran_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(tran_su_t5, transition_causality__overdetermined_collapse_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(tran_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'transition_causality' kernel. This 'overdetermined_collapse_reading' emphasizes structural inevitability, contrasting with the 'contingent_choice_reading' (policy decisions could have averted it) and 'hybrid_trigger_reading' (structural contradictions needed trigger events).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
