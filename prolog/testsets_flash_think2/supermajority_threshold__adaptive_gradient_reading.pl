% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive Gradient Reading)
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'adaptive gradient' reading of
 *   supermajority thresholds, which views them as functional tools whose
 *   legitimacy derives from their calibration to actual social consensus
 *   formation rates and the costs of reversing policy. The threshold is not
 *   intrinsically good or bad, but rather a mechanism that requires
 *   evidence-based tuning to prevent either instability (if too low) or
 *   ossification and extraction (if too high). The metrics reflect a moderate
 *   level of extraction and suppression, acknowledging that even a
 *   'functional tool' can impose costs if not perfectly tuned, or if its
 *   tuning is neglected.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.45).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.65).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, 'deec5be0-d575-46cb-b9d7-262166798e71').
narrative_ontology:cs_kernel_codification('deec5be0-d575-46cb-b9d7-262166798e71', formalized).
narrative_ontology:cs_authority_grounding('deec5be0-d575-46cb-b9d7-262166798e71', expertise).
narrative_ontology:cs_interpretation_layer_present('deec5be0-d575-46cb-b9d7-262166798e71').
narrative_ontology:cs_reading_relation('deec5be0-d575-46cb-b9d7-262166798e71', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('deec5be0-d575-46cb-b9d7-262166798e71', supermajority_threshold__minoritarian_veto_reading, influences).
narrative_ontology:cs_axiom('deec5be0-d575-46cb-b9d7-262166798e71', foundational, threshold_as_functional_tool).
narrative_ontology:cs_axiom_status(threshold_as_functional_tool, holdable).
narrative_ontology:cs_axiom_grounding('deec5be0-d575-46cb-b9d7-262166798e71', threshold_as_functional_tool, empirically_contingent).
narrative_ontology:cs_axiom('deec5be0-d575-46cb-b9d7-262166798e71', foundational, legitimacy_from_performance).
narrative_ontology:cs_axiom_status(legitimacy_from_performance, holdable).
narrative_ontology:cs_axiom_grounding('deec5be0-d575-46cb-b9d7-262166798e71', legitimacy_from_performance, instrumental).
narrative_ontology:cs_reference_frame('deec5be0-d575-46cb-b9d7-262166798e71', optimally_tuned_governance).
narrative_ontology:cs_drift_state('deec5be0-d575-46cb-b9d7-262166798e71', contemporary_political_polarization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('deec5be0-d575-46cb-b9d7-262166798e71', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, status_quo_beneficiaries).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, political_minority).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, political_majority).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, reform_advocates).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, institutional_stability_doctrine).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, deliberative_democracy_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for evaluating and proposing adjustments to supermajority thresholds based on empirical data regarding consensus formation and policy reversibility costs. Their legitimacy depends on evidence-based tuning.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, institutional_designers, agenda_setter,
    analytical, generational, analytical, national).

% Bears the cost of delayed or blocked policy changes that enjoy simple majority support but cannot clear the supermajority threshold. Seeks to lower thresholds to enable more responsive governance.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, political_majority, payer,
    powerful, biographical, constrained, national).

% Benefits from the supermajority threshold as it protects their interests and prevents rapid policy shifts by a simple majority. Often advocates for maintaining or raising thresholds to ensure stability.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, political_minority, beneficiary,
    organized, biographical, mobile, national).

% Analyzes the functional performance of supermajority thresholds, advocating for adjustments that optimize for long-term societal well-being, balancing stability with adaptability. They are not directly subject to the constraint but seek to influence its tuning.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, public_interest_advocates, observer,
    moderate, generational, analytical, national).

% Benefits from the inertia created by high supermajority thresholds, which protects existing arrangements and privileges from being easily overturned. Often aligns with political minorities to resist threshold adjustments.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, status_quo_beneficiaries, beneficiary,
    powerful, generational, constrained, national).

% Represents groups pushing for specific policy changes that are blocked by supermajority requirements. They bear the costs of inaction and advocate for thresholds to be tuned to allow necessary reforms.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, reform_advocates, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__adaptive_gradient_reading, status_quo_beneficiaries).
narrative_ontology:fixing_cost_class(supermajority_threshold__adaptive_gradient_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pace and durability of constitutional and legislative change, balancing the need for stability with the capacity for adaptation, by requiring a higher bar for certain decisions.
% TRANSFER_FUNCTION: Transfers decision-making power from simple majorities to a higher consensus threshold, imposing costs of delay and inaction on those seeking change, while conferring stability and protection on existing arrangements and minority interests.
% ABSENT_VOICES: Future generations, whose interests in adaptable governance may be undermined by ossified thresholds, and those whose urgent needs are unmet due to the difficulty of enacting necessary reforms.
% DISAPPEARANCE_RATIONALE: If supermajority thresholds vanished overnight, the fundamental rules of governance would revert to simple majority rule, leading to a rapid increase in policy volatility, potentially undermining institutional stability and minority protections. The entire political system would reorganize around a lower bar for change.
% FOUNDING_PROBLEM: To prevent transient majoritarian passions from undermining fundamental constitutional principles or minority rights, and to ensure that significant changes reflect a deeper, more durable societal consensus, while also managing the costs of policy reversibility.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, political scientists, and historical analyses of constitutional crises corroborate the ongoing relevance of balancing stability and adaptability. Independent institutional design reviews and comparative constitutional studies provide external validation for the problem's persistence, distinct from the self-serving claims of those benefiting from the status quo.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).
:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate, reflecting the inherent costs of a higher decision-making bar, which can become extractive if the threshold is poorly calibrated or captured by status quo interests. Suppression (0.65) is high because the threshold actively blocks simple majority action. Theater ratio (0.2) is low, as the mechanism is genuinely functional in its intent, even if its performance is suboptimal. The claimed type is 'rope' because, in this reading, the threshold's ideal function is to coordinate stability and adaptability for collective benefit, rather than to extract.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional designers, the threshold is a tool to be optimized. From the political majority, it's a barrier. From the political minority, it's a safeguard. The engine's per-seat classification will reflect these divergences, measuring how the 'rope' claimed type is experienced differently based on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional designers and public interest advocates are positioned as observers or agenda-setters, seeking to optimize the constraint. Political majorities and reform advocates are payers, bearing the costs of blocked change. Political minorities and status quo beneficiaries are beneficiaries, protected by the higher bar for change. The 'adaptive gradient' perspective implies that these directionalities are not fixed but should shift with proper tuning.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_effectiveness,
    'Is the supermajority threshold actually calibrated to current social consensus formation rates and policy reversibility costs, or is it an artifact of historical design?',
    'Empirical studies comparing legislative outcomes and public opinion with threshold levels, and cost-benefit analyses of policy reversals.',
    'If miscalibrated, the constraint''s effective extractiveness and suppression are higher than optimal, potentially leading to reclassification as a Tangled Rope or Snare from the payer seats. If well-calibrated, it reinforces the Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(calibration_effectiveness, empirical, 'Whether the threshold''s design aligns with its functional purpose.').

omega_variable(
    measurement_of_consensus_costs,
    'Are ''social consensus formation rates'' and ''reversibility costs'' objectively measurable in a way that can reliably inform threshold tuning?',
    'Development and validation of robust quantitative metrics and methodologies by independent academic bodies, accepted across political science and economics.',
    'If these metrics are not reliably measurable, the ''adaptive gradient'' reading''s claim to evidence-based tuning is undermined, potentially shifting its authority grounding from ''expertise'' to ''conventional'' or ''extraction''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_consensus_costs, conceptual, 'The epistemic feasibility of the adaptive gradient''s core premise.').

omega_variable(
    capture_of_tuning_process,
    'Is the process of ''evidence-based tuning'' itself susceptible to capture by status quo beneficiaries or political minorities, preventing optimal adaptation?',
    'Analysis of legislative history and lobbying efforts surrounding past attempts to adjust supermajority thresholds, and comparative studies of institutional design processes.',
    'If the tuning process is captured, the constraint''s claimed ''rope'' function is compromised, and its effective extractiveness for political majorities and reform advocates would be higher, pushing it towards a Tangled Rope or Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_of_tuning_process, empirical, 'Whether the tuning mechanism itself is free from extractive influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'supermajority_threshold' kernel, each representing a distinct structural claim about its function and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
