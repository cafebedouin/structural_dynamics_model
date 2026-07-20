% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Sovereignty as Conditional Responsibility (R2P)
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the conditional_responsibility reading of
 *   the westphalia_sovereignty kernel: states forfeit territorial
 *   inviolability when they fail to protect populations from mass atrocities.
 *   Formalized through the Responsibility to Protect (R2P) doctrine in the
 *   2005 World Summit Outcome, it lowers the intervention threshold below
 *   absolute non-intervention while claiming to preserve sovereignty as
 *   responsibility rather than privilege. The constraint is administered by
 *   the UN Security Council and global governance institutions. It carries
 *   genuine coordination function (halting atrocities through multilateral
 *   authorization) alongside asymmetric extraction (selective enforcement
 *   concentrating adjudicative power in the P5, intervention legitimacy in
 *   Western-led coalitions, and sovereignty costs on weak or geopolitically
 *   isolated states).
 *
 * KEY AGENTS:
 *   - global_governance_institutions: Primary agenda-setter (institutional/global/constrained) â adjudicates atrocity thresholds and authorizes intervention
 *   - humanitarian_intervention_coalitions: Primary beneficiary (powerful/global/constrained) â collects legitimacy and legal cover for military action
 *   - populations_under_atrocity_regimes: Primary target (powerless/local/trapped) â bears costs of both non-intervention and intervention
 *   - failing_states: Sovereignty target (moderate/national/trapped) â loses territorial inviolability without recourse
 *   - global_south_dissenters: Excluded voice (organized/global/constrained) â contests selective application but is marginalized procedurally
 *   - international_legal_scholars: Analytical observer (analytical/global/analytical) â documents the gap between universal claims and selective activation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.75).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.7).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.75).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Sovereignty as Conditional Responsibility (R2P)").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '0ac8d2e0-933c-4a64-bd0d-994b8b8a3561').
narrative_ontology:cs_kernel_codification('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', formalized).
narrative_ontology:cs_authority_grounding('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', lineage).
narrative_ontology:cs_interpretation_layer_present('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561').
narrative_ontology:cs_reading_relation('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', sovereignty_conditional_on_protection, conventional).
narrative_ontology:cs_axiom('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', foundational, atrocity_threshold_triggers_forfeiture).
narrative_ontology:cs_axiom_status(atrocity_threshold_triggers_forfeiture, holdable).
narrative_ontology:cs_axiom_grounding('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', atrocity_threshold_triggers_forfeiture, conventional).
narrative_ontology:cs_reference_frame('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', sovereignty_as_responsibility).
narrative_ontology:cs_drift_state('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', post_r2p_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0ac8d2e0-933c-4a64-bd0d-994b8b8a3561', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, failing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the conditional sovereignty framework through Security Council authorization, General Assembly resolutions, and ICC jurisdiction. They adjudicate when atrocity thresholds are met and authorize intervention, gaining institutional authority and relevance from the doctrine's activation.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Gain multilateral legitimacy and legal cover for military intervention when atrocity thresholds are declared met. Their operations depend on the constraint's activation to distinguish humanitarian enforcement from aggression, collecting geopolitical and normative benefits from authorized action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, constrained, global).

% Bear the costs of both atrocity and intervention. When the constraint fails to trigger, they remain unprotected; when it activates, they often suffer collateral damage from military intervention. They have no exit from the territorial state and no voice in the international adjudication.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer,
    powerless, immediate, trapped, local).

% Lose territorial inviolability and sovereign equality when adjudicated to have failed the responsibility to protect. They bear the costs of military intervention, regime change, and territorial partition, with no recourse against the adjudication due to power asymmetries in global governance.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, failing_states, payer,
    moderate, biographical, trapped, national).

% Contest the selective application of the doctrine, noting that intervention correlates with geopolitical interest rather than atrocity severity. They are procedurally marginalized in Security Council decisions and their objections are treated as obstruction rather than legitimate critique.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_south_dissenters, excluded,
    organized, generational, constrained, global).

% Analyze the gap between the doctrine's universal claims and its selective activation. They document when atrocity thresholds are ignored for allied states and manufactured for adversaries, providing the empirical record of the constraint's drift.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a multilateral threshold and authorization procedure for overriding territorial sovereignty to halt mass atrocities, converting unilateral intervention impulses into a collectively administered framework.
% TRANSFER_FUNCTION: Moves territorial inviolability from states that fail to protect populations to intervention coalitions and global governance institutions when an atrocity threshold is adjudicated to have been met.
% ABSENT_VOICES: Populations in non-strategic regions where atrocities fail to trigger international response, and Global South states contesting selective application, are structurally excluded from the adjudication room.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the legal architecture for R2P-authorized intervention would collapse; atrocity responses would revert to raw power politics or absolute non-intervention, and the UN Security Council would lose its claimed monopoly on legitimate coercion.
% FOUNDING_PROBLEM: Post-Cold War mass atrocities in Rwanda and the Balkans revealed the lethal cost of absolute non-intervention while Great Power disagreement paralyzed Chapter VII collective security.
% FOUNDING_PROBLEM_CORROBORATION: The International Commission on Intervention and State Sovereignty (ICISS) attested the problem from outside pure state interest in its 2001 report. However, corroboration is contested because the same major powers that endorsed R2P subsequently activated it selectively, suggesting the founding problem narrative serves as cover for geopolitical extraction.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint concentrates adjudicative authority in P5-dominated institutions and licenses intervention disproportionately against geopolitically weak or isolated targets. Suppression is high (0.70) because the framework actively delegitimizes alternative models (absolute non-intervention, regional autonomy) by branding them as indifference to atrocity. Theater ratio is substantial (0.55): annual R2P debates, Special Adviser offices, and Focal Points sustain performative activity that exceeds functional protection outcomes. Accessibility collapse (0.60) reflects that once R2P became the dominant frame, alternatives like unilateral humanitarian intervention or absolute sovereignty collapsed as discursively viable. Resistance (0.65) captures persistent Global South opposition and P5 vetoes that fragment application. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The humanitarian_intervention_coalition seat computes the constraint as genuine coordination (it solves the collective-action problem of mobilizing multilateral will against atrocities). The failing_state and global_south_dissenter seats compute it as extraction (adjudication tracks power, not atrocity). The engine derives this divergence from the structural data: same constraint, same metrics, different directionalities based on beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   global_governance_institutions and humanitarian_intervention_coalitions are structural beneficiaries (d near 0.0) because the constraint subsidizes their authority and operational legitimacy. populations_under_atrocity_regimes and failing_states are structural targets (d near 1.0) because the constraint extracts sovereignty protection from the latter and physical security from the former (when intervention harms or when non-intervention leaves them exposed). global_south_dissenters are excluded from the beneficiary structure, placing them at high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring BOTH coordination and victim elements for tangled_rope. The genuine coordination function (atrocity prevention through multilateral authorization) is structurally declared via beneficiaries and the coordination_function answer. The extraction is declared via victims and the selectivity documented in measurements. A snare reading would require denying the coordination function entirely; a rope reading would require denying the victims. The mandatrophy question is whether the coordination function has atrophied into theaterâaddressed by the rising theater_ratio and contested founding_problem_status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_ambiguity,
    'Is the conditional responsibility doctrine applied based on atrocity severity or on the geopolitical alignment and power of the target state?',
    'Statistical analysis of intervention authorization versus atrocity severity, controlling for target-state alliance structure and military feasibility.',
    'If selectivity correlates with geopolitical interest, the constraint functions as graded sovereignty in conditional clothing, raising extractiveness and shifting computed classification toward snare. If selectivity correlates with atrocity severity, the coordination function is structurally genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_ambiguity, empirical, 'Whether enforcement tracks atrocity or geopolitics').

omega_variable(
    conditional_vs_graded_boundary,
    'Does the conditional responsibility reading collapse into graded sovereignty in practice, making the distinction between the two readings merely discursive?',
    'Comparative case analysis of intervention thresholds across states with varying institutional capacity and geopolitical alignment, testing whether threshold violations or capacity deficits better predict intervention.',
    'If the distinction collapses in practice, the conditional responsibility reading is a normative cover for scalar capacity-based hierarchy, and the kernel''s sibling readings are functionally indistinguishable in operation despite their discursive separation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_graded_boundary, conceptual, 'Whether conditional and graded sovereignty readings are operationally distinct').

omega_variable(
    absolute_non_intervention_foreclosure,
    'Has the conditional responsibility reading genuinely foreclosed absolute non-intervention in international legal practice, or do they coexist in a layered legal order?',
    'Trace state pleadings and judicial opinions in ICJ cases involving intervention to determine whether absolute non-intervention is treated as overridden or as a residual default.',
    'If absolute non-intervention survives as a residual norm, the foreclosure relation is aspirational rather than structural, and the kernel remains contested rather than resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_non_intervention_foreclosure, conceptual, 'Whether the foreclosure of absolute non-intervention is legally effective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(westphalia_cond_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(westphalia_cond_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.25).
narrative_ontology:measurement(westphalia_cond_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.35).
narrative_ontology:measurement(westphalia_cond_tr_t15, westphalia_sovereignty__conditional_responsibility, theater_ratio, 15, 0.45).
narrative_ontology:measurement(westphalia_cond_tr_t20, westphalia_sovereignty__conditional_responsibility, theater_ratio, 20, 0.5).
narrative_ontology:measurement(westphalia_cond_tr_t25, westphalia_sovereignty__conditional_responsibility, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(westphalia_cond_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(westphalia_cond_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(westphalia_cond_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(westphalia_cond_be_t15, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(westphalia_cond_be_t20, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(westphalia_cond_be_t25, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 25, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(westphalia_cond_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(westphalia_cond_su_t5, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(westphalia_cond_su_t10, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(westphalia_cond_su_t15, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(westphalia_cond_su_t20, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(westphalia_cond_su_t25, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
