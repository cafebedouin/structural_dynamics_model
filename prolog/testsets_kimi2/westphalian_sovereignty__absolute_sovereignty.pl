% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Westphalian Absolute Sovereignty Norm
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the absolute sovereignty reading of
 *   the contested Westphalian sovereignty kernel. Under this reading,
 *   sovereignty grants states unconditional authority over domestic affairs
 *   and renders external interference categorically illegitimate. The norm is
 *   codified in the UN Charter and defended by a sovereigntist state
 *   coalition. While it coordinates interstate order by assigning exclusive
 *   territorial jurisdiction, it simultaneously operates as an asymmetric
 *   extraction mechanism: state executives collect a non-interference shield
 *   that blocks humanitarian intervention and external accountability, while
 *   domestic populations under repressive regimes bear the costs of denied
 *   protection. The structural asymmetry between the beneficiary state and
 *   the trapped domestic population is the core of the tangled-rope
 *   classification.
 *
 * KEY AGENTS:
 *   - State executives (agenda-setter/beneficiary): Institutional power, generational time horizon, constrained exit; defend and collect from the absolute non-interference shield.
 *   - Domestic populations under repression (payer): Powerless, immediate time horizon, trapped exit; bear the costs of blocked external recourse.
 *   - Humanitarian intervention advocates (excluded): Organized, constrained exit; procedurally marginalized by the absolutist veto.
 *   - International human rights monitoring bodies (observer): Institutional, constrained exit; document gaps without enforcement authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.58).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.72).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Westphalian Absolute Sovereignty Norm").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '8500c6fe-d90b-4842-aea4-e0c9b4aa0163').
narrative_ontology:cs_kernel_codification('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', formalized).
narrative_ontology:cs_authority_grounding('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', lineage).
narrative_ontology:cs_interpretation_layer_present('8500c6fe-d90b-4842-aea4-e0c9b4aa0163').
narrative_ontology:cs_reading_relation('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', foundational, unconditional_domestic_supremacy).
narrative_ontology:cs_axiom_status(unconditional_domestic_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', unconditional_domestic_supremacy, conventional).
narrative_ontology:cs_axiom('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', foundational, external_interference_prohibition).
narrative_ontology:cs_axiom_status(external_interference_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', external_interference_prohibition, conventional).
narrative_ontology:cs_reference_frame('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', westphalian_territorial_autonomy).
narrative_ontology:cs_drift_state('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8500c6fe-d90b-4842-aea4-e0c9b4aa0163', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, state_executives).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce the absolute sovereignty norm through the UN Charter, international treaties, and customary law; benefit from a legal shield that blocks external scrutiny and intervention in domestic governance regardless of human rights conduct.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, state_executives, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, state_executives, beneficiary).

% Bear the costs of closed political space and denied external protection; lack standing in international law to challenge the absolutist shield that insulates their governments from humanitarian intervention or accountability.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, national).

% Advance responsibility-to-protect and human rights enforcement frameworks; are procedurally excluded from legitimate action by the absolute non-interference principle and the institutional veto it empowers.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_advocates, excluded,
    organized, biographical, constrained, global).

% Document violations and publish findings but lack enforcement authority against states that invoke absolute sovereignty; observe the structural gap between human rights norms and the non-interference shield.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_human_rights_monitoring_bodies, observer,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, state_executives).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interstate order by assigning exclusive territorial jurisdiction to recognized states and eliminating competing external claims to authority, thereby suppressing constant warfare over who governs whom.
% TRANSFER_FUNCTION: Transfers authority and impunity from the international community to state executives, and transfers the costs of repression from those executives to domestic populations who are denied external recourse.
% ABSENT_VOICES: Domestic populations under repression have no seat at the international law table; humanitarian intervention advocates are procedurally marginalized by the absolutist veto and Charter-based non-interference doctrines.
% DISAPPEARANCE_RATIONALE: If absolute sovereignty vanished overnight, the interstate system would face immediate claims for intervention in repressive states, international institutions would reconfigure around conditional or graduated authority, and state executives would lose their legal shield against external scrutiny and humanitarian action.
% FOUNDING_PROBLEM: Chronic interstate warfare in Europe driven by religious and dynastic claims to authority across territorial boundaries; the Peace of Westphalia sought to end external claims to domestic jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: International historians attest the founding problem was 17th-century European religious conflict. Human rights scholars and international legal theorists from outside the beneficiary states attest the problem has mutated and the absolutist solution now generates distinct harms; sovereigntist legal scholars within the beneficiary tradition contest this reading.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.58 because the shield decouples state conduct from external accountability, enabling sustained repression that extracts security and autonomy from domestic populations. Suppression is higher (0.72) because the norm requires active institutional defenseâUN veto usage, diplomatic coalitions, and legal doctrinal workâto block the humanitarian intervention alternatives that would otherwise erode it. Theater ratio is moderate (0.42): much UN discourse defending absolute sovereignty is performative repetition of Westphalian formulas that mask functional state practice of selective compliance and strategic hypocrisy. Accessibility collapse (0.60) reflects that alternatives like R2P have been partially institutionalized but remain blocked by procedural veto. Resistance (0.55) captures sustained contestation from human rights advocates and some liberal states. The measurement series run on a single shared time grid (1945â2024) so every metric is sampled at every observed time point.
 *
 * PERSPECTIVAL GAP:
 *   The state executive seat experiences the constraint as foundational order and legitimate defense of self-determination; the domestic population under repression experiences the same structure as abandonment and denied agency. The engine computes this divergence from the structural dataâbeneficiary/victim declarations, power asymmetry, and exit differentiationâwithout requiring a reconciled single type.
 *
 * DIRECTIONALITY LOGIC:
 *   State executives are declared beneficiaries with institutional power and constrained exit, placing their directionality near the beneficiary end (low d). Domestic populations are declared victims with powerless status and trapped exit, placing their directionality near the full-target end (high d). Humanitarian advocates are excluded from the arrangement entirely and carry no beneficiary offset. The engine will amplify effective extraction for the trapped domestic population and damp it for the state executive beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâEuropean religious wars and cross-border dynastic claimsâwas substantially solved or transformed by the mid-20th century, yet the arrangement persists. The persistence is not merely inertia; it has been actively recaptured by repressive regimes as a shield against human rights conditionality. This prevents mislabeling the constraint as pure coordination (rope) by documenting that the coordination function now rides on asymmetric extraction, and prevents mislabeling it as pure snare by acknowledging the genuine interstate-order function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_shield,
    'Is absolute sovereignty a spontaneous feature of international order that would persist without enforcement, or a constructed legal shield maintained by state elites to prevent external accountability?',
    'Comparative historical analysis of pre-Westphalian order and counterfactual analysis of interstate system stability under conditional sovereignty frameworks.',
    'If constructed rather than spontaneous, the constraint is a false-summit candidate and its classification as tangled_rope is reinforced; if spontaneous, its extraction may be re-evaluated as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_shield, conceptual, 'Whether the sovereignty norm is natural law or constructed extraction.').

omega_variable(
    beneficiary_concentration,
    'Does the non-interference shield benefit all states equally, or is the extraction asymmetrically captured by repressive regimes at the expense of their domestic populations?',
    'Empirical analysis of sovereignty invocation patterns: frequency and success of absolute sovereignty defenses correlated with regime type and human rights conduct.',
    'If benefit is universal and symmetric, the constraint trends toward rope; if concentrated in repressive regimes, tangled_rope is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Whether sovereignty benefits are symmetrically or asymmetrically distributed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of humanitarian intervention primarily structural (UN veto, Charter doctrine, institutional rules) or internalized (state elites normatively identity-locked to non-interference as constitutive of statehood)?',
    'Post-crisis trajectory analysis: when structural barriers to intervention are temporarily lifted, do state elites continue to resist intervention normatively?',
    'If internalized, effective suppression exceeds the structural measure and the constraint''s inertia is stronger than institutional analysis suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of intervention alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1960, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(west_be_t1960, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(west_su_t1960, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalian_sovereignty kernel, decomposed from the colloquial label into structurally distinct claims: absolute, conditional, and graduated sovereignty. Each reading carries a different epsilon, stakeholder structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
