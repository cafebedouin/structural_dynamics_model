% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Referee Authority
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint describes the authority of the WTO Dispute Settlement
 *   Body (DSB) as a binding referee, where its panel rulings create
 *   compliance obligations for member states, grounded in treaty law. From
 *   this 'binding referee' perspective, member states have explicitly
 *   surrendered a degree of policy discretion within WTO-covered domains in
 *   exchange for the benefits of a stable, rules-based global trading system.
 *   Non-compliance is viewed as a treaty violation, potentially leading to
 *   authorized retaliation, rather than a mere policy choice.
 *
 * KEY AGENTS:
 *   - wto_member_states_seeking_enforcement: Beneficiary (institutional/mobile) — benefits from enforcement of rules
 *   - wto_member_states_found_in_violation: Payer (institutional/constrained) — bears costs of compliance or retaliation
 *   - wto_secretariat_and_panels: Agenda Setter (institutional/analytical) — administers and interprets treaty law
 *   - domestic_policy_autonomy: Victim (non-agent/trapped) — the policy space constrained by rulings
 *   - global_trading_system: Beneficiary (non-agent/universal) — benefits from stability and predictability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.65).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Referee Authority").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '7caa6d60-c76e-4b49-b06e-edd792648b5b').
narrative_ontology:cs_kernel_codification('7caa6d60-c76e-4b49-b06e-edd792648b5b', formalized).
narrative_ontology:cs_authority_grounding('7caa6d60-c76e-4b49-b06e-edd792648b5b', lineage).
narrative_ontology:cs_interpretation_layer_present('7caa6d60-c76e-4b49-b06e-edd792648b5b').
narrative_ontology:cs_reading_relation('7caa6d60-c76e-4b49-b06e-edd792648b5b', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('7caa6d60-c76e-4b49-b06e-edd792648b5b', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('7caa6d60-c76e-4b49-b06e-edd792648b5b', foundational, treaty_law_is_supreme).
narrative_ontology:cs_axiom_status(treaty_law_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('7caa6d60-c76e-4b49-b06e-edd792648b5b', treaty_law_is_supreme, deontological).
narrative_ontology:cs_axiom('7caa6d60-c76e-4b49-b06e-edd792648b5b', foundational, sovereignty_can_be_pooled_for_mutual_gain).
narrative_ontology:cs_axiom_status(sovereignty_can_be_pooled_for_mutual_gain, holdable).
narrative_ontology:cs_axiom_grounding('7caa6d60-c76e-4b49-b06e-edd792648b5b', sovereignty_can_be_pooled_for_mutual_gain, conventional).
narrative_ontology:cs_reference_frame('7caa6d60-c76e-4b49-b06e-edd792648b5b', rules_based_multilateralism).
narrative_ontology:cs_drift_state('7caa6d60-c76e-4b49-b06e-edd792648b5b', contemporary_era_of_nationalism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7caa6d60-c76e-4b49-b06e-edd792648b5b', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_states_seeking_enforcement).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, global_trading_system).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, wto_member_states_found_in_violation).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because states must alter domestic policies or face trade sanctions, representing a direct cost. Suppression (0.75) is high due to the binding nature of rulings and the threat of authorized retaliation, which limits policy alternatives. Theater ratio (0.1) is low, as the DSB's function is largely effective and not performative; its rulings have real-world consequences. Accessibility collapse (0.7) is high because once a ruling is made, the policy space for the violating state is significantly narrowed. Resistance (0.3) is moderate, as states often challenge rulings but ultimately face strong pressure to comply.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states seeking enforcement, the DSB is a crucial mechanism for fair trade, ensuring compliance and predictability. For states found in violation, it is an external imposition that curtails sovereign policy space. The WTO Secretariat and panels view it as upholding the rules-based international order. These divergent views are captured by the different roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   WTO member states seeking enforcement are beneficiaries (d=0.0-0.2) as the system works to their advantage. WTO member states found in violation are targets (d=0.8-1.0) as they bear the direct costs of compliance or retaliation. The WTO Secretariat and panels are agenda setters (d=0.4-0.6), administering the system. Domestic policy autonomy is a victim (d=1.0) as it is directly curtailed. The global trading system is a diffuse beneficiary (d=0.0).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the DSB as a pure Snare, acknowledging its genuine coordination function in providing a stable, rules-based trading environment. However, it also highlights the asymmetric extraction from states found in violation, which is sustained by active enforcement and the suppression of alternative policy choices. The 'binding referee' reading emphasizes the enforcement aspect, which is critical for the Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the WTO DSB''s authority truly binding and referee-like, or is it primarily advisory and coordination-focused?',
    'Analysis of compliance rates without authorized retaliation, and the frequency of negotiated settlements versus imposed remedies.',
    'If primarily advisory, the constraint''s effective extraction and suppression are lower, reclassifying it closer to a Rope. If binding, the current classification as Tangled Rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'This constraint is the ''binding_referee_reading'' of the ''wto_dsb_authority'' kernel. Sibling readings (''advisory_coordination_reading'', ''judicial_activism_reading'') would alter the perceived binding nature and legitimacy of DSB rulings.').

omega_variable(
    sovereignty_tradeoff_legitimacy,
    'To what extent did member states genuinely consent to surrender policy discretion in exchange for market access, versus being coerced by the structure of global trade?',
    'Historical analysis of treaty negotiations, state declarations, and the presence of viable exit options for states from the WTO system.',
    'If consent was genuinely free, the extraction is a legitimate cost of coordination. If coerced, the extraction is more akin to a Snare, as the ''benefit'' of market access is effectively unavoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_tradeoff_legitimacy, preference, 'Assesses the normative grounding of the trade-off between sovereignty and market access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__binding_referee_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__binding_referee_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__binding_referee_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__binding_referee_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__binding_referee_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__binding_referee_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.1).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'wto_dsb_authority' kernel. It emphasizes the binding nature of DSB rulings, contrasting with the 'advisory_coordination_reading' (which sees DSB as facilitating negotiation) and the 'judicial_activism_reading' (which views DSB as overstepping its mandate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
