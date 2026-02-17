% ============================================================================
% CONSTRAINT STORY: ibm_shield_contract_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_ibm_shield_contract_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ibm_shield_contract_2026
 * human_readable: IBM SHIELD IDIQ Program Contract
 * domain: technological/political
 * * SUMMARY:
 * IBM has secured a massive IDIQ (Indefinite Delivery, Indefinite Quantity)
 * contract for the SHIELD program, intended to automate the military OODA loop
 * (Observe, Orient, Decide, Act) via AI-enabled sensing and command. While
 * framed as a "Rope" for national survival, the "mission-grade security" and
 * proprietary nature create a high-extraction environment that benefits the
 * contractor and military apparatus while suppressing democratic oversight.
 * * KEY AGENTS:
 * - Democratic Oversight Bodies: Subject (Powerless)
 * - Defense Contractors & Military Leadership: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ibm_shield_contract_2026, 0.62). % High capital allocation away from public funds.
domain_priors:suppression_score(ibm_shield_contract_2026, 0.78).   % Mission-grade secrecy suppresses alternatives and oversight.
domain_priors:theater_ratio(ibm_shield_contract_2026, 0.18).       % High functional delivery, not just performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ibm_shield_contract_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(ibm_shield_contract_2026, theater_ratio, 0.18).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(ibm_shield_contract_2026, tangled_rope).
narrative_ontology:human_readable(ibm_shield_contract_2026, "IBM SHIELD IDIQ Program Contract").

% Binary flags
domain_priors:requires_active_enforcement(ibm_shield_contract_2026). % Contractual obligations and security protocols require active maintenance.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, defense_contractors).
narrative_ontology:constraint_victim(ibm_shield_contract_2026, democratic_oversight).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For oversight bodies, the contract is a Snare. The high suppression (secrecy)
% and extraction (diversion of funds and decision-making power) trap them,
% making meaningful review impossible.
% χ = 0.62 * π(powerless, 1.5) * σ(national, 1.0) = 0.93
constraint_indexing:constraint_classification(ibm_shield_contract_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the institutional beneficiaries (contractors, military), the program is
% a pure Rope, coordinating vast resources towards a critical security goal.
% The extraction is perceived as investment, not loss.
% χ = 0.62 * π(institutional, -0.2) * σ(national, 1.0) = -0.124 (felt as a benefit)
constraint_indexing:constraint_classification(ibm_shield_contract_2026, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view recognizes both functions. It is a genuine coordination
% mechanism (Rope-like) but also features high, asymmetric extraction and
% requires active enforcement to maintain its structure, classifying it as a
% Tangled Rope.
% χ = 0.62 * π(analytical, 1.15) * σ(global, 1.2) = 0.8556
constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_contract_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(ibm_shield_contract_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(ibm_shield_contract_2026, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(ibm_shield_contract_2026, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(ibm_shield_contract_2026).

:- end_tests(ibm_shield_contract_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a large-scale, technologically advanced defense
 * contract. The scores reflect its dual nature.
 * - Base Extractiveness (0.62) is high, representing the immense redirection of
 *   public capital and decision-making authority to a private-public entity.
 * - Suppression Score (0.78) is also high, stemming from national security
 *   classifications that prevent public scrutiny and competitive alternatives.
 * - Theater Ratio (0.18) is low because the program is expected to be highly
 *   functional, not merely performative.
 *
 * The Perspectival Gap is stark:
 * - For oversight bodies (`powerless`), it's a `Snare`. They are trapped by
 *   secrecy and complexity, unable to perform their function.
 * - For contractors (`institutional`), it's a `Rope`. It's a tool for organizing
 *   resources to achieve a stated, valuable goal.
 *
 * * MANDATROPHY ANALYSIS:
 * The `Tangled Rope` classification from the analytical perspective is crucial.
 * It prevents the system from simplifying the situation into either a pure
 * `Rope` (ignoring the victims) or a pure `Snare` (ignoring the genuine
 * coordination function for national defense). It correctly identifies the
 * hybrid nature where a coordination goal is used to justify and sustain a
 * system of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_shield_contract_2026,
    'Does the AI automation component functionally remove human-in-the-loop veto power over lethal action, despite policy claims to the contrary?',
    'Red-team audits of the SHIELD command-and-control architecture and decision latency data under simulated crisis conditions.',
    'If true, the constraint becomes a near-Mountain of automated violence, escalating its Snare properties. If false, it remains a Tangled Rope where human judgment is the final arbiter.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ibm_shield_contract_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.62 > 0.46), requiring temporal data.
% The model shows extraction accumulating as the program becomes more embedded,
% while theater ratio slightly decreases as initial inefficiencies are resolved.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(isc26_tr_t0, ibm_shield_contract_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(isc26_tr_t5, ibm_shield_contract_2026, theater_ratio, 5, 0.20).
narrative_ontology:measurement(isc26_tr_t10, ibm_shield_contract_2026, theater_ratio, 10, 0.18).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(isc26_ex_t0, ibm_shield_contract_2026, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(isc26_ex_t5, ibm_shield_contract_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(isc26_ex_t10, ibm_shield_contract_2026, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The SHIELD program is a command-and-control system, making it a form of
% enforcement mechanism.
narrative_ontology:coordination_type(ibm_shield_contract_2026, enforcement_mechanism).

% This contract would have significant downstream effects on regulations
% concerning AI in warfare and technology export controls.
narrative_ontology:affects_constraint(ibm_shield_contract_2026, ai_weapon_system_export_controls).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */