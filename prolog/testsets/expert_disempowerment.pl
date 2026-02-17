% ============================================================================
% CONSTRAINT STORY: expert_disempowerment
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_expert_disempowerment, []).

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
 * * constraint_id: expert_disempowerment
 * human_readable: Algorithmic Oversight Erosion
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents the systematic stripping of discretionary power
 * from domain experts (e.g., doctors, engineers) in favor of rigid, automated
 * decision-support systems. While marketed as "coordination," it functions
 * as a Snare by removing human accountability and professional agency, capturing
 * efficiency gains for the institution while offloading liability.
 * * KEY AGENTS:
 * - Domain Specialist: Subject (Powerless)
 * - System Administrator/Institution: Beneficiary (Institutional)
 * - Liability Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.74) because the "efficiency" gains are captured by the
% institution while the expert bears the cognitive load of a "theater" of choice.
domain_priors:base_extractiveness(expert_disempowerment, 0.74).
domain_priors:suppression_score(expert_disempowerment, 0.65).
domain_priors:theater_ratio(expert_disempowerment, 0.82). % High theater: The expert is "consulted" but cannot override.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(expert_disempowerment, extractiveness, 0.74).
narrative_ontology:constraint_metric(expert_disempowerment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(expert_disempowerment, theater_ratio, 0.82).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(expert_disempowerment, tangled_rope).
narrative_ontology:human_readable(expert_disempowerment, "Algorithmic Oversight Erosion").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(expert_disempowerment).
narrative_ontology:constraint_beneficiary(expert_disempowerment, system_administrators).
narrative_ontology:constraint_victim(expert_disempowerment, domain_specialists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The specialist is trapped in a workflow where they have responsibility but no authority.
constraint_indexing:constraint_classification(expert_disempowerment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the automation as a Rope for scale, reliability, and risk mitigation.
constraint_indexing:constraint_classification(expert_disempowerment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context reveals the hybrid nature: a coordination function
% (standardization) coupled with high, asymmetric extraction (loss of agency).
constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A different analytical view, focused on function vs performance.
% Theater ratio (0.82) > 0.70 triggers Piton: The expert's role is now an inertial relic.
constraint_indexing:constraint_classification(expert_disempowerment, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(expert_disempowerment, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expert_disempowerment_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict.
    constraint_indexing:constraint_classification(expert_disempowerment, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(expert_disempowerment, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger_logic) :-
    % Ensure high theater results in Piton classification for auditors.
    constraint_indexing:constraint_classification(expert_disempowerment, piton,
        context(agent_power(analytical), time_horizon(historical), exit_options(arbitrage), _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(expert_disempowerment),
    narrative_ontology:constraint_beneficiary(expert_disempowerment, _),
    narrative_ontology:constraint_victim(expert_disempowerment, _).

:- end_tests(expert_disempowerment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) represents the loss of professional optionality and agency,
 * which is captured by the institution as reduced liability and operational cost.
 * The high theater_ratio (0.82) is key—the expert is kept in the loop purely
 * for legal liability ("human in the loop" theater), not for operational leverage.
 * The suppression score (0.65) reflects the lack of alternative workflows; non-compliance
 * is grounds for dismissal.
 *
 * PERSPECTIVAL GAP:
 * The Expert feels a Snare because their skill is commoditized and their authority removed.
 * The Institution feels a Rope because their liability is standardized and operations are scaled.
 * The Analytical observer sees a Tangled Rope, acknowledging both the coordination function
 * and the severe asymmetric extraction.
 *
 * [RESOLVED MANDATROPHY]:
 * This constraint presents a classic Mandatrophy risk: a system with a valid coordination
 * function (Rope) that also functions as a severe extraction mechanism (Snare).
 * The Tangled Rope classification resolves this by acknowledging both facets. Furthermore,
 * the Piton classification from the auditor's perspective highlights the system's decay:
 * the human role has become so performative (theater > 0.7) that it's now an inertial
 * component maintained for liability rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_discretionary_value,
    'Is human discretion a bottleneck (Mountain of human error) or a necessary safety valve (making its removal a Snare)?',
    'Blind audit of "Expert Override" vs "System Suggestion" outcomes in critical failure scenarios over a long time horizon.',
    'If Expert > System: Confirms Snare of automation. If System > Expert: Suggests a Mountain of human cognitive limits.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(expert_disempowerment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as automation became more trusted and
% institutional power was consolidated.
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(ed_tr_t0, expert_disempowerment, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ed_tr_t5, expert_disempowerment, theater_ratio, 5, 0.65).
narrative_ontology:measurement(ed_tr_t10, expert_disempowerment, theater_ratio, 10, 0.82).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(ed_ex_t0, expert_disempowerment, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ed_ex_t5, expert_disempowerment, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(ed_ex_t10, expert_disempowerment, base_extractiveness, 10, 0.74).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The system functions as a rigid enforcement mechanism for institutional policy.
narrative_ontology:coordination_type(expert_disempowerment, enforcement_mechanism).

% This constraint is often coupled with liability and insurance frameworks,
% where algorithmic compliance lowers institutional risk premiums.
narrative_ontology:affects_constraint(expert_disempowerment, professional_liability_framework).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */