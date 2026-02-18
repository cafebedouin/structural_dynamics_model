% ============================================================================
% CONSTRAINT STORY: cs_ecmo_bridge
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cs_ecmo_bridge, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cs_ecmo_bridge
 * human_readable: ECMO Bridge to Transplant
 * domain: technological
 * * SUMMARY:
 * An artificial lung system (ECMO) allows surgeons to keep a patient alive for a limited time before a lung transplant. This acts as a bridge, providing crucial time to find a suitable organ and perform the complex surgery. While life-saving, it involves immense cost, risk, and dependency for the patient.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Hospital/Surgeons: Beneficiary (Institutional)
 * - Healthcare System Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cs_ecmo_bridge, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cs_ecmo_bridge, 0.30).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cs_ecmo_bridge, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cs_ecmo_bridge, extractiveness, 0.55).
narrative_ontology:constraint_metric(cs_ecmo_bridge, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(cs_ecmo_bridge, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cs_ecmo_bridge, tangled_rope).
narrative_ontology:human_readable(cs_ecmo_bridge, "ECMO Bridge to Transplant").
narrative_ontology:topic_domain(cs_ecmo_bridge, "technological").

% Binary flags
% narrative_ontology:has_sunset_clause(cs_ecmo_bridge).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cs_ecmo_bridge). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, hospital_system).
narrative_ontology:constraint_victim(cs_ecmo_bridge, patient).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The patient is powerless and trapped, facing immense physical, emotional, and
% financial extraction. The technology is their only hope, but it's a high-risk,
% high-cost dependency. Effective extraction χ = 0.55 * 1.5 (powerless) * 1.0 (national) = 0.825.
constraint_indexing:constraint_classification(cs_ecmo_bridge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The hospital system sees a powerful coordination tool. It enables them to save
% lives that would otherwise be lost, enhancing their capabilities and reputation.
% Effective extraction χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11 (a net benefit).
constraint_indexing:constraint_classification(cs_ecmo_bridge, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with both a genuine coordination function (saving lives)
% and significant asymmetric extraction from the patient. It requires active enforcement
% (medical teams, technology). This is the definition of a Tangled Rope.
% Effective extraction χ = 0.55 * 1.15 (analytical) * 1.2 (global) = 0.759.
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cs_ecmo_bridge_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(tangled_rope_conditions) :-
    % Verify the analytical observer sees a tangled_rope.
    constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope, context(agent_power(analytical), _, _, _)),
    % And that the structural conditions are met.
    domain_priors:requires_active_enforcement(cs_ecmo_bridge),
    narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, _),
    narrative_ontology:constraint_victim(cs_ecmo_bridge, _).

test(high_extraction_base) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cs_ecmo_bridge, ExtMetricName, E),
    E >= 0.46. % Ensures it's high-extraction Snare/Tangled.

:- end_tests(cs_ecmo_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ECMO bridge is a classic Tangled Rope. It has a clear, undeniable coordination function: it bridges a patient's biological failure to the availability of a transplant organ, coordinating complex medical resources to save a life. This function is why the hospital (institutional) sees it as a Rope. However, this coordination comes at a massive cost to the patient (powerless), who experiences extreme physical dependency, financial burden, and risk, making it feel like a Snare. The base extractiveness (0.55) reflects this high cost/risk factor, while the moderate suppression (0.30) reflects that while the patient is trapped in their condition, the technology itself is an option, not a mandate. The low theater ratio (0.10) confirms this is a highly functional, non-performative system, making a Piton classification impossible.
 *
 * The analytical perspective, which accounts for both the coordination and the asymmetric extraction, correctly identifies it as a Tangled Rope. This classification is critical because it acknowledges both the life-saving utility and the severe burdens imposed, preventing a simplistic "good" (Rope) or "bad" (Snare) judgment.
 *
 * [RESOLVED MANDATROPHY]
 * The Tangled Rope classification prevents the system from mislabeling the ECMO bridge as a pure Snare. A Snare-only classification would ignore its genuine, life-saving coordination function. By requiring the declaration of a beneficiary, a victim, and active enforcement, the Tangled Rope type correctly models systems that are simultaneously beneficial for one group and extractive for another, which is common in advanced medical technology, finance, and platform economies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cs_ecmo_bridge_1,
    'Does the long-term quality of life for survivors justify the immense short-term extraction (cost, risk, suffering)?',
    'Longitudinal studies tracking patient health, quality of life, and survival rates post-transplant against a control group.',
    'If outcomes are poor, the constraint shifts closer to a Snare. If outcomes are excellent, it reinforces the Tangled Rope classification (high extraction for high reward).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cs_ecmo_bridge, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% This models the technology's adoption and cost intensification over time.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (remains low as it's a functional system):
narrative_ontology:measurement(cs_ecmo_bridge_tr_t0, cs_ecmo_bridge, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cs_ecmo_bridge_tr_t5, cs_ecmo_bridge, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cs_ecmo_bridge_tr_t10, cs_ecmo_bridge, theater_ratio, 10, 0.10).

% Extraction over time (costs and dependency increase as technology becomes standard):
narrative_ontology:measurement(cs_ecmo_bridge_ex_t0, cs_ecmo_bridge, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cs_ecmo_bridge_ex_t5, cs_ecmo_bridge, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(cs_ecmo_bridge_ex_t10, cs_ecmo_bridge, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This technology is a mechanism for allocating scarce resources (medical
% staff, equipment, and the patient's viability) until another resource
% (a donor organ) becomes available.
narrative_ontology:coordination_type(cs_ecmo_bridge, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(cs_ecmo_bridge, 0.0).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(cs_ecmo_bridge, organ_donation_policy).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */