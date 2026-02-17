% ============================================================================
% CONSTRAINT STORY: codex_access
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_codex_access, []).

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
 * * constraint_id: codex_access
 * human_readable: OpenAI Codex Access Control
 * domain: technological/economic
 * * SUMMARY:
 * OpenAI controls access to its Codex models via an API or dedicated application. This creates a constraint by governing access to a powerful tool, fostering dependencies and limiting alternatives for some developers while providing significant productivity benefits to others. The platform owner extracts value through usage fees and data collection.
 * * KEY AGENTS:
 * - Novice Developer: Subject (Powerless)
 * - OpenAI: Beneficiary (Institutional)
 * - Independent Researcher: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(codex_access, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(codex_access, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(codex_access, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(codex_access, extractiveness, 0.55).
narrative_ontology:constraint_metric(codex_access, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(codex_access, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(codex_access, tangled_rope).
narrative_ontology:human_readable(codex_access, "OpenAI Codex Access Control").

% Binary flags
% narrative_ontology:has_sunset_clause(codex_access).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(codex_access). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(codex_access, platform_users).
narrative_ontology:constraint_victim(codex_access, dependent_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as a predatory trap due to dependency and high switching costs.
constraint_indexing:constraint_classification(codex_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination for accessing advanced AI.
constraint_indexing:constraint_classification(codex_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED_ROPE)
% Default analytical context (civilizational/analytical/global).
% Recognizes both the coordination function and the asymmetric extraction.
constraint_indexing:constraint_classification(codex_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(codex_access_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(codex_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(codex_access, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation_high_extraction) :-
    % This is a high-extraction constraint, so E must be >= 0.46
    narrative_ontology:constraint_metric(codex_access, extractiveness, E),
    E >= 0.46.

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer sees a tangled_rope
    constraint_indexing:constraint_classification(codex_access, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(codex_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint is a canonical Tangled Rope. The base extractiveness (0.55) and suppression (0.70) are high, reflecting the platform's market power and the difficulty for developers to switch to a comparable alternative.
 *
 * PERSPECTIVAL GAP:
 * - A 'powerless' developer, trapped by dependency on the API for their product, perceives the constraint as a Snare. The effective extraction is high (χ = 0.55 * 1.5 * 1.2 = 0.99), feeling almost total.
 * - The 'institutional' actor (OpenAI) views it as a Rope, a necessary mechanism to coordinate access, manage load, and fund further research. For them, effective extraction is negative (χ = 0.55 * -0.2 * 1.2 = -0.132), meaning they see it as a net investment in the ecosystem.
 * - The 'analytical' observer identifies both the genuine coordination function (beneficiaries exist) and the asymmetric extraction (victims exist), alongside the need for active enforcement (API keys, rate limits). This combination is the definition of a Tangled Rope.
 *
 * The Piton classification was removed as it is not applicable; the theater_ratio is low (0.30), indicating the system is highly functional and not merely performative.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification correctly captures the dual nature of the platform. A simpler Snare classification would ignore the genuine value and coordination it provides to many users, while a Rope classification would ignore the extractive power dynamics and high switching costs imposed on dependent developers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_codex_access,
    'Will truly comparable, open-source alternatives to proprietary coding models achieve critical mass and infrastructure parity?',
    'Track the development, funding, and adoption rates of leading open-source code generation models and their hosting platforms.',
    'If True, suppression score would decrease, potentially reclassifying the constraint to a Rope. If False, the Snare/Tangled Rope nature is reinforced.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(codex_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% This model shows a system that launched with high utility and gradually
% increased its extractive potential as dependency grew.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(codex_access_tr_t0, codex_access, theater_ratio, 0, 0.10).
narrative_ontology:measurement(codex_access_tr_t5, codex_access, theater_ratio, 5, 0.20).
narrative_ontology:measurement(codex_access_tr_t10, codex_access, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(codex_access_ex_t0, codex_access, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(codex_access_ex_t5, codex_access, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(codex_access_ex_t10, codex_access, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(codex_access, resource_allocation).

% Network relationships (structural influence edges)
% This constraint creates dependencies that affect other parts of the ecosystem.
narrative_ontology:affects_constraint(codex_access, developer_tooling_dependency).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */