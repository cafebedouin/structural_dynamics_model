% ============================================================================
% CONSTRAINT STORY: openclaw_regulation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_openclaw_regulation, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: openclaw_regulation
 * human_readable: Regulation of Autonomous AI Assistant OpenClaw
 * domain: technological
 * * SUMMARY:
 * The emergence of OpenClaw, a viral AI assistant with autonomous capabilities, necessitates regulatory frameworks to address potential misuse and security risks. These regulations aim to balance innovation with safeguards against unintended consequences and malicious exploitation.
 * * KEY AGENTS:
 * - Users: Subject (Powerless)
 * - Regulators/Developers: Beneficiary (Institutional)
 * - Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(openclaw_regulation, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(openclaw_regulation, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(openclaw_regulation, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(openclaw_regulation, extractiveness, 0.55).
narrative_ontology:constraint_metric(openclaw_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(openclaw_regulation, theater_ratio, 0.30).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(openclaw_regulation, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(openclaw_regulation).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(openclaw_regulation). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(openclaw_regulation, developers_regulators).
narrative_ontology:constraint_victim(openclaw_regulation, sandboxed_ai_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as an immutable limit or predatory trap.
% χ = 0.55 * 1.5 (powerless) * 1.0 (national) = 0.825. This is a clear Snare.
constraint_indexing:constraint_classification(openclaw_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
% χ = 0.55 * -0.2 (institutional) * 1.0 (national) = -0.11. Negative extraction is felt as a pure benefit. Rope.
constraint_indexing:constraint_classification(openclaw_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED_ROPE)
% Default analytical context (civilizational/analytical/global).
% χ = 0.55 * 1.15 (analytical) * 1.2 (global) = 0.759. High extraction.
% The system has beneficiaries, victims, and requires enforcement, meeting all criteria for a Tangled Rope.
constraint_indexing:constraint_classification(openclaw_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openclaw_regulation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(openclaw_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openclaw_regulation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(openclaw_regulation, ExtMetricName, E),
    (E =< 0.15 -> false ; E >= 0.46). % Ensures it's a high-extraction Snare/Tangled, not a Rope or Mountain.

:- end_tests(openclaw_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The OpenClaw regulation is a canonical Tangled Rope. It has a genuine coordination function (benefiting developers and regulators by creating a stable, predictable market and mitigating systemic risk) while simultaneously imposing high costs and restrictions on end-users (asymmetric extraction). The high suppression score (0.70) reflects the mandatory nature of the regulations, limiting user choice and requiring active enforcement.
 * The perspectival gap is stark: users (powerless, trapped) experience the system as a Snare, feeling its full extractive force (χ=0.825). In contrast, institutional beneficiaries perceive it as a Rope, as the negative power modifier makes the extraction feel like a net benefit (χ=-0.11). The analytical observer, accounting for all factors, correctly identifies the hybrid nature of the constraint as a Tangled Rope.
 * The constraint claims to be 'coordination', which is true from the beneficiary's perspective, but this claim masks the high extraction felt by victims, a typical feature of Tangled Ropes.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the regulation as pure extraction (Snare) by recognizing the coordination function provided to developers and regulators in maintaining the AI system and setting standards. This distinguishes it from purely exploitative systems where no genuine coordination benefit exists. This resolution is critical for policy analysis, as it acknowledges both the stated benefits and the hidden costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_openclaw_regulation,
    'Will the benefits of regulating OpenClaw outweigh the potential stifling of innovation?',
    'Longitudinal studies tracking the impact of regulation on AI development and adoption rates.',
    'If True: Sustainable AI ecosystem with user protection. If False: Stifled innovation and loss of competitive advantage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openclaw_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(openclaw_regulation_tr_t0, openclaw_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(openclaw_regulation_tr_t5, openclaw_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(openclaw_regulation_tr_t10, openclaw_regulation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(openclaw_regulation_ex_t0, openclaw_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(openclaw_regulation_ex_t5, openclaw_regulation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(openclaw_regulation_ex_t10, openclaw_regulation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(openclaw_regulation, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(openclaw_regulation, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(openclaw_regulation, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */