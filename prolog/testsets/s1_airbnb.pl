% ============================================================================
% CONSTRAINT STORY: airbnb_str_regulation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_airbnb_str_regulation, []).

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
 * * constraint_id: airbnb_str_regulation
 * human_readable: Short-Term Rental (STR) Regulatory Framework
 * domain: economic/political
 * * SUMMARY:
 * A global patchwork of municipal laws, zoning restrictions, and tax requirements that govern home-sharing. These regulations range from simple registration to "One Host, One Home" policies and outright bans, aiming to balance the "sharing economy" with housing affordability and neighborhood character. The framework acts as a filter determining who can legally participate in the market, creating winners and losers.
 * * KEY AGENTS:
 * - Individual Casual Hosts: Subject (Powerless)
 * - Local Municipalities & Hotel Incumbents: Beneficiary (Institutional)
 * - Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(airbnb_str_regulation, 0.60). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(airbnb_str_regulation, 0.80).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(airbnb_str_regulation, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(airbnb_str_regulation, extractiveness, 0.60).
narrative_ontology:constraint_metric(airbnb_str_regulation, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(airbnb_str_regulation, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(airbnb_str_regulation, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(airbnb_str_regulation). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(airbnb_str_regulation, local_government_treasuries).
narrative_ontology:constraint_beneficiary(airbnb_str_regulation, hotel_industry).
narrative_ontology:constraint_victim(airbnb_str_regulation, individual_casual_hosts).
narrative_ontology:constraint_victim(airbnb_str_regulation, professional_hosts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a casual host, the regulations are a trap. The high suppression (permit complexity,
% night caps) and extraction (fees, lost income) make participation costly and risky.
% χ = 0.60 * 1.5 (powerless) * 0.8 (local) = 0.72. This is high effective extraction.
constraint_indexing:constraint_classification(airbnb_str_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For a city council or hotel lobby, the framework is a necessary coordination tool (Rope)
% to manage housing stock, collect taxes, and ensure fair competition.
% χ = 0.60 * -0.2 (institutional) * 1.0 (national) = -0.12. The extraction is not felt.
constraint_indexing:constraint_classification(airbnb_str_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view reveals a Tangled Rope. It has a genuine coordination function
% (beneficiaries exist) but also imposes high, asymmetric extraction on a specific group
% (victims exist) and requires active enforcement. This captures the dual nature of the policy.
constraint_indexing:constraint_classification(airbnb_str_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airbnb_str_regulation_tests).

test(perspectival_gap) :-
    % Verify the gap between the host (powerless) and regulator (institutional).
    constraint_indexing:constraint_classification(airbnb_str_regulation, snare, context(agent_power(powerless), _, _, spatial_scope(local))),
    constraint_indexing:constraint_classification(airbnb_str_regulation, rope, context(agent_power(institutional), _, _, spatial_scope(national))).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(airbnb_str_regulation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(airbnb_str_regulation, extractiveness, E),
    E >= 0.46.

:- end_tests(airbnb_str_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint story models the regulatory environment for short-term rentals (STRs)
 * as a classic example of a Tangled Rope.
 * - Base Extractiveness (0.60): Represents significant value transfer via taxes, fees,
 *   and lost revenue for hosts, which benefits municipal budgets and incumbent hotels.
 * - Suppression (0.80): High due to complex registration, caps on rental nights, and
 *   zoning restrictions that actively limit participation.
 * - Perspectival Gap: The core conflict is captured by the perspectival gap. To a casual
 *   host with limited power and exit options, the rules are a Snare. To the institutional
 *   regulator, they are a Rope for managing urban space.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification for the analytical observer is critical. It prevents
 * the system from collapsing the constraint into either the regulator's 'pure coordination'
 * (Rope) narrative or the host's 'pure extraction' (Snare) experience. It correctly
 * identifies the dual nature of the system: it provides a genuine coordination function
 * (managing housing stock) while simultaneously creating asymmetric extraction that
 * benefits incumbents, requiring active state enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_airbnb_str_regulation,
    'Do STR restrictions actually lower long-term rents, or do they simply reallocate traveler revenue to hotels?',
    'Post-regulation rent tracking in synthetic control cities (e.g., NYC post-2023).',
    'If it lowers rents, the Rope aspect is stronger. If it only benefits hotels, the Snare aspect is stronger.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(airbnb_str_regulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models a gradual increase in
% regulatory complexity and capture over the interval.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (slight increase as bureaucracy solidifies):
narrative_ontology:measurement(airbnb_str_regulation_tr_t0, airbnb_str_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(airbnb_str_regulation_tr_t5, airbnb_str_regulation, theater_ratio, 5, 0.12).
narrative_ontology:measurement(airbnb_str_regulation_tr_t10, airbnb_str_regulation, theater_ratio, 10, 0.15).

% Extraction over time (increases as more fees and restrictions are added):
narrative_ontology:measurement(airbnb_str_regulation_ex_t0, airbnb_str_regulation, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(airbnb_str_regulation_ex_t5, airbnb_str_regulation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(airbnb_str_regulation_ex_t10, airbnb_str_regulation, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(airbnb_str_regulation, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */