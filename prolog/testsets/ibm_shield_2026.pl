% ============================================================================
% CONSTRAINT STORY: ibm_shield_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ibm_shield_2026, []).

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
 * * constraint_id: ibm_shield_2026
 * human_readable: IBM SHIELD Program (MDA Contract)
 * domain: political/technological
 * * SUMMARY:
 * IBM's $151B SHIELD contract with the Missile Defense Agency (MDA) operationalizes
 * AI-enabled sensing for national defense. While providing a critical coordination
 * function (protecting the nation), the IDIQ contract structure and mission-grade
 * security create a "Snare" of technological and budgetary opacity for the public,
 * leading to a Tangled Rope classification from an analytical perspective.
 * * KEY AGENTS:
 * - General Public: Subject (Powerless)
 * - IBM / MDA: Beneficiary (Institutional)
 * - Defense Auditors: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ibm_shield_2026, 0.62). % High due to the massive capital allocation ($151B) and proprietary AI governance.
domain_priors:suppression_score(ibm_shield_2026, 0.75).   % High as "mission-grade security" limits public oversight and alternative vendors.
domain_priors:theater_ratio(ibm_shield_2026, 0.20).       % Low because the contract focuses on rapid, functional delivery to the warfighter.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ibm_shield_2026, extractiveness, 0.62).
narrative_ontology:constraint_metric(ibm_shield_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ibm_shield_2026, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be an enforcement mechanism for national security.
narrative_ontology:constraint_claim(ibm_shield_2026, tangled_rope).
narrative_ontology:human_readable(ibm_shield_2026, "IBM SHIELD Program (MDA Contract)").
narrative_ontology:topic_domain(ibm_shield_2026, "political/technological").

% Binary flags
domain_priors:requires_active_enforcement(ibm_shield_2026). % Required for Tangled Rope. The contract is enforced by federal law and security protocols.

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(ibm_shield_2026, national_security_apparatus).
narrative_ontology:constraint_victim(ibm_shield_2026, public_budgetary_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The public sees the SHIELD program as a Snare: a massive, opaque financial
% commitment they cannot exit or influence.
% χ = 0.62 * 1.5 (powerless) * 1.0 (national) = 0.93
constraint_indexing:constraint_classification(ibm_shield_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% MDA and IBM view this as a Rope—essential coordination of sensing and AI
% to ensure national survival, with low perceived extraction.
% χ = 0.62 * -0.2 (institutional) * 1.0 (national) = -0.124 (felt as a benefit)
constraint_indexing:constraint_classification(ibm_shield_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analysts see a Tangled Rope: Genuine existential security coordination
% mixed with high rent-extraction from the IDIQ structure.
% χ = 0.62 * 1.15 (analytical) * 1.2 (global) = 0.8556
constraint_indexing:constraint_classification(ibm_shield_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ibm_shield_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ibm_shield_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ibm_shield_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(tangled_rope_conditions_met) :-
    % Verify that the conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(ibm_shield_2026),
    narrative_ontology:constraint_beneficiary(ibm_shield_2026, _), % Derives has_coordination_function
    narrative_ontology:constraint_victim(ibm_shield_2026, _).     % Derives has_asymmetric_extraction

test(high_extraction_threshold) :-
    narrative_ontology:constraint_metric(ibm_shield_2026, extractiveness, E),
    E >= 0.46.

:- end_tests(ibm_shield_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness of 0.62 reflects the enormous $151B budget, a significant
 * diversion of public funds into a private entity's proprietary systems. The high
 * suppression score of 0.75 is due to the "mission-grade security" classification,
 * which effectively prevents public oversight, audits, and competition, locking the
 * government into a single vendor.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' public, the effective extraction is amplified (χ=0.93),
 *   making it a clear Snare. They bear the cost without transparency or control.
 * - For the 'institutional' beneficiary (MDA/IBM), the extraction is inverted
 *   (χ=-0.124), perceived as a resource or benefit. It's a Rope that coordinates
 *   national defense, a critical function.
 * - The 'analytical' observer sees both sides. The system provides real coordination
 *   (beneficiary exists) but also has asymmetric extraction (victim exists) and
 *   requires active enforcement. This combination is the textbook definition of a
 *   Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 * This constraint is a classic case where Mandatrophy could occur if the system
 * only looked at the victim's perspective (pure Snare) or the beneficiary's
 * (pure Rope). The Tangled Rope classification is the resolution mechanism. It
 * correctly identifies that a valid, even essential, coordination function
 * (national defense) is coupled with a highly extractive, suppressive implementation.
 * This prevents the system from dismissing the constraint's function while still
 * flagging its severe extractive properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_shield_2026,
    'Will "mission-grade security" prevent effective civilian AI governance, or is it a necessary shield for national security secrets?',
    'Declassification and review of built-in AI governance protocols by a trusted, independent non-defense agency (e.g., NIST).',
    'If protocols are robust and auditable, it leans towards a complex Rope. If they are opaque and self-serving, it confirms a permanent Snare for AI ethics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ibm_shield_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the constraint's evolution from initial award (T=0)
% to full operationalization (T=10). This is a high-extraction constraint,
% so lifecycle data is required.

% Theater ratio over time (remains low and stable, indicating functionality):
narrative_ontology:measurement(ibm_shield_2026_tr_t0, ibm_shield_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ibm_shield_2026_tr_t5, ibm_shield_2026, theater_ratio, 5, 0.18).
narrative_ontology:measurement(ibm_shield_2026_tr_t10, ibm_shield_2026, theater_ratio, 10, 0.20).

% Extraction over time (grows as the program scales and locks in its position):
narrative_ontology:measurement(ibm_shield_2026_ex_t0, ibm_shield_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(ibm_shield_2026_ex_t5, ibm_shield_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(ibm_shield_2026_ex_t10, ibm_shield_2026, base_extractiveness, 10, 0.62).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% As a defense system, it functions as an enforcement mechanism.
narrative_ontology:coordination_type(ibm_shield_2026, enforcement_mechanism).

% Network relationships (structural influence edges)
% This massive defense AI contract directly influences the development and
% regulation of AI ethics within the defense sector.
narrative_ontology:affects_constraint(ibm_shield_2026, ai_ethics_in_defense).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */