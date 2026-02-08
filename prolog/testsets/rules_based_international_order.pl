% ============================================================================
% CONSTRAINT STORY: rules_based_international_order
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_rules_based_international_order, []).

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
 * * constraint_id: rules_based_international_order
 * human_readable: The Rules-Based International Order
 * domain: political/economic
 * * SUMMARY:
 * The international system of institutions (WTO, UN, NATO) and norms that 
 * historically provided predictability and coordination. In the modern era, 
 * this system is increasingly used by great powers as an instrument of 
 * coercion and subordination against middle powers, transforming what was 
 * claimed to be a public good into a mechanism for asymmetric extraction.
 * * KEY AGENTS:
 * - Middle Powers: Subject (Powerless/Moderate)
 * - Great Powers: Beneficiary (Institutional)
 * - Systems Theorist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(rules_based_international_order, 0.75). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(rules_based_international_order, 0.60).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(rules_based_international_order, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(rules_based_international_order, extractiveness, 0.75).
narrative_ontology:constraint_metric(rules_based_international_order, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(rules_based_international_order, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(rules_based_international_order, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(rules_based_international_order). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(rules_based_international_order, great_powers).
narrative_ontology:constraint_victim(rules_based_international_order, middle_powers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a middle power, the order is a trap. Integration becomes a source of
% subordination, where supply chains and tariffs are used as weapons.
% χ = 0.75 (ε) * 1.0 (π(moderate)) * 1.2 (σ(global)) = 0.9. This is a clear Snare.
constraint_indexing:constraint_classification(rules_based_international_order, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For a great power, the rules are a functional coordination mechanism to be
% used or discarded strategically. The extractive cost is externalized.
% χ = 0.75 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.18. This is a Rope.
constraint_indexing:constraint_classification(rules_based_international_order, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the genuine coordination function (beneficiary exists)
% and the asymmetric extraction (victim exists), maintained by active enforcement.
% This is the canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(rules_based_international_order, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PASSIVE PARTICIPANT (SNARE)
% A powerless state or entity participating in the "ritual" of compliance to
% avoid trouble experiences the system as an inescapable, coercive trap.
% χ = 0.75 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 1.125. A very strong Snare.
constraint_indexing:constraint_classification(rules_based_international_order, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rules_based_international_order_tests).

test(perspectival_gap_beneficiary_vs_victim) :-
    % Verify the gap between the beneficiary (Great Power) and victim (Middle Power).
    constraint_indexing:constraint_classification(rules_based_international_order, TypeVictim, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(rules_based_international_order, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeVictim == snare),
    assertion(TypeBeneficiary == rope),
    TypeVictim \= TypeBeneficiary.

test(analytical_observer_sees_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(rules_based_international_order, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_requirements_met) :-
    % A Tangled Rope requires beneficiaries, victims, and active enforcement.
    narrative_ontology:constraint_beneficiary(rules_based_international_order, _),
    narrative_ontology:constraint_victim(rules_based_international_order, _),
    domain_priors:requires_active_enforcement(rules_based_international_order).

:- end_tests(rules_based_international_order_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.75) and suppression (0.60) are high, reflecting
 * the modern use of the international system for coercion (e.g., weaponized
 * tariffs, supply chain control). The theater ratio is low (0.15) because the
 * system is still highly functional, not merely performative.
 *
 * The Perspectival Gap is stark:
 * - For Great Powers (institutional), the system is a 'Rope'. They created it,
 *   can opt out (arbitrage), and externalize the costs. Effective extraction is negative.
 * - For Middle Powers (moderate/powerless), the system is a 'Snare'. They are
 *   trapped by integration, making them vulnerable to coercion. Effective
 *   extraction is amplified to extreme levels.
 * - The Analytical observer, seeing both the coordination and the extraction,
 *   classifies it as a 'Tangled Rope', the correct structural designation.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction is resolved by the Tangled Rope classification. The system
 * avoids mislabeling the constraint as a pure Snare by acknowledging its
 * genuine (though asymmetric) coordination function, which is precisely what
 * the 'Rope' perspective of the beneficiary captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_rbio_intent,
    "Is the high extraction (0.75) an emergent property of great power competition, or a deliberate, designed feature to maintain hegemony?",
    "Declassification of diplomatic cables from the 1945-1960 period showing design intent vs. analysis of modern-day policy papers showing reactive coercion.",
    "If emergent: Tangled Rope (unintended consequence). If designed: Snare (predatory intent from inception).",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rules_based_international_order, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the system's degradation from a more coordinative
% mechanism into a highly extractive one. This triggers extraction_accumulation
% drift detection.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(rbio_tr_t0, rules_based_international_order, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rbio_tr_t5, rules_based_international_order, theater_ratio, 5, 0.12).
narrative_ontology:measurement(rbio_tr_t10, rules_based_international_order, theater_ratio, 10, 0.15).

% Extraction over time (significant increase):
narrative_ontology:measurement(rbio_ex_t0, rules_based_international_order, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rbio_ex_t5, rules_based_international_order, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rbio_ex_t10, rules_based_international_order, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system is a form of global infrastructure for diplomacy and trade.
narrative_ontology:coordination_type(rules_based_international_order, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */