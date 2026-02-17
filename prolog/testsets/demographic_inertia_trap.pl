% ============================================================================
% CONSTRAINT STORY: demographic_inertia_trap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_demographic_inertia_trap, []).

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
 * * constraint_id: demographic_inertia_trap
 * human_readable: The Generational Wealth Siphon
 * domain: social/economic
 * * SUMMARY:
 * A scenario where a massive, aging demographic majority maintains political
 * control to enforce economic transfers (pensions, healthcare, zoning) from
 * a shrinking youth minority. Because the demographic shift is a "slow-motion
 * collision," it feels like an immutable Mountain to the young, while serving
 * as a critical coordination Rope for the stability of the elderly. The system
 * classifies it as a Tangled Rope, recognizing both functions.
 * * KEY AGENTS:
 * - Young Professional: Subject (Powerless)
 * - Pensioner Bloc: Beneficiary (Institutional)
 * - Actuarial Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as it liquidates the lifetime optionality of the
% youth to service the maintenance costs of the legacy demographic.
domain_priors:base_extractiveness(demographic_inertia_trap, 0.78).
domain_priors:suppression_score(demographic_inertia_trap, 0.65).
domain_priors:theater_ratio(demographic_inertia_trap, 0.40).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(demographic_inertia_trap, extractiveness, 0.78).
narrative_ontology:constraint_metric(demographic_inertia_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(demographic_inertia_trap, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a social contract for elder care.
narrative_ontology:constraint_claim(demographic_inertia_trap, tangled_rope).
narrative_ontology:human_readable(demographic_inertia_trap, "The Generational Wealth Siphon").

% Binary flags
% This system of transfers requires tax collection and pension distribution,
% which are actively enforced. This is a required property for Tangled Rope.
domain_priors:requires_active_enforcement(demographic_inertia_trap).

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, pensioner_bloc).
narrative_ontology:constraint_victim(demographic_inertia_trap, youth_minority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To a young individual, the demographic pyramid feels like an unchangeable law of
% nature (Mountain), but the system classifies the high, coercive extraction as a Snare.
% χ = 0.78 (ε) * 1.5 (π(powerless)) * 1.0 (σ(national)) = 1.17.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The elderly bloc views the trap as a Rope—it is the only way to coordinate
% societal resources to prevent a collapse of the elder-care social contract.
% χ = 0.78 (ε) * -0.2 (π(institutional)) * 1.0 (σ(national)) = -0.156.
constraint_indexing:constraint_classification(demographic_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the present coordination (Rope) inextricably tied to predatory extraction (Snare).
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_inertia_trap_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(demographic_inertia_trap, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(demographic_inertia_trap, ExtMetricName, E),
    E >= 0.46. % Ensures high-extraction triggers Omega/Mandatrophy rules.

:- end_tests(demographic_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) reflects the 'Mandatrophy' threshold where
 * the social contract has shifted from mutual support to generational extraction.
 * The suppression score (0.65) reflects the high political and social barriers
 * to reforming the system.
 *
 * PERSPECTIVAL GAP:
 * The Young Professional feels a Snare. The effective extraction is over 1.0,
 * representing a system that consumes more future value than they can generate.
 * While it feels immutable like a Mountain due to its scale, its constructed and
 * enforced nature makes it a Snare. The Pensioner Bloc sees a Rope because their
 * livelihood depends on the institutionalized coordination of wealth transfers,
 * and from their perspective, the extraction is negative (a net benefit).
 *
 * [RESOLVED MANDATROPHY]:
 * Resolved via Tangled Rope classification from the analytical perspective. This
 * recognizes the coordination intent (preventing elderly poverty) while explicitly
 * flagging the 0.78 base extraction from the youth minority as a systemic
 * failure point, preventing misclassification as a pure Snare or pure Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_demographic_inertia_trap,
    'Can AI-driven productivity growth outpace the dependency ratio, neutralizing the extraction?',
    'Tracking the delta between real GDP per worker and the elder-support tax burden over a decade.',
    'If growth > burden, the constraint may soften towards a Rope. If burden > growth, it hardens into a terminal Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(demographic_inertia_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as the demographic imbalance grew.
% Initially, it was a sustainable social contract, but extraction accumulated
% as the dependency ratio worsened.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(dit_tr_t0, demographic_inertia_trap, theater_ratio, 0, 0.10).
narrative_ontology:measurement(dit_tr_t5, demographic_inertia_trap, theater_ratio, 5, 0.25).
narrative_ontology:measurement(dit_tr_t10, demographic_inertia_trap, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(dit_ex_t0, demographic_inertia_trap, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dit_ex_t5, demographic_inertia_trap, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(dit_ex_t10, demographic_inertia_trap, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system is fundamentally a mechanism for allocating resources (taxes, benefits).
narrative_ontology:coordination_type(demographic_inertia_trap, resource_allocation).

% Network relationships (structural influence edges)
% This form of capital extraction from the young directly impacts their ability
% to participate in the housing market, reinforcing its rigidity.
narrative_ontology:affects_constraint(demographic_inertia_trap, housing_market_rigidity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */