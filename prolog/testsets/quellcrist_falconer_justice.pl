% ============================================================================
% CONSTRAINT STORY: quellcrist_falconer_justice
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_quellcrist_falconer_justice, []).

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
 * * constraint_id: quellcrist_falconer_justice
 * human_readable: The Machinery of Justice (Quellist)
 * domain: political
 * * SUMMARY:
 * This constraint represents the "Machinery of Justice" as described by Quellcrist Falconer in Richard K. Morgan's "Altered Carbon": a cold, institutional apparatus that serves the "players" (creatures of power) while systematically liquidating, displacing, and torturing the "little people". It functions by reframing systemic harm as "just business" or "politics" to suppress personal accountability.
 * * KEY AGENTS:
 * - The Little People: Powerless subjects who suffer "torture and brutal execution".
 * - The Power Player: Institutional actors who slide under Justice with a "wink and a grin".
 * - The Insurrectionary (Falconer): An analytical observer who advocates for "clawing" justice through personal, dangerous action.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(quellcrist_falconer_justice, 0.90). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(quellcrist_falconer_justice, 0.80).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(quellcrist_falconer_justice, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(quellcrist_falconer_justice, extractiveness, 0.90).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(quellcrist_falconer_justice, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(quellcrist_falconer_justice, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(quellcrist_falconer_justice). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(quellcrist_falconer_justice, power_players).
narrative_ontology:constraint_victim(quellcrist_falconer_justice, little_people).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the "little people", the system is a pure extraction machine.
% χ = 0.90 (ε) * 1.5 (π(powerless)) * 0.8 (σ(local)) = 1.08. This is a definitive Snare.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the "power players", the system is a coordination tool that benefits them.
% χ = 0.90 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.216. Negative extraction indicates a net benefit.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view must account for both the coordination function and the asymmetric extraction.
% The system has beneficiaries, victims, high extraction, and requires active enforcement. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quellcrist_falconer_justice_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(quellcrist_falconer_justice, tangled_rope, context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(quellcrist_falconer_justice, E),
    domain_priors:suppression_score(quellcrist_falconer_justice, S),
    assertion(E >= 0.46),
    assertion(S >= 0.60).

:- end_tests(quellcrist_falconer_justice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect Falconer's description of a system that performs "torture and brutal execution" (extractiveness: 0.9) and reframes this as "just business" to prevent recourse (suppression: 0.8). The system is highly functional, not theatrical, hence the low theater_ratio.
 *
 * The perspectival gap is extreme: for the institutional "players", it is a Rope that provides order and removes obstacles (negative effective extraction). For the powerless "little people", it is a lethal Snare (extremely high effective extraction).
 *
 * The analytical perspective must be Tangled Rope. The original file's classification of 'Mountain' was inconsistent with the high extraction, suppression, and active enforcement metrics. A Mountain is a passive, low-extraction limit. This system is an active, predatory machine. The Tangled Rope classification correctly captures its dual nature as a coordination mechanism for one group funded by the violent extraction from another.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic example of resolved Mandatrophy. The system's claim to be "Justice" (a coordination mechanism) is a facade for its primary function of extraction. The Tangled Rope classification prevents the system from being misread as either a pure Snare (ignoring its utility to the powerful) or a pure Rope (ignoring its victims). It correctly identifies that the coordination and extraction are inextricably linked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_quellcrist_falconer_justice_1,
    "At what threshold of 'dangerous' behavior does a 'little person' effectively transform into a 'player' in the eyes of the machine?",
    "Audit of deal-making patterns vs. casualty rates in personal/insurrectionary actions.",
    "If low: Insurrection is a Rope. If high: Insurrection is a suicide-Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_quellcrist_falconer_justice_2,
    "Is the cold, slow nature of the machine a functional necessity for galactic governance or a predatory choice for efficient liquidation?",
    "Comparison of 'Hardware/Software' allocation for deal-making vs. execution.",
    "If necessity: Mountain-like properties. If predatory choice: Pure Snare/Tangled Rope.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(quellcrist_falconer_justice, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This model shows the "Machinery of Justice"
% becoming more brutally efficient over time, shedding its performative aspects
% (theater) and maximizing its extractive function.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(qfj_tr_t0, quellcrist_falconer_justice, theater_ratio, 0, 0.40).
narrative_ontology:measurement(qfj_tr_t5, quellcrist_falconer_justice, theater_ratio, 5, 0.20).
narrative_ontology:measurement(qfj_tr_t10, quellcrist_falconer_justice, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(qfj_ex_t0, quellcrist_falconer_justice, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(qfj_ex_t5, quellcrist_falconer_justice, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(qfj_ex_t10, quellcrist_falconer_justice, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The "Machinery of Justice" is, by its own claim, a system for enforcing rules.
narrative_ontology:coordination_type(quellcrist_falconer_justice, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */