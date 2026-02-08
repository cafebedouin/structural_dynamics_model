% ============================================================================
% CONSTRAINT STORY: factional_instability
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_factional_instability, []).

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
 * * constraint_id: factional_instability
 * human_readable: The Republican Remedy for Factional Violence
 * domain: political
 * * SUMMARY:
 * Based on Federalist Paper No. 10, this constraint models the system of a
 * large representative republic designed to control the "violence of faction."
 * James Madison argues that since the causes of faction (liberty and human
 * nature) are immutable, the effects must be controlled. The constraint is
 * the constitutional structure itself, which filters popular will to protect
 * minority rights (especially property) and prevent a majority from imposing
 * its "ruling passion" on the community.
 * * KEY AGENTS:
 * - Debtor Class (Majority Faction): The subject, whose desire for policies like
 *   debt abolition is suppressed by the system.
 * - Creditor/Property-Owning Class (Minority Faction): The beneficiary, whose
 *   assets are protected from the majority will.
 * - The Constitutional Architect (Madison): The analytical observer, viewing the
 *   system as a necessary mechanism to balance liberty and order.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(factional_instability, 0.70). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(factional_instability, 0.50).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(factional_instability, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(factional_instability, extractiveness, 0.70).
narrative_ontology:constraint_metric(factional_instability, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(factional_instability, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(factional_instability, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(factional_instability). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(factional_instability, property_owners).
narrative_ontology:constraint_victim(factional_instability, debtor_class).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The debtor class, whose political power is curtailed by the system.
% They feel the full extractive force (χ = 0.7 * 1.5 * 1.0 = 1.05), viewing
% the constitutional structure as a trap preventing economic relief.
constraint_indexing:constraint_classification(factional_instability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The property-owning class, who see the system as essential coordination.
% Their institutional power insulates them from extraction (χ = 0.7 * -0.2 * 1.0 = -0.14),
% making the system appear as a pure public good that secures order and property.
constraint_indexing:constraint_classification(factional_instability, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The constitutional architect (Madison) or a modern political scientist.
% This view recognizes the system's dual nature: it provides genuine coordination
% (preventing anarchy) but does so via asymmetric extraction (suppressing the
% majority will to protect a minority's property).
constraint_indexing:constraint_classification(factional_instability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(factional_instability_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(factional_instability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(factional_instability, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(factional_instability, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_conditions_met) :-
    % Verify the structural properties required for a Tangled Rope classification are present.
    narrative_ontology:constraint_beneficiary(factional_instability, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(factional_instability, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(factional_instability).

:- end_tests(factional_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of Federalist 10 is the tension between creating a system for the
 * "public good" and protecting the rights of a specific minority (property
 * owners). This is a canonical Tangled Rope.
 * - Base Extractiveness (0.70): High, as the system's primary function is to
 *   prevent one group (the majority debtor class) from using political power
 *   to extract resources from another (the minority creditor class).
 * - Suppression (0.50): Moderate. The system doesn't outlaw factions but
 *   "filters" and dilutes their power through representation and scale,
 *   suppressing their ability to act.
 * - Perspectival Gap: The gap is profound. For the debtor class, the system is a
 *   Snare, an oppressive structure preventing just economic outcomes. For the
 *   creditor class, it is a Rope, a vital coordination mechanism preventing
 *   the chaos of pure democracy.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A simpler analysis might classify this system as a Snare (it protects the
 * rich) or a Rope (it creates stability). The Tangled Rope classification is
 * crucial because it acknowledges both truths simultaneously. It prevents
 * mandatrophy by recognizing that the system's coordination function (stability)
 * is structurally inseparable from its asymmetric extraction function
 * (protecting property). The system is neither a pure public good nor a pure
 * mechanism of oppression; it is a functional, durable hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_factional_instability,
    'Can "enlightened statesmen" be relied upon to adjust clashing interests, or will they inevitably be captured by faction?',
    'Historical analysis of legislative outcomes vs. public vs. special interests over centuries.',
    'If statesmen are reliable, the system functions closer to a Rope. If they are captured, it degrades into a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(factional_instability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Here, we model the constitutional
% system becoming slightly more extractive over time as its mechanisms become
% more efficient at protecting established interests, while its purpose remains
% functional (low theater).
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(factional_instability_tr_t0, factional_instability, theater_ratio, 0, 0.05).
narrative_ontology:measurement(factional_instability_tr_t5, factional_instability, theater_ratio, 5, 0.08).
narrative_ontology:measurement(factional_instability_tr_t10, factional_instability, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(factional_instability_ex_t0, factional_instability, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(factional_instability_ex_t5, factional_instability, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(factional_instability_ex_t10, factional_instability, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The Republican system is a mechanism for enforcing a particular social order.
narrative_ontology:coordination_type(factional_instability, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */