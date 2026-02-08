% ============================================================================
% CONSTRAINT STORY: rent_seeking_equilibrium
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_rent_seeking_equilibrium, []).

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
 * * constraint_id: rent_seeking_equilibrium
 * human_readable: The Toll-Bridge Stagnation
 * domain: economic/political
 * * SUMMARY:
 * A scenario where economic agents invest more resources in capturing
 * existing wealth through political or legal influence (rent-seeking) than
 * in creating new value. This "Rope" of institutional stability coordinates
 * elite interests but acts as a "Snare" for the productive economy,
 * liquidating innovation and labor surplus into static barriers to entry.
 * * KEY AGENTS:
 * - Aspiring Entrepreneur: Subject (Powerless), Victim
 * - Incumbent Lobbyist: Beneficiary (Institutional)
 * - Institutional Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) because the equilibrium siphons the surplus of
% productive labor into "rent" payments for access to artificially
% restricted markets.
domain_priors:base_extractiveness(rent_seeking_equilibrium, 0.84).
domain_priors:suppression_score(rent_seeking_equilibrium, 0.76). % High suppression of non-rent-seeking alternatives.
domain_priors:theater_ratio(rent_seeking_equilibrium, 0.88).    % Piton threshold (> 0.70) triggered by "regulatory compliance" branding.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, 0.84).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(rent_seeking_equilibrium, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination mechanism for market stability.
narrative_ontology:constraint_claim(rent_seeking_equilibrium, tangled_rope).

% Binary flags
% Rent-seeking regulations require state power to enforce barriers to entry.
domain_priors:requires_active_enforcement(rent_seeking_equilibrium).

% Structural property derivation hooks:
% These are required for the Tangled Rope classification.
narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, incumbent_lobbyists).
narrative_ontology:constraint_victim(rent_seeking_equilibrium, aspiring_entrepreneurs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the entrepreneur, the equilibrium is a snare: the "cost of entry" is
% a political fee rather than a technical challenge, trapping them in
% low-margin compliance.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The incumbent views the regulation as a Rope—the essential coordination
% substrate that prevents "market chaos" and ensures long-term
% institutional stability.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The default analytical view detects both a coordination function (for
% incumbents) and asymmetric extraction (from entrants), requiring active
% enforcement. This is the canonical Tangled Rope signature.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A different analytical view, focused on functional utility, sees the high
% theater ratio (0.88 > 0.70) and classifies it as a Piton: the "market safety"
% regulations are an inert spike of logic masking pure wealth transfer.
constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rent_seeking_equilibrium_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional incumbent.
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    domain_priors:theater_ratio(rent_seeking_equilibrium, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(rent_seeking_equilibrium, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure high extraction (0.84) is correctly registered.
    narrative_ontology:constraint_metric(rent_seeking_equilibrium, extractiveness, E),
    E >= 0.46.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(rent_seeking_equilibrium, _),
    narrative_ontology:constraint_victim(rent_seeking_equilibrium, _),
    domain_priors:requires_active_enforcement(rent_seeking_equilibrium).

:- end_tests(rent_seeking_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the
 * "coordination" benefit of market regulation is effectively consumed
 * by the parasitic liquidation of the subject's competitive agency. The
 * high theater ratio (0.88) indicates that the performative aspects of
 * compliance and "safety" have almost entirely replaced any original
 * function, justifying the Piton classification.
 *
 * * PERSPECTIVAL GAP:
 * The Aspiring Entrepreneur feels a Snare because their success depends
 * on political connections rather than merit. The Lobbyist sees a Rope
 * because the barriers to entry prevent the "friction" of price wars
 * and coordinate stable, high-margin industry behavior. The Analytical
 * Observer sees a Tangled Rope, recognizing both the coordination and
 * the asymmetric extraction it enables.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The Tangled Rope classification is critical here.
 * It prevents the system from simplifying this to a pure Snare, which would
 * miss the coordination function that gives the constraint its stability and
 * institutional support. By declaring beneficiaries, victims, and the need
 * for enforcement, the model correctly identifies a hybrid constraint where
 * extraction is laundered through a coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_rent_seeking_equilibrium,
    'Can the system distinguish "rent-seeking" from "essential safety" (Snare vs Mountain)?',
    'Tracking the delta between compliance costs and actual safety outcomes over a 20-year horizon.',
    'If delta is high: Snare of policy. If delta is zero: Mountain of Irreducible Risk.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rent_seeking_equilibrium, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint models a system that began with a legitimate coordination
% function but degraded over time into pure extraction and theater.
% The extraction accumulated, and the theater ratio rose as the original
% function atrophied.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(rse_tr_t0, rent_seeking_equilibrium, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rse_tr_t5, rent_seeking_equilibrium, theater_ratio, 5, 0.55).
narrative_ontology:measurement(rse_tr_t10, rent_seeking_equilibrium, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rse_ex_t0, rent_seeking_equilibrium, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(rse_ex_t5, rent_seeking_equilibrium, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rse_ex_t10, rent_seeking_equilibrium, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The coordination type is an enforcement mechanism (regulatory capture).
narrative_ontology:coordination_type(rent_seeking_equilibrium, enforcement_mechanism).

% Network relationship: Regulatory capture that creates rent-seeking
% equilibria often leads to rigidities in other markets, like labor.
narrative_ontology:affects_constraint(rent_seeking_equilibrium, labor_market_flexibility).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */