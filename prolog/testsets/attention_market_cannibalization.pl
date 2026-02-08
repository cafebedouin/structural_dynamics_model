% ============================================================================
% CONSTRAINT STORY: attention_market_cannibalization
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_attention_market_cannibalization, []).

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
 * * constraint_id: attention_market_cannibalization
 * human_readable: The Cognitive Exhaustion Loop
 * domain: economic/psychological/technological
 * * SUMMARY:
 * A scenario where competing digital platforms optimize for "engagement" by
 * siphoning more of a subject's finite attentional bandwidth than is
 * required for the subject to maintain their primary life-functions (work,
 * health, relationships). This coordination mechanism for content distribution
 * becomes a "Snare" as the subject's long-term cognitive agency is
 * liquidated to satisfy immediate algorithmic reward signals, trapping
 * the user in a territory of terminal distraction where the cost of "opting
 * out" is total social or economic exclusion.
 * * KEY AGENTS:
 * - Digital Participant: Subject (Powerless)
 * - Attention Platform: Beneficiary (Institutional)
 * - Neuro-Economic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(attention_market_cannibalization, 0.91). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(attention_market_cannibalization, 0.84).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(attention_market_cannibalization, 0.88).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(attention_market_cannibalization, extractiveness, 0.91).
narrative_ontology:constraint_metric(attention_market_cannibalization, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(attention_market_cannibalization, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(attention_market_cannibalization, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(attention_market_cannibalization). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(attention_market_cannibalization, attention_platform).
narrative_ontology:constraint_victim(attention_market_cannibalization, digital_participant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The participant is trapped: the "Rope" of connection liquidates their
% capacity for sustained focus, making exit psychologically and socially impossible.
constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform views the system as a Rope—the only way to coordinate
% a global market for information and remain competitive.
constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.91) and high suppression (0.84) masking as
% essential coordination. The presence of beneficiaries, victims, and active
% enforcement confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(attention_market_cannibalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% The high theater ratio (0.88) triggers a Piton classification. The "Terms of Service"
% agreement is an inertial spike; it performatively signals consent for a
% process that has already bypassed biological agency.
constraint_indexing:constraint_classification(attention_market_cannibalization, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(attention_market_cannibalization, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_cannibalization_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless participant vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
        context(agent_power(institutional), _, _, _)),
    \+ constraint_indexing:constraint_classification(attention_market_cannibalization, rope,
        context(agent_power(powerless), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    domain_priors:theater_ratio(attention_market_cannibalization, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(attention_market_cannibalization, piton,
        context(agent_power(analytical), _, _, _)).

test(snare_from_high_extraction) :-
    % Verify that high base extraction (0.91) results in a Snare classification for the powerless.
    domain_priors:base_extractiveness(attention_market_cannibalization, E),
    E > 0.46,
    constraint_indexing:constraint_classification(attention_market_cannibalization, snare,
        context(agent_power(powerless), _, _, _)).

:- end_tests(attention_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) is extremely high, reflecting a "Mandatrophy" state where the "coordination" benefit of information access is achieved by liquidating the subject's primary biological capacity for agency. The suppression score (0.84) represents the high psychological and social cost of opting out. The high theater ratio (0.88) reflects performative "Digital Wellbeing" features that mask the underlying extraction.
 *
 * * PERSPECTIVAL GAP:
 * The Digital Participant experiences a Snare because their "leisure" has become high-extraction labor they cannot stop. The Platform, however, views this as a Rope, as the total capture of attention is the only way to coordinate a legible and monetizable global social fabric and remain competitive.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system resolves this extreme extraction by not defaulting to a simple Snare classification. The analytical perspective reveals a more complex structure. The high theater ratio triggers a Piton classification, correctly identifying features like "Terms of Service" or "Wellness Dashboards" as inert, performative artifacts. The combination of a clear coordination function (for the platform) and asymmetric extraction (for the user) also leads to a Tangled Rope classification. This prevents the system from mislabeling the coordination aspect as pure, simple extraction, providing a more nuanced analysis of the failure mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_attentional_recovery_limit,
    'Can "Cognitive Sovereignty" laws restore the Rope, or is capture a physical law of neural networks (Snare vs Mountain)?',
    'Tracking the success rate of "Device-Free Zones" in restoring deep-work capacity in urban professionals.',
    'If recovery holds: Snare of current culture. If it fails: Mountain of Neurological Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(attention_market_cannibalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio: Rising from functional engagement optimization (0.20)
% to extreme "Digital Wellbeing" theater (0.88) that performatively signals
% care while siphoning attention surplus.
narrative_ontology:measurement(cannibal_tr_t0, attention_market_cannibalization, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cannibal_tr_t5, attention_market_cannibalization, theater_ratio, 5, 0.54).
narrative_ontology:measurement(cannibal_tr_t10, attention_market_cannibalization, theater_ratio, 10, 0.88).

% Extraction: Progressive accumulation of attentional capture as platform
% competition liquidates the subject's primary life-function bandwidth.
narrative_ontology:measurement(cannibal_ex_t0, attention_market_cannibalization, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cannibal_ex_t5, attention_market_cannibalization, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(cannibal_ex_t10, attention_market_cannibalization, base_extractiveness, 10, 0.91).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(attention_market_cannibalization, global_infrastructure).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(attention_market_cannibalization, mental_health_degradation).
% narrative_ontology:affects_constraint(attention_market_cannibalization, labor_market_precarity).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */