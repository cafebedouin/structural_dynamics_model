% ============================================================================
% CONSTRAINT STORY: swift_legacy_piton
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_swift_legacy_piton, []).

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
 * * constraint_id: swift_legacy_piton
 * human_readable: The SWIFT Financial Messaging Inertia
 * domain: technological/economic
 * * SUMMARY:
 * A global financial messaging standard that has outlived its technical
 * utility but remains mandatory due to the "Maintenance Debt" of the global
 * banking system. It functions as a Piton—inertial maintenance of a
 * decaying constraint, where performative updates mask underlying stagnation.
 * * KEY AGENTS:
 * - Global Remitters & Merchants: Subject (Powerless) - Face 3-5 day delays and opaque fees.
 * - Legacy Financial Institutions: Beneficiary (Institutional) - Rely on the Piton for gatekeeping and fee extraction.
 * - Fintech Systems Auditor: Auditor (Analytical) - Observes the "Piton" gap between claimed and actual function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(swift_legacy_piton, 0.49). % High: Opaque correspondent banking fees and liquidity costs.
domain_priors:suppression_score(swift_legacy_piton, 0.90).   % Extremely High: National economies are disconnected if removed without a replacement.
domain_priors:theater_ratio(swift_legacy_piton, 0.75).       % High: Modern "updates" (gpi) are often theatrical patches on 1970s tech.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(swift_legacy_piton, extractiveness, 0.49).
narrative_ontology:constraint_metric(swift_legacy_piton, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(swift_legacy_piton, theater_ratio, 0.75).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(swift_legacy_piton, piton).

% Binary flags
domain_priors:requires_active_enforcement(swift_legacy_piton). % Sanctions and compliance regimes enforce participation.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(swift_legacy_piton, legacy_financial_institutions).
narrative_ontology:constraint_victim(swift_legacy_piton, global_remitters_and_merchants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE GLOBAL SOUTH MERCHANT (SNARE)
% To the merchant, SWIFT is a Snare—a mandatory, slow, and extractive trap with no viable state-sanctioned exit.
% χ = 0.49 (ε) * 1.5 (π(powerless)) * 1.2 (σ(global)) = 0.882
constraint_indexing:constraint_classification(swift_legacy_piton, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CENTRAL BANK (ROPE)
% To the state, it remains a Rope—the only reliable way to coordinate global liquidity and enforcement.
% χ = 0.49 (ε) * -0.2 (π(institutional)) * 1.2 (σ(global)) = -0.1176 (felt as a benefit/necessity)
constraint_indexing:constraint_classification(swift_legacy_piton, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Detection of "Piton" status: High theater_ratio indicates the protocol is maintained by inertia, not efficiency.
constraint_indexing:constraint_classification(swift_legacy_piton, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(swift_legacy_piton, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(swift_legacy_piton_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(swift_legacy_piton, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(swift_legacy_piton, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(piton_threshold_validation) :-
    % Verify the analytical observer detects the Piton signature based on theater ratio.
    domain_priors:theater_ratio(swift_legacy_piton, TR),
    (TR > 0.70 ->
        constraint_indexing:constraint_classification(swift_legacy_piton, piton, context(agent_power(analytical), _, _, _))
    ;   \+ constraint_indexing:constraint_classification(swift_legacy_piton, piton, context(agent_power(analytical), _, _, _))
    ).

:- end_tests(swift_legacy_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high base_extractiveness (0.49) and suppression_score (0.90) create a classic Snare for powerless users.
 * However, for institutional actors, the negative power modifier π(institutional)=-0.2 makes the effective extraction
 * negative, classifying it as a Rope that provides coordination benefits. The theater_ratio (0.75) reflects how
 * SWIFT markets "innovation" (like SWIFT gpi) to mask the underlying maintenance debt of 50-year-old messaging standards.
 * This high theater ratio is what allows the analytical observer to correctly classify it as a Piton.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * Mandatrophy is resolved by the "Piton" classification. The system is no longer a "Rope" of efficiency;
 * it is a structural fossil that persists because the cost of "untethering" the global economy is too high.
 * The system correctly identifies that what an institution sees as a Rope can simultaneously be a Snare for users
 * and a decaying Piton for auditors, preventing a monolithic and incorrect classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_piton_snap,
    'When does the friction of the Piton exceed the cost of migrating to an Alternative Scaffold (e.g., ISO20022-native systems)?',
    'Comparative transaction volume growth and cost analysis: SWIFT vs. mBridge/FedNow/P2P networks.',
    'If snap occurs: Sudden collapse of the legacy Rope and rapid adoption of a new standard. If delayed: Continued Piton-decay and rent-seeking.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(swift_legacy_piton, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has high extraction (0.49 > 0.46), requiring temporal data.
% The model shows extraction accumulating as the system became entrenched,
% and theater ratio rising sharply in the latter half of the interval as
% challengers (e.g., crypto, fintech) emerged, forcing performative innovation.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(swift_legacy_piton_tr_t0, swift_legacy_piton, theater_ratio, 0, 0.10).
narrative_ontology:measurement(swift_legacy_piton_tr_t5, swift_legacy_piton, theater_ratio, 5, 0.50).
narrative_ontology:measurement(swift_legacy_piton_tr_t10, swift_legacy_piton, theater_ratio, 10, 0.75).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(swift_legacy_piton_ex_t0, swift_legacy_piton, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(swift_legacy_piton_ex_t5, swift_legacy_piton, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(swift_legacy_piton_ex_t10, swift_legacy_piton, base_extractiveness, 10, 0.49).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(swift_legacy_piton, information_standard).

% Network relationships (structural influence edges)
% The inertia of the SWIFT Piton directly affects the adoption rate and
% design of its intended successor, the ISO20022 standard.
narrative_ontology:affects_constraint(swift_legacy_piton, iso20022_migration_scaffold).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */