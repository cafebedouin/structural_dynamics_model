% ============================================================================
% CONSTRAINT STORY: global_hoarding_scaling_laws
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_global_hoarding_scaling_laws, []).

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
 * * constraint_id: global_hoarding_scaling_laws
 * human_readable: The Planetary Siphon
 * domain: economic/political
 * * SUMMARY:
 * An expansion of localized selfishness into a globalized extraction system.
 * As the spatial scope increases, the "suppression score" scales due to the
 * increased distance between the beneficiary and the victims, making the
 * constraint appear as an immutable "Mountain" to the powerless, while its
 * beneficiaries see it as essential coordination.
 * * KEY AGENTS:
 * - The Global Precariat: Subject (Powerless) - Distributed victims.
 * - The Sovereign Fund: Beneficiary (Institutional) - The global hoarder.
 * - The Forensic Economist: Auditor (Analytical) - Mapping the flow of value.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is extremely high, and suppression is scaled by global scope.
domain_priors:base_extractiveness(global_hoarding_scaling_laws, 0.85).
domain_priors:suppression_score(global_hoarding_scaling_laws, 0.90).
domain_priors:theater_ratio(global_hoarding_scaling_laws, 0.60).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, extractiveness, 0.85).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(global_hoarding_scaling_laws, theater_ratio, 0.60).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(global_hoarding_scaling_laws, tangled_rope).

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(global_hoarding_scaling_laws).
narrative_ontology:constraint_beneficiary(global_hoarding_scaling_laws, sovereign_fund).
narrative_ontology:constraint_victim(global_hoarding_scaling_laws, global_precariat).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% At a global scale with zero exit options, the overwhelming extraction and
% suppression are felt as an unchangeable natural law, not a constructed system.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Sovereign Fund views the system as essential and efficient global coordination,
% experiencing negative extraction (net benefit).
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function for beneficiaries and the
% severe, asymmetric extraction from victims, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(global_hoarding_scaling_laws, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_hoarding_scaling_laws_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the tangled rope.
    constraint_indexing:constraint_classification(global_hoarding_scaling_laws, tangled_rope, context(agent_power(analytical), _, _, _)).

test(extraction_is_severe) :-
    domain_priors:base_extractiveness(global_hoarding_scaling_laws, E),
    E > 0.80.

:- end_tests(global_hoarding_scaling_laws_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a system of global capital extraction that is so vast and
 * complex it produces radically different realities for its participants.
 * - For the 'Global Precariat' (powerless, trapped), the effective extraction
 *   (χ = 0.85 * 1.5 * 1.2 = 1.53) and suppression (0.90) are so absolute that the
 *   system is perceived as a 'Mountain'—an unchangeable feature of the world.
 * - For the 'Sovereign Fund' (institutional, mobile), the system provides a net
 *   benefit (χ is negative), making it appear as a pure 'Rope' of coordination.
 * - The analytical observer, however, can see both sides. The presence of
 *   beneficiaries, victims, and active enforcement against a backdrop of high
 *   extraction and suppression is the classic signature of a 'Tangled Rope'.
 *
 * [RESOLVED MANDATROPHY]
 * The extreme extraction (0.85) is sustained by the global scope, which creates
 * information asymmetry and fragments opposition. The system's claim to be
 * 'coordination' is a key part of its theatrical maintenance (theater_ratio=0.60),
 * but the underlying mechanics are pure asymmetric extraction, correctly
 * identified by the Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_global_coalition,
    'Can the Precariat achieve "organized" power at a global scale?',
    'Detection of cross-border labor/resource coordination events that successfully challenge the system.',
    'If True: The Mountain perspective would collapse, reverting to a Snare as the system becomes contestable. If False: The Mountain perception solidifies.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_hoarding_scaling_laws, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Institutionalization of global justifications)
narrative_ontology:measurement(ghs_tr_t0, global_hoarding_scaling_laws, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ghs_tr_t5, global_hoarding_scaling_laws, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ghs_tr_t10, global_hoarding_scaling_laws, theater_ratio, 10, 0.60).

% Extraction over time (The "Siphon" effect intensifying)
narrative_ontology:measurement(ghs_ex_t0, global_hoarding_scaling_laws, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(ghs_ex_t5, global_hoarding_scaling_laws, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(ghs_ex_t10, global_hoarding_scaling_laws, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This system functions as a kind of global infrastructure for capital flow.
narrative_ontology:coordination_type(global_hoarding_scaling_laws, global_infrastructure).

% This form of capital hoarding directly impacts investment in and availability
% of other critical global infrastructure, like semiconductor supply chains.
narrative_ontology:affects_constraint(global_hoarding_scaling_laws, semiconductor_supply).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */