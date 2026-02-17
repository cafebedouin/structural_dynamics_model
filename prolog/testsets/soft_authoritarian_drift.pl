% ============================================================================
% CONSTRAINT STORY: soft_authoritarian_drift
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_soft_authoritarian_drift, []).

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
 * * constraint_id: soft_authoritarian_drift
 * human_readable: The Incremental Compliance Web
 * domain: political/social/technological
 * * SUMMARY:
 * A scenario where a democratic or open system incrementally adopts restrictive
 * measures—often under the guise of safety, efficiency, or "community standards"—
 * that gradually erode individual autonomy. This "Rope" for social
 * coordination and risk mitigation becomes a "Snare" for the subject, as the
 * cumulative weight of "soft" nudges and administrative requirements
 * liquidates the practical possibility of dissent or exit.
 * * KEY AGENTS:
 * - Average Citizen: Subject (Powerless)
 * - Administrative Architect: Beneficiary (Institutional)
 * - Constitutional Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) as the system siphons the subject's behavioral
% optionality into the maintenance of a frictionless, managed equilibrium.
domain_priors:base_extractiveness(soft_authoritarian_drift, 0.84).
domain_priors:suppression_score(soft_authoritarian_drift, 0.72).
domain_priors:theater_ratio(soft_authoritarian_drift, 0.89). % High theater: maintaining democratic terminology while hollowing the substance.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(soft_authoritarian_drift, extractiveness, 0.84).
narrative_ontology:constraint_metric(soft_authoritarian_drift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(soft_authoritarian_drift, theater_ratio, 0.89).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary coordination mechanism for public safety.
narrative_ontology:constraint_claim(soft_authoritarian_drift, tangled_rope).
narrative_ontology:human_readable(soft_authoritarian_drift, "The Incremental Compliance Web").

% Binary flags and structural properties required for Tangled Rope.
domain_priors:requires_active_enforcement(soft_authoritarian_drift).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, administrative_architects).
narrative_ontology:constraint_victim(soft_authoritarian_drift, average_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the citizen, the drift is a snare: each individual rule seems
% reasonable, but the total set traps them in a high-compliance, low-agency state.
constraint_indexing:constraint_classification(soft_authoritarian_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the drift as a Rope—the essential coordination of
% modern, complex societies required to prevent "harmful" volatility.
constraint_indexing:constraint_classification(soft_authoritarian_drift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and suppression (0.72) as a hybrid
% Tangled Rope, where a genuine coordination function is coupled with
% severe asymmetric extraction, requiring active enforcement.
constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civil_izational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Democratic Processes"
% are an inertial spike—they exist in form but no longer steer the outcome.
constraint_indexing:constraint_classification(soft_authoritarian_drift, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(soft_authoritarian_drift_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(soft_authoritarian_drift, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(soft_authoritarian_drift, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(soft_authoritarian_drift, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(soft_authoritarian_drift, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions_met) :-
    % Verify that all structural preconditions for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(soft_authoritarian_drift, _),
    narrative_ontology:constraint_victim(soft_authoritarian_drift, _),
    domain_priors:requires_active_enforcement(soft_authoritarian_drift).

:- end_tests(soft_authoritarian_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the
 * "coordination" benefit of societal safety is effectively paid for by
 * the liquidation of the subject's primary political agency. The high
 * suppression (0.72) represents the difficulty of opting out of the
 * digital and administrative systems that enforce compliance. The extreme
 * theater ratio (0.89) captures the performative maintenance of democratic
 * language and ritual while the underlying function has atrophied.
 *
 * * PERSPECTIVAL GAP:
 * The Citizen feels a Snare because their "freedom" is now restricted to
 * choices within a pre-approved administrative range. The Architect sees
 * a Rope because the drift ensures the coordination of predictable social
 * outcomes and minimizes the "noise" of genuine dissent.
 *
 * * [RESOLVED MANDATROPHY]:
 * The Mandatrophy is resolved by the Analytical Observer's classification of
 * the system as a Tangled Rope. This classification correctly identifies that
 * while a coordination function exists (benefiting the architects), it is
 * inextricably linked to a severe, asymmetric extraction of agency from the
 * citizenry. This prevents the system from being misclassified as a pure Snare
 * (ignoring the coordination claim) or a pure Rope (ignoring the extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_democratic_resilience,
    'Can the drift be reversed by internal reform, or is the institutional inertia final (Snare vs Mountain)?',
    'Tracking the success rate of legislative reversals of "emergency-origin" compliance mandates.',
    'If reversal occurs: Snare of current policy. If reversal fails: Mountain of Institutional Sclerosis.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(soft_authoritarian_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The narrative describes a "drift," so metrics intensify over the interval.
% This data is required because base_extractiveness > 0.46.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(sad_tr_t0, soft_authoritarian_drift, theater_ratio, 0, 0.15).
narrative_ontology:measurement(sad_tr_t5, soft_authoritarian_drift, theater_ratio, 5, 0.55).
narrative_ontology:measurement(sad_tr_t10, soft_authoritarian_drift, theater_ratio, 10, 0.89).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(sad_ex_t0, soft_authoritarian_drift, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(sad_ex_t5, soft_authoritarian_drift, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(sad_ex_t10, soft_authoritarian_drift, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system is a web of rules and compliance checks.
narrative_ontology:coordination_type(soft_authoritarian_drift, enforcement_mechanism).

% Network relationships (structural influence edges)
% This drift is often coupled with and enabled by digital identity systems.
narrative_ontology:affects_constraint(soft_authoritarian_drift, digital_identity_systems).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */