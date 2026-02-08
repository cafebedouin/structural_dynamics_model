% ============================================================================
% CONSTRAINT STORY: agency_atrophy
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_agency_atrophy, []).

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
 * * constraint_id: agency_atrophy
 * human_readable: The Outsourced Cognition Trap
 * domain: technological/cognitive
 * * SUMMARY:
 * A scenario where the "Rope" of automated decision-support systems (AI assistants,
 * predictive navigation, automated scheduling) becomes so efficient that the
 * subject ceases to exercise the underlying cognitive skill. This coordination
 * tool for personal productivity becomes a "Snare" as the subject's primary
 * problem-solving and navigational agency is liquidated through disuse,
 * trapping them in a terminal dependency where they are no longer capable of
 * functioning without the substrate.
 * * KEY AGENTS:
 * - Dependent User: Subject (Powerless)
 * - Automation Provider: Beneficiary (Institutional)
 * - Cognitive Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the near-total siphoning of the subject's
% cognitive surplus into the maintenance of a substrate-dependent existence.
domain_priors:base_extractiveness(agency_atrophy, 0.88).
domain_priors:suppression_score(agency_atrophy, 0.79).   % Manual alternatives are suppressed by the extreme "friction" of non-automated life.
domain_priors:theater_ratio(agency_atrophy, 0.92).       % Extreme theater: "Personal Growth" apps that performatively signal agency while 0.88 extraction occurs.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(agency_atrophy, extractiveness, 0.88).
narrative_ontology:constraint_metric(agency_atrophy, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(agency_atrophy, theater_ratio, 0.92).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a coordination tool for efficiency.
narrative_ontology:constraint_claim(agency_atrophy, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(agency_atrophy). % Required for Tangled Rope

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(agency_atrophy, automation_provider).
narrative_ontology:constraint_victim(agency_atrophy, dependent_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: the "Rope" of convenience has liquidated their
% neurological capacity for the skill, making exit physically impossible.
constraint_indexing:constraint_classification(agency_atrophy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The provider views the automation as a Rope—the essential coordination
% substrate for achieving a high-efficiency, friction-free global society.
constraint_indexing:constraint_classification(agency_atrophy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) and suppression (0.79) masking as essential
% coordination. The presence of both beneficiaries and victims, plus active
% enforcement (via friction), confirms the Tangled Rope structure.
constraint_indexing:constraint_classification(agency_atrophy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Brain Training"
% module is an inertial spike; it performatively signals cognitive health
% while the underlying agency has already atrophied.
constraint_indexing:constraint_classification(agency_atrophy, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agency_atrophy_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless user vs Rope for the institutional provider.
    constraint_indexing:constraint_classification(agency_atrophy, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agency_atrophy, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(agency_atrophy, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92 >= 0.70) correctly triggers the Piton classification.
    domain_priors:theater_ratio(agency_atrophy, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(agency_atrophy, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    domain_priors:requires_active_enforcement(agency_atrophy),
    narrative_ontology:constraint_beneficiary(agency_atrophy, _),
    narrative_ontology:constraint_victim(agency_atrophy, _).

:- end_tests(agency_atrophy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * "coordination" benefit of automated life is achieved by liquidating the
 * subject's primary neurological capacity for independent agency. The high
 * theater ratio (0.92) captures the performative "gamification" of cognitive
 * tasks that masks this underlying atrophy, leading to the Piton classification.
 *
 * * PERSPECTIVAL GAP:
 * The Dependent User feels a Snare because their survival is now tethered
 * to a subscription they cannot mentally afford to cancel. The Provider
 * sees a Rope because the automation coordinates a perfectly predictable
 * and legible global labor and consumption market.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Resolved via the Tangled Rope and Piton classifications.
 * The Tangled Rope classification is critical here, as it acknowledges the
 * genuine coordination function (perceived by the beneficiary) while correctly
 * identifying the severe asymmetric extraction imposed on the victim. This
 * prevents the system from simplifying the constraint to a pure Snare, which
 * would ignore its origins as a coordination tool. The Piton classification
 * further resolves this by identifying the "Self-Actualization" features as
 * inert, theatrical components of the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_neuroplastic_reversibility,
    'Can "Cognitive Rehabilitation" restore the Rope, or is atrophy a biological "Mountain" (Snare vs Mountain)?',
    'Tracking the recovery velocity of spatial navigation skills in users after 12 months of "GPS-dark" living.',
    'If skills return: Snare of current lifestyle. If they fail: Mountain of Biological Critical Periods.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(agency_atrophy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional decision-support (0.35) to
% inertial "Agency Theater" (0.92) as the skill atrophies.
narrative_ontology:measurement(aa_tr_t0, agency_atrophy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aa_tr_t5, agency_atrophy, theater_ratio, 5, 0.68).
narrative_ontology:measurement(aa_tr_t10, agency_atrophy, theater_ratio, 10, 0.92).

% Extraction: Progressive liquidation of neurological agency into
% terminal substrate-dependency.
narrative_ontology:measurement(aa_ex_t0, agency_atrophy, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(aa_ex_t5, agency_atrophy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(aa_ex_t10, agency_atrophy, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system provides the underlying substrate for cognitive tasks.
narrative_ontology:coordination_type(agency_atrophy, global_infrastructure).

% Network relationships (structural influence edges)
% Atrophy of individual agency directly impacts the structure of the labor market.
narrative_ontology:affects_constraint(agency_atrophy, cognitive_labor_market).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */