% ============================================================================
% CONSTRAINT STORY: hu_2026_election_rules
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hu_2026_election_rules, []).

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
 * * constraint_id: hu_2026_election_rules
 * human_readable: The 2026 Hungarian Mixed-Member Majoritarian "Inertia"
 * domain: political
 * * SUMMARY:
 * The 199-seat Hungarian parliamentary system utilizes a winner-compensated mixed
 * model where votes from constituency victors are added back to national lists.
 * Redrawn 2024 district boundaries (reducing Budapest seats and increasing Pest
 * County seats) are perceived by critics as a mechanism to force a 3-5% "popular
 * vote tax" on the opposition TISZA party to achieve a majority.
 * * KEY AGENTS:
 * - [Opposition Voters]: Subject (Powerless) - Faces a "plurality trap" where a
 * majority of votes can result in a minority of seats.
 * - [Fidesz-KDNP Alliance]: Beneficiary (Institutional) - Maintains governance
 * stability and parliamentary efficiency through the current framework.
 * - [OSCE/Analytical Observers]: Auditor (Analytical) - Evaluates the "tangled"
 * nature of coordination for stability vs. asymmetric power extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction represents the seat-to-vote premium (historically ~18% gain for the largest party).
domain_priors:base_extractiveness(hu_2026_election_rules, 0.58). % Snare extraction >= 0.46
% Suppression represents the high barrier for entry and fragmentation risks.
domain_priors:suppression_score(hu_2026_election_rules, 0.62).
domain_priors:theater_ratio(hu_2026_election_rules, 0.45).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(hu_2026_election_rules, extractiveness, 0.58).
narrative_ontology:constraint_metric(hu_2026_election_rules, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hu_2026_election_rules, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(hu_2026_election_rules, tangled_rope).
narrative_ontology:human_readable(hu_2026_election_rules, "The 2026 Hungarian Mixed-Member Majoritarian \"Inertia\"").

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(hu_2026_election_rules).
narrative_ontology:constraint_beneficiary(hu_2026_election_rules, fidesz_kdnp_alliance).
narrative_ontology:constraint_victim(hu_2026_election_rules, opposition_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The opposition voter views the system as a Snare; gerrymandered districts
% (spatial_scope: national) act as a trap where individual votes are diluted.
constraint_indexing:constraint_classification(hu_2026_election_rules, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The incumbent views the law as a Rope—essential coordination infrastructure
% preventing "coalition chaos" and ensuring generational national continuity.
constraint_indexing:constraint_classification(hu_2026_election_rules, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Historical analysis detects high extraction (0.58) combined with coordination functions.
% This is a classic Tangled Rope where the "extraction" is baked into the "coordination."
constraint_indexing:constraint_classification(hu_2026_election_rules, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hu_2026_election_rules_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(hu_2026_election_rules, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hu_2026_election_rules, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(hu_2026_election_rules, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    narrative_ontology:constraint_metric(hu_2026_election_rules, extractiveness, E),
    E >= 0.46. % Confirms high-extraction signature requiring Omega resolution.

:- end_tests(hu_2026_election_rules_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Perspectival Gap exists because of "Winner Compensation." For the beneficiary,
 * this is a coordination tool (Rope) that turns a plurality into a strong mandate.
 * For the subject, it is a Snare because it allows the governing party to
 * "extract" parliamentary seats even if their popular support is stagnating
 * around 41-44%.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by recognizing that the 2024 amendments (gerrymandering
 * in Budapest) shifted the system from a "Rope" (intended for stability) toward
 * a "Snare" (intended for incumbency preservation). The Tangled Rope classification
 * is critical here. A simpler model might classify this as a pure Snare, missing
 * the genuine (if asymmetric) coordination function of producing a stable government.
 * This prevents Mandatrophy by acknowledging both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_hu_fairness_elasticity,
    'Can the TISZA party popular support exceed the 3-5% "gerrymandering tax"?',
    'Historical D\'Hondt simulation vs. 2026 polling delta.',
    'If TISZA > 5% lead, the Snare snaps (Mountain behavior). If TISZA < 3% lead, Fidesz retains plurality seats.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hu_2026_election_rules, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The system's extractive properties have intensified over time, particularly
% after the 2012 and 2024 reforms.
% T=0: Pre-2012 system
% T=5: Post-2012 reforms (winner compensation)
% T=10: Post-2024 reforms (district boundary changes)

% Theater ratio over time (performative claims of "stability" vs function):
narrative_ontology:measurement(hu_2026_election_rules_tr_t0, hu_2026_election_rules, theater_ratio, 0, 0.20).
narrative_ontology:measurement(hu_2026_election_rules_tr_t5, hu_2026_election_rules, theater_ratio, 5, 0.35).
narrative_ontology:measurement(hu_2026_election_rules_tr_t10, hu_2026_election_rules, theater_ratio, 10, 0.45).

% Extraction over time (vote-to-seat premium):
narrative_ontology:measurement(hu_2026_election_rules_ex_t0, hu_2026_election_rules, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hu_2026_election_rules_ex_t5, hu_2026_election_rules, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(hu_2026_election_rules_ex_t10, hu_2026_election_rules, base_extractiveness, 10, 0.58).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Electoral systems are a form of resource allocation (parliamentary seats).
narrative_ontology:coordination_type(hu_2026_election_rules, resource_allocation).

% The electoral rules structurally influence the composition of other state bodies.
narrative_ontology:affects_constraint(hu_2026_election_rules, hu_media_council_composition).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */