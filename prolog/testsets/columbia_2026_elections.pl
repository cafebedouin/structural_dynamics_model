% ============================================================================
% CONSTRAINT STORY: colombia_2026_presidential_election
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(colombia_2026_presidential_election, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: colombia_2026_presidential_election
 *   human_readable: 2026 Colombian Presidential Election Structure
 *   domain: political
 *
 * SUMMARY:
 *   The 2026 Colombian presidential election is a structural constraint defined
 *   by a constitutional one-term limit on the incumbent, forcing a transition.
 *   The system exhibits extreme polarization between the incumbent's leftist
 *   coalition and an emerging populist right, creating a two-round runoff
 *   system that functions as both a coordination mechanism for political blocs
 *   and an extractive process that marginalizes centrist alternatives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Undecided/Centrist Voters: Primary target (powerless/trapped) — bears the cost of reduced choice due to polarization.
 *   - Polarized Political Blocs (Pacto Histórico, Populist Right): Primary beneficiary (institutional/arbitrage) — benefits from the two-round system that consolidates their power.
 *   - Centrist Candidates (e.g., Sergio Fajardo): Secondary target (moderate/constrained) — structurally marginalized by the system's incentives.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: While elections are coordination mechanisms, the intense polarization
% and use of state/corporate machinery for campaign leverage create asymmetric
% benefit flows, justifying a moderate-to-high extraction score.
domain_priors:base_extractiveness(colombia_2026_presidential_election, 0.45).

% Rationale: The media logic and donor-pacing of the polarized "two-pole" race
% effectively marginalize third-way options. A score of 0.40 is the threshold
% for Tangled Rope, reflecting the structural, rather than legal, suppression.
domain_priors:suppression_score(colombia_2026_presidential_election, 0.40).

% Rationale: The election is a high-stakes political process, but the core
% function (choosing a leader) is real. The theater is in the campaign rhetoric,
% not a substitute for the function itself.
domain_priors:theater_ratio(colombia_2026_presidential_election, 0.20).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colombia_2026_presidential_election, extractiveness, 0.45).
narrative_ontology:constraint_metric(colombia_2026_presidential_election, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(colombia_2026_presidential_election, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(colombia_2026_presidential_election, tangled_rope).
narrative_ontology:human_readable(colombia_2026_presidential_election, "2026 Colombian Presidential Election Structure").
narrative_ontology:topic_domain(colombia_2026_presidential_election, "political").

% --- Binary flags ---
% The electoral structure is maintained by the National Electoral Council (CNE)
% and the constitutional court; it is a legal mandate.
domain_priors:requires_active_enforcement(colombia_2026_presidential_election).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(colombia_2026_presidential_election, polarized_political_blocs).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(colombia_2026_presidential_election, undecided_centrist_voters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% For a centrist or undecided voter, the system forces a choice between two
% extremes, extracting the value of their nuanced preference. The coordination
% function is still present (an election happens), but it's tangled with high
% extraction.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.45 * 1.42 * 1.0 (national) = 0.639. This is in the Tangled Rope range [0.40, 0.90].
constraint_indexing:constraint_classification(colombia_2026_presidential_election, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the major political blocs, the election is a pure coordination tool to
% consolidate power and implement their agenda. They benefit from the polarization
% that the system incentivizes.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ = 0.45 * -0.12 * 1.0 (national) = -0.054. This is a clear Rope.
constraint_indexing:constraint_classification(colombia_2026_presidential_election, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (electing a government) and
% the asymmetric extraction (marginalization of the center). The system is a
% hybrid, a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.45 * 1.15 * 1.2 (global) = 0.621. This is in the Tangled Rope range.
constraint_indexing:constraint_classification(colombia_2026_presidential_election, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colombia_2026_presidential_election_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, TypeTarget,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, TypeBeneficiary,
        context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == tangled_rope),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_claim_consistency) :-
    % The analytical perspective must match the constraint_claim.
    narrative_ontology:constraint_claim(colombia_2026_presidential_election, ClaimType),
    constraint_indexing:constraint_classification(colombia_2026_presidential_election, AnalyticalType,
        context(agent_power(analytical), _, _, _)),
    assertion(ClaimType == AnalyticalType).

test(tangled_rope_structural_properties) :-
    % Verify the three required properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(colombia_2026_presidential_election, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(colombia_2026_presidential_election, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(colombia_2026_presidential_election).

:- end_tests(colombia_2026_presidential_election_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) and suppression (0.40) were chosen to
 *   model a system that is functionally a coordination mechanism (an election)
 *   but whose incentive structure creates significant negative externalities
 *   (extraction) for certain participants. The suppression score of 0.40 is
 *   key, placing it just inside the Tangled Rope category to reflect the
 *   structural, not legal, marginalization of centrist options.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the beneficiaries (polarized political blocs), the
 *   system is an efficient Rope for coordinating power. For the victims
 *   (undecided/centrist voters), it is a Tangled Rope that offers a choice
 *   while simultaneously extracting the value of moderation and nuance, forcing
 *   them into a binary they may not prefer.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `polarized_political_blocs`. The two-round runoff system
 *     benefits actors who can consolidate large, ideologically rigid bases.
 *     They have `arbitrage` exit because they define and manipulate the rules.
 *   - Victims: `undecided_centrist_voters`. This group bears the cost of
 *     polarization. Their options are suppressed, and they are `trapped` into
 *     choosing the "lesser of two evils," a classic extractive dynamic.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the hybrid nature of the constraint.
 *   Labeling it a pure Rope would ignore the real, structural costs imposed on
 *   centrist voters. Labeling it a pure Snare would deny its genuine coordination
 *   function in selecting a national leader. Tangled Rope captures this duality,
 *   preventing misclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_colombian_polarization,
    'Will the two-round system intensify polarization to the point where centrist alternatives are structurally eliminated?',
    'Post-election analysis of vote fragmentation and centrist party viability after the May 2026 results.',
    'If centrists survive: The constraint remains a Tangled Rope. If eliminated: It hardens into a Snare for non-aligned voters.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_colombian_polarization, empirical, 'Whether polarization structurally eliminates centrist options in the 2026 election cycle.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(colombia_2026_presidential_election, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the intensification of polarization and performative
% campaigning as the election approaches.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(colombia_2026_presidential_election_tr_t0, colombia_2026_presidential_election, theater_ratio, 0, 0.10).
narrative_ontology:measurement(colombia_2026_presidential_election_tr_t5, colombia_2026_presidential_election, theater_ratio, 5, 0.15).
narrative_ontology:measurement(colombia_2026_presidential_election_tr_t10, colombia_2026_presidential_election, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(colombia_2026_presidential_election_ex_t0, colombia_2026_presidential_election, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(colombia_2026_presidential_election_ex_t5, colombia_2026_presidential_election, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(colombia_2026_presidential_election_ex_t10, colombia_2026_presidential_election, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: An election is a mechanism for allocating political power.
narrative_ontology:coordination_type(colombia_2026_presidential_election, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% groups and their exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */