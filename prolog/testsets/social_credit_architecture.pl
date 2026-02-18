% ============================================================================
% CONSTRAINT STORY: social_credit_architecture
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-16
% ============================================================================

:- module(constraint_social_credit_architecture, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: social_credit_architecture
 *   human_readable: Social Credit Architecture
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   A national-scale system that operationalizes "trustworthiness" by decoding
 *   behavioral data into a unified score. It creates real-world consequences
 *   (e.g., access to travel, loans, employment) based on compliance with
 *   state-sanctioned norms, blending surveillance with social engineering.
 *
 * KEY AGENTS (by structural relationship):
 *   - Low-Score Citizens: Primary target (powerless/trapped) — bears extraction
 *   - State Planners & High-Score Elites: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - External Policy Analysts: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_credit_architecture, 0.80). % Snare extraction >= 0.46
domain_priors:suppression_score(social_credit_architecture, 0.70).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(social_credit_architecture, 0.15).       % Not a Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_credit_architecture, extractiveness, 0.80).
narrative_ontology:constraint_metric(social_credit_architecture, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(social_credit_architecture, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
% The system is analytically a Tangled Rope, possessing both a genuine
% coordination function and severe asymmetric extraction.
narrative_ontology:constraint_claim(social_credit_architecture, tangled_rope).
narrative_ontology:human_readable(social_credit_architecture, "Social Credit Architecture").
narrative_ontology:topic_domain(social_credit_architecture, "social/political/technological").

% --- Binary flags ---
domain_priors:requires_active_enforcement(social_credit_architecture). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
narrative_ontology:constraint_beneficiary(social_credit_architecture, state_planners).
narrative_ontology:constraint_victim(social_credit_architecture, low_score_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% For a citizen with a low score, the system is a trap that curtails economic
% and social mobility. Engine derives d from victim status + trapped exit
% (d ≈ 0.95), leading to high effective extraction (χ).
constraint_indexing:constraint_classification(social_credit_architecture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For the state and high-scoring elites, it's a coordination mechanism that
% reduces social friction. Engine derives d from beneficiary status + arbitrage
% exit (d ≈ 0.05), leading to negative effective extraction (χ, felt as a subsidy).
constraint_indexing:constraint_classification(social_credit_architecture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function and the severe, asymmetric
% extraction. It is a hybrid system, not a pure snare or pure rope.
% The analytical perspective has a canonical d ≈ 0.72.
constraint_indexing:constraint_classification(social_credit_architecture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_credit_architecture_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Snare) and institutional (Rope) views.
    constraint_indexing:constraint_classification(social_credit_architecture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_credit_architecture, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer must see the hybrid nature of the constraint.
    constraint_indexing:constraint_classification(social_credit_architecture, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical == tangled_rope.

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(social_credit_architecture, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(social_credit_architecture, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(social_credit_architecture).

:- end_tests(social_credit_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.80 to reflect the profound impact on life
 *   chances for those who fall afoul of the system. It extracts social and
 *   political compliance. The suppression score of 0.70 reflects the difficulty
 *   of opting out in a highly digitized society where the system is pervasive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a low-score citizen (powerless/trapped), the system is
 *   a Snare—a coercive trap with no escape. For state planners (institutional/arbitrage),
 *   it is a Rope—a valuable tool for maintaining social order and predictability.
 *   This difference is driven by their structural relationship to the constraint,
 *   which the engine captures via directionality (d).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'state_planners' and high-score elites benefit from reduced
 *     social friction and increased predictability. Their arbitrage exit options
 *     and beneficiary status result in a low directionality score (d), making
 *     effective extraction (χ) negative (a subsidy).
 *   - Victims: 'low_score_citizens' bear the costs through restricted access
 *     to services and mobility. Their trapped status and victim designation
 *     result in a high directionality score (d), maximizing effective extraction.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The system's public claim to be a pure coordination tool ('rope') for building
 *   societal trust is a form of mandatrophy. The analytical classification as
 *   'tangled_rope' resolves this by acknowledging the genuine coordination
 *   function (for beneficiaries) while simultaneously accounting for the severe,
 *   asymmetric extraction imposed on victims. This prevents the system from being
 *   misclassified as a pure Snare (ignoring its coordination role) or a pure Rope
 *   (ignoring its coercive nature). The omega_variable also addresses this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_social_credit_architecture,
    'Is the system primarily for economic enforcement (e.g., debt) or for political repression?',
    'Analysis of the ratio of financial defaulters vs. political dissenters on blacklists over a 5-year period.',
    'If primarily economic, it functions closer to a harsh credit system (Tangled Rope). If primarily political, it is a pure instrument of state control (Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(social_credit_architecture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (0.80 > 0.46) shows its
% gradual intensification from a pilot program to a full-scale architecture.
%
% Theater ratio over time (subtle increase in performative compliance):
narrative_ontology:measurement(sca_tr_t0, social_credit_architecture, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sca_tr_t5, social_credit_architecture, theater_ratio, 5, 0.10).
narrative_ontology:measurement(sca_tr_t10, social_credit_architecture, theater_ratio, 10, 0.15).

% Extraction over time (scope creep and intensification):
narrative_ontology:measurement(sca_ex_t0, social_credit_architecture, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(sca_ex_t5, social_credit_architecture, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(sca_ex_t10, social_credit_architecture, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system's primary function is enforcing behavioral norms.
narrative_ontology:coordination_type(social_credit_architecture, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */