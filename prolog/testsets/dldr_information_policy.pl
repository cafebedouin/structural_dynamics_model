% ============================================================================
% CONSTRAINT STORY: dldr_information_policy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_dldr_information_policy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dldr_information_policy
 *   human_readable: "Don’t Like, Don’t Read" (DLDR) Information Policy
 *   domain: technological/social
 *
 * SUMMARY:
 *   This constraint represents the "Don’t Like, Don’t Read" (DLDR) policy, a
 *   coordination mechanism common in online archives where the responsibility
 *   for content filtering shifts from the platform to the reader. It relies on a
 *   granular, user-generated tagging system that allows individuals to make an
 *   informed determination of content suitability before engagement, thereby
 *   enabling a pluralistic and "unsanitized" information environment.
 *
 * KEY AGENTS (by structural relationship):
 *   - Users Expecting Sanitization: Primary target (powerless/trapped) — bears the cognitive load of navigating a complex, unfiltered system.
 *   - Archive Users & Content Creators: Primary beneficiaries (moderate/mobile) — benefit from autonomy and freedom of expression.
 *   - Platform Administrators: Institutional beneficiary (institutional/arbitrage) — benefit from a scalable community moderation system that reduces liability.
 *   - Analytical Observer: Sees the full structure as a low-extraction coordination mechanism.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The extraction is the cognitive/labor "tax" of tagging for creators
% and vetting for readers. It's a real cost, but low and symmetric, representing
% the price of participation in a complex coordination system.
domain_priors:base_extractiveness(dldr_information_policy, 0.20).

% Rationale: The policy's entire purpose is to resist content suppression by
% substituting it with informational signaling (tags). The suppression score is
% low, reflecting the open nature of the archive.
domain_priors:suppression_score(dldr_information_policy, 0.15).
domain_priors:theater_ratio(dldr_information_policy, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, 0.20).
narrative_ontology:constraint_metric(dldr_information_policy, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(dldr_information_policy, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
% The system is fundamentally a coordination mechanism (Rope).
narrative_ontology:constraint_claim(dldr_information_policy, rope).
narrative_ontology:human_readable(dldr_information_policy, "\"Don’t Like, Don’t Read\" (DLDR) Information Policy").

% --- Binary flags ---
% The system requires active community participation and platform support for
% the tagging taxonomy to remain functional.
domain_priors:requires_active_enforcement(dldr_information_policy).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(dldr_information_policy, archive_users).
narrative_ontology:constraint_beneficiary(dldr_information_policy, content_creators).
%
% Who bears disproportionate cost?
% The cost is the cognitive load on users accustomed to pre-filtered environments.
narrative_ontology:constraint_victim(dldr_information_policy, users_expecting_sanitization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% This is a uniform-type constraint (Rope-only). The classification is stable
% across all perspectives, demonstrating a robust coordination function. The
% perspectival differences manifest in the calculated chi (χ) value, not the type.

% PERSPECTIVE 1: THE USER ACCUSTOMED TO SANITIZATION (ROPE)
% Agent who bears the most cognitive load. They are a victim of the complexity.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42
% χ ≈ 0.20 * 1.42 * 1.0 (national) = 0.284. This is still well within the Rope
% threshold (χ <= 0.35), showing the system is a coordination tool even for
% those who find it burdensome.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PLATFORM ADMINISTRATOR (ROPE)
% Agent who benefits most. Engine derives d from:
% beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
% For the platform, the system is a highly efficient Rope that outsources
% moderation costs and reduces liability.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ARCHIVE USER (ROPE)
% The intended user who leverages the system for autonomy. They are a beneficiary
% with mobile exit options. The derived d is low, resulting in a low positive χ.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% Default analytical context. Engine derives d ≈ 0.72 -> f(d) ≈ 1.15.
% χ ≈ 0.20 * 1.15 * 1.2 (global) = 0.276. The system is analytically a Rope.
constraint_indexing:constraint_classification(dldr_information_policy, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dldr_information_policy_tests).

test(is_uniform_rope) :-
    % Verify it is a Rope from key perspectives.
    constraint_indexing:constraint_classification(dldr_information_policy, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dldr_information_policy, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(dldr_information_policy, rope, context(agent_power(analytical), _, _, _)).

test(rope_threshold_adherence) :-
    % Verify that base metrics are consistent with a Rope classification.
    narrative_ontology:constraint_metric(dldr_information_policy, extractiveness, E),
    E =< 0.45.

:- end_tests(dldr_information_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file incorrectly claimed this constraint was a Mountain while
 *   assigning metrics (ε=0.35, suppression=0.2) that violated Mountain
 *   thresholds. This regeneration corrects the core model. The DLDR policy is
 *   fundamentally a coordination mechanism (a Rope), not a natural law.
 *
 *   The base extractiveness was lowered to 0.20 to better reflect that the
 *   "cost" (tagging/vetting labor) is a symmetric price of participation, not
 *   asymmetric extraction. The suppression remains low (0.15) as the system's
 *   goal is to enable content, not hide it.
 *
 * PERSPECTIVAL GAP:
 *   The key insight is that this is a uniform-type constraint: it classifies as
 *   a Rope from all perspectives. The perspectival gap is not in the type but
 *   in the *effective extraction (χ)*. For the platform admin (institutional),
 *   χ is negative—the system subsidizes them. For the user accustomed to
 *   sanitization (powerless), χ is positive and higher—it's a functional but
 *   costly tool. For the intended archive user (moderate), χ is low and
 *   positive. This demonstrates a robust Rope that accommodates multiple
 *   relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `archive_users` and `content_creators` gain autonomy and expressive freedom.
 *   - Victim: `users_expecting_sanitization` bear the cognitive cost of adapting to an unfiltered environment. This framing correctly identifies the source of friction in the system without misclassifying the system itself as a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This analysis prevents the mislabeling of a functional coordination system
 *   as something more extractive. The original file's classification of the
 *   powerless perspective as a Snare was a category error; it was classifying
 *   the *alternative* to DLDR (i.e., mainstream sanitization), not DLDR itself.
 *   By correctly modeling DLDR as a robust Rope, we see how coordination can
 *   feel costly to some participants without being structurally extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_dldr_efficacy,
    "Does granular tagging actually protect users (Rope) or does it facilitate trauma-tourism and maladaptive avoidance (Tangled Rope)?",
    "Longitudinal study of user behavior and psychological impact of tag engagement vs. avoidance.",
    "If protective: Rope. If it creates new harms: could drift toward Tangled Rope if platforms exploit the data.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dldr_information_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.20) is below the
% 0.46 threshold for mandatory lifecycle drift tracking.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The DLDR policy is a classic information standard for coordinating user expectations.
narrative_ontology:coordination_type(dldr_information_policy, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships in this system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */