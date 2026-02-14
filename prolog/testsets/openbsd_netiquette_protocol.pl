% ============================================================================
% CONSTRAINT STORY: openbsd_netiquette_protocol
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_openbsd_netiquette_protocol, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: openbsd_netiquette_protocol
 *   human_readable: OpenBSD Mailing List Netiquette Protocol
 *   domain: technological/social
 *
 * SUMMARY:
 *   This constraint represents the rigorous online communication standards for
 *   OpenBSD mailing lists, which prioritize content clarity and technical
 *   preparation over formatting and social convenience. It mandates that
 *   contributors "do their homework" (research, provide logs, etc.) and strip
 *   away formatting distractions (HTML, top-posting) to facilitate efficient
 *   technical discussion. The protocol acts as a strong filter, coordinating
 *   expert communication while extracting significant preparatory labor from
 *   all participants, especially newcomers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Newcomers/Casual Contributors: Primary target (powerless/trapped) — bear the extraction of preparatory "homework" and risk being ignored or rebuked for non-compliance.
 *   - Core Developers/Maintainers: Primary beneficiary (institutional/arbitrage) — benefit from high-signal, low-noise communication that conserves their attention.
 *   - Experienced Contributors: Secondary target/beneficiary (moderate/mobile) — understand and benefit from the rules but must still perform the extracted labor.
 *   - Analytical Observer: Sees the full dual-function structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: High. The "homework tax" is significant. It extracts substantial
% preparatory labor, log analysis, and formatting effort before a question is
% even considered. This conserves the collective attention of the group at the
% direct expense of the individual contributor.
domain_priors:base_extractiveness(openbsd_netiquette_protocol, 0.60).

% Rationale: High. The protocol explicitly suppresses common communication modes
% like HTML email, top-posting, and casual social chatter. Alternatives are
% actively rejected to maintain the integrity of the information standard.
domain_priors:suppression_score(openbsd_netiquette_protocol, 0.60).

% Rationale: Very low. The protocol is almost entirely functional. The rules
% directly serve the goal of technical clarity with minimal performative overhead.
domain_priors:theater_ratio(openbsd_netiquette_protocol, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, extractiveness, 0.60).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(openbsd_netiquette_protocol, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(openbsd_netiquette_protocol, tangled_rope).

% --- Binary flags ---
% Required for Tangled Rope. The norms are actively enforced by moderators and
% community members who will ignore or correct non-compliant posts.
domain_priors:requires_active_enforcement(openbsd_netiquette_protocol).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(openbsd_netiquette_protocol, core_developers_and_maintainers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(openbsd_netiquette_protocol, newcomers_and_casual_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE NEWCOMER (PRIMARY TARGET)
% For a newcomer unfamiliar with the rules, the protocol is a Snare. Their
% request for help is "strangled" by the high preparatory requirements, and
% non-compliance leads to being ignored (social liquidation), extracting their
% time and effort with no return.
% Math: χ = 0.60 * f(d=0.95) * σ(local=0.8) ≈ 0.60 * 1.42 * 0.8 ≈ 0.68 (Snare)
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE CORE DEVELOPER (PRIMARY BENEFICIARY)
% For the maintainers, the protocol is a pure Rope. It coordinates communication
% efficiently, filtering out noise and ensuring they only see well-researched,
% clearly articulated issues. The extractive cost is borne by others.
% Math: χ = 0.60 * f(d=0.05) * σ(national=1.0) ≈ 0.60 * -0.12 * 1.0 ≈ -0.07 (Rope)
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXPERIENCED CONTRIBUTOR
% For a regular, experienced contributor, the protocol is a Tangled Rope. They
% benefit from the coordination (Rope aspect) but must still perform the
% significant "homework" labor (Snare aspect) for every contribution. They
% experience both the coordination function and the asymmetric extraction.
% Math: χ = 0.60 * f(d=0.85) * σ(global=1.2) ≈ 0.60 * 1.15 * 1.2 ≈ 0.83 (Tangled Rope)
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analytical observer sees the complete structure: a system with a genuine
% coordination function that is sustained by high, asymmetric extraction and
% active enforcement. This is the definition of a Tangled Rope.
% Math: χ = 0.60 * f(d=0.72) * σ(global=1.2) ≈ 0.60 * 1.15 * 1.2 ≈ 0.83 (Tangled Rope)
constraint_indexing:constraint_classification(openbsd_netiquette_protocol, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openbsd_netiquette_protocol_tests).

test(perspectival_gap) :-
    % Verify the gap between the newcomer (target) and core dev (beneficiary).
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, snare, context(agent_power(powerless), _, trapped, _)),
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, rope, context(agent_power(institutional), _, arbitrage, _)),
    constraint_indexing:constraint_classification(openbsd_netiquette_protocol, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base metrics align with a high-extraction Tangled Rope/Snare.
    narrative_ontology:constraint_metric(openbsd_netiquette_protocol, extractiveness, E),
    narrative_ontology:constraint_metric(openbsd_netiquette_protocol, suppression_requirement, S),
    E >= 0.46,
    S >= 0.40.

:- end_tests(openbsd_netiquette_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file's metrics (ε=0.3) were too low to be consistent with its
 *   own narrative of a "Snare" for newcomers. The base extractiveness was
 *   increased to 0.60 to reflect the significant "homework tax" the protocol
 *   imposes on all contributors. This tax is not a side effect; it is the
 *   primary mechanism that makes the coordination function work for the
 *   beneficiaries. The suppression score of 0.60 reflects the explicit and
 *   active rejection of common communication standards (HTML, top-posting).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the core developers (beneficiaries), the system is
 *   a perfect Rope, delivering high-quality information with minimal effort on
 *   their part. For the newcomer (target), it is a Snare, demanding immense
 *   upfront work with a high risk of failure and social rebuke. The experienced
 *   contributor and the analytical observer see the true nature: a Tangled Rope
 *   where the valuable coordination is inextricably linked to, and paid for by,
 *   the coercive extraction of labor.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `core_developers_and_maintainers`. They receive the benefit
 *     (clarity, saved time) without bearing the full cost. Their institutional
 *     power and arbitrage exit options give them a low directionality (d).
 *   - Victims: `newcomers_and_casual_contributors`. They bear the full cost of
 *     the "homework tax" and risk social liquidation. Their powerless status
 *     and trapped exit options (to get help, they must comply) give them a
 *     high directionality (d).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the constraint as a Tangled Rope,
 *   avoiding two potential errors. It is not a pure Rope, because the
 *   coordination is funded by significant, asymmetric extraction. It is not a
 *   pure Snare, because it possesses a genuine and highly effective coordination
 *   function for its beneficiaries. The `tangled_rope` classification captures
 *   this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_openbsd_netiquette_protocol,
    "Is the 'homework' requirement a functional necessity for clarity (Tangled Rope) or an elitist tool for social gatekeeping whose primary function is exclusion (Snare)?",
    "Audit of community growth vs. problem-resolution speed in 'high-homework' vs 'low-homework' technical communities.",
    "If necessity: A functional Tangled Rope. If gatekeeping: A pure Snare masquerading as a coordination tool.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openbsd_netiquette_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is required as base_extractiveness > 0.46.
% These norms are stable within established technical communities, so we model
% a flat trajectory over the interval.

% Theater ratio over time:
narrative_ontology:measurement(onp_tr_t0, openbsd_netiquette_protocol, theater_ratio, 0, 0.05).
narrative_ontology:measurement(onp_tr_t5, openbsd_netiquette_protocol, theater_ratio, 5, 0.05).
narrative_ontology:measurement(onp_tr_t10, openbsd_netiquette_protocol, theater_ratio, 10, 0.05).

% Extraction over time:
narrative_ontology:measurement(onp_ex_t0, openbsd_netiquette_protocol, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(onp_ex_t5, openbsd_netiquette_protocol, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(onp_ex_t10, openbsd_netiquette_protocol, base_extractiveness, 10, 0.60).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The protocol's primary function is to standardize communication for clarity.
narrative_ontology:coordination_type(openbsd_netiquette_protocol, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics of this constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */