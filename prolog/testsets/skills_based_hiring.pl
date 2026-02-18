% ============================================================================
% CONSTRAINT STORY: skills_based_hiring
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_skills_based_hiring, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: skills_based_hiring
 *   human_readable: Skills-Based Hiring (De-credentialing)
 *   domain: economic/technological/social
 *
 * SUMMARY:
 *   Skills-based hiring is a matching market shift where employers prioritize
 *   specific, verifiable competencies over traditional degree credentials.
 *   This moves the bottleneck from "Institutional Pedigree" (College Match)
 *   to "Granular Verification" (Skill Match), replacing a high-cost,
 *   one-time filter with a lower-cost, continuous evaluation system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mid-tier college graduates: Primary target (powerless/trapped) — their primary signaling asset (degree) is devalued, forcing them to compete on granular skills where they may not have an advantage.
 *   - Self-taught experts & bootcamp graduates: Primary beneficiary (moderate/mobile) — gain access to high-wage labor markets previously gated by expensive credentials.
 *   - Employers & hiring platforms: Secondary beneficiary (institutional/arbitrage) — gain a wider, more accurately-assessed talent pool and reduce reliance on noisy proxies like university prestige.
 *   - Analytical observer: Sees the full structure of the market shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Lower than traditional credentialism (ε~0.5+) as it reduces the high upfront
% cost of a 4-year degree, but non-zero due to costs of certification platforms,
% continuous testing, and potential for wage suppression via increased competition.
domain_priors:base_extractiveness(skills_based_hiring, 0.25).
% Low suppression; it explicitly creates and validates alternative pathways
% to employment (bootcamps, self-teaching, micro-credentials).
domain_priors:suppression_score(skills_based_hiring, 0.2).
% Low theater; the system is highly functional, focused on direct assessment
% of productive capability rather than performative signaling.
domain_priors:theater_ratio(skills_based_hiring, 0.11).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, 0.25).
narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(skills_based_hiring, theater_ratio, 0.11).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(skills_based_hiring, rope).
narrative_ontology:human_readable(skills_based_hiring, "Skills-Based Hiring (De-credentialing)").
narrative_ontology:topic_domain(skills_based_hiring, "economic/technological/social").

% --- Binary flags ---
% The shift requires active enforcement by HR departments and hiring platforms
% to ignore degree proxies and implement skills-first evaluation frameworks.
% This declaration resolves a potential SCAFFOLD_DANGER_ZONE lint error.
domain_priors:requires_active_enforcement(skills_based_hiring).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(skills_based_hiring, self_taught_experts).
narrative_ontology:constraint_beneficiary(skills_based_hiring, employers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(skills_based_hiring, mid_tier_liberal_arts_colleges).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE DEGREE-HOLDER WITH DEVALUED CREDENTIALS (ROPE)
% Agent whose primary signaling asset is weakened. While their experience is
% negative (increased competition, potential wage suppression), the underlying
% structure is still a low-suppression coordination mechanism. The metrics
% (ε=0.25, S=0.2) do not support a Snare or Tangled Rope classification.
% χ = 0.25 * f(d≈0.95) * σ(national=1.0) ≈ 0.25 * 1.42 * 1.0 = 0.355.
% This χ is on the Rope/Tangled Rope boundary, but low suppression (0.2)
% makes it a Rope.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SELF-TAUGHT EXPERT (ROPE)
% Agent who gains access to previously gated markets. For this beneficiary,
% the system is a pure coordination mechanism with low overhead.
% χ will be negative due to beneficiary status and mobile exit options.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE EMPLOYER / HIRING PLATFORM (ROPE)
% The institutional beneficiary who uses the mechanism to optimize talent
% matching. The constraint is a service that reduces search costs.
% χ will be negative due to beneficiary status and arbitrage exit options.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% The analyst sees a global shift in labor market coordination.
% χ = 0.25 * f(d≈0.72) * σ(global=1.2) ≈ 0.25 * 1.15 * 1.2 = 0.345.
% This is below the Rope ceiling (χ ≤ 0.35), confirming the Rope classification.
constraint_indexing:constraint_classification(skills_based_hiring, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skills_based_hiring_tests).

test(is_uniform_rope) :-
    % Verify that the constraint is a uniform-type Rope from all key perspectives.
    constraint_indexing:constraint_classification(skills_based_hiring, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(skills_based_hiring, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(skills_based_hiring, rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify base metrics are within expected ranges for a Rope.
    narrative_ontology:constraint_metric(skills_based_hiring, extractiveness, E),
    narrative_ontology:constraint_metric(skills_based_hiring, suppression_requirement, S),
    E =< 0.45,
    S < 0.40.

:- end_tests(skills_based_hiring_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε) was set to 0.25 to reflect a system that
 *   significantly lowers the high entry cost of a traditional degree (ε > 0.5)
 *   but introduces new, smaller costs via certification platforms and
 *   continuous testing. Suppression is low (0.2) because the system's
 *   primary function is to create and validate alternative career paths.
 *   The original file's classification of 'Snare' for the victim and 'Mountain'
 *   for the institution were inconsistent with these metrics. The corrected
 *   analysis shows it is a uniform-type Rope, though one with negative
 *   consequences for those whose assets (degrees) are devalued by the new
 *   coordination standard.
 *
 * PERSPECTIVAL GAP:
 *   There is no gap in classification type; all agents perceive a Rope. The gap
 *   is in valence. For beneficiaries (self-taught experts, employers), it is a
 *   liberating Rope that provides access and efficiency. For victims (holders
 *   of devalued credentials), it is an oppressive Rope that increases
 *   competition and uncertainty, even though its structural extractiveness and
 *   suppression are low.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are `self_taught_experts` and `employers`, who gain access
 *   to new opportunities and talent pools. The primary victim group is
 *   `mid_tier_liberal_arts_colleges` and their graduates, whose primary
 *   signaling mechanism is devalued. This structural relationship drives the
 *   directionality `d`, leading to a positive (but low) effective
 *   extraction (χ) for victims and a negative χ for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a classic case where a negative experience could be mislabeled as
 *   a Snare. The framework correctly identifies it as a Rope by focusing on
 *   the low structural suppression and moderate extraction. The negative
 *   experience of the victim is captured by the positive χ value from their
 *   perspective, without misclassifying the entire constraint. The addition of
 *   `requires_active_enforcement` prevents a misclassification as a Scaffold,
 *   as the constraint is an ongoing market rule, not a temporary support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_skills_based_hiring,
    'How fast will AI-driven tools make specific human skills obsolete, potentially turning this Rope into a Snare of continuous re-skilling?',
    'Tracking the half-life of technical certifications and wage premiums for specific skills.',
    'If skill half-life is short, the coordination benefit is outweighed by re-skilling costs (increasing ε), shifting it to a Tangled Rope or Snare. If long, it remains a stable Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_skills_based_hiring, empirical, 'Rate of skill obsolescence and its impact on the long-term extractiveness of continuous certification.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(skills_based_hiring, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is low (<= 0.46), so temporal measurements are not
% strictly required by the linter.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
% narrative_ontology:coordination_type(skills_based_hiring, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */