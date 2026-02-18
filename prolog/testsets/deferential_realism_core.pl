% ============================================================================
% CONSTRAINT STORY: deferential_realism_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_deferential_realism_framework, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_framework
 *   human_readable: The Deferential Realism Classification System
 *   domain: epistemological/political
 *
 * SUMMARY:
 *   The Deferential Realism framework itself acts as a constraint on how
 *   information is processed and how truth-claims are validated. It
 *   disambiguates complex socio-political claims by enforcing indexical
 *   specification (P,T,E,S), thereby coordinating analysis across different
 *   perspectives.
 *
 * KEY AGENTS (by structural relationship):
 *   - Universalist Ideologues: Primary target (powerless/trapped) — The framework
 *     suppresses their ability to make un-indexed, universal claims by classifying
 *     them as underspecified.
 *   - System Designers & Analytical Observers: Primary beneficiary (institutional/arbitrage) —
 *     The framework provides a coordination tool for disambiguating complex claims
 *     and resolving apparent paradoxes.
 *   - Policy Reformers: Secondary beneficiary (organized/mobile) — Use the framework
 *     to deconstruct incumbent narratives and propose structurally sound alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_framework, 0.20).
domain_priors:suppression_score(deferential_realism_framework, 0.35).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(deferential_realism_framework, 0.11).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_framework, extractiveness, 0.20).
narrative_ontology:constraint_metric(deferential_realism_framework, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(deferential_realism_framework, theater_ratio, 0.11).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(deferential_realism_framework, rope).
narrative_ontology:human_readable(deferential_realism_framework, "The Deferential Realism Classification System").
narrative_ontology:topic_domain(deferential_realism_framework, "epistemological/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(deferential_realism_framework).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(deferential_realism_framework, analytical_observers).
narrative_ontology:constraint_beneficiary(deferential_realism_framework, policy_reformers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(deferential_realism_framework, universalist_ideologues).

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

% PERSPECTIVE 1: THE UNIVERSALIST IDEOLOGUE (TARGET)
% Experiences the framework as a coercive system that invalidates their worldview.
% Despite the feeling of coercion (high suppression), the actual extraction is low,
% resulting in a Rope classification.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% χ = 0.20 * 1.42 * 1.0 (national) = 0.284. This is below the Rope ceiling (0.35).
constraint_indexing:constraint_classification(deferential_realism_framework, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE SYSTEM DESIGNER (BENEFICIARY)
% Views the framework as a pure coordination tool for disambiguating truth claims.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% χ is negative, indicating a subsidy (the framework creates value for this agent).
constraint_indexing:constraint_classification(deferential_realism_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the framework as a low-extraction coordination mechanism, acknowledging
% its function and its suppressive effect on un-indexed claims.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.20 * 1.15 * 1.2 (global) = 0.276. This is below the Rope ceiling (0.35).
constraint_indexing:constraint_classification(deferential_realism_framework, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_framework_tests).

test(uniform_rope_classification) :-
    % Verify that the framework is a uniform-type Rope from all key perspectives.
    constraint_indexing:constraint_classification(deferential_realism_framework, rope,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_framework, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_framework, rope,
        context(agent_power(analytical), _, _, _)).

test(low_extraction_validation) :-
    % Verify that the base extractiveness is low, consistent with a coordination mechanism.
    narrative_ontology:constraint_metric(deferential_realism_framework, extractiveness, E),
    E < 0.30.

:- end_tests(deferential_realism_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.20) represents the cognitive load required to
 *   use the framework. This is a low but non-zero cost. The suppression score
 *   (0.35) is moderate, reflecting that the framework actively invalidates
 *   un-indexed, universalist claims, which is a form of suppressing a class
 *   of speech. Despite the moderate suppression, the low extraction ensures
 *   the constraint cannot be classified as a Snare from any perspective.
 *   The maximum possible effective extraction (χ) is ~0.34, which is within
 *   the Rope category.
 *
 * PERSPECTIVAL GAP:
 *   There is no gap in classification type; all agents see a Rope. The gap is
 *   in valence. For beneficiaries (designers, analysts), it is a positive-sum
 *   coordination tool. For victims (ideologues), it is a coercive, oppressive
 *   coordination tool that dismantles their worldview. The high suppression
 *   score explains why it *feels* like a Snare to its targets, even though
 *   its extractive cost is too low to qualify.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `analytical_observers` and `policy_reformers` gain a tool
 *     to disambiguate complex systems, giving them an advantage.
 *   - Victims: `universalist_ideologues` lose the ability to make claims that
 *     rely on indexical ambiguity for their power. The framework extracts this
 *     rhetorical power from them.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates how a constraint can be coercive (moderate suppression)
 *   without being highly extractive. Classifying it as a uniform Rope prevents
 *   the mislabeling of its suppressive function as pure extraction. It correctly
 *   identifies the mechanism as coordination, while acknowledging (via the
 *   suppression score and victim declaration) that this coordination has
 *   asymmetric costs and is not benign for all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_dr_adoption,
    'Will the framework be adopted voluntarily (Rope) or enforced by informational gatekeepers (Tangled Rope/Snare)?',
    'Monitor the ratio of grassroots adoption vs. institutional mandatory audit implementation over time.',
    'If enforced by institutions, its base extractiveness could increase as it becomes a tool for gatekeeping, potentially transforming it into a Tangled Rope.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_dr_adoption, empirical, 'The ratio of voluntary vs. enforced adoption will determine the constraint''s future classification.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(deferential_realism_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The framework's metrics are modeled as stable over its initial lifecycle.
% Extraction is low, so this section is for good practice rather than requirement.
%
% Theater ratio over time:
narrative_ontology:measurement(dr_framework_tr_t0, deferential_realism_framework, theater_ratio, 0, 0.11).
narrative_ontology:measurement(dr_framework_tr_t5, deferential_realism_framework, theater_ratio, 5, 0.11).
narrative_ontology:measurement(dr_framework_tr_t10, deferential_realism_framework, theater_ratio, 10, 0.11).

% Extraction over time:
narrative_ontology:measurement(dr_framework_ex_t0, deferential_realism_framework, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(dr_framework_ex_t5, deferential_realism_framework, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(dr_framework_ex_t10, deferential_realism_framework, base_extractiveness, 10, 0.20).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The framework is a standard for structuring and validating information.
narrative_ontology:coordination_type(deferential_realism_framework, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */