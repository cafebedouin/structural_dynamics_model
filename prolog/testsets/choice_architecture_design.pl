% ============================================================================
% CONSTRAINT STORY: choice_architecture_design
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_choice_architecture_design, []).

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
% See: epsilon_invariance_princi ple.md

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: choice_architecture_design
 *   human_readable: Libertarian Paternalist Nudges
 *   domain: psychological/economic/social
 *
 * SUMMARY:
 *   This constraint models the deliberate design of choice environments to
 *   "nudge" people towards specific, often socially beneficial, outcomes
 *   (e.g., higher retirement savings via opt-out defaults). It functions by
 *   leveraging predictable human cognitive biases. While preserving freedom
 *   of choice in theory, it imposes cognitive costs on those who wish to
 *   deviate from the nudged path, creating an asymmetric extraction.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of 3 stories decomposed from the colloquial label
 * "Choice Architecture". Decomposed because ε differs across observables.
 * Related stories:
 *   - cognitive_bias_unavoidability (ε=0.05, Mountain): The unchangeable fact
 *     that human cognition is biased and some architecture is always present.
 *   - dark_pattern_extraction (ε=0.85, Snare): The malicious use of these
 *     principles for purely extractive purposes (e.g., deceptive subscriptions).
 *
 * KEY AGENTS (by structural relationship):
 *   - Individuals with minority preferences: Primary target (powerless/trapped) —
 *     bear the cognitive cost of opting out of the designed default.
 *   - Choice architects (policymakers, designers): Primary beneficiary
 *     (institutional/arbitrage) — achieve policy goals with low political cost.
 *   - The nudged population: Secondary beneficiary (moderate/mobile) —
 *     achieve better outcomes on average with reduced cognitive load.
 *   - Analytical observer: Sees both the coordination benefit and the
 *     asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(choice_architecture_design, 0.48).
domain_priors:suppression_score(choice_architecture_design, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(choice_architecture_design, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(choice_architecture_design, extractiveness, 0.48).
narrative_ontology:constraint_metric(choice_architecture_design, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(choice_architecture_design, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(choice_architecture_design, tangled_rope).
narrative_ontology:human_readable(choice_architecture_design, "Libertarian Paternalist Nudges").
narrative_ontology:topic_domain(choice_architecture_design, "psychological/economic/social").

% --- Binary flags ---
domain_priors:requires_active_enforcement(choice_architecture_design). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(choice_architecture_design, choice_architects).
narrative_ontology:constraint_beneficiary(choice_architecture_design, nudged_population).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(choice_architecture_design, individuals_with_minority_preferences).

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
% An individual whose preferences run counter to the default. They must exert
% cognitive effort to overcome the nudge. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(choice_architecture_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The choice architect (e.g., policymaker) who achieves a social goal.
% Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(choice_architecture_design, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the genuine coordination function (helping the majority) and the
% asymmetric extraction (cost imposed on the minority).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(choice_architecture_design, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(choice_architecture_design_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(choice_architecture_design, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(choice_architecture_design, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    assertion(TypeTarget == snare),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(analytical_classification) :-
    constraint_indexing:constraint_classification(choice_architecture_design, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_gates) :-
    narrative_ontology:constraint_beneficiary(choice_architecture_design, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(choice_architecture_design, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(choice_architecture_design).

:- end_tests(choice_architecture_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) and suppression (0.55) are set to reflect
 *   a Tangled Rope. The extraction isn't direct financial cost, but the
 *   cognitive cost of overcoming inertia and the potential for suboptimal
 *   outcomes for those who don't fit the "default" profile. Suppression is
 *   moderate because alternatives are available but are made less salient.
 *   The constraint requires active enforcement (the design must be created
 *   and maintained), has clear beneficiaries (the architect and the nudged
 *   majority), and a clear victim group (those who must opt-out).
 *
 * PERSPECTIVAL GAP:
 *   - The 'powerless' individual with minority preferences sees a Snare. The
 *     environment is designed to make their desired choice harder, imposing
 *     a real cost and subtly coercing them towards the default.
 *   - The 'institutional' architect sees a Rope. They are solving a coordination
 *     problem (e.g., getting people to save for retirement) with a tool that
 *     feels minimally coercive and highly effective. The negative extraction
 *     reflects the political and social capital gained.
 *   - The 'analytical' observer sees the full picture: a Tangled Rope that
 *     achieves a coordination goal but does so by imposing an asymmetric cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the architects who achieve policy goals and the majority
 *   of the population who benefit from the default. The victims are the minority
 *   whose preferences are not aligned with the default, forcing them to pay a
 *   cognitive "tax" to exercise their choice. This maps directly to the
 *   structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of nudging. It is
 *   not a pure Rope because it imposes real, asymmetric costs on a subset of
 *   the population. It is not a pure Snare because it has a genuine, often
 *   significant, coordination function that benefits many. The Tangled Rope
 *   classification prevents mislabeling this complex tool as either purely
 *   benevolent coordination or purely malicious extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_choice_architecture_design,
    'Is the line between a benevolent "nudge" (Tangled Rope) and a manipulative "dark pattern" (Snare) empirically measurable or purely a matter of intent?',
    'Comparative analysis of user outcomes and architect incentives in A/B tested choice environments, combined with regulatory definitions (e.g., FTC, EU DSA).',
    'If measurable, a clear boundary can be regulated. If not, the distinction remains a conceptual argument, making all such designs suspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_choice_architecture_design, conceptual, 'Distinguishing benevolent nudges from manipulative dark patterns based on intent vs. outcome.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(choice_architecture_design, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required as base_extractiveness (0.48) > 0.46.
% Models the drift from a purely academic concept to a widely used,
% sometimes co-opted, commercial and policy tool.
%
% Theater ratio over time:
narrative_ontology:measurement(cad_tr_t0, choice_architecture_design, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cad_tr_t5, choice_architecture_design, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cad_tr_t10, choice_architecture_design, theater_ratio, 10, 0.15).

% Extraction over time:
narrative_ontology:measurement(cad_ex_t0, choice_architecture_design, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(cad_ex_t5, choice_architecture_design, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cad_ex_t10, choice_architecture_design, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It standardizes the presentation of choices.
narrative_ontology:coordination_type(choice_architecture_design, information_standard).

% Network relationships: This constraint is enabled by the underlying Mountain
% of cognitive bias, and it can degrade into a Snare of dark patterns.
narrative_ontology:affects_constraint(cognitive_bias_unavoidability, choice_architecture_design).
narrative_ontology:affects_constraint(choice_architecture_design, dark_pattern_extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations accurately models the relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */