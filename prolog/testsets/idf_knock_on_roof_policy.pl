% ============================================================================
% CONSTRAINT STORY: idf_knock_on_roof_policy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_idf_knock_on_roof_policy, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: idf_knock_on_roof_policy
 *   human_readable: IDF "Knock on the Roof" Warning Policy
 *   domain: political
 *
 * SUMMARY:
 *   The "Knock on the Roof" policy is a military tactic used by the Israel
 *   Defense Forces (IDF) in Gaza. It involves dropping a low-yield or
 *   non-explosive munition on the roof of a targeted building to warn
 *   inhabitants to evacuate minutes before a larger, destructive airstrike.
 *   While framed as a measure to minimize civilian casualties, it coexists
 *   with high extraction by placing the full burden of survival on a
 *   powerless population and providing legal/moral justification for
 *   strikes in dense urban environments.
 *
 * KEY AGENTS (by structural relationship):
 *   - Palestinian Civilians in Gaza: Primary target (powerless/trapped) — bear the risk of misinterpretation, inability to flee, and death. The burden of distinction is shifted onto them.
 *   - Israeli Military Command (IDF): Primary beneficiary (institutional/arbitrage) — benefits from the perceived legal compliance, enabling a wider range of targets and managing international reputation.
 *   - International Legal/Humanitarian Bodies: Analytical observer — evaluates the policy against International Humanitarian Law (IHL), seeing both the coordination signal and the coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(idf_knock_on_roof_policy, 0.55).
domain_priors:suppression_score(idf_knock_on_roof_policy, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(idf_knock_on_roof_policy, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, extractiveness, 0.55).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(idf_knock_on_roof_policy, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(idf_knock_on_roof_policy, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(idf_knock_on_roof_policy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(idf_knock_on_roof_policy, israeli_military_command).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(idf_knock_on_roof_policy, palestinian_civilians_in_gaza).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> PASSED

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

% PERSPECTIVE 1: THE PRIMARY TARGET (PALESTINIAN CIVILIAN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% Calculation: χ = 0.55 * 1.42 * 0.8 (local scope) ≈ 0.62. This is on the
% cusp between a high-end Tangled Rope and a low-end Snare (Snare χ ≥ 0.66).
% The classification as Tangled Rope reflects that a (highly coercive)
% coordination signal is still perceived, but it is almost entirely
% overwhelmed by the extractive nature of the choice presented.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (IDF COMMAND)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% Calculation: χ = 0.55 * -0.12 * 0.9 (regional scope) ≈ -0.06.
% From this perspective, the constraint is a pure coordination tool that
% provides legal and reputational benefits at a negative cost.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (UN / HUMAN RIGHTS ORG)
% Default analytical context. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% Sees both the coordination function and the asymmetric extraction.
% Calculation: χ = 0.55 * 1.15 * 1.2 (global scope) ≈ 0.76.
% With ε=0.55, supp=0.85, and χ=0.76, this is a clear Tangled Rope.
constraint_indexing:constraint_classification(idf_knock_on_roof_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(idf_knock_on_roof_policy_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Tangled Rope) and beneficiary (Rope).
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == tangled_rope,
    TypeBeneficiary == rope,
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_gate_validation) :-
    % Verify that all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(idf_knock_on_roof_policy, _),
    narrative_ontology:constraint_victim(idf_knock_on_roof_policy, _),
    domain_priors:requires_active_enforcement(idf_knock_on_roof_policy).

test(analytical_claim_matches_classification) :-
    % Verify the analytical classification matches the declared constraint claim.
    narrative_ontology:constraint_claim(idf_knock_on_roof_policy, ClaimedType),
    constraint_indexing:constraint_classification(idf_knock_on_roof_policy, AnalyticalType, context(agent_power(analytical), _, _, _)),
    ClaimedType == AnalyticalType.

:- end_tests(idf_knock_on_roof_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. The policy extracts significant legal, moral, and strategic value for the enforcer. It manufactures consent for actions that would otherwise face greater international condemnation, effectively shifting the moral hazard for civilian casualties onto the victims.
 *   - Suppression (0.85): Extremely high. For civilians in a targeted building, the only alternative to following the warning is catastrophic. The policy suppresses other interpretations of IHL that might deem the entire strike disproportionate or indiscriminate.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: Rope vs. Tangled Rope.
 *   - The IDF (Beneficiary) experiences it as a Rope: a pure coordination tool that solves a complex legal/ethical problem (how to strike targets in civilian areas) and adds value by enhancing operational freedom. The extractive component is externalized and not perceived.
 *   - The Palestinian Civilian (Target) experiences it as a high-end Tangled Rope, bordering on a Snare. They perceive the coordination signal (the warning), but it is delivered under such extreme coercion and with such a high burden of risk that it feels almost entirely extractive.
 *   - The Analytical Observer sees both functions simultaneously—a genuine warning system coexisting with profound, asymmetric extraction—and thus classifies it as a canonical Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `israeli_military_command`. The policy provides "lawfare" insulation and expands the set of targetable locations. This is the primary structural reason for its existence.
 *   - Victim: `palestinian_civilians_in_gaza`. They bear the entire physical and psychological cost. The constraint forces them into a high-stakes, rapid decision under duress, with failure resulting in death. This clear structural relationship drives the high `d` value for their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful demonstration of how the framework avoids misclassification. A naive analysis might label the policy a Rope ("it's a warning system to save lives") or a Snare ("it's a pretext to kill people"). Deferential Realism correctly identifies it as a Tangled Rope by requiring the analyst to account for BOTH the coordination function (the warning) and the asymmetric extraction (the burden shift and legal justification). It reveals how a mechanism of "care" can be instrumentalized to facilitate violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_idf_knock_on_roof_policy,
    'Is the primary strategic intent of the policy to minimize civilian casualties (coordination) or to provide legal justification for striking otherwise prohibited targets (extraction)?',
    'Access to classified IDF targeting directives, legal reviews, and post-strike casualty analysis.',
    'If primarily for casualty minimization, ε would be lower (~0.35, still Tangled Rope). If primarily for legal cover, ε would be higher (~0.70, becoming a Snare from analytical view).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(idf_knock_on_roof_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the policy's formalization and increased reliance
% over time, indicating extraction_accumulation drift. It began as an ad-hoc
% tactic and evolved into a core component of urban warfare doctrine.

% Theater ratio over time (stable and low, as the policy is functional):
narrative_ontology:measurement(idf_korp_tr_t0, idf_knock_on_roof_policy, theater_ratio, 0, 0.30).
narrative_ontology:measurement(idf_korp_tr_t5, idf_knock_on_roof_policy, theater_ratio, 5, 0.25).
narrative_ontology:measurement(idf_korp_tr_t10, idf_knock_on_roof_policy, theater_ratio, 10, 0.20).

% Extraction over time (shows increasing reliance and burden-shifting):
narrative_ontology:measurement(idf_korp_ex_t0, idf_knock_on_roof_policy, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(idf_korp_ex_t5, idf_knock_on_roof_policy, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(idf_korp_ex_t10, idf_knock_on_roof_policy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It is an enforcement mechanism for a specific, permissive
% interpretation of International Humanitarian Law.
narrative_ontology:coordination_type(idf_knock_on_roof_policy, enforcement_mechanism).

% Network relationships: The policy's effectiveness and coercive power are
% directly dependent on the broader constraint of the Gaza blockade, which
% creates the 'trapped' exit condition for civilians.
narrative_ontology:affects_constraint(gaza_blockade, idf_knock_on_roof_policy).
narrative_ontology:affects_constraint(idf_knock_on_roof_policy, post_conflict_reconstruction).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The structural derivation from
% beneficiary/victim declarations and exit options accurately models the
% power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */