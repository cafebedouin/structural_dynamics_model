% ============================================================================
% CONSTRAINT STORY: bayes_theorem_cognitive_bias
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-28
% ============================================================================

:- module(constraint_bayes_theorem_cognitive_bias, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bayes_theorem_cognitive_bias
 *   human_readable: Bayesian Reasoning Bias (Base Rate Neglect)
 *   domain: social
 *
 * SUMMARY:
 *   This constraint describes the cognitive bias wherein individuals systematically underutilize or ignore base rate information when making judgments under uncertainty, even when provided with relevant conditional probabilities. This leads to inaccurate estimations of the probability of an event.
 *
 * KEY AGENTS (by structural relationship):
 *   - Base-Rate Neglectors: Primary target (powerless/trapped) — suffers from inaccurate judgments
 *   - Manipulators: Primary beneficiary (powerful/arbitrage) — leverages biased reasoning for gain (e.g., advertisers, political actors)
 *   - Experts: Analytical observer (analytical/analytical) — understands the full structure and impact of the bias
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bayes_theorem_cognitive_bias, 0.48).
domain_priors:suppression_score(bayes_theorem_cognitive_bias, 0.50).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(bayes_theorem_cognitive_bias, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, extractiveness, 0.48).
narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, accessibility_collapse, 0.90).
% narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(bayes_theorem_cognitive_bias, tangled_rope).
narrative_ontology:human_readable(bayes_theorem_cognitive_bias, "Bayesian Reasoning Bias (Base Rate Neglect)").
narrative_ontology:topic_domain(bayes_theorem_cognitive_bias, "social").

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(bayes_theorem_cognitive_bias).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(bayes_theorem_cognitive_bias). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(bayes_theorem_cognitive_bias).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(bayes_theorem_cognitive_bias, manipulators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(bayes_theorem_cognitive_bias, base_rate_neglectors).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE/MOUNTAIN)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%
% NOTE: Per "Dynamic Coalition" extension, this agent's power may be
% upgraded to 'organized' if the constraint is a snare with a critical
% mass of victims, potentially changing the classification.
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives. Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% When a constraint operates between institutional actors with different
% structural relationships, declare separate perspectives for each.
% The engine differentiates via directionality: different exit_options
% produce different d values even for the same power atom.
%
% Example — Regulatory capture:
%
% % Perspective 4A: Captured regulator (institutional, constrained exit)
% constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(national))).
%
% % Perspective 4B: Regulated company (institutional, arbitrage exit)
% constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, [type],
%     context(agent_power(institutional),
%             time_horizon(generational),
%             exit_options(arbitrage),
%             spatial_scope(national))).

% PERSPECTIVE 5: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, χ ≤ 0.30, theater ≤ 0.70.
% constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, scaffold,
%     context(agent_power(organized),
%             time_horizon(generational),
%             exit_options(constrained),
%             spatial_scope(continental))) :-
%     narrative_ontology:has_sunset_clause(bayes_theorem_cognitive_bias).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bayes_theorem_cognitive_bias_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bayes_theorem_cognitive_bias, TypeBeneficiary, context(agent_power(powerful), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(bayes_theorem_cognitive_bias, ExtMetricName, E),
    (E =< 0.25 -> true ; E >= 0.46). % Mountain or high-extraction Snare/Tangled.

:- end_tests(bayes_theorem_cognitive_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness score (0.48) reflects the degree to which the bias leads to suboptimal decision-making and resource allocation. The suppression score (0.50) reflects how difficult it is for individuals to overcome this bias, given psychological predispositions and manipulative messaging. The theater ratio (0.10) is low because the bias is a genuine cognitive process, not a performative act.
 *
 * PERSPECTIVAL GAP:
 *   The target (base-rate neglectors) experiences the bias as a "Snare" because it consistently leads to poor judgments and negative consequences, from which escape is difficult. The beneficiary (manipulators) views it as a "Rope" because it allows them to influence behavior effectively and predictably, serving as a reliable tool. The analytical observer sees it as a "Tangled Rope" because it involves both a coordination failure in rational reasoning and asymmetric extraction of value by those who exploit it.
 *
 * DIRECTIONALITY LOGIC:
 *   Base-rate neglectors are the victims because their judgment is systematically skewed, leading to exploitable errors. Manipulators are beneficiaries because they can leverage this predictable error to gain advantages. Advertisers, politicians, and other influencers benefit from the public's susceptibility to this bias.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   N/A. This is primarily a cognitive bias operating at the individual level. However, institutions can contribute to or mitigate this bias through educational efforts or manipulative campaigns.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a "Tangled Rope" acknowledges that it's not simply a failure of individual rationality (which would be a piton) but an exploitable cognitive vulnerability. This prevents it from being mislabeled as a pure failure of coordination (Rope) or simply a natural limitation of human cognition (Mountain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bayes_theorem_cognitive_bias,
    'To what extent is this bias an inherent limitation of human cognition versus a learned behavior?',
    'Controlled experiments measuring susceptibility across different cultures and educational levels.',
    'If inherent, it''s closer to a Mountain; if learned, it''s more malleable as a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bayes_theorem_cognitive_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(bayes_theorem_cognitive_bias_tr_t0, bayes_theorem_cognitive_bias, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bayes_theorem_cognitive_bias_tr_t5, bayes_theorem_cognitive_bias, theater_ratio, 5, 0.12).
narrative_ontology:measurement(bayes_theorem_cognitive_bias_tr_t10, bayes_theorem_cognitive_bias, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(bayes_theorem_cognitive_bias_ex_t0, bayes_theorem_cognitive_bias, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bayes_theorem_cognitive_bias_ex_t5, bayes_theorem_cognitive_bias, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bayes_theorem_cognitive_bias_ex_t10, bayes_theorem_cognitive_bias, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(bayes_theorem_cognitive_bias, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(bayes_theorem_cognitive_bias, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(bayes_theorem_cognitive_bias, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(bayes_theorem_cognitive_bias, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(bayes_theorem_cognitive_bias, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */