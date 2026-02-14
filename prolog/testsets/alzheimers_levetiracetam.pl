% ============================================================================
% CONSTRAINT STORY: alzheimers_levetiracetam
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_alzheimers_levetiracetam, []).

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
 *   constraint_id: alzheimers_levetiracetam
 *   human_readable: Levetiracetam as Alzheimer's preventative
 *   domain: social
 *
 * SUMMARY:
 *   The off-label use of levetiracetam, a common anti-seizure drug, to prevent or treat Alzheimer's is proposed. This constraint represents the potential benefit and risks associated with widespread preventative use of a drug with known side effects. The constraint arises from balancing potential cognitive benefits against the extraction risks of side effects and the financial costs of the drug to individuals and healthcare systems, especially when its efficacy for this purpose is not yet definitively proven.
 *
 * KEY AGENTS (by structural relationship):
 *   - Vulnerable individuals at risk of Alzheimer's: Primary target (powerless/trapped) — bear extraction of side effects and financial cost with few alternatives.
 *   - General population at risk of Alzheimer's: Secondary target (moderate/constrained) - can weigh options more freely.
 *   - Pharmaceutical companies: Primary beneficiary (institutional/arbitrage) — benefits from increased drug sales.
 *   - Healthcare systems: Secondary actor (institutional/constrained) - constrained by budget and efficacy of intervention.
 *   - Analytical observer: Analytical observer (analytical/analytical) — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alzheimers_levetiracetam, 0.35).
domain_priors:suppression_score(alzheimers_levetiracetam, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(alzheimers_levetiracetam, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alzheimers_levetiracetam, extractiveness, 0.35).
narrative_ontology:constraint_metric(alzheimers_levetiracetam, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(alzheimers_levetiracetam, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(alzheimers_levetiracetam, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(alzheimers_levetiracetam). % Requires prescription/medical system enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(alzheimers_levetiracetam, pharmaceutical_companies).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(alzheimers_levetiracetam, individuals_at_risk).

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

% PERSPECTIVE 1A: THE VULNERABLE TARGET (SNARE)
% Agent who bears the most extraction, with no alternatives. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(alzheimers_levetiracetam, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 1B: THE GENERAL AT-RISK INDIVIDUAL (TANGLED ROPE)
% An individual with more resources and options, who can weigh the pros and cons.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(alzheimers_levetiracetam, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(alzheimers_levetiracetam, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES (declare when applicable) ---
% The healthcare system bears some of the cost in terms of prescription coverage, but also benefits from potential savings in long-term care costs if the intervention is effective.
%
% Perspective 4: Healthcare system (institutional, constrained exit)
constraint_indexing:constraint_classification(alzheimers_levetiracetam, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alzheimers_levetiracetam_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the most vulnerable target and the beneficiary.
    constraint_indexing:constraint_classification(alzheimers_levetiracetam, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alzheimers_levetiracetam, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(tangled_rope_metric_validation) :-
    % Verify metrics are consistent with a Tangled Rope classification.
    narrative_ontology:constraint_metric(alzheimers_levetiracetam, extractiveness, E),
    narrative_ontology:constraint_metric(alzheimers_levetiracetam, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(alzheimers_levetiracetam_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set at 0.35 to represent the risk of side effects and financial cost associated with levetiracetam use. The suppression score is raised to 0.45 to reflect the reality that for individuals with strong genetic predispositions or early symptoms, the lack of proven, widely available pharmaceutical alternatives creates significant pressure to adopt this treatment, effectively suppressing other options. These metrics, along with the need for medical system enforcement, meet the criteria for a Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   Individuals who are powerless and trapped by their condition perceive this as a snare due to the direct risks they bear with no real alternative. Pharmaceutical companies view it as a rope - a coordination mechanism to supply preventative medicine for profit. The analytical observer sees the hybrid nature of a genuine coordination function (potential medical benefit) and asymmetric extraction (side effects, cost), thus classifying it as a tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical companies benefit from increased sales of levetiracetam, hence declared beneficiary. Individuals at risk of Alzheimer's bear the costs of potential side effects and financial burden, hence declared victim. The derived 'd' values reflect these structural relationships.
 *
 * INTER-INSTITUTIONAL DYNAMICS (if applicable):
 *   The healthcare system, as an institutional actor, must balance the costs of covering levetiracetam prescriptions against the potential benefits of preventing Alzheimer's. The exit option 'constrained' represents the limited budgetary flexibility and policy inertia of healthcare systems.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope, rather than a pure rope or snare, prevents mislabeling. It acknowledges both the coordination function (potential preventative medicine) and the inherent extraction (side effects, financial cost).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_alzheimers_levetiracetam,
    'Is levetiracetam genuinely effective as a preventative measure for Alzheimer\'s?',
    'Large-scale, long-term, double-blind clinical trials.',
    'If true, reduces extraction and shifts towards rope; if false, it is a pure snare for patients.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(alzheimers_levetiracetam, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. Since base_extractiveness < 0.46, this
% is not strictly required, but included to model the potential evolution of
% the constraint as more data becomes available.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(alzheimers_levetiracetam_tr_t0, alzheimers_levetiracetam, theater_ratio, 0, 0.05).
narrative_ontology:measurement(alzheimers_levetiracetam_tr_t5, alzheimers_levetiracetam, theater_ratio, 5, 0.10).
narrative_ontology:measurement(alzheimers_levetiracetam_tr_t10, alzheimers_levetiracetam, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(alzheimers_levetiracetam_ex_t0, alzheimers_levetiracetam, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(alzheimers_levetiracetam_ex_t5, alzheimers_levetiracetam, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(alzheimers_levetiracetam_ex_t10, alzheimers_levetiracetam, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(alzheimers_levetiracetam, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% status and exit options accurately models the directionality for each agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */