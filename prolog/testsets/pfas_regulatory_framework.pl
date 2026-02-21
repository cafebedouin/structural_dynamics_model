% ============================================================================
% CONSTRAINT STORY: pfas_regulatory_framework
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_pfas_regulatory_framework, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pfas_regulatory_framework
 *   human_readable: Permissive Regulatory Framework for PFAS Chemicals in Consumer Products
 *   domain: economic/political
 *
 * SUMMARY:
 *   Despite growing evidence of significant health risks (cancer, immune system damage),
 *   per- and polyfluoroalkyl substances (PFAS), known as "forever chemicals," remain
 *   prevalent in consumer goods like food packaging and, consequently, food itself.
 *   This constraint represents the regulatory environment that permits their use,
 *   effectively externalizing long-term health and environmental costs from
 *   manufacturers onto the general public. The system has a coordination function
 *   (providing industry with cheap, effective chemicals) but is dominated by
 *   asymmetric extraction (public health degradation).
 *
 * KEY AGENTS (by structural relationship):
 *   - European Consumers: Primary target (powerless/trapped) — involuntarily ingest PFAS and bear the long-term health costs.
 *   - Chemical & Food Producers: Primary beneficiary (institutional/arbitrage) — benefit from the performance and low cost of PFAS, with liability externalized.
 *   - European Regulators (e.g., EFSA): Inter-institutional actor (institutional/constrained) — tasked with public safety but constrained by industrial lobbying, scientific complexity, and political inertia, resulting in slow and inadequate action.
 *   - Analytical Observer: Analytical (analytical/analytical) — sees the full structure of industrial benefit, public harm, and regulatory failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pfas_regulatory_framework, 0.78). % High extraction of public health value and future cleanup costs.
domain_priors:suppression_score(pfas_regulatory_framework, 0.85).   % Structural property (raw, unscaled). High due to industrial lobbying and lack of viable, labeled alternatives for consumers.
domain_priors:theater_ratio(pfas_regulatory_framework, 0.40).       % Piton detection (>= 0.70). Regulations exist but are ineffective, indicating a significant theatrical component.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pfas_regulatory_framework, extractiveness, 0.78).
narrative_ontology:constraint_metric(pfas_regulatory_framework, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(pfas_regulatory_framework, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(pfas_regulatory_framework, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(pfas_regulatory_framework). % Required for Tangled Rope. The *failure* to regulate more strictly is the active enforcement mechanism.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(pfas_regulatory_framework, chemical_and_food_producers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(pfas_regulatory_framework, european_consumers).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are met).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function.
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: continental=1.1, global=1.2.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% European consumers who are trapped into consuming contaminated products.
% Engine derives: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.78 * f(0.95) * σ(continental) = 0.78 * 1.42 * 1.1 ≈ 1.21. This is a clear Snare.
constraint_indexing:constraint_classification(pfas_regulatory_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Producers who benefit from using cheap, effective chemicals.
% Engine derives: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ = 0.78 * f(0.05) * σ(continental) = 0.78 * -0.12 * 1.1 ≈ -0.10. This is a pure Rope.
constraint_indexing:constraint_classification(pfas_regulatory_framework, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Sees both the coordination function for industry and the extraction from the public.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.78 * f(0.72) * σ(global) = 0.78 * 1.15 * 1.2 ≈ 1.07. Meets Tangled Rope/Snare χ.
% The presence of a beneficiary (coordination) and victim (extraction) + enforcement
% leads to the Tangled Rope classification.
constraint_indexing:constraint_classification(pfas_regulatory_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE INTER-INSTITUTIONAL REGULATOR (TANGLED ROPE)
% The European Food Safety Authority (EFSA) is an institutional actor but is
% constrained by political and industrial pressure. They cannot easily exit the
% current framework. Their 'constrained' exit option results in a directionality
% value between the beneficiary and victim, reflecting their conflicted position.
constraint_indexing:constraint_classification(pfas_regulatory_framework, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pfas_regulatory_framework_tests).

test(perspectival_gap_consumer_vs_producer) :-
    constraint_indexing:constraint_classification(pfas_regulatory_framework, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(pfas_regulatory_framework, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: Snare (powerless) vs. Rope (institutional)~n').

test(analytical_claim_matches_tangled_rope) :-
    constraint_indexing:constraint_classification(pfas_regulatory_framework, tangled_rope,
        context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(pfas_regulatory_framework, tangled_rope).

test(tangled_rope_structural_gates_met) :-
    narrative_ontology:constraint_beneficiary(pfas_regulatory_framework, _),
    narrative_ontology:constraint_victim(pfas_regulatory_framework, _),
    domain_priors:requires_active_enforcement(pfas_regulatory_framework).

:- end_tests(pfas_regulatory_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.78): Represents the severe, long-term, and widespread negative externality of public health degradation and future environmental cleanup costs. This is not a minor financial transfer but a fundamental extraction of well-being.
 *   - Suppression (S=0.85): Consumers have virtually no way to avoid PFAS. It is unlabeled, ubiquitous, and alternatives are not readily available. Furthermore, intense industry lobbying actively suppresses stricter regulations, which are the only viable alternative at scale.
 *   - The classification is Tangled Rope because the system possesses both a genuine (if perverse) coordination function and a massive extractive component. The coordination allows producers to standardize on cheap, high-performance materials. The extraction is the offloading of systemic risk onto an unaware public.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For Chemical & Food Producers (beneficiaries with arbitrage exit), the framework is a Rope—a useful, cost-reducing coordination tool. For Consumers (victims with trapped exit), it is a Snare—an inescapable trap that damages their health for others' profit. This massive divergence in classification is characteristic of highly extractive Tangled Ropes.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The regulator's perspective is crucial. As an 'institutional' actor with 'constrained' exit, they don't experience the pure benefit of the producer (arbitrage) nor the pure harm of the consumer (trapped). They are caught in the middle, managing a system they know is harmful but which they cannot easily dismantle due to political and economic capture. Their experience is that of a Tangled Rope, a messy compromise where coordination and extraction are inseparably bound.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the problem. A simpler analysis might label it a pure Snare, ignoring the industrial logic that keeps it in place. Another might mislabel it a Rope, focusing only on the "benefits" to industry while ignoring the externalized costs. The Tangled Rope classification captures the reality: a system of *coordinated extraction*, where the coordination function serves to make the extraction more efficient and resilient. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_pfas_regulatory_framework,
    'Is the regulatory failure due to deliberate industrial capture or to institutional inertia and the sheer complexity of regulating thousands of chemicals?',
    'Internal documents from regulatory bodies and lobbying firms; comparative analysis with regulatory systems that successfully banned similar chemical classes.',
    'If deliberate capture, the system is fundamentally a Snare with theatrical Rope elements. If inertia/complexity, it is a Tangled Rope degrading into a Piton.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_pfas_regulatory_framework, empirical, 'Distinguishing between deliberate industrial capture and institutional inertia as the primary driver of regulatory failure.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(pfas_regulatory_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time as production of PFAS scaled and
% scientific evidence of harm accumulated, while regulation failed to keep pace.
% This represents a classic extraction_accumulation drift.

% Theater ratio over time (initial weak regulations gave way to more performative ones):
narrative_ontology:measurement(pfas_tr_t0, pfas_regulatory_framework, theater_ratio, 0, 0.10).
narrative_ontology:measurement(pfas_tr_t5, pfas_regulatory_framework, theater_ratio, 5, 0.25).
narrative_ontology:measurement(pfas_tr_t10, pfas_regulatory_framework, theater_ratio, 10, 0.40).

% Extraction over time (scale of contamination and known health costs have increased):
narrative_ontology:measurement(pfas_ex_t0, pfas_regulatory_framework, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pfas_ex_t5, pfas_regulatory_framework, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(pfas_ex_t10, pfas_regulatory_framework, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The coordination function is to provide a standardized, permissible set of
% materials for industrial processes.
narrative_ontology:coordination_type(pfas_regulatory_framework, information_standard).

% This regulatory failure erodes public trust in adjacent systems.
narrative_ontology:affects_constraint(pfas_regulatory_framework, public_trust_in_food_safety).
narrative_ontology:affects_constraint(pfas_regulatory_framework, chemical_industry_liability_shield).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation from
% beneficiary/victim declarations and exit options (trapped, arbitrage, constrained)
% accurately models the directionality for each key agent.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */