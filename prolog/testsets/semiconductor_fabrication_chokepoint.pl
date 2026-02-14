% ============================================================================
% CONSTRAINT STORY: semiconductor_fabrication_chokepoint
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-25
% ============================================================================

:- module(constraint_semiconductor_fabrication_chokepoint, []).

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
 *   constraint_id: semiconductor_fabrication_chokepoint
 *   human_readable: The geopolitical and capital chokepoint of leading-edge semiconductor manufacturing (e.g., TSMC 2nm node)
 *   domain: technological
 *
 * SUMMARY:
 *   The fabrication of leading-edge semiconductors (sub-3nm) requires such
 *   extraordinary capital investment (~$20B+ per fab), specialized knowledge,
 *   and complex supply chains that it is concentrated in a single company
 *   (TSMC) in a geopolitically sensitive region (Taiwan). This creates a
 *   powerful constraint on the global technology ecosystem, acting as both a
 *   vital coordination point and a severe extraction/risk mechanism.
 *
 * KEY AGENTS (by structural relationship):
 *   - Small Hardware Startups: Primary target (powerless/trapped) — bears the full cost of access via high MOQs and has no leverage or alternative.
 *   - Competing Foundries (e.g., Intel, Samsung): Secondary target (organized/constrained) — bears costs of immense R&D and capital competition, often failing to keep pace.
 *   - TSMC and its Key Customers (e.g., Apple, Nvidia): Primary beneficiary (institutional/arbitrage) — benefit from access to the world's most advanced manufacturing, enabling market dominance.
 *   - Western Governments (US/EU): Inter-institutional actor (institutional/constrained) — benefit from their companies' use of TSMC, but are constrained by the geopolitical dependency, prompting massive subsidy programs (e.g., CHIPS Act) to create alternatives.
 *   - Supply Chain Analyst: Analytical observer — sees the full dual nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_fabrication_chokepoint, 0.55).
domain_priors:suppression_score(semiconductor_fabrication_chokepoint, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(semiconductor_fabrication_chokepoint, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, extractiveness, 0.55).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(semiconductor_fabrication_chokepoint, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(semiconductor_fabrication_chokepoint, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(semiconductor_fabrication_chokepoint). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, tsmc_and_fabless_leaders).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, small_hardware_startups).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, competing_foundries).
narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, geopolitically_exposed_nations).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (A SMALL HARDWARE STARTUP)
% A startup is trapped by the immense capital and knowledge barrier, facing
% prohibitive costs and minimum order quantities. For them, the chokepoint is a
% pure extraction mechanism that can kill their business. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SECONDARY TARGET (A COMPETING FOUNDRY)
% A competing foundry like Intel or a smaller firm is trapped by the immense
% capital and knowledge barrier. For them, TSMC's lead is a pure extraction
% mechanism that suppresses their market position. Engine derives high d from
% victim membership + constrained exit.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (A FABLESS LEADER LIKE APPLE/NVIDIA)
% From Apple's perspective, this is a pure coordination mechanism. It provides
% a reliable, cutting-edge platform to build their dominant products on.
% The extraction is a cost of business they can easily bear. Engine derives
% low/negative d from beneficiary membership + arbitrage exit.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees both the coordination function and the asymmetric extraction/risk.
% The classification correctly resolves to Tangled Rope, capturing the duality.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% A Western government sees both sides. Its companies (Apple) benefit, creating
% economic value (Rope aspect). But the nation faces huge systemic risk from
% the supply chain concentration (Snare aspect). The exit option is 'constrained'
% because building a domestic alternative (onshoring) is a decade-long,
% monumentally expensive project.
constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_fabrication_chokepoint_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between a startup (target) and a fabless leader (beneficiary).
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true. % The existence of both clauses passes the test.

test(analytical_view_is_tangled_rope) :-
    % The analytical observer should correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(semiconductor_fabrication_chokepoint, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint can only be a Tangled Rope if it has a coordination function (beneficiary),
    % asymmetric extraction (victim), and requires active enforcement.
    narrative_ontology:constraint_beneficiary(semiconductor_fabrication_chokepoint, _),
    narrative_ontology:constraint_victim(semiconductor_fabrication_chokepoint, _),
    domain_priors:requires_active_enforcement(semiconductor_fabrication_chokepoint).

:- end_tests(semiconductor_fabrication_chokepoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, reflecting the monopoly pricing power, strategic leverage, and the ability to dictate terms to the entire tech ecosystem. This value represents the surplus value captured beyond a competitive market.
 *   - Suppression Score (0.85): Extremely high. The barrier to entry for a competing leading-edge fab is arguably one of the highest in any industry, requiring hundreds of billions in capital, decades of institutional knowledge, and control over a fragile global supply chain (e.g., ASML's EUV machines).
 *   - `requires_active_enforcement`: This is crucial. The lead is not static; it is "enforced" by out-spending and out-innovating all rivals in a relentless cycle, which is a form of active maintenance of the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For a beneficiary like Apple, the constraint is a `Rope`—a predictable, high-performance manufacturing standard that enables their entire business model. For a small startup, it's a `Snare`—a barrier of prohibitive costs and MOQs that prevents them from even entering the market. For a competitor like Intel, it's also a `Snare`—a seemingly insurmountable barrier that extracts market share. For a government, it's a `Tangled Rope`—a source of economic strength for its corporations but also a critical national security vulnerability.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries (`tsmc_and_fabless_leaders`): These entities directly profit from the constraint's existence. Their `arbitrage` exit (using a slightly older, cheaper node) gives them leverage, leading to a low derived `d` value and a Rope classification.
 *   - Victims (`small_hardware_startups`, `competing_foundries`, `geopolitically_exposed_nations`): These entities bear the direct costs (R&D burn, market loss) and systemic risks. Their `trapped` or `constrained` exit options lead to a high derived `d` value and a Snare/Tangled Rope classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The dynamic between a fabless tech giant (Apple) and its host nation (USA) is captured here. Both are `institutional` actors, but their exit options differ. Apple has `arbitrage` (it can choose between nodes/suppliers over a long horizon). The US government has a `constrained` exit (onshoring is a massive, slow, and uncertain policy goal). This difference in exit optionality correctly places them in different perspectival buckets, highlighting the tension between corporate and national interests.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary example of why the Tangled Rope category is essential. Classifying this as a pure `Snare` would ignore the immense, undeniable coordination benefit it provides to the global tech industry. Conversely, classifying it as a `Rope` would dangerously ignore the extreme extraction and geopolitical risk concentration. The Tangled Rope classification provides the necessary nuance, acknowledging that a single structure can be both coordinative and extractive simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_semiconductor_fabrication_chokepoint,
    'Will state-backed subsidies (e.g., US/EU CHIPS Acts) succeed in creating a viable, competitive alternative to TSMC, thereby reducing the suppression score of this constraint?',
    'Observation of market share for leading-edge nodes in 5-10 years. If a non-TSMC fab achieves >20% market share at the n-1 node, the constraint is weakening.',
    'If subsidies fail, the constraint solidifies into a near-permanent Snare from the national perspective. If they succeed, it may diffuse into a more balanced Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(semiconductor_fabrication_chokepoint, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The capital intensity and thus the extractive potential of Moore's Law has
% increased dramatically over the last decade as competitors dropped out.
% Base extraction was lower when more firms could compete at the leading edge.
% Theater ratio remains low as this is a highly functional system.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(sfc_tr_t0, semiconductor_fabrication_chokepoint, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sfc_tr_t5, semiconductor_fabrication_chokepoint, theater_ratio, 5, 0.08).
narrative_ontology:measurement(sfc_tr_t10, semiconductor_fabrication_chokepoint, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(sfc_ex_t0, semiconductor_fabrication_chokepoint, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfc_ex_t5, semiconductor_fabrication_chokepoint, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sfc_ex_t10, semiconductor_fabrication_chokepoint, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The leading-edge fab network is a piece of global digital infrastructure.
narrative_ontology:coordination_type(semiconductor_fabrication_chokepoint, global_infrastructure).

% Network relationships (structural influence edges)
% The capability of this fabrication node directly enables and constrains
% the performance scaling of large AI models.
narrative_ontology:affects_constraint(semiconductor_fabrication_chokepoint, ai_model_scaling_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The structural derivation chain
% (beneficiary/victim declarations + exit options) correctly models the
% directionality for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */