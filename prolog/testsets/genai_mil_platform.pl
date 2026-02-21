% ============================================================================
% CONSTRAINT STORY: genai_mil_platform
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_genai_mil_platform, []).

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
 *   constraint_id: genai_mil_platform
 *   human_readable: "Mandatory Use of the GenAI.mil Platform for Defense Intelligence Analysis"
 *   domain: technological
 *
 * SUMMARY:
 *   A Department of Defense (DoD) directive mandates that all intelligence analysts
 *   use a centralized, contractor-provided Generative AI platform (GenAI.mil) for
 *   creating and disseminating reports. The platform is intended to accelerate
 *   analysis and standardize outputs (coordination), but it also creates deep
 *   dependency on a single vendor, deskills analysts by automating core tasks, and
 *   allows for centralized monitoring of analytical work (extraction).
 *
 * KEY AGENTS (by structural relationship):
 *   - DoD Intelligence Analysts: Primary target (powerless/trapped) — forced to use the system, which devalues their skills and subjects them to monitoring.
 *   - The Platform Contractor: Primary beneficiary (institutional/arbitrage) — secures a lucrative, long-term monopoly contract and controls the technological ecosystem.
 *   - DoD Leadership: Secondary beneficiary (institutional/constrained) — achieves perceived efficiency and standardization but becomes dependent on the contractor with high switching costs.
 *   - Analytical Observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genai_mil_platform, 0.48).
domain_priors:suppression_score(genai_mil_platform, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(genai_mil_platform, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genai_mil_platform, extractiveness, 0.48).
narrative_ontology:constraint_metric(genai_mil_platform, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(genai_mil_platform, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(genai_mil_platform, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(genai_mil_platform). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(genai_mil_platform, platform_contractor).
narrative_ontology:constraint_beneficiary(genai_mil_platform, dod_leadership).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(genai_mil_platform, dod_analysts).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The DoD Analyst. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.48 * 1.42 * 1.0 = 0.68. This exceeds the Snare threshold (χ ≥ 0.66).
constraint_indexing:constraint_classification(genai_mil_platform, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Platform Contractor. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
%   χ = 0.48 * -0.12 * 1.0 = -0.0576. Clearly a Rope.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. This is the basis for constraint_claim/2.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
%   χ = 0.48 * 1.15 * 1.2 (global scope) = 0.6624.
% This meets the χ threshold for a Snare, but because a coordination function
% is present (beneficiary declared), the classifier selects Tangled Rope.
constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% Differentiating between the two institutional beneficiaries.

% Perspective 4A: DoD Leadership (Constrained Beneficiary)
% Beneficiary status with constrained exit raises d above pure arbitrage.
% d derivation for (beneficiary, constrained) gives a value between 0.2 and 0.4.
% Let's assume d ≈ 0.3 -> f(d) ≈ 0.17.
% χ = 0.48 * 0.17 * 1.0 = 0.0816. Still a Rope, but with a positive χ,
% reflecting the risks and lock-in they are accepting.
constraint_indexing:constraint_classification(genai_mil_platform, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genai_mil_platform_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(genai_mil_platform, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(genai_mil_platform, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(genai_mil_platform, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements) :-
    domain_priors:base_extractiveness(genai_mil_platform, E), E >= 0.30,
    domain_priors:suppression_score(genai_mil_platform, S), S >= 0.40,
    narrative_ontology:constraint_beneficiary(genai_mil_platform, _),
    narrative_ontology:constraint_victim(genai_mil_platform, _),
    domain_priors:requires_active_enforcement(genai_mil_platform).

:- end_tests(genai_mil_platform_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects the significant transfer of value
 *     from the government to the contractor, not just in dollars, but in the capture of
 *     institutional knowledge, control over the analytical process, and the deskilling
 *     of the analyst workforce.
 *   - Suppression Score (0.75): The mandate to use a single platform is highly coercive,
 *     suppressing a free market of alternative tools, open-source models, and traditional
 *     analytical methods.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the trapped analyst, the platform is a Snare that devalues their
 *   cognitive skills and autonomy. For the contractor with arbitrage exit options, it's a
 *   pure Rope, a beneficial coordination standard they provide to a grateful customer.
 *   This highlights the core tension of a Tangled Rope: a single object that is
 *   simultaneously a coordination tool and an extraction machine.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint is directed from the DoD (leadership and analysts) towards the contractor.
 *   - Beneficiaries: The 'platform_contractor' benefits financially and strategically.
 *     'dod_leadership' benefits from perceived modernization and standardized reporting.
 *   - Victim: The 'dod_analysts' bear the costs of skill atrophy, loss of autonomy, and
 *     potential career stagnation. The system correctly models this flow of costs and
 *     benefits to derive directionality.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story models the critical difference between the two institutional beneficiaries.
 *   The contractor has `exit_options(arbitrage)`—they can sell their platform elsewhere,
 *   pivot their technology, and are not existentially tied to this single contract. DoD
 *   Leadership, however, has `exit_options(constrained)`. Once the platform is deeply
 *   integrated, switching to a new system would be astronomically expensive and politically
 *   untenable. This difference in exit optionality is why their derived directionality (d)
 *   differs, leading to different perceived effective extraction (χ), even though both are
 *   classified as Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies this system as a Tangled Rope, avoiding the
 *   pitfalls of simpler analyses. A purely pro-technology view would miss the extraction
 *   and call it a Rope. A purely cynical view would miss the genuine (if overstated)
 *   coordination benefits and call it a Snare. The Tangled Rope classification correctly
 *   identifies that it possesses BOTH a genuine coordination function AND an asymmetric
 *   extractive component, which is the defining feature of many large-scale technology
 *   integrations in institutional settings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_genai_mil_platform,
    'Is the platform''s claimed performance boost genuine and sustainable, or is it a temporary effect that will be eroded by model brittleness and adversarial adaptation?',
    'Long-term, independent red-teaming and comparison of platform-assisted vs. human-only analytical teams on novel, out-of-distribution intelligence problems.',
    'If genuine, the constraint is a justifiable (though still extractive) Tangled Rope. If illusory, its coordination function is pure theater, making it a Piton or a Snare justified by false pretenses.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(genai_mil_platform, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a high-extraction constraint
% (ε=0.48 > 0.46), this is required. The trajectory shows a system that
% started as a promising tool (lower extraction) and hardened into an
% extractive dependency over time.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(gmp_tr_t0, genai_mil_platform, theater_ratio, 0, 0.10).
narrative_ontology:measurement(gmp_tr_t5, genai_mil_platform, theater_ratio, 5, 0.25).
narrative_ontology:measurement(gmp_tr_t10, genai_mil_platform, theater_ratio, 10, 0.40).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(gmp_ex_t0, genai_mil_platform, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gmp_ex_t5, genai_mil_platform, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(gmp_ex_t10, genai_mil_platform, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The platform's primary function is to standardize analytical products.
narrative_ontology:coordination_type(genai_mil_platform, information_standard).

% Network relationships (structural influence edges)
% This platform directly creates vendor lock-in, which is its own constraint.
narrative_ontology:affects_constraint(genai_mil_platform, dod_procurement_lockin).
narrative_ontology:affects_constraint(genai_mil_platform, analyst_career_progression).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The structural derivation chain,
% using beneficiary/victim declarations combined with the distinct exit options
% (trapped, constrained, arbitrage), accurately computes the different
% directionality values (d) for each agent, capturing the narrative's
% structural dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */