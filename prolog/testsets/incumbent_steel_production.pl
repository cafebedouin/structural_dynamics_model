% ============================================================================
% CONSTRAINT STORY: incumbent_steel_production
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_incumbent_steel_production, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: incumbent_steel_production
 *   human_readable: "Incumbent Blast Furnace Steel Production Method"
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The global steel industry is dominated by the blast furnace method, which
 *   requires high-grade iron ore and coking coal. This creates enormous
 *   capital barriers to entry, significant CO2 emissions, and dependencies on
 *   specific global supply chains, structurally suppressing alternative,
 *   cleaner, and more flexible production methods.
 *
 * KEY AGENTS (by structural relationship):
 *   - Innovators & Domestic Producers: Primary target (powerless/constrained) — bears the cost of suppression and high entry barriers.
 *   - Incumbent Steel Corporations: Primary beneficiary (institutional/arbitrage) — benefits from protected market, high barriers, and established supply chains.
 *   - National Regulators: Inter-institutional actor (institutional/constrained) — seeks strategic independence and emissions reduction but is constrained by incumbents' economic power.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination, extraction, and suppression.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_steel_production, 0.55).
domain_priors:suppression_score(incumbent_steel_production, 0.80).   % Structural property (raw, unscaled). High capital cost is a massive barrier.
domain_priors:theater_ratio(incumbent_steel_production, 0.20).       % Piton detection (>= 0.70). Some greenwashing, but primary function is real.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_steel_production, extractiveness, 0.55).
narrative_ontology:constraint_metric(incumbent_steel_production, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(incumbent_steel_production, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(incumbent_steel_production, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(incumbent_steel_production). % Required for Tangled Rope. Enforced by capital markets, lobbying, and infrastructure lock-in.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(incumbent_steel_production, incumbent_steel_corporations).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, coking_coal_exporters).
narrative_ontology:constraint_beneficiary(incumbent_steel_production, high_grade_iron_ore_exporters).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(incumbent_steel_production, innovators_and_domestic_producers).
narrative_ontology:constraint_victim(incumbent_steel_production, general_public_environment). % Externalized CO2 costs.

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three) -> MET
%   Snare:        victim required -> MET

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

% PERSPECTIVE 1: THE PRIMARY TARGET (INNOVATORS & NEW ENTRANTS)
% They are victims with trapped/constrained exit. The enormous capital
% cost and integrated supply chains form a barrier that extracts all potential
% surplus from innovation, functioning as a snare.
% Engine derives d from: victim + trapped → d≈0.95 → f(d)≈1.42 → high χ
constraint_indexing:constraint_classification(incumbent_steel_production, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (INCUMBENT CORPORATIONS)
% As beneficiaries with arbitrage exit (lobbying, capital reallocation),
% they experience the system as a highly profitable coordination mechanism.
% Engine derives d from: beneficiary + arbitrage → d≈0.05 → f(d)≈-0.12 → negative χ
constraint_indexing:constraint_classification(incumbent_steel_production, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the entire structure: the genuine coordination function of producing
% steel, but also the massive asymmetric extraction and suppression of
% alternatives. This dual nature is the definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.55 * 1.15 * 1.2 (global scope) = 0.76. This high effective extraction
% combined with a coordination function and high suppression confirms Tangled Rope.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates differently for different institutional actors.

% Perspective 4A: National Regulators
% They are institutional but constrained by the economic and political power
% of the incumbents. They are partial victims of the strategic vulnerability
% the constraint creates. Their derived 'd' is higher than the incumbents'.
% Engine derives d from: (partial) victim + constrained → d≈0.6 → f(d)≈0.85
% χ = 0.55 * 0.85 * 1.0 (national scope) = 0.47. This is on the cusp, but
% still seen as a high-friction Rope or a low-grade Tangled Rope.
constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_steel_production_tests).

test(perspectival_gap_target_vs_beneficiary) :-
    constraint_indexing:constraint_classification(incumbent_steel_production, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(incumbent_steel_production, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap verified: powerless sees snare, institutional sees rope.~n').

test(tangled_rope_analytical_view) :-
    constraint_indexing:constraint_classification(incumbent_steel_production, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical classification as Tangled Rope verified.~n').

test(tangled_rope_structural_gates) :-
    domain_priors:base_extractiveness(incumbent_steel_production, E), E >= 0.30,
    domain_priors:suppression_score(incumbent_steel_production, S), S >= 0.40,
    narrative_ontology:constraint_beneficiary(incumbent_steel_production, _),
    narrative_ontology:constraint_victim(incumbent_steel_production, _),
    domain_priors:requires_active_enforcement(incumbent_steel_production),
    format('... Structural gates for Tangled Rope passed.~n').

:- end_tests(incumbent_steel_production_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Represents the economic rents captured by incumbents due to high barriers to entry, plus the externalized environmental costs imposed on society.
 *   - Suppression (s=0.80): Reflects the immense capital cost of blast furnaces and integrated mills, which effectively blocks most new entrants and technologies, regardless of their potential efficiency or environmental benefits. This is a core feature of the constraint.
 *   - The combination of a genuine, necessary coordination function (making steel) with high extraction and high suppression makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   - Innovators (powerless/trapped) see a Snare. For them, the system's primary function is to prevent their entry and capture any value they create. The capital barrier is absolute.
 *   - Incumbents (institutional/arbitrage) see a Rope. The system is a stable, highly profitable mechanism for coordinating a complex global industry, from which they are the primary beneficiaries. The 'extraction' is simply profit to them.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries are the owners of the incumbent capital stack (steel corporations) and the key suppliers locked into that stack (coking coal, high-grade ore). Their business models depend on this constraint remaining in place.
 *   - Victims are those structurally excluded by the constraint: innovators with alternative technologies, and the global public that bears the environmental cost (CO2 emissions) of this specific production method.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model includes two distinct institutional perspectives. The incumbent corporations (arbitrage exit) experience the system as a beneficial Rope. National regulators (constrained exit), however, experience it differently. They are pressured by incumbents' lobbying and economic importance, limiting their ability to foster alternatives. This 'constrained' exit option raises their derived directionality (d), leading to a higher effective extraction (χ) and a Tangled Rope classification, capturing the friction and undesirable lock-in they face.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly avoids two potential errors. It is not a pure Snare, because it performs a vital economic coordination function. It is not a pure Rope, because its benefits are distributed with extreme asymmetry, and its existence actively suppresses superior alternatives. The Tangled Rope classification captures this essential duality of function and extraction, which is the core of the problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_incumbent_steel_production,
    'Is the suppression of new tech due to inherent capital scale (quasi-Mountain) or active lobbying and anti-competitive practices by incumbents (Snare-like)?',
    'Analysis of historical capital allocation in the steel sector, paired with records of lobbying against environmental regulations or subsidies for new tech.',
    'If primarily capital scale, the constraint is more "natural" and harder to change. If primarily active suppression, policy interventions could be more effective.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(incumbent_steel_production, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction (0.55) is > 0.46, so temporal data is required.
% This models industry consolidation and increased focus on "greenwashing"
% over the last few decades (interval 0 to 10).

% Theater ratio over time (metric_substitution: greenwashing increases):
narrative_ontology:measurement(isp_tr_t0, incumbent_steel_production, theater_ratio, 0, 0.10).
narrative_ontology:measurement(isp_tr_t5, incumbent_steel_production, theater_ratio, 5, 0.15).
narrative_ontology:measurement(isp_tr_t10, incumbent_steel_production, theater_ratio, 10, 0.20).

% Extraction over time (extraction_accumulation: consolidation increases rents):
narrative_ontology:measurement(isp_ex_t0, incumbent_steel_production, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(isp_ex_t5, incumbent_steel_production, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(isp_ex_t10, incumbent_steel_production, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system functions as a form of global infrastructure for a basic material.
narrative_ontology:coordination_type(incumbent_steel_production, global_infrastructure).

% Network relationships (structural influence edges)
% The steel industry's structure affects national security and is tied to fossil fuels.
narrative_ontology:affects_constraint(incumbent_steel_production, national_security_supply_chain).
narrative_ontology:affects_constraint(fossil_fuel_dependency, incumbent_steel_production).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural derivation chain,
% using beneficiary/victim declarations combined with the different exit_options
% for incumbents ('arbitrage') vs. regulators ('constrained'), accurately
% captures the directionality dynamics without manual intervention.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */