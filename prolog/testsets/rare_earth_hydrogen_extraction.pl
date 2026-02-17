% ============================================================================
% CONSTRAINT STORY: rare_earth_hydrogen_extraction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-29
% ============================================================================

:- module(constraint_rare_earth_hydrogen_extraction, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rare_earth_hydrogen_extraction
 *   human_readable: Rare Earth Element Dependency for Core Hydrogen Extraction
 *   domain: economic, technological, geopolitical
 *
 * SUMMARY:
 *   Accessing hydrogen from Earth's core, as theorized by some research, requires advanced extraction technologies heavily reliant on rare earth elements (REEs). The dependency on these elements, concentrated in specific geographical regions, creates a geopolitical and economic constraint, potentially leading to resource competition and strategic vulnerabilities.
 *
 * KEY AGENTS (by structural relationship):
 *   - Developing Nations: Primary target (powerless/trapped) — bears extreme extraction costs, potentially locked out of the technology.
 *   - Industrial Hydrogen Consumers: Secondary target (moderate/constrained) — bears extraction costs from supply chain risks.
 *   - Dominant REE Producers (e.g., China): Primary beneficiary (institutional/arbitrage) — benefits from increased demand for REEs.
 *   - Analytical Community: Analytical observer (analytical/analytical) — assesses feasibility and risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_hydrogen_extraction, 0.48).
domain_priors:suppression_score(rare_earth_hydrogen_extraction, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(rare_earth_hydrogen_extraction, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, extractiveness, 0.48).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(rare_earth_hydrogen_extraction, tangled_rope).
narrative_ontology:human_readable(rare_earth_hydrogen_extraction, "Rare Earth Element Dependency for Core Hydrogen Extraction").

% --- Binary flags ---
domain_priors:requires_active_enforcement(rare_earth_hydrogen_extraction). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(rare_earth_hydrogen_extraction, dominant_ree_producers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, hydrogen_consumers).
narrative_ontology:constraint_victim(rare_earth_hydrogen_extraction, developing_nations).


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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Developing nations without REE resources, trapped by economic dependency.
% victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDUSTRIAL HYDROGEN CONSUMERS (SNARE)
% Agent who bears the extraction costs related to REE dependencies.
% victim membership + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high χ
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REE PRODUCERS (ROPE)
% Agent who benefits from increased demand for REEs.
% beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_hydrogen_extraction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_hydrogen_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(rare_earth_hydrogen_extraction, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope/Snare.

:- end_tests(rare_earth_hydrogen_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness score of 0.48 reflects the inherent costs associated with accessing and processing REEs, including environmental impact and geopolitical risks. The suppression score of 0.55 indicates moderate barriers to alternative technologies due to the current dominance of REEs in high-performance applications. The low theater ratio reflects the limited public discourse about REE dependency risks in core hydrogen extraction.
 *
 * PERSPECTIVAL GAP:
 *   Powerless actors (developing nations) and industrial consumers perceive a snare due to their dependency on potentially unstable REE supply chains. REE producers view it as a rope, facilitating economic activity. The analytical observer sees a tangled rope, acknowledging both the coordination benefits (enabling hydrogen extraction) and the asymmetric extraction (risks and costs borne by consumers).
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant REE producers (e.g., China) benefit from increased demand, making them the beneficiary. Hydrogen consumers and developing nations bear the costs and risks of REE dependency, including potential supply chain disruptions and price increases, making them the victims. The derived directionality reflects these structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling this scenario as pure extraction (snare). The use of REEs enables a potential coordination benefit (hydrogen production) despite the risks and vulnerabilities associated with their supply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ree_hydrogen,
    'Will alternative technologies reduce REE dependency in hydrogen extraction?',
    'Research and development breakthroughs in material science.',
    'True: Reduced geopolitical risk. False: Continued or increased risk.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rare_earth_hydrogen_extraction, 0, 10).

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
narrative_ontology:measurement(rare_earth_hydrogen_extraction_tr_t0, rare_earth_hydrogen_extraction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rare_earth_hydrogen_extraction_tr_t5, rare_earth_hydrogen_extraction, theater_ratio, 5, 0.20).
narrative_ontology:measurement(rare_earth_hydrogen_extraction_tr_t10, rare_earth_hydrogen_extraction, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(rare_earth_hydrogen_extraction_ex_t0, rare_earth_hydrogen_extraction, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rare_earth_hydrogen_extraction_ex_t5, rare_earth_hydrogen_extraction, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rare_earth_hydrogen_extraction_ex_t10, rare_earth_hydrogen_extraction, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(rare_earth_hydrogen_extraction, resource_allocation).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% Example: REE supply chains also affect semiconductor production.
narrative_ontology:affects_constraint(rare_earth_hydrogen_extraction, semiconductor_supply).

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
% constraint_indexing:directionality_override(rare_earth_hydrogen_extraction, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */