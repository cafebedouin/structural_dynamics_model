% ============================================================================
% CONSTRAINT STORY: china_ev_export_oversupply
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_china_ev_export_oversupply, []).

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
 *   constraint_id: china_ev_export_oversupply
 *   human_readable: Chinese EV Export Oversupply and Market Distortion
 *   domain: economic/political
 *
 * SUMMARY:
 *   China's extensive subsidies for its electric vehicle (EV) industry, coupled with aggressive export strategies, are creating a global oversupply of EVs. This oversupply is driving down prices, distorting international markets, and threatening the viability of domestic EV industries in other countries, potentially leading to trade tensions and protectionist measures.
 *
 * KEY AGENTS (by structural relationship):
 *   - Workers in Western EV Manufacturing: Primary target (powerless/trapped) — face job losses due to industry pressure.
 *   - Western EV Manufacturers: Corporate target (moderate/constrained) — faces suppressed market share and potential bankruptcy.
 *   - Chinese EV Industry: Primary beneficiary (institutional/arbitrage) — benefits from increased market share and global influence.
 *   - Governments of Importing Nations: Secondary actor (institutional/constrained) — balances economic benefit of cheap EVs with the need to protect domestic industries.
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees the full structure of market distortion.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_ev_export_oversupply, 0.55).
domain_priors:suppression_score(china_ev_export_oversupply, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(china_ev_export_oversupply, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_ev_export_oversupply, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_ev_export_oversupply, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(china_ev_export_oversupply, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(china_ev_export_oversupply, tangled_rope).
narrative_ontology:human_readable(china_ev_export_oversupply, "Chinese EV Export Oversupply and Market Distortion").

% --- Binary flags ---
domain_priors:requires_active_enforcement(china_ev_export_oversupply). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(china_ev_export_oversupply, chinese_ev_industry).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(china_ev_export_oversupply, western_ev_manufacturers).
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

% PERSPECTIVE 1: WORKERS IN WESTERN EV MANUFACTURING (SNARE)
% Agent who bears the most direct extraction via job loss. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(china_ev_export_oversupply, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHINESE EV INDUSTRY (ROPE)
% Agent who benefits from increased market share and global influence. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(china_ev_export_oversupply, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% Used by the bridge to derive constraint_claim.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(china_ev_export_oversupply, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN EV MANUFACTURERS (SNARE)
% Corporate agent who faces suppressed market share. Engine derives d from:
%   victim membership + constrained exit → d ≈ 0.85 → f(d) ≈ 1.15 → high χ
constraint_indexing:constraint_classification(china_ev_export_oversupply, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% Governments of Importing Nations: Balancing act between cheap EVs and domestic industry protection.
% Perspective 5A: Pro-Cheap EV Government (institutional, arbitrage exit - benefits consumers)
constraint_indexing:constraint_classification(china_ev_export_oversupply, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 5B: Pro-Domestic Industry Government (institutional, constrained exit - protects jobs)
constraint_indexing:constraint_classification(china_ev_export_oversupply, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_ev_export_oversupply_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_ev_export_oversupply, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(china_ev_export_oversupply, ExtMetricName, E),
    E >= 0.46. % High-extraction Tangled Rope/Snare.

:- end_tests(china_ev_export_oversupply_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope from an analytical view due to the combination of a coordination function (providing cheaper EVs globally, accelerating EV adoption) and asymmetric extraction (harming domestic manufacturers and their workers in importing nations). The base extractiveness (ε=0.55) is high, reflecting the significant market distortion caused by state subsidies. Suppression (0.65) is also high because competing on price becomes nearly impossible for unsubsidized firms. The theater ratio is low because the primary activity is genuine economic competition, not performative action.
 *
 * PERSPECTIVAL GAP:
 *   - Workers in Western EV firms (powerless, trapped) see a Snare, as their livelihoods are directly threatened by forces beyond their control.
 *   - Western EV manufacturers (moderate, constrained) also see a Snare, as they are trapped in a market distorted by state-backed oversupply.
 *   - The Chinese EV industry (institutional, arbitrage) views it as a Rope, a successful coordination of industrial policy that benefits them immensely.
 *   - Governments of importing nations are split, seeing either a Rope (consumer benefit) or a Tangled Rope (industrial harm), depending on their policy priorities.
 *
 * DIRECTIONALITY LOGIC:
 *   The Chinese EV industry is the clear beneficiary, gaining global market share. Western EV manufacturers and their employees are the victims, bearing the costs through reduced sales, suppressed wages, and job losses. The governments of importing nations are in a complex position, weighing the benefits of cheaper EVs for their consumers against the need to protect their domestic industries.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   Governments of importing nations face a trade-off. A government prioritizing consumer welfare and climate goals might see cheap EVs as a benefit (Rope), having the `arbitrage` option to import them. A government prioritizing domestic industrial strategy and employment is `constrained` by the need to protect its local firms, experiencing the market distortion as extractive (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids mislabeling a complex trade issue as pure extraction. It correctly identifies the genuine coordination function (accelerating the availability of affordable EVs) while simultaneously capturing the severe asymmetric extraction that harms specific groups (Western firms and labor). If it were a pure Snare, there would be no coordination benefit to anyone but the extractor, which is not the case here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_china_ev_export_oversupply,
    'To what extent will governments of importing nations impose protectionist measures (e.g., tariffs) against Chinese EVs?',
    'Monitoring of trade policy decisions and tariff implementations in the US and EU.',
    'If True (significant tariffs imposed): The constraint weakens. Market access for Chinese EVs is reduced, mitigating the extraction from Western manufacturers and potentially shifting the analytical classification towards a less severe Tangled Rope. If False (no significant measures): The constraint intensifies. Market dominance by Chinese EVs grows, exacerbating the extraction and reinforcing the Snare classification from victim perspectives.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(china_ev_export_oversupply, 0, 10).

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
narrative_ontology:measurement(china_ev_export_oversupply_tr_t0, china_ev_export_oversupply, theater_ratio, 0, 0.20).
narrative_ontology:measurement(china_ev_export_oversupply_tr_t5, china_ev_export_oversupply, theater_ratio, 5, 0.25).
narrative_ontology:measurement(china_ev_export_oversupply_tr_t10, china_ev_export_oversupply, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(china_ev_export_oversupply_ex_t0, china_ev_export_oversupply, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(china_ev_export_oversupply_ex_t5, china_ev_export_oversupply, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(china_ev_export_oversupply_ex_t10, china_ev_export_oversupply, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(china_ev_export_oversupply, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(china_ev_export_oversupply, 0.40).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(china_ev_export_oversupply, [other_constraint_id]).

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
% constraint_indexing:directionality_override(china_ev_export_oversupply, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */