% ============================================================================
% CONSTRAINT STORY: us_china_chip_tariffs_v2
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% ============================================================================

:- module(constraint_us_china_chip_tariffs_v2, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_china_chip_tariffs_v2
 *   human_readable: US Tariffs on Chinese High-Tech Goods (2024)
 *   domain: economic/political
 *
 * SUMMARY:
 *   The US government has imposed steep tariffs on Chinese high-tech imports,
 *   including a 50% tariff on semiconductors by 2025. The stated goals are
 *   to protect domestic industries from unfair competition, bolster national
 *   security, and encourage on-shoring of critical manufacturing. This policy
 *   creates a significant economic barrier, restructuring supply chains and
 *   market access.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chinese Tech Exporters: Primary target (organized/constrained) — bear extraction via lost market access.
 *   - US Domestic Producers: Primary beneficiary (institutional/arbitrage) — benefit from reduced competition.
 *   - US Government: Architect/Enforcer (institutional/arbitrage) — frames the policy as a coordination mechanism.
 *   - US Downstream Industries/Consumers: Secondary victims (powerless/trapped) — face higher component costs.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_chip_tariffs_v2, 0.52).
domain_priors:suppression_score(us_china_chip_tariffs_v2, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_china_chip_tariffs_v2, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_china_chip_tariffs_v2, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_china_chip_tariffs_v2, tangled_rope).
narrative_ontology:human_readable(us_china_chip_tariffs_v2, "US Tariffs on Chinese High-Tech Goods (2024)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_china_chip_tariffs_v2). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, us_domestic_producers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, chinese_tech_exporters).
narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, us_downstream_industries).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (CHINESE EXPORTERS)
% As victims with constrained market access, they perceive a highly extractive Snare.
% Engine derives d from: victim membership + organized power + constrained exit -> high d -> high f(d) -> high χ
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US DOMESTIC PRODUCERS)
% As beneficiaries with arbitrage power, they see a beneficial coordination Rope.
% Engine derives d from: beneficiary membership + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees both the coordination function and the asymmetric extraction, classifying it as a Tangled Rope.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---

% PERSPECTIVE 4: THE ARCHITECT (US GOVERNMENT)
% From the government's perspective, this is an instrument of industrial policy,
% a Rope to coordinate domestic economic and security goals.
% As a beneficiary with arbitrage exit, it experiences low/negative extraction.
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SECONDARY VICTIM (US DOWNSTREAM INDUSTRIES/CONSUMERS)
% These groups bear costs through higher prices and have limited ability to avoid them.
% Engine derives d from: victim membership + powerless + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> very high χ
constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_chip_tariffs_v2_tests).

test(perspectival_gap) :-
    % Verify the core perspectival gap between the target and beneficiary.
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, snare, context(agent_power(organized), _, _, _)),
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, rope, context(agent_power(institutional), _, _, _)),
    true.

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(us_china_chip_tariffs_v2, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_compliance) :-
    % Verify all three conditions for a Tangled Rope are met.
    domain_priors:requires_active_enforcement(us_china_chip_tariffs_v2),
    narrative_ontology:constraint_beneficiary(us_china_chip_tariffs_v2, _),
    narrative_ontology:constraint_victim(us_china_chip_tariffs_v2, _).

:- end_tests(us_china_chip_tariffs_v2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.52): Represents the significant economic value transfer
 *     and opportunity cost imposed by a 50-100% tariff. It's high enough to feel
 *     like a Snare to its targets.
 *   - Suppression (0.75): The tariffs are a state-enforced policy that severely
 *     restricts the alternative of sourcing cheaper components from China for the US market.
 *   - The combination of a clear coordination goal (on-shoring, national security)
 *     and a clear extractive mechanism (tariffs) makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is acute. For beneficiary US producers, the tariff is a Rope that
 *   coordinates market conditions in their favor, shielding them from competition.
 *   For the targeted Chinese exporters, it is a Snare that cuts them out of a
 *   major market, extracting opportunity and revenue. This disagreement is the
 *   hallmark of protectionist policy, which by design benefits a domestic group
 *   at the expense of foreign competitors and often domestic consumers.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from the structural relationships.
 *   - `us_domestic_producers` are declared beneficiaries. With their institutional power
 *     and `arbitrage` exit (lobbying, pricing power), the engine derives a very low `d`,
 *     leading to a negative effective extraction (χ) and a Rope classification.
 *   - `chinese_tech_exporters` are declared victims. Their `organized` power and
 *     `constrained` exit from the US market result in a high `d`, high positive χ,
 *     and a Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling. A purely free-market analyst might call
 *   this a Snare, ignoring the stated coordination goals of the US government.
 *   A purely state-centric view might call it a Rope, ignoring the severe extraction
 *   imposed on exporters and consumers. The Tangled Rope classification from the
 *   analytical perspective correctly identifies that it is BOTH: a policy that uses
 *   an extractive mechanism in service of a coordination goal for a specific in-group.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_us_china_chip_tariffs_v2,
    'Will the tariffs successfully onshore sustainable US manufacturing, or primarily serve as a permanent extractive subsidy for incumbent firms?',
    'Longitudinal analysis of US semiconductor market share, innovation rates, and consumer prices over a 10-year period.',
    'If successful, the Rope aspect is validated. If not, the policy degenerates into a Snare/Piton where extraction outweighs coordination.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_china_chip_tariffs_v2, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Since base_extractiveness (0.52) > 0.46, temporal data is required.
% This models the policy starting with clear intent (low theater) but with the risk
% of political performance increasing if economic goals are not met. Extraction remains
% consistently high.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(us_china_chip_tariffs_v2_tr_t0, us_china_chip_tariffs_v2, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_china_chip_tariffs_v2_tr_t5, us_china_chip_tariffs_v2, theater_ratio, 5, 0.18).
narrative_ontology:measurement(us_china_chip_tariffs_v2_tr_t10, us_china_chip_tariffs_v2, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_china_chip_tariffs_v2_ex_t0, us_china_chip_tariffs_v2, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(us_china_chip_tariffs_v2_ex_t5, us_china_chip_tariffs_v2, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(us_china_chip_tariffs_v2_ex_t10, us_china_chip_tariffs_v2, base_extractiveness, 10, 0.52).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The policy is a mechanism to re-allocate market resources
% and investment toward domestic actors.
narrative_ontology:coordination_type(us_china_chip_tariffs_v2, resource_allocation).

% Network relationships: This tariff policy directly impacts the structure and
% stability of the global semiconductor supply chain.
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, semiconductor_supply_chain).
narrative_ontology:affects_constraint(us_china_chip_tariffs_v2, global_tech_standards).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and their respective exit
% options accurately models the structural dynamics of the tariff policy.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */