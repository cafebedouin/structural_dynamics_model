% ============================================================================
% CONSTRAINT STORY: incumbent_competitive_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_competitive_moat, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: incumbent_competitive_moat
 *   human_readable: Incumbent Competitive Moat in Concentrated Markets
 *   domain: economic/industrial_organization
 *
 * SUMMARY:
 *   An incumbent competitive moat is a structural constraint protecting a
 *   dominant firm's market position through interconnected barriers:
 *   economies of scale that make cost leadership self-reinforcing, network
 *   effects that increase service value as the user base grows, switching
 *   costs that lock customers into the incumbent's ecosystem, and control of
 *   complementary assets that smaller competitors cannot replicate. The moat
 *   exhibits a fundamental structural ambiguity: it represents genuine
 *   coordination efficiency (standardization, ecosystem depth, research
 *   investment justified by scale) and simultaneously operates as an
 *   extraction mechanism (price umbrella, suppressed competition, reduced
 *   consumer choice). The constraint's classification depends entirely on the
 *   observer's position. The incumbent sees functional coordination that
 *   reduces production costs and increases service value. Powerless consumers
 *   locked into the ecosystem see pure extraction with no exit options.
 *   Organized competitors see mixed coordination (ecosystem benefits) and
 *   extraction (price umbrella). The analytical observer risks naturalizing
 *   contingent institutional arrangements (property rights regimes, merger
 *   law, patent length) as immutable economic laws. The measurement
 *   trajectory shows that as market concentration increases over 10 periods,
 *   both extractiveness and suppression requirement rise — the moat deepens
 *   through accumulating switching costs and network lock-in, requiring
 *   increasing active enforcement to maintain incumbent dominance.
 *
 * KEY AGENTS:
 *   - Incumbent Firm: Primary beneficiary (institutional/arbitrage) — captures monopoly rents through price setting above marginal cost; experiences moat as functional coordination mechanism
 *   - Price-Sensitive Consumer: Primary victim (powerless/trapped) — locked into ecosystem through switching costs; no exit options; faces prices above competitive level; bears full extraction cost
 *   - Potential Entrant Firm: Secondary victim (powerless/trapped) — excluded from market through insurmountable barriers; capital requirements, scale disadvantages, and network effects prevent competitive entry
 *   - Locked-In Enterprise Customer: Mixed victim (moderate/constrained) — depends on incumbent platform for operations; benefits from ecosystem stability but faces price extraction through switching-cost lock-in
 *   - Competitive Fringe and Small Rivals: Organized victim (organized/constrained) — maintain marginal market positions through specialization or geographic segmentation; benefit from platform access but suppressed by incumbent's price umbrella
 *   - Regulatory Authority: Conditional actor (institutional/constrained) — enforcement choices (IP protection length, merger approval, antitrust enforcement) directly determine barrier malleability and moat sustainability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_competitive_moat, 0.58).
domain_priors:suppression_score(incumbent_competitive_moat, 0.62).
domain_priors:theater_ratio(incumbent_competitive_moat, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_competitive_moat, extractiveness, 0.58).
narrative_ontology:constraint_metric(incumbent_competitive_moat, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(incumbent_competitive_moat, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_competitive_moat, tangled_rope).
narrative_ontology:human_readable(incumbent_competitive_moat, "Incumbent Competitive Moat in Concentrated Markets").
narrative_ontology:topic_domain(incumbent_competitive_moat, "economic/industrial_organization").

domain_priors:requires_active_enforcement(incumbent_competitive_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_competitive_moat, incumbent_firm).
narrative_ontology:constraint_beneficiary(incumbent_competitive_moat, coordinated_consumer_base).
narrative_ontology:constraint_victim(incumbent_competitive_moat, entrant_firms).
narrative_ontology:constraint_victim(incumbent_competitive_moat, suppressed_price_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-SENSITIVE CONSUMER (SNARE) — Locked into incumbent ecosystem through switching costs (data lock-in, ecosystem integration, retraining burden). Cannot exit without sacrificing accumulated investments. Faces prices above marginal cost with no credible alternatives. Experiences pure extraction: the moat's entire function from this position is rent capture.
constraint_indexing:constraint_classification(incumbent_competitive_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POTENTIAL ENTRANT FIRM (SNARE) — Faces insurmountable barriers: incumbent's scale economies, network effects that disadvantage small competitors, predatory pricing during market entry, control of essential complementary assets. Capital requirements exceed achievable financing for startups. Pure extraction — the moat's function for entrants is complete market exclusion.
constraint_indexing:constraint_classification(incumbent_competitive_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Experiences the moat as coordination mechanism: network effects generate genuine consumer surplus through standardization and ecosystem depth. Scale economies reduce production costs below competitors. The firm sees legitimate efficiency gains and views the moat as functional coordination, not rent extraction. High extractive power but substantial consumer benefit justifies the classification as rope from this perspective.
constraint_indexing:constraint_classification(incumbent_competitive_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETITIVE FRINGE (TANGLED ROPE) — Organized rivals perceive both coordination benefits (access to incumbent's ecosystem enables niche positioning) and extraction (price umbrella maintained by incumbent limits competitive pressure). Genuine coordination function exists — complementary products depend on the incumbent's platform stability — alongside asymmetric extraction of monopoly rents. Organized agents have some bargaining power and partial exit options (specialization, geographic segmentation, technological differentiation), making their experience genuinely mixed.
constraint_indexing:constraint_classification(incumbent_competitive_moat, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCKED-IN ENTERPRISE CUSTOMER (TANGLED ROPE) — Medium-sized firm dependent on incumbent's platform for core operations. Coordination function is genuine: the incumbent's ecosystem provides critical infrastructure, standardization, and integration. But switching costs (data migration, workflow reengineering, staff retraining, integration work) create price extraction: the incumbent can raise prices knowing switching is prohibitively expensive. Moderate power and constrained exit create mixed experience of coordination and extraction.
constraint_indexing:constraint_classification(incumbent_competitive_moat, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, scale economies and network effects appear as immutable features of industrial organization: firms that achieve coordination at scale cannot be easily displaced, and this efficiency advantage appears as natural law rather than contingent institutional constraint. The analytical observer risks classifying the moat as an unchangeable property of markets. However, structural data (identifiable beneficiaries who maintain the moat through enforceable barriers, victims who could exit given reduced switching costs) contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of contingent market arrangements.
constraint_indexing:constraint_classification(incumbent_competitive_moat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_competitive_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_competitive_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_competitive_moat, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_competitive_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(incumbent_competitive_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The incumbent captures substantial rents through price-setting power above marginal cost. However, the value is not as high as pure monopoly extraction (0.8+) because genuine economies of scale and network effects do reduce unit costs and increase consumer surplus. The trade-off between coordination efficiency and rent capture justifies a mid-range tangled value rather than snare-level (0.66+) extractiveness. Suppression (0.62): Significant. Barriers to entry include minimum viable scale requirements, network effect disadvantages for small competitors, switching costs that lock consumers in, and active incumbent enforcement (predatory pricing, exclusive dealing, patent litigation). Entry is not impossible but requires substantial capital and involves high probability of failure. The measurement trajectory shows suppression rising over time as switching costs accumulate and network effects strengthen. Theater ratio (0.35): Low to moderate. The moat's mechanisms are largely functional coordination (economies of scale are real, network effects do exist) rather than performative. Unlike verification bottlenecks or degraded institutions, the moat's primary function is actual cost reduction and service standardization, not ritual maintenance. The low theater suggests the classification as Tangled Rope (mixed coordination-extraction) is more accurate than as Piton (degraded ritual).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces exceptionally wide perspectival divergence because the structural relationship to the moat differs sharply across agent positions. The incumbent firm genuinely experiences coordination — network effects increase service value, economies of scale reduce unit costs, ecosystem integration provides consumer benefits. A consumer locked into the ecosystem by high switching costs genuinely experiences extraction — they pay above-marginal-cost prices and cannot exit. A potential entrant genuinely experiences exclusion — minimum viable scale makes entry capital-prohibitive. Organized competitors genuinely experience mixed effects — the incumbent's platform provides ecosystem benefits but the price umbrella suppresses competitive pressure. The analytical observer risks collapse into false naturalism: treating the moat as an immutable feature of industrial organization (mountain) when the barriers are partly contingent on regulatory choices (IP length, merger law) and could be altered through policy. The false summit detection should fire here: if the incumbent is identified as a beneficiary, the mountain classification becomes questionable — moats that benefit identifiable actors are not natural laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from power level, exit options, and beneficiary/victim status. The incumbent firm (institutional/arbitrage) experiences minimal directionality — d ≈ 0.15, producing negative or near-zero chi through the sigmoid f(d). They are the beneficiary; arbitrage exit options mean they control the constraint's terms. Price-sensitive consumers (powerless/trapped) experience maximal directionality — d ≈ 0.95, producing chi ≈ 1.42 through f(d). They are pure victims with no exit. Potential entrants (powerless/trapped) similarly experience d ≈ 0.95 — they cannot enter regardless of cost. Competitive fringe (organized/constrained) experiences d ≈ 0.55, producing chi ≈ 0.75 — they have some organizational power and partial exit options (specialization, niches) but are suppressed by the price umbrella. Enterprise customers (moderate/constrained) experience d ≈ 0.65, producing chi ≈ 1.00 — they depend on the platform (high victim status) but have moderate power to negotiate and partial exit options at high cost. The perspectival gap is large: incumbent sees rope (coordination benefits justify scale); powerless consumers see snare (no escape); organized competitors see tangled rope (mixed experience of platform benefits and suppression).
 *
 * MANDATROPHY ANALYSIS:
 *   The incumbent competitive moat resolves the mandatrophy through structural acknowledgment of genuine coordination alongside genuine extraction. Tangled Rope is the only classification that captures the real phenomenon: the moat does provide coordination benefits (standardization, ecosystem efficiency, network value) while simultaneously extracting rents through price-setting power. The error would be to collapse toward either pole: classifying the moat as Rope (pure coordination, benefiting from scale efficiencies) ignores the extraction of monopoly rents and switching-cost lock-in of consumers. Classifying it as Snare (pure extraction) ignores the genuine consumer surplus from network effects and cost reductions. The mandatrophy is resolved by accepting that both claims are structurally true from their respective positions. From the incumbent's position, the moat is a valuable coordination mechanism. From the trapped consumer's position, it is pure extraction. The canonical classifier correctly produces different types from different indexical positions. The apparent contradiction reflects real structural difference, not analytical error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consumer_surplus_distribution,
    'What proportion of network effect and scale economy benefits flow to consumers versus being captured as incumbent monopoly rents?',
    'Comparative analysis of price-cost margins across market stages; consumer surplus modeling before/after competitive entry; measurement of price elasticity and deadweight loss',
    'If consumer capture >70%: classification shifts toward Rope (genuine coordination dominates). If incumbent capture >70%: shifts toward Snare (extraction dominates coordination function). Mid-range values (40-60%) support Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_surplus_distribution, empirical, 'Distribution of efficiency gains between consumer surplus and incumbent rents').

omega_variable(
    barrier_malleability_threshold,
    'How much of the moat''s protective power derives from natural economies of scale versus enforcement of artificial barriers (IP lockdown, exclusive dealing, predatory pricing, control of complementary assets)?',
    'Decomposition of barrier sources: patent analysis, exclusive contract audit, pricing vs marginal cost regression, complementary asset ownership mapping. Historical comparison to earlier market periods with less concentrated barriers.',
    'If >60% natural: moat appears more like mountain (immutable efficiency). If >60% enforced: moat appears as snare (contingent extraction requiring active suppression). This determines whether the constraint is genuinely unchangeable or artificially maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(barrier_malleability_threshold, empirical, 'Proportion of moat protection derived from natural economies versus enforcement').

omega_variable(
    switching_cost_irreversibility,
    'Are switching costs genuinely irreversible (sunk investment in incumbent-specific assets that lose value upon exit) or reversible (high-cost but recoverable investment)?',
    'Audit of customer switching costs by type: data migration reversibility, workflow redesign portability, staff skill transferability, ecosystem integration recovery. Comparison to firms that have successfully switched; measurement of actual exit costs incurred.',
    'If predominantly irreversible: victims are functionally trapped, supporting Snare classification. If predominantly reversible: victims are constrained but mobile, supporting Tangled Rope. Irreversibility threshold = 60% of total switching cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_irreversibility, empirical, 'Whether switching costs lock customers in irreversibly or are reversible at high cost').

omega_variable(
    network_effect_causality,
    'Do network effects genuinely arise from coordination benefits (more users increase service value for all) or from artificial platform lock-in (more users on incumbent system create switching-cost barriers for others)?',
    'Historical analysis: comparison of service value during network growth periods (genuinely increasing?) versus maturity periods (value flat or declining?). User preference surveys before/after competitor emergence. Measurement of cross-platform compatibility costs.',
    'If effects are genuine coordination benefits: incumbent''s dominant position is efficiently justified, supporting Rope classification. If effects are artificial lock-in: network dominance becomes rent-extraction mechanism, supporting Snare. Mixed causality supports Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_causality, empirical, 'Whether network effects represent genuine coordination benefits or artificial lock-in').

omega_variable(
    regulatory_enforcement_role,
    'How much of the moat''s effectiveness depends on regulatory protection (IP rights, merger approval barriers, exclusive licensing) versus natural competitive barriers (cost structure, complementary asset control)?',
    'Comparative historical analysis: market concentration before/after regulatory changes (patent term extension, antitrust enforcement, merger approval). Counterfactual analysis of incumbent position under alternative IP regimes.',
    'If regulatory protection >50%: moat is contingent on policy, suggesting Scaffold (with potential sunset through policy change) or Tangled Rope (policy-maintained extraction). If regulatory <30%: suggests natural economic barriers (possibly Mountain). Mixed suggests true Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_enforcement_role, empirical, 'Degree to which moat effectiveness depends on regulatory enforcement versus natural barriers').

omega_variable(
    innovation_suppression_mechanism,
    'Does the incumbent actively suppress entrant innovation (predatory pricing, exclusive dealing, patent thicket deployment) or does market structure passively discourage entry through cost barriers?',
    'Case analysis of entry attempts: measurement of incumbent response patterns (price cuts, patent litigation, acquisition of competitors, exclusive contracts). Comparison to markets where passive barriers exist without active suppression.',
    'If active suppression is significant: moat requires continuous enforcement (Snare, Tangled Rope with high requires_active_enforcement). If passive barriers dominate: moat may be structural but not actively enforced (Rope or Mountain). Active suppression ratio >50% indicates requires_active_enforcement=true.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innovation_suppression_mechanism, empirical, 'Whether incumbent actively suppresses entry or market structure passively discourages it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_competitive_moat, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moat_tr_t0, incumbent_competitive_moat, theater_ratio, 0, 0.25).
narrative_ontology:measurement(moat_tr_t5, incumbent_competitive_moat, theater_ratio, 5, 0.28).
narrative_ontology:measurement(moat_tr_t10, incumbent_competitive_moat, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(moat_be_t0, incumbent_competitive_moat, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(moat_be_t5, incumbent_competitive_moat, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(moat_be_t10, incumbent_competitive_moat, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(moat_su_t0, incumbent_competitive_moat, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(moat_su_t5, incumbent_competitive_moat, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(moat_su_t10, incumbent_competitive_moat, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_competitive_moat, resource_allocation).
narrative_ontology:boltzmann_floor_override(incumbent_competitive_moat, 0.12).
narrative_ontology:affects_constraint(incumbent_competitive_moat, network_effect_lock_in).
narrative_ontology:affects_constraint(incumbent_competitive_moat, switching_cost_accumulation).
narrative_ontology:affects_constraint(incumbent_competitive_moat, scale_economy_threshold).

% DUAL FORMULATION NOTE:
% The incumbent competitive moat is a macroscopic structural constraint composed of multiple microstructural constraints: network effects (upstream), switching costs (downstream mechanism), and scale economies (efficiency driver). Each has its own ε value and could be decomposed into a constraint family. The moat story models the integrated effect; the network edges link to its component mechanisms for detailed analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(incumbent_competitive_moat, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
