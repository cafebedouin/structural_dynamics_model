% ============================================================================
% CONSTRAINT STORY: north_american_supply_chain_coupling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_american_supply_chain_coupling, []).

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
 *   constraint_id: north_american_supply_chain_coupling
 *   human_readable: North American Supply Chain Coupling
 *   domain: economic/geopolitical/trade
 *
 * SUMMARY:
 *   North American supply chain coupling, institutionalized through NAFTA
 *   (1994) and USMCA (2020), creates a regional production network where
 *   firms optimize across labor markets, regulatory environments, and
 *   geographic proximity. This generates genuine coordination benefits (lower
 *   costs, faster logistics, scale efficiency) alongside asymmetric
 *   extraction. Small suppliers in Mexico and labor-dependent communities in
 *   the U.S. and Canada are locked into relationships they cannot exit
 *   without economic collapse. Multinational firms extract value through wage
 *   suppression, monopsony power, and geographic arbitrage while maintaining
 *   formal coordination narratives ('mutual benefit,' 'competitiveness').
 *   National governments are structurally locked into the system despite loss
 *   of policy autonomy. The constraint demonstrates how coordination
 *   mechanisms can embed asymmetric extraction when power, exit options, and
 *   structural dependence differ across agents. The rising extractiveness
 *   (0.35 → 0.58 over 20 years) reflects increasing labor suppression, supply
 *   concentration, and government regulatory capture.
 *
 * KEY AGENTS:
 *   - Integrated multinational firms (institutional/arbitrage): Primary beneficiaries. Control production location, labor sourcing, and supply chains. Experience the constraint as pure coordination enabling cost reduction and margin extraction. High exit optionality.
 *   - Small domestic suppliers in Mexico and Canada (powerless/trapped): Primary victims. Specialized production and lack of alternatives create structural entrapment. Wage suppression and dependency extract value. No realistic exit.
 *   - Labor-dependent manufacturing communities (moderate/constrained): Secondary victims. Communities dependent on integrated supply employment face wage suppression, job instability, and vulnerability to supply chain shocks. Can organize but cannot easily exit without economic collapse.
 *   - National governments (institutional/constrained): Trapped between coordination benefits and autonomy costs. Cannot exit without disrupting economies but lose policy space (labor standards, environmental regulation, industrial policy). Experience extraction through regulatory capture and geopolitical constraint.
 *   - Geopolitically autonomous state capacity (powerful/mobile, but structurally constrained): Loss of strategic autonomy despite formal sovereignty. Decoupling from supply chains carries massive economic and security costs, suppressing policy exit options.
 *   - Analytical observer: Risks naturalizing asymmetric extraction as inevitable coordination cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_american_supply_chain_coupling, 0.58).
domain_priors:suppression_score(north_american_supply_chain_coupling, 0.62).
domain_priors:theater_ratio(north_american_supply_chain_coupling, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_american_supply_chain_coupling, extractiveness, 0.58).
narrative_ontology:constraint_metric(north_american_supply_chain_coupling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(north_american_supply_chain_coupling, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_american_supply_chain_coupling, tangled_rope).
narrative_ontology:human_readable(north_american_supply_chain_coupling, "North American Supply Chain Coupling").
narrative_ontology:topic_domain(north_american_supply_chain_coupling, "economic/geopolitical/trade").

domain_priors:requires_active_enforcement(north_american_supply_chain_coupling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_american_supply_chain_coupling, integrated_multinational_firms).
narrative_ontology:constraint_beneficiary(north_american_supply_chain_coupling, capital_intensive_industries).
narrative_ontology:constraint_victim(north_american_supply_chain_coupling, small_domestic_suppliers).
narrative_ontology:constraint_victim(north_american_supply_chain_coupling, labor_dependent_communities).
narrative_ontology:constraint_victim(north_american_supply_chain_coupling, geopolitically_autonomous_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SUPPLIER COMMUNITIES (SNARE) — Small suppliers in Mexico and Canada are locked into supply relationships with no realistic exit. Specialized production, sunk capital, and lack of alternative markets create structural entrapment. Wage suppression and dependency on multinational buyer terms extract value while alternatives (e.g., selling to local/regional markets) are economically unviable. Maximum suppression: geographic and economic barriers prevent exit.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LABOR-DEPENDENT MANUFACTURING COMMUNITIES (TANGLED ROPE) — U.S. and Canadian industrial towns depend on integrated production chains for employment. The system coordinates genuine economic activity (manufacturing, logistics, trade) but extracts through wage suppression, job instability, and vulnerability to supply chain shocks. Communities cannot easily exit without economic collapse, but some agency exists (union organizing, political pressure). Moderate experienced extraction.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTEGRATED MULTINATIONAL FIRMS (ROPE) — Corporations see the coupling as pure coordination: optimizing production location, labor cost, logistics, and supply redundancy. The constraint enables arbitrage across borders and labor markets. High exit optionality (can shift production, vertically integrate, or source alternatives). Net beneficiary — the system extracts value toward this agent.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL REGULATORY INSTITUTIONS (TANGLED ROPE) — Governments coordinate genuine economic benefits (intra-regional trade, competitive positioning, foreign direct investment) while bearing extraction costs: wage suppression, labor standards erosion, environmental externalizing, and loss of regulatory autonomy. Cannot easily exit without disrupting economies. Some enforcement agency (trade agreements, tariffs) exists but limited by interconnection. Institutions experience both coordination necessity and asymmetric extraction.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / COMPETITIVE LOGIC (ROPE) — From a civilizational view, North American integration creates genuine regional competitive advantage against Chinese and European manufacturing. The coupling mechanism genuinely coordinates production efficiency, supply resilience (vs distant suppliers), and comparative advantage. Low suppression from this view — participation is voluntary negotiation between sovereign states. However, structural data shows high suppression (0.62), indicating that the analytical observer is not capturing the asymmetric distribution of costs and benefits.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

% PERSPECTIVE 6: GEOPOLITICAL AUTONOMY PERSPECTIVE (SNARE) — North American supply chain coupling reduces each nation's strategic autonomy. Dependence on integrated production limits independent policy choices (labor standards, environmental regulation, industrial policy, trade negotiations). Even though governments are 'powerful' and 'mobile' in global terms, they are 'trapped' within the continental architecture. Effective extraction: loss of policy space and strategic options in exchange for coordination benefits that unevenly concentrate in beneficiary firms. The constraint suppresses exit (decoupling carries massive economic costs) while extracting policy autonomy.
constraint_indexing:constraint_classification(north_american_supply_chain_coupling, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_american_supply_chain_coupling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_american_supply_chain_coupling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_american_supply_chain_coupling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_american_supply_chain_coupling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(north_american_supply_chain_coupling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The coupling system generates genuine efficiency gains (coordination) but concentrates them in multinational firms while distributing costs (wage suppression, labor standards erosion, environmental externalizing, policy constraint) to small suppliers, workers, and governments. The value reflects accumulated extraction — not maximally severe (it's not pure rent-seeking) but substantial. Suppression (0.62): Moderate-high. Structural barriers to exit include: sunk capital in specialized production, lack of alternative markets for suppliers, economic dependence of labor communities, and geopolitical lock-in for governments (decoupling carries >10% GDP costs). Barriers are high but not absolute — some suppliers can diversify, some communities can reorganize, some governments retain some policy space. Theater ratio (0.48): Low-moderate. The constraint has genuine coordination function (supply chain optimization, regional competitiveness are real) but increasingly relies on narratives about 'mutual benefit' and 'fair trade' to maintain legitimacy despite observable extraction. Theater is not dominant — the coordination function is structurally real — but is rising as extraction increases and contradicts public commitment to labor standards.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiaries and victims. Multinational firms see pure coordination (Rope) — the system genuinely enables their efficiency and arbitrage objectives. They are not lying about coordination benefits; those benefits are structurally real. But dependent suppliers and labor communities see pure extraction (Snare) — the system locks them in while suppressing their wages and alternatives. Governments occupy an intermediate position (Tangled Rope) — they experience genuine economic coordination benefits but at the cost of policy autonomy and structural entrapment. The analytical observer risks seeing pure Rope (competitive regional advantage) and missing the asymmetric distribution. This perspectival gap is diagnostic: it reveals that the constraint combines real coordination with real extraction, and that the coordination benefits concentrate in a subset of actors while extraction costs distribute more broadly.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (multinational firms): Derived d ≈ 0.15 (beneficiary status + arbitrage/mobile exit). Low directionality → low f(d) → negative or minimal chi contribution → they perceive the constraint as enabling rather than extractive. Victims (suppliers, labor, governments): Derived d ≈ 0.80-0.90 (victim status + trapped/constrained exit). High directionality → high f(d) ≈ 1.10-1.35 → chi amplification → they perceive high extraction. The directionality derivation captures a fundamental structural fact: same constraint, same base properties, but vastly different experienced extractiveness depending on power and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Tangled Rope is the correct analytical classification: genuine coordination function (regional supply optimization) coexists with asymmetric extraction (concentrated gains for multinationals, distributed losses for suppliers/workers/governments). The beneficiary perspective (Rope) captures the coordination truth. The victim perspectives (Snare, Tangled Rope) capture the extraction truth. Neither is wrong — they describe different structural positions within the same constraint. The mandatrophy is resolved by recognizing that coordination and extraction are not mutually exclusive. The constraint that genuinely coordinates supply networks ALSO genuinely extracts value from smaller and less mobile agents. The classification must capture both. Piton is not present (theater is only 0.48, not ≥0.70) — the coordination function is real, not theatrical. Mountain is not present (extractiveness > 0.25, suppression > 0.05) — this is not an immutable natural law but a human-constructed institutional arrangement. Snare is too severe for the beneficiary perspective (they do benefit) but correct for victim perspectives experiencing high extraction. Rope is too optimistic for the full constraint — it misses the asymmetric extraction and suppression. Tangled Rope correctly models both the coordination function and the asymmetric extraction that coexist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_structural_participation,
    'Are firms and states participating in USMCA supply chains by genuine choice (Rope) or by structural entrapment masked as choice (Snare/Tangled Rope)?',
    'Counterfactual analysis: What would happen if a nation attempted to decouple? If exit costs exceed 15% of GDP or require 5+ years of industrial reorganization, participation is structurally forced despite formal voluntariness.',
    'If genuinely voluntary: classification shifts toward Rope for institutional perspectives. If structurally forced: Snare/Tangled Rope persists. Current ambiguity derives from confusing formal treaty voluntariness with structural economic entrapment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_structural_participation, empirical, 'Whether supply chain participation is voluntary choice or structural entrapment').

omega_variable(
    extraction_vs_comparative_advantage,
    'Does high suppression (0.62) reflect genuine asymmetric extraction or inevitable friction in any multi-party coordination system?',
    'Distribution analysis: quantify wage gaps, profit concentration, investment flows, and externality bearing across the three nations. If multinational firms capture 60%+ of gains while bearing <20% of labor/environmental costs, extraction is structural. If gains distribute within 80/20 range of population share, friction is normal coordination cost.',
    'If extraction: Tangled Rope/Snare classification holds. If coordination friction: classification shifts toward pure Rope. Current high suppression score reflects observed wage suppression and labor standards erosion; empirical distribution analysis would validate or revise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_comparative_advantage, empirical, 'Whether suppression reflects asymmetric extraction or normal coordination costs').

omega_variable(
    decoupling_feasibility_timeline,
    'What is the feasible timeline and cost for a nation to decouple from North American supply chains without economic catastrophe?',
    'Industrial policy modeling: estimate time to rebuild domestic supply capacity, reorient trade relationships, and restructure labor markets. Cross-reference with historical precedent (post-NAFTA trade negotiations, COVID supply chain disruptions, semiconductor reshoring efforts).',
    'If decoupling < 3 years at moderate cost: exit options upgrade from ''trapped'' to ''constrained'' for labor communities and governments. If decoupling > 10 years at catastrophic cost: confirms structural entrapment. Timeline directly affects chi calculation for powerless and institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoupling_feasibility_timeline, empirical, 'Feasible timeline and cost for supply chain decoupling').

omega_variable(
    multinational_extractive_agency,
    'Do integrated multinationals actively engineer supply chain lock-in (deliberate extraction) or is lock-in a side effect of efficiency optimization?',
    'Documentary analysis: internal corporate strategy documents, supply chain design choices, labor relation policies. Behavioral markers: Do firms deliberately maintain supplier dependence, suppress local wages through monopsony power, or actively resist labor organizing? Or do these outcomes emerge from supply chain optimization algorithms indifferent to distributional effects?',
    'If deliberate: Snare characterization strengthened for victim perspectives. If accidental: Tangled Rope (genuine coordination + unintended extraction) holds. Agency distinction affects narratives about reform possibility — intentional extraction requires confrontation; accidental extraction might respond to regulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multinational_extractive_agency, conceptual, 'Whether supply chain lock-in is deliberate extraction or side effect of efficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_american_supply_chain_coupling, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nasc_tr_t0, north_american_supply_chain_coupling, theater_ratio, 0, 0.32).
narrative_ontology:measurement(nasc_tr_t10, north_american_supply_chain_coupling, theater_ratio, 10, 0.4).
narrative_ontology:measurement(nasc_tr_t20, north_american_supply_chain_coupling, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(nasc_be_t0, north_american_supply_chain_coupling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nasc_be_t10, north_american_supply_chain_coupling, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(nasc_be_t20, north_american_supply_chain_coupling, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_american_supply_chain_coupling, resource_allocation).
narrative_ontology:affects_constraint(north_american_supply_chain_coupling, us_mexico_wage_differential_extraction).
narrative_ontology:affects_constraint(north_american_supply_chain_coupling, supply_chain_concentration_risk).
narrative_ontology:affects_constraint(north_american_supply_chain_coupling, geopolitical_decoupling_feasibility).

% DUAL FORMULATION NOTE:
% North American supply chain coupling decomposes into three distinct constraints: (1) wage differential extraction (labor market arbitrage by multinationals), (2) supply chain concentration (systemic risk from geographic clustering), and (3) geopolitical decoupling costs (strategic autonomy loss). This story models the integrated coupling system. The decomposed stories track specific mechanisms and have different epsilon values reflecting their distinct empirical status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_american_supply_chain_coupling, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
