% ============================================================================
% CONSTRAINT STORY: indian_import_tariffs_eu
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_import_tariffs_eu, []).

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
 *   constraint_id: indian_import_tariffs_eu
 *   human_readable: Indian Protective Tariffs on European Union Imports (Autos & Spirits)
 *   domain: economic/political
 *
 * SUMMARY:
 *   India's protective tariffs on European Union automotive and spirits
 *   imports represent a structured tension between legitimate infant-industry
 *   protection and extractive rent-seeking. The constraint emerged in the
 *   early 2000s when India's automotive and distilled spirits sectors were
 *   nascent, facing mature EU competition. Tariffs (reaching 150% on imported
 *   autos, 150-200% on imported spirits) were justified as necessary shields
 *   for developing domestic capacity. Over two decades, the constraint has
 *   evolved: domestic Indian manufacturers (Maruti, Bajaj, Diageo India
 *   subsidiaries) have achieved significant scale and sophistication, yet
 *   tariff rates remain high, showing signs of institutional inertia. The
 *   measured extractiveness has risen from 0.35 to 0.58 over the interval,
 *   and theater_ratio has grown from 0.40 to 0.55, indicating that the
 *   protective function (genuine coordination benefit) has decayed while the
 *   extraction mechanism (price-support, revenue capture, producer surplus)
 *   has strengthened. The constraint exhibits all six classification types
 *   depending on perspective: pure extraction (snare) for consumers and
 *   foreign exporters; coordination (rope) for Indian industry; hybrid
 *   (tangled_rope) from analytical view; temporary (scaffold) from global
 *   liberalization advocates (though empirically weak); degraded (piton) from
 *   the perspective of protectionist institutional arrangements persisting
 *   beyond functional necessity.
 *
 * KEY AGENTS:
 *   - Indian automotive manufacturers (Maruti Suzuki, Hyundai India, Tata Motors): Primary beneficiary (institutional/arbitrage) — protected from EU competition, capture domestic market share premium, control pricing
 *   - Indian spirits distillers (Diageo India, Pernod Ricard India, Radico Khaitan): Primary beneficiary (institutional/arbitrage) — tariff-protected from imported premium spirits, maintain margin premium
 *   - EU automotive exporters (Volkswagen, BMW, Mercedes, Audi): Primary victim (moderate/constrained) — high tariffs reduce export volumes, trigger FDI localization as partial circumvention
 *   - EU spirits exporters (Pernod Ricard international, Rémy Cointreau, Bacardi): Primary victim (moderate/constrained) — tariffs lock them out of premium import segment, force local-subsidiary models
 *   - Indian consumers (especially middle/upper-income households seeking premium vehicles and imported spirits): Primary victim (powerless/trapped) — high prices due to tariffs, restricted choice, no exit option
 *   - WTO and multilateral trade institutions: Secondary actor (organized/constrained) — dispute resolution mechanism; enforcement authority but limited power over sovereign tariff-setting
 *   - Indian government (Ministry of Commerce, Finance): Primary beneficiary (institutional/arbitrage) — tariff revenue, political support from domestic industry, policy autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_import_tariffs_eu, 0.58).
domain_priors:suppression_score(indian_import_tariffs_eu, 0.72).
domain_priors:theater_ratio(indian_import_tariffs_eu, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_import_tariffs_eu, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(indian_import_tariffs_eu, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_import_tariffs_eu, tangled_rope).
narrative_ontology:human_readable(indian_import_tariffs_eu, "Indian Protective Tariffs on European Union Imports (Autos & Spirits)").
narrative_ontology:topic_domain(indian_import_tariffs_eu, "economic/political").

domain_priors:requires_active_enforcement(indian_import_tariffs_eu).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_automotive_manufacturers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_spirits_distillers).
narrative_ontology:constraint_beneficiary(indian_import_tariffs_eu, indian_government_revenue).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, eu_automotive_exporters).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, eu_spirits_exporters).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, indian_consumers).
narrative_ontology:constraint_victim(indian_import_tariffs_eu, trade_norm_liberalization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN CONSUMERS (SNARE) — Trapped within Indian borders by residency, cannot freely access EU imports at global prices. High tariffs artificially restrict choice and raise prices on premium automotive and spirits categories. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Pure extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EU EXPORTERS (SNARE) — Constrained by WTO rules, bilateral trade agreements, and retaliation risks. Cannot easily exit Indian market without abandoning market share to competitors or relocating production. High tariffs (150% on autos, 150-200% on spirits) extract rents from their sales volumes or force production relocation. d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.85. High effective extraction.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EU INSTITUTIONAL ACTORS (TANGLED ROPE) — WTO dispute mechanisms, bilateral negotiations (e.g., EU-India trade talks), and reciprocal tariff threats create mixed coordination and extraction. EU sees tariff as violation of MFN principles (coordination norm) but also uses reciprocal tariffs as leverage. Constrained by multilateral rules but benefit from negotiation platforms that allow sectoral concessions. d≈0.68, f(d)≈1.05, σ=1.1 → χ≈0.62.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIAN GOVERNMENT / DOMESTIC INDUSTRY (ROPE) — Primary beneficiary with high agency and exit options (set tariff rates, adjust schedules, negotiate with trading partners). Tariffs solve coordination problem: protecting nascent/developing domestic automotive and spirits industries from mature EU competitors. Benefits include revenue capture, local industry development, and domestic political support from manufacturing sectors. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Negative effective extraction = net beneficiary. Coordination function: tariff protects infant industries and domestic labor, enabling coordinated industrial development.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL LIBERALIZATION COALITION (SCAFFOLD) — Multilateral organizations (WTO), trade economists, and liberalization advocates see tariffs as temporary barriers in a longer trend toward integration. Empirically weak: tariffs have persisted 15+ years without scheduled sunset. theater_ratio=0.55 reflects partial performativity (tariffs justify domestic political positions but don't fully block trade through production relocation, smuggling, or gray-market channels). Coalition has mobile options (negotiations, investor pressure, supply chain restructuring). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.43. But has_sunset_clause is empirically false — no formal sunset scheduled. This perspective is aspirational rather than structural.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PROTECTIONIST INSTITUTIONAL INERTIA (PITON) — Tariff structure persists through political-economy inertia: organized domestic manufacturers lobby for continuation, government revenue dependency increases, and tariff schedules become embedded in budget projections. theater_ratio=0.55 reflects performative function (tariffs are justified as 'infant industry protection' or 'strategic autonomy' despite limited evidence of industrial maturation or genuine strategic necessity). The functional protection has likely peaked; tariffs now persist partly through institutional momentum. d≈0.12, f(d)≈0.02, σ=1.0 → χ≈0.01. Very low effective extraction from this perspective — the institutional actor sees itself as maintaining a policy regime, not as extracting rents.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, tariffs are a hybrid mechanism: they genuinely enable infant industry protection (coordination benefit, rare in trade policy) while simultaneously extracting rents from consumers and foreign exporters. The constraint is neither pure extraction (snare) nor pure coordination (rope) — it requires active institutional enforcement and produces asymmetric distribution of gains. Beneficiaries (Indian industry, government) gain more than coordination benefit would justify; victims (consumers, EU exporters) lose more than the consumer-protection rationale explains. χ is constrained by the WTO dispute mechanism (if extraction becomes visible, retaliation becomes likely). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(indian_import_tariffs_eu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_import_tariffs_eu_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_import_tariffs_eu, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_import_tariffs_eu, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_import_tariffs_eu, TR),
    TR >= 0.70.

:- end_tests(indian_import_tariffs_eu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. Initial tariffs (2000-2005) had high legitimate protection function (ε≈0.35) — Indian auto/spirits industries were genuinely nascent and required protection from mature EU competition. Over 20 years, domestic industries achieved scale and efficiency (Maruti captures 40% Indian auto market; Diageo India is profitable and competitive), reducing the coordination justification for tariffs. Current extractiveness (0.58) reflects that tariffs now function primarily as producer-surplus capture and consumer-price support rather than infant-industry protection. The rise from 0.35 to 0.58 indicates Goodhart drift: the original protection rationale has largely succeeded, but the tariff mechanism persists and extracts rents. Suppression (0.72): High and stable. Tariffs suppress alternatives through four mechanisms: (1) price mechanism (150% tariff makes EU imports unaffordable except for ultra-premium segment), (2) regulatory/customs barriers (documentation, certification requirements), (3) political risk (potential retaliation if India removes tariffs), (4) sunk costs (domestic suppliers have organized to defend tariff regime). Theater ratio (0.55): Moderate and rising. Initial justification was genuine infant-industry protection (low theater, high function). Current discourse emphasizes 'Make in India,' 'Atmanirbhar Bharat' (self-reliance), and 'strategic autonomy,' which are partially performative — they justify tariffs in terms of national development while the mechanism functions largely as producer-protection. Theater has increased from 0.40 (simple protection) to 0.55 (multi-layered nationalist framing) as the functional justification has weakened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a large perspectival gap driven by structural position and exit options. Indian consumers and EU exporters both see snare classification — they are trapped (consumers by residency, exporters by market commitment and WTO rules) and cannot escape the tariff extraction. Indian industry and government see rope or beneficiary-aligned classification — they experience the tariff as coordination mechanism that solves the collective action problem of competing with mature EU firms while building domestic capacity. EU institutions see tangled_rope — tariff violates WTO MFN norms (extraction framing) but also provides negotiating surface for bilateral concessions (coordination framing). The global liberalization coalition sees scaffold (temporary barrier to eventual free trade), but this perspective is empirically weak — no scheduled sunset exists, and tariffs show signs of institutional inertia rather than planned phase-out. The institutional inertia perspective sees piton — tariffs persist through political economy momentum (domestic manufacturer lobbying, revenue dependency, nationalist framing) despite weakened functional justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Indian consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Consumers cannot exit India, cannot freely access EU imports, bear full cost of tariff as higher prices and restricted choice. EU exporters: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. Can partially exit via FDI relocation (Volkswagen Group India, Pernod Ricard India subsidiaries), but constrained by capital requirements, scale thresholds, and residual tariff/regulatory barriers. WTO institutions: Victim + constrained → d≈0.68, f(d)≈1.05. Moderate extraction. Can activate dispute mechanisms but constrained by WTO enforcement gaps and India's political autonomy. Indian government/industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Maximum beneficiary position. High agency (set tariff rates, adjust schedules), multiple exit options (negotiate removal, adjust rates, relocate industry), net extraction negative (they extract rents from the constraint). Global liberalization coalition: Mobile agent → d≈0.50, f(d)≈0.65. Moderate effective extraction. But this is a false scaffold — no real sunset. Protectionist institutional inertia: Institutional + constrained → d≈0.12, f(d)≈0.02. Very low extraction from this perspective; the institutional actor views itself as maintaining policy, not extracting.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's core mandatrophy is: 'Is this protection (coordination) or extraction (snare)?' The answer is contingent on temporal decomposition. At inception (2000), tariffs were genuine coordination mechanism solving infant-industry problem: extractiveness≈0.35, protection function was dominant, tangled_rope classification was justified. After 20 years (2020), domestic industries have matured, yet tariffs persist at high rates, increasing extractiveness to 0.58 while protection rationale weakens. The constraint has shifted from tangled_rope (legitimate hybrid) toward snare (degraded extraction) as the coordination function decayed. This is NOT a failure of the classification system — it's a success. The system detects the temporal drift: rising extractiveness + stable suppression + rising theater_ratio = signal of institutional inertia and rent-seeking replacing legitimate coordination. The mandatrophy resolves by recognizing that the same tariff schedule can be BOTH legitimate protection (at t=0) and extractive rent-seeking (at t=20) depending on whether the protection function is still needed. Policy response: phase-out schedule keyed to measurable industry maturity metrics (R&D investment, export share, cost competitiveness) would convert snare → scaffold, making the extraction explicit and time-limited rather than hidden and perpetual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infant_industry_maturation_threshold,
    'Has the Indian automotive and spirits industry reached maturity sufficient to justify phase-down of tariff protection, or is protection now primarily extractive rent-seeking?',
    'Comparative analysis: Indian auto/spirits industry metrics (R&D investment, export share, quality metrics, cost competitiveness vs EU) at tariff inception (2000s) vs present; identification of cost declines attributable to protection vs global innovation; assessment of whether domestic industry would collapse if tariffs were removed (true infant industry) or would consolidate with lower margins (rent-extraction phase).',
    'If maturity threshold passed: tariff classification should shift from tangled_rope toward snare (coordination function lost, extraction mechanism persists). If not passed: tangled_rope classification is justified, and phase-down timeline is policy question. If permanently extended without metrics: piton classification dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infant_industry_maturation_threshold, empirical, 'Whether Indian industry has matured beyond infant industry protection').

omega_variable(
    wto_dispute_escalation_trajectory,
    'Will WTO dispute resolution mechanisms force tariff reductions, or do power asymmetries and enforcement gaps allow India to sustain high tariffs indefinitely?',
    'Analysis of WTO case docket (EU v. India tariff disputes), historical precedent for similar cases, India''s compliance history with dispute settlements, and measurement of political cost to India of continued non-compliance vs domestic political benefit of maintaining tariffs.',
    'If WTO enforcement strengthens: scaffold perspective becomes empirically valid (sunset becomes likely through institutional pressure). If WTO enforcement weakens: tangled_rope persists indefinitely, morphing toward piton as political justification decays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wto_dispute_escalation_trajectory, empirical, 'Whether WTO mechanisms can force tariff reduction').

omega_variable(
    production_relocation_feasibility,
    'Can EU automotive/spirits firms relocate production to India or third countries with tariff access, thereby circumventing tariff extraction, or do tariff barriers make relocation uneconomical?',
    'Analysis of recent FDI in automotive and spirits sectors in India; measurement of tariff rate vs relocation cost differential; identification of cases where EU firms have established Indian subsidiaries in response to tariffs (partial circumvention) vs cases where tariffs have caused market exit (extraction success).',
    'If relocation is feasible: tariff extraction is constrained by capital mobility; effective extraction χ is lower than base extractiveness suggests; snare classification downgraded. If relocation is blocked (by investment barriers, scale requirements): tangled_rope classification confirmed (extraction is structurally enforced despite coordination framing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(production_relocation_feasibility, empirical, 'Whether tariff barriers can be circumvented by production relocation').

omega_variable(
    domestic_consumer_welfare_trade_off,
    'What magnitude of consumer welfare loss (higher prices, reduced choice) is justified by infant industry protection gains, and is that trade-off being made explicitly or hidden?',
    'Econometric analysis: comparison of consumer prices for tariffed goods in India vs global benchmarks; quantification of consumer surplus loss; comparison with measured benefits to domestic industry (employment, wage growth, R&D investment); assessment of whether trade-off is transparent in policy discourse or obscured.',
    'If trade-off is made explicit and justified: tangled_rope classification is acceptable within a legitimate policy framework. If consumer costs are hidden or misrepresented: classification shifts toward snare (extraction disguised as protection), revealing mandatrophy failure in policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_consumer_welfare_trade_off, empirical, 'Magnitude and transparency of consumer welfare trade-off').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_import_tariffs_eu, 2000, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itar_tr_t0, indian_import_tariffs_eu, theater_ratio, 0, 0.4).
narrative_ontology:measurement(itar_tr_t5, indian_import_tariffs_eu, theater_ratio, 5, 0.48).
narrative_ontology:measurement(itar_tr_t10, indian_import_tariffs_eu, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(itar_be_t0, indian_import_tariffs_eu, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(itar_be_t5, indian_import_tariffs_eu, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(itar_be_t10, indian_import_tariffs_eu, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_import_tariffs_eu, resource_allocation).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, wto_most_favored_nation_principle).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, eu_india_trade_reciprocity).
narrative_ontology:affects_constraint(indian_import_tariffs_eu, domestic_industry_protection_race).

% DUAL FORMULATION NOTE:
% Indian tariffs on EU imports are downstream of the broader WTO MFN principle (which they technically violate) and the infant-industry exception (which justifies them). The constraint family decomposes into: (1) the MFN principle as a mountain-like coordination norm, (2) the infant-industry exception as a legitimate scaffold, and (3) the specific Indian tariff schedule as a tangled_rope that increasingly resembles snare as industries mature. Each has distinct ε: MFN≈0.05 (mountain), infant-industry exception≈0.25 (rope with sunset), Indian tariffs≈0.58 (tangled_rope drifting toward snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_import_tariffs_eu, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
