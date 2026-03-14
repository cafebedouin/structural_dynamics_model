% ============================================================================
% CONSTRAINT STORY: us_tariff_escalation_trade_war
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_tariff_escalation_trade_war, []).

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
 *   constraint_id: us_tariff_escalation_trade_war
 *   human_readable: US Tariff Escalation Trade War
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   US tariff escalation functions as a hybrid coordination-extraction
 *   mechanism that reveals deep structural asymmetries in international trade
 *   relationships. The tariff policy simultaneously coordinates political
 *   constituencies around protectionism (expressing constituent demand for
 *   sector protection), extracts value from import-dependent consumers
 *   through price increases, maintains distributive asymmetries across
 *   geographic regions and supply-chain positions, and generates
 *   administrative theater through tariff bureaucracy. The constraint
 *   exhibits all six DR types depending on perspective: pure extraction
 *   (Snare) from the powerless consumer and retaliating exporter,
 *   coordination (Rope) from protected domestic sectors, mixed
 *   extraction-coordination (Tangled Rope) from small retailers and the
 *   government itself, temporary problem with sunset (Scaffold) from
 *   organized trade coalitions, degraded ritual (Piton) from tariff
 *   bureaucracy, and false natural law (Mountain) from civilizational
 *   analytics. The measurements show escalating extractiveness and theater
 *   ratio over the interval, reflecting accumulating tariff rates,
 *   retaliatory cycles, and administrative complexity. The theater increase
 *   reflects that tariff justifications have become increasingly performative
 *   as economic rationales (national security, reciprocal fairness) compete
 *   with political motivations (protecting constituencies, symbolic
 *   nationalism).
 *
 * KEY AGENTS:
 *   - Import-Dependent Consumers: Primary victim (powerless/trapped) — bear diffuse costs through price increases; no exit from tariff pass-through
 *   - Protected Domestic Sectors: Primary beneficiary (institutional/arbitrage) — gain from reduced import competition and can negotiate carve-outs; strong agency
 *   - Small Retailers: Secondary victim (moderate/constrained) — face conflicting pressures (competition reduction vs cost increases); cannot negotiate tariff terms
 *   - Retaliating Exporters: Secondary victim (powerless/trapped) — foreign sectors targeted by US counter-tariffs; trapped in escalation cycle they cannot exit unilaterally
 *   - US Government: Mixed role (institutional/constrained) — coordinates protectionist coalition while extracting revenue; constrained by political costs of unilateral elimination
 *   - Trade Coalition (WTO, multinational firms): Organized actors (organized/constrained) — perceive tariffs as temporary negotiable mechanism with sunset through trade agreements
 *   - Tariff Bureaucracy: Institutional actor (institutional/arbitrage) — maintains administrative apparatus; experiences degraded ritual; has arbitrage options through classification and exemption powers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as immutable feature of competitive international relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_tariff_escalation_trade_war, 0.58).
domain_priors:suppression_score(us_tariff_escalation_trade_war, 0.65).
domain_priors:theater_ratio(us_tariff_escalation_trade_war, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_tariff_escalation_trade_war, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_tariff_escalation_trade_war, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_tariff_escalation_trade_war, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_tariff_escalation_trade_war, tangled_rope).
narrative_ontology:human_readable(us_tariff_escalation_trade_war, "US Tariff Escalation Trade War").
narrative_ontology:topic_domain(us_tariff_escalation_trade_war, "economic/geopolitical").

domain_priors:requires_active_enforcement(us_tariff_escalation_trade_war).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_tariff_escalation_trade_war, domestic_protected_sectors).
narrative_ontology:constraint_beneficiary(us_tariff_escalation_trade_war, us_government_revenue).
narrative_ontology:constraint_beneficiary(us_tariff_escalation_trade_war, political_constituencies).
narrative_ontology:constraint_victim(us_tariff_escalation_trade_war, import_dependent_consumers).
narrative_ontology:constraint_victim(us_tariff_escalation_trade_war, global_supply_chains).
narrative_ontology:constraint_victim(us_tariff_escalation_trade_war, retaliating_exporters).
narrative_ontology:constraint_victim(us_tariff_escalation_trade_war, small_retailers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPORT-DEPENDENT CONSUMER (SNARE) — Trapped within tariff pass-through with no exit options. Wage earner cannot negotiate international trade policy; tariff costs are absorbed as higher prices on electronics, textiles, household goods. No collective action mechanism. Maximum extraction experienced with minimal coordination function.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL RETAILER (TANGLED ROPE) — Constrained by supply chain dependencies and capital costs of inventory. Benefits from coordination function: tariffs reduce large-scale import competition, protecting local retail margins. But tariffs also increase cost of goods sold, reducing profitability. High suppression (cannot exit supply chains or renegotiate tariff schedules) combined with asymmetric extraction (big-box retailers absorb tariffs differently than independents). Mixed experience: some coordination benefit alongside significant extraction.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROTECTED DOMESTIC SECTOR (ROPE) — Experiences tariffs as pure coordination mechanism for collective action. Steel mills, auto manufacturers, semiconductor producers benefit from reduced import competition. Exit option is arbitrage: can shift production overseas, negotiate tariff carve-outs, or lobby for exemptions. Net beneficiary with strong agency.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RETALIATING EXPORTER (SNARE) — Trapped by tariff escalation cycle with no unilateral exit. Foreign governments impose counter-tariffs on US agricultural and industrial exports; US farmers and manufacturers cannot escape this retaliation through individual action. High suppression and extraction: locked into trade war dynamics by national policy.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADE COALITION (SCAFFOLD) — Organized actors (WTO, multinational corporations, export councils) perceive tariffs as a temporary extraction mechanism with a sunset: negotiated trade agreements, USMCA-style deals, and dispute resolution mechanisms represent pathways to tariff reduction. These coalitions have agency and see an exit path through renegotiation. Theater is moderate (tariff policy is substantive, not purely performative), and suppression declines if negotiated agreements materialize.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TARIFF BUREAUCRACY (PITON) — The enforcement apparatus (Commerce Department, Trade Representative office) maintains tariff administration rituals that are substantially performative. Detailed product classification, rule-of-origin determinations, and exemption petitions generate administrative theater: the machinery of tariff policy persists through institutional inertia even when functional coordination goals could be served by simpler mechanisms. The bureaucracy has arbitrage options (reclassification authorities, exemption powers) and experiences tariffs as a degraded system maintained because alternatives haven't fully displaced it.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: US GOVERNMENT (TANGLED ROPE) — Experiences tariffs as coordinating both a political coalition (protectionist constituencies) and extracting revenue (tariff income, leverage over trade partners). Exit options are constrained: unilateral tariff elimination triggers domestic political costs; but tariff maintenance requires continuous enforcement and retaliation management. Genuine coordination function (expressing constituent preferences) combined with asymmetric extraction (concentrated benefits to protected sectors, diffuse costs to consumers). Active enforcement required.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the tariff-retaliation cycle appears as an immutable feature of competitive international relations: when trade imbalances exist, protectionist pressure is inevitable. This perspective sees tariff escalation as a natural law of political economy — states always have incentive to protect domestic sectors when facing trade deficits. However, structural data contradicts this naturalization: tariff policy is contingent (negotiable, reversible), and the extraction mechanism is institutional, not physical.
constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_tariff_escalation_trade_war_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_tariff_escalation_trade_war, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_tariff_escalation_trade_war, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_tariff_escalation_trade_war, TR),
    TR >= 0.70.

:- end_tests(us_tariff_escalation_trade_war_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The tariff mechanism extracts from import-dependent consumers and retaliating exporters, but extraction is not maximal because: (1) protected sectors do provide genuine coordination function (expressing political demand), (2) tariff revenue accrues to government (not private extraction), and (3) organized trade coalitions have negotiation pathways. The value reflects substantial extraction (price passthrough to consumers, retaliation on exporters) combined with genuine coordination benefits for protected constituencies. Suppression (0.65): High. Significant structural barriers to exit: consumers cannot negotiate tariff pass-through, small retailers cannot renegotiate supply chains, retaliating exporters cannot escape counter-tariffs unilaterally, and US workers in exposed sectors cannot shift to non-tariff-affected employment instantly. Suppression rises as tariff rates escalate and retaliation broadens. Theater ratio (0.55): Moderate. Tariff policy is substantive (not purely performative) — it genuinely transfers resources and changes relative prices. But theater increases during escalation cycles: political justifications (national security, reciprocal fairness) diverge from economic rationales, and detailed tariff classification becomes increasingly baroque. Theater has risen from 0.38 to 0.55 over the interval as administrative complexity and rhetorical justification have accumulated.
 *
 * PERSPECTIVAL GAP:
 *   The central perspectival gap separates protected sectors (who perceive coordination) from import-dependent consumers (who perceive pure extraction). The gap reflects genuine structural asymmetry: tariff policy simultaneously serves a coordination function (expressing constituency demand) and an extraction function (transferring wealth to protected sectors via consumer surplus loss). Neither perspective is false — both capture real structural features. The gap widens when tariff rates escalate and retaliation broadens, because coordination benefits to protected sectors become increasingly concentrated while extraction costs diffuse across consumers. Small retailers occupy a liminal position: they benefit from reduced import competition but lose from input cost increases, making their perspective (Tangled Rope) a genuine hybrid. The organized trade coalition perspective (Scaffold) represents a distinct gap: organized actors with negotiation capacity perceive tariffs as reversible (sunset logic), while powerless agents perceive them as immutable (Mountain or Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain flows from beneficiary/victim declarations plus exit options. Protected domestic sectors are declared beneficiaries with institutional power and arbitrage exit options — they derive low d (~0.15), experiencing negative or minimal effective extraction chi. Import-dependent consumers are victims with powerless status and trapped exit — they derive high d (~0.95), experiencing maximum extraction chi. Small retailers are both partly beneficiary (reduced competition) and partly victim (cost increases), with moderate power and constrained exit — they derive moderate-high d (~0.60), experiencing moderate extraction chi. Retaliating exporters are victims with powerless status and trapped exit — they derive high d, experiencing maximum extraction. US government benefits from revenue and coordination while facing constrained exit — it derives moderate-high d (~0.65), reflecting that tariff continuation entails significant management costs (retaliation cycles, political pressure). Tariff bureaucracy, positioned as institutional beneficiary with arbitrage, derives low d despite facilitating extraction, because their structural role is administrative. The scope modifier σ(S) amplifies extraction at national scale (σ=1.0) and global scale (σ=1.2) where retaliatory dynamics compound effects, compared to local scale (σ=0.8) where tariff impacts are more contained.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that tariff policy genuinely coordinates (protectionist constituencies express demand) while genuinely extracting (consumers lose surplus, exporters face retaliation). The classification as Tangled Rope is not a compromise — it reflects structural features that would be missed by pure coordination (Rope) or pure extraction (Snare) classifications. The mandatrophy failure would occur if the policy were mislabeled as pure Rope (hiding extraction costs) or pure Snare (denying coordination function). The perspectival gap demonstrates why: from the beneficiary position, tariffs solve a real collective action problem (competing sectors have incentive to fragment, tariff coordinates them); from the victim position, tariffs are pure extraction with no coordination benefit. Both are correct from their structural positions. The Tangled Rope classification insists on holding both truths simultaneously, preventing the false simplification that political coordination justifies economic extraction. The measurements (rising extractiveness and theater) confirm that the coordination function is diminishing relative to extraction as tariff rates accumulate and retaliation broadens — the constraint is drifting toward Snare as it matures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tariff_revenue_vs_protection_motive,
    'Is the primary function of tariffs to raise government revenue or to protect domestic sectors from import competition?',
    'Analysis of tariff schedule design: if structured to maximize revenue, tariffs would be uniform across sectors; if structured to protect, tariffs concentrate on politically sensitive sectors (steel, autos, agriculture). Comparison of tariff revenue collected vs protection value provided to domestic producers.',
    'If revenue-driven: extractiveness is higher (pure rent extraction from consumers). If protection-driven: extractiveness is lower and coordination function is stronger. Classification could shift from Snare (revenue) to Tangled Rope (protection with mixed coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tariff_revenue_vs_protection_motive, empirical, 'Whether tariffs function primarily as revenue or protection mechanism').

omega_variable(
    retaliation_escalation_termination,
    'What mechanism terminates the tariff-retaliation cycle, and does the cycle have a natural equilibrium or structural endpoint?',
    'Historical analysis of past tariff wars (Smoot-Hawley, 1980s auto VRAs, early-2000s steel tariffs); tracking of trade war trajectories; identification of negotiated endpoints vs unilateral de-escalation patterns.',
    'If cycle is self-terminating through negotiation: scaffold perspective is realistic, and the constraint has genuine sunset logic. If cycle persists absent external shock: the constraint is more extractive (Snare) than coordination-based (Rope), and termination requires power asymmetry or exhaustion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_escalation_termination, empirical, 'Whether tariff cycles have natural termination mechanisms').

omega_variable(
    consumer_price_passthrough_heterogeneity,
    'Do tariff costs pass through uniformly to consumer prices, or do supply-chain actors absorb costs unevenly based on market power?',
    'Price-tracking analysis: comparison of tariff rates to retail price increases for identical products across different market segments (luxury vs budget, concentrated vs competitive supply chains); measurement of margin compression vs price increases by retailer type.',
    'If uniform passthrough: extraction is simple and measurable (tariff = consumer cost). If heterogeneous: large retailers can absorb/negotiate tariffs while small retailers cannot, shifting extraction distribution and increasing suppression on constrained actors. Could raise small-retailer classification from Tangled Rope to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_price_passthrough_heterogeneity, empirical, 'Whether tariff costs pass through uniformly to consumer prices').

omega_variable(
    geographic_concentration_of_costs_benefits,
    'Are tariff costs and benefits geographically concentrated (regional impacts) or diffuse (national impacts)?',
    'Regional economic analysis: tariff impact mapping by congressional district, state, and sector; correlation of tariff benefits (protected sector employment) to tariff costs (higher consumer prices, retail job losses); identification of policy-winner and policy-loser geographies.',
    'If concentrated: the geographic losers experience maximum suppression and extraction (their region bears costs of national policy they did not choose). Perspectival gap widens between protected/protected-free regions. Could justify scaling suppression upward to reflect regional entrapment. If diffuse: suppression applies uniformly and national-scale coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_concentration_of_costs_benefits, empirical, 'Whether tariff impacts are geographically concentrated or diffuse').

omega_variable(
    retaliatory_capacity_asymmetry,
    'Do trading partners have symmetric capacity to retaliate against US tariffs, or is retaliation constrained by imbalanced trade relationships?',
    'Analysis of US export composition and trading partner tariff schedules; measurement of retaliation scope (what tariff rates are available on US exports) vs impact (what volume of US exports face increased tariffs). Identification of sectors where US is vulnerable vs protected from retaliation.',
    'If asymmetric: some trading partners are trapped (cannot retaliate proportionally) and experience high suppression. Others with large US export dependence can retaliate heavily. Retaliating exporters'' classification could shift from Snare (trapped) to Tangled Rope (constrained but with some agency through retaliation). Changes directionality distribution across global perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliatory_capacity_asymmetry, empirical, 'Whether retaliatory capacity is symmetric across trading partners').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_tariff_escalation_trade_war, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tariff_tr_t0, us_tariff_escalation_trade_war, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tariff_tr_t3, us_tariff_escalation_trade_war, theater_ratio, 3, 0.47).
narrative_ontology:measurement(tariff_tr_t6, us_tariff_escalation_trade_war, theater_ratio, 6, 0.55).
narrative_ontology:measurement(tariff_tr_t9, us_tariff_escalation_trade_war, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(tariff_be_t0, us_tariff_escalation_trade_war, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(tariff_be_t3, us_tariff_escalation_trade_war, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(tariff_be_t6, us_tariff_escalation_trade_war, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(tariff_be_t9, us_tariff_escalation_trade_war, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_tariff_escalation_trade_war, resource_allocation).
narrative_ontology:affects_constraint(us_tariff_escalation_trade_war, global_supply_chain_fragmentation).
narrative_ontology:affects_constraint(us_tariff_escalation_trade_war, geopolitical_trade_bloc_formation).
narrative_ontology:affects_constraint(us_tariff_escalation_trade_war, currency_manipulation_dynamics).

% DUAL FORMULATION NOTE:
% US tariff escalation interacts with three downstream constraints: global supply chain fragmentation (tariffs accelerate geographic concentration of production), geopolitical trade bloc formation (tariffs strengthen US vs China vs EU alignments), and currency manipulation dynamics (tariffs create devaluation incentives to offset effects). Each downstream constraint has its own epsilon reflecting distinct structural features; tariff escalation influences but does not determine their classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_tariff_escalation_trade_war, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
