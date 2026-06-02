% ============================================================================
% CONSTRAINT STORY: mercosur_tariff_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mercosur_tariff_harmonization, []).

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
 *   constraint_id: mercosur_tariff_harmonization
 *   human_readable: Mercosur Common External Tariff Harmonization
 *   domain: trade_policy/regional_integration
 *
 * SUMMARY:
 *   Mercosur tariff harmonization, formalized in 1995 with the Common
 *   External Tariff (CET), represents a structural commitment among four
 *   South American states (Brazil, Argentina, Paraguay, Uruguay) to maintain
 *   tariff alignment and restrict intra-regional variance. The constraint
 *   solves a real collective action problem — preventing unilateral tariff
 *   reductions that would undermine the regional protection bargain — while
 *   distributing costs asymmetrically across member states and domestic
 *   constituencies. Brazil and Argentina benefit from the enforcement of
 *   tariff discipline that enables them to maintain industrial protection;
 *   smaller members (Paraguay, Uruguay) face constrained policy autonomy;
 *   consumers across the region bear elevated prices; agricultural exporters
 *   in Uruguay lose competitiveness. The constraint exhibits all
 *   characteristics of a tangled rope: genuine coordination function
 *   (preventing a tariff race to the bottom), asymmetric extraction (costs
 *   concentrated on consumers and smaller states), active enforcement (CET
 *   committees, dispute mechanisms), and mixed power distributions across
 *   institutional actors. The theater ratio has risen over the interval as
 *   CET committee procedures have accumulated without corresponding deepening
 *   of actual tariff coordination — compliance disputes are now routed
 *   through increasingly elaborate institutional structures that produce
 *   limited tariff changes, suggesting the constraint is drifting toward
 *   piton (performative maintenance) as the core coordination problem becomes
 *   less acute and the institutional apparatus becomes an end in itself.
 *
 * KEY AGENTS:
 *   - Brazilian Industrial Protected Sectors: Primary beneficiary (institutional/arbitrage) — capture rents from tariff protection and enforce tariff discipline against undercutting
 *   - Argentine Protected Industry: Secondary beneficiary (powerful/constrained) — benefits from CET protection but faces constraints from smaller members' pressure for flexibility
 *   - Consumers Across Mercosur: Primary victim (powerless/trapped) — bear elevated prices from tariff protection with no organizational voice or exit option
 *   - Small Member States (Paraguay, Uruguay): Secondary victim/constrained beneficiary (powerful/constrained) — face policy autonomy reduction and suffer sectoral losses (Uruguay agriculture) while gaining access to regional market
 *   - Uruguayan Agricultural Exporters: Tertiary victim (powerful/trapped) — lose export competitiveness due to CET structure that disadvantages agricultural goods relative to manufactures
 *   - Mercosur Institutional Apparatus: Enforcer (institutional/constrained) — coordinates tariff discipline while selectively applying enforcement to benefit larger members
 *   - Regional Trade Labor Unions: Organized affected actor (organized/constrained) — benefit from industrial protection and employment stability but face wage suppression and reduced consumer purchasing power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional choice as inherent to regional integration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mercosur_tariff_harmonization, 0.52).
domain_priors:suppression_score(mercosur_tariff_harmonization, 0.58).
domain_priors:theater_ratio(mercosur_tariff_harmonization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, extractiveness, 0.52).
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(mercosur_tariff_harmonization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mercosur_tariff_harmonization, tangled_rope).
narrative_ontology:human_readable(mercosur_tariff_harmonization, "Mercosur Common External Tariff Harmonization").
narrative_ontology:topic_domain(mercosur_tariff_harmonization, "trade_policy/regional_integration").

domain_priors:requires_active_enforcement(mercosur_tariff_harmonization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mercosur_tariff_harmonization, brazilian_industrial_firms).
narrative_ontology:constraint_beneficiary(mercosur_tariff_harmonization, argentine_protected_sectors).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, small_state_policy_autonomy).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, consumer_purchasing_power).
narrative_ontology:constraint_victim(mercosur_tariff_harmonization, uruguayan_agricultural_exporters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER PURCHASING POWER (SNARE) — Trapped by tariff walls that raise domestic prices on manufactured goods. Cannot exit the regional market structure. Bears full cost of protection with no organizational voice. Extraction runs pure: prices remain artificially elevated to subsidize protected industrial sectors, with no coordination benefit to the consumer.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SMALL MEMBER STATE (TANGLED ROPE) — Constrained by exit costs (trade dependence, regional isolation if departing, retaliation risk from larger members). But also benefits from regional market access and industrial development support. Extracted from: tariff rates are set by larger members; smaller states have disproportionate veto power in theory but face defection penalties. Mixed extraction — some coordination benefit (access to Mercosur market) layered with asymmetric cost-bearing.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAZILIAN PROTECTED INDUSTRY (ROPE) — Primary beneficiary. Experiences CET as pure coordination: it enables them to coordinate with smaller members to maintain tariff discipline and block undercutting. Zero effective extraction for this actor — the constraint solves their core problem (preventing a tariff race to the bottom) while distributing costs elsewhere. Arbitrage option is strong (can exit and negotiate bilateral deals).
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGIONAL TRADE LABOR UNIONS (TANGLED ROPE) — Organized agents face both coordination benefits (stable employment in protected sectors, predictable industrial policy) and extraction (wage suppression in tariff-sheltered industries, lower purchasing power from elevated consumer prices). Exit is constrained by regional labor market dependence. Real coordination function (labor protections, sectoral bargaining) exists alongside extraction (inability to renegotiate wage shares in protected rents).
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: URUGUAYAN AGRICULTURAL EXPORTERS (SNARE) — Trapped by a constraint that sacrifices their export competitiveness to protect Brazilian and Argentine industry. Uruguay has comparative advantage in livestock and agricultural products but faces CET rates that subsidize manufactured imports from larger members and reduce demand for their exports. No coordination function for this group — pure extraction with no exit option short of regional departure.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: MERCOSUR INSTITUTIONS (TANGLED ROPE) — The common market secretariat and dispute mechanisms face mixed incentives. They coordinate genuine regional trade (coordination function), but also enforce tariff discipline that benefits larger members and constrains smaller ones. Enforcement is selective — Brazil and Argentina face lower discipline costs than Paraguay and Uruguay. Extraction runs through the institution (selective enforcement) while coordination also runs through (dispute resolution, tariff negotiation protocols).
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON FRAMING) — From a civilizational view, regional tariff harmonization could be seen as a necessary coordination mechanism in a fragmented global trade order. But the theater ratio (0.64) reveals that the CET regime maintains substantial performative infrastructure (committee structures, compliance monitoring, dispute panel procedures) that increasingly lacks functional power relative to bilateral trade agreements and supply-chain realities. The constraint persists through institutional inertia even as its core coordination function has atrophied.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL NATURAL LAW VIEW (MOUNTAIN) — Risk perspective that tariff harmonization appears as an immutable feature of regional integration: 'any customs union must harmonize tariffs.' But the structural data contradicts the mountain gate: tariff rates are continuously renegotiated, exemptions abound (automotive, sugar, ethanol special regimes), and the regime's extractive properties are contingent on political will, not physical necessity. False summit candidate — naturalization of a political arrangement.
constraint_indexing:constraint_classification(mercosur_tariff_harmonization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mercosur_tariff_harmonization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mercosur_tariff_harmonization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mercosur_tariff_harmonization, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mercosur_tariff_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mercosur_tariff_harmonization, TR),
    TR >= 0.70.

:- end_tests(mercosur_tariff_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The CET regime generates clear extraction from consumers (elevated prices), smaller-state policy autonomy (constrained tariff flexibility), and agricultural exporters (distorted incentive structure against agriculture). But extraction is not total because genuine coordination benefits exist: the constraint prevents a tariff race to the bottom that would benefit no one, enables smaller states to maintain industrial development capacity, and creates stable predictability for regional trade. The measurement captures this mixed profile. The trajectory over time shows extractiveness rising (0.35 → 0.52) as tariff rates have incrementally increased and the regime has locked in rather than evolved. Suppression (0.58): Moderate-high, stable across the interval. Barriers to exit include trade dependence (difficult for smaller members to renegotiate bilaterally), retaliation risk (departing members face trade punishment), institutional lock-in (dispute mechanisms and protocols create sunk costs), and lack of alternative regional arrangements. These are substantial barriers but not absolute — renegotiation is possible (as evidenced by various exemptions and special regimes), distinguishing this from total suppression (0.70+). Theater ratio (0.64): High and rising. CET committee structures, compliance monitoring, and dispute panels have proliferated without corresponding expansion in actual tariff changes. Many committee outputs are procedural rather than substantive. The rising trajectory (0.48 → 0.64) indicates the institutional apparatus is accumulating performance-of-coordination without deepening coordination itself, consistent with piton dynamics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The Brazilian protected industrial sector sees pure coordination (Rope) — the CET solves their problem of maintaining tariff discipline. The consumer sees pure extraction (Snare) — elevated prices with no exit. The small state (Paraguay/Uruguay) sees mixed extraction and benefit (Tangled Rope) — constrained but accessing the regional market. The Mercosur institution sees itself as enforcing coordination while selectively distributing extraction (Tangled Rope) — some institutional actors enforce discipline, others resist. The piton perspective reveals the institutional apparatus as increasingly performative — elaborate procedures generating limited actual tariff changes. The natural law perspective risks mischaracterizing the entire regime as inherent to regional integration, when it is actually a contingent institutional choice. The analytical observer's most valuable role is making visible what each actor takes for granted: the constraint simultaneously solves a real problem (preventing tariff race-to-the-bottom) and extracts value from those with least power to resist (consumers, small states, agricultural exporters).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from its structural position in the extraction flow. Brazilian protected industry: beneficiary + arbitrage option → low d (0.10-0.20) → negative or minimal χ. Consumer: victim + trapped → high d (0.85-0.95) → high χ. Small state: victim and beneficiary simultaneously, constrained options → moderate d (0.55-0.65) → moderate χ. Labor unions: organized victims with negotiation power → moderate-low d (0.50-0.60) → moderate χ. Mercosur institution: beneficiary of enforcement fees and institutional growth, but constrained by member states → moderate d (0.45-0.55) → moderate χ. The piton perspective itself is not indexed to a power atom — it is a observational classification revealing the theatrical quality of the institutional apparatus. The natural law perspective risks zero d (treating the constraint as background condition) but the engine's false summit detector will identify the beneficiary declarations (Brazilian industry, Argentine protected sectors) and flag the mountain classification as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that 'coordination or extraction?' is not a binary choice but an index-relative classification. The same rule (common external tariff of 35% on automotive, 20% on appliances, 15% on agriculture) solves the Brazilian industrial sector's coordination problem (maintaining protection) while extracting from consumers (elevated prices). The coordination and extraction are not sequential or conditional — they are simultaneous and inherent to the structure. The tangled rope type exists precisely to capture this simultaneity: genuine coordination function (preventing race-to-the-bottom) + asymmetric extraction (costs on consumers/small states) + active enforcement (tariff committees). Mandatrophy is resolved by recognizing that the constraint is tangled rope from the analytical observer's perspective (the structural reality is mixed), while individual actors perceive it from their position-specific index: rope for beneficiaries, snare for powerless victims. The natural law perspective's 'tariff harmonization is inherent to customs unions' would resolve mandatrophy only if empirical analysis confirmed that all working regional arrangements require perfect harmonization — but they do not, making the natural law claim a false summit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    brazil_hegemonic_extraction,
    'Does Brazil''s structural dominance mean the CET regime is fundamentally extractive from smaller members, or does Brazil''s enforcement of tariff discipline actually enable smaller states to maintain industrial protection they could not negotiate independently?',
    'Counterfactual analysis: compare Paraguay and Uruguay sectoral tariff rates (if negotiated bilaterally with Brazil) to actual CET rates. Measure whether smaller states would achieve higher or lower effective protection without harmonization.',
    'If smaller states benefit from CET discipline: reclassify from snare to tangled_rope for small-state perspective. If extraction exceeds benefit: snare classification confirmed. The mandatrophy hinges on this empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brazil_hegemonic_extraction, empirical, 'Whether Brazil''s tariff enforcement benefits or exploits smaller members').

omega_variable(
    consumer_price_pass_through,
    'What fraction of CET-induced price elevations are passed through to consumers vs absorbed by retailer margins, distribution rents, or currency exchange dynamics?',
    'Price surveys across Mercosur members: comparison of identical manufactured goods in tariff-protected vs tariff-free product categories, controlling for exchange rate and logistics costs.',
    'If pass-through is high (>0.80): consumer extraction is severe; snare classification for consumer perspective confirmed. If pass-through is low (<0.50): extraction is primarily a transfer to retailers and distributors, not consumers; reclassify to piton (performative extraction). If mixed: tangled_rope applies to consumer perspective (some coordination benefit in the form of price stability, some extraction in elevated prices).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_price_pass_through, empirical, 'Fraction of tariff-induced price increases passed through to consumers').

omega_variable(
    regional_supply_chain_coordination,
    'To what extent does Mercosur tariff harmonization enable genuine supply-chain integration (intra-regional intermediate goods trade), vs functioning primarily as a protectionist cartel?',
    'Supply-chain decomposition: measure intra-regional value-added in manufactured trade. Compare growth rates of intermediate vs final-good trade within Mercosur since CET adoption. Benchmark against pre-1995 and counterfactual bilateral scenarios.',
    'If supply-chain coordination is strong: coordination function is real; rope classification strengthens for Brazilian/Argentine perspectives. If integration is weak: CET functions primarily as extraction; snare and tangled_rope classifications confirmed. The constraint''s true type hinges on whether the tariff regime solves a real coordination problem or merely distributes rents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_supply_chain_coordination, empirical, 'Whether CET enables genuine supply-chain integration or functions as protectionist cartel').

omega_variable(
    exit_cost_measurement,
    'What is the actual cost to Paraguay or Uruguay of departing Mercosur vs negotiating bilateral tariff agreements with Brazil and Argentina?',
    'Trade gravity modeling: estimate trade volumes and tariff revenue under withdrawal scenarios. Include retaliation penalties, loss of dispute panel access, and renegotiation deadweight loss. Compare to status quo and bilateral alternatives.',
    'If exit cost is high (>30% trade loss): trapped classification confirmed for small states. If exit cost is moderate (10-30%): constrained classification confirmed. If exit cost is low (<10%): mobile classification implied; small-state perspective would shift toward rope or scaffold. The suppression metric depends critically on this measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Actual trade and welfare cost to small members of Mercosur withdrawal').

omega_variable(
    natural_law_naturalization_risk,
    'Is the mountain perspective''s claim that ''tariff harmonization is inherent to customs unions'' empirically grounded, or does it naturalize a contingent institutional choice?',
    'Historical and comparative analysis: examine African Union, ASEAN, and other regional blocs to measure: (a) how many require perfect tariff harmonization vs allow flexibility, (b) whether harmonization correlates with integration success or is orthogonal to it, (c) how often harmonization clauses are invoked vs overridden in practice.',
    'If harmonization is functionally necessary for regional stability: mountain classification (at civilizational scale) is justified. If harmonization is one design choice among many: false summit confirmed; engine reclassifies to tangled_rope based on beneficiary declarations. The natural law claim will fail the accessibility_collapse and resistance gates if alternative institutional designs exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_naturalization_risk, empirical, 'Whether tariff harmonization is inherent to customs unions or contingent institutional choice').

omega_variable(
    piton_theater_trajectory,
    'Is the theater ratio rising (piton degradation) or stable (piton equilibrium)?',
    'Institutional analysis: track evolution of CET committee procedures, compliance disputes, and enforcement actions over 5-year rolling windows. Measure ratio of procedural output (meetings, directives, committee reports) to actual tariff revisions and policy changes.',
    'If theater is rising: piton classification is confirmed and accelerating; the regime is increasingly performative. If theater is stable: piton is a steady state, not a degradation trajectory. If theater is falling: the regime is reconsolidating around genuine coordination; reclassify toward tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_theater_trajectory, empirical, 'Whether Mercosur CET committee theater is rising or stable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mercosur_tariff_harmonization, 1995, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mercosur_tariff_tr_t0, mercosur_tariff_harmonization, theater_ratio, 0, 0.48).
narrative_ontology:measurement(mercosur_tariff_tr_t7, mercosur_tariff_harmonization, theater_ratio, 7, 0.56).
narrative_ontology:measurement(mercosur_tariff_tr_t14, mercosur_tariff_harmonization, theater_ratio, 14, 0.64).

% Extraction over time
narrative_ontology:measurement(mercosur_tariff_be_t0, mercosur_tariff_harmonization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mercosur_tariff_be_t7, mercosur_tariff_harmonization, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(mercosur_tariff_be_t14, mercosur_tariff_harmonization, base_extractiveness, 14, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(mercosur_tariff_su_t0, mercosur_tariff_harmonization, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(mercosur_tariff_su_t7, mercosur_tariff_harmonization, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(mercosur_tariff_su_t14, mercosur_tariff_harmonization, suppression_requirement, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mercosur_tariff_harmonization, resource_allocation).
narrative_ontology:affects_constraint(mercosur_tariff_harmonization, agricultural_subsidy_rents_mercosur).
narrative_ontology:affects_constraint(mercosur_tariff_harmonization, uruguayan_trade_isolation_risk).
narrative_ontology:affects_constraint(mercosur_tariff_harmonization, brazilian_industrial_rent_seeking).

% DUAL FORMULATION NOTE:
% Mercosur tariff harmonization is a single constraint with stable epsilon across observables. The apparent decomposition (coordination problem, extraction mechanism, institutional theater) is perspectival, not observable-dependent — all three are simultaneously present in the structure. Do not split into separate stories. The constraint exemplifies how a single structural arrangement can be Rope (coordination-dominant view) or Snare (extraction-dominant view) or Piton (theater-dominant view) depending on the observer's structural position and time horizon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mercosur_tariff_harmonization, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
