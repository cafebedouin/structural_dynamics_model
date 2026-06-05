% ============================================================================
% CONSTRAINT STORY: eu_ev_tariff_wall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_ev_tariff_wall, []).

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
 *   constraint_id: eu_ev_tariff_wall
 *   human_readable: EU Tariffs and Trade Barriers on Chinese Electric Vehicles
 *   domain: economic/political
 *
 * SUMMARY:
 *   The EU tariff wall on Chinese electric vehicles represents a structural
 *   tension between legitimate industrial protection and protectionist
 *   extraction. Beginning in 2023 with anti-subsidy investigations and
 *   culminating in 2024 with tariffs of 17.4-38.1% on Chinese EV imports, the
 *   constraint combines a genuine coordination function (protecting EU
 *   automakers and battery manufacturers during EV transition) with
 *   significant asymmetric extraction (harming Chinese exporters,
 *   constraining EU consumers, and slowing global decarbonization). The
 *   constraint exhibits different character from different structural
 *   positions: EU incumbents experience it as enabling coordination for
 *   transition investment; Chinese exporters experience it as a tariff-wall
 *   trap; EU consumers experience price inflation with constrained
 *   alternatives; organized climate advocates see it as temporary protection
 *   with a sunset (EU supply chains maturing); the WTO system sees it as a
 *   performative legal framework applied post-hoc to justify political
 *   decisions. The theater ratio (0.64) reflects that anti-subsidy
 *   investigations are substantially procedural — the actual drivers are
 *   political economy (incumbent lobbying, industrial policy competition with
 *   China) but framed as scientific determinations of unfair subsidy.
 *
 * KEY AGENTS:
 *   - EU Incumbent Automakers (VW, BMW, Mercedes, Stellantis): Institutional beneficiaries (powerful/arbitrage) — capture market protection and gain transition time without price competition
 *   - Chinese EV Exporters (BYD, NIO, XPeng, Li Auto, SAIC): Primary victims (powerless/trapped) — face 17.4-38.1% tariffs with no exit option except market abandonment
 *   - EU Consumers: Secondary victims (moderate/constrained) — face EV price inflation; limited domestic alternatives; delayed transition access
 *   - EU Battery Manufacturers (CATL EU operations, Northvolt, ACC): Mixed beneficiary-victim (powerful/mobile) — protected from Chinese competition but constrained by tariff cost cascades
 *   - Climate and Trade Advocates: Organized coalition (organized/constrained) — see sunset logic; justify protection as temporary during EU supply chain maturation
 *   - WTO Trade System: Institutional actor (institutional/arbitrage) — maintains procedural legitimacy through anti-subsidy investigations; lacks real enforcement leverage; persists through inertia
 *   - Global EV Transition: Abstract victim (analytical/trapped) — slowed by tariff fragmentation despite shared climate objective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_ev_tariff_wall, 0.58).
domain_priors:suppression_score(eu_ev_tariff_wall, 0.68).
domain_priors:theater_ratio(eu_ev_tariff_wall, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_ev_tariff_wall, extractiveness, 0.58).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_ev_tariff_wall, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_ev_tariff_wall, tangled_rope).
narrative_ontology:human_readable(eu_ev_tariff_wall, "EU Tariffs and Trade Barriers on Chinese Electric Vehicles").
narrative_ontology:topic_domain(eu_ev_tariff_wall, "economic/political").

domain_priors:requires_active_enforcement(eu_ev_tariff_wall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, eu_incumbent_automakers).
narrative_ontology:constraint_beneficiary(eu_ev_tariff_wall, eu_battery_manufacturers).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, chinese_ev_exporters).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, eu_consumers).
narrative_ontology:constraint_victim(eu_ev_tariff_wall, global_ev_transition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE EV EXPORTERS (SNARE) — Trapped by EU anti-subsidy investigations and tariff barriers (17.4-38.1% added tariffs). No exit option without abandoning EU market. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.97. Pure extraction: exporters bear full cost, bear full suppression, have no alternatives.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EU CONSUMERS (SNARE) — Constrained choice. EV affordability deteriorates as tariffs raise prices. Limited domestic alternatives; must either pay premium or delay transition. d≈0.85, f(d)≈1.15, σ=1.0 → χ≈0.67. High extraction: consumers pay tariff-inflated prices with constrained alternatives.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EU INCUMBENT AUTOMAKERS (ROPE) — Beneficiaries via tariff protection. Experiences constraint as coordination: domestic market protection enables investment in EV transition without price competition. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary; protection subsidizes transition costs.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: EU BATTERY MANUFACTURERS (TANGLED ROPE) — Both beneficiary and victim. Tariffs protect EU battery supply chain from Chinese competition (coordination function); simultaneously, tariffs increase costs for EU EV manufacturers who source globally (extraction function). d≈0.48, f(d)≈0.60, σ=1.1 → χ≈0.38. Mixed: protection enables investment but constrains competitive sourcing.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE/TRADE ADVOCATES (SCAFFOLD) — Organized actors (environmental NGOs, trade unions, development advocates) see tariffs as temporary measures with sunset logic. Tariffs are justified as transition protection: allowing EU supply chains to mature while maintaining climate ambitions. Exit path: as EU battery capacity and EV production scale, tariff dependency declines (estimated 5-10 year sunset). d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.32. Theater moderate (0.64): scientific justification (carbon footprint claims) mixed with protectionist framing.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WTO TRADE GOVERNANCE (PITON) — The tariff regime invokes WTO rules (anti-subsidy investigations, safeguard clauses) but the application is substantially performative. WTO dispute mechanisms move at decadal timescales; investigations (anti-subsidy claims against Chinese state support) are theatrical assertions difficult to falsify or resolve. d≈0.10, f(d)≈-0.05, σ=1.2 → χ≈-0.04. Piton gate: theater_ratio=0.64 (legal procedures maintained but lack functional constraint on actual tariff deployment). WTO persists through institutional inertia; real leverage is political/economic, not adjudicative.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: GLOBAL EV TRANSITION (SNARE) — From a civilizational perspective, the global EV transition (goal: decarbonization) is victimized by tariff fragmentation. Tariffs slow technology diffusion, preserve cost structures, and fragment supply chains that would scale faster under integrated global competition. The transition is trapped in a zero-sum protection game despite shared climate objective. d≈0.88, f(d)≈1.28, σ=1.2 → χ≈0.88. High extraction: transition goal bears full cost of protectionist barriers; no exit without political realignment.
constraint_indexing:constraint_classification(eu_ev_tariff_wall, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_ev_tariff_wall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_ev_tariff_wall, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_ev_tariff_wall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_ev_tariff_wall, TR),
    TR >= 0.70.

:- end_tests(eu_ev_tariff_wall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from Chinese exporters (tariff barriers trap them) and EU consumers (price inflation), but extraction is not total because the beneficiaries (EU incumbents) genuinely experience coordination benefit for their transition. The measurement trajectory from 0.32 to 0.58 reflects accumulating tariff burden — initially (2023) investigations were preliminary; by 2024-2025 implementation was firm. Suppression (0.68): High. Multiple barriers: tariff walls (direct), anti-subsidy investigations (procedural), supply chain localization incentives, and political messaging against Chinese competition all reduce alternatives for Chinese exporters. EU consumers have suppressed alternatives — limited domestic EV capacity at low cost. Theater ratio (0.64): Moderate-high. Anti-subsidy investigations provide scientific legitimacy narrative (carbon footprint claims, state subsidy calculations) but the actual driver is industrial protection and geopolitical competition. The investigations move slowly; tariff deployment is political. Theater has increased over the interval as investigative procedures became more elaborate while real constraint (tariff implementation) accelerated. Claimed type (Tangled Rope): Confirmed. The constraint has BOTH a genuine coordination function (enabling EU transition investment) AND asymmetric extraction (harming exporters and consumers). Requires active enforcement (true): tariff regimes require continuous customs administration, investigation procedures, and political support.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals a maximal perspectival gap. EU incumbent automakers see Rope (coordination enabling their transition). Chinese exporters see Snare (trapped by tariff wall). EU consumers see Snare (price inflation, constrained alternatives). EU battery manufacturers see Tangled Rope (both protected and constrained). Organized climate advocates see Scaffold (temporary protection with sunset as EU capacity matures). WTO system sees Piton (performative legal framework maintaining institutional legitimacy without enforcement). Global climate transition (analytical view) sees Snare (slowed by tariff fragmentation despite shared objective). The perspectival gap does NOT resolve into consensus type — instead it reveals that different stakeholders have incommensurable structural interests. The 'correct' classification depends on whose transition goal and whose market position you prioritize.
 *
 * DIRECTIONALITY LOGIC:
 *   Chinese EV Exporters: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. EU Consumers: Victim + constrained → d≈0.85, f(d)≈1.15. High extraction (price inflation). EU Incumbent Automakers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary (protection subsidy). EU Battery Manufacturers: Both beneficiary (protection) + victim (cost cascades) + mobile → d≈0.48, f(d)≈0.60. Moderate mixed. Climate advocates: Organized + constrained → d≈0.42, f(d)≈0.42. Organized agents see exit path (sunset). WTO System: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Piton classification comes from theater gate, not from directionality. Global Transition: Analytical + trapped (no political exit) → d≈0.88, f(d)≈1.28. High extraction — climate goal trapped by protectionist zero-sum.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by distinguishing the coordination function from the extraction overlay. Tangled Rope (claimed type) is correct: the constraint enables genuine EU transition coordination (beneficiary incumbents invest in EV capacity with protected market time) while simultaneously extracting from victims (trapped exporters, constrained consumers, slowed global transition). The mandatrophy question is: 'Is this protection legitimate industrial policy or protectionist extraction mislabeled as enforcement?' The resolution requires distinguishing authentic subsidy unfairness (if Chinese subsidies are structurally asymmetric) from political protection. The three omegas (subsidy empiricism, EU supply chain viability, consumer price elasticity) directly address this. If Chinese subsidies are equivalent to EU incentives AND EU supply chains can scale without protection AND tariff price harm exceeds localization benefits, then the constraint should reclassify toward pure Snare (protection mislabeled as fair-trade enforcement). If the inverse holds (asymmetric subsidies + structural protection necessity + net climate benefit), then Tangled Rope is correct and the scaffold sunset is real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    chinese_subsidy_empiricism,
    'Do Chinese subsidies to EV manufacturers constitute actionable unfair advantage or legitimate industrial policy equivalent to EU development incentives?',
    'Comparative analysis of subsidy structures (R&D, capital access, land, labor) across EU and Chinese EV sectors; WTO dispute findings; economic modeling of subsidy pass-through to consumer prices',
    'If actionable: tariffs are justified coordination enforcement (tangled rope remains valid). If legitimate policy: tariffs are pure protectionism misframed as enforcement (constraint should reclassify to snare from EU institutional perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chinese_subsidy_empiricism, empirical, 'Whether Chinese subsidies constitute unfair advantage vs. legitimate policy equivalence').

omega_variable(
    eu_supply_chain_viability,
    'Can EU battery and EV manufacturing scale to meet European demand within 5-10 years without tariff protection, or is protection structurally necessary?',
    'Capacity buildout tracking; cost curve analysis (EU vs. Chinese manufacturing costs); capital investment commitments; supply chain bottleneck identification',
    'If viable without protection: scaffold sunset is real; tariffs should terminate as commitment. If structural necessity: tariffs are permanent protection; constraint should reclassify to snare or tangled rope with indefinite extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eu_supply_chain_viability, empirical, 'Whether EU supply chain can scale without tariff protection within sunset window').

omega_variable(
    consumer_price_elasticity_harm,
    'What is the net climate impact of EV price inflation caused by tariffs (fewer consumers can afford EV transition) vs. supply chain localization benefits (reduced shipping carbon)?',
    'Modeling of demand elasticity across income bands; carbon accounting for tariff-induced price increases; lifecycle analysis of domestic vs. imported EV supply chains',
    'If price harm > supply localization benefit: tariffs slow decarbonization; constraint victimizes the global transition (snare classification from analytical view confirmed). If benefits outweigh harm: tangled rope classification sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_price_elasticity_harm, empirical, 'Net climate impact of tariff-induced price inflation vs. supply chain localization').

omega_variable(
    chinese_retaliation_dynamics,
    'Will Chinese retaliatory tariffs on EU goods (agriculture, machinery, auto parts) neutralize the protective benefit to EU automakers or create a new equilibrium extraction?',
    'Monitoring of Chinese tariff announcements and implementation; modeling of tariff revenue flows; assessment of sector-by-sector impact on EU manufacturing',
    'If retaliation severe: EU incumbent automakers become victims rather than beneficiaries (constraint shifts from tangled rope to snare for incumbents). If retaliation limited: benefits to EU automakers preserved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_retaliation_dynamics, empirical, 'Scale and impact of Chinese retaliatory tariffs on EU sectors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_ev_tariff_wall, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_ev_tr_t0, eu_ev_tariff_wall, theater_ratio, 0, 0.48).
narrative_ontology:measurement(eu_ev_tr_t2, eu_ev_tariff_wall, theater_ratio, 2, 0.56).
narrative_ontology:measurement(eu_ev_tr_t5, eu_ev_tariff_wall, theater_ratio, 5, 0.64).

% Extraction over time
narrative_ontology:measurement(eu_ev_be_t0, eu_ev_tariff_wall, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eu_ev_be_t2, eu_ev_tariff_wall, base_extractiveness, 2, 0.47).
narrative_ontology:measurement(eu_ev_be_t5, eu_ev_tariff_wall, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_ev_tariff_wall, resource_allocation).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, chinese_battery_supply_dependency).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, global_ev_supply_chain_fragmentation).
narrative_ontology:affects_constraint(eu_ev_tariff_wall, eu_green_transition_financing).

% DUAL FORMULATION NOTE:
% The tariff wall decomposes into two structurally distinct claims: (1) protection as legitimate coordination (EU supply chain maturation) with a real sunset window (5-10 years to scale capacity), and (2) protection as extraction (political economy of incumbent lobbying, geopolitical competition framed as enforcement). These are not the same constraint viewed from different angles — they have different ε values and different resolution mechanisms. The coordination-function view (ε≈0.30, Tangled Rope) and the pure-extraction view (ε≈0.68, Snare) differ by a factor of 2.3 in base extractiveness. The empirical resolution depends on whether the sunset is genuine (supply chain can scale) or aspirational (protection is permanent).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_ev_tariff_wall, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
