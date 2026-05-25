% ============================================================================
% CONSTRAINT STORY: us_chips_act_subsidy_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_chips_act_subsidy_race, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_chips_act_subsidy_race
 *   human_readable: US CHIPS Act Subsidy Race: Semiconductor Manufacturing Coordination and Extraction
 *   domain: economic_policy/industrial_policy/geopolitics
 *
 * SUMMARY:
 *   The US CHIPS Act (2022) authorizes $39 billion in direct subsidies and
 *   tax incentives to semiconductor manufacturers, ostensibly to address
 *   supply chain vulnerabilities and geopolitical competition from China. The
 *   constraint exhibits textbook Tangled Rope properties: a genuine
 *   coordination problem (semiconductor manufacturing concentration creates
 *   supply chain risk, markets underinvest in redundant capacity due to
 *   externalities) is paired with asymmetric extraction (subsidies
 *   concentrate benefits among incumbent manufacturers while dispersing costs
 *   across taxpayers and non-recipient competitors). The extractiveness value
 *   (0.58) reflects that the subsidy mechanism serves both functions
 *   simultaneously — it genuinely addresses a coordination failure while
 *   simultaneously enriching incumbent manufacturers and disadvantaging
 *   competitors. Theater ratio (0.48) is moderate because the industrial
 *   policy rationale is substantive (supply chain risk is real) but
 *   increasingly theatrical as the program functions to lock in incumbent
 *   advantage and create path dependency for continued subsidies. The
 *   suppression value (0.65) reflects that non-recipients, foreign
 *   competitors, and taxpayers face high barriers to organizing or exiting:
 *   they cannot refuse to fund subsidies (taxation), cannot easily enter the
 *   subsidized pool (application barriers, existing facility advantages), and
 *   cannot exit the competitive market (global semiconductor dependency is
 *   inescapable).
 *
 * KEY AGENTS:
 *   - CHIPS Act Recipient Manufacturers (Intel, Samsung US operations, TSMC Arizona fab): Primary beneficiary (institutional/arbitrage) — receive direct subsidy payments, tax incentives, and competitive advantage through barrier elevation
 *   - US Government and Congressional Sponsors: Organized beneficiary (organized/constrained) — achieve geopolitical hedging objectives and consolidate power among incumbent allies; constrained by political sustainability of program
 *   - Foreign Semiconductor Manufacturers (TSMC Taiwan, Samsung South Korea, Huawei China, MediaTek): Primary victim (powerless/trapped) — face tariff barriers, export restrictions, and unequal subsidy access; cannot exit global competition
 *   - Non-Recipient Domestic Manufacturers and Startups: Secondary victim (moderate/constrained) — disadvantaged by unequal subsidy access despite being US-based; higher capital costs and slower growth trajectories
 *   - US Taxpayers and Non-Recipient Regions: Dispersed victim (powerless/trapped) — bear $39+ billion cost with minimal direct benefit; suppressed through tax financing and distributed cost structure
 *   - Chinese Government and Competitors: Competitive responder (powerful/constrained) — responds with own subsidies (SMIC, Huawei support); engaged in subsidy arms race dynamic
 *   - Analytical Observer: Structural analyst (analytical/analytical) — recognizes both genuine supply chain coordination problem and extraction mechanism creating competitive lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_chips_act_subsidy_race, 0.58).
domain_priors:suppression_score(us_chips_act_subsidy_race, 0.65).
domain_priors:theater_ratio(us_chips_act_subsidy_race, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_chips_act_subsidy_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_chips_act_subsidy_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(us_chips_act_subsidy_race, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_chips_act_subsidy_race, tangled_rope).
narrative_ontology:human_readable(us_chips_act_subsidy_race, "US CHIPS Act Subsidy Race: Semiconductor Manufacturing Coordination and Extraction").
narrative_ontology:topic_domain(us_chips_act_subsidy_race, "economic_policy/industrial_policy/geopolitics").

domain_priors:requires_active_enforcement(us_chips_act_subsidy_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_chips_act_subsidy_race, incumbent_semiconductor_manufacturers).
narrative_ontology:constraint_beneficiary(us_chips_act_subsidy_race, us_government_strategic_objective).
narrative_ontology:constraint_victim(us_chips_act_subsidy_race, non_subsidized_competitors).
narrative_ontology:constraint_victim(us_chips_act_subsidy_race, taxpayers).
narrative_ontology:constraint_victim(us_chips_act_subsidy_race, non_recipient_manufacturing_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOREIGN MANUFACTURERS (SNARE) — Trapped in a competitive landscape where US-based competitors receive massive subsidies while they face tariffs, export restrictions, and market access barriers. Cannot exit without abandoning their market share in the US. Maximum extraction through competitive disadvantage. No coordination benefit — this is pure rent extraction masquerading as national competitiveness policy.
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-RECIPIENT DOMESTIC MANUFACTURERS (TANGLED ROPE) — Constrained by unequal access to subsidies despite being US-based. Face higher borrowing costs and cannot compete with subsidized incumbents. Genuine coordination benefit exists (secure US supply chain supports all domestic players) but overshadowed by asymmetric extraction favoring CHIPS Act recipients. Can theoretically apply for funding but face high barriers and delayed timelines.
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHIPS ACT RECIPIENTS (ROPE) — Direct beneficiaries of up to $39 billion in subsidies. Experience the constraint as pure coordination: securing US semiconductor manufacturing capacity addresses genuine collective action problem (supply chain resilience, geopolitical hedging, manufacturing cost reduction). Net beneficiaries with high exit optionality — can relocate if subsidy conditions become unfavorable. Extraction flows toward them.
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US GOVERNMENT (TANGLED ROPE) — Organized agent coordinating genuine collective defense (semiconductor supply chain independence from China-controlled manufacturing). Simultaneously extracting from taxpayers and competitors through subsidy concentration and industrial policy lock-in. Genuine coordination function (addressing technological and geopolitical risk) exists alongside political economy extraction (consolidating power among incumbent manufacturers, creating path dependency for continued subsidies).
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAXPAYERS AND NON-RECIPIENT REGIONS (SNARE) — Trapped in subsidy financing with no exit. Bear $39+ billion in public expenditure for concentrated geographic and corporate benefits. Minimal coordination benefit (can point to supply chain benefits but distributed thinly). Suppression through deficit financing and inflation impact. Cannot organize effective resistance due to dispersed costs and concentrated benefits (classic Olson political economy).
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL TECH ECOSYSTEM / CHINESE RESPONSE (SCAFFOLD) — Sees CHIPS Act as temporary political coordination mechanism with built-in sunset dynamics. China responds with its own subsidies (SMIC funding, Huawei support programs). Competitive spiral is inherently unstable — eventually economic logic reasserts as subsidy burden constrains other investments. High mobility (technology moves across borders regardless of subsidy regime). Theater is high (both sides maintain industrial policy theater) but constraint is temporary if economic fundamentals eventually assert.
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, scaffold,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination function (semiconductor supply chain resilience is a real collective good that markets undervalue due to geopolitical externalities) and real extraction (subsidy concentration extracts from competitors and taxpayers, creates moral hazard, locks in incumbent advantage, distorts innovation incentives). The constraint will persist for 10-15 years (sunset horizon) due to political lock-in and continued Chinese competition, regardless of economic efficiency.
constraint_indexing:constraint_classification(us_chips_act_subsidy_race, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_chips_act_subsidy_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_chips_act_subsidy_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_chips_act_subsidy_race, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_chips_act_subsidy_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_chips_act_subsidy_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The CHIPS Act exhibits moderate-high extractiveness. The genuine coordination component — addressing semiconductor supply chain concentration risk — would justify ε ≈ 0.20-0.30 in a pure coordination mechanism. However, the subsidy concentration among incumbents (Intel, Samsung US, TSMC Arizona) while excluding competitors or new entrants extracts an additional ε ≈ 0.28-0.38 through artificial rent creation. The combined effect is ε ≈ 0.58. This value reflects that subsidy programs serve legitimate geopolitical functions (coordination) while simultaneously extracting through competitive disadvantage creation (rent-seeking). Suppression (0.65): Suppression is high because exit options are structurally limited. Non-recipients cannot avoid funding (tax requirement), cannot easily enter the subsidized pool (application barriers, infrastructure requirements, incumbent advantages), and cannot exit global semiconductor competition (universal dependency). Foreign competitors cannot reorganize their government subsidies (structural constraint) without massive political upheaval. Taxpayers are dispersed (classic Olson political economy) and cannot organize resistance. Theater ratio (0.48): Moderate. The industrial policy narrative is substantive — semiconductor supply chain risk is real, Chinese competition is real — but the program is increasingly theatrical as it functions to lock in incumbent advantage and justify ongoing subsidies. Over the interval, theater increases as the program becomes less about addressing immediate supply chain risk and more about maintaining subsidy flows to politically connected manufacturers.
 *
 * PERSPECTIVAL GAP:
 *   The CHIPS Act generates maximum perspectival divergence. Recipients (Intel, Samsung US, TSMC Arizona) see pure coordination and profit opportunity (Rope) — they are solving a real supply chain problem while capturing benefits. The US government sees coordinated geopolitical hedging with political constraints (Tangled Rope) — genuine supply chain defense objective paired with extraction of taxpayer resources and competitive distortion. Taxpayers and non-recipients see pure extraction (Snare) — they fund subsidies for competitors' profit. Foreign manufacturers see a snare disguised as industrial policy (Snare) — barriers and tariffs extract from them through competitive disadvantage. Startups and non-recipient domestic manufacturers see mixed extraction and coordination (Tangled Rope) — they benefit from supply chain resilience but are disadvantaged by unequal subsidy access. The analytical observer sees the full Tangled Rope structure: genuine coordination problem exists, is being addressed through subsidy mechanism, but the mechanism simultaneously creates extraction through rent concentration. The perspectival gap reveals the political economy logic — framing subsidies as pure coordination (government/recipient view) versus acknowledging extraction component (taxpayer/foreign competitor view).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and power/exit combinations. CHIPS Act recipients have institutional power and arbitrage exit options (can relocate or adjust production if terms become unfavorable) — they derive low or negative d values from the sigmoid f(d), meaning extraction runs toward them (they benefit). Foreign competitors have trapped exit options (cannot reorganize their home governments' subsidies) and are victims — they derive high d values, experiencing maximum extraction. Taxpayers and non-recipients are powerless/trapped victims — high d, high experienced extraction. The US government is an organized beneficiary with constrained exit (political commitment to supply chain resilience) — moderate d value reflecting that they capture strategic benefit while bearing political cost. Non-recipient domestic manufacturers have moderate power and constrained exit — moderate d, experiencing moderate extraction (constrained agents face high costs but retain some agency). The pipeline computes chi from d and produces perspectival extractiveness values that align with the structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing how industrial policy coordinates a real problem (supply chain concentration) while simultaneously enabling extraction (rent concentration). The key to resolution is that BOTH functions are real. A pure-coordination reading (Rope) misses the extraction mechanism and competitive distortion. A pure-extraction reading (Snare) misses the genuine supply chain risk that justifies some coordination. The Tangled Rope classification captures both: the subsidy program has a genuine coordination function (addresses supply chain risk), requires active enforcement (government administration, recipient compliance monitoring, eligibility requirements), and produces asymmetric extraction (benefits concentrate among recipients, costs disperse across taxpayers). The mandatrophy resolves by noting that evaluating CHIPS Act strictly as 'is this good industrial policy?' (focusing on coordination success) yields underestimation of extraction costs and distortions. Evaluating it strictly as 'is this subsidizing incumbents?' (focusing on extraction) yields underestimation of genuine supply chain coordination benefits. The Tangled Rope classification is the accurate structural description: both functions exist, the question is the relative weighting and sustainability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_effectiveness_threshold,
    'Does $39 billion in subsidies actually achieve semiconductor supply chain independence, or does it merely redirect profits while maintaining Chinese manufacturing dependency for mature nodes?',
    'Long-term tracking: (1) US semiconductor fab capacity as % of global capacity; (2) US advanced node production volume vs. Chinese mature node production; (3) Cost per transistor produced domestically vs. foreign competitors; (4) actual supply chain disruption resistance during future geopolitical events',
    'If subsidies achieve independence: constraint reclassifies as legitimate Rope/Scaffold with coordination benefit. If subsidies merely create profit redirection with persistent supply chain dependency: constraint is primarily Snare with minimal coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsidy_effectiveness_threshold, empirical, 'Whether subsidies achieve actual supply chain independence or merely redirect profits').

omega_variable(
    moral_hazard_lock_in,
    'Once recipient manufacturers receive subsidies, can the US government credibly threaten to withdraw funding, or does path dependency create irreversible lock-in?',
    'Political economy analysis: (1) study of past subsidy withdrawal precedents; (2) analysis of manufacturer political influence growth post-subsidy; (3) tracking of subsequent subsidy rounds or condition softening',
    'If credible withdrawal threat exists: constraint is Tangled Rope with sunset logic (subsidies temporary). If lock-in occurs: constraint becomes permanent Snare where extraction institutionalizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moral_hazard_lock_in, empirical, 'Whether subsidy lock-in creates irreversible path dependency').

omega_variable(
    global_subsidy_race_stability,
    'Is the global semiconductor subsidy race (US, China, EU, Japan, South Korea all offering incentives) stable, or does it collapse into a mutually-disadvantageous equilibrium where subsidies offset and no player gains competitive advantage?',
    'Game-theoretic modeling and empirical tracking: (1) compare subsidy-to-revenue ratios across countries and years; (2) measure market share changes for subsidized vs. non-subsidized manufacturers; (3) track whether later rounds of subsidy acceleration occur (arms race dynamic) or stabilize',
    'If subsidy arms race occurs: extractiveness increases over time (measurement oscillation shows rising values). If equilibrium stabilizes: extractiveness plateaus and may decline as collective action problem becomes visible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_subsidy_race_stability, empirical, 'Stability of global semiconductor subsidy race dynamics').

omega_variable(
    technological_fungibility,
    'Are advanced semiconductor manufacturing capabilities truly geopolitically critical (cannot be easily substituted or relocated), or could rapid technology advances in chiplets, heterogeneous integration, or alternative architectures make subsidized fabs obsolete before payoff horizon?',
    'Technical analysis and market tracking: (1) pace of chiplet adoption and success; (2) emergence of alternative manufacturing paradigms; (3) capital reallocation patterns; (4) actual fab utilization rates for cutting-edge vs. mature nodes',
    'If fungibility is high: constraints reclassify as Piton (theater with degraded function). If truly critical: Tangled Rope with genuine coordination benefit confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_fungibility, empirical, 'Technological fungibility of subsidized manufacturing capabilities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_chips_act_subsidy_race, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chips_tr_t0, us_chips_act_subsidy_race, theater_ratio, 0, 0.35).
narrative_ontology:measurement(chips_tr_t3, us_chips_act_subsidy_race, theater_ratio, 3, 0.42).
narrative_ontology:measurement(chips_tr_t6, us_chips_act_subsidy_race, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(chips_be_t0, us_chips_act_subsidy_race, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(chips_be_t2, us_chips_act_subsidy_race, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(chips_be_t5, us_chips_act_subsidy_race, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_chips_act_subsidy_race, resource_allocation).
narrative_ontology:affects_constraint(us_chips_act_subsidy_race, chinese_semiconductor_subsidy_race).
narrative_ontology:affects_constraint(us_chips_act_subsidy_race, eu_chips_act_competition).
narrative_ontology:affects_constraint(us_chips_act_subsidy_race, semiconductor_supply_chain_resilience).
narrative_ontology:affects_constraint(us_chips_act_subsidy_race, incumbent_advantage_lock_in).

% DUAL FORMULATION NOTE:
% CHIPS Act subsidy race is downstream of the semiconductor geopolitical competition constraint but represents a distinct structural policy response. Upstream constraint (geopolitical supply chain risk) has higher empirical consensus and lower extractiveness ε ≈ 0.12 (genuine coordination problem). CHIPS Act response has higher extractiveness ε ≈ 0.58 (adds rent concentration and competitive distortion). The two-constraint family shows how a genuine coordination problem can spawn a mixed coordination-extraction policy response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
