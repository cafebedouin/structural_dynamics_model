% ============================================================================
% CONSTRAINT STORY: origin_country_wage_stagnation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_origin_country_wage_stagnation, []).

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
 *   constraint_id: origin_country_wage_stagnation
 *   human_readable: Origin Country Wage Stagnation: Remittance Dependency and Structural Extraction
 *   domain: economic_development/labor_migration
 *
 * SUMMARY:
 *   Wage stagnation in origin countries of significant migration is a
 *   structural constraint that coordinates labor supply matching between
 *   receiving and origin countries while simultaneously extracting human
 *   capital and suppressing wage growth in origins. The constraint operates
 *   through emigration of skilled and semi-skilled workers, remittance
 *   inflows that distort local price signals and reduce domestic wage
 *   competition incentives, and institutional dependency on external income
 *   flows. The theaters include development rhetoric ('emigration as safety
 *   valve,' 'remittances fund development') that masks the structural
 *   extraction: productive investment is deferred, human capital is drained,
 *   and domestic wage competition is suppressed. The constraint exhibits all
 *   six DR types depending on observational position: snare from the
 *   perspective of trapped domestic workers, tangled rope from origin
 *   governments and skilled domestic actors, rope from receiving-country
 *   labor markets, piton from the post-colonial development model that
 *   perpetuates the myth of emigration-driven development, scaffold from
 *   organized development coalitions that see wage-led growth alternatives,
 *   and tangled rope from the analytical observer who sees genuine
 *   coordination coupled with genuine extraction.
 *
 * KEY AGENTS:
 *   - Stagnant Domestic Worker: Primary victim (powerless/trapped) — no emigration option, faces demoralizing wage decline as skilled workers leave and remittances suppress local wage competition
 *   - Skilled Domestic Entrepreneur: Secondary victim (moderate/constrained) — benefits from ecosystem coordination but faces wage constraints and emigration temptation
 *   - Migrant-Receiving Country Labor Market: Primary beneficiary (institutional/arbitrage) — coordinates labor supply matching; receives wage moderation benefit; can arbitrage out of wage pressure
 *   - Origin Country Government: Mixed actor (institutional/constrained) — benefits from remittance foreign exchange but constrained by brain drain and institutional dependency
 *   - Development Model Perpetuators: Secondary beneficiary (institutional/arbitrage) — benefit from continued emigration and remittance dependency; avoid institutional reform costs
 *   - Domestic Economic Development Coalition: Organized change agent (organized/mobile) — sees exit path through wage-led growth and productive investment redirection
 *   - Remittance Financial Intermediaries: Hidden beneficiary (institutional/arbitrage) — extract rent from transfer fees and exchange rate spreads
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(origin_country_wage_stagnation, 0.58).
domain_priors:suppression_score(origin_country_wage_stagnation, 0.62).
domain_priors:theater_ratio(origin_country_wage_stagnation, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(origin_country_wage_stagnation, extractiveness, 0.58).
narrative_ontology:constraint_metric(origin_country_wage_stagnation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(origin_country_wage_stagnation, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(origin_country_wage_stagnation, tangled_rope).
narrative_ontology:human_readable(origin_country_wage_stagnation, "Origin Country Wage Stagnation: Remittance Dependency and Structural Extraction").
narrative_ontology:topic_domain(origin_country_wage_stagnation, "economic_development/labor_migration").

domain_priors:requires_active_enforcement(origin_country_wage_stagnation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(origin_country_wage_stagnation, migrant_receiving_countries).
narrative_ontology:constraint_beneficiary(origin_country_wage_stagnation, diaspora_capital_exporters).
narrative_ontology:constraint_beneficiary(origin_country_wage_stagnation, remittance_financial_intermediaries).
narrative_ontology:constraint_victim(origin_country_wage_stagnation, origin_country_labor_force).
narrative_ontology:constraint_victim(origin_country_wage_stagnation, domestic_wage_competitiveness).
narrative_ontology:constraint_victim(origin_country_wage_stagnation, productive_investment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STAGNANT DOMESTIC WORKER (SNARE) — No exit from the local labor market; wages stagnate as emigration drains human capital and remittance inflows suppress domestic wage competition. Trapped by lack of migration opportunity, skill mismatch, or family obligations. Bears the full extraction cost while the labor market gradually demoralizes.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SKILLED DOMESTIC ENTREPRENEUR (TANGLED ROPE) — Coordinating local production and finding suppliers benefits from the ecosystem, but faces wage constraints as skilled workers emigrate and remittance inflows distort price signals. Can exit through emigration but at significant personal/family cost. Mixed benefit-cost calculus.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MIGRANT-RECEIVING COUNTRY LABOR MARKET (ROPE) — Benefits from emigration as a coordination mechanism: labor supply matches local shortages, wages are moderated in receiving countries, and labor mobility solves local matching problems. Pure coordination from this perspective — the constraint enables the labor matching that would otherwise require wage increases or domestic labor retraining.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORIGIN COUNTRY GOVERNMENT (TANGLED ROPE) — Coordinates remittance inflows (foreign exchange revenue, poverty reduction in remittance-receiving households) but bears the extraction cost of brain drain, wage stagnation, and dependency on external income flows. Cannot easily exit remittance dependency without restructuring the economy; constrained by capital flight risk and loss of foreign exchange.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL DEVELOPMENT MODEL (PITON) — The constraint persists through institutional inertia: the assumption that emigration solves development (safety valve rhetoric) is theaters — the actual function of remittances is to fund consumption, not productive investment. Brain drain persists; wage stagnation persists. The model is maintained because exit strategies (domestic capital investment, wage-led growth) are politically difficult or require institutional transformation that benefits are not distributed to maintain the old model.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DOMESTIC ECONOMIC DEVELOPMENT COALITION (SCAFFOLD) — Organized agents (unions, development NGOs, regional governments) see wage stagnation as a temporary coordination failure with a potential sunset: wage-led growth strategies, skills-matching programs, and productive investment redirection of remittance capital could break the dependency cycle. High perceived agency and exit path; suppression declines as coalition strength increases. Has sunset logic because the constraint can be deliberately dissolved through policy.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global civilizational perspective, the constraint is structurally hybrid: it genuinely coordinates labor supply matching and poverty reduction via remittances (coordination function) while simultaneously extracting human capital from origin countries and suppressing wage growth (asymmetric extraction). Beneficial for receiving countries and some households; extractive for the origin country as a whole and for trapped domestic workers. The constraint persists because the beneficiaries have higher power and arbitrage optionality than the victims.
constraint_indexing:constraint_classification(origin_country_wage_stagnation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(origin_country_wage_stagnation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(origin_country_wage_stagnation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(origin_country_wage_stagnation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(origin_country_wage_stagnation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(origin_country_wage_stagnation, TR),
    TR >= 0.70.

:- end_tests(origin_country_wage_stagnation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts human capital (brain drain measured by emigration of skilled workers), suppresses wage growth (origin-country wage stagnation despite receiving-country labor shortages), and creates institutional dependency on remittance inflows. The extraction is not maximal (snare level, ≥0.66) because remittances do provide genuine poverty reduction in receiving households and some coordination benefit exists. Over the 30-year interval, extractiveness has increased from 0.35 to 0.62 as brain drain has accumulated and remittance dependency has deepened. Suppression (0.62): High. Multiple barriers prevent exit from the constraint: (1) structural — limited emigration opportunities for most workers, (2) institutional — government dependency on remittance foreign exchange, (3) informational — development model theater that frames emigration as solution rather than extraction, (4) psychological — identity lock for government officials who have built careers on remittance-dependent development narratives. Theater ratio (0.51): Moderate. The development discourse (emigration as safety valve, remittances as poverty-fighting tool) is substantially performative — the actual function is human capital drainage and wage suppression. However, the coordination function (labor matching, household poverty reduction) is genuine, reducing the pure theater component. As institutional dependency has deepened, the theater ratio has increased from 0.38 to 0.51.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap is maximized between receiving-country labor markets (rope: pure coordination with wage moderation benefit) and stagnant domestic workers (snare: pure extraction with no exit). This is the defining feature of the tangled rope classification — genuine coordination for some agents (labor matching) coupled with genuine extraction for others (wage suppression, brain drain). The gap also appears between the piton perspective (institutional degradation theater) and the scaffold perspective (sunset logic through organized coalition action). The gap reveals that the constraint is not natural or inevitable but maintained through power asymmetries and institutional inertia.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is determined by their structural position relative to the extraction flow. Receiving-country labor markets benefit (low d ~0.15, institutional/arbitrage) and experience negative effective extraction (χ < 0). Origin-country governments are mixed (d ~0.55, constrained/institutional) — they benefit from remittance flows but lose human capital and institutional capacity. Stagnant domestic workers are fully targeted (d ~0.95, powerless/trapped) and experience maximum extraction. Skilled entrepreneurs face ambiguous positioning (d ~0.60, moderate/constrained) — they benefit from some ecosystem coordination but are suppressed by wage dynamics and exodus temptation. The remittance financial intermediaries have low d (~0.20, institutional/arbitrage) and benefit from the constraint through rent extraction (fee capture). The coalition for domestic development has relatively low d (~0.35, organized/mobile) because they have organizational power and see exit pathways, even though they currently experience the constraint as something imposed on their preferred development model.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mislabeling through perspectival differentiation. If we collapsed all perspectives into a single 'global' view, we might classify this as pure rope (labor market coordination) or pure snare (human capital extraction). Instead, the framework shows it is tangled rope: coordination for beneficiaries, extraction for victims. The mandatrophy is resolved by recognizing that beneficiaries experience coordination while victims experience extraction from the same underlying constraint structure. The theater (development rhetoric) creates the mandatrophy risk — the framing obscures that brain drain and wage suppression are features, not bugs, of the system. The piton perspective confirms the theater through institutional inertia analysis: the development model persists not because it works but because institutional actors benefit from continuing it. The scaffold perspective shows that exit is possible through deliberate coalition action (wage-led growth strategies), confirming that the constraint is not natural or inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remittance_productivity_threshold,
    'At what level of remittance dependency does the constraint shift from poverty mitigation to productive investment suppression?',
    'Comparative analysis of remittance utilization patterns across countries: correlation between remittance/GDP ratio and domestic investment rates, productivity growth, and wage growth trajectory',
    'If threshold is low (< 10% GDP): constraint operates as extraction mechanism across most origins. If threshold is high (> 25% GDP): many countries safely absorb remittances as consumption without wage suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_productivity_threshold, empirical, 'Remittance dependency level at which productive investment suppression begins').

omega_variable(
    brain_drain_reversibility,
    'Is the human capital drain from emigration reversible through skills-matching programs, or does the extraction mechanism involve permanent knowledge loss?',
    'Longitudinal studies of return migration and skills-repatriation programs; measurement of knowledge transfer effectiveness vs. persistent wage competition from diaspora networks',
    'If reversible: scaffold sunset clause is credible — domestic investment in matching and return incentives can break the constraint. If irreversible: constraint is closer to mountain — extraction mechanism is locked in by path dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brain_drain_reversibility, empirical, 'Whether brain drain is reversible through policy intervention').

omega_variable(
    remittance_financial_intermediary_extraction,
    'What proportion of remittance extraction is captured by financial intermediaries (transfer fees, exchange rate spreads) vs. structural wage suppression in origin countries?',
    'Cost-of-transfer analysis; comparison of official vs informal remittance channels; measurement of beneficiary/intermediary split across corridors',
    'If high (> 40%): the constraint is significantly driven by rent-seeking intermediaries, and policy intervention on fee structures could reduce suppression. If low (< 15%): extraction is primarily structural (wage competition, capital flight), and fee regulation alone cannot solve it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remittance_financial_intermediary_extraction, empirical, 'Proportion of remittance extraction by financial intermediaries').

omega_variable(
    receiving_country_wage_moderation_causality,
    'Does emigration actually moderate wages in receiving countries, or are other factors (technology, globalizing supply chains) the primary drivers of wage stagnation globally?',
    'Wage growth decomposition analysis in receiving countries; causal identification of migration''s contribution vs. other factors using quasi-experimental migration shocks',
    'If migration is significant causal factor: the constraint reflects genuine labor market coordination (Rope classification is accurate). If minimal effect: the constraint is more about extraction of origin-country human capital than global labor market coordination (Snare classification is more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(receiving_country_wage_moderation_causality, empirical, 'Causal effect of emigration on receiving-country wage moderation').

omega_variable(
    government_substitution_effect,
    'Does remittance inflow allow origin-country governments to avoid productive investment and institutional reform?',
    'Comparison of social spending, tax capacity, and institutional development between high-remittance and low-remittance countries; measurement of policy-induced substitution vs. exogenous remittance shocks',
    'If substitution is strong: remittance dependency becomes an institutional lock-in mechanism (piton escalation). If weak: remittances are genuinely supplementary, and governments are not avoiding reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_substitution_effect, empirical, 'Whether remittances substitute for government productive investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(origin_country_wage_stagnation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wagstag_tr_t0, origin_country_wage_stagnation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(wagstag_tr_t10, origin_country_wage_stagnation, theater_ratio, 10, 0.45).
narrative_ontology:measurement(wagstag_tr_t20, origin_country_wage_stagnation, theater_ratio, 20, 0.51).
narrative_ontology:measurement(wagstag_tr_t30, origin_country_wage_stagnation, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(wagstag_be_t0, origin_country_wage_stagnation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wagstag_be_t10, origin_country_wage_stagnation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wagstag_be_t20, origin_country_wage_stagnation, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(wagstag_be_t30, origin_country_wage_stagnation, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(origin_country_wage_stagnation, resource_allocation).
narrative_ontology:affects_constraint(origin_country_wage_stagnation, remittance_dependent_household_poverty_trap).
narrative_ontology:affects_constraint(origin_country_wage_stagnation, receiving_country_low_wage_equilibrium).
narrative_ontology:affects_constraint(origin_country_wage_stagnation, origin_country_institutional_capacity_degradation).

% DUAL FORMULATION NOTE:
% Origin country wage stagnation is the downstream constraint in a causal family with remittance-dependent household poverty traps (upstream) and receiving-country low-wage equilibrium (parallel). Decomposed because each constraint has distinct ε values: remittance poverty traps (ε≈0.45, coordination/extraction hybrid), receiving-country wage equilibrium (ε≈0.35, pure coordination), origin-country wage stagnation (ε≈0.58, stronger extraction). The network captures that constraining one member affects all — reducing remittance dependency or increasing receiving-country wages would necessarily transform origin-country wage stagnation dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(origin_country_wage_stagnation, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
