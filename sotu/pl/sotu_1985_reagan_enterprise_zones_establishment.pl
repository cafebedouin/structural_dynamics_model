% ============================================================================
% CONSTRAINT STORY: sotu_1985_reagan_enterprise_zones_establishment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1985_reagan_enterprise_zones_establishment, []).

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
 *   constraint_id: sotu_1985_reagan_enterprise_zones_establishment
 *   human_readable: Enterprise Zones Establishment (SOTU 1985)
 *   domain: regulatory/economic_development
 *
 * SUMMARY:
 *   Enterprise zones represent a foundational regulatory sandboxing mechanism
 *   intended to decentralize economic opportunity through localized tax and
 *   regulatory relief. The 1985 SOTU framing positions zones as mechanisms
 *   for 'breaking poverty cycles' by enabling entrepreneurship in 'abandoned
 *   corners' of economically depressed areas. The constraint exhibits genuine
 *   coordination function (reducing friction for capital-entrepreneur
 *   alignment in underserved markets) alongside extractive dynamics
 *   (displacement pressure, municipal fiscal loss, beneficiary
 *   concentration). The theater ratio (0.55) reflects moderate performative
 *   content: zone success is measured through employment and revenue metrics
 *   that may not capture actual impact on target populations or local
 *   economic stability. Base extractiveness rises from 0.35 to 0.52 over the
 *   measurement interval, indicating that as zones mature, extractive
 *   mechanisms (displacement, tax base erosion, competitive disadvantage for
 *   non-zone local businesses) become more salient than coordination gains.
 *
 * KEY AGENTS:
 *   - Displaced Lower-Income Residents: Primary victim (powerless/trapped) — face gentrification and displacement; lack capital for relocation or alternative employment
 *   - Pre-Existing Local Businesses: Secondary victim (moderate/constrained) — face competition from subsidized entrants and rising property costs; constrained by limited capital
 *   - External Capital Investors & Zone-Eligible Entrepreneurs: Primary beneficiaries (powerful/arbitrage) — experience zone as pure coordination; high exit options; capture tax incentive value
 *   - Municipal Development Authorities: Organized agents (organized/constrained) — frame zone as temporary development stimulus with sunset; constrained by fiscal pressures
 *   - Regulatory Apparatus: Institutional actor (institutional/arbitrage) — maintains zone administrative structure; theater increases as original productivity gains plateau
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing zone structure as market mechanism; false summit risk for treating regulatory arbitrage as natural economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1985_reagan_enterprise_zones_establishment, 0.52).
domain_priors:suppression_score(sotu_1985_reagan_enterprise_zones_establishment, 0.48).
domain_priors:theater_ratio(sotu_1985_reagan_enterprise_zones_establishment, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1985_reagan_enterprise_zones_establishment, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1985_reagan_enterprise_zones_establishment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1985_reagan_enterprise_zones_establishment, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1985_reagan_enterprise_zones_establishment, tangled_rope).
narrative_ontology:human_readable(sotu_1985_reagan_enterprise_zones_establishment, "Enterprise Zones Establishment (SOTU 1985)").
narrative_ontology:topic_domain(sotu_1985_reagan_enterprise_zones_establishment, "regulatory/economic_development").

domain_priors:requires_active_enforcement(sotu_1985_reagan_enterprise_zones_establishment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1985_reagan_enterprise_zones_establishment, external_capital_investors).
narrative_ontology:constraint_beneficiary(sotu_1985_reagan_enterprise_zones_establishment, zone_entrepreneurs_with_access).
narrative_ontology:constraint_victim(sotu_1985_reagan_enterprise_zones_establishment, displaced_lower_income_residents).
narrative_ontology:constraint_victim(sotu_1985_reagan_enterprise_zones_establishment, non_zone_local_businesses).
narrative_ontology:constraint_victim(sotu_1985_reagan_enterprise_zones_establishment, municipal_tax_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED RESIDENTS (SNARE) — Face gentrification pressure and displacement as zone investment raises property values and rents. Trapped by lack of capital for relocation, limited job mobility outside zone, and wage stagnation relative to property cost increases. Maximum extraction from their position: they bear the direct cost of zone-driven displacement with minimal exit options. The zone's investment mechanism extracts neighborhood stability.
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PRE-EXISTING LOCAL BUSINESSES (TANGLED ROPE) — Benefit from expanded local demand and reduced tax burden if they qualify; harmed by competition from subsidized newcomers, increased property rents, and regulatory complexity of zone structures. Constrained by limited capital and relocation costs. Mixed extraction: some businesses see coordination (shared infrastructure improvements, expanded customer base) alongside extraction (competitive disadvantage from subsidized entrants, rent increases).
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: EXTERNAL CAPITAL & ELIGIBLE ENTREPRENEURS (ROPE) — Primary beneficiaries. Experience the zone as pure coordination mechanism: tax incentives and regulatory relief reduce friction for investment and business formation. High exit options (capital is mobile, entrepreneurs can relocate to other zones or non-zone jurisdictions). The constraint solves their coordination problem: aligning municipal incentives with investor interests. Net positive extractiveness flow toward this agent.
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MUNICIPAL DEVELOPMENT AUTHORITIES (SCAFFOLD) — Experience the constraint as temporary coordination with explicit sunset: zone designations are meant to be time-limited (typically 10-20 year terms) to prevent permanent regulatory divergence. Low effective extraction because the constraint has a defined endpoint and municipalities retain agency over zone parameters. Development authorities see the theater as moderate: zone metrics (jobs created, tax revenue recovered) are tracked and measured, distinguishing real coordination from performative theater.
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — The zone structure maintains significant performative content: zone designation creates administrative overhead (eligibility verification, compliance tracking) with uncertain actual impact on economic mobility. Many zones show minimal employment growth or capital formation despite tax incentives. The regulatory mechanism persists through institutional inertia and political rhetoric despite degraded real function. Theater ratio rises as zones age and original productivity gains plateau — the regulatory structure becomes maintenance of zone identity rather than economic stimulus.
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET-BASED NATURALNESS (MOUNTAIN) — Risk naturalizing the zone structure as an immutable market mechanism: 'capital flows to regulatory arbitrage opportunities' and 'entrepreneurs respond to incentives' appear as laws of economic behavior. But the structural data contradicts this naturalization — identifiable beneficiaries (external investors), identifiable victims (existing residents, municipal tax base), and active enforcement mechanisms indicate a constructed institutional arrangement, not a natural law. Engine false summit detector will flag the naturalizing frame.
constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1985_reagan_enterprise_zones_establishment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1985_reagan_enterprise_zones_establishment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1985_reagan_enterprise_zones_establishment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1985_reagan_enterprise_zones_establishment, TR),
    TR >= 0.70.

:- end_tests(sotu_1985_reagan_enterprise_zones_establishment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high and rising. Initial zones (t=0, ε=0.35) show genuine coordination function as reduced friction enables previously blocked capital-entrepreneur pairings. But as zones mature (t=10, ε=0.52), extractive mechanisms dominate: gentrification-driven displacement of lower-income residents, municipal tax base erosion, and competitive disadvantage for non-zone local businesses accumulate. The rising trajectory reflects deadweight loss in tax incentives (investors who would have invested anyway capture most subsidy) and minimal net new job creation for target populations. Suppression (0.48): Moderate. Residents face material barriers (lack of capital for relocation, limited employment mobility, information asymmetries about zone eligibility) but not total lockout — some lower-income entrepreneurs do access zone benefits. Pre-existing businesses face high-but-surmountable costs (relocation, competitive adaptation). Municipal governments retain some agency (zone parameters negotiable). Theater ratio (0.55): Zone metrics (jobs created, tax revenue recovered, businesses formed) are measured and tracked, distinguishing real progress from pure performance. But many zones show minimal employment growth or capital formation despite incentives, and the metrics don't capture displacement or broader inequality effects. Theater increases over time as zones age and original productivity gains plateau while administrative overhead persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single regulatory mechanism produces radically different classification outcomes depending on agent position. The external investor (powerful/arbitrage) sees Rope — coordination mechanism that solves their capital allocation problem. The displaced resident (powerless/trapped) sees Snare — pure extraction with no exit and no coordination benefit for them. The pre-existing local business (moderate/constrained) sees Tangled Rope — genuine coordination (shared infrastructure) alongside extraction (unfair competition). The municipal development authority (organized/constrained) sees Scaffold — temporary development tool with sunset and measured outcomes. The regulatory apparatus (institutional/arbitrage) sees Piton — performative zone administration that persists through inertia despite minimal impact. The civilizational analytical observer risks seeing Mountain (market-based naturalness) but structural data reveals false summit — the zone is constructed institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from their structural relationship to the zone mechanism. External capital investors are beneficiaries with arbitrage exit options: low d produces negative chi (subsidy flow toward them). Displaced residents are victims with zero exit: high d produces high chi (extraction from them). Pre-existing businesses are victims with constrained exit: moderate-high d produces moderate-high chi. Municipalities are mixed (tax revenue loss then recovery) with constrained exit: moderate d produces moderate chi. The beneficiary concentration effect: external capital captures most of the tax incentive value (realized as higher returns), while victims are spread across multiple classes (residents, local businesses, municipal tax base), preventing coalition. This distribution asymmetry is why the constraint persists despite negative effects on majority of local agents — beneficiaries are concentrated and organized; victims are diffused and have conflicting interests.
 *
 * MANDATROPHY ANALYSIS:
 *   CLASSIFICATION STABILITY: The Tangled Rope classification holds across the entire interval because both coordination and extraction functions remain present. However, the proportional balance shifts: early zones (t=0-5) show stronger coordination function (capital access genuinely unlocked); mature zones (t=5-10) show stronger extraction function (displacement and fiscal loss dominate). The classification doesn't change because both are structural features. The rising extractiveness reflects the proportional shift, not category shift. At high enough extractiveness (ε > 0.70), the constraint could reclassify to Snare; the measurement trajectory suggests this threshold could be approached in the 15-20 year range if displacement and fiscal loss continue to accelerate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_causation_ambiguity,
    'What proportion of gentrification and displacement in zone areas is caused by zone designation vs. independent urban trends?',
    'Comparative analysis of property value and rent trajectories in designated zones vs. demographically similar non-zone neighborhoods over 10-20 year periods; instrumental variable analysis of zone eligibility cutoffs',
    'If zone-caused displacement > 40%: snare classification for residents is confirmed; extraction from local stability is primary effect. If zone-caused < 20%: gentrification may be independent of zone mechanism; extraction may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causation_ambiguity, empirical, 'Causal attribution of displacement to zone designation vs. independent trends').

omega_variable(
    job_creation_versus_relocation,
    'Do enterprise zones create net new employment or primarily relocate existing jobs from non-zone areas?',
    'Regional employment analysis; tracking of business relocations into zones vs. new formations; comparison with counterfactual employment growth in absence of zones',
    'If net new > 50% of zone employment: coordination function is real (Rope classification gains support). If primarily relocation < 30% net new: zones are extraction mechanism disguised as development (Snare/Tangled Rope confirmed); redistribution without creation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(job_creation_versus_relocation, empirical, 'Whether zone employment is net new creation or job relocation').

omega_variable(
    tax_incentive_deadweight_loss,
    'What fraction of zone investments would have occurred absent the tax incentives (deadweight loss in incentive spending)?',
    'Business surveys on investment decisions; comparison of zone businesses'' reported incentive sensitivity to non-zone business investment patterns; elasticity estimation',
    'If deadweight loss > 70%: zone tax expenditure is mostly transfer to investors who would have invested anyway (high extraction from tax base). If deadweight loss < 30%: incentives are genuinely marginal to investment decisions; real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_incentive_deadweight_loss, empirical, 'Proportion of zone investment that would occur without tax incentives').

omega_variable(
    minority_entrepreneurship_access,
    'Do enterprise zones meaningfully increase entrepreneurship rates among minority and lower-income populations, or do incentives primarily benefit external capital?',
    'Demographic analysis of zone business ownership; comparison of minority business formation rates in zones vs. non-zones; capital access analysis for minority entrepreneurs',
    'If minority entrepreneurship access rises > 20%: stated purpose (breaking poverty cycles via entrepreneurship) is partially realized; tangled rope structure holds. If < 5% increase: zone structure primarily benefits external capital (pure extraction mechanism); snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minority_entrepreneurship_access, empirical, 'Access to entrepreneurship opportunities for minorities in enterprise zones').

omega_variable(
    regulatory_sandboxing_spillover,
    'Do regulatory relief mechanisms in zones create spillover effects or maintain separate enforcement regimes?',
    'Audit of regulatory compliance and enforcement intensity in zones vs. non-zones; analysis of whether reduced regulations create competitive advantages or local inefficiencies; environmental and labor standard compliance tracking',
    'If spillover effects are significant: zones create regulatory arbitrage externalities (pollution, labor violations leak into non-zone areas); extraction mechanism extends beyond direct residents. If contained: extraction is localized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sandboxing_spillover, empirical, 'Spillover effects of zone regulatory relief into surrounding areas').

omega_variable(
    municipal_fiscal_sustainability,
    'Do zones eventually recover foregone tax revenue through economic growth, or do municipalities face permanent fiscal loss?',
    '20-30 year fiscal analysis of zone cities; tracking of tax base growth, municipal revenue per capita, and service provision quality post-zone designation vs. pre-zone baseline',
    'If recovery > 80%: tangled rope (short-term extraction offset by long-term coordination gains). If recovery < 30%: permanent extraction from municipal tax base; snare classification for cities supported by zone tax revenue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_fiscal_sustainability, empirical, 'Municipal fiscal recovery from foregone zone tax revenues').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1985_reagan_enterprise_zones_establishment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ez1985_tr_t0, sotu_1985_reagan_enterprise_zones_establishment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ez1985_tr_t5, sotu_1985_reagan_enterprise_zones_establishment, theater_ratio, 5, 0.5).
narrative_ontology:measurement(ez1985_tr_t10, sotu_1985_reagan_enterprise_zones_establishment, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(ez1985_be_t0, sotu_1985_reagan_enterprise_zones_establishment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ez1985_be_t5, sotu_1985_reagan_enterprise_zones_establishment, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ez1985_be_t10, sotu_1985_reagan_enterprise_zones_establishment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1985_reagan_enterprise_zones_establishment, resource_allocation).
narrative_ontology:affects_constraint(sotu_1985_reagan_enterprise_zones_establishment, gentrification_displacement_mechanism).
narrative_ontology:affects_constraint(sotu_1985_reagan_enterprise_zones_establishment, municipal_fiscal_sustainability_crisis).
narrative_ontology:affects_constraint(sotu_1985_reagan_enterprise_zones_establishment, regulatory_arbitrage_in_labor_standards).

% DUAL FORMULATION NOTE:
% Enterprise zones decompose into three structurally distinct constraints: (1) Capital allocation coordination (enterprise zone mechanism itself), (2) Gentrification-driven displacement (victims: existing residents; mechanism: property value appreciation), (3) Municipal fiscal sustainability (victims: municipal tax base; mechanism: foregone revenue). Each has distinct ε values and victim populations. The zone establishment story (this file) is upstream; gentrification and fiscal sustainability are downstream effects that would not occur absent the zone mechanism's capital attraction function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1985_reagan_enterprise_zones_establishment, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
