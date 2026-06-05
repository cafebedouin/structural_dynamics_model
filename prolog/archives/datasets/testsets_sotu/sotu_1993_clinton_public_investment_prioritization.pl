% ============================================================================
% CONSTRAINT STORY: sotu_1993_clinton_public_investment_prioritization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1993_clinton_public_investment_prioritization, []).

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
 *   constraint_id: sotu_1993_clinton_public_investment_prioritization
 *   human_readable: Federal Budget Rebalancing: Public Consumption to Long-Term Investment (1993-2026)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The 1993 Clinton budget rebalancing represents a fundamental
 *   institutional reframing of the federal government's fiscal role: from a
 *   consumption-redistribution state (welfare, Social Security, immediate
 *   transfers) toward an investment-development state (infrastructure,
 *   education, R&D, productive capacity formation). This constraint exhibits
 *   the full Deferential Realism spectrum across political time horizons.
 *   Current-period welfare recipients experience extraction (snare). Current
 *   workers experience mixed coordination and extraction (tangled rope).
 *   Future beneficiaries experience pure coordination (rope). The
 *   congressional budget apparatus treats investment as consumption to be cut
 *   alongside welfare (piton). A civilizational observer risks naturalizing
 *   the time-preference tradeoff as immutable law (false-summit mountain).
 *   The constraint's extractiveness rises over its first decade (0.38 → 0.52)
 *   as the rebalancing deepens, while theater increases (0.42 → 0.58) as the
 *   gap between rhetorical investment commitment and actual implementation
 *   widens. The mandatrophy is unresolved: the constraint remains contested
 *   between those experiencing present extraction and those expecting future
 *   benefits.
 *
 * KEY AGENTS:
 *   - Current Consumption Recipients: Primary victims (powerless/trapped) — welfare, SSI, Medicare beneficiaries face program constraints while government invests in future productivity
 *   - Elderly Fixed-Income Populations: Primary victims (powerless/trapped) — Social Security COLA restraint, Medicare means-testing, and reduced transfer programs directly reduce purchasing power with zero future benefit
 *   - Current-Period Labor Force: Secondary victims/mixed (moderate/constrained) — higher taxation to fund investment, constrained by inability to exit labor markets or defer consumption needs
 *   - Future Generations: Primary beneficiaries (institutional/arbitrage) — future workers and businesses benefit from improved infrastructure, education, R&D capacity
 *   - Infrastructure and High-Skill Industries: Secondary beneficiaries (institutional/arbitrage) — roads, bridges, broadband, universities, research institutions directly funded
 *   - Organized Labor: Organized beneficiary (organized/constrained) — unions benefit from job creation in construction and skilled trades but lose bargaining power as public sector employment declines
 *   - Congressional Budget Process: Institutional keeper (institutional/arbitrage) — budget committees and OMB maintain unified accounting framework that treats investment as consumption
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the political economy of time preference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1993_clinton_public_investment_prioritization, 0.52).
domain_priors:suppression_score(sotu_1993_clinton_public_investment_prioritization, 0.48).
domain_priors:theater_ratio(sotu_1993_clinton_public_investment_prioritization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1993_clinton_public_investment_prioritization, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1993_clinton_public_investment_prioritization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1993_clinton_public_investment_prioritization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1993_clinton_public_investment_prioritization, tangled_rope).
narrative_ontology:human_readable(sotu_1993_clinton_public_investment_prioritization, "Federal Budget Rebalancing: Public Consumption to Long-Term Investment (1993-2026)").
narrative_ontology:topic_domain(sotu_1993_clinton_public_investment_prioritization, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1993_clinton_public_investment_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_public_investment_prioritization, future_generations).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_public_investment_prioritization, productivity_growth_sector).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_public_investment_prioritization, infrastructure_dependent_industries).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_public_investment_prioritization, high_skill_labor_markets).
narrative_ontology:constraint_victim(sotu_1993_clinton_public_investment_prioritization, current_consumption_recipients).
narrative_ontology:constraint_victim(sotu_1993_clinton_public_investment_prioritization, current_beneficiaries_of_transfer_programs).
narrative_ontology:constraint_victim(sotu_1993_clinton_public_investment_prioritization, elderly_fixed_income_populations).
narrative_ontology:constraint_victim(sotu_1993_clinton_public_investment_prioritization, immediate_welfare_dependent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CURRENT CONSUMPTION VICTIMS (SNARE) — Welfare recipients, Social Security dependents, and immediate-need populations face deferred benefits while government reallocates resources to long-term investment with negligible near-term benefit to them. Trapped by biological time horizon (retirees cannot wait 20 years for productivity gains) and lack of alternative income sources. Maximum extraction from this agent: explicit zero-sum transfer from present to future.
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CURRENT LABOR FORCE (TANGLED ROPE) — Workers in prime earning years experience constrained exit: they benefit from improved infrastructure and education systems (coordination) while simultaneously bearing the costs of program cuts and deferred consumption support through reduced transfers and higher taxation. Coordination exists (better roads, educated workforce) alongside asymmetric extraction (they fund investment they may not directly benefit from within their biographical horizon).
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE INSTITUTIONAL BENEFICIARIES (ROPE) — Business infrastructure developers, technology sectors, higher-education institutions, and infrastructure-dependent industries benefit from investment redistribution. They experience the constraint as pure coordination: government is solving the collective-action problem of under-investment in public goods that markets undersupply. High exit capacity (can arbitrage private investment if public investment fails). Net beneficiary.
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED LABOR (TANGLED ROPE) — Unions experience genuine coordination benefit (infrastructure and job-creation investment creates employment) alongside extraction (deferred wage growth through program cuts, union membership decline from privatization, constrained exit from bargaining unit limitations). Organized power allows negotiation but not exit.
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BUDGET PROCESS INSTITUTIONS (PITON) — Congressional budgeting procedures and deficit-reduction frameworks treat public investment as consumption to be reduced alongside welfare. The constraint maintains theater (annual appropriations, baseline budgeting, unified budget accounting) while the functional distinction between investment and consumption has atrophied. Procedures persist through institutional inertia despite low functional coherence — modern budget processes struggle to implement long-term investment logic.
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the time preference tradeoff appears immutable: all finite-lived agents must prefer present consumption to future productivity. The intertemporal constraint is a structural feature of human temporal discounting and political economy. However, the engine's false-summit detection identifies this as naturalization of a policy choice (the federal deficit, tax treatment of investment vs. consumption, budgetary accounting conventions) rather than a law of nature.
constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1993_clinton_public_investment_prioritization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1993_clinton_public_investment_prioritization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1993_clinton_public_investment_prioritization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1993_clinton_public_investment_prioritization, TR),
    TR >= 0.70.

:- end_tests(sotu_1993_clinton_public_investment_prioritization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint enforces an explicit zero-sum transfer from present to future through federal budget reallocation. The extraction is genuine (current welfare recipients have less) and measurable (program restraint quantified in CBO baselines). It is not maximal (Snare-level 0.66+) because infrastructure and education benefits do accrue to current populations (highways, schools) and because the rebalancing is not purely extractive — coordination function exists. The 0.52 value reflects that this is a Tangled Rope: real coordination function (solving public goods undersupply) coupled with asymmetric extraction (costs borne by present powerless, benefits received by future). Suppression (0.48): Moderate. Barriers to exit from the constraint include: (a) current welfare recipients cannot defer their consumption needs; (b) taxpayers face legal obligation to fund rebalancing; (c) political mechanisms for reversing the rebalancing are weakened by long-term spending commitments. However, suppression is not high (Snare-level 0.60+) because: (a) democratic mechanisms allow political reversal; (b) budget rebalancing is reversible via legislative action; (c) some populations (infrastructure workers, future students) actively benefit and have exit capacity. Theater (0.58): Moderate-high. The constraint exhibits significant performative elements: (a) rhetorical commitment to 'investment' in government documents exceeds actual capital formation increases; (b) budget accounting treats public investment the same as consumption (unified budget framework rather than capital budget); (c) appropriations processes handle investment and consumption identically despite different functional requirements. Theater rises over the interval (0.42 → 0.61) as the gap between 'we are investing for the future' messaging and the procedural inability to implement true long-term investment logic widens. This rising theater is diagnostic: the constraint's institutional implementation increasingly relies on narrative claim rather than structural change in how government actually functions.
 *
 * PERSPECTIVAL GAP:
 *   Primary gap: present vs. future. Current welfare recipients experience maximum extraction (Snare); future workers experience maximum benefit (Rope). Current labor force experiences mixed (Tangled Rope). Secondary gap: organized vs. powerless. Union workers see more benefit from job creation, giving them constrained rather than trapped exit. Powerless elderly have no mobilization capacity. Tertiary gap: institutional commitment vs. procedural capacity. Budget institutions maintain unified accounting that treats investment and consumption identically, creating a piton classification: they lack functional capacity to implement long-term investment logic even as they rhetorically embrace it. The false-summit mountain (civilizational observer) reveals that the temporal tradeoff is not a law of nature but a policy choice — the constraint's severity depends on tax incidence, transfer design, and budget accounting methods, all alterable through legislation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position within the constraint. Current consumption victims have d=0.95 (full targets of extraction — benefits flow away from them, costs toward them). Current workers have d=0.60 (symmetric with slight extraction — they pay some costs, receive some infrastructure benefits). Future beneficiaries have d=0.05 (full beneficiaries — costs borne elsewhere, benefits accrue to them). The budget process has d=0.10 (institutional beneficiary — continues its authority and narrative legitimacy). The analytical observer has d=0.72 (observational position, no direct structural location). The sigmoid f(d) transforms these positions into experienced extractiveness chi. High d (victims) → high f(d) → high χ experienced extraction. Low d (beneficiaries) → low f(d) → low χ experienced extraction. Symmetric d=0.60 → f(d)≈0.75 → moderate χ. The constraint's claimed type (Tangled Rope) emerges from this structure: genuine coordination function (public goods provision) coupled with measurable directionality asymmetry (costs and benefits not equally distributed across agents).
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY. The constraint's classification as Tangled Rope depends entirely on whether one believes the public investment benefits are real and sufficient to justify the present extraction. The omega variables identify three critical ambiguities: (1) measurement of investment vs. consumption (definitional boundary affects ε by ±0.33); (2) causal attribution of productivity growth (confounded with technological cycles); (3) distributional concentration (are benefits concentrated in specific regions/groups while costs spread broadly, making it effectively Snare?). The mandatrophy is unresolved because these empirical and conceptual questions remain contested 30+ years after the constraint's implementation. The rising theater ratio (0.42 → 0.61) suggests the constraint may be degrading into Piton: procedural commitment to investment without structural implementation. If future productivity growth fails to materialize (productivity puzzle post-2007), or if investment benefits concentrate in specific regions/occupations while costs spread broadly, reclassification to Snare becomes defensible. The false-summit mountain (natural-law time preference) must be maintained as a live alternative because temporal discounting IS a real structural feature of human life — the constraint exploits natural time preference rather than creating it from scratch. The resolution will require long-term outcome measurement (comparing actual 2025-2050 productivity and distributional outcomes to counterfactual no-rebalancing scenarios) and political legitimacy assessment (did democratic processes genuinely endorse this redistribution across time, or was it elite imposition?).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    investment_measurement_ambiguity,
    'What constitutes ''public investment'' versus ''consumption'' in federal budgeting? Are human capital transfers (education, healthcare) investment or consumption?',
    'International comparison of countries using different definitional frameworks; outcome correlation analysis (countries treating healthcare as investment vs. consumption); long-term growth accounting attribution',
    'If education/healthcare classified as investment: extractiveness drops to 0.35 (Rope). If classified as consumption: extractiveness rises to 0.68 (Snare). The constraint''s type classification is highly sensitive to this definitional boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(investment_measurement_ambiguity, conceptual, 'Definitional boundary between public investment and consumption').

omega_variable(
    productivity_growth_attribution,
    'Can the 1990s-2000s productivity acceleration be causally attributed to the 1993 budget rebalancing, or is it driven by technological cycles (internet adoption, Moore''s Law)?',
    'Econometric decomposition of productivity growth; comparison with peer economies with different budget mixes; identification of causal pathways from specific investment categories to measured productivity',
    'If causally attributable: investment redistribution is genuine coordination with measurable long-term benefit (Rope strengthened). If technological: redistribution is arbitrary timing coincidence (Snare strengthened, Mountain weakened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_growth_attribution, empirical, 'Causal attribution of 1990s productivity growth to budget rebalancing').

omega_variable(
    intergenerational_discounting_legitimacy,
    'Is the choice to privilege future over present consumption a legitimate democratic decision or an imposition of elite time preferences on politically powerless current recipients?',
    'Public opinion tracking; electoral analysis of how budget rebalancing affected voting patterns across income groups; comparison with jurisdictions using explicit intergenerational deliberative processes',
    'If legitimate democratic preference: tangled_rope classification stands. If elite imposition: reclassify current victims'' perspective as Snare with higher suppression and analytical perspective as false-summit mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discounting_legitimacy, preference, 'Legitimacy of intergenerational preference aggregation in budget choices').

omega_variable(
    distributional_incidence_concentration,
    'Do the benefits of public investment concentrate in specific geographic regions, demographic groups, or skill categories while costs are spread broadly? Is the constraint actually extracting from diffuse present populations to concentrated future beneficiaries?',
    'Regional analysis of investment location; demographic decomposition of who attends improved schools, uses infrastructure, benefits from job creation; occupational wage premium tracking before/after constraint',
    'If benefits highly concentrated: reclassify as Snare (asymmetric extraction concentrated). If broadly distributed: Tangled Rope classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_concentration, empirical, 'Distributional concentration of investment benefits').

omega_variable(
    crowding_out_mechanism,
    'Does federal public investment crowd out private investment, leaving total capital formation unchanged? Or does it genuinely increase aggregate investment?',
    'Time-series analysis of private vs. public investment during rebalancing period; international comparison with countries having different public/private investment mixes; event-study analysis around major policy shifts',
    'If high crowding-out: constraint is zero-sum redistribution (Snare). If low crowding-out: constraint enables genuine new investment (Rope strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crowding_out_mechanism, empirical, 'Degree of private investment crowding-out from public investment expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1993_clinton_public_investment_prioritization, 1993, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu93_tr_t0, sotu_1993_clinton_public_investment_prioritization, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu93_tr_t5, sotu_1993_clinton_public_investment_prioritization, theater_ratio, 5, 0.55).
narrative_ontology:measurement(sotu93_tr_t10, sotu_1993_clinton_public_investment_prioritization, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sotu93_tr_t20, sotu_1993_clinton_public_investment_prioritization, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(sotu93_be_t0, sotu_1993_clinton_public_investment_prioritization, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sotu93_be_t5, sotu_1993_clinton_public_investment_prioritization, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(sotu93_be_t10, sotu_1993_clinton_public_investment_prioritization, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(sotu93_be_t20, sotu_1993_clinton_public_investment_prioritization, base_extractiveness, 20, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1993_clinton_public_investment_prioritization, resource_allocation).
narrative_ontology:affects_constraint(sotu_1993_clinton_public_investment_prioritization, federal_tax_progressivity_design).
narrative_ontology:affects_constraint(sotu_1993_clinton_public_investment_prioritization, social_security_reform_debate).
narrative_ontology:affects_constraint(sotu_1993_clinton_public_investment_prioritization, infrastructure_investment_capacity).
narrative_ontology:affects_constraint(sotu_1993_clinton_public_investment_prioritization, higher_education_financing_structure).

% DUAL FORMULATION NOTE:
% Public investment rebalancing decomposes into multiple domain-specific constraints with different extractiveness values. The federal tax progressivity structure (ε=0.44) determines who bears rebalancing costs. Social Security reform debate (ε=0.58) directly implements extraction from elderly. Infrastructure investment capacity (ε=0.28, Rope) shows whether coordination actually occurs. Higher education financing (ε=0.51, Tangled Rope) shows asymmetric benefit distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1993_clinton_public_investment_prioritization, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
