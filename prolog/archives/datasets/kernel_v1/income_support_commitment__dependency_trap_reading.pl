% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Income Support Commitment: Dependency Trap Reading
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   Income support as implemented in most welfare states creates a structural
 *   tension that this reading instantiates: between the coordination benefit
 *   of a consumption floor and the extraction mechanism of reduced labor
 *   market discipline and skill atrophy. This is ONE READING of the contested
 *   income support commitment kernel. The dependency-trap reading emphasizes
 *   the mechanisms through which income support weakens market discipline,
 *   reduces skill accumulation, and creates intergenerational habituation to
 *   non-work. Future recipients enter the victim set through skill atrophy
 *   and reduced employability. Current taxpayers benefit through reduced
 *   short-term fiscal burden and social costs (crime, desperation-driven
 *   consumption). The reading coexists with two sibling readings: the
 *   freedom-floor reading (which emphasizes income support as a precondition
 *   for dignity and freedom from desperation) and the care-economy reading
 *   (which emphasizes income support as recognition of unpaid care work's
 *   value). The three readings compete for institutional dominance, shaping
 *   whether policy design prioritizes work requirements (dependency-trap),
 *   universality and unconditional support (freedom-floor), or care-sector
 *   recognition and interdependence (care-economy). This constraint story
 *   instantiates the dependency-trap reading only, documenting its structural
 *   implications and resolving ambiguities specific to this reading's
 *   framework.
 *
 * KEY AGENTS:
 *   - Future Income Support Recipients: Primary victim (powerless/trapped at generational scope) — face skill atrophy, employer screening bias, intergenerational habituation to non-work
 *   - Current Taxpayers / Fiscal Authority: Primary beneficiary (institutional/arbitrage) — benefit from consumption floor stability, reduced crime and desperation costs, lower fiscal pressure in short term
 *   - Marginal Labor Market Participants: Secondary agent (moderate/constrained) — benefit from income floor, constrained by wage discipline effects and reentry costs
 *   - Labor Market Organizing Coalition: Organized agent (organized/constrained) — perceive support as temporary enabling infrastructure for labor power, with sunset logic through organizing
 *   - Welfare Bureaucracy: Institutional actor (institutional/arbitrage) — maintains performative administrative apparatus with 30-40% theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional design as immutable incentive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.58).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.62).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Income Support Commitment: Dependency Trap Reading").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'd70f26d3-92ac-490b-8962-9c48c28dae01').
narrative_ontology:cs_kernel_codification('d70f26d3-92ac-490b-8962-9c48c28dae01', formalized).
narrative_ontology:cs_authority_grounding('d70f26d3-92ac-490b-8962-9c48c28dae01', lineage).
narrative_ontology:cs_interpretation_layer_present('d70f26d3-92ac-490b-8962-9c48c28dae01').
narrative_ontology:cs_reading_relation('d70f26d3-92ac-490b-8962-9c48c28dae01', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('d70f26d3-92ac-490b-8962-9c48c28dae01', income_support_commitment__care_economy_reading, coexists_with).
narrative_ontology:cs_axiom('d70f26d3-92ac-490b-8962-9c48c28dae01', foundational, work_market_discipline_necessary).
narrative_ontology:cs_axiom_status(work_market_discipline_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d70f26d3-92ac-490b-8962-9c48c28dae01', work_market_discipline_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d70f26d3-92ac-490b-8962-9c48c28dae01', secondary, skill_atrophy_mechanism_real).
narrative_ontology:cs_axiom_status(skill_atrophy_mechanism_real, holdable).
narrative_ontology:cs_axiom_grounding('d70f26d3-92ac-490b-8962-9c48c28dae01', skill_atrophy_mechanism_real, empirically_contingent).
narrative_ontology:cs_reference_frame('d70f26d3-92ac-490b-8962-9c48c28dae01', labor_market_discipline_regime).
narrative_ontology:cs_drift_state('d70f26d3-92ac-490b-8962-9c48c28dae01', contemporary_welfare_state_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d70f26d3-92ac-490b-8962-9c48c28dae01', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, current_taxpayers).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, welfare_bureaucracy).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, future_income_support_recipients).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, labor_market_discipline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOME SUPPORT RECIPIENT / INTERGENERATIONAL (SNARE) — Once enrolled in income support, recipients face skill atrophy, reduced work history accumulation, and employer screening bias. The longer the spell, the harder exit becomes. For intergenerational timeframes, children born into recipient households inherit reduced human capital accumulation, lower school engagement, and family habituation to non-work. The constraint extracts from future generations through mechanism of identity lock and normalized economic inactivity. Maximum extraction perceived by the powerless at generational scope.
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARGINAL LABOR MARKET PARTICIPANT (TANGLED ROPE) — Benefits from income floor and reduced desperation-driven wage acceptance. Also bears extraction: employer wage discipline weakens (if labor supply shifts due to income support availability), and wage growth may stagnate as market discipline erodes. Career progression becomes costlier (must re-accumulate credentials if exiting support). Moderate power; constrained exit options due to skill and credential costs.
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL AUTHORITY / CURRENT TAXPAYER (ROPE) — Experiences income support commitment as coordination mechanism: stabilizes labor supply, reduces desperation-driven crime and social costs, enables consumption and demand. Perceives primary benefit in short-term fiscal and social stability. At immediate timeframe, the extraction mechanism is opaque; the beneficiary role is clear (lower crime, more stable consumption = lower enforcement costs and better tax base). Current-period benefits dominate the calculus.
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR MARKET ORGANIZING COALITION (SCAFFOLD) — Organized agents (unions, labor advocates, worker organizing) perceive income support as temporary infrastructure supporting wage floors and labor organizing capacity. The sunset logic: as labor market tightens and worker power increases through organization, income support becomes less necessary. Coalition sees the constraint as enabling transition to stronger labor market fundamentals, not permanent dependency. Temporary support with exit path through labor power.
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WELFARE BUREAUCRATIC APPARATUS (PITON) — The administrative apparatus that implements income support has become substantially performative. Case management, eligibility verification, and compliance monitoring consume 30-40% of program expenditure in many jurisdictions while generating minimal work incentive effects. The bureaucracy persists through institutional inertia and because alternatives (unconditional basic income, negative income tax with unified administration) have not fully crystallized. The apparatus sees its own operations as degraded but lacks coalitional pressure to reform. Theater ratio (0.55) reflects that administrative theater has not fully consumed the coordination function, but significant portion is performative gatekeeping rather than effective support.
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, the tension between income support and work incentives appears as an immutable tradeoff: any income floor weakens the desperation mechanism that drives labor supply. The constraint appears as a natural law of incentive structures, independent of institutional design. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that what appears as a fundamental incentive law is actually a contingent feature of specific institutional arrangements (means-testing cliffs, implicit marginal tax rates, skill atrophy mechanisms).
constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_commitment__dependency_trap_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, TR),
    TR >= 0.70.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The dependency-trap reading identifies extraction through multiple mechanisms: (1) skill atrophy reducing future employability and wage capacity, (2) intergenerational habituation reducing aspiration and capability accumulation, (3) weakened labor market discipline allowing employer monopsony power to persist, (4) career restart costs for exit from support. These are not pure benefits-provisioning; they are extraction mechanisms that compound over time. The measurement trajectory (0.35 → 0.58) reflects accumulating extraction as recipients spend longer in support and intergenerational effects emerge. Suppression (0.62): Moderate-high. Barriers to exit include: skill atrophy (reversible but costly), employer screening bias against gaps in work history, means-test cliffs creating implicit 80-100% marginal tax rates, childcare costs and family stability disruption from accepting low-wage work, reduced credit access and housing options during support period. These are formidable but not total — some recipients do successfully exit to stable employment. Suppression rises from 0.45 to 0.62 as institutional cliffs become sharper and screening bias hardens. Theater ratio (0.55): Moderate. Significant administrative theater exists (eligibility verification, case management consuming 30-40% of expenditure) but the core income-provision function remains genuine. Theater has risen from 0.38 to 0.55 as means-testing compliance monitoring has intensified without corresponding improvement in work incentive effectiveness. The ratio reflects that support systems are not purely performative (funds do reach recipients) but substantial portion is theater rather than effective re-engagement mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates sharp perspectival gaps with sibling readings. The freedom-floor reading emphasizes dignity and unconditional entitlement; the dependency-trap reading emphasizes work incentives and market discipline. The care-economy reading emphasizes recognition of care work's value independent of market work; the dependency-trap reading emphasizes market participation as primary criterion. The snare perspective (powerless recipient, generational timeframe) emphasizes intergenerational trap and identity lock; the rope perspective (current taxpayer, immediate timeframe) emphasizes coordination benefit. The piton perspective reveals that the apparatus has become substantially performative despite the core extraction mechanism remaining real. The mountain perspective naturalizes the incentive tradeoff as immutable, but the structural data reveals it as a contingent feature of specific institutional design (means-test cliffs, work requirements, credential-dependent hiring).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Current taxpayers (beneficiary + arbitrage exit) experience low d ≈ 0.10 → negative effective extraction. Future recipients (victim + trapped exit at generational scope, but constrained at biographical scope) experience high d ≈ 0.85 → high effective extraction. Marginal labor participants (mixed agent with constrained exit) experience d ≈ 0.60 → moderate extraction. The analytical observer (analytical power, analytical exit) derives canonical d ≈ 0.73 but experiences high extraction because the observable (skill atrophy mechanisms, intergenerational effects) places the observer at analytical remove from the lived experience of extraction. No overrides are needed — the structural derivation from beneficiary/victim + exit options produces accurate d values for all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The dependency-trap reading resolves mandatrophy by anchoring to the mechanism of future recipient harm through skill atrophy and market discipline weakening. This reading is not claiming 'income support is always snare' — the institutional perspective sees coordination (rope), the organized coalition sees sunset structure (scaffold), the bureaucracy sees degradation (piton). Rather, this reading instantiates a specific structural claim: that the long-term effect on future recipients is extractive, regardless of the current generation's experience of coordination benefit. The mandatrophy is resolved by disaggregating temporal perspectives — immediate (current taxpayer benefit) from generational (future recipient harm). Both are real; the reading emphasizes one temporal direction of the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_atrophy_mechanism_clarity,
    'Is observed skill atrophy in long-term income support recipients primarily due to non-use of capabilities, or to selection effects (lower-skill individuals more likely to enter and remain on support)?',
    'Longitudinal skills testing (literacy, numeracy, technical capability) pre and post support enrollment, matched to control group of similar baseline capability in non-support trajectory. Exogenous policy variation in support generosity/duration.',
    'If primary mechanism is non-use atrophy: skill decay is reversible through retraining and workplace re-engagement; constraint is partially escape-able at moderate cost. If selection effect dominates: observed cohort differences reflect pre-existing heterogeneity, and atrophy fear may be overstated or addressed through selection-aware policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_mechanism_clarity, empirical, 'Whether skill atrophy is non-use mechanism or selection artifact').

omega_variable(
    labor_market_discipline_mechanism,
    'Does income support reduce effective market discipline on employer wage-setting and working conditions, or do labor supply shifts from reduced desperation cancel out demand-side monopsony power gains?',
    'Comparison of wage growth, benefit provision, and working condition improvements in labor markets with vs. without income support; analysis of employer job design and safety investment in high vs. low support jurisdictions. Test for differential effects by skill level and labor supply elasticity.',
    'If support weakens discipline: wage stagnation and condition degradation likely, extraction of labor value increases. If labor supply effects dominate: worker power improves, wage growth accelerates, market discipline strengthens. Classification may shift toward rope (coordination benefit dominates) from snare (extraction dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_discipline_mechanism, empirical, 'Whether income support reduces employer market discipline').

omega_variable(
    intergenerational_habituation_vs_aspiration,
    'Do children born into income-support households exhibit reduced labor market aspiration due to habituation and normalized non-work, or do they exhibit improved educational and labor outcomes due to reduced childhood poverty stress and material security?',
    'Longitudinal outcome tracking (educational attainment, employment rates, earnings) comparing children born into support-recipient households with non-recipient controls matched on baseline parental education and earnings. Policy experiments varying support generosity and duration.',
    'If habituation dominates: intergenerational dependency trap confirmed, extraction extends across generations through identity lock mechanism, snare classification strengthened. If poverty-alleviation effects dominate: long-term human capital improves, snare classification weakens toward tangled_rope or rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_habituation_vs_aspiration, empirical, 'Intergenerational effects of income support on children''s outcomes and aspirations').

omega_variable(
    reading_identity_contested_kernel,
    'Is income support primarily a safety-net intervention (dependency-trap reading) or a recognition of care economy contributions and relational interdependence (care-economy reading) or a floor for freedom and dignity (freedom-floor reading)?',
    'Examination of authority legitimacy claims: which foundational premise (reducing dependency through work incentives / recognizing care value / guaranteeing dignity floor) does the authority structure actually adjudicate? Which reading''s axioms are operationalized in policy design, measurement systems, and success criteria?',
    'If dependency-trap reading''s axiom (work-market-discipline-necessary) governs: policy design emphasizes clawbacks, time limits, work requirements. If care-economy reading''s axiom (care-work-valuable-regardless-of-market) governs: policy design emphasizes unconditional support and care-sector recognition. If freedom-floor reading''s axiom (dignity-floor-precondition) governs: policy design emphasizes universality and decoupling from work. The three readings coexist but influence institutional design; dominance shifts over jurisdictions and time periods.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_contested_kernel, conceptual, 'Kernel ambiguity: which reading dominates the institutional authority structure?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(incsup_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(incsup_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(incsup_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(incsup_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(incsup_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(incsup_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(incsup_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(incsup_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement(incsup_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__care_economy_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, labor_market_monopsony_power).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, means_test_welfare_cliff).

% DUAL FORMULATION NOTE:
% The income support commitment kernel decomposes into three structurally distinct constraint stories with different ε values and different victim/beneficiary structures. This story (dependency-trap reading) emphasizes future recipient harm through skill atrophy and market discipline weakening (ε=0.58). The freedom-floor reading emphasizes dignity as foundational (likely ε < 0.30, rope classification). The care-economy reading emphasizes care work recognition (likely ε ≈ 0.25, rope or tangled_rope). Each reading is a complete constraint story with its own perspectives, beneficiary/victim declarations, and measurements. They link through network.affects_constraints because they address the same kernel and compete for institutional dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
