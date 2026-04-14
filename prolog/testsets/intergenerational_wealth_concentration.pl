% ============================================================================
% CONSTRAINT STORY: intergenerational_wealth_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intergenerational_wealth_concentration, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: intergenerational_wealth_concentration
 *   human_readable: Intergenerational Wealth Concentration
 *   domain: economic/social/institutional
 *
 * SUMMARY:
 *   Intergenerational wealth concentration is a structural constraint where
 *   inherited capital compounds faster than wage income grows, creating
 *   self-perpetuating advantage for wealth inheritors and structural
 *   disadvantage for wage-dependent workers. The constraint operates through
 *   multiple mechanisms: legal inheritance and estate transfer, access to
 *   credit and capital markets, tax avoidance structures, educational
 *   advantages that compound inherited advantage, and social networks that
 *   correlate with wealth. This is a pure extraction constraint from the
 *   perspective of those locked outside the wealth accumulation system, but
 *   appears as coordination (efficient capital deployment) from the financial
 *   services perspective. The constraint exhibits high theater_ratio
 *   reflecting the gap between the meritocratic narrative (talent and hard
 *   work create wealth) and the structural reality (inheritance and access to
 *   capital are dominant). Extractiveness has increased over the 60-year
 *   interval as wealth concentration has accelerated, inheritance mechanisms
 *   have become more sophisticated, and the wage-productivity gap has
 *   widened. The theater ratio has increased as the gap between egalitarian
 *   rhetoric and actual concentration grows, requiring more performative
 *   justification.
 *
 * KEY AGENTS:
 *   - Wage-Dependent Workers: Primary victims (powerless/trapped) — locked outside capital accumulation; intergenerational reproduction of poverty despite wage work
 *   - Wealth Inheritors: Primary beneficiaries (institutional/arbitrage) — accumulate capital through inheritance independent of effort; compound advantages across generations
 *   - Financial Services Industry: Secondary beneficiary (institutional/arbitrage) — extracts value through estate planning, trust management, investment advisory services serving wealth concentration
 *   - Small Business Owners: Mixed position (moderate/constrained) — benefit from potential wealth transfer but constrained by unequal competitive advantage and succession uncertainty
 *   - Globalized Manufacturing Sector: Organized actors (organized/constrained) — both benefit from inherited capital investment and constrained by wage-pressure instability
 *   - Inheritance Tax System: Institutional actor (institutional/arbitrage) — designed as constraint mechanism but substantially performative through avoidance structures; maintains legitimacy despite degraded function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional arrangements as mathematical laws of capital accumulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intergenerational_wealth_concentration, 0.68).
domain_priors:suppression_score(intergenerational_wealth_concentration, 0.72).
domain_priors:theater_ratio(intergenerational_wealth_concentration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intergenerational_wealth_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(intergenerational_wealth_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(intergenerational_wealth_concentration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intergenerational_wealth_concentration, snare).
narrative_ontology:human_readable(intergenerational_wealth_concentration, "Intergenerational Wealth Concentration").
narrative_ontology:topic_domain(intergenerational_wealth_concentration, "economic/social/institutional").

domain_priors:requires_active_enforcement(intergenerational_wealth_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intergenerational_wealth_concentration, wealth_inheritors).
narrative_ontology:constraint_beneficiary(intergenerational_wealth_concentration, financial_services_industry).
narrative_ontology:constraint_beneficiary(intergenerational_wealth_concentration, estate_planning_professionals).
narrative_ontology:constraint_victim(intergenerational_wealth_concentration, non_inheriting_population).
narrative_ontology:constraint_victim(intergenerational_wealth_concentration, wage_dependent_workers).
narrative_ontology:constraint_victim(intergenerational_wealth_concentration, structurally_excluded_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE-DEPENDENT WORKER (SNARE) — No inheritance, no capital accumulation pathway, constrained to labor income. Exit is structurally impossible within the constraint's operation horizon. Each generational cycle reproduces the trap: wealth compounds for inheritors while wage earners face rising costs of housing, education, healthcare. Maximum experienced extraction from the perspective of those locked outside the wealth accumulation mechanism.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Benefits from capital ownership and potential wealth transfer to heirs, but constrained by estate taxation, succession planning uncertainty, and the need to compete against intergenerationally advantaged firms. Genuine coordination function (wealth concentration incentivizes productive capital deployment); genuine extraction cost (high-friction wealth transfer and competitive disadvantage if starting without inherited assets). Mixed experience: benefits alongside costs.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SERVICES INDUSTRY (ROPE) — Experiences intergenerational wealth concentration as pure coordination: wealth concentration creates demand for estate planning, trust management, investment advisory, and wealth structuring services. Benefits directly from the constraint without bearing extraction cost. High arbitrage capacity — can shift business models, regulatory jurisdictions, and service portfolios based on wealth distribution shifts.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBALIZED MANUFACTURING SECTOR (TANGLED ROPE) — Organized actors (multinational corporations, supply chains) both benefit from and are constrained by intergenerational wealth concentration. Benefits: capital inherited by family offices drives long-term investment in manufacturing capacity. Constrained: must compete for limited capital pools against rent-seeking financial services; labor cost pressures from wage-dependent workers with no inherited wealth buffer creates instability. Dual role creates perspectival ambiguity at organized power level.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INHERITANCE TAX SYSTEM (PITON) — Designed to enforce redistributive constraints on wealth transfer; now substantially performative. Theater ratio driven by extensive avoidance mechanisms (trusts, charitable structures, gifting strategies, offshore placement) that reduce effective tax rates far below statutory rates. The system maintains legitimacy through ritual compliance while actual wealth concentration proceeds unchecked. High inertia: tax code persists despite degraded function because political will for enforcement is absent and replacement systems are politically infeasible.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a certain frame, intergenerational wealth concentration appears as an immutable consequence of capital accumulation and compound interest: if wealth compounds at rates higher than wage growth, concentration is inevitable by mathematics alone. This perspective naturalizes contingent institutional arrangements (inheritance law, tax avoidance mechanisms, capital gains taxation, access to credit) as natural economic laws. The engine's false summit detector identifies this as naturalization requiring structural decomposition.
constraint_indexing:constraint_classification(intergenerational_wealth_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intergenerational_wealth_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intergenerational_wealth_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intergenerational_wealth_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intergenerational_wealth_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intergenerational_wealth_concentration, TR),
    TR >= 0.70.

:- end_tests(intergenerational_wealth_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically transfers resources from wage-dependent workers to capital inheritors. Starting extractiveness (0.52) reflects that inheritance mechanisms were weaker mid-20th century; current value (0.68) reflects decades of legal sophistication, tax avoidance infrastructure, and widening wage-capital productivity gaps. The trajectory shows acceleration: compound interest on inherited capital outpaces wage growth, and tax avoidance mechanisms have become more effective. Suppression (0.72): High. Multiple barriers prevent exit: capital controls and asset location laws restrict moving wealth across borders; lack of inherited capital prevents participation in wealth accumulation; educational disadvantage from poverty correlates with locked-in wage work; housing costs (driven by inherited capital investment) consume wage surplus preventing capital accumulation. Crucially, suppression is partly internalized through meritocratic framing that attributes wealth to talent rather than inheritance, creating identity-locked exit for some (those who believe hard work alone determines outcomes). Theater ratio (0.58): Moderate-high. The constraint maintains legitimacy through the narrative that wealth reflects merit, talent, and productive contribution — despite structural evidence that inheritance dominates. Tax systems maintain legitimacy through ritual compliance (high statutory rates, routine avoidance accepted as normal). Estate planning industry performs complexity and propriety. As the gap between egalitarian rhetoric and actual concentration grows (omega 4), theater ratio rises to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Critical gap between beneficiary rope/arbitrage perspectives (experience low extraction or coordination) and victim snare/trapped perspectives (experience high extraction with no exit). The gap reveals the asymmetry: the constraint is experienced as natural law (mountain) or efficient coordination (rope) by those who benefit, but as pure extraction (snare) by those trapped. The piton perspective shows degradation of the constraint's official function (inheritance tax as redistribution) while its extractive function accelerates, maintained through theater. The analytical mountain perspective reveals the highest-stakes gap: naturalizing institutional arrangements as mathematical law, when decomposition shows contingent design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (wealth inheritors, financial services industry) derive directionality from their position as capital extractors — d = 0.05 to 0.15. They benefit from the constraint and have arbitrage exit capacity (can move capital, restructure assets, shift jurisdictions). Institutional beneficiaries with arbitrage exit show negative effective extraction. Victims (wage-dependent workers, structurally excluded groups) derive directionality from their position as capital sources (their suppressed wages fund inherited capital growth) and their trapped exit options — d = 0.90 to 0.95. Maximum experienced extraction from trapped position. Mixed agents (small business owners, organized manufacturing) derive intermediate directionality from constrained exit and mixed beneficiary/victim status — d = 0.50 to 0.65. The analytical observer (d = 0.72) experiences moderate extraction from the analytical position's distance from capital flows.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates why mandatrophy matters. The high extractiveness (0.68) and suppression (0.72) produce snare classification, but the constraint maintains legitimacy through: (1) meritocratic narrative (you deserve what you have/you could inherit if you tried), (2) financial services rhetoric (wealth concentration enables productive capital deployment), and (3) mathematical framing (compound interest is immutable natural law). These cover stories prevent recognition of snare mechanics. Resolution requires distinguishing: What is genuinely coordination (efficient capital deployment)? What is genuinely extraction (inherited advantage accumulation)? What is theater (meritocratic narrative covering inheritance)? The mandatory answer: decompose into separate constraints. The wealth_accumulation_coordination constraint (ε=0.15, Rope) captures genuine coordination function — efficient capital deployment does require some incentive for inheritance. The inheritance_wealth_transfer constraint (ε=0.72, Snare) captures pure extraction — inheritance mechanisms above coordination baseline produce asymmetric advantage independent of productivity. The meritocratic_narrative constraint (ε=0.65, Piton) captures the performative maintenance — theater_ratio 0.85+ — of the false summit that conflates these mechanisms. The mandatrophy resolves by showing all three constraints are simultaneously true and should be modeled separately with their own ε values and perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherited_vs_accumulated_wealth_fungibility,
    'Are inherited wealth and self-accumulated wealth functionally equivalent in terms of extractive power, or does the inheritance mechanism create additional extraction beyond capital accumulation?',
    'Comparative analysis of intergenerational mobility rates across jurisdictions with different inheritance tax regimes and trust law frameworks. Temporal measurement of wealth concentration slopes in high-enforcement vs low-enforcement regimes.',
    'If functionally equivalent: the constraint is mathematically immutable (mountain). If inheritance mechanism amplifies extraction: the constraint is institutional and reversible (snare). Determines whether bases for reform exist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherited_vs_accumulated_wealth_fungibility, empirical, 'Whether inheritance law amplifies extraction beyond compound interest mechanics').

omega_variable(
    capital_flight_and_exit_capacity,
    'How much of the constraint''s effective suppression depends on exit barriers (capital controls, enforcement of tax law, geographic immobility) vs internalized acceptance of wealth inequality?',
    'Measurement of capital flight rates during estate tax policy shifts; analysis of behavioral responses to inheritance law changes across jurisdictions; survey data on perceived fairness and exit desire.',
    'If exit barriers dominate: suppression is structural (can be dismantled by border/tax enforcement). If internalized acceptance dominates: suppression is identity-locked (requires identity frame shifts). Determines intervention leverage points.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_flight_and_exit_capacity, empirical, 'Ratio of structural vs internalized suppression mechanisms').

omega_variable(
    global_wealth_inequality_plateau,
    'Does intergenerational wealth concentration operate under a natural ceiling beyond which further concentration becomes economically unstable, or can it accumulate indefinitely?',
    'Long-run historical analysis of wealth concentration under different regulatory regimes; dynamic modeling of wealth distribution under compound growth assumptions; identification of empirical concentration plateaus or breakpoints.',
    'If ceiling exists: constraint approaches natural limit (mountain-adjacent). If unbounded: extraction can accumulate indefinitely (pure snare). Determines whether the constraint is self-limiting or requires intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_wealth_inequality_plateau, empirical, 'Whether wealth concentration has natural ceiling or operates unbounded').

omega_variable(
    meritocratic_framing_vs_inheritance_reality,
    'To what extent does the constraint depend on maintaining the narrative that wealth indicates merit and talent, rather than inheritance?',
    'Measurement of educational access and earnings outcomes controlling for parental wealth; longitudinal tracking of belief in meritocracy before/after exposure to inheritance data; analysis of political responses when meritocratic framing breaks down.',
    'If meritocratic framing is critical: the constraint is identity-locked and vulnerable to narrative shift (rope at identity_locked exit). If independent of framing: the constraint is structurally maintained by law and capital access (snare regardless of narrative). Determines whether cognitive shift can destabilize the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meritocratic_framing_vs_inheritance_reality, conceptual, 'Whether meritocratic narrative is essential to constraint maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intergenerational_wealth_concentration, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iwc_tr_t0, intergenerational_wealth_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iwc_tr_t20, intergenerational_wealth_concentration, theater_ratio, 20, 0.48).
narrative_ontology:measurement(iwc_tr_t40, intergenerational_wealth_concentration, theater_ratio, 40, 0.58).
narrative_ontology:measurement(iwc_tr_t60, intergenerational_wealth_concentration, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(iwc_be_t0, intergenerational_wealth_concentration, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(iwc_be_t20, intergenerational_wealth_concentration, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(iwc_be_t40, intergenerational_wealth_concentration, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(iwc_be_t60, intergenerational_wealth_concentration, base_extractiveness, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intergenerational_wealth_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(intergenerational_wealth_concentration, 0.2).
narrative_ontology:affects_constraint(intergenerational_wealth_concentration, educational_access_stratification).
narrative_ontology:affects_constraint(intergenerational_wealth_concentration, real_estate_wealth_lock).
narrative_ontology:affects_constraint(intergenerational_wealth_concentration, wage_productivity_divergence).
narrative_ontology:affects_constraint(intergenerational_wealth_concentration, financial_services_rent_extraction).
narrative_ontology:affects_constraint(intergenerational_wealth_concentration, tax_avoidance_industry_growth).

% DUAL FORMULATION NOTE:
% Intergenerational wealth concentration decomposes into three structurally distinct constraints: (1) wealth_accumulation_coordination (ε=0.15, Rope) — genuine coordination function of inheritance incentivizing productive capital deployment, (2) inheritance_wealth_transfer (ε=0.72, Snare) — pure extraction of inherited advantage independent of productivity, (3) meritocratic_narrative (ε=0.65, Piton) — performative maintenance of the false summit that conflates coordination and extraction. Each story should be authored separately with distinct beneficiary/victim structures and perspectives. The current story models the aggregate constraint; decomposition enables precise intervention targeting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intergenerational_wealth_concentration, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
