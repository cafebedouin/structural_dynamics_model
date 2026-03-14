% ============================================================================
% CONSTRAINT STORY: income_inequality_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_inequality_accumulation, []).

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
 *   constraint_id: income_inequality_accumulation
 *   human_readable: Income Inequality Accumulation Trap
 *   domain: economic/structural
 *
 * SUMMARY:
 *   Income inequality accumulation represents a structural constraint where
 *   wealth compounds asymmetrically relative to wage income, creating
 *   self-reinforcing extraction from wage earners to capital owners. The
 *   constraint exhibits Snare properties at the powerless level (workers
 *   trapped in wage stagnation despite full-time employment) while
 *   manifesting as Rope (coordination function) for beneficiaries and as a
 *   potentially soluble Scaffold problem for organized reform coalitions. The
 *   theater ratio has increased over the interval (0.35 to 0.58) reflecting
 *   the proliferation of performative discourse about inequality — rising
 *   awareness, policy proposals, and public debate — without corresponding
 *   structural redistribution. Base extractiveness has risen from 0.42 to
 *   0.68 as wealth concentration has accelerated, particularly since 2000 as
 *   capital gains have dramatically outpaced wage growth and tax rates on
 *   capital have declined relative to wage taxes. Suppression (0.65) is high
 *   due to structural barriers: workers face high cost of capital
 *   acquisition, educational credentialing as gating mechanism, geographic
 *   immobility tied to housing costs, and deliberate weakening of collective
 *   bargaining institutions.
 *
 * KEY AGENTS:
 *   - Wage Earners and Precariat Workers: Primary victims (powerless/trapped) — face structural inability to accumulate capital despite labor income; subsistence consumption leaves no savings capacity; wage suppression via labor oversupply and union decline.
 *   - Capital Owners and High-Wealth Cohort: Primary beneficiaries (institutional/arbitrage) — capture asymmetric returns on invested capital; can deploy capital across jurisdictions and asset classes; benefit from capital-favorable tax policy and inheritance mechanisms.
 *   - Professional Salaried Class: Secondary actor (moderate/constrained) — possess credentials for partial wealth accumulation; experience mixed coordination (progressive infrastructure funding) and extraction (diverging returns on capital vs. labor); can exit via entrepreneurship but face capital barriers.
 *   - Labor Union Movement: Institutional degradation exemplar (organized/constrained) — historically coordinated wage compression and redistribution; now diminished from 35% to 10% union density; retains theatrical presence but lost structural power.
 *   - Progressive Tax and Transfer Coalition: Reform coalition (organized/constrained) — advocates for alternative coordination via progressive taxation, wealth taxes, UBI, and inheritance reform; have agency but face organized capital opposition.
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing policy-contingent inequality as mathematical inevitability; r > g framework can become false summit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_inequality_accumulation, 0.68).
domain_priors:suppression_score(income_inequality_accumulation, 0.65).
domain_priors:theater_ratio(income_inequality_accumulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_inequality_accumulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_inequality_accumulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_inequality_accumulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_inequality_accumulation, snare).
narrative_ontology:human_readable(income_inequality_accumulation, "Income Inequality Accumulation Trap").
narrative_ontology:topic_domain(income_inequality_accumulation, "economic/structural").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_inequality_accumulation, capital_owners).
narrative_ontology:constraint_beneficiary(income_inequality_accumulation, high_wealth_cohort).
narrative_ontology:constraint_victim(income_inequality_accumulation, wage_earners).
narrative_ontology:constraint_victim(income_inequality_accumulation, precariat_workers).
narrative_ontology:constraint_victim(income_inequality_accumulation, intergenerational_mobility_trapped).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIAT WORKER (SNARE) — Wage earners face structural immobility. Income goes to subsistence; wealth accumulation is impossible despite full-time work. Exit requires capital acquisition but extraction prevents accumulation. No arbitrage available — geographic, sectoral, and credential mobility all require upfront capital the worker cannot access. Captures maximum experienced extraction.
constraint_indexing:constraint_classification(income_inequality_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROFESSIONAL SALARIED CLASS (TANGLED ROPE) — Possesses credentials and income for partial wealth accumulation. Experiences genuine coordination function: progressive taxation funds infrastructure that enables economic function. Simultaneously experiences asymmetric extraction: returns on capital far exceed returns on labor; wealth accumulation inequality diverges from income inequality. Can exit via entrepreneurship or capital deployment but faces barriers of capital requirement, time cost, and risk.
constraint_indexing:constraint_classification(income_inequality_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL OWNERSHIP CLASS (ROPE) — Net beneficiary. Experiences the constraint as coordination mechanism: capital returns, reinvestment cycles, and wealth concentration mechanisms function smoothly. Tax policy, capital markets, and inheritance law coordinate to enable wealth accumulation. Experiences extraction as flowing toward them, not away. High-mobility arbitrage: can reallocate capital across jurisdictions, sectors, and asset classes.
constraint_indexing:constraint_classification(income_inequality_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION MOVEMENT (PITON) — Historically functioned as collective bargaining coordinator and redistribution mechanism. Now largely degraded institutional actor: union density declined from 35% (1950s) to 10% (2020s) in US. Retains performative role in wage negotiation but has lost structural power to constrain capital mobility or redirect accumulated wealth. Theater ratio high — ritualized collective bargaining persists despite reduced worker leverage. Organization remains but functional power atrophied.
constraint_indexing:constraint_classification(income_inequality_accumulation, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE TAX AND TRANSFER COALITION (SCAFFOLD) — Organized agents (union remnants, progressive policy coalitions, UBI advocates) perceive inequality accumulation as a solvable coordination problem with structural sunset mechanisms. Progressive taxation, wealth taxes, inherited wealth caps, and universal basic income represent alternative coordination pathways that could stabilize inequality at lower levels. These agents have agency and envision realistic exit from the accumulation trap. Theater ratio moderate — actual redistribution occurs (unemployment insurance, social security, EITC) alongside performative debate. Classification turns on whether sunset clause is credible.
constraint_indexing:constraint_classification(income_inequality_accumulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, inequality accumulation might appear as an immutable mathematical law: compound returns on capital necessarily diverge from linear wage growth; if r > g (capital return rate > growth rate), wealth concentration is inevitable. Piketty's framework naturalizes inequality as following from deep economic laws. However, the base properties contradict mountain classification — suppression (0.65) and extractiveness (0.68) are too high, beneficiary/victim structure is clearly designed, and resistance to redistribution policy is institutional not logical. The mountain reading is false naturalization.
constraint_indexing:constraint_classification(income_inequality_accumulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_inequality_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_inequality_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_inequality_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_inequality_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_inequality_accumulation, TR),
    TR >= 0.70.

:- end_tests(income_inequality_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting strong asymmetry between capital returns and wage growth. Real wage growth has stagnated since 1980s while capital returns have compounded at 5-8% annually, creating divergence that is policy-contingent (tax treatment, governance rules, labor law) not mathematical necessity. Suppression (0.65): Moderate-high, reflecting multiple structural barriers to worker exit: (1) capital requirement for upward mobility creates catch-22 (need wealth to earn capital returns but need returns to accumulate wealth); (2) credential inflation (college degree now required for jobs previously accessible to high school graduates); (3) housing costs anchoring workers geographically; (4) deliberate labor supply management via welfare policy and immigration controls; (5) union decline removing collective bargaining capacity. Theater ratio (0.58): Moderate-high and increasing. Policy debate about inequality has proliferated (media attention, UBI pilots, wealth tax proposals) while actual redistribution has stalled (top 1% share rose from 10% to 20% over interval). Theater reflects disconnect between problem recognition and structural change. Claimed type (Snare): High-extractiveness constraint where primary exit mechanism is unavailable — cannot exit wage-labor trap without capital accumulation, but capital accumulation is precisely what the extraction prevents.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that income inequality accumulation is not a unitary phenomenon but a presheaf of structural positions. From the precariat perspective it is a Snare (pure extraction with no exit). From the capital owner perspective it is a Rope (beneficial coordination). From the reform coalition perspective it is a Scaffold (temporary problem with policy solutions). From the analyst perspective it risks appearing as a Mountain (mathematical inevitability). Each reading is locally coherent within its structural position but globally incompatible — the same constraint cannot simultaneously be inescapable extraction, beneficial coordination, a solvable problem, and a natural law. This logical inconsistency is diagnostic: it indicates that the base properties reflect policy choices (tax code, labor law, inheritance rules, credential gatekeeping) that different actors experience from incompatible structural positions. The gap is not measurement error but structural. Resolving it requires moving beyond single-perspective analysis to acknowledge that inequality accumulation IS a design choice — institutions could be reorganized to produce lower inequality trajectories with different distributional outcomes for each perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to the extraction flow. Wage earners (powerless/trapped) occupy maximum d ≈ 0.95: all barriers to exit are material (capital requirement, time cost, credential gatekeeping), and the extraction flow runs entirely toward capital owners. Capital owners (institutional/arbitrage) occupy minimum d ≈ 0.05: they are beneficiaries with high mobility (can reallocate capital, relocate to favorable tax jurisdictions, switch sectors). The professional class (moderate/constrained) occupies middle d ≈ 0.60: can exit via entrepreneurship or capital deployment but face significant barriers (capital requirement, time/risk cost, opportunity cost of lost salaried income). The reform coalition (organized/constrained) occupies d ≈ 0.55: organized enough to create alternative coordination pathways but constrained by organized capital opposition and institutional stickiness. The piton union (organized/constrained) occupies d ≈ 0.70: ostensibly working for workers but structurally captured by institutional decline, unable to exert genuine constraint on capital accumulation despite formal labor-representation role. The sigmoid function f(d) amplifies the experienced extractiveness for trapped and constrained agents (high d produces high f(d) ≈ 1.2-1.4) while compressing it for beneficiaries (low d produces low f(d) ≈ -0.12 to 0.02).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves by acknowledging that all six classifications are correct from their respective structural positions. The Snare classification for workers is real — they are trapped in a structure that extracts from them without exit. The Rope classification for capital owners is real — they benefit from smoothly functioning wealth accumulation mechanisms. The Tangled Rope for professionals is real — they experience both coordination benefits and extraction. The Scaffold for reformers is real — alternative institutional designs exist with credible sunset clauses (UBI could replace means-tested benefits, wealth tax could replace income tax, cooperative ownership could replace shareholder primacy). The Piton for unions is real — once-functional coordination institutions have degraded. The risk of false Mountain (natural law) is real — the r > g framework naturalizes what is policy contingent. The mandatrophy is not resolved by choosing 'the correct type' but by recognizing that inequality accumulation is a **policy-design presheaf**: different tax codes, labor laws, inheritance rules, and corporate governance structures produce different inequality trajectories. The Snare is not inevitable; it is designed by choices to suppress labor bargaining power, structure capital-favorable tax policy, and weaken redistribution institutions. An analytical observer who moves across structural positions discovers that the 'natural law' reading (Mountain) is a false summit — the constraint naturalizes what could be otherwise designed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_return_rate_assumption,
    'Does the r > g dynamic reflect unchangeable mathematics or contingent policy choices (capital gains tax, inheritance law, corporate governance)?',
    'Historical comparison across tax regimes and countries; periods where r was constrained and g elevated via policy; counterfactual modeling of alternative tax/governance structures',
    'If mathematical: inequality accumulation is mountain-class inevitable. If contingent: constraints are institutional policy choices, reclassifying as snare or tangled_rope amenable to design alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_return_rate_assumption, conceptual, 'Whether r > g reflects mathematics or policy contingency').

omega_variable(
    intergenerational_mobility_bottleneck,
    'Is low intergenerational mobility a consequence of inequality accumulation or a separable structural constraint? Can capital accumulation be decoupled from dynastic wealth transfer?',
    'Analysis of mobility trajectories in high-inequality vs lower-inequality economies; mechanisms specific to wealth transfer vs wage accumulation; feasibility of inheritance-tax alternatives',
    'If decoupled: wealth concentration is snare but mobility remains possible with policy (scaffold exit). If coupled: both wealth and mobility are trapped (full snare with no mitigation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_bottleneck, empirical, 'Whether intergenerational mobility can be decoupled from wealth accumulation').

omega_variable(
    alternative_coordination_feasibility,
    'Do alternative wealth allocation mechanisms (UBI, wealth taxes, cooperative ownership, stakeholder capitalism) constitute genuine coordination alternatives or theater masking continued extraction?',
    'Pilot program data on UBI and wealth tax efficacy; comparative institutional analysis of cooperative vs. capitalist wage-setting; mechanisms by which alternative coordination could fail or degrade',
    'If genuine alternatives exist: scaffold perspective confirmed, sunset credible, policy escape routes real. If theater: all alternatives collapse or get captured, snare is inescapable, mountain false naturalization fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether alternative wealth allocation mechanisms are viable').

omega_variable(
    labor_supply_elasticity_constraint,
    'How much of wage suppression reflects genuine scarcity of low-skill jobs vs. deliberate labor supply creation through welfare policy, immigration control, and credential inflation?',
    'Comparison of wage trajectories across job market tightness; policy counterfactuals (universal job guarantee, open immigration, credential reform); measurement of actual vs. constructed labor scarcity',
    'If scarcity is genuine: wage floors are constrained by fundamentals (mountain-like). If constructed: suppression is policy-designed (snare, fully extractive). Mix of both shifts extraction balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_constraint, empirical, 'Extent to which low-wage labor scarcity is constructed vs. genuine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_inequality_accumulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ineq_tr_t0, income_inequality_accumulation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ineq_tr_t10, income_inequality_accumulation, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ineq_tr_t20, income_inequality_accumulation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(ineq_be_t0, income_inequality_accumulation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ineq_be_t10, income_inequality_accumulation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ineq_be_t20, income_inequality_accumulation, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_inequality_accumulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_inequality_accumulation, 0.18).
narrative_ontology:affects_constraint(income_inequality_accumulation, wage_stagnation_structural).
narrative_ontology:affects_constraint(income_inequality_accumulation, capital_gains_prioritization).
narrative_ontology:affects_constraint(income_inequality_accumulation, educational_credential_inflation).
narrative_ontology:affects_constraint(income_inequality_accumulation, housing_cost_immobility).

% DUAL FORMULATION NOTE:
% Income inequality accumulation is upstream of multiple structural constraints. Wage stagnation, capital gains tax preferences, credential inflation as labor gatekeeping, and housing-cost-driven geographic immobility are all mechanistically downstream of the core inequality accumulation dynamic. Each has its own ε value reflecting measurement-specific aspects, but all are coupled to the primary inequality constraint through the wealth concentration mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_inequality_accumulation, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
