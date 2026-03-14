% ============================================================================
% CONSTRAINT STORY: generational_wealth_accumulation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_wealth_accumulation_asymmetry, []).

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
 *   constraint_id: generational_wealth_accumulation_asymmetry
 *   human_readable: Generational Wealth Accumulation Asymmetry
 *   domain: economic/intergenerational
 *
 * SUMMARY:
 *   Generational wealth accumulation asymmetry describes the structural
 *   constraint in which capital already deployed generates returns faster
 *   than wages accumulate, creating a self-reinforcing gap between inheriting
 *   and non-inheriting cohorts. This constraint exhibits genuine coordination
 *   function (capital markets allocate resources, families transfer wealth
 *   across generations, financial institutions provide essential
 *   intermediation) alongside systematic extraction (compound growth
 *   guarantees wealth concentration independent of merit, inherited
 *   advantages create structural barriers to accumulation for others,
 *   financial services extract rents). The constraint is neither pure
 *   coordination nor pure extraction but a hybrid mechanism where the
 *   coordination function enables the extraction mechanism. The theater ratio
 *   of 0.48 reflects moderate performative content: meritocratic ideology
 *   ('level playing field,' 'anyone can build wealth') provides narrative
 *   cover for structural mechanisms that systematically disadvantage
 *   non-inheritors. Over the 60-year interval measured, extractiveness has
 *   increased from 0.32 to 0.62, driven by accelerating capital returns
 *   relative to wage growth and declining real inheritance tax rates. Theater
 *   ratio has also increased slightly (0.38 to 0.52), indicating growing gap
 *   between meritocratic narrative and structural reality.
 *
 * KEY AGENTS:
 *   - Non-Inheriting Wage Workers: Primary victims (powerless/trapped) — structural barriers prevent capital accumulation; income from wages insufficient to build wealth; no legitimate exit pathway
 *   - First-Generation Wealth Builders: Secondary victims/moderate actors (moderate/constrained) — face both coordination benefits (credit access, institutional support) and extraction costs (competing with inherited capital, eventual market saturation); constrained exit (costly relocation, sector change, debt burden)
 *   - Wealth-Inheriting Families: Primary beneficiaries (institutional/arbitrage) — constraint designed around their intergenerational transfer; zero suppression experienced; abundant exit options (tax planning, diversification); net extraction flows toward them
 *   - Financial Services Industry: Secondary beneficiaries (institutional/arbitrage) — extract rents through asset management fees, lending spreads, trust administration; benefit from wealth concentration and complexity that justifies their intermediation
 *   - Policy Reform Coalition: Organized challengers (organized/constrained) — recognize both coordination function and extraction; organized but politically constrained by beneficiary power concentration; moderate effective extraction despite organized status
 *   - Meritocratic Ideology: Institutional narrative actor (institutional/arbitrage) — provides cover story for extraction through 'level playing field' and 'anyone can succeed' framing; increasingly strained as extraction accelerates and gap widens
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees constraint as genuinely hybrid (both coordination and extraction real); recognizes that compounding at civilizational time horizon approaches inevitability; time horizon and scope significantly affect classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_wealth_accumulation_asymmetry, 0.58).
domain_priors:suppression_score(generational_wealth_accumulation_asymmetry, 0.65).
domain_priors:theater_ratio(generational_wealth_accumulation_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_wealth_accumulation_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(generational_wealth_accumulation_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(generational_wealth_accumulation_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_wealth_accumulation_asymmetry, tangled_rope).
narrative_ontology:human_readable(generational_wealth_accumulation_asymmetry, "Generational Wealth Accumulation Asymmetry").
narrative_ontology:topic_domain(generational_wealth_accumulation_asymmetry, "economic/intergenerational").

domain_priors:requires_active_enforcement(generational_wealth_accumulation_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(generational_wealth_accumulation_asymmetry, wealth_inheriting_families).
narrative_ontology:constraint_beneficiary(generational_wealth_accumulation_asymmetry, financial_service_providers).
narrative_ontology:constraint_beneficiary(generational_wealth_accumulation_asymmetry, property_owners).
narrative_ontology:constraint_victim(generational_wealth_accumulation_asymmetry, non_inheriting_cohorts).
narrative_ontology:constraint_victim(generational_wealth_accumulation_asymmetry, first_generation_builders).
narrative_ontology:constraint_victim(generational_wealth_accumulation_asymmetry, wage_dependent_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-INHERITING WAGE WORKER (SNARE) — Trapped by structural barriers to asset accumulation. Income from wages follows predictable growth; wealth accumulation requires capital already deployed. Suppression is total: credit constraints, down payment barriers, geographic immobility, educational debt burden. No legitimate exit pathway exists except through rare upward mobility. Experienced extraction is maximal.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER / FIRST-GENERATION BUILDER (TANGLED ROPE) — Faces genuine coordination problem: capital markets must allocate credit efficiently AND wealth concentration creates extraction as accumulation accelerates. Early builders benefit from institutional mechanisms (credit access, tax incentives for business formation) but later encounter suppression as inherited capital dominates their sector. Mixed: coordination function exists (market efficiency) but asymmetric extraction emerges as wealth compounds. Exit is costly but possible (debt burden, geographic relocation, sector change).
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL SERVICES INDUSTRY (ROPE) — Net beneficiary via fee extraction (asset management, lending spreads, trust administration). Experiences constraint as pure coordination: efficiently channeling capital requires standard mechanisms. Exit options abundant (regulatory arbitrage, product innovation). Extraction runs toward this sector — they coordinate wealth transfer and extract rents. No suppression experienced; suppression is deployed by them through product opacity and fee structures.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTH-INHERITING FAMILY (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism: intergenerational wealth transfer is the mechanism's core function. Tax codes, trust structures, and estate planning are perceived as legitimate tools for coordinating family financial continuity. Zero suppression (the constraint enables their goals). Beneficiary status is structural — constraint extracts on their behalf from all other agents. No exit pressure; system is designed around their participation.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY REFORM COALITION (TANGLED ROPE) — Organized agents (labor unions, housing advocates, wealth-tax proponents) see both coordination failure (market allocation mechanisms are breaking down) and extraction (wealth compounding creates structural inequality). Recognize beneficiary coordination function (families need wealth transfer mechanisms) but demand redistribution. Exit constrained by political power asymmetry and diffuse beneficiary interests. Moderate effective extraction despite organized status.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MERITOCRATIC IDEOLOGY (PITON) — Theater ratio high (0.48 already reflects substantial performative content). Institutions claim inheritance-blind opportunity (admissions essays, 'level playing field' rhetoric, scholarships as compensation) while structural mechanisms preserve wealth pathways. Ideology maintains the constraint through narrative cover without functional verification. Declining theater might reveal true mechanism: inherited capital determines outcomes more than merit.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, generational wealth asymmetry contains both genuine coordination function (intergenerational capital transfer enables long-term investment, family continuity, entrepreneurship) AND systematic extraction (compound growth of inherited capital guarantees wealth concentration regardless of merit or effort). The constraint is not purely extractive nor purely coordinative — it is genuinely hybrid. Effective extraction chi scales with scope: national scope shows moderate chi (0.58); global scope shows that wealth concentration persists across borders, amplifying chi. Time horizon matters: at biographical scale, constraints are visible and bearable; at civilizational scale, compounding creates inevitability that mimics natural law.
constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_wealth_accumulation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_wealth_accumulation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generational_wealth_accumulation_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(generational_wealth_accumulation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(generational_wealth_accumulation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significantly from non-inheriting cohorts through multiple mechanisms: (1) capital returns systematically exceed wage growth, (2) inherited capital compounds independent of effort, (3) financial services extract rents through products designed for existing wealth, (4) entry barriers increase as capital concentration rises. However, extractiveness is not maximal (not 0.85+) because: (a) wage workers can accumulate some capital over time (slow, but possible), (b) some intergenerational mobility occurs through extreme effort (rare but visible), (c) institutional mechanisms do provide real coordination function beyond extraction (credit allocation does serve genuine economic purposes). The measured increase from 0.32 to 0.62 reflects declining countervailing forces: inheritance tax rates have fallen, wage-capital return gap has widened, wealth concentration has accelerated. Suppression (0.65): High. Barriers to capital accumulation are substantial and structural: credit constraints (banks favor collateral-backed lending, which favors existing wealth), down payment requirements (housing market gates entry), educational debt (reduces saving capacity), income volatility (prevents consistent capital deployment), geographic immobility (trapped in low-wage regions by family/social roots). These barriers are real external constraints, not internal identity locks — the suppression is structural. Theater ratio (0.48): Moderate. Significant performative content exists in meritocratic framing and 'anyone can build wealth' narratives, but the constraint also has genuine functional content: capital markets do allocate resources, wealth transfer does enable long-term planning, financial intermediation does solve coordination problems. The gap between narrative ('merit determines outcomes') and mechanism ('inherited capital compounds regardless of merit') is growing, evidenced by rising gap between theater ratio expectations and actual outcomes. Rising theater ratio (0.38 → 0.52) indicates ideology becoming increasingly strained as structural extraction accelerates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across institutional positions. The wealth-inheriting family sees coordination (Rope) — legitimate intergenerational transfer with institutional support. The financial services industry also sees coordination (Rope) — efficient capital allocation through their intermediation. The first-generation builder sees mixed (Tangled Rope) — benefits from early-stage credit access but increasingly constrained by inherited capital dominance. The non-inheriting wage worker sees pure extraction (Snare) — no pathway to capital accumulation, total suppression, structural exclusion. The policy reform coalition sees hybrid with potential exit (Tangled Rope) — recognizes both genuine coordination and systematic extraction, but politically constrained. The meritocratic ideology performs as functional (Rope) — claims wealth follows merit — but increasingly strained (approaching Piton status as gap widens). The analytical observer sees genuine hybrid (Tangled Rope) — coordination function is real AND extraction is real, and they reinforce each other. The gap is not observational but structural: each perspective is reading a different flow of the same constraint. Beneficiaries see coordination because the constraint coordinates on their behalf. Victims see extraction because they bear the cost of that coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural relationships. Wealth-inheriting families occupy beneficiary + arbitrage position: low d (≈0.10), producing negative f(d) ≈ -0.01, which means effective extraction χ runs toward them (constraint subsidizes them). Financial services occupy secondary beneficiary + institutional position: d ≈0.20, producing f(d) ≈0.02, also negative χ (constraint benefits them). Non-inheriting wage workers occupy victim + trapped position: d ≈0.95, producing f(d) ≈1.42, maximum effective extraction (constraint extracts maximally from them). First-generation builders occupy mixed position (some benefit from credit access, some extraction from capital competition): d ≈0.60, producing f(d) ≈0.85, moderate extraction. Policy reform coalition occupies organized + constrained victim position: d ≈0.70, producing f(d) ≈1.10, high extraction despite organized status (organized power insufficient to offset structural targeting). The temporal dimension amplifies: at immediate time horizon, extraction appears moderate (borrowing available, assets growing); at generational time horizon, compounding becomes visible (wealth gap widens); at civilizational time horizon, gap approaches inevitability (looks like natural law but is policy-contingent).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE SATISFIED: (1) Genuine coordination function exists: capital markets allocate resources, families coordinate intergenerational wealth transfer, financial services intermediate these flows efficiently. These functions are not merely cover stories — they solve real coordination problems. (2) Asymmetric extraction is real: non-inheriting cohorts bear disproportionate costs (suppression via credit barriers, wealth concentration excluding them from capital returns); beneficiaries gain asymmetric benefits (inherited capital compounds, financial services extract rents, compounding guarantees wealth concentration). (3) Active enforcement required: tax codes (inheritance tax rates, capital gains treatment), credit regulations (collateral requirements, lending standards), educational systems (credentials as capital proxy), financial market structure (barriers to entry for unsophisticated investors). These are actively maintained to sustain the wealth transfer function. MANDATROPHY RESOLUTION: The constraint's ambiguity is not 'is this coordination or extraction?' but 'how much of each?'. At the analytical/civilizational level, the answer is: both, equally, and they are structurally linked (the coordination function creates the extraction mechanism, the extraction accumulation validates the need for coordination). The constraint is Tangled Rope from all institutional perspectives because all agents recognize both the coordination and extraction — their disagreement is about weighting and fairness, not about whether both exist. The snare classification (from powerless/trapped) is not a competing classification but a perspectival reading of the same hybrid: for trapped agents, the tangled rope looks like a pure snare because they experience only the extraction and cannot exit. For beneficiaries, it looks like pure coordination because they benefit from the coordination function and do not experience suppression. Both are correct perspectival readings. The analytical reading (Tangled Rope, both equally real) is the structural summary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_returns_versus_wage_growth,
    'Are capital returns structurally decoupled from wage growth, or does the decoupling reflect cyclical economic conditions?',
    'Long-run (50+ year) correlation analysis between equity returns, real wage growth, and productivity growth across economies with different capital taxation regimes',
    'If permanently decoupled: extraction is unavoidable and snare classification dominates. If cyclical: policy intervention can re-couple them, scaffold classification becomes viable. Determines whether the constraint is structural (mountain-adjacent) or policy-dependent (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_returns_versus_wage_growth, empirical, 'Structural decoupling of capital returns from wage growth').

omega_variable(
    inheritance_vs_meritocratic_sorting,
    'To what degree do inherited advantages compound through meritocratic selection (high-aptitude individuals with capital access outperform) versus simply reproducing themselves (capital creates pathways independent of merit)?',
    'Comparison of outcomes between (high-aptitude, no inherited capital) and (low-aptitude, high inherited capital) cohorts; correlation of genetic IQ scores with wealth outcomes controlling for inheritance',
    'If sorting dominant: constraint functions as rope with secondary extraction. If reproduction dominant: constraint is snare disguised as meritocracy. Determines whether the piton perspective''s ideology is functional (rope) or performative (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inheritance_vs_meritocratic_sorting, empirical, 'Relative strength of meritocratic sorting versus inheritance reproduction').

omega_variable(
    wealth_taxation_feasibility,
    'Can wealth taxation (property tax, estate tax, wealth tax) be implemented at rates that reverse accumulation asymmetry without capital flight or avoidance rendering the tax ineffective?',
    'Historical data from European wealth taxes; comparative analysis of capital mobility under different tax regimes; modeling of evasion rates and behavioral response to wealth tax increases',
    'If feasible and politically viable: scaffold classification with genuine sunset (policy can decouple returns from inheritance). If infeasible or avoidable: extraction is structural and snare classification persists. Determines whether policy reform coalition''s exit path is real or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wealth_taxation_feasibility, empirical, 'Feasibility of wealth taxation as extraction reversal mechanism').

omega_variable(
    intergenerational_mobility_floor,
    'Does generational wealth asymmetry set a hard floor on social mobility (some fraction of non-inheriting cohorts can never accumulate comparable wealth regardless of effort), or is mobility floor conditional on time horizon and market conditions?',
    'Longitudinal analysis of wealth trajectories across multiple cohorts; identification of whether non-inheriting individuals ever reach inherited-wealth baseline within their lifetime',
    'If hard floor exists: constraint is mountain-adjacent (structural limit on mobility). If conditional: floor is policy-modifiable and snare classification is appropriate (extraction, not law). Determines whether the powerless agent''s exit is impossible or merely expensive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mobility_floor, empirical, 'Whether social mobility has a hard structural floor under inheritance asymmetry').

omega_variable(
    financial_literacy_substitution,
    'Can financial literacy interventions and retail investment accessibility (low-fee index funds, fractional shares) meaningfully narrow the wealth accumulation gap, or is the gap driven by capital availability rather than investment knowledge?',
    'Controlled trials of financial education programs; comparison of outcomes between education-treated and untreated cohorts in same capital availability contexts; correlation of financial knowledge with wealth outcomes',
    'If substitutable: constraint operates primarily through suppression of information (rope with education solution). If not substitutable: constraint is structural (capital scarcity itself is the extraction, snare classification correct). Determines whether the theater ratio reflects genuine coordination (meritocratic ideology has functional basis) or pure performance (ideology masks capital barriers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_literacy_substitution, empirical, 'Substitutability of financial literacy for capital availability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_wealth_accumulation_asymmetry, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gwaa_tr_t0, generational_wealth_accumulation_asymmetry, theater_ratio, 0, 0.38).
narrative_ontology:measurement(gwaa_tr_t20, generational_wealth_accumulation_asymmetry, theater_ratio, 20, 0.43).
narrative_ontology:measurement(gwaa_tr_t40, generational_wealth_accumulation_asymmetry, theater_ratio, 40, 0.48).
narrative_ontology:measurement(gwaa_tr_t60, generational_wealth_accumulation_asymmetry, theater_ratio, 60, 0.52).

% Extraction over time
narrative_ontology:measurement(gwaa_be_t0, generational_wealth_accumulation_asymmetry, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gwaa_be_t20, generational_wealth_accumulation_asymmetry, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(gwaa_be_t40, generational_wealth_accumulation_asymmetry, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(gwaa_be_t60, generational_wealth_accumulation_asymmetry, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_wealth_accumulation_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(generational_wealth_accumulation_asymmetry, 0.18).
narrative_ontology:affects_constraint(generational_wealth_accumulation_asymmetry, intergenerational_social_mobility).
narrative_ontology:affects_constraint(generational_wealth_accumulation_asymmetry, educational_credentialism).
narrative_ontology:affects_constraint(generational_wealth_accumulation_asymmetry, housing_market_financialization).
narrative_ontology:affects_constraint(generational_wealth_accumulation_asymmetry, debt_trap_reproduction).

% DUAL FORMULATION NOTE:
% Generational wealth asymmetry is a parent constraint affecting multiple downstream constraints. Intergenerational mobility, educational credentialism, housing financialization, and debt trap reproduction all operate within the structural field created by this constraint. Each downstream constraint has its own extractiveness value reflecting specific mechanisms; this parent constraint captures the meta-mechanism (capital returns > wage growth) that enables all of them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(generational_wealth_accumulation_asymmetry, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
