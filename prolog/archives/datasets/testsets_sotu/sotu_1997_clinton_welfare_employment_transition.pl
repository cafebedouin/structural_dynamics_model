% ============================================================================
% CONSTRAINT STORY: sotu_1997_clinton_welfare_employment_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1997_clinton_welfare_employment_transition, []).

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
 *   constraint_id: sotu_1997_clinton_welfare_employment_transition
 *   human_readable: Welfare-to-Work Employment Transition (1997 PRWORA)
 *   domain: social_policy/labor_market
 *
 * SUMMARY:
 *   The 1997 Personal Responsibility and Work Opportunity Reconciliation Act
 *   (PRWORA) institutionalized a constraint coupling mandatory work
 *   requirements for welfare recipients with employer tax credits (Work
 *   Opportunity Tax Credit), state job placement targets, and
 *   government-funded transition services (training, childcare,
 *   transportation). The constraint exhibits all classic features of tangled
 *   rope: genuine coordination function (matching workers with employers,
 *   bundling support services to enable transition) coupled with asymmetric
 *   extraction (wage suppression, vulnerability creation, shift of social
 *   risk from the state to the individual labor market). The constraint
 *   redistributes benefits to employers (subsidized hiring, reduced training
 *   costs, labor supply assurance) and states (reduced visible welfare
 *   caseloads, cost savings) while imposing costs on welfare recipients (work
 *   obligation, wage reduction, precarity) and low-wage workers generally
 *   (labor market saturation, wage depression). The theater ratio (0.48,
 *   increasing to 0.52) reflects the gap between policy narrative (work
 *   promotes dignity and self-sufficiency) and structural reality (many
 *   transition jobs are part-time, minimum-wage, or below-subsistence;
 *   support services time-limited; work obligations coerce labor market entry
 *   regardless of readiness). The constraint's extractiveness increased from
 *   0.42 to 0.60 over the interval as the initial coordination phase
 *   (matching workers to jobs, establishing services) gave way to
 *   enforcement-heavy management of long-term precarity.
 *
 * KEY AGENTS:
 *   - Welfare Recipients Under Mandate: Primary victims (powerless/trapped) — lose benefits if refusing work; forced into labor market with minimal bargaining power; face time-limited support services
 *   - Low-Wage Workers Without Welfare History: Secondary victims (powerless/trapped) — face wage suppression and competitive displacement from influx of subsidized welfare-worker hiring
 *   - Employers Hiring Welfare Workers: Primary beneficiaries (institutional/arbitrage) — receive tax credits, government-subsidized training, assured worker pipeline; benefit from reduced bargaining power of hires
 *   - State Welfare Administration: Institutional actor (organized/constrained) — constrained by federal compliance and caseload reduction targets; benefits from reduced visible welfare dependency and cost savings; coordinates state employment infrastructure
 *   - Support Service Providers: Secondary institutional actor (institutional/constrained) — provide training, childcare, transportation, job placement services; benefit from contract funding; constrained by time-limited authorization and sufficiency debates
 *   - Analytical Observer: Civilizational view (analytical/analytical) — observes genuine coordination coupled with structural extraction; notes tension between sunset logic (services time-limited) and perpetual precarity (workers cycling through low-wage labor)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1997_clinton_welfare_employment_transition, 0.58).
domain_priors:suppression_score(sotu_1997_clinton_welfare_employment_transition, 0.65).
domain_priors:theater_ratio(sotu_1997_clinton_welfare_employment_transition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1997_clinton_welfare_employment_transition, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1997_clinton_welfare_employment_transition, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1997_clinton_welfare_employment_transition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1997_clinton_welfare_employment_transition, tangled_rope).
narrative_ontology:human_readable(sotu_1997_clinton_welfare_employment_transition, "Welfare-to-Work Employment Transition (1997 PRWORA)").
narrative_ontology:topic_domain(sotu_1997_clinton_welfare_employment_transition, "social_policy/labor_market").

domain_priors:requires_active_enforcement(sotu_1997_clinton_welfare_employment_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_welfare_employment_transition, employers_of_welfare_workers).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_welfare_employment_transition, state_administrators).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_welfare_employment_transition, tax_credit_recipients).
narrative_ontology:constraint_victim(sotu_1997_clinton_welfare_employment_transition, welfare_recipients_forced_transition).
narrative_ontology:constraint_victim(sotu_1997_clinton_welfare_employment_transition, low_wage_workers_labor_market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE RECIPIENT (SNARE) — Trapped by work requirement with no genuine choice. Loses benefits if refusing work; faces low-wage labor market with limited bargaining power. The constraint extracts compliance (work obligation) and vulnerability (willingness to accept poor conditions to retain benefits access). Zero degrees of freedom within biographical horizon.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-WAGE WORKER (SNARE) — Faces labor market saturated with welfare-to-work entrants subsidized by employers. Wage suppression and reduced bargaining power from the influx of desperate workers. No organizational capacity to resist or exit. Bears extraction through competitive displacement.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences pure coordination: tax credits, subsidized training, government-assured worker pipeline. Benefits from reduced hiring costs and assured labor supply. The constraint solves a coordination problem (matching welfare workers with employers) while extracting value (lower wages, subsidies, reduced liability for training). Beneficiary with maximum optionality.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE WELFARE ADMINISTRATION (TANGLED ROPE) — Constrained by federal matching requirements and job placement targets, but benefits from reduced welfare caseloads and cost savings. Coordinates state-level employment infrastructure (training programs, childcare subsidies, job matching). Genuine coordination function coupled with extractive pressure to reduce visible welfare dependency (theater). Can exit through federal compliance violations but at severe cost.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPPORT SERVICES (SCAFFOLD) — Training programs, childcare subsidies, transportation assistance, and job placement services are structured as temporary scaffolding. Designed to bridge transition; sunset logic embedded in the policy (services time-limited, phased out as recipients move into stable employment). Low theater because services have genuine functional purpose. Constrained by time-limit gates and funding cycles. Classified as scaffold if exit path is real (recipients become self-sufficient); classified as tangled rope if services become permanent subsidy for chronic low-wage work.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WORK ETHIC NARRATIVE (PITON) — The cultural framing of welfare-to-work as moral imperative (work is dignifying, welfare is dependency, employment solves poverty) persists despite evidence that many recipients are already working, that transition jobs often pay below subsistence, and that work requirement + low wages + time-limited services creates perpetual precarity. The narrative maintains legitimacy through performative job placement statistics while the actual mechanism extracts effort and suppresses alternatives. Theater ratio reflects the gap between policy rhetoric (empowerment through work) and structural reality (subsidized precarity).
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint exhibits genuine coordination (matching welfare workers with employers, bundling support services) coupled with asymmetric extraction (wage suppression, vulnerability creation, shift of dependency risk from the state to the individual). The beneficiary structure is institutional/multinational: employers benefit through subsidies; states benefit through caseload reduction; welfare recipients and low-wage workers bear costs. The constraint persists because it solves problems for the powerful while distributing costs to the powerless.
constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1997_clinton_welfare_employment_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1997_clinton_welfare_employment_transition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1997_clinton_welfare_employment_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1997_clinton_welfare_employment_transition, TR),
    TR >= 0.70.

:- end_tests(sotu_1997_clinton_welfare_employment_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts compliance (work obligation, time-limited services force rapid labor market entry), wages (employers capture tax credit value; workers' reduced bargaining power suppresses wages below counterfactual), and risk (shift of income volatility from guaranteed transfer to contingent employment). However, extractiveness is not maximal (snare range ≥ 0.66) because genuine coordination components exist: employers are solving a hiring problem; states are enabling infrastructure development; many recipients genuinely transition into sustained employment (though a subpopulation cycles or remains chronically precarious). The measurement trajectory (0.42 → 0.60) shows extractiveness increasing as the initial enthusiasm phase (successful placements, visible caseload reduction) gave way to long-term management of precarity. Suppression (0.65): High. Multiple suppression mechanisms: time-limited support services force rapid entry before readiness; work requirements eliminate refusal option; low-wage job saturation limits bargaining power; childcare/transportation support undersizing constrains choice; time limits on benefits reset the pressure periodically. Suppression is structural (external barriers) but also partly internalized through the cultural narrative that work is morally obligatory. Theater ratio (0.48, rising to 0.52): Moderate, increasing. Policy rhetoric emphasizes dignity, self-sufficiency, and economic empowerment through work. Actual outcome for many recipients: part-time, minimum-wage, or below-subsistence jobs with high turnover, minimal benefits, and chronic instability. The theater increases over time as policy success is measured by caseload reduction and job placement statistics (which remain high) while income stability and poverty reduction metrics show persistent problems (which remain hidden from statistical visibility).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces sharp perspectival divergence. From the employer's view (rope), the policy is pure coordination: it solves the hiring problem, provides trained workers, and creates a stable labor supply. From the welfare recipient's view (snare), it is pure extraction: work requirement with no genuine choice, time-limited services that don't match actual transition time, wages that don't sustain independence. From the state administrator's view (tangled rope), it is mixed: coordination function (infrastructure, service delivery) coupled with extractive pressure (federal targets, caseload reduction metrics). The support services perspective splits: if services are adequate and recipients achieve sustained employment, they are genuine scaffold (temporary bridge). If services are undersized theater designed to accelerate labor market entry before readiness, they become extractive (snare component). The work ethic narrative (piton) frames the whole constraint as morally necessary and individually empowering while remaining largely indifferent to structural outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary-victim asymmetry drives high effective extractiveness. Employers benefit from subsidized hiring — the tax credit reduces labor costs, government training reduces firm investment, assured worker pipeline reduces search time. The derivation yields low directionality (d ≈ 0.10–0.20) for employers: beneficiary status + arbitrage exit → low d → negative f(d) → constraint appears to benefit this agent. Welfare recipients and low-wage workers, by contrast, are victims with trapped or constrained exit. Victims + trapped exit → high d → high f(d) → constraint extracts from these agents. States occupy an intermediate position: beneficiary of reduced caseloads but constrained by federal compliance requirements and political pressure to maintain visible success. The inter-institutional perspective shows the constraint working as a coordination mechanism that systematically advantages employers and states while disadvantaging individual workers. The analytical observer's directionality is high (d ≈ 0.72) because the analyst measures impact across all agents and sees net extraction despite coordinated appearance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that tangled rope and snare classifications are not contradictory but perspectival. From any single observer position (beneficiary, victim, analyst), the classification is unambiguous. The apparent conflict arises only when trying to assign a single type to a constraint that structurally produces different outcomes for different agents. The tangled rope classification is correct from the institutional analyst's perspective: the constraint genuinely coordinates (matches workers with jobs, funds services, solves state program structure) while extracting asymmetrically (workers bear risk, employers capture value, states shift dependency costs). The snare classification is correct from the trapped welfare recipient's perspective: they experience no coordination benefit, only extraction (work requirement, wage suppression, precarity). The piton classification reveals that cultural framing (work ethic narrative) maintains the constraint's legitimacy by naturalizing outcomes as individual choices and moral imperatives, masking the structural extraction. The scaffold classification is conditional: if support services actually enable sustained above-poverty employment, the sunset logic is real and the constraint functions as described. If services are theater undersized by design, the scaffold is a false frame and the constraint is snare with temporary theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transition_trajectory_sustainability,
    'Do welfare-to-work entrants sustain employment at above-poverty wages, or do they cycle between low-wage work and welfare re-enrollment?',
    'Longitudinal income tracking; comparison of post-transition earnings to poverty line and to comparable non-welfare workers; measurement of welfare re-enrollment rates within 3, 5, and 10 years',
    'If sustained above poverty: constraint is temporary bridge (scaffold). If cyclical or chronically low-wage: constraint is permanent extraction mechanism disguised as transition (snare). If mixed outcomes vary by region/subgroup: constraint is tangled rope with differential vulnerability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_trajectory_sustainability, empirical, 'Whether welfare-to-work creates sustainable above-poverty employment').

omega_variable(
    support_service_adequacy_decay,
    'Do time-limited support services (childcare, training, transportation) provide sufficient duration for recipients to reach stable employment, or are they intentionally undersized to force rapid labor market entry?',
    'Cost-benefit analysis of support service duration vs. typical time to skill acquisition and stable job placement; comparison of actual service usage to recipient-identified need; state administrative documentation of service allocation decisions',
    'If adequate: support services function as real scaffolding (sunset justified). If undersized: services are theater (piton) or extractive accelerants (snare) pushing recipients into precarity before readiness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(support_service_adequacy_decay, empirical, 'Whether support services are adequately sized or theater').

omega_variable(
    wage_suppression_magnitude,
    'What fraction of the welfare-to-work wage depression (relative to comparable non-subsidized hiring) accrues to the employer tax credit, and what fraction is pure extraction from the worker''s bargaining power collapse?',
    'Matched-pair analysis: welfare workers'' wages vs. non-welfare workers in identical roles; employer survey of tax credit value vs. actual wage reduction; measurement of counterfactual hiring rates absent the tax credit',
    'If tax credit equals wage reduction: the extraction is approximately transparent (tangled rope). If wage reduction exceeds tax credit: pure extraction is present (snare component confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_magnitude, empirical, 'Decomposition of wage suppression into credit value and pure extraction').

omega_variable(
    institutional_benefit_concentration,
    'Does the policy disproportionately benefit large employers and franchise chains capable of navigating tax credit administration, or do small employers capture equivalent value?',
    'IRS Form 8586 data (Work Opportunity Tax Credit claims) stratified by employer size and industry; administrative interviews with tax preparers and HR departments at different scales',
    'If concentrated in large firms: the constraint creates institutional hierarchy (large-firm beneficiaries, small-firm competitors, workers) suggesting hidden power asymmetry. If distributed: the constraint is more genuinely tangled rope across institutional spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_concentration, empirical, 'Whether policy benefits concentrate in large employers').

omega_variable(
    state_caseload_reduction_mechanism,
    'Are declining welfare caseloads due to successful employment transitions, benefit time limits forcing people off rolls, or movement to disability/SSDI programs?',
    'Cohort tracking of welfare exits; comparison of caseload decline rates to employment entry rates; measurement of SSDI application rates and approval rates for welfare leavers during transition period',
    'If mostly employment: constraint functions as described (tangled rope). If mostly time limits or SSDI redirection: constraint achieves cost reduction through administrative exclusion, not successful transition (snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_caseload_reduction_mechanism, empirical, 'Mechanism of welfare caseload reduction').

omega_variable(
    employer_labor_demand_substitution,
    'Does the subsidized welfare-worker hiring substitute for ordinary hiring that would occur without the tax credit, or does it create genuinely new jobs?',
    'Matched control analysis: hiring patterns in firms with high tax credit utilization vs. firms without; measurement of overall labor force participation and unemployment in states with aggressive vs. passive welfare-to-work programs',
    'If substitution: the policy redistributes jobs from non-subsidized workers to subsidized workers without increasing employment (pure redistribution favoring employers). If new job creation: the constraint creates genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_labor_demand_substitution, empirical, 'Whether tax credits create new jobs or substitute').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1997_clinton_welfare_employment_transition, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welt_tr_t0, sotu_1997_clinton_welfare_employment_transition, theater_ratio, 0, 0.35).
narrative_ontology:measurement(welt_tr_t3, sotu_1997_clinton_welfare_employment_transition, theater_ratio, 3, 0.42).
narrative_ontology:measurement(welt_tr_t6, sotu_1997_clinton_welfare_employment_transition, theater_ratio, 6, 0.48).
narrative_ontology:measurement(welt_tr_t9, sotu_1997_clinton_welfare_employment_transition, theater_ratio, 9, 0.52).

% Extraction over time
narrative_ontology:measurement(welt_be_t0, sotu_1997_clinton_welfare_employment_transition, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(welt_be_t3, sotu_1997_clinton_welfare_employment_transition, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(welt_be_t6, sotu_1997_clinton_welfare_employment_transition, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(welt_be_t9, sotu_1997_clinton_welfare_employment_transition, base_extractiveness, 9, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1997_clinton_welfare_employment_transition, resource_allocation).
narrative_ontology:affects_constraint(sotu_1997_clinton_welfare_employment_transition, earned_income_tax_credit_behavioral_incentive).
narrative_ontology:affects_constraint(sotu_1997_clinton_welfare_employment_transition, minimum_wage_suppression_low_skill_labor_market).
narrative_ontology:affects_constraint(sotu_1997_clinton_welfare_employment_transition, childcare_affordability_double_bind).

% DUAL FORMULATION NOTE:
% Welfare-to-work is upstream of low-wage labor market wage suppression and downstream of federal safety net restructuring. The constraint family shows how policy coordination at state level (welfare-to-work) propagates to labor market effects (wage suppression) and access constraints (childcare affordability). Each story has different epsilon: welfare-to-work itself is high-extractive tangled rope; minimum wage suppression is snare (powerless workers trapped); childcare affordability is tangled rope (families forced into precarious dual-earning or single-breadwinner traps).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1997_clinton_welfare_employment_transition, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
