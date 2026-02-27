% ============================================================================
% CONSTRAINT STORY: demographic_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_inertia_trap, []).

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
 *   constraint_id: demographic_inertia_trap
 *   human_readable: The Generational Wealth Siphon
 *   domain: social/economic
 *
 * SUMMARY:
 *   The generational wealth siphon emerges when demographic imbalance grants
 *   persistent electoral control to a numerically large but temporally finite
 *   cohort. Elderly voters (65+) comprise 18-20% of the population in
 *   developed economies but vote at 60%+ turnout rates, giving them 22-25% of
 *   actual electoral power. This plurality is sufficient to determine
 *   outcomes in close elections and to block reforms threatening their
 *   benefits (pensions, subsidized healthcare, zoning restrictions that
 *   protect housing wealth). The constraint operates through democratic
 *   mechanisms: budget allocations, tax policy, entitlement structures, and
 *   monetary policy preferences all flow from electoral incentives favoring
 *   current retirees over working youth. The key asymmetry is temporal:
 *   elderly voters will not live long enough to experience the full fiscal
 *   consequences of unsustainable transfers, while youth must live with the
 *   debt-service burden and deferred opportunity costs (reduced public
 *   investment, suppressed wages, delayed homeownership). The theater
 *   component (0.45) reflects that the transfer is justified through
 *   narratives of 'earned benefits' and 'intergenerational reciprocity,' even
 *   when demographic and fiscal analysis shows the reciprocity is
 *   increasingly illusory. The constraint is not immutable: demographic
 *   decline of the elderly cohort, fiscal pressure forcing parametric reform,
 *   or youth mobilization breaking the elderly voting bloc could all sunset
 *   this arrangement. But within the next 15-25 years, absent major policy
 *   shifts, the extraction mechanism operates at high efficiency: suppression
 *   through political powerlessness and constrained exit options;
 *   extractiveness driven by the aging majority's ability to impose transfers
 *   through electoral lock-in.
 *
 * KEY AGENTS:
 *   - Retired Majority (65+): Primary beneficiary (powerful/mobile/arbitrage) — controls electoral plurality; captures pension, healthcare, and wealth-protection policies; exits constraint through death (temporal bound)
 *   - Working Youth (18-40): Primary victim (powerless/trapped/immediate) — trapped in national tax/pension systems; immobile labor markets; powerless in electoral block; bear extraction through payroll taxes and suppressed opportunity
 *   - Employed Middle Generation (40-65): Secondary actor (moderate/constrained/generational) — both extractor (toward youth) and victim-in-waiting (toward elderly); faces conflicting incentives
 *   - Pension Fund Administrators: Institutional beneficiary (institutional/arbitrage) — manage transfers as routine; experience constraint as legitimate coordination
 *   - Youth Coalition Reformers: Organized actors (organized/constrained) — advocates for parametric reform; see sunset as achievable through democratic pressure
 *   - Fiscal Commons: Victim (powerless/trapped) — abstract collective good (fiscal sustainability, public investment, intergenerational equity) that cannot exit or organize
 *   - Future Generations: Ultimate victim (powerless/trapped/civilizational) — will inherit debt burden and degraded public investment; cannot participate in current electoral decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_inertia_trap, 0.58).
domain_priors:suppression_score(demographic_inertia_trap, 0.68).
domain_priors:theater_ratio(demographic_inertia_trap, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_inertia_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(demographic_inertia_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(demographic_inertia_trap, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_inertia_trap, snare).
narrative_ontology:human_readable(demographic_inertia_trap, "The Generational Wealth Siphon").
narrative_ontology:topic_domain(demographic_inertia_trap, "social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, retired_majority).
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, pension_fund_administrators).
narrative_ontology:constraint_victim(demographic_inertia_trap, working_youth).
narrative_ontology:constraint_victim(demographic_inertia_trap, future_generations).
narrative_ontology:constraint_victim(demographic_inertia_trap, fiscal_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING YOUTH (SNARE) — Trapped by immobile labor markets, student debt, and political powerlessness. Cannot exit national tax/pension systems. Bears extraction through inflated payroll taxes, suppressed wages, and deferred homeownership. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PENSION FUND ADMINISTRATORS (ROPE) — Institutional beneficiaries managing intergenerational transfers as routine coordination. See the constraint as legitimate stewardship of earned benefits. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; experiences coordination function.
constraint_indexing:constraint_classification(demographic_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ELDERLY POLITICAL MAJORITY (SNARE) — Although individually powerful and mobile, the collective acts as a snare when wielding majority electoral control to enforce transfers. Voters aged 65+ determine national elections; their bloc solidarity on pension/healthcare issues ensures extraction. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Paradox: individually beneficiary (d≈0.0), but the institutional constraint mechanism itself (majority rule without demographic adjustment) becomes extractive from younger cohorts' perspective. Engine resolves this as powerful/mobile/beneficiary in isolation (Rope), but the snare classification reflects that the elderly voting block collectively enforces extraction.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYED MIDDLE GENERATION (TANGLED ROPE) — Ages 40-60 with some retirement savings and political influence, but still paying peak taxes. See constraint as mixed: (1) they will eventually benefit from pensions they fund, (2) they extract from youth cohort below them, (3) they are constrained by electoral dynamics they cannot escape. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44. Genuine coordination function (intergenerational support) mixed with asymmetric extraction.
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: YOUTH COALITION REFORMERS (SCAFFOLD) — Organized advocates pushing for parametric reform (means-testing, retirement age adjustment, contribution diversification). See constraint as temporary institutional artifact that can be sunset through democratic reform. χ ≤ 0.30; theater ≤ 0.70. Sunset clause emerges from: (1) demographic shift (elderly cohort naturally shrinks), (2) fiscal pressure forcing reform, (3) growing youth political mobilization. Estimated sunset: 15-25 years as demographics rebalance and electoral incentives shift.
constraint_indexing:constraint_classification(demographic_inertia_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOCIAL COMPACT NARRATIVE (PITON) — The constraint is maintained through theater: invocation of 'social contract,' 'earned benefits,' 'intergenerational reciprocity.' These narratives frame extraction as mutual obligation and legitimate reward. theater_ratio=0.45 suggests moderate performative content — some genuine coordination function remains, but much energy goes to justifying why current transfers cannot be questioned. The piton classification acknowledges that without narrative maintenance, the constraint would collapse.
constraint_indexing:constraint_classification(demographic_inertia_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (ATTEMPTED MOUNTAIN) — Risk of false summit: 'Intergenerational wealth transfer is inherent to human societies' or 'demographic majority rule is an immutable political law.' The base properties (ε=0.58, suppression=0.68) contradict mountain thresholds (ε≤0.25, suppression≤0.05). This perspective tests whether the constraint is natural law or contingent institutional design. The engine's false summit detector will flag this — the constraint is not a law of nature but a policy choice subject to reform.
constraint_indexing:constraint_classification(demographic_inertia_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demographic_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(demographic_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(demographic_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The elderly majority captures explicit transfers (pensions, healthcare subsidies) averaging 12-15% of GDP in developed economies, with additional implicit transfers through zoning protection (housing wealth extraction) and monetary policy preference (inflation tolerance to reduce real debt burden). The extraction is not absolute (0.75+) because some genuine coordination remains — current workers do expect future pension benefits, and current retirees did contribute during their working years. However, the extraction is severe because the contribution-to-benefit ratio has deteriorated with demographic shifts; today's youth will likely receive <70% of real benefits relative to contributions, while elderly receive >120%. The trajectory from ε=0.32 to 0.58 reflects accelerating extraction as the elderly cohort ages and becomes numerically dominant relative to working-age population. Suppression (0.68): High. The mechanism suppresses alternatives through: (1) electoral lock-in — elderly bloc voting prevents reform, (2) political framing — 'earned benefits' narrative delegitimizes discussion of means-testing or reduction, (3) institutional stickiness — pension structures have high path dependency, (4) fiscal constraints — reforming entitlements requires unpopular tax increases or benefit cuts on someone, (5) demographic immobility — youth cannot easily relocate to avoid payroll taxes. Suppression is not maximal (0.80+) because some alternatives exist: geographic migration, informal economy participation, political mobilization. Theater ratio (0.45): Moderate. The constraint maintains significant theatrical justification — 'social contract,' 'earned benefits,' 'insurance principle' — but these narratives have weakened as demographic realities have become transparent. The trajectory from 0.38 to 0.45 reflects increasing reliance on theater as the fiscal gap grows; if the constraint were purely functional coordination, theater would remain flat. The modest theater ratio means the constraint has real coordination content (unlike pitons), but the growth trajectory signals degradation toward piton status if reforms are not implemented.
 *
 * PERSPECTIVAL GAP:
 *   The elderly majority sees a rope: legitimate coordination where current transfers are reciprocal (they earned their pensions through decades of contribution) and young workers will eventually benefit from the same system. The pension administrators see a rope: routine stewardship of intergenerational obligation. The working youth see a snare: they are locked into a system that extracts 12-15% of their income, constrains their mobility, and promises illusory reciprocal benefits they may never receive. The employed middle generation sees tangled rope: they are both beneficiary (will receive pensions) and extractor (receive higher pensions relative to youth contribution rates) and victim (currently over-taxed relative to distant benefits). The youth coalition reformers see a scaffold: the constraint is real but temporary, subject to democratic reform and demographic sunset. The fiscal commons sees snare: an abstract collective good being extracted by the visible elderly bloc with no voice in the decision. The false summit perspective risks naturalizing this as an immutable law of aging societies — 'all democracies transfer to elderly cohorts' — when the structural data shows this is a contingent policy choice subject to reform.
 *
 * DIRECTIONALITY LOGIC:
 *   Retired majority: Powerful + arbitrage → d≈0.10, f(d)≈-0.08. But as an electoral bloc enforcing transfers, d→0.2-0.3 (beneficiary with institutional power). Pension administrators: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Working youth: Powerless + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. Employed middle generation: Moderate + constrained → d≈0.55, f(d)≈0.75. Mixed position — both within and outside extraction. Youth coalition: Organized + constrained → d≈0.40, f(d)≈0.40. Agency despite constraints. Fiscal commons: Powerless + trapped → d≈0.95, f(d)≈1.42. Maximum extraction (cannot participate in redistribution decision). Future generations: Powerless + trapped → d≈0.95, f(d)≈1.42. Maximum extraction (not yet existent to object).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by the perspectival structure itself. The constraint is NOT mislabeled as rope when it should be snare, nor vice versa. Instead, it IS BOTH, depending on structural position. (1) For the elderly majority and pension managers, it genuinely functions as coordination (rope) — the system transfers benefits to those who contributed, and most participants expect eventual reciprocity. (2) For youth, it functions as snare — they have no exit, suppressed alternatives, and extraction without credible reciprocal benefit. (3) For the fiscal commons and future generations, it is pure snare — abstract collectives bearing costs without voice. The snare classification in base_properties reflects the empirical reality that the extraction mechanism can be sustained even when reciprocity is illusory; the multiple perspectives show why different agents disagree. The mandatrophy is resolved by recognizing that what appears as 'earned benefits' from the elderly perspective is actually 'deferred extraction' from the youth perspective — the same policy structure, same transfers, but opposite classification depending on whether you are the current beneficiary or the future bearer. This is not ambiguity; it is accurate structural mapping. The constraint becomes more clearly snare (not rope) as demographics deteriorate and promised reciprocity becomes impossible to deliver — at that point, elderly voters will still have electoral power to maintain transfers, but the fairness/reciprocity justification will collapse, leaving only naked extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_transition_timeline,
    'How quickly will demographic rebalancing reduce the electoral power of the elderly cohort, and will fiscal pressure force reform before or after this natural rebalancing?',
    'Demographic projections for voting-age population by cohort; comparison of fiscal sustainability timelines with electoral transition timelines; historical cases of demographic-driven policy reform',
    'If rebalancing occurs before fiscal crisis: constraint softens naturally through shrinking beneficiary bloc. If fiscal crisis forces reform first: structured political conflict likely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_transition_timeline, empirical, 'Demographic timeline for electoral power shift').

omega_variable(
    means_testing_political_feasibility,
    'Can means-testing or progressive restructuring of pensions pass democratic majorities before the elderly cohort becomes small enough to lose electoral control?',
    'Polling on means-testing support by age cohort; historical precedent from countries that implemented parametric reform; scenario modeling of coalition-building under different demographic compositions',
    'If feasible: scaffold sunset becomes real, constraint transforms to temporary institutional arrangement. If infeasible: constraint persists until demographic natural decline forces it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(means_testing_political_feasibility, empirical, 'Feasibility of democratic reform before demographic shift').

omega_variable(
    intergenerational_reciprocity_actual_vs_promised,
    'Will the youth cohort actually receive equivalent pension benefits when they retire, given projection of fiscal unsustainability, or is the promised reciprocity illusory?',
    'Actuarial analysis of benefit payouts under current contribution rates; modeling of pension solvency if no reforms implemented; comparison of promised vs projected benefits by cohort',
    'If promised benefits are deliverable: constraint is genuine coordination (Rope from longer time horizon). If illusory: constraint is pure extraction with false reciprocity narrative (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_reciprocity_actual_vs_promised, empirical, 'Whether promised intergenerational reciprocity is fiscally viable').

omega_variable(
    youth_coalition_critical_mass,
    'At what population share and political mobilization threshold does the youth cohort gain sufficient electoral power to break the elderly majority''s blocking coalition?',
    'Voting bloc analysis by age; turnout rates by age cohort; coalition modeling; comparison with historical examples of demographic-driven political realignment',
    'Critical mass may arrive before demographic rebalancing due to differential turnout changes; determines whether scaffold sunset is reactive (forced by youth power) or passive (elderly decline).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_coalition_critical_mass, empirical, 'Youth coalition critical mass for electoral power shift').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_inertia_trap, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demog_tr_t0, demographic_inertia_trap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(demog_tr_t15, demographic_inertia_trap, theater_ratio, 15, 0.41).
narrative_ontology:measurement(demog_tr_t30, demographic_inertia_trap, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(demog_be_t0, demographic_inertia_trap, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(demog_be_t15, demographic_inertia_trap, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(demog_be_t30, demographic_inertia_trap, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_inertia_trap, resource_allocation).
narrative_ontology:affects_constraint(demographic_inertia_trap, housing_wealth_concentration).
narrative_ontology:affects_constraint(demographic_inertia_trap, fiscal_sustainability_commons).
narrative_ontology:affects_constraint(demographic_inertia_trap, youth_labor_mobility_constraint).

% DUAL FORMULATION NOTE:
% The generational wealth siphon is downstream of housing wealth concentration (elderly cohort controls zoning and property supply, enabling housing wealth extraction) and upstream of fiscal sustainability commons (pension obligations degrade public investment and accumulate sovereign debt). Youth labor mobility is constrained by both the payroll tax burden and geographic inability to escape national entitlement systems. These constraints form a cluster where the elderly asset position (housing, pension claims, electoral control) enables extraction across multiple domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(demographic_inertia_trap, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
