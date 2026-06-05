% ============================================================================
% CONSTRAINT STORY: sotu_1994_clinton_reinvention_government
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1994_clinton_reinvention_government, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1994_clinton_reinvention_government
 *   human_readable: Federal Workforce Reduction and Reinvention of Government (1994-1999)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The National Performance Review (NPR), led by Vice President Al Gore from
 *   1993 onward, targeted the federal government as an institutional
 *   constraint that extracted value through administrative overhead and
 *   inefficiency. The reform mechanism was straightforward: reduce the
 *   federal workforce by 252,000 (approximately 12% of total employment) over
 *   five years, returning federal employment to 1960s levels while
 *   maintaining (or improving) service delivery. This positioned federal
 *   employees and dependent communities as the targets of extraction, while
 *   deficit-reduction advocates and remaining agencies experienced the
 *   constraint as a coordination mechanism solving the collective action
 *   problem of fiscal discipline. The reform demonstrates how the same
 *   institutional arrangement — mandatory workforce reduction — appears as
 *   either an immutable law (civilizational fiscal constraint), a
 *   coordination mechanism (deficit control), a hybrid with real costs
 *   (tangled rope), pure extraction (snare from employee perspective), or
 *   degraded theater (piton as efficiency narratives detach from actual
 *   outcomes). The constraint's extractiveness (0.58) reflects
 *   moderate-to-high extraction with significant suppression (0.68) through
 *   immobilized career capital and sectoral/geographic retraining barriers.
 *
 * KEY AGENTS:
 *   - Federal Employees (powerless/trapped): Primary victims bearing direct career and income loss; sunk pension vesting and skill specificity create high suppression.
 *   - Contractor Workers and Regional Economies (moderate/constrained): Secondary victims experiencing reduced federal spending and procurement cuts; exit is costly but possible.
 *   - Deficit-Reduction Coalition (institutional/arbitrage): Primary beneficiary; includes OMB, budget hawks in Congress, fiscally conservative constituency; experiences constraint as coordination mechanism for deficit control.
 *   - Remaining Federal Agencies (institutional/constrained): Mixed beneficiary/victim; gain efficiency discipline, lose operational capacity; cannot easily exit constraint.
 *   - Taxpayers/Benefit Recipients (powerful/mobile): Perceived beneficiary of lower future tax burden but bears extraction cost of reduced service quality; can exit through political voice or relocation.
 *   - Administrative Efficiency Narrative (institutional/arbitrage): Institutional actor maintaining reform rationale; theater increases as discourse about 'doing more with less' becomes decoupled from measurable outcomes.
 *   - Analytical Observer (analytical/analytical): Civilizational perspective at risk of naturalizing a contingent political choice as fiscal inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1994_clinton_reinvention_government, 0.58).
domain_priors:suppression_score(sotu_1994_clinton_reinvention_government, 0.68).
domain_priors:theater_ratio(sotu_1994_clinton_reinvention_government, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1994_clinton_reinvention_government, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1994_clinton_reinvention_government, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1994_clinton_reinvention_government, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1994_clinton_reinvention_government, tangled_rope).
narrative_ontology:human_readable(sotu_1994_clinton_reinvention_government, "Federal Workforce Reduction and Reinvention of Government (1994-1999)").
narrative_ontology:topic_domain(sotu_1994_clinton_reinvention_government, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1994_clinton_reinvention_government).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1994_clinton_reinvention_government, deficit_reduction_coalition).
narrative_ontology:constraint_beneficiary(sotu_1994_clinton_reinvention_government, remaining_federal_agencies).
narrative_ontology:constraint_beneficiary(sotu_1994_clinton_reinvention_government, taxpayer_base).
narrative_ontology:constraint_victim(sotu_1994_clinton_reinvention_government, federal_employees).
narrative_ontology:constraint_victim(sotu_1994_clinton_reinvention_government, contractor_workers).
narrative_ontology:constraint_victim(sotu_1994_clinton_reinvention_government, affected_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEE (SNARE) — Faces involuntary reduction-in-force (RIF) with limited exit options. Career capital (federal pension vesting, health benefits, security clearance) is sunk in the constraint. Geographical and sector-specific retraining barriers create high suppression. No voice in reform design; extraction occurs through job loss and forced career transition. Maximum experienced extraction.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTRACTOR WORKERS / LOCAL ECONOMIES (SNARE) — Indirect extraction through reduced federal spending in dependent regions. Contractor layoffs follow procurement cuts. Small communities relying on federal facilities (military bases, regional offices) experience economic contraction. High suppression — economic transition costs are real but diffuse; exit is costly (relocation, retraining). Extraction is less severe than federal employees (can seek private-sector work) but significant structural constraint.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEFICIT REDUCTION COALITION (ROPE) — Primary beneficiary. Experiencing the constraint as coordination mechanism: downsizing makes deficit reduction concrete and measurable (252,000 jobs = quantifiable fiscal impact). No extraction experienced by this agent; the constraint solves their collective action problem (demonstrating fiscal discipline). Low suppression, high coordination value. Arbitrage exit option — can redirect savings to other policy goals if political conditions shift.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REMAINING FEDERAL AGENCIES (TANGLED ROPE) — Mixed experience. Genuine coordination benefit: lean staffing enforces prioritization and operational efficiency. Extraction cost: loss of institutional capacity, morale damage, reduced ability to execute mandates. Constrained exit — cannot easily expand workforce without reversing the policy reform. The constraint simultaneously enables efficiency (coordination) and degrades service delivery (extraction). Moderate effective extraction because the beneficiary status (reduced overhead) is paired with real operational cost.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TAXPAYERS / PUBLIC BENEFIT RECIPIENTS (TANGLED ROPE) — Perceived benefit (lower future deficits, lower tax burden) paired with extraction cost (reduced government service quality, longer application processing, reduced regulatory effectiveness). Mobile exit option — can shift voting, tax avoidance, or geographic location in response. The constraint has real coordination function (deficit sustainability) but real extraction cost (service degradation). Experienced extraction is moderate-to-high because powerful agents can exit if dissatisfied; the constraint is maintained by belief in deficit necessity, not coercion.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ADMINISTRATIVE EFFICIENCY NARRATIVE (PITON) — The reform's justification (streamlined government, elimination of waste) has become partially detached from its structural function. Reinvention theater persists after policy maturation: performance metrics emphasize headcount reduction rather than outcome improvement; the reform becomes an end in itself. Theater ratio (0.55) reflects that administrative efficiency gains were real but later discourse about 'doing more with less' became ritualistic. The constraint is maintained partly through inertia (the narrative persists) rather than current functional need.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FISCAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational view, deficit reduction is presented as an immutable constraint: government budgets cannot spend indefinitely beyond revenue; fiscal imbalance creates civilizational risk. This perspective naturalizes the deficit as a force of nature. However, the structural data contradicts this: identifiable beneficiaries (deficit reduction coalition) and clear victims (federal employees) indicate this is a contingent policy choice, not a natural law. The engine detects this as a false summit — naturalizing a political choice as fiscal inevitability.
constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1994_clinton_reinvention_government_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1994_clinton_reinvention_government, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1994_clinton_reinvention_government, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1994_clinton_reinvention_government, TR),
    TR >= 0.70.

:- end_tests(sotu_1994_clinton_reinvention_government_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The constraint does extract measurable resources — 252,000 jobs lost, reduced federal payroll, lower benefits obligations. But extractiveness is not maximal because: (a) some reduction reflects genuine inefficiency elimination (legitimate downsizing), (b) some of the 'extraction' flows to other federal agencies in the form of operational authority consolidation rather than pure benefit capture, (c) deficit reduction itself has a public-goods component (fiscal sustainability), which creates genuine coordination value alongside extraction. The measurement trajectory (0.42 → 0.58 over 5 years) reflects that initial extractiveness was moderate (reform framed as 'efficiency') but increased as actual job losses accumulated and service degradation became visible. Suppression (0.68): Moderate-to-high. Federal employees face real barriers to exit: pension vesting structures create strong incentive to stay (defined-benefit pensions are backloaded), federal job skills have limited private-sector transferability, geographic retraining barriers exist in communities dependent on federal facilities. However, suppression is not maximal (0.90+) because exit is possible at a cost, not completely blocked. Theater ratio (0.55): Moderate. The reform's justification involved genuine efficiency rhetoric ('reinventing government,' eliminating waste), which was partly real (some administrative redundancies existed) but increasingly theatrical over time. By year 5, performance discourse emphasized headcount reduction as an end in itself rather than a means to measurable service improvement, indicating rising theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the full perspectival divergence of DR classification. Federal employees see pure extraction (Snare) because their only exit is involuntary career transition. The deficit-reduction coalition sees pure coordination (Rope) because downsizing directly solves their collective action problem (making deficit reduction visible and measurable). Remaining agencies see mixed extraction and coordination (Tangled Rope) because they are simultaneously empowered (lean operations, forced prioritization) and constrained (reduced capacity, morale damage). Taxpayers see a more ambiguous tangled rope because they perceive future benefit (deficit sustainability) paired with current extraction cost (reduced government service quality). The administrative efficiency narrative increasingly appears as piton (performative maintenance of reform discourse) as actual outcomes diverge from initial justifications. The civilizational analytical observer risks treating fiscal necessity as natural law when structural analysis reveals a contingent policy choice with identifiable beneficiaries and victims. The perspectival gap reveals that classification varies inversely with beneficiary status: those who benefit (deficit hawks, remaining agency leadership) see rope or coordination; those who bear costs (federal employees, dependent communities) see snare or tangled rope; observers risk naturalizing beneficiary preferences as law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from each agent's structural position: beneficiary/victim status, power level, and exit options determine their experienced extraction (d value, fed into sigmoid f(d) to produce chi). Federal employees occupy trapped positions with high d (~0.92): they are identifiable victims with severely constrained exit. This produces high f(d) (~1.38), amplifying experienced extractiveness. The deficit-reduction coalition occupies beneficiary positions with arbitrage exit options (~d=0.08): they are not targets of extraction and can redirect savings elsewhere. This produces low f(d) (~-0.10), yielding negative effective extraction (they experience coordination, not cost). Remaining federal agencies occupy constrained institutional positions (~d=0.55): they benefit from overhead reduction but lose operational capacity. This produces moderate f(d) (~0.75), yielding the tangled-rope experience: real coordination benefit paired with real cost. Taxpayers with mobile exit (~d=0.70) experience moderate-to-high f(d) (~1.05), reflecting that perceived benefit (deficit reduction) is uncertain and paired with certain extraction cost (service degradation). The piton perspective derives from theater ratio (0.55) exceeding the Boltzmann floor for coordination mechanisms (~0.15-0.20), indicating that efficiency narratives have become partially decoupled from actual functional outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint would normally present a mandatrophy puzzle: does it classify as rope (deficit reduction as coordination solution), snare (workforce extraction), tangled rope (both), or something else? The resolution emerges from perspectival differentiation. The deficit-reduction coalition genuinely solves a coordination problem (making fiscal discipline concrete). Federal employees are genuinely targeted for extraction. These are not contradictory — they describe different agents' experiences of the same constraint. Mandatrophy is resolved by recognizing that no single type captures all relationships; tangled_rope for the constraint overall means 'this has both coordination and extraction functions, and perspectival analysis reveals which agents experience which.' The false-summit diagnostic (mountain view of fiscal necessity) reveals where the mandatrophy can be resolved: treating deficit reduction as an immutable law naturalizes what is actually a contingent policy benefiting specific coalitions. Once depoliticized (naturalized as mountain), the constraint becomes harder to reform — the mountain framing suppresses recognition of alternative fiscal arrangements (higher taxes, reduced military spending, different benefit structures) that would avoid federal workforce extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deficit_necessity_vs_political_choice,
    'Is deficit reduction a civilizational fiscal imperative or a politically contingent policy choice?',
    'Comparative fiscal history: identify periods of sustainable higher deficit-to-GDP ratios and periods of rapid deficit reduction without productivity gains; analyze whether deficit reduction correlates with improved long-term economic indicators or merely shifts resource burden temporally',
    'If imperative: mountain classification is justified (natural law of accounting). If contingent: false summit confirmed — the constraint naturalizes a policy choice benefiting deficit-reduction coalition at expense of federal employees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_necessity_vs_political_choice, conceptual, 'Whether deficit reduction is fiscal necessity or political choice').

omega_variable(
    service_quality_degradation_causation,
    'Is observed government service degradation (longer processing times, reduced enforcement, deteriorated infrastructure maintenance) causally attributable to workforce reduction or to other factors (aging infrastructure, shifting demand, funding cuts to specific programs)?',
    'Time-series analysis of service metrics (permit processing time, compliance investigation rates, facility maintenance backlog) controlling for program-specific funding, population-adjusted demand, and prior workforce trends; causal decomposition via synthetic controls or instrumental variable estimation',
    'If reduction is primary cause: tangled-rope extraction is real and quantifiable (service degradation is direct cost). If other factors dominate: extraction narrative is overstated, and the constraint may classify as rope (pure coordination) from taxpayer perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(service_quality_degradation_causation, empirical, 'Whether service degradation is caused by workforce reduction').

omega_variable(
    contractor_substitution_displacement,
    'Did downsizing federal employment shift work to private contractors, resulting in cost transfer rather than genuine cost reduction?',
    'Federal spending data: compare federal personnel costs vs total compensation (including contractor costs) pre- and post-reform; track specific task transfers from federal staff to contractors; quantify indirect cost inflation (contract markup, overhead allocation)',
    'If substantial contractor substitution: deficit reduction is partially theatrical (theater_ratio rises); net taxpayer savings are lower than headcount reduction suggests; extraction is shifted from federal employees to contractor-dependent communities rather than eliminated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_substitution_displacement, empirical, 'Whether downsizing shifted work to private contractors').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.68) structural (federal employees genuinely trapped by pension vesting, skill specificity, geographic immobility) or partly internalized (employees have internalized the narrative that government downsizing is necessary and inevitable)?',
    'Survey evidence on federal employee beliefs about career alternatives; analysis of career transition success rates post-RIF (employment within 12-24 months, wage replacement ratios); comparison of RIF-eligible employees who voluntarily separated vs those forced out',
    'If structural: suppression reflects real barriers to exit, and snare classification is accurate. If partly internalized: suppression may be higher than structural data suggests (employee identity fusion with federal status compounds material barriers), and identity_locked exit option becomes relevant for subset of workforce.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural or partly internalized').

omega_variable(
    coalition_stability_and_sunset_potential,
    'Is the deficit-reduction coalition stable enough to sustain this constraint indefinitely, or does the constraint contain inherent pressures toward reform/reversal (service quality degradation creating countervailing coalition, generational change in political priorities, fiscal surplus enabling workforce rebuilding)?',
    'Political economy analysis: track congressional voting patterns on federal workforce expansion/contraction proposals; measure public opinion shifts on government downsizing; analyze whether service degradation triggers demand for agency reinvestment; model coalition composition over 20+ year horizon',
    'If stable: constraint persists as tangled rope. If pressures toward reform: sunset potential exists (reclassify as scaffold with implicit sunset), or the constraint may degrade into piton as original rationale weakens but institutional arrangements persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_and_sunset_potential, empirical, 'Whether deficit-reduction coalition can sustain this constraint long-term').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1994_clinton_reinvention_government, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reinv_tr_t0, sotu_1994_clinton_reinvention_government, theater_ratio, 0, 0.38).
narrative_ontology:measurement(reinv_tr_t2, sotu_1994_clinton_reinvention_government, theater_ratio, 2, 0.48).
narrative_ontology:measurement(reinv_tr_t5, sotu_1994_clinton_reinvention_government, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(reinv_be_t0, sotu_1994_clinton_reinvention_government, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(reinv_be_t2, sotu_1994_clinton_reinvention_government, base_extractiveness, 2, 0.54).
narrative_ontology:measurement(reinv_be_t5, sotu_1994_clinton_reinvention_government, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1994_clinton_reinvention_government, resource_allocation).
narrative_ontology:boltzmann_floor_override(sotu_1994_clinton_reinvention_government, 0.12).
narrative_ontology:affects_constraint(sotu_1994_clinton_reinvention_government, federal_pension_vesting_lock).
narrative_ontology:affects_constraint(sotu_1994_clinton_reinvention_government, regional_dependency_federal_spending).
narrative_ontology:affects_constraint(sotu_1994_clinton_reinvention_government, contractor_procurement_substitution).

% DUAL FORMULATION NOTE:
% This constraint operates as a forcing function on downstream constraints. Federal pension vesting creates identity_locked dynamics for employees (exiting government requires abandoning pension capital). Regional economic dependency creates institutional-level tangled rope for communities. Contractor substitution creates a secondary extraction mechanism where federal workforce reduction is replaced by private contractor markup overhead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
