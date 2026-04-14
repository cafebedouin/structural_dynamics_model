% ============================================================================
% CONSTRAINT STORY: sotu_1969_johnson_medicare_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1969_johnson_medicare_expansion, []).

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
 *   constraint_id: sotu_1969_johnson_medicare_expansion
 *   human_readable: Medicare Universal Healthcare Entitlement for the Elderly
 *   domain: healthcare/social_policy
 *
 * SUMMARY:
 *   Medicare, enacted in 1965 and implemented in 1966, represents a
 *   foundational shift from market-mediated to federal-entitlement-based
 *   healthcare delivery for the elderly. The constraint embeds a specific
 *   institutional mechanism: mandatory payroll taxation funding centralized
 *   risk pooling and provider reimbursement. Structurally, Medicare solves a
 *   genuine coordination problem (elderly cannot negotiate healthcare prices;
 *   individual risk pools are insufficient) while simultaneously embedding
 *   extraction through cost-shifting to working-age contributors and locking
 *   the healthcare system into fee-for-service reimbursement incentives. The
 *   constraint demonstrates the full spectrum of DR classification because
 *   different agents experience the same mechanism through radically
 *   different temporal and power positions. The elderly beneficiary
 *   (powerless, trapped) sees coordination. The working-age taxpayer
 *   (moderate, constrained) sees mixed coordination and extraction. The
 *   federal fiscal system (organized, generational horizon) sees extraction
 *   accumulation as demographic ratios shift. The analytical observer sees a
 *   structural lock that prevents alternative coordination models. The
 *   constraint's extractiveness has increased from 0.15 (when the elderly
 *   population was smaller and medical costs lower) to 0.32 (when demographic
 *   pressure and cost inflation create fiscal strain), demonstrating how the
 *   same institutional mechanism's extraction character changes over time.
 *
 * KEY AGENTS:
 *   - Elderly citizens (age 65+): Primary beneficiary (powerless/trapped) — guaranteed healthcare access regardless of income; experience constraint as liberation from market dependency
 *   - Working-age payroll contributors: Primary victim (moderate/constrained) — mandatory 2.9% payroll tax; bear cost of intergenerational transfer without guaranteed equivalent future benefit
 *   - Healthcare providers (hospitals, physicians): Secondary beneficiary (institutional/mobile) — stable reimbursement and guaranteed patient base; coordinate around Medicare rates
 *   - Federal fiscal system (Treasury, HHS, CMS): Organized institutional actor (organized/constrained) — manages extraction accumulation as demographic ratios shift and medical costs exceed wage growth
 *   - CMS administrative apparatus: Maintains piton-like institutional inertia (institutional/arbitrage) — bureaucratic overhead and ritual processing sustain the constraint through path dependence
 *   - Alternative delivery systems (ACOs, value-based providers): Potential exit pathway (powerful/mobile) — scaffold sunset logic: if value-based care matures, fee-for-service Medicare loses structural necessity
 *   - Analytical observer: Sees structural lock preventing institutional innovation (analytical/analytical) — snare classification based on path-dependent constraint on healthcare system innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1969_johnson_medicare_expansion, 0.32).
domain_priors:suppression_score(sotu_1969_johnson_medicare_expansion, 0.25).
domain_priors:theater_ratio(sotu_1969_johnson_medicare_expansion, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1969_johnson_medicare_expansion, extractiveness, 0.32).
narrative_ontology:constraint_metric(sotu_1969_johnson_medicare_expansion, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(sotu_1969_johnson_medicare_expansion, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1969_johnson_medicare_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_1969_johnson_medicare_expansion, "Medicare Universal Healthcare Entitlement for the Elderly").
narrative_ontology:topic_domain(sotu_1969_johnson_medicare_expansion, "healthcare/social_policy").

domain_priors:requires_active_enforcement(sotu_1969_johnson_medicare_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_medicare_expansion, elderly_citizens_age_65_plus).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_medicare_expansion, healthcare_providers_institutional).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_medicare_expansion, federal_administrative_apparatus).
narrative_ontology:constraint_victim(sotu_1969_johnson_medicare_expansion, working_age_payroll_contributors).
narrative_ontology:constraint_victim(sotu_1969_johnson_medicare_expansion, healthcare_cost_inflation_absorbers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ELDERLY BENEFICIARY (ROPE) — The primary beneficiary sees Medicare as pure coordination: the entitlement solves a collective action problem (individual elderly cannot negotiate healthcare prices; risk pools require scale). Exit is trapped — mandatory participation in Social Security ties to Medicare enrollment — but the agent experiences this as beneficial coordination, not extraction. The constraint guarantees access; the powerless agent with no market power experiences this as liberation from market dependency, not subjugation.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-AGE TAXPAYER (TANGLED ROPE) — Constrained exit (cannot opt out of payroll tax without leaving formal employment or emigrating). Bears extraction through mandatory contributions; benefits from future Medicare eligibility. Genuine coordination function exists (risk pooling, intergenerational solidarity) but paired with asymmetric extraction: current workers subsidize current elderly, with no guarantee of equivalent future benefits (demographic ratio shifts mean future workers pay more per retiree). Mixed experience: some reciprocal benefit, significant present-time extraction.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE PROVIDERS (ROPE) — Institutional actors (hospitals, physicians) see Medicare as a coordination mechanism that solves provider profitability fragmentation: standardized reimbursement, predictable cash flow, large patient base. Exit is mobile — providers can opt out or limit Medicare patients — but coordination benefits offset exit costs. The constraint creates a stable revenue stream that enables long-term investment. No primary victim status from provider perspective; extraction runs toward the government payer, not from providers.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL FISCAL SYSTEM (TANGLED ROPE) — Organized institutional actor (Treasury, HHS, CMS). Genuine coordination function: Medicare pools risk across the entire elderly population and spreads cost across the working population (genuine social insurance coordination). Paired with extraction accumulation: medical cost inflation structurally exceeds wage growth and payroll tax revenue growth, creating fiscal pressure. The constraint's extraction mechanism grows over time as demographic ratios shift (fewer workers per retiree) and medical technology drives cost acceleration. Constrained exit: cannot abandon Medicare without political upheaval; must continuously adjust financing mechanisms.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICARE BUREAUCRATIC SYSTEM (PITON) — The CMS administrative apparatus maintains the constraint through performative compliance and ritual documentation. Theater ratio (0.38) reflects substantial administrative overhead — claims processing, utilization review, coverage determination procedures — that performs administrative legitimacy more than it optimizes healthcare delivery. The system persists through institutional inertia and path dependence (alternatives require Congressional action), not because it is the most efficient solution. The apparatus has arbitrage exit (can be partially privatized or delegated) but maintains its scope through bureaucratic expansion and regulatory entrenchment.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HEALTHCARE SYSTEM INNOVATION PATHWAY (SCAFFOLD) — From the perspective of potential alternative systems (accountable care organizations, capitated payment models, value-based care), Medicare is a temporary coordination structure whose extraction mechanism becomes visible when compared to value-based alternatives. The constraint has a potential sunset: if integrated delivery systems and predictive analytics mature sufficiently, the need for centralized risk pooling diminishes. High-powered agents (major health systems, pharmaceutical firms, tech companies) see alternatives to traditional Medicare. The constraint persists because these alternatives are not yet mature, but the sunset logic is structural — cost-plus reimbursement is increasingly recognized as an extraction mechanism once better coordination models exist.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL LOCK (SNARE) — From an analytical position outside the system, Medicare's true extractive mechanism becomes visible: the constraint locks in a specific healthcare delivery model (fee-for-service reimbursement, provider-centric governance, centralized bureaucratic adjudication) that extracts value from patients and workers while preventing alternative coordination models from maturing. The elderly are trapped not by Medicare membership (they benefit) but by the path-dependent lock it creates on the entire healthcare system. Working-age contributors are trapped by inability to exit payroll taxes. The analytical view sees the snare as the structural lock that prevents institutional innovation, not as extractive in the redistributive sense, but extractive in the opportunity-cost sense: resources spent on administrative overhead and fee-for-service reimbursement are unavailable for direct care.
constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, snare,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1969_johnson_medicare_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1969_johnson_medicare_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1969_johnson_medicare_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_1969_johnson_medicare_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. Medicare's extraction mechanism operates primarily through demographic fiscal pressure and cost-shifting to working-age contributors, not through direct coercion or predatory terms. The constraint has genuine coordination benefits (risk pooling, universal coverage for elderly, provider stability) that offset extraction character. Measured as moderate rather than high because: (1) elderly beneficiaries receive genuine value; (2) working-age contributors accrue future Medicare eligibility; (3) providers benefit from stable reimbursement. The trajectory from 0.15 to 0.32 reflects extraction accumulation driven by demographic shift (fewer workers per retiree) and medical cost inflation exceeding payroll tax revenue growth. This is not predatory extraction but structural fiscal pressure — the constraint is extracting more over time not through policy changes but through demographic and technological trends. Suppression (0.25): Low-moderate. Barriers to exit include: mandatory payroll tax enrollment (cannot opt out without leaving formal employment), lack of viable alternatives for elderly who cannot access private insurance, regulatory barriers to alternative models. But suppression is not total — working-age taxpayers can migrate to other countries (politically costly but structurally possible), alternative healthcare models exist in other countries, providers can opt out or limit Medicare patients. Theater ratio (0.38): Moderate. Administrative overhead reflects genuine transaction costs of coordinating across 50 million beneficiaries and hundreds of thousands of providers, not pure ritual. CMS requires claims processing, utilization review, coverage determinations — these have some administrative cost necessity. But ratio includes performative elements: prior authorization procedures that delay care but prevent outlier charges, quality metrics that measure process compliance more than patient outcomes, documentation requirements that reflect administrative legitimacy more than care coordination. The trajectory upward (0.28 to 0.38) reflects increasing regulatory complexity and administrative burden per claim.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence: elderly beneficiaries see rope (pure coordination), working-age contributors see tangled rope (mixed coordination and extraction), the federal fiscal system sees snare-like extraction accumulation, while the analytical observer sees the structural lock constraining alternatives. The elderly perspective (powerless/trapped) produces rope classification because they experience the constraint solving their access problem without viable alternatives (mortality makes exit impossible). The working-age perspective (moderate/constrained) produces tangled rope because they bear current extraction through payroll taxes while accruing future benefits and having partial exit options (though costly). The federal perspective (organized/generational) sees extraction accumulation becoming the dominant experience as demographic trends make coordination increasingly costly. The analytical perspective sees snare because it recognizes that the constraint locks the entire healthcare system into a specific model (fee-for-service), preventing more efficient alternatives from maturing. This gap reveals that Medicare's classification depends entirely on temporal position: immediate experience (elderly benefit) versus generational accumulation (fiscal strain), and on power position (powerless elderly experience coordination; organized federal system experiences extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Medicare's directionality structure is fundamentally intergenerational and power-stratified. The elderly (powerless/trapped) have d ≈ 0.05 (full beneficiary status) — they receive concentrated benefits and face no reasonable exit option. The working-age contributor (moderate/constrained) has d ≈ 0.65 (modest target status) — they bear extraction through payroll taxes, but the extraction is partial (future benefits, spillover effects, moral affinity) rather than total. Healthcare providers (institutional/mobile) have d ≈ 0.25 (partial beneficiary status) — they benefit from guaranteed reimbursement and large patient base, with meaningful exit options (can limit Medicare patients without elimination). The federal fiscal system (organized/constrained) has d ≈ 0.70 (target status) — bears the cost of extraction accumulation over generational time as demographic pressure builds. The CMS apparatus (institutional/arbitrage) has d ≈ 0.30 (moderate beneficiary) — benefits from institutional entrenchment and budgetary expansion. The analytical observer (analytical/analytical) has d ≈ 0.72 (analytical target) — sees the structural lock that prevents institutional innovation. The perspectival gap is substantial because the same mechanism (federal entitlement administration with centralized risk pooling) appears as beneficial coordination to those who experience it as access guarantee (elderly) and as extractive fiscal burden to those who fund it through future-discounted contributions (working-age). The working-age taxpayer's moderate power level allows constrained exit (emigration, opt-out through employment transitions), creating a middle-ground perspective that sees both coordination and extraction simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   Medicare resolves the mandatrophy by separating coordination function from extraction mechanism. The genuine coordination problem: elderly cannot purchase healthcare in fragmented markets; cross-subsidization through intergenerational transfer is a real coordination solution. The extractive mechanism: payroll tax creates fiscal pressure on working-age contributors; fee-for-service reimbursement creates cost-inflation incentives; path dependence prevents alternative models from maturing. The constraint is neither pure rope (because extraction accumulation is real and growing) nor pure snare (because elderly genuinely benefit and providers are not trapped). Tangled rope captures the hybrid: genuine coordination function (risk pooling, universal coverage) paired with asymmetric extraction (demographic fiscal pressure, cost inflation, path-dependent lock). The mandatrophy is resolved by recognizing that the constraint's type changes over temporal horizon: at immediate horizon (elderly's life expectancy), it appears as rope (access guaranteed). At generational horizon (50-year fiscal sustainability), it appears as snare (extraction accumulation). At civilizational horizon (will alternative systems ever mature?), it appears as piton (institutional inertia sustaining a degraded model). No single type is 'correct' — the presheaf of classifications over different power/time positions is the complete answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generational_sustainability_threshold,
    'At what worker-to-beneficiary ratio does Medicare''s payroll tax financing model become structurally unsustainable without major reform?',
    'Demographic projections; comparative analysis of other social insurance systems'' sustainability models; modeling of required payroll tax increases or benefit reductions over 30-year horizon',
    'If threshold is near (3:1 or lower): Medicare is approaching mandatory restructuring, moving from stable coordination to explicit extraction mechanism. If threshold is distant (below 2:1 for extended period): constraint maintains rope-like stability longer, delaying reclassification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_sustainability_threshold, empirical, 'Demographic sustainability threshold for payroll tax financing').

omega_variable(
    medical_cost_inflation_causation,
    'Is medical cost inflation structurally inherent to healthcare delivery (unavoidable technological advances) or systemically driven by fee-for-service reimbursement incentives that Medicare embeds?',
    'Comparative international analysis of cost trajectories in different payment models; controlled studies of cost growth in capitated vs fee-for-service segments; identification of cost components attributable to unnecessary utilization vs genuine medical advance',
    'If inherent: Medicare extraction is unavoidable cost of medical progress (reduces culpability to coordinate rather than extract). If system-driven: Medicare''s fee-for-service embeddedness is extractive mechanism that could be reformed (reclassifies extraction as contingent rather than structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_cost_inflation_causation, empirical, 'Causation of medical cost inflation relative to reimbursement incentives').

omega_variable(
    alternative_coordination_viability,
    'Can integrated delivery systems, value-based care, and predictive analytics actually deliver equivalent or superior coordination at lower cost than centralized Medicare, or are these alternatives aspirational rather than structurally viable?',
    'Long-term outcomes data from accountable care organizations, managed competition experiments, and international value-based payment systems; identification of failure modes and sustainability barriers in alternative models',
    'If viable: scaffold perspective is accurate — Medicare has a real sunset path. If alternatives fail: Medicare''s structure is contingently optimal, and scaffold classification becomes over-optimistic about exit paths. Constraint would reclassify as more snare-like (fewer real alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_viability, empirical, 'Whether value-based care alternatives can supplant traditional Medicare structurally').

omega_variable(
    elderly_exit_optionality,
    'Is the elderly beneficiary''s trapped exit status (mandatory Medicare enrollment) a welfare loss (extraction) or a legitimate coordination mechanism that prevents adverse selection and ensures universal coverage?',
    'Revealed preference studies (willingness-to-pay for Medicare vs alternative private insurance if available); comparison of health outcomes and financial security across elderly populations in countries with different enrollment structures; analysis of opt-out attempts and their outcomes',
    'If welfare loss: trapped status is extraction mechanism, reclassifying elderly from rope beneficiary to snare victim. If legitimate coordination: trapped status enables universal coverage that elderly would not rationally purchase individually (prisoner''s dilemma of healthcare risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elderly_exit_optionality, preference, 'Whether mandatory Medicare enrollment for elderly is welfare loss or legitimate coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1969_johnson_medicare_expansion, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medicare_tr_t0, sotu_1969_johnson_medicare_expansion, theater_ratio, 0, 0.28).
narrative_ontology:measurement(medicare_tr_t5, sotu_1969_johnson_medicare_expansion, theater_ratio, 5, 0.33).
narrative_ontology:measurement(medicare_tr_t10, sotu_1969_johnson_medicare_expansion, theater_ratio, 10, 0.38).
narrative_ontology:measurement(medicare_tr_t15, sotu_1969_johnson_medicare_expansion, theater_ratio, 15, 0.42).

% Extraction over time
narrative_ontology:measurement(medicare_be_t0, sotu_1969_johnson_medicare_expansion, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(medicare_be_t5, sotu_1969_johnson_medicare_expansion, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(medicare_be_t10, sotu_1969_johnson_medicare_expansion, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(medicare_be_t15, sotu_1969_johnson_medicare_expansion, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1969_johnson_medicare_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_1969_johnson_medicare_expansion, payroll_tax_mandatory_extraction).
narrative_ontology:affects_constraint(sotu_1969_johnson_medicare_expansion, fee_for_service_cost_inflation).
narrative_ontology:affects_constraint(sotu_1969_johnson_medicare_expansion, provider_reimbursement_lock).
narrative_ontology:affects_constraint(sotu_1969_johnson_medicare_expansion, elderly_healthcare_access).

% DUAL FORMULATION NOTE:
% Medicare exists at the intersection of four structurally distinct constraints: (1) the elderly access problem (solved by Medicare coordination), (2) the working-age funding burden (created by payroll tax mechanism), (3) the fee-for-service cost inflation (embedded in reimbursement model), (4) the institutional lock preventing alternative delivery models. Each story has its own ε value reflecting the empirical severity of each problem. The Medicare story treats the entitlement mechanism itself; the upstream stories are the problems it solves or creates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1969_johnson_medicare_expansion, moderate, 0.65).
constraint_indexing:directionality_override(sotu_1969_johnson_medicare_expansion, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
