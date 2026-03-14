% ============================================================================
% CONSTRAINT STORY: hiring_discrimination_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiring_discrimination_automation, []).

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
 *   constraint_id: hiring_discrimination_automation
 *   human_readable: Hiring Discrimination Automation
 *   domain: labor_economics/algorithmic_governance
 *
 * SUMMARY:
 *   Hiring discrimination automation encodes historical bias into algorithmic
 *   decision-making systems while obscuring the discrimination mechanism
 *   behind technical complexity and plausible deniability. Organizations
 *   deploy screening algorithms ostensibly to reduce hiring friction and
 *   standardize candidate evaluation, but the systems systematically exclude
 *   protected classes through mechanisms including biased training data,
 *   proxy discrimination (penalizing résumé gaps, non-traditional education,
 *   non-English names), and opacity that prevents both applicants and
 *   regulators from identifying the discrimination. The constraint exhibits a
 *   multi-agent extraction structure: discriminated applicants are trapped
 *   with no appeal or recourse; hiring organizations benefit from efficiency
 *   gains and legal liability diffusion; algorithm vendors benefit from scale
 *   and reduced accountability; regulators and civil rights organizations are
 *   constrained by technical complexity and vendor opacity. The theater ratio
 *   (0.68) reflects that formal compliance mechanisms (EEOC audits, fairness
 *   certifications, diversity initiatives) persist while actual algorithmic
 *   behavior diverges from stated non-discrimination intent.
 *
 * KEY AGENTS:
 *   - Discriminated Applicants: Primary victims (powerless/trapped) — face systematic algorithmic exclusion with no knowledge of criteria, no appeal mechanism, no exit from labor market dependency
 *   - Hiring Organizations: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains, standardization benefits, and liability diffusion; can switch vendors or revert to human screening without existential cost
 *   - Algorithm Vendors: Secondary beneficiaries (powerful/arbitrage) — profit from scale, have perfect exit option (deploy new models), minimal liability exposure
 *   - Labor Market Equity (Systemic): Tertiary victim (moderate/constrained) — cumulative bias-stacking degrades fair opportunity capacity; constrained by institutional lock-in and path dependence
 *   - Civil Rights Organizations: Organized challengers (organized/constrained) — coordinate legal action and advocacy, but systematically outlawyered and under-resourced; constrained by funding and slow legal processes
 *   - Regulatory Agencies (EEOC, state labor departments): Institutional enforcers (institutional/constrained) — mandate transparency and liability, but lack technical expertise and resources to audit effectively; enforcement is slow relative to vendor deployment speed
 *   - Analytical Observer: Risk of false naturalization (analytical/analytical) — can misidentify contingent institutional arrangement as inherent property of machine learning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiring_discrimination_automation, 0.58).
domain_priors:suppression_score(hiring_discrimination_automation, 0.65).
domain_priors:theater_ratio(hiring_discrimination_automation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiring_discrimination_automation, extractiveness, 0.58).
narrative_ontology:constraint_metric(hiring_discrimination_automation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(hiring_discrimination_automation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiring_discrimination_automation, snare).
narrative_ontology:human_readable(hiring_discrimination_automation, "Hiring Discrimination Automation").
narrative_ontology:topic_domain(hiring_discrimination_automation, "labor_economics/algorithmic_governance").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiring_discrimination_automation, hiring_organizations).
narrative_ontology:constraint_beneficiary(hiring_discrimination_automation, algorithm_vendors).
narrative_ontology:constraint_victim(hiring_discrimination_automation, discriminated_job_applicants).
narrative_ontology:constraint_victim(hiring_discrimination_automation, labor_market_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRIMINATED APPLICANT (SNARE) — Faces systematic algorithmic exclusion with no recourse, appeal mechanism, or knowledge of the filtering criteria. Trapped by labor market dependency and algorithm opacity. Zero degrees of freedom: cannot see what disqualified them, cannot contest the decision, cannot exit the job market. Maximum extraction — bears full cost of biased training data and hidden proxies.
constraint_indexing:constraint_classification(hiring_discrimination_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR MARKET EQUITY (SNARE) — Systemic capacity for fair opportunity degrades as automation scales. Constrained by institutional lock-in and path dependence; cannot exit algorithmic hiring once embedded in recruiter workflows. Experiences extraction as cumulative bias-stacking: multiple algorithmic filters compound discriminatory effects. High extraction, moderate suppression — some regulatory pushback exists but insufficient to halt embedding.
constraint_indexing:constraint_classification(hiring_discrimination_automation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CIVIL RIGHTS ORGANIZATIONS (TANGLED ROPE) — Both constrained and partially equipped to challenge the constraint. Genuine coordination function exists (advocacy organizations coordinate legal action, demand transparency, establish audit standards). But also extraction: organizations are resource-limited, funding-dependent, and systematically outlawyered by hiring firms. Asymmetric enforcement: legal victories are narrow and slow, while algorithmic deployment accelerates.
constraint_indexing:constraint_classification(hiring_discrimination_automation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIRING ORGANIZATIONS (ROPE) — Experiences the constraint as coordination: algorithms reduce hiring friction, standardize screening, and provide plausible deniability ('we didn't discriminate, the algorithm did'). Benefits from efficiency gains and legal liability diffusion. Arbitrage exit available: can switch vendors, adjust algorithms, or revert to human screening without existential cost. Net beneficiary.
constraint_indexing:constraint_classification(hiring_discrimination_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALGORITHM VENDORS (ROPE) — Primary technical beneficiary. Experiences constraint as coordination mechanism: algorithms 'solve' hiring inefficiency. Arbitrage exit is perfect: can change algorithms, deploy new models, or exit market segment with minimal cost. High power, high exit option, low extraction. Vendors have the most agency in the system.
constraint_indexing:constraint_classification(hiring_discrimination_automation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY AGENCIES (TANGLED ROPE) — Genuinely trying to coordinate fair hiring norms (coordination function: establish audit standards, demand transparency, create legal liability). But constrained by technical capacity gaps, jurisdictional limits, and vendor opacity. Coordination function is real; extraction asymmetry is real — vendors can hide discriminatory features, agencies lack expertise to audit them. Active enforcement required but under-resourced.
constraint_indexing:constraint_classification(hiring_discrimination_automation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: EEOC (PITON) — Formal mandate to enforce non-discrimination, but functional capacity has degraded as hiring has automated. Theater-heavy: conducts audits, issues guidance, brings cases — but enforcement is slow, settlements are small relative to vendor profits, and firms adjust algorithms minimally after enforcement action. The EEOC's ritual persists (formal agency structure, legal authority) but the real constraint-stopping power has atrophied. Piton classification derives from high theater ratio: formal enforcement mechanism that performs legitimacy rather than producing substantial behavioral change.
constraint_indexing:constraint_classification(hiring_discrimination_automation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (APPARENT MOUNTAIN) — Risk of naturalizing as inherent: 'algorithms must encode training data; bias in training data is inevitable; therefore biased hiring is an inherent feature of algorithmic systems.' This perspective appears mountain-like (unchangeable law of machine learning). But the structural data reveals this as false naturalization: algorithmic bias is not an immutable property — it is the contingent result of vendor choices (what data to use, what proxies to exclude, what fairness definition to optimize for), organizational choices (whether to audit, whether to build fairness constraints), and regulatory choices (whether to mandate transparency, whether to impose liability). A false summit that naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(hiring_discrimination_automation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiring_discrimination_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiring_discrimination_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiring_discrimination_automation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiring_discrimination_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiring_discrimination_automation, TR),
    TR >= 0.70.

:- end_tests(hiring_discrimination_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint transfers significant economic and opportunity value from discriminated applicants to hiring organizations. Training data bias, proxy discrimination, and vendor design choices that prioritize efficiency over fairness all represent extraction mechanisms. The value is measured in lost job opportunities, wage suppression for excluded groups, and reduced labor market participation. The rising trajectory (0.25→0.42→0.58) reflects that algorithmic hiring adoption is accelerating: extractiveness was moderate when adoption was low; as more organizations deploy algorithms, the extraction scales. Suppression (0.65): High. Multiple mechanisms prevent exit or mitigation: (1) Applicants cannot see algorithmic filtering criteria or contest decisions; (2) Hiring organizations have low switching costs (arbitrage available) so no pressure to change; (3) Algorithm vendors hide discriminatory features behind technical complexity and proprietary code; (4) Regulatory agencies lack auditing capacity; (5) Racial and socioeconomic gaps in access to 'resume-optimized' credentials entrench discrimination. Theater Ratio (0.68): High. Formal compliance mechanisms abound (EEOC enforcement, fairness certifications, 'diversity hiring' initiatives, vendor fairness statements) but deliver minimal actual behavior change. Organizations can claim non-discrimination while deploying biased algorithms because the bias is algorithmically encoded and technically obscure. Hiring algorithms provide plausible deniability ('the algorithm decided, not us'). Theater ratio rising (0.35→0.52→0.68) reflects increasing professionalization of fairness theater: fairness audits, certified algorithms, diversity compliance reports accumulate without proportional reduction in discriminatory outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how the same structural mechanism produces dramatically different classifications depending on observer position. The discriminated applicant sees Snare (pure extraction, no coordination, no exit). The hiring organization sees Rope (coordination, efficiency, no felt extraction). The vendor sees Rope (coordination of hiring efficiency, massive arbitrage exit). The regulator sees Tangled Rope (coordination function exists but constrained by vendor opacity and low enforcement leverage). Labor market equity sees Snare (cumulative degradation, no exit, no coordination benefit). The EEOC sees its own function as Piton (formal authority persists, actual impact degrades, high theater). The analytical observer risks seeing Mountain (inevitability of algorithmic bias) but this is a false summit — the extractive design choices and vendor opacity are contingent, not inherent.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extraction (chi) is determined by: (1) base extractiveness (structural mechanism: historical bias in training data, proxy discrimination, opacity); (2) their directionality (d) derived from power atom + exit options + beneficiary/victim status; (3) scope modifier (national scope = σ=1.0). The engine derives d automatically from structural declarations. Discriminated applicants (powerless/trapped/victim) get high d → high f(d) → high chi experienced extraction. Hiring organizations (institutional/arbitrage/beneficiary) get low d → negative/minimal f(d) → negative chi (they experience benefit, not extraction). Algorithm vendors (powerful/arbitrage/beneficiary) get very low d → minimal f(d) → massive net benefit. The perspectival gap (applicant sees 0.95, organization sees -0.10) reveals the extraction mechanism: it transfers value from applicants to organizations through algorithmic mediation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve the mandatrophy question ('is it coordination or extraction?') — it exemplifies why the mandatrophy is index-relative. From the hiring organization's perspective, the constraint is pure coordination (Rope): it solves the hiring efficiency problem. From the discriminated applicant's perspective, it is pure extraction (Snare): they bear costs with zero benefit. From the regulator's perspective, it is mixed (Tangled Rope): coordination function (developing fairness standards) plus extraction asymmetry (enforcement is slow, vendors have exit, applicants are trapped). The constraint's true nature is that it IS both coordination and extraction simultaneously, depending on your structural position. The system coordinates hiring efficiency (genuine coordination function) while extracting opportunity from protected classes (genuine extraction mechanism). No single classification is correct; the presheaf over the observation site IS the answer. The mandatrophy is resolved by recognizing that 'is it coordination or extraction?' is the wrong question — the right question is 'from whose perspective, and why?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_discriminativeness_threshold,
    'At what degree of statistical correlation with protected class does a feature become an unacceptable proxy for discrimination?',
    'Legal precedent analysis (disparate impact doctrine), empirical correlation studies, audit results showing feature removal impact on protected-class outcomes',
    'If threshold is strict (correlation > 0.3): many seemingly-innocent features must be removed, reducing model accuracy. If threshold is loose (correlation > 0.7): proxies can hide in apparently neutral features. Different thresholds yield different suppression values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_discriminativeness_threshold, empirical, 'Statistical threshold for identifying unacceptable proxy discrimination').

omega_variable(
    vendor_knowledge_of_bias,
    'How much do algorithm vendors know ex ante about the discriminatory effects of their systems? Is bias baked in intentionally, negligently, or discovered post-deployment?',
    'Deposition discovery, vendor code review, internal communication analysis, expert witness testimony on industry standard practices',
    'If intentional: extractiveness stays high (Snare confirmed, vendors are knowing beneficiaries). If negligent: extractiveness may drop (Tangled Rope — vendors have some defense). If discovered post-deployment: vendors have minimal liability (extraction mechanism is ignorance, not willful exploitation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_knowledge_of_bias, empirical, 'Degree of vendor knowledge regarding discriminatory effects').

omega_variable(
    audit_frequency_and_access,
    'Can civil rights organizations and regulatory agencies audit deployed hiring algorithms with sufficient frequency and access to catch and report bias?',
    'Empirical audit studies (undercover testing with demographically different resumes), FOIA requests for algorithm documentation, vendor disclosure mandates and compliance rates',
    'If audit access is high: suppression drops (discriminated applicants have some recourse). If audit access is low: suppression stays high (Snare persists because discrimination is invisible). Theater ratio also varies — high-transparency systems may have higher theater (performative auditing) but lower actual suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audit_frequency_and_access, empirical, 'Feasibility and frequency of algorithm auditing').

omega_variable(
    algorithmic_switching_costs,
    'How costly is it for hiring organizations to switch algorithm vendors or revert to human screening if discovered discrimination becomes a liability threat?',
    'Cost-benefit analysis of vendor switching, retraining of recruiters, retooling of hiring workflows, regulatory compliance cost estimates',
    'If switching costs are low: hiring orgs have high arbitrage exit, constraint is Rope from their perspective. If switching costs are high: hiring orgs are constrained, constraint is Tangled Rope from their perspective. This affects directionality and effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_switching_costs, empirical, 'Cost barriers for hiring organizations to exit algorithmic hiring').

omega_variable(
    market_concentration_vendor_power,
    'Is the algorithm vendor market concentrated enough to exercise monopoly power, or competitive enough that hiring organizations can threaten to switch?',
    'Market share analysis, vendor count in hiring-algorithm segment, customer switching rates, contract lock-in terms',
    'If concentrated: vendors have high power and low exit cost, confirm Rope. If competitive: hiring organizations have exit leverage, potentially elevating constraint from Snare to Tangled Rope. Market structure directly affects directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_concentration_vendor_power, empirical, 'Market concentration in hiring algorithm vendor segment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiring_discrimination_automation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hda_tr_t0, hiring_discrimination_automation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hda_tr_t5, hiring_discrimination_automation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(hda_tr_t10, hiring_discrimination_automation, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(hda_be_t0, hiring_discrimination_automation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(hda_be_t5, hiring_discrimination_automation, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(hda_be_t10, hiring_discrimination_automation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiring_discrimination_automation, resource_allocation).
narrative_ontology:affects_constraint(hiring_discrimination_automation, algorithmic_wage_discrimination).
narrative_ontology:affects_constraint(hiring_discrimination_automation, opaque_algorithmic_decision_making).
narrative_ontology:affects_constraint(hiring_discrimination_automation, vendor_liability_diffusion).

% DUAL FORMULATION NOTE:
% Hiring discrimination automation decomposes into distinct constraints: (1) Historical bias encoding in training data (ε=0.15, Mountain-like); (2) Proxy discrimination through seemingly-neutral features (ε=0.48, Tangled Rope); (3) Opacity preventing audit and appeal (ε=0.62, Snare). This story aggregates the integrated system-level constraint (ε=0.58, Snare). Each decomposed constraint has different ε values reflecting different measurement bases (statistical bias vs structural opacity vs extractive design).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiring_discrimination_automation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
