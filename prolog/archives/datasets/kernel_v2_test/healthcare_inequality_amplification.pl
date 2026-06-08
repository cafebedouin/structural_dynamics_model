% ============================================================================
% CONSTRAINT STORY: healthcare_inequality_amplification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_inequality_amplification, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: healthcare_inequality_amplification
 *   human_readable: Healthcare Inequality Amplification Through AI-Guided Genomic Health Prediction
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The healthcare inequality amplification constraint describes how
 *   AI-guided genomic health prediction (AIGHP) systems interact with
 *   existing socioeconomic and racial health disparities to create
 *   compounding extraction mechanisms. The constraint operates through three
 *   coupled channels: (1) insurance affordability — genomic risk scores
 *   enable actuarially precise pricing that segments risk pools, making
 *   coverage unaffordable for high-risk individuals who are
 *   disproportionately poor; (2) algorithmic accuracy gaps — AIGHP systems
 *   trained predominantly on European-ancestry datasets produce
 *   systematically lower accuracy for non-European populations, creating
 *   differential quality of care; (3) data pressure — individuals face
 *   coercive incentives to share genetic data to access coverage, with
 *   refusal interpreted as high-risk. The constraint is downstream of
 *   discrimination_substrate (the legal and technical infrastructure enabling
 *   genomic discrimination) and amplifies pre-existing inequalities rather
 *   than creating them de novo. The theater ratio (0.58) reflects substantial
 *   performative commitment to equity: insurance companies and AIGHP
 *   providers issue diversity statements and fund inclusive dataset
 *   initiatives while implementing risk-based pricing that systematically
 *   excludes disadvantaged groups. Public health agencies issue equity
 *   guidelines without enforcement mechanisms. The constraint exhibits rising
 *   extractiveness (0.42 → 0.68) and suppression (0.58 → 0.72) over the
 *   interval as AIGHP adoption increases and risk segmentation intensifies.
 *
 * KEY AGENTS:
 *   - Poor High-Risk Individuals: Primary victim (powerless/trapped) — face compounding extraction through unaffordable premiums, data coercion, and reduced healthcare access; cannot exit insurance system without losing care entirely
 *   - Non-European Ancestry Populations: Primary victim (powerless/identity_locked) — trapped by biological immutability of ancestry; experience systematically lower AIGHP accuracy due to training data bias; cannot exit their genetic heritage
 *   - Wealthy Low-Risk Individuals: Primary beneficiary (powerful/arbitrage) — capture lower premiums through demonstrated low genomic risk; can exit to concierge medicine or international healthcare markets; extraction flows toward them through risk segmentation
 *   - Insurance Companies: Primary beneficiary (institutional/arbitrage) — extract rents through actuarially precise risk-based pricing; can exit unprofitable markets; experience constraint as coordination (solving adverse selection)
 *   - AIGHP Platform Providers: Secondary beneficiary (institutional/arbitrage) — extract rents through platform fees and data licensing; benefit from expanding genomic testing market regardless of equity outcomes
 *   - Middle-Income Moderate-Risk: Mixed position (moderate/constrained) — experience genuine coordination benefit (early intervention) alongside extraction (premium increases, data pressure); can exit at significant cost
 *   - Public Health Agencies: Constrained institutional actor (institutional/constrained) — mandated to promote innovation and protect vulnerable populations simultaneously; budgets absorb costs of growing uninsured population; high theater ratio (equity rhetoric without enforcement power)
 *   - Patient Advocacy Coalition: Organized resistance (organized/mobile) — building legislative and litigation pathways to comprehensive genetic non-discrimination protections; see constraint as temporary with sunset mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies actuarial fairness doctrine as extraction laundering; sees coordination story as cover for wealth transfer from sick to healthy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_inequality_amplification, 0.68).
domain_priors:suppression_score(healthcare_inequality_amplification, 0.72).
domain_priors:theater_ratio(healthcare_inequality_amplification, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_inequality_amplification, extractiveness, 0.68).
narrative_ontology:constraint_metric(healthcare_inequality_amplification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(healthcare_inequality_amplification, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_inequality_amplification, snare).
narrative_ontology:human_readable(healthcare_inequality_amplification, "Healthcare Inequality Amplification Through AI-Guided Genomic Health Prediction").
narrative_ontology:topic_domain(healthcare_inequality_amplification, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(healthcare_inequality_amplification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_inequality_amplification, wealthy_low_genomic_risk_individuals).
narrative_ontology:constraint_beneficiary(healthcare_inequality_amplification, insurance_companies).
narrative_ontology:constraint_beneficiary(healthcare_inequality_amplification, aighp_platform_providers).
narrative_ontology:constraint_victim(healthcare_inequality_amplification, poor_high_genomic_risk_groups).
narrative_ontology:constraint_victim(healthcare_inequality_amplification, non_european_ancestry_populations).
narrative_ontology:constraint_victim(healthcare_inequality_amplification, uninsured_and_underinsured_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POOR HIGH-RISK INDIVIDUALS (SNARE) — Trapped by economic dependency on employer-based insurance and lack of alternative coverage options. Face compounding extraction: higher premiums due to genomic risk scores, pressure to share genetic data to access any coverage, and systematically lower AIGHP accuracy due to training data bias. Cannot exit the insurance system without losing access to healthcare entirely. Maximum experienced extraction.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-EUROPEAN ANCESTRY POPULATIONS (SNARE) — Identity-locked through ancestry itself: cannot exit their genetic heritage. AIGHP systems trained predominantly on European-ancestry datasets produce systematically lower accuracy for non-European populations, creating a structural accuracy gap that compounds existing health disparities. The identity lock is not internalized framing but biological immutability — ancestry is not a choice. Extraction operates through differential algorithmic performance that cannot be escaped.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-INCOME MODERATE-RISK (TANGLED ROPE) — Experience genuine coordination benefit: AIGHP enables early intervention and personalized prevention strategies that reduce long-term health costs. But also face extraction: premium increases based on genomic risk, pressure to share genetic data to maintain coverage, and career mobility constraints (job lock due to pre-existing condition concerns). Can exit to alternative insurance markets at significant cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY LOW-RISK INDIVIDUALS (ROPE) — Net beneficiaries. AIGHP enables them to demonstrate low genomic risk and secure lower premiums or opt out of pooled insurance entirely through concierge medicine. Can arbitrage across insurance markets and healthcare systems internationally. Experience the constraint as pure coordination: genetic information enables efficient matching to low-cost, high-quality care. Extraction flows toward them through risk segmentation.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSURANCE COMPANIES (ROPE) — Primary beneficiaries of risk segmentation. AIGHP enables actuarially precise pricing that shifts high-risk individuals out of profitable risk pools. Can exit unprofitable markets and concentrate on low-risk segments. Experience the constraint as coordination: genetic data solves adverse selection problems and enables efficient capital allocation. Theater ratio reflects performative commitment to equity while implementing risk-based pricing.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC HEALTH AGENCIES (TANGLED ROPE) — Constrained by legislative mandates to both promote precision medicine innovation and protect vulnerable populations. Experience genuine coordination function: AIGHP could enable population-level disease prevention and resource allocation. But also face extraction: agency budgets must cover growing uninsured population as private insurance becomes unaffordable for high-risk groups. Cannot exit regulatory responsibility. Mixed coordination and extraction with high theater ratio (equity rhetoric without enforcement power).
constraint_indexing:constraint_classification(healthcare_inequality_amplification, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PATIENT ADVOCACY COALITION (SCAFFOLD) — Organized groups (genetic disease advocacy organizations, health equity coalitions, civil rights groups) see the inequality amplification as a temporary coordination failure with a sunset: comprehensive genetic non-discrimination legislation, universal healthcare coverage, and algorithmic fairness standards are being built through legislative advocacy and litigation. Mobile exit options through coalition-building across disease communities. Sunset mechanism: federal legislation extending GINA protections to life and disability insurance, plus algorithmic accountability requirements.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the constraint is pure extraction masquerading as coordination. The 'actuarial fairness' justification naturalizes discrimination: treating genetic risk as an individual rather than collective responsibility fragments risk pools and makes insurance unaffordable for those who need it most. The coordination story (efficient risk pricing) is cover for extraction (wealth transfer from sick to healthy, poor to rich). Suppression operates through legal infrastructure (employment-based insurance, GINA gaps, state-level regulatory fragmentation) that prevents collective risk-sharing alternatives.
constraint_indexing:constraint_classification(healthcare_inequality_amplification, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_inequality_amplification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_inequality_amplification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_inequality_amplification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_inequality_amplification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(healthcare_inequality_amplification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts through multiple coupled mechanisms: (1) wealth transfer from high-risk to low-risk individuals via risk segmentation (those who need insurance most pay the most or are priced out entirely); (2) data extraction under coercive conditions (share genetic data or face coverage denial); (3) quality-of-care extraction through algorithmic accuracy gaps (non-European ancestry populations receive systematically lower-quality predictions). The extraction is not total (0.68 rather than 0.85+) because some coordination function exists: AIGHP does enable early intervention and personalized prevention for those who can access it. But the coordination benefit is asymmetrically distributed — wealthy low-risk individuals capture most of the benefit while poor high-risk individuals bear most of the cost. Suppression (0.72): High. Structural barriers to exit include: (1) employment-based insurance system creates job lock; (2) GINA gaps leave life, disability, and long-term care insurance unprotected; (3) state-level regulatory fragmentation prevents uniform protections; (4) medical underwriting practices treat genetic data refusal as high-risk signal, creating coercive data-sharing incentives; (5) lack of universal coverage alternative means exit from private insurance equals exit from healthcare access. Suppression is not total because some exit paths exist (Medicaid for qualifying low-income individuals, state-level protections in some jurisdictions, organized advocacy building legislative alternatives), but barriers are severe for most victims. Theater ratio (0.58): Moderate-high. Substantial performative activity: insurance companies issue diversity and inclusion statements while implementing risk-based pricing; AIGHP providers fund inclusive dataset initiatives (small-scale, underfunded) while deploying biased algorithms at scale; public health agencies issue equity guidelines without enforcement mechanisms; professional societies adopt ethical frameworks without binding standards. The theater has increased over the interval (0.38 → 0.58) as equity concerns have become reputationally salient, but functional equity mechanisms remain weak. The theater is not total (0.58 rather than 0.75+) because some genuine coordination activity exists: patient advocacy coalitions are building real legislative pathways, some insurers are experimenting with community rating, some researchers are working on algorithmic fairness.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence driven by structural position. Wealthy low-risk individuals and insurance companies experience pure coordination (Rope): genetic information enables efficient matching and risk pricing. Poor high-risk individuals and non-European ancestry populations experience pure extraction (Snare): the system prices them out of coverage, coerces data sharing, and delivers lower-quality predictions. Middle-income moderate-risk individuals experience mixed coordination and extraction (Tangled Rope): genuine early intervention benefits alongside premium increases and data pressure. Public health agencies experience the constraint as Tangled Rope with high theater: mandated to promote innovation and protect equity simultaneously, with rhetoric exceeding enforcement capacity. Patient advocacy coalitions see a temporary problem with a sunset (Scaffold): legislative and litigation pathways are being built. The analytical observer sees pure extraction masquerading as coordination: actuarial fairness doctrine naturalizes discrimination by treating genetic risk as individual rather than collective responsibility. The perspectival gap is not a disagreement about facts but a structural consequence of position: beneficiaries experience the constraint's coordination function, victims experience its extraction function, and the analytical observer sees the coordination story as cover. The gap is measurable through differential experienced extraction (chi) derived from directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Poor high-risk individuals and non-European ancestry populations are declared victims with trapped/identity_locked exit → high d → high experienced extraction. Wealthy low-risk individuals and insurance companies are declared beneficiaries with arbitrage exit → low d → low or negative experienced extraction (they benefit from the constraint). Middle-income moderate-risk individuals are in both beneficiary and victim groups (genuine coordination benefit alongside extraction) with constrained exit → moderate d → moderate experienced extraction. Public health agencies are not in beneficiary or victim groups but have constrained exit → moderate d derived from power level. Patient advocacy coalition is not in beneficiary or victim groups but has mobile exit (can build alternative pathways) → lower d. The identity_locked exit option for non-European ancestry populations reflects biological immutability: ancestry is not a choice and cannot be exited. This is distinct from internalized framing (the other common use of identity_locked) — the lock is structural (genetic heritage) rather than cognitive. The analytical observer perspective uses analytical exit, which produces d from the power atom's canonical fallback since no beneficiary/victim declaration applies to the analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the coordination vs. extraction classification is observer-dependent. From the insurance company's perspective, AIGHP solves a genuine coordination problem: adverse selection makes risk pooling unsustainable without actuarially fair pricing. From the poor high-risk individual's perspective, actuarial fairness is extraction laundering: it fragments collective risk-sharing and makes insurance unaffordable for those who need it most. Both perspectives are structurally valid. The mandatrophy is not 'which perspective is correct?' but 'which structural position are you measuring from?' The analytical observer's role is to identify that the coordination story (efficient risk pricing) serves an extraction function (wealth transfer from sick to healthy) and that the 'actuarial fairness' framing naturalizes what is actually a policy choice (individual vs. collective responsibility for genetic risk). The constraint is not purely extractive (some genuine coordination function exists) and not purely coordinative (substantial extraction operates through risk segmentation). The tangled_rope classification at the analytical level captures this: the constraint must possess BOTH coordination function AND asymmetric extraction, with active enforcement required to maintain the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actuarial_fairness_vs_solidarity,
    'Is actuarial risk-based pricing a legitimate coordination mechanism (efficient capital allocation) or an extraction mechanism (discrimination laundering)?',
    'Comparative analysis of health outcomes and financial protection in risk-pooled vs. risk-segmented insurance systems; longitudinal tracking of uninsured rates and medical bankruptcy by income and genomic risk; cross-national comparison of universal vs. market-based systems',
    'If coordination: Rope from more perspectives, lower extractiveness. If extraction: Snare from more perspectives, higher extractiveness. Determines whether the constraint is a market efficiency mechanism or a wealth transfer mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actuarial_fairness_vs_solidarity, preference, 'Whether actuarial pricing is coordination or extraction').

omega_variable(
    algorithmic_accuracy_gap_persistence,
    'Is the AIGHP accuracy gap for non-European ancestry populations a temporary data insufficiency (resolvable through inclusive dataset collection) or a structural feature of genomic prediction (ancestry-specific genetic architectures require separate models)?',
    'Longitudinal tracking of AIGHP accuracy by ancestry as training datasets diversify; analysis of whether accuracy convergence occurs or whether ancestry-specific models remain necessary; assessment of whether commercial incentives support dataset diversification',
    'If temporary: Scaffold perspective gains support, extractiveness decreases over time. If structural: Snare perspective confirmed, extraction persists indefinitely. Determines whether the constraint has a natural sunset or requires active intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_accuracy_gap_persistence, empirical, 'Whether AIGHP accuracy gap is temporary or structural').

omega_variable(
    gina_extension_feasibility,
    'Is comprehensive genetic non-discrimination legislation (extending GINA to life, disability, and long-term care insurance) politically feasible, or does insurance industry lobbying power prevent meaningful expansion?',
    'Legislative tracking of GINA expansion bills; analysis of insurance industry lobbying expenditures and campaign contributions; assessment of state-level regulatory fragmentation and preemption dynamics',
    'If feasible: Scaffold perspective confirmed, sunset mechanism is real. If blocked: Snare perspective confirmed, suppression is durable. Determines whether organized advocacy can create exit paths or whether victims remain trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gina_extension_feasibility, empirical, 'Whether GINA expansion is politically achievable').

omega_variable(
    universal_coverage_alternative,
    'Does universal healthcare coverage eliminate the inequality amplification mechanism, or do genomic risk stratifications persist through differential quality tiers, wait times, or supplemental insurance markets?',
    'Cross-national comparison of health outcome disparities by genomic risk in universal vs. market-based systems; analysis of private supplemental insurance markets in universal systems; assessment of whether genomic data influences resource allocation in single-payer contexts',
    'If eliminates: Universal coverage is a genuine exit path, scaffold perspective strengthened. If persists: Extraction mechanism is deeper than insurance market structure, snare perspective confirmed even in alternative systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_coverage_alternative, empirical, 'Whether universal coverage eliminates genomic stratification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_inequality_amplification, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hia_theater_t0, healthcare_inequality_amplification, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hia_theater_t3, healthcare_inequality_amplification, theater_ratio, 3, 0.46).
narrative_ontology:measurement(hia_theater_t6, healthcare_inequality_amplification, theater_ratio, 6, 0.52).
narrative_ontology:measurement(hia_theater_t10, healthcare_inequality_amplification, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(hia_extract_t0, healthcare_inequality_amplification, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hia_extract_t3, healthcare_inequality_amplification, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(hia_extract_t6, healthcare_inequality_amplification, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(hia_extract_t10, healthcare_inequality_amplification, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hia_suppress_t0, healthcare_inequality_amplification, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hia_suppress_t3, healthcare_inequality_amplification, suppression_requirement, 3, 0.64).
narrative_ontology:measurement(hia_suppress_t6, healthcare_inequality_amplification, suppression_requirement, 6, 0.69).
narrative_ontology:measurement(hia_suppress_t10, healthcare_inequality_amplification, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_inequality_amplification, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of discrimination_substrate, which provides the legal and technical infrastructure enabling genomic discrimination. The discrimination_substrate constraint has its own extractiveness reflecting the baseline discriminatory capacity of the infrastructure; healthcare_inequality_amplification has its own extractiveness reflecting the compounding effect when genomic discrimination interacts with existing socioeconomic and racial health disparities. The two constraints are structurally distinct: discrimination_substrate could exist without amplifying inequality (if baseline health equity were high), and inequality amplification requires but is not reducible to the discrimination substrate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
