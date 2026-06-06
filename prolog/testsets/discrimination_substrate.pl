% ============================================================================
% CONSTRAINT STORY: discrimination_substrate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_discrimination_substrate, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: discrimination_substrate
 *   human_readable: Genomic Risk as Discrimination Substrate in UK Insurance and Employment
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The UK legal framework creates a discrimination substrate by excluding
 *   genetic characteristics from Equality Act 2010 protected status while
 *   permitting insurers and employers to use genomic risk information in
 *   underwriting and hiring decisions. The Association of British Insurers'
 *   voluntary Code on Genetic Testing provides theatrical protection: it
 *   prohibits use of predictive genetic tests for most insurance products but
 *   permits family history use (which proxies genotype), allows 'voluntary'
 *   disclosure (which insurers can incentivize), and covers only late-onset
 *   conditions. The constraint has intensified over the 2010-2022 interval as
 *   genomic medicine expanded: direct-to-consumer testing, NHS genomic
 *   medicine service rollout, and research biobank participation have all
 *   increased the volume of genetic information in circulation.
 *   Extractiveness has risen from 0.35 to 0.68 as actuarial models
 *   incorporating genomic risk have matured and as the victim population has
 *   grown (more people have disclosed genotypes). Theater ratio has risen
 *   modestly from 0.30 to 0.45 as the gap between the ABI Code's stated
 *   protections and its actual coverage has become more apparent. Suppression
 *   has risen from 0.55 to 0.72 as exit options have narrowed: individuals
 *   cannot avoid genetic testing when it is clinically indicated, cannot
 *   prevent family members from testing (which reveals shared variants), and
 *   cannot exit insurance or employment markets that use genetic information.
 *   The constraint is a snare from the victim perspective
 *   (powerless/identity_locked individuals with high genomic risk) but a rope
 *   from the beneficiary perspective (institutional/arbitrage insurers and
 *   employers). The analytical perspective classifies it as tangled_rope:
 *   genuine coordination function (precision medicine, actuarial accuracy)
 *   coexists with substantial extraction (discrimination against genetically
 *   unlucky individuals with no legal recourse).
 *
 * KEY AGENTS:
 *   - High Genomic Risk Individuals: Primary victims (powerless/identity_locked) — cannot exit their genotype or the markets that penalize it; face premium loading, coverage exclusions, employment screening with no Equality Act protection
 *   - Genetic Minority Populations: Secondary victims (powerless/trapped) — population-level risk stratification concentrates adverse selection in already-marginalized communities (Ashkenazi BRCA, African-ancestry sickle cell trait)
 *   - Insurers and Actuarial Firms: Primary beneficiaries (institutional/arbitrage) — genomic risk stratification enables finer-grained underwriting and adverse selection avoidance; experience as coordination (actuarial fairness)
 *   - Employers: Secondary beneficiaries (institutional/arbitrage) — genomic risk information enables workforce optimization and healthcare cost management; experience as coordination (information asymmetry reduction)
 *   - Clinical Genetics Services: Mixed position (moderate/constrained) — benefit from genomic medicine expansion but bear cost of patient trust erosion when genetic information becomes discrimination substrate
 *   - Genetic Privacy Advocacy Coalition: Organized agents (organized/constrained) — see legislative sunset path (Equality Act amendment) but constrained by insurance lobby; estimated 5-15 year timeline
 *   - ABI Voluntary Code Governance: Institutional actor (institutional/constrained) — maintains theatrical moratorium to forestall statutory regulation; code persists through inertia, not function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (precision medicine) coexisting with substantial extraction (legal discrimination against genetic minorities)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(discrimination_substrate, 0.68).
domain_priors:suppression_score(discrimination_substrate, 0.72).
domain_priors:theater_ratio(discrimination_substrate, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(discrimination_substrate, extractiveness, 0.68).
narrative_ontology:constraint_metric(discrimination_substrate, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(discrimination_substrate, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(discrimination_substrate, snare).
narrative_ontology:human_readable(discrimination_substrate, "Genomic Risk as Discrimination Substrate in UK Insurance and Employment").
narrative_ontology:topic_domain(discrimination_substrate, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(discrimination_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(discrimination_substrate, insurers_seeking_risk_stratification).
narrative_ontology:constraint_beneficiary(discrimination_substrate, employers_seeking_productivity_optimization).
narrative_ontology:constraint_beneficiary(discrimination_substrate, actuarial_modeling_firms).
narrative_ontology:constraint_victim(discrimination_substrate, high_genomic_risk_individuals).
narrative_ontology:constraint_victim(discrimination_substrate, genetic_minority_populations).
narrative_ontology:constraint_victim(discrimination_substrate, future_generations_with_disclosed_genotypes).
narrative_ontology:constraint_vindicates(discrimination_substrate, actuarial_fairness_doctrine).
narrative_ontology:constraint_vindicates(discrimination_substrate, genetic_exceptionalism_rejection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HIGH GENOMIC RISK INDIVIDUAL (SNARE) — Identity-locked because genotype is immutable and increasingly disclosed through healthcare interactions, family testing cascades, and research participation. Cannot exit the risk category or the insurance/employment markets that use it. Faces premium loading, coverage exclusions, employment screening, and educational tracking with no legal recourse under current UK equality law. Maximum extraction: bears full cost of genetic lottery with no protection.
constraint_indexing:constraint_classification(discrimination_substrate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: GENETIC MINORITY POPULATIONS (SNARE) — Trapped by population-level risk stratification that concentrates adverse selection in already-marginalized communities. Ashkenazi Jewish populations face BRCA founder variant loading; African-ancestry populations face sickle cell trait discrimination despite carrier status being benign. Cannot exit their ancestry or the actuarial models that encode it. Generational time horizon: discrimination compounds across family networks and future generations.
constraint_indexing:constraint_classification(discrimination_substrate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CLINICAL GENETICS SERVICES (TANGLED ROPE) — Constrained by dual mandate: provide diagnostic benefit to patients while navigating insurance disclosure requirements and employment screening requests. Benefits from genomic medicine expansion (funding, prestige, clinical utility) but also bears cost of patient trust erosion when genetic information becomes discrimination substrate. Mixed coordination-extraction: the system both enables medical benefit and creates vulnerability.
constraint_indexing:constraint_classification(discrimination_substrate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSURERS AND ACTUARIAL FIRMS (ROPE) — Primary beneficiaries. Experience the constraint as coordination: genomic risk stratification solves the legitimate actuarial problem of pricing heterogeneous risk pools. Arbitrage exit: can shift to markets or products where genetic information is less available, or lobby for expanded access. Net extraction flows toward this agent. The 'actuarial fairness' framing naturalizes extraction as technical necessity.
constraint_indexing:constraint_classification(discrimination_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EMPLOYERS (ROPE) — Secondary beneficiaries. Genomic risk information enables workforce optimization: screening out high-healthcare-cost employees, targeting training investment toward low-absenteeism-risk workers, structuring long-term incentives around predicted longevity. Arbitrage exit: can relocate to jurisdictions with weaker genetic privacy protections or use third-party screening services. Experience as coordination: solving information asymmetry in labor markets.
constraint_indexing:constraint_classification(discrimination_substrate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GENETIC PRIVACY ADVOCACY COALITION (SCAFFOLD) — Organized agents (Genetic Alliance UK, patient advocacy groups, privacy NGOs) see the discrimination substrate as a temporary coordination failure with a legislative sunset: the Equality Act 2010 can be amended to include genetic characteristics as a protected class, and the voluntary ABI Code can be replaced with statutory prohibition. Constrained by political economy (insurance lobby strength) but has agency and sees an exit path through law reform. Estimated sunset: 5-15 years for legislative change if advocacy succeeds.
constraint_indexing:constraint_classification(discrimination_substrate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ABI VOLUNTARY CODE (PITON) — The Association of British Insurers' voluntary moratorium on genetic test use is substantially theatrical. Covers only predictive tests for late-onset conditions, excludes family history (which proxies genotype), permits use of test results patients 'voluntarily' disclose, and has no enforcement mechanism beyond industry self-reporting. The code persists through institutional inertia and regulatory forbearance, not because it provides meaningful protection. Theater ratio reflects that the governance ritual is maintained to forestall statutory regulation while permitting continued extraction.
constraint_indexing:constraint_classification(discrimination_substrate, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, genomic risk stratification has genuine coordination function (enabling precision medicine, research participation, family planning) AND asymmetric extraction (concentrating insurance/employment costs on genetically unlucky individuals with no legal protection). The analytical classification is tangled_rope rather than snare because the coordination function is real and substantial, not merely cover. However, the extraction is also real and substantial: the UK legal framework permits discrimination that other jurisdictions (e.g., GINA in the US, though limited) prohibit. The constraint requires active enforcement (insurers must invest in genetic risk modeling; employers must implement screening) and has identifiable victims.
constraint_indexing:constraint_classification(discrimination_substrate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(discrimination_substrate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(discrimination_substrate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(discrimination_substrate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(discrimination_substrate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(discrimination_substrate, TR),
    TR >= 0.70.

:- end_tests(discrimination_substrate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Insurers and employers capture substantial benefit from genomic risk stratification (adverse selection avoidance, workforce optimization) while high-risk individuals bear concentrated costs (premium loading, coverage exclusions, employment screening) with no legal protection. The extraction is not total (some high-risk individuals can still obtain insurance, though at higher cost; some employers do not screen) but is substantial and rising as genomic information becomes more available and actuarial models mature. The 0.68 value reflects that the constraint extracts from a specific victim population (high genomic risk individuals, genetic minority populations) rather than from the general population, and that the extraction is legally permitted rather than prohibited. Suppression (0.72): High. Exit options are severely constrained: individuals cannot change their genotype, cannot prevent family members from testing (which reveals shared variants), cannot avoid genetic testing when clinically indicated, and cannot exit insurance or employment markets. The identity_locked exit option for high-risk individuals reflects that genotype is immutable and increasingly disclosed through healthcare interactions. Suppression has risen over the interval as genomic medicine has expanded and as more individuals have disclosed genotypes. Theater ratio (0.45): Moderate. The ABI voluntary Code provides some real protection (prohibits use of predictive tests for most insurance products) but also substantial theater: permits family history use (which proxies genotype), allows 'voluntary' disclosure (which insurers can incentivize), covers only late-onset conditions, and has no enforcement mechanism. The theater ratio reflects the gap between the Code's stated protections and its actual coverage, which has widened as genomic information has become more available and as the limitations of the Code have become more apparent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a classic beneficiary-victim perspectival gap. Insurers and employers see coordination (rope): genomic risk stratification solves legitimate actuarial and information asymmetry problems, and the 'actuarial fairness' framing naturalizes extraction as technical necessity. High-risk individuals and genetic minority populations see pure extraction (snare): they bear concentrated costs with no legal protection, cannot exit their genotype or the markets that penalize it, and the coordination story is cover for discrimination. Clinical genetics services see mixed coordination-extraction (tangled_rope): the system both enables medical benefit and creates vulnerability. The genetic privacy advocacy coalition sees a temporary problem with a legislative sunset (scaffold): the Equality Act can be amended to include genetic characteristics as a protected class. The ABI Code governance sees its own degraded ritual (piton): the voluntary moratorium persists through inertia and regulatory forbearance, not because it provides meaningful protection. The analytical observer sees tangled_rope at the civilizational level: genuine coordination function (precision medicine, actuarial accuracy) coexists with substantial extraction (legal discrimination against genetically unlucky individuals). The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?' The beneficiaries' rope is their genuine experience; the victims' snare is their genuine experience; the analytical tangled_rope is the structural reality that contains both.
 *
 * DIRECTIONALITY LOGIC:
 *   High genomic risk individuals are full victims (d → 1.0): they bear the full cost of genetic lottery with no legal protection, cannot exit their genotype or the markets that penalize it, and have no coordination benefit from the constraint. Genetic minority populations are also full victims (d → 1.0): population-level risk stratification concentrates adverse selection in already-marginalized communities, and they are trapped by ancestry-based actuarial models. Clinical genetics services are mixed (d → 0.5): they benefit from genomic medicine expansion (funding, prestige, clinical utility) but also bear cost of patient trust erosion when genetic information becomes discrimination substrate. Insurers and actuarial firms are full beneficiaries (d → 0.0): genomic risk stratification solves their adverse selection problem, they have arbitrage exit options, and extraction flows toward them. Employers are also full beneficiaries (d → 0.0): genomic risk information enables workforce optimization, they have arbitrage exit, and they capture productivity gains. The genetic privacy advocacy coalition has moderate directionality (d → 0.4): they are organized and see a legislative exit path, but are constrained by political economy and bear some cost of the current regime (advocacy resource expenditure, delayed reform). The ABI Code governance has low directionality (d → 0.2): the voluntary code provides some protection (reducing extraction) but is substantially theatrical, and the governance body is institutionally positioned to benefit from regulatory forbearance. The analytical observer has symmetric directionality (d → 0.5): sees both genuine coordination function and substantial extraction, with no structural position favoring either.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the coordination function (precision medicine, actuarial accuracy) and the extraction function (discrimination against genetic minorities) are structurally entangled, not separable. The insurers' claim that genomic risk stratification is pure coordination (actuarial fairness) is false: the constraint extracts from identifiable victims (high-risk individuals, genetic minority populations) who bear concentrated costs with no legal protection. The victims' claim that genomic risk stratification is pure extraction (discrimination disguised as science) is also incomplete: the coordination function is real (precision medicine enables targeted treatment; actuarial accuracy enables risk pooling). The analytical classification is tangled_rope: the constraint has BOTH a genuine coordination function AND asymmetric extraction, and requires active enforcement to hold (insurers must invest in genetic risk modeling; the legal framework must permit discrimination). The mandatrophy is resolved by recognizing that the question is not 'coordination or extraction?' but 'how much of each, and who bears the cost?' The UK legal framework has chosen to permit extraction (no protected characteristic status for genotype) in service of coordination (actuarial fairness, precision medicine). Other jurisdictions (e.g., GINA in the US, though limited) have chosen differently, prohibiting genetic discrimination in insurance and employment. The choice is a policy preference, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protected_characteristic_threshold,
    'What threshold of immutability and social salience qualifies a characteristic for Equality Act protection, and does genotype meet it?',
    'Comparative analysis of existing protected characteristics (race, disability, sex) against genotype on dimensions of immutability, visibility, historical discrimination, and group identification. Legal scholarship on characteristic-based vs. conduct-based discrimination. Case law development on genetic discrimination claims under existing protected characteristics (disability, race).',
    'If genotype meets threshold: legislative amendment to Equality Act is normatively required, strengthening scaffold perspective. If genotype does not meet threshold: discrimination substrate is not a legal gap but a policy choice, weakening victim claims and strengthening actuarial fairness framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_characteristic_threshold, conceptual, 'Whether genotype meets the normative threshold for protected characteristic status').

omega_variable(
    actuarial_fairness_vs_solidarity,
    'Does actuarial fairness (pricing risk accurately) take normative priority over insurance solidarity (pooling risk across healthy and sick), and on what grounds?',
    'Philosophical analysis of insurance as risk-pooling vs. risk-pricing. Historical analysis of which risks have been deemed ''uninsurable'' or subject to mandatory pooling (pre-existing conditions under ACA, pregnancy under UK maternity law). Public deliberation on acceptable vs. unacceptable bases for risk stratification.',
    'If actuarial fairness dominates: insurers'' rope perspective is normatively correct, and genetic discrimination is efficient pricing. If solidarity dominates: victims'' snare perspective is normatively correct, and genetic discrimination is extractive rent-seeking disguised as technical necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(actuarial_fairness_vs_solidarity, preference, 'Normative priority between actuarial fairness and insurance solidarity principles').

omega_variable(
    family_history_proxy_equivalence,
    'Is family history of genetic disease a sufficiently accurate proxy for genotype that permitting family-history-based underwriting undermines any genetic-test moratorium?',
    'Empirical analysis of family history predictive accuracy vs. direct genetic testing for high-penetrance conditions (BRCA, Huntington''s, familial hypercholesterolemia). Actuarial modeling of premium differentials based on family history alone vs. family history plus genetic test results. Legal analysis of whether family history constitutes ''genetic information'' under various statutory definitions.',
    'If family history is highly predictive: the ABI Code''s genetic test moratorium is theatrical (piton perspective confirmed) because insurers can achieve similar risk stratification through family history alone. If family history is weakly predictive: the moratorium provides real protection, and the piton perspective overstates the theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(family_history_proxy_equivalence, empirical, 'Whether family history proxies genotype sufficiently to undermine genetic test moratoria').

omega_variable(
    polygenic_risk_score_threshold,
    'At what predictive accuracy do polygenic risk scores (PRS) for common complex diseases (diabetes, heart disease, Alzheimer''s) become actuarially useful for insurance underwriting, and has that threshold been reached?',
    'Longitudinal tracking of PRS predictive performance (AUC, calibration) for major disease endpoints in UK Biobank and other cohorts. Actuarial modeling of premium adjustments justified by PRS vs. traditional risk factors (BMI, smoking, blood pressure). Industry adoption timelines and regulatory responses.',
    'If PRS threshold is reached: the discrimination substrate expands from rare monogenic conditions to common complex diseases, affecting a much larger victim population and increasing extraction severity. If PRS threshold is not reached: the constraint remains concentrated on rare high-penetrance variants, limiting victim population and extraction scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polygenic_risk_score_threshold, empirical, 'Whether polygenic risk scores have reached actuarial utility threshold for common diseases').

omega_variable(
    employment_screening_prevalence,
    'What is the actual prevalence of genomic information use in UK employment decisions, and is it concentrated in specific sectors or widespread?',
    'Survey of HR practices across sectors. Freedom of Information requests to public sector employers. Analysis of employment tribunal cases mentioning genetic information. Investigative journalism on genetic screening in high-stakes occupations (aviation, military, finance).',
    'If widespread: the employment extraction channel is as severe as the insurance channel, and the constraint''s total extractiveness is higher than base estimate. If concentrated or rare: employment extraction is a future risk rather than current reality, and the constraint''s extractiveness is primarily insurance-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employment_screening_prevalence, empirical, 'Actual prevalence and sectoral distribution of genomic information use in UK employment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(discrimination_substrate, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(discrim_theater_2010, discrimination_substrate, theater_ratio, 0, 0.3).
narrative_ontology:measurement(discrim_theater_2013, discrimination_substrate, theater_ratio, 3, 0.35).
narrative_ontology:measurement(discrim_theater_2016, discrimination_substrate, theater_ratio, 6, 0.38).
narrative_ontology:measurement(discrim_theater_2019, discrimination_substrate, theater_ratio, 9, 0.42).
narrative_ontology:measurement(discrim_theater_2022, discrimination_substrate, theater_ratio, 12, 0.45).

% Extraction over time
narrative_ontology:measurement(discrim_extract_2010, discrimination_substrate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(discrim_extract_2013, discrimination_substrate, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(discrim_extract_2016, discrimination_substrate, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(discrim_extract_2019, discrimination_substrate, base_extractiveness, 9, 0.6).
narrative_ontology:measurement(discrim_extract_2022, discrimination_substrate, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(discrim_suppress_2010, discrimination_substrate, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(discrim_suppress_2016, discrimination_substrate, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(discrim_suppress_2022, discrimination_substrate, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(discrimination_substrate, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural_privacy_erosion (the expanding volume of genetic information in circulation), regulatory_adequacy_gap (the absence of statutory genetic discrimination protection), and scientific_viability_uncertainty (the contested predictive validity of polygenic risk scores). The discrimination substrate is the legal and actuarial mechanism that converts genetic information into differential treatment; the upstream constraints determine how much genetic information is available, whether legal protection exists, and how accurate the risk stratification is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
