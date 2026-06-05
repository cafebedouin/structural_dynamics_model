% ============================================================================
% CONSTRAINT STORY: sti_clinical_testing_bottleneck
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sti_clinical_testing_bottleneck, []).

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
 *   constraint_id: sti_clinical_testing_bottleneck
 *   human_readable: Requirement for Clinical Lab Testing for Chlamydia/Gonorrhea
 *   domain: medical/technological/public_health
 *
 * SUMMARY:
 *   The requirement for clinical laboratory testing of chlamydia and
 *   gonorrhea creates a structural bottleneck that exhibits properties of
 *   both coordination and extraction. Traditionally justified as a
 *   quality-control and clinical-judgment mechanism, the centralized testing
 *   requirement has become increasingly extractive as diagnostic technology
 *   has advanced (nucleic acid amplification tests are robust and simple)
 *   while regulatory barriers have remained static. The constraint operates
 *   differently depending on the agent's structural position: rural
 *   symptomatic individuals experience pure extraction (Snare); asymptomatic
 *   carriers are effectively undetected (extraction through enforced
 *   non-testing); public health authorities see a coordination function
 *   undermined by gatekeeping (Tangled Rope); clinical laboratories benefit
 *   from guaranteed specimen volume (Rope); home testing alternatives are
 *   emerging with a visible regulatory sunset (Scaffold); and medical
 *   licensing frameworks persist through institutional inertia despite
 *   technological obsolescence (Piton). The theater ratio (0.64) reflects
 *   that physician involvement in routine STI testing is substantially
 *   performative: for straightforward chlamydia/gonorrhea diagnosis, the
 *   clinical consultation adds ritual compliance but minimal diagnostic
 *   value. This constraint demonstrates how technological change (rapid
 *   diagnostics, telehealth, at-home collection) can expose coordination
 *   mechanisms that were justified in earlier eras but become extractive when
 *   conditions shift.
 *
 * KEY AGENTS:
 *   - Symptomatic individuals in resource-limited settings: Primary victims (powerless/trapped) — bear full cost of access barriers and diagnostic delays
 *   - Asymptomatic carriers: Hidden victims (powerless/trapped) — system enforces non-testing, enabling continued transmission without their awareness
 *   - Public health authorities: Organized victims (organized/constrained) — mandate to reduce transmission but constrained by clinical-only testing pathways that miss majority of infections
 *   - Clinical laboratory system: Primary beneficiary (institutional/arbitrage) — receives guaranteed specimen volume and payment flow from centralized requirement
 *   - Healthcare system gatekeepers (physicians, clinics): Secondary beneficiary (institutional/arbitrage) — maintain professional gatekeeping and capture consultation fees
 *   - Home testing alternative providers: Emerging organized actors (organized/mobile) — developing decentralized testing pathways with mobile exit options
 *   - Medical licensing framework: Institutional custodian (institutional/arbitrage) — enforces physician requirement through credentialing, benefits from professional gatekeeping
 *   - Analytical observer: Civilian/universal position (analytical/analytical) — risks naturalizing regulatory choice as inherent medical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, 0.52).
domain_priors:suppression_score(sti_clinical_testing_bottleneck, 0.68).
domain_priors:theater_ratio(sti_clinical_testing_bottleneck, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, extractiveness, 0.52).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sti_clinical_testing_bottleneck, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sti_clinical_testing_bottleneck, tangled_rope).
narrative_ontology:human_readable(sti_clinical_testing_bottleneck, "Requirement for Clinical Lab Testing for Chlamydia/Gonorrhea").
narrative_ontology:topic_domain(sti_clinical_testing_bottleneck, "medical/technological/public_health").

domain_priors:requires_active_enforcement(sti_clinical_testing_bottleneck).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, clinical_laboratories).
narrative_ontology:constraint_beneficiary(sti_clinical_testing_bottleneck, healthcare_system_gatekeepers).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, symptomatic_individuals).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, asymptomatic_carriers).
narrative_ontology:constraint_victim(sti_clinical_testing_bottleneck, public_health_transmission_prevention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL SYMPTOMATIC PATIENT (SNARE) — Trapped in clinical gatekeeping. Symptoms present but no access to testing without traveling 40+ miles to nearest clinic, incurring time off work, childcare costs, and 3-5 day confirmation wait. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73. Pure extraction: mandatory clinical pathway extracts time, money, and health risk.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASYMPTOMATIC CARRIER (SNARE) — Most chlamydia/gonorrhea infections are asymptomatic (60-80%). Carrier has zero motivation to seek testing without direct symptoms. Clinical-only pathway means carriers remain undetected, continue transmission, and bear zero cost despite being transmission vectors. System extracts from future partners and public health. d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.69. Snare through enforced non-testing.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN MOTIVATED INDIVIDUAL (TANGLED ROPE) — Has clinic access, motivation to test (STI-aware), but still constrained: must schedule appointment (1-2 week wait), pay copay ($20-150), collect sample (uncomfortable, often painful for urethral/cervical swabs), wait 3-5 days for results. Benefits from diagnostic accuracy of clinical confirmation and counseling. Costs are real but navigable. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.38. Mixed coordination (needs confirmation) and extraction (unnecessary friction).
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC HEALTH AUTHORITY (TANGLED ROPE) — Organized agent with mandate to reduce STI transmission. Benefits from clinical lab capacity (reliable results, quality control, epidemiological surveillance). Constrained by the fact that clinical-only pathways prevent detection of 70-80% of infections (asymptomatic carriers). System requires coordination (lab infrastructure) but extraction mechanism (gatekeeping) undermines public health mission. d≈0.42, f(d)≈0.42, σ=1.0 → χ≈0.22. Moderate effective extraction because this agent has agency and can push alternatives.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CLINICAL LABORATORY SYSTEM (ROPE) — Primary beneficiary. Centralized testing requirement means guaranteed specimen volume, payment flow, regulatory compliance, and professional status. Sees the constraint as coordination: 'We maintain quality through centralized expertise.' Can arbitrage by expanding testing menus, implementing rapid turnaround technologies (PCR), or staying entrenched. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary. Effective extraction is negative (subsidy) because barrier is enforced for their benefit.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HOME TESTING ALTERNATIVE COALITION (SCAFFOLD) — Organized agents (tech companies, public health startups, sexual health nonprofits) developing direct-to-consumer STI testing (mail-in kits, at-home collection with lab confirmation, telehealth-connected rapid tests). See clinical-only testing as a temporary regulatory constraint with visible sunset. d≈0.35, f(d)≈0.33, σ=1.0 → χ≈0.17. Scaffold because: (1) coordination function exists (testing still requires lab, just with reduced friction), (2) has_sunset_clause implicit in regulatory trends (FDA, CMS modernizing telehealth/home testing rules), (3) suppression declining as alternatives gain traction.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: MEDICAL LICENSING FRAMEWORK (PITON) — Requires physician/provider involvement in STI testing because 'STI diagnosis requires clinical judgment.' This rule persists through professional credentialing inertia: it was sensible before rapid diagnostics existed, but continues despite technological obsolescence. theater_ratio=0.64: substantial performative content (physician consult largely ritual for straightforward STI testing). The licensing system arbitrages through gatekeeping but sees its own rule as degraded — many physicians view home testing as inevitable. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT) — Risk of naturalizing the constraint as inherent to diagnosis: 'Any health condition requires professional evaluation.' But base properties (ε=0.52, suppression=0.68) contradict mountain gates (ε ≤ 0.25, suppression ≤ 0.05). Chlamydia/gonorrhea diagnosis is simple binomial testing (nucleic acid amplification), not complex judgment. This mountain classification is a false summit — it naturalizes a regulatory choice as scientific law.
constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sti_clinical_testing_bottleneck_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sti_clinical_testing_bottleneck, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sti_clinical_testing_bottleneck, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sti_clinical_testing_bottleneck, TR),
    TR >= 0.70.

:- end_tests(sti_clinical_testing_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The clinical testing requirement extracts time (appointment wait, clinic visit, 3-5 day result turnaround), money (copay, transportation), and delayed treatment from symptomatic individuals. For asymptomatic carriers (majority of infections), the extraction is through enforced non-testing and continued transmission. However, extractiveness is not extreme (ε < 0.70) because: (1) clinical testing does provide genuine diagnostic confirmation and counseling benefits, and (2) coordinated lab infrastructure has real quality-control value. The extraction is parasitic on coordination, not pure. Suppression (0.68): Moderate-high. Significant barriers to alternatives include regulatory prohibition of physician-independent STI testing in many states, FDA/clinical credentialing requirements, professional licensing gatekeeping, and insurance reimbursement tied to clinical encounter codes. But suppression is not total (< 0.90) because home testing is emerging and regulatory momentum is shifting toward telehealth/decentralized testing. Theater ratio (0.64): Moderate-high. Physician involvement in routine STI testing is substantially performative. For uncomplicated chlamydia/gonorrhea, the 'clinical judgment' narrative is largely ritual: most cases follow straightforward diagnostic and treatment protocols. The theater increased over time (from 0.48 to 0.64) as diagnostic technology became simpler and more reliable, yet physician gatekeeping remained constant — the gap between ritual requirement and technical necessity widened.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence across all agent types. Rural symptomatic individuals see a Snare with no exit (trapped, powerless). Public health sees a Tangled Rope where coordination (lab infrastructure) is undermined by extraction (gatekeeping). Clinical laboratories see a Rope that guarantees their specimen volume and enables quality control. Home testing coalitions see a Scaffold with visible regulatory sunset — they have agency and can see the path to alternative pathways. Medical licensing sees a Piton — the rule persists through inertia despite technological obsolescence. The analytical observer risks a Mountain, but this is a false summit. The perspectival range is: snare (rural trapped) to mountain (false summit). This diversity confirms the tangled rope diagnosis at base level: genuine coordination mixed with real extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Symptomatic rural individual: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction for least mobile agent. Asymptomatic carrier: Victim + trapped (enforced non-testing) → d≈0.88, f(d)≈1.32. High extraction through absence of testing option. Public health authority: Victim + constrained → d≈0.42, f(d)≈0.42. Organized agent with constrained mobility; they can push alternatives but are bound by current regulations. Urban motivated individual: Victim + constrained → d≈0.58, f(d)≈0.72. Mixed — has clinic access but faces friction. Clinical laboratory: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net subsidy through regulatory protection. Medical licensing: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net subsidy through professional gatekeeping. Home testing coalition: Organized + mobile → d≈0.35, f(d)≈0.33. Low effective extraction because this actor has real alternatives emerging.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint avoids mandatrophy (mislabeling pure extraction as coordination) by explicitly declaring both beneficiaries (clinical labs, physicians, licensing system) and victims (patients, public health). The clinical-only requirement does provide a genuine coordination function (lab quality control, diagnostic accuracy, epidemiological surveillance) — but this coordination function is not the primary driver of the bottleneck. The bottleneck's primary function is extraction: guaranteeing specimen volume for clinical labs and maintaining professional gatekeeping for physicians. The constraint is Tangled Rope (not pure Snare) because it genuinely coordinates lab infrastructure AND extracts from patients. The measurement data (theater_ratio increasing from 0.48 to 0.64) supports this diagnosis: as diagnostic technology simplified, the 'clinical judgment' narrative became more performative, yet the bottleneck persisted and extraction increased. This is the canonical Tangled Rope pattern: coordination + inertia + extraction bundled together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clinical_judgment_necessity,
    'How much of the clinical testing requirement reflects genuine diagnostic necessity versus regulatory gatekeeping?',
    'Comparative analysis of STI diagnostic protocols across regulatory regimes (US vs UK vs EU); evaluation of diagnostic accuracy for home-collected vs clinician-collected samples; retrospective analysis of physician decision-making on STI cases (how often does ''clinical judgment'' override lab result?)',
    'If necessity is high (>70%): clinical testing may be Rope or even Mountain. If gatekeeping dominates (<30% necessity): constraint is clearly Snare/Tangled Rope. At 30-70% mixed: Tangled Rope is correct classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clinical_judgment_necessity, empirical, 'Whether clinical judgment necessity justifies the testing bottleneck').

omega_variable(
    transmission_prevention_efficacy,
    'Does faster access to STI testing (via home kits) actually reduce transmission rates compared to clinical-only testing?',
    'Cohort comparison: regions/populations with home testing access vs clinical-only controls, measured by incident infection rates, partner notification time, partner treatment uptake. Analysis of asymptomatic carrier detection rates.',
    'If home testing reduces transmission by >20%: public health victim is real and extraction is severe (Snare confirmed). If reduction is <5%: extraction argument weakens; may be more Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_prevention_efficacy, empirical, 'Impact of rapid testing access on STI transmission reduction').

omega_variable(
    regulatory_sunset_credibility,
    'Are regulatory changes (FDA approval of at-home testing, telehealth billing reform, physician-independent testing) actually materializing or is the scaffold hypothesis aspirational?',
    'Tracking of FDA authorizations for home STI tests; CMS reimbursement policy changes; state-by-state telehealth testing rules over next 5 years; market share of direct-to-consumer STI testing vs clinical testing.',
    'If regulations change in 2-5 years: scaffold is real. If gatekeeping persists and alternatives remain niche (>10 years): constraint may degrade to Piton rather than Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_credibility, empirical, 'Whether regulatory sunset for home/telehealth testing is materializing').

omega_variable(
    quality_control_maintenance,
    'Can diagnostic accuracy and quality control be maintained if STI testing is decentralized (home collection, distributed labs, telehealth-guided)?',
    'Comparative analysis of false positive/negative rates in decentralized vs centralized testing; evaluation of chain-of-custody data for home-collected samples; accuracy data from existing home STI testing services.',
    'If accuracy is maintained: clinical laboratory monopoly is purely extractive (Snare). If accuracy degrades >5%: there is a genuine quality coordination function (Rope/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(quality_control_maintenance, empirical, 'Whether decentralized testing can maintain diagnostic accuracy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sti_clinical_testing_bottleneck, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sti_tr_t0, sti_clinical_testing_bottleneck, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sti_tr_t10, sti_clinical_testing_bottleneck, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sti_tr_t20, sti_clinical_testing_bottleneck, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(sti_be_t0, sti_clinical_testing_bottleneck, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sti_be_t10, sti_clinical_testing_bottleneck, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(sti_be_t20, sti_clinical_testing_bottleneck, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sti_clinical_testing_bottleneck, enforcement_mechanism).
narrative_ontology:affects_constraint(sti_clinical_testing_bottleneck, std_transmission_prevention_infrastructure).
narrative_ontology:affects_constraint(sti_clinical_testing_bottleneck, sexual_health_equity_access).

% DUAL FORMULATION NOTE:
% The clinical testing bottleneck is downstream of regulatory frameworks (medical licensing, FDA credentialing) and upstream of transmission prevention outcomes (STI prevalence, partner notification speed). This story focuses on the bottleneck itself (ε=0.52); upstream regulatory stories (ε likely higher, more purely extractive) and downstream transmission prevention stories (ε variable by intervention type) represent separate constraint family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sti_clinical_testing_bottleneck, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
