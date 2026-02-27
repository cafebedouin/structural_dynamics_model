% ============================================================================
% CONSTRAINT STORY: requirement_invasive_diabetes_testing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_requirement_invasive_diabetes_testing, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: requirement_invasive_diabetes_testing
 *   human_readable: The Requirement for Invasive Blood Testing to Diagnose and Monitor Diabetes
 *   domain: healthcare/medical_technology
 *
 * SUMMARY:
 *   The requirement for invasive blood testing to diagnose and monitor
 *   diabetes has dominated clinical practice for four decades, creating a
 *   structural tension between medical legitimacy (blood glucose is the gold
 *   standard biomarker) and patient burden (repeated needle sticks, pain,
 *   infection risk, psychological burden). The constraint exhibits tangled
 *   rope structure: it solves a real coordination problem (standardized,
 *   high-confidence measurement protocols) while simultaneously extracting
 *   from patients who bear the full burden of invasiveness. The rise of
 *   continuous glucose monitoring (CGM) and emerging non-invasive modalities
 *   (spectroscopic, saliva-based, subcutaneous interstitial fluid sensors) is
 *   creating alternative diagnostic pathways with explicit sunset logic — as
 *   these mature and gain reimbursement, the invasive-test-only requirement
 *   will transition from mandatory standard of care to optional reference
 *   modality. The theater_ratio has increased from 0.42 to 0.58 over the
 *   40-year interval as regulatory justifications have become increasingly
 *   performative: modern regulators maintain invasive-test-only standards
 *   partly through conservative liability logic ('proven modality') and
 *   partly because the evidence base for alternatives is still accumulating,
 *   rather than because blood access is physically necessary. The constraint
 *   reveals how technological lock-in operates through institutional inertia
 *   — once a diagnostic modality becomes embedded in standards, guidelines,
 *   insurance reimbursement, and clinical training, the institutional costs
 *   of transition become high even when superior alternatives emerge.
 *
 * KEY AGENTS:
 *   - Diabetic patients: Primary victim (powerless/trapped) — diagnosis and effective self-management require repeated invasive testing; bear full cost of pain, inconvenience, infection risk, and psychological burden
 *   - Diagnostic device manufacturers: Primary beneficiary (institutional/arbitrage) — $10B+ annual global market revenue dependent on continued invasive testing requirement; see constraint as coordination mechanism
 *   - Clinical laboratory network: Secondary beneficiary (institutional/arbitrage) — phlebotomy infrastructure, clinical laboratory testing, accreditation systems create recurring revenue stream and professional employment
 *   - Primary care physicians: Secondary victim (moderate/constrained) — constrained by medical standards of care, liability requirements, evidence-based guidelines that mandate invasive testing; also benefit from coordination function (validated diagnostic data)
 *   - Continuous glucose monitoring advocates: Organized coalition (organized/constrained) — patient advocates, progressive healthcare systems, some device manufacturers building alternative pathways with sunset logic
 *   - Regulatory agencies (FDA, EMA, CLIA): Institutional actors (institutional/arbitrage) — maintain conservative validation standards; piton perspective reveals performative justification as alternatives mature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(requirement_invasive_diabetes_testing, 0.38).
domain_priors:suppression_score(requirement_invasive_diabetes_testing, 0.62).
domain_priors:theater_ratio(requirement_invasive_diabetes_testing, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, extractiveness, 0.38).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(requirement_invasive_diabetes_testing, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(requirement_invasive_diabetes_testing, tangled_rope).
narrative_ontology:human_readable(requirement_invasive_diabetes_testing, "The Requirement for Invasive Blood Testing to Diagnose and Monitor Diabetes").
narrative_ontology:topic_domain(requirement_invasive_diabetes_testing, "healthcare/medical_technology").

domain_priors:requires_active_enforcement(requirement_invasive_diabetes_testing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, diagnostic_device_manufacturers).
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, clinical_laboratory_network).
narrative_ontology:constraint_beneficiary(requirement_invasive_diabetes_testing, phlebotomists).
narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, diabetic_patients).
narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, prediabetic_population).
narrative_ontology:constraint_victim(requirement_invasive_diabetes_testing, frequent_monitoring_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIABETIC PATIENT (SNARE) — Trapped by medical necessity. Diagnosis requires invasive testing; effective self-management requires repeated testing (4-8 times annually for HbA1c, finger-pricks for home glucose monitoring). No alternative diagnostic pathway exists in standard medical practice. Patient bears full cost: pain, inconvenience, infection risk, psychological burden of repeated phlebotomy. d≈0.92, f(d)≈1.40, σ=1.1 → χ≈0.60.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIMARY CARE PHYSICIAN (TANGLED ROPE) — Constrained by medical standards of care and liability (must use validated invasive tests). Also benefits from the coordination function: invasive blood tests provide high-confidence baseline data that enables treatment decisions and risk stratification. Trapped by evidence base and legal requirement. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIAGNOSTIC DEVICE MANUFACTURERS (ROPE) — Primary beneficiary. Invasive testing requirement creates reliable recurring revenue: $10B+ annual global market for glucose meters, lancets, test strips, phlebotomy supplies. The constraint is a coordination mechanism for them — standardized testing protocols ensure compatibility and supply chain stability. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary; sees constraint as essential coordination.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CGM ADVOCACY COALITION (SCAFFOLD) — Organized movement (patient advocates, some manufacturers, progressive healthcare systems) building non-invasive/minimally-invasive alternatives: continuous glucose monitors (subcutaneous sensors, wearables), non-invasive spectroscopic glucose measurement, saliva-based biomarkers. These are creating parallel diagnostic pathways with sunset logic — as accuracy and reimbursement improve, invasive testing becomes optional. d≈0.35, f(d)≈0.32, σ=1.1 → χ≈0.12. Low effective extraction because coalition has agency and clear exit path.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CLINICAL LABORATORY ACCREDITATION SYSTEM (PITON) — The accreditation standards (CLIA, ISO 15189) that mandate invasive blood testing were designed when non-invasive alternatives did not exist. Now they persist through institutional inertia: regulators maintain invasive-test-only standards partly due to conservative liability logic ('proven modality') and partly because alternatives haven't fully proven themselves in regulatory terms. theater_ratio=0.58 reflects that much of the justification is now performative — 'we require invasive testing because that's what we've always validated' — rather than functionally necessary. The standard endures due to path dependency.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, one might frame blood glucose measurement as an immutable physical constraint: glucose lives in blood, so blood access is required for direct measurement. This naturalizes the constraint as a law of nature. However, the structural data (ε=0.38, suppression=0.62, theater=0.58) contradicts this. The engine will identify this as a false summit: non-invasive measurement (spectroscopic, saliva-based, interstitial fluid via subcutaneous sensors) is physically possible. The 'blood access is required' claim naturalizes a contingent technological choice (direct bloodstream measurement preferred by 1970s equipment) as inevitable.
constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(requirement_invasive_diabetes_testing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(requirement_invasive_diabetes_testing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(requirement_invasive_diabetes_testing, TR),
    TR >= 0.70.

:- end_tests(requirement_invasive_diabetes_testing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The invasive testing requirement imposes real costs on patients (pain, infection risk, psychological burden, time) that represent value extraction: diagnostic companies and laboratory networks capture economic rents from a captive patient population with limited alternatives. However, the extraction is not as severe as a classic snare (ε≈0.72) because the testing serves a genuine clinical function (blood glucose measurement for diagnosis and risk stratification) that patients also benefit from. The extraction lies in the enforcement of invasiveness as the only pathway when less-invasive alternatives exist but are not yet fully accepted. Suppression (0.62): Moderate-high. Significant barriers include regulatory conservatism (invasive-test-only in official guidelines), reimbursement structures that favor established modalities, clinical inertia (training, habit, liability risk of adopting unproven alternatives), and the accumulated institutional infrastructure around invasive testing (laboratory networks, accreditation systems). Patients cannot easily opt out of testing (medical necessity) and cannot easily access non-invasive alternatives (not standard of care, not reimbursed). Theater ratio (0.58): Moderate. Increasing over time. Regulatory justifications have become increasingly performative — the claim that 'blood access is required for measurement' naturalizes a contingent technological choice (direct blood measurement preferred when alternatives didn't exist) as inevitable. The actual justification is now 'we maintain invasive-test-only standards because that's what we've always validated and alternatives haven't yet fully proven themselves,' which is partly functional (conservative safety logic) and partly theatrical (path dependency masquerading as necessity).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates significant perspectival divergence. Patients (powerless/trapped) see a snare — they have no choice but to endure repeated needle sticks. Manufacturers and laboratories (institutional/arbitrage) see a rope — invasive testing is a coordination mechanism that ensures standardized, reproducible, globally compatible measurement and sustainable revenue. Primary care physicians (moderate/constrained) see a tangled rope — they benefit from the coordination function (validated data) but are also constrained by the requirement. The CGM advocacy coalition (organized/constrained) sees a temporary problem with a sunset — alternative modalities are building parallel pathways that will make invasive testing optional within 10-20 years. Regulators and the clinical laboratory establishment (institutional/arbitrage) see a piton — the invasive-test-only standard persists through institutional inertia and conservative liability logic, not because it's functionally necessary. The analytical observer risks seeing a false natural law (mountain) — 'glucose lives in blood, so blood access is required' — but the structural data (moderate extraction, high suppression, rising theater) reveals this as a naturalized contingency, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Diabetic patients: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no exit options. Primary care physicians: Mixed (both benefit from coordination and are constrained by standard) + constrained → d≈0.68, f(d)≈1.05. Significant extraction with coordination component. Diagnostic manufacturers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; see constraint as coordinating mechanism. Laboratory networks: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary from infrastructure and recurring revenue. CGM coalition: Organized + constrained → d≈0.35, f(d)≈0.32. Low effective extraction because coalition has agency and visible exit pathway. Regulators: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton classification comes from theater gate (0.58) indicating performative justification, not from high chi.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the constraint is transitional. The invasive blood testing requirement is a genuine tangled rope during the period when non-invasive alternatives do not yet meet clinical and regulatory equivalence standards. Once non-invasive modalities achieve accuracy parity, regulatory approval, and reimbursement parity, the constraint will transition to a scaffold with explicit sunset — invasive testing will remain available as a reference standard but will no longer be mandatory. The piton perspective (performative standards) is emerging now because the justification for invasive-only testing is becoming increasingly theatrical: regulators maintain the standard partly from conservative liability logic and partly from institutional inertia ('we've always done it this way'), not from physical or chemical necessity. The false natural law perspective ('blood glucose is in blood, so blood access is required') naturalizes a 1970s technological choice (direct blood measurement) as inevitable, when spectroscopic, interstitial fluid, and saliva-based modalities offer physically viable alternatives. The analytical observer should recognize that the constraint's classification will evolve: tangled rope (present) → scaffold (2-5 years as CGM accuracy improves) → rope (10+ years as non-invasive becomes standard, invasive becomes optional) or piton (10+ years if invasive testing persists through pure institutional inertia despite superior alternatives).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    noninvasive_accuracy_threshold,
    'What accuracy threshold for non-invasive glucose measurement (e.g., continuous interstitial fluid sensors, spectroscopic methods) would constitute clinical equivalence to invasive blood testing for diagnostic purposes?',
    'Randomized controlled trials comparing non-invasive modality to gold-standard venous plasma glucose and invasive CGM; equivalence criteria from FDA/EMA guidance; clinical outcomes analysis (do treatment changes based on non-invasive data produce equivalent HbA1c targets?)',
    'If threshold achievable with current technology: scaffold perspective confirmed, invasive testing becomes optional for most patients within 5-10 years. If threshold requires fundamental advances: invasive testing remains standard of care; scaffold timeline extends beyond 20 years.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(noninvasive_accuracy_threshold, empirical, 'Accuracy threshold for non-invasive glucose measurement clinical equivalence').

omega_variable(
    reimbursement_pathway_sufficiency,
    'Will major healthcare systems and insurers reimburse continuous glucose monitoring and non-invasive alternatives at parity with or above invasive testing, given equivalent or superior patient outcomes?',
    'Analysis of reimbursement policy trends (2020-2026); cost-effectiveness modeling comparing CGM + non-invasive vs traditional invasive; patient outcome data (HbA1c, complication rates, quality of life) by modality; insurance formulary evolution',
    'If reimbursement shifts: economic incentive for invasive testing collapses; manufacturers pivot to non-invasive; scaffold sunset accelerates. If reimbursement maintains invasive preference: economic extraction persists despite superior alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reimbursement_pathway_sufficiency, empirical, 'Whether healthcare systems will reimburse non-invasive alternatives at parity').

omega_variable(
    regulatory_validation_lag,
    'Is the FDA/EMA regulatory validation lag for non-invasive modalities (currently 5-15 years from prototype to approval) a structural feature of thorough safety vetting or an unnecessary conservative gate that delays patient benefit?',
    'Comparative analysis of non-invasive modality failure rates post-approval vs invasive modality failure rates post-approval; adverse event reporting; regulatory delay impact on clinical adoption timelines; alternative regulatory pathways (adaptive approval, real-world evidence)',
    'If lag is necessary safety vetting: scaffold timeline justified; piton perspective (conservative standards) is functional. If lag is excessive: regulatory inertia is sustaining extraction; accelerated pathways would collapse the constraint within 2-5 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_validation_lag, conceptual, 'Whether FDA/EMA validation lag for non-invasive glucose measurement is necessary or excessive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(requirement_invasive_diabetes_testing, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(invdiab_tr_t0, requirement_invasive_diabetes_testing, theater_ratio, 0, 0.42).
narrative_ontology:measurement(invdiab_tr_t20, requirement_invasive_diabetes_testing, theater_ratio, 20, 0.5).
narrative_ontology:measurement(invdiab_tr_t40, requirement_invasive_diabetes_testing, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(invdiab_be_t0, requirement_invasive_diabetes_testing, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(invdiab_be_t20, requirement_invasive_diabetes_testing, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(invdiab_be_t40, requirement_invasive_diabetes_testing, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(requirement_invasive_diabetes_testing, information_standard).
narrative_ontology:affects_constraint(requirement_invasive_diabetes_testing, diabetes_self_monitoring_burden).
narrative_ontology:affects_constraint(requirement_invasive_diabetes_testing, medical_device_market_concentration).

% DUAL FORMULATION NOTE:
% The invasive blood testing requirement is upstream of two structural constraints: (1) diabetes self-monitoring burden (the psychological and practical load of frequent testing creates barriers to effective self-management), and (2) medical device market concentration (the stable revenue stream from captive testing requirement enables consolidation). This story focuses on the invasive-test-only requirement itself; the downstream stories address how this requirement cascades into patient behavior and market structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
