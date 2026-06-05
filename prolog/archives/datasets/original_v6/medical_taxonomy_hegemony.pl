% ============================================================================
% CONSTRAINT STORY: medical_taxonomy_hegemony
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_taxonomy_hegemony, []).

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
 *   constraint_id: medical_taxonomy_hegemony
 *   human_readable: Medical Taxonomy Hegemony: ICD/DSM Classification as Extraction and Coordination
 *   domain: medical/psychiatric/regulatory
 *
 * SUMMARY:
 *   Medical taxonomy hegemony—the institutional dominance of DSM (Diagnostic
 *   and Statistical Manual) for psychiatric diagnosis and ICD (International
 *   Classification of Diseases) for medical classification—creates a
 *   structural constraint that simultaneously coordinates clinical practice
 *   and extracts value from patients, clinicians, and alternative diagnostic
 *   frameworks. The constraint exhibits characteristics of all six DR types
 *   depending on observer position: a pure extraction mechanism (snare) for
 *   patients with atypical presentations; a coordination tool (rope) for
 *   insurance and pharmaceutical infrastructure; a mixed system (tangled
 *   rope) for clinicians and researchers; a performative revision process
 *   (piton) that appears evidence-driven while preserving categorical
 *   hegemony; and a temporary institutional arrangement (scaffold) from the
 *   perspective of emerging dimensional medicine approaches. The constraint's
 *   theater_ratio (0.61) reflects that diagnostic revision processes create
 *   appearance of evidence-driven updating (DSM-5 consultation panels,
 *   literature reviews, field trials) while preserving the fundamental
 *   categorical structure that serves institutional interests. Base
 *   extractiveness has risen from 0.35 to 0.58 over 20 years due to: (1)
 *   pharmaceutical-driven category expansion (DSM-5 recognized ADHD, autism
 *   spectrum, and mood dysregulation far more broadly than DSM-IV), (2)
 *   insurance requirement that all treatments be coded to specific diagnoses
 *   (creating pressure to diagnose when treatment might be more supportive),
 *   and (3) exclusion of non-Western diagnostic frameworks (Traditional
 *   Chinese Medicine, Ayurvedic classification, Indigenous healing
 *   frameworks) from insurance/approval chains. The constraint satisfies the
 *   tangled_rope definition: genuine coordination function (clinical
 *   communication, research standardization, statistical tracking) exists
 *   alongside asymmetric extraction (from patients who don't fit categories,
 *   from clinicians whose judgment is constrained, from research that
 *   challenges categorical assumptions).
 *
 * KEY AGENTS:
 *   - Patients with Atypical Presentations: Primary victim (powerless/trapped) — no alternative diagnosis pathway; insurance denies coverage for 'unclassified' conditions; bears full extraction cost
 *   - Excluded Populations (genetic minorities, non-Western populations): Primary victim (powerless/trapped) — diagnostic criteria derived from WEIRD cohorts; alternative medical frameworks excluded from institutional chain
 *   - Clinical Innovators and Researchers: Secondary victim (organized/constrained) — publishing pressure, funding bias toward DSM-recognized disorders, difficulty studying non-categorized phenomena
 *   - Clinicians: Mixed actor (moderate/constrained) — genuine coordination benefit alongside extraction of autonomy, coding overhead, liability constraints
 *   - Billing and Insurance Infrastructure: Primary beneficiary (institutional/arbitrage) — taxonomy enables their core function; can arbitrage between coding schemes
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — approval, marketing, and reimbursement tied to diagnosis codes; can lobby for category expansions
 *   - Dimensional Medicine Coalition: Organized agent (organized/constrained) — building alternative pathways; constrained by current institutional requirement but with visible sunset
 *   - Diagnostic Revision Committees: Institutional actor (institutional/arbitrage) — maintain performative update process; preserve categorical structure through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_taxonomy_hegemony, 0.58).
domain_priors:suppression_score(medical_taxonomy_hegemony, 0.68).
domain_priors:theater_ratio(medical_taxonomy_hegemony, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_taxonomy_hegemony, extractiveness, 0.58).
narrative_ontology:constraint_metric(medical_taxonomy_hegemony, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(medical_taxonomy_hegemony, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_taxonomy_hegemony, tangled_rope).
narrative_ontology:human_readable(medical_taxonomy_hegemony, "Medical Taxonomy Hegemony: ICD/DSM Classification as Extraction and Coordination").
narrative_ontology:topic_domain(medical_taxonomy_hegemony, "medical/psychiatric/regulatory").

domain_priors:requires_active_enforcement(medical_taxonomy_hegemony).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_taxonomy_hegemony, billing_infrastructure).
narrative_ontology:constraint_beneficiary(medical_taxonomy_hegemony, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(medical_taxonomy_hegemony, diagnostic_gatekeepers).
narrative_ontology:constraint_victim(medical_taxonomy_hegemony, patients_with_atypical_presentations).
narrative_ontology:constraint_victim(medical_taxonomy_hegemony, excluded_populations).
narrative_ontology:constraint_victim(medical_taxonomy_hegemony, clinical_innovation).
narrative_ontology:constraint_victim(medical_taxonomy_hegemony, diagnostic_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATYPICAL PATIENT (SNARE) — A person whose symptoms do not fit DSM categories precisely, or whose condition spans multiple diagnostic codes, experiences the taxonomy as a rigid extraction mechanism. No alternative diagnosis pathway exists within the treatment system. Insurance denies coverage for 'unclassified' conditions. Clinicians must force-fit the presentation into existing categories or the patient receives no institutional care. The patient is trapped — structural barriers (insurance, medical licensing, pharmaceutical regulation) prevent exit. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED POPULATIONS (SNARE) — Genetic and ethnic minorities underrepresented in diagnostic validation cohorts experience systematic misdiagnosis or non-recognition of conditions specific to their populations. The taxonomy canonicalizes diagnostic criteria derived from WEIRD (Western, Educated, Industrialized, Rich, Democratic) cohorts. Alternative diagnostic frameworks exist in non-Western medicine but are excluded from insurance/pharmaceutical approval chains. Trapped by institutional requirement to use ICD/DSM for any institutional interaction. Structural extraction with no exit.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CLINICIAN (TANGLED ROPE) — A physician or psychiatrist uses the taxonomy as a genuine coordination tool (communicating diagnoses across systems, accessing evidence-based protocols, enabling continuity of care) but also experiences it as an extraction mechanism (coding overhead, diagnostic box-fitting that constrains clinical judgment, insurance pre-authorization requirements tied to specific codes, liability risk for 'off-label' treatments that don't match the DSM diagnosis). Constrained by licensing regulations, malpractice exposure, and reimbursement dependency. Mixed benefit — genuine coordination benefit alongside significant extraction of time and clinical autonomy.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BILLING INFRASTRUCTURE (ROPE) — Insurance companies, hospital billing departments, and pharmaceutical reimbursement systems use ICD/DSM codes as a pure coordination mechanism for resource allocation, claims processing, and statistical tracking. The taxonomy enables their core function and creates value for them through standardization. They experience the constraint as beneficial coordination with low extraction cost — they can arbitrage between coding schemes if needed or lobby for new codes. Net beneficiary position.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL MANUFACTURERS (ROPE) — Drug approval, marketing, and reimbursement all depend on correspondence between approved indications and DSM/ICD codes. The taxonomy enables them to define markets and target populations for drug development. Manufacturers can influence taxonomy revisions (DSM-5 expansion of diagnostic categories) and arbitrage between codes when pursuing new indications. Beneficiary with exit options — they experience the constraint as a coordination mechanism that serves their interests.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLINICAL RESEARCH COMMUNITY (TANGLED ROPE) — Organized researchers experience the taxonomy as both coordination (DSM/ICD enables standardized phenotyping, meta-analysis, and multi-site trials) and extraction (pressure to categorize complex phenomena into existing boxes; difficulty publishing results that challenge diagnostic categories; inability to pursue research on non-recognized conditions; funding tied to DSM-recognized disorders). Constrained by journal publication norms, grant review criteria, and pharmaceutical sponsorship patterns. Real coordination benefit alongside significant research innovation constraints.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: DIAGNOSTIC REVISION PROCESS (PITON) — The DSM revision process (DSM-IV to DSM-5) appears as a formal mechanism for updating diagnostic criteria based on emerging evidence, but the actual function is largely performative. Revisions are driven as much by consensus theater, pharmaceutical influence, and path dependence as by scientific evidence. Alternative diagnostic frameworks (ICD-11 body-mind categories, Network Medicine approaches, computational nosology) exist but cannot displace the established hegemony. The revision machinery persists through inertia and institutional requirement rather than genuine adaptive function. Theater ratio is high — the consultation panels, literature reviews, and field trials create appearance of evidence-driven updating without fundamentally challenging the categorical structure itself.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: DIMENSIONAL MEDICINE COALITION (SCAFFOLD) — Organized researchers and clinicians advocating for dimensional (symptom-based) rather than categorical (disease-based) diagnosis, network medicine approaches, and computational nosology see the categorical taxonomy as a temporary institutional constraint with a sunset. NIH Research Domain Criteria (RDoC), transdiagnostic treatment protocols, and computational approaches to symptom clustering represent alternative pathways that are gradually displacing categorical diagnosis. This perspective sees the taxonomy as a transitional coordination mechanism that will be replaced by evidence-driven dimensional systems as technology matures and institutional norms shift. Constrained by current institutional requirement to use DSM/ICD for insurance/approval, but with genuine exit pathway visible on the horizon (estimated 15-25 year sunset as electronic health records and machine learning enable continuous phenotyping).
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some diagnostic taxonomy is inherent to medicine: any treatment system must categorize patients into clinically meaningful groups, and any categorization system must make boundary-drawing decisions that will be imperfect and contested. This perspective sees the extraction as an immutable feature of how medical knowledge is organized — humans cannot escape categorical thinking, and therefore some patients will always experience misclassification. However, the structural data contradicts the mountain classification — the engine should compute this as a false summit, revealing that cognitive limits on categorical thinking are being naturalized to justify institutional preservation of a specific taxonomy rather than evolution toward better categorization methods.
constraint_indexing:constraint_classification(medical_taxonomy_hegemony, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_taxonomy_hegemony_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_taxonomy_hegemony, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_taxonomy_hegemony, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_taxonomy_hegemony, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_taxonomy_hegemony, TR),
    TR >= 0.70.

:- end_tests(medical_taxonomy_hegemony_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts substantially but not absolutely. The beneficiary group (insurance, pharmaceutical) captures significant value through the coordination system, and they can shape its evolution. Patients in atypical categories experience near-total extraction (no exit). The constraint's extractiveness has increased over time due to pharmaceutical-driven category expansion and insurance tightening. Suppression (0.68): High. Structural barriers to exit include insurance requirement, medical licensing regulations, pharmaceutical approval processes, and exclusion of alternative frameworks. Patients cannot receive institutional treatment without DSM/ICD coding. Clinicians face malpractice liability for using alternative diagnostic frameworks. Research funding is biased toward DSM-recognized disorders. Non-Western medical systems are excluded from approval chains. Theater ratio (0.61): Moderate-high. The DSM revision process creates appearance of evidence-driven updating through consultation panels, literature reviews, and field trials, but preserves the fundamental categorical structure because that structure serves institutional interests. The theater has increased as the revision process has become more elaborate while the underlying logic (categorical disease entities vs dimensional symptom clustering) remains unchanged. Alternative diagnostic frameworks (RDoC, network medicine, computational nosology) exist but cannot penetrate the institutional barrier.
 *
 * PERSPECTIVAL GAP:
 *   The maximum gap exists between the beneficiary perspective (insurance/pharmaceutical, which sees Rope coordination) and the victim perspective (atypical patients, which sees Snare extraction). The beneficiary experiences the taxonomy as enabling their core function with minimal overhead cost. The victim experiences it as a rigid box that denies them treatment if they don't fit. The clinician perspective (Tangled Rope) bridges these two: clinicians get genuine coordination benefit (standardized communication, evidence-based protocols) but also bear extraction cost (autonomy reduction, coding overhead, liability constraints). The scaffold perspective (dimensional medicine coalition) recognizes that the current categorical hegemony is institutional rather than inevitable, and that alternative systems are being built to replace it. The piton perspective (revision process) observes that the machinery for updating the taxonomy is largely performative — it creates theater of evidence-driven progress while preserving the fundamental categorical structure because that structure has become embedded in insurance, pharmaceutical, and regulatory systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from structural position relative to the extraction flow. Insurance and pharmaceutical actors are beneficiaries with arbitrage options (d ≈ 0.1-0.2, low experienced extractiveness). Patients with atypical presentations are victims with no exit (d ≈ 0.95, maximum experienced extractiveness). Clinicians are victims with constrained exit (high cost to exit institutional medicine; d ≈ 0.65-0.75). The dimensional medicine coalition is organized with a visible exit pathway (d ≈ 0.5-0.6, moderate extraction). The key insight: the same institutional arrangement produces wildly different chi values for different observers because their exit options and beneficiary status differ fundamentally. The insurance company experiences chi approaching zero (beneficiary plus arbitrage options), while the patient in the atypical category experiences chi > 1.0 (victim plus trapped exit). The tangled_rope classification is correct because genuine coordination function exists (clinical communication, statistical tracking) alongside asymmetric extraction (from patients, from clinical innovation, from research directed at alternative frameworks).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival presheaf: The mandate of this constraint is to provide a common diagnostic language for medicine — a genuine coordination goal. But the specific implementation (categorical DSM/ICD hegemony) extracts value by: (1) creating moats around pharmaceutical approval and insurance reimbursement; (2) excluding non-Western diagnostic frameworks; (3) constraining clinical innovation that doesn't fit categories; (4) forcing patients into boxes that don't fit them. The mandatrophy is resolved by observing that the coordination function COULD be served by many different taxonomic systems (dimensional, network-based, computational nosology, pluralistic). The specific DSM/ICD system is not mandated by the coordination goal — it is maintained by institutional lock-in (insurance systems, pharmaceutical regulation, medical licensing) and beneficiary preservation. The scaffold and dimensional medicine perspectives show that alternative systems are technically and scientifically viable. The mandatrophy dissolution reveals that this is not a case where extraction is necessary to achieve coordination, but rather where an institutional choice has been made to preserve a particular taxonomy because it benefits insurance and pharmaceutical interests, while framing that choice as inevitable or evidence-driven (theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_inevitable_misclassification,
    'Is the measured extraction (0.58) a feature of any diagnostic taxonomy (inevitable categorical imprecision) or a result of specific institutional choices (DSM hegemony) that could be replaced by better systems?',
    'Comparative analysis of misclassification rates in RDoC/dimensional approaches vs categorical DSM diagnosis; patient satisfaction and treatment outcomes in systems using alternative taxonomies; computational nosology error rates on held-out validation sets',
    'If inherent: extraction is unavoidable and mountain classification is correct. If institutional: extraction is contingent and suppressible through taxonomy reform, making tangled_rope the accurate classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_vs_inevitable_misclassification, empirical, 'Whether extraction is inherent to categorization or contingent to DSM hegemony').

omega_variable(
    pharmaceutical_influence_on_taxonomy,
    'To what degree does pharmaceutical manufacturer lobbying and drug approval incentives shape DSM diagnostic category expansions vs genuine clinical evidence?',
    'Quantitative analysis of pharmaceutical sponsorship patterns in DSM-5 revision process; comparison between diagnostic criteria changes with strong pharma approval incentives vs those without; tracking of medication approvals following diagnostic expansion',
    'If high influence: beneficiary group (pharmaceutical) is actively maintaining extraction mechanism. If low influence: categorization decisions are driven by clinical evidence and the beneficiary position is incidental. Changes the mandatrophy analysis significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_influence_on_taxonomy, empirical, 'Pharmaceutical manufacturer influence on taxonomy revisions').

omega_variable(
    alternative_taxonomy_adoption_timeline,
    'What is the realistic timeline for dimensional/network medicine approaches to displace categorical diagnosis at scale in institutional medicine?',
    'Longitudinal tracking of RDoC integration in clinical practice; adoption rates of dimensional assessment tools in EHR systems; prediction models based on institutional change dynamics in healthcare IT and reimbursement policy',
    'If < 10 years: scaffold sunset is near-term and structural change is already underway. If 20-30+ years: scaffold perspective is aspirational; the constraint may persist as snare/tangled_rope for multiple career lifespans. Affects confidence in mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_taxonomy_adoption_timeline, empirical, 'Timeline for dimensional medicine displacement of categorical diagnosis').

omega_variable(
    cross_cultural_diagnostic_validity,
    'Do DSM diagnostic categories have equivalent validity and utility across non-Western populations, or is the apparent hegemony partly a product of convergence bias in the validation literature?',
    'Meta-analysis of cross-cultural diagnostic validity studies; systematic review of diagnostic discordance rates between DSM and ICD-10/ICD-11 across geographic regions; comparison of symptom prevalence and clustering in non-WEIRD populations',
    'If equivalent validity: the taxonomy represents genuine universal structure and extraction is largely institutional. If divergent validity: the taxonomy is culturally specific and extracted from non-Western populations through institutional pressure to use WEIRD-derived categories. Changes assessment of victim status and suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_cultural_diagnostic_validity, empirical, 'Cross-cultural validity and applicability of DSM categories').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_taxonomy_hegemony, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medtax_tr_t0, medical_taxonomy_hegemony, theater_ratio, 0, 0.48).
narrative_ontology:measurement(medtax_tr_t10, medical_taxonomy_hegemony, theater_ratio, 10, 0.55).
narrative_ontology:measurement(medtax_tr_t20, medical_taxonomy_hegemony, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(medtax_be_t0, medical_taxonomy_hegemony, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(medtax_be_t10, medical_taxonomy_hegemony, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(medtax_be_t20, medical_taxonomy_hegemony, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_taxonomy_hegemony, identity_coordination).
narrative_ontology:affects_constraint(medical_taxonomy_hegemony, pharmaceutical_market_definition).
narrative_ontology:affects_constraint(medical_taxonomy_hegemony, insurance_reimbursement_architecture).
narrative_ontology:affects_constraint(medical_taxonomy_hegemony, psychiatric_research_funding_bias).

% DUAL FORMULATION NOTE:
% Medical taxonomy hegemony decomposes into three related constraints with distinct ε values: (1) diagnostic_categorization_logic (ε=0.25, Mountain or Rope — the abstract problem of classifying complex phenomena into clinically useful groups); (2) dsmv_institutional_dominance (ε=0.58, Tangled Rope — the specific choice to use DSM as the hegemonic system); (3) pharmaceutical_taxonomy_influence (ε=0.52, Tangled Rope — pharmaceutical manufacturer influence on category expansion). This story focuses on the institutional dominance story. The others can be written separately and linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_taxonomy_hegemony, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
