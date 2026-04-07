% ============================================================================
% CONSTRAINT STORY: microbiota_biomarker_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_microbiota_biomarker_stratification, []).

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
 *   constraint_id: microbiota_biomarker_stratification
 *   human_readable: Microbiota Biomarker Stratification in Clinical Medicine
 *   domain: biomedical/clinical_microbiology/personalized_medicine
 *
 * SUMMARY:
 *   Microbiota biomarker stratification represents a structural constraint
 *   embedded in the convergence of personalized medicine, commercial
 *   diagnostics, and clinical microbiology. The constraint operates at the
 *   intersection of legitimate scientific coordination (multiple research
 *   groups sharing methods and data to advance microbiota science) and
 *   extractive behavior (proprietary lock-in on biomarker panels, licensing
 *   restrictions, data-sharing barriers). The core tension: microbiota
 *   composition is genuinely variable across individuals and clinically
 *   relevant for treatment response, but the mechanisms for translating this
 *   variation into clinical biomarkers are controlled by commercial actors
 *   who benefit from information asymmetry and lack of alternatives. The
 *   constraint exhibits all six classification types depending on the
 *   observer's structural position, making it a rich site for understanding
 *   how institutional arrangements naturalize as biological necessity.
 *
 * KEY AGENTS:
 *   - Patient Cohorts: Primary victims (powerless/trapped) — depend on biomarker stratification for treatment decisions but have no access to underlying biomarker logic or alternative stratification methods
 *   - Clinicians Without Proprietary Access: Secondary victims (moderate/constrained) — face barriers to accessing proprietary panels and must choose between costly commercial testing or less-accurate open biomarkers
 *   - Low-Resource Healthcare Systems: Secondary victims (organized/constrained) — cannot afford proprietary test costs and face regulatory barriers to developing local biomarker panels
 *   - Diagnostic Test Manufacturers: Primary beneficiaries (institutional/arbitrage) — control proprietary biomarker panels and benefit from market demand for stratified diagnostics
 *   - Microbiota Research Industry: Secondary beneficiary (institutional/arbitrage) — benefits from publication demand for biomarker validation and from commercial partnerships
 *   - Academic Microbiota Researchers: Mixed (moderate/constrained) — coordinate scientific knowledge but face extraction through IP control and publication barriers
 *   - Regulatory Approval System: Institutional actor (institutional/arbitrage) — validates biomarkers through retrospective cohorts (theater) while lacking mechanisms to enforce prospective validation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (proprietary control, licensing restrictions) as inherent biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(microbiota_biomarker_stratification, 0.52).
domain_priors:suppression_score(microbiota_biomarker_stratification, 0.58).
domain_priors:theater_ratio(microbiota_biomarker_stratification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(microbiota_biomarker_stratification, extractiveness, 0.52).
narrative_ontology:constraint_metric(microbiota_biomarker_stratification, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(microbiota_biomarker_stratification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(microbiota_biomarker_stratification, tangled_rope).
narrative_ontology:human_readable(microbiota_biomarker_stratification, "Microbiota Biomarker Stratification in Clinical Medicine").
narrative_ontology:topic_domain(microbiota_biomarker_stratification, "biomedical/clinical_microbiology/personalized_medicine").

domain_priors:requires_active_enforcement(microbiota_biomarker_stratification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(microbiota_biomarker_stratification, diagnostic_test_manufacturers).
narrative_ontology:constraint_beneficiary(microbiota_biomarker_stratification, microbiota_research_industry).
narrative_ontology:constraint_victim(microbiota_biomarker_stratification, patient_stratification_accuracy).
narrative_ontology:constraint_victim(microbiota_biomarker_stratification, clinicians_without_proprietary_access).
narrative_ontology:constraint_victim(microbiota_biomarker_stratification, low_resource_healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT COHORT (SNARE) — Patients have no exit from the constraint. Clinical decisions about treatment stratification depend on proprietary biomarker panels controlled by commercial actors. The patient bears the cost of misclassification (wrong treatment pathway, delayed diagnosis, unnecessary therapy) but has no ability to access or understand the underlying biomarker logic. Trapped by the requirement for medical care and by information asymmetry.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLINICIAN WITHOUT PROPRIETARY ACCESS (SNARE) — Clinicians who lack licenses or institutional access to proprietary microbiota panels face constrained options: use less-accurate public biomarkers, request costly proprietary testing, or revert to clinical judgment without biomarker guidance. The constraint extracts time (additional clinical consultation for justification), money (test costs), and epistemic authority (reliance on black-box panels). High suppression due to regulatory barriers (panels are validated and approved, creating legitimacy). Some exit exists (clinical judgment alone) but is professionally risky.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACADEMIC MICROBIOTA RESEARCHER (TANGLED ROPE) — Researchers genuinely coordinate the advance of microbiota knowledge (cooperation function), yet face extraction through proprietary lock-in: publication in high-impact journals increasingly requires commercial biomarker validation, samples may be restricted by data-sharing barriers, and researchers who develop novel biomarkers often lose control of their use. Constrained exit due to publication incentives and funding dependencies. Both coordination (knowledge sharing) and extraction (IP control) are present.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DIAGNOSTIC TEST MANUFACTURER (ROPE) — Manufacturers benefit from the coordination function: microbiota stratification enables personalized medicine, which drives demand for specialized diagnostics. They experience the constraint as solving a genuine problem (how to identify patient subgroups). High exit options (arbitrage) because they can shift product lines, sell proprietary panels, or license technology. Net beneficiary, but the classification as Rope reflects genuine coordination value in enabling stratified care.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LOW-RESOURCE HEALTHCARE SYSTEM (TANGLED ROPE) — Healthcare systems in low-income regions experience both coordination and extraction. The coordination function: access to microbiota biomarkers could improve patient outcomes in settings where traditional diagnostics are unavailable. The extraction: proprietary test costs are prohibitive, data-sharing barriers prevent adaptation to local microbiota profiles, and licensing restrictions prevent local test development. Constrained exit due to regulatory and financial barriers. Organized enough to negotiate but structurally limited.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY APPROVAL SYSTEM (PITON) — Clinical validation and regulatory approval for microbiota biomarkers are increasingly performative: panels are validated on retrospective cohorts (theater), then deployed on prospective patients (different distribution). The theater_ratio reflects validation studies that show apparent predictive power but fail to generalize. Regulators have arbitrage options (they could demand prospective validation, require open biomarker definitions, or mandate transparency) but default to retrospective approval theater. Maintained through institutional inertia.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, microbiota variation is inherent to human biology: each patient's microbiota is unique, reflecting genetics, diet, environment, and history. Stratification based on biomarkers thus appears to be a natural consequence of biological reality — patients differ, diagnostics must stratify them, and proprietary control is necessary to fund research. However, the base properties contradict the mountain classification. The extractiveness, suppression, and theater ratios indicate institutional contingencies (IP control, validation theater, licensing barriers) rather than natural laws. False summit detection applies.
constraint_indexing:constraint_classification(microbiota_biomarker_stratification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(microbiota_biomarker_stratification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(microbiota_biomarker_stratification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(microbiota_biomarker_stratification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(microbiota_biomarker_stratification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(microbiota_biomarker_stratification, TR),
    TR >= 0.70.

:- end_tests(microbiota_biomarker_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint captures significant value from clinicians and patients through proprietary panel licensing and information asymmetry. However, extractiveness is not severe because (1) genuine coordination value exists (microbiota research does advance), (2) some open-source biomarkers are available (QIIME2, mothur), and (3) clinical benefits from stratification, though contested, are non-zero. The extractiveness trajectory over the interval reflects increasing proprietary control as commercial diagnostics firms consolidate market position. Suppression (0.58): Moderate-high. Barriers to exit include regulatory validation (proprietary panels are approved; open alternatives are not), data-sharing restrictions (raw microbiota data often requires institutional agreements), knowledge asymmetry (biomarker logic is proprietary), and career incentives (researchers are rewarded for using commercial panels in publications). However, suppression is not total because open-source tools exist and clinical judgment remains viable. Theater ratio (0.68): Moderate-high. Clinical validation for microbiota biomarkers typically occurs on retrospective cohorts drawn from the same institutions that develop the biomarkers, introducing substantial selection bias. The theater reflects performative validation (high apparent predictive power on training data) that often fails to generalize to prospective patient populations. The increasing theater_ratio indicates growing gap between validation claims and prospective performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (Rope) and victim (Snare) perspectives reveals the extraction mechanism. Manufacturers see low-cost coordination; patients see high-cost constraints. Regulatory system sees its own process as degraded (Piton) — unable to enforce prospective validation despite knowing that retrospective theater inflates apparent performance. This gap is diagnostic: it reveals that the constraint is not a natural law of microbiota biology but an institutional arrangement where information asymmetry and licensing restrictions concentrate benefits and distribute costs asymmetrically.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect structural position. Manufacturers (beneficiaries, arbitrage exit) experience low effective extraction. Patients (victims, trapped exit) experience high effective extraction. Clinicians (mixed: benefit from biomarkers, harmed by lack of access) experience moderate extraction. The computed chi values will show high extraction for trapped victims and low extraction for beneficiaries with exit options — the asymmetry itself is the signal that the constraint is extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint meets all three tangled rope gates. (1) Beneficiaries exist and are named (diagnostic manufacturers, research industry). (2) Victims exist and are named (patients, clinicians without access, low-resource systems). (3) Active enforcement is required — licensing agreements, data-use restrictions, publication norms, and regulatory validation all actively maintain the constraint. The mandatrophy resolves by recognizing that both coordination (genuine scientific advance) and extraction (proprietary lock-in) are structurally real. The constraint would not persist without the coordination value (researchers genuinely advance microbiota science); it would not extract without the institutional control mechanisms (proprietary licensing, data restrictions). The Tangled Rope classification is precise: it is a hybrid, and both the coordination and extraction components are measurable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_generalization_failure,
    'Do microbiota biomarkers trained on retrospective cohorts actually generalize to prospective patient populations, or do they show substantial performance degradation?',
    'Prospective validation studies tracking biomarker predictions on out-of-sample populations; meta-analysis of retrospective vs prospective biomarker performance; examination of cohort-specific confounders (diet, geography, sample processing)',
    'If poor generalization: biomarker stratification is extraction theater (high theater_ratio justified). If good generalization: theater_ratio should decrease and classification shifts toward Rope. Current high theater_ratio suggests poor generalization is common.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biomarker_generalization_failure, empirical, 'Whether microbiota biomarkers generalize from retrospective to prospective validation').

omega_variable(
    proprietary_necessity_for_innovation,
    'Is proprietary control actually necessary to fund microbiota biomarker development, or does it primarily extract rent from clinicians and patients while slowing reproducible science?',
    'Comparative analysis of innovation rates in proprietary vs open-source biomarker development; funding flow analysis (how much proprietary revenue goes to research vs shareholder return); examination of whether open-source microbiota tools (QIIME2, mothur) accelerate or decelerate innovation relative to proprietary panels',
    'If proprietary necessary: beneficiary status is justified (extraction is coordination cost). If primarily rent extraction: victims'' classification strengthens (constraint moves toward Snare). Current separation of innovation communities suggests both mechanisms are present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_necessity_for_innovation, empirical, 'Whether proprietary control is necessary for microbiota biomarker innovation').

omega_variable(
    stratification_versus_patient_outcome,
    'Do microbiota-based patient stratifications actually improve clinical outcomes compared to simpler clinical or demographic stratification?',
    'Randomized controlled trials comparing microbiota-stratified treatment to standard care; effect size meta-analysis; analysis of whether improved stratification translates to cost-effective clinical benefit',
    'If no outcome improvement: constraint is theater (classification shifts toward Piton). If significant improvement: Tangled Rope justified. If improvement only in proprietary panels (not open biomarkers): extraction is confirmed (Snare strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_versus_patient_outcome, empirical, 'Whether microbiota stratification improves patient clinical outcomes').

omega_variable(
    data_accessibility_and_reproducibility,
    'What proportion of microbiota biomarker studies release raw sequence data, processed abundance tables, and biomarker definitions openly versus restricting to proprietary access?',
    'Systematic review of microbiota biomarker publications; data repository audit (GEO, SRA, Zenodo); analysis of institutional policies requiring open data vs allowing proprietary withholding',
    'If high restriction: suppression mechanism confirmed (epistemic barriers prevent field-wide scrutiny). If high openness: coordination function is primary. Current publishing norms suggest substantial restriction despite open data mandates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_accessibility_and_reproducibility, empirical, 'Proportion of microbiota studies releasing data openly vs proprietary').

omega_variable(
    geographic_microbiota_variation,
    'Do microbiota biomarkers developed on Western patient cohorts transfer to non-Western populations, or do they require local retraining?',
    'Cross-geographic biomarker validation (apply Western-trained models to non-Western cohorts); analysis of biomarker transferability; comparison of research investment across geographic regions',
    'If poor transfer: constraint creates global inequity (low-resource systems cannot use existing panels and cannot fund local development). Victim status of low-resource systems is confirmed. If good transfer: some universality supports coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geographic_microbiota_variation, empirical, 'Whether microbiota biomarkers transfer across geographic populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(microbiota_biomarker_stratification, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mbms_tr_t0, microbiota_biomarker_stratification, theater_ratio, 0, 0.52).
narrative_ontology:measurement(mbms_tr_t3, microbiota_biomarker_stratification, theater_ratio, 3, 0.6).
narrative_ontology:measurement(mbms_tr_t6, microbiota_biomarker_stratification, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(mbms_be_t0, microbiota_biomarker_stratification, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mbms_be_t3, microbiota_biomarker_stratification, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mbms_be_t6, microbiota_biomarker_stratification, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(microbiota_biomarker_stratification, information_standard).
narrative_ontology:affects_constraint(microbiota_biomarker_stratification, precision_medicine_accessibility).
narrative_ontology:affects_constraint(microbiota_biomarker_stratification, microbiota_ip_licensing).

% DUAL FORMULATION NOTE:
% Microbiota biomarker stratification decomposes into two related but distinct constraints: (1) the scientific/clinical coordination problem (how to stratify patients based on microbiota variation for treatment response), which is a genuine coordination problem, and (2) the institutional/commercial extraction problem (proprietary control of biomarker definitions and licensing), which is the mechanism by which value is extracted from clinicians and patients. This story focuses on the second constraint — the institutional arrangement — while network links to the first constraint (precision_medicine_accessibility) capture the upstream coordination challenge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(microbiota_biomarker_stratification, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
