% ============================================================================
% CONSTRAINT STORY: psychiatric_nosology_institutional_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_psychiatric_nosology_institutional_lock_in, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: psychiatric_nosology_institutional_lock_in
 *   human_readable: Psychiatric Nosology Institutional Lock-In
 *   domain: mental_health/institutional_epistemology
 *
 * SUMMARY:
 *   Psychiatric nosology (disease classification) in the DSM-5 and ICD-11
 *   operates as an institutional constraint that coordinates multiple actors
 *   (clinicians, researchers, insurers, patients) around categorical
 *   diagnoses while simultaneously extracting value from those whose work is
 *   constrained by outdated epistemology. The constraint is neither pure
 *   coordination nor pure extraction — it is tangled rope at its core, with
 *   dominant Snare aspects from the patient perspective and Piton (degraded
 *   institutional theater) aspects when examining the DSM revision process.
 *   The neurobiological evidence increasingly demonstrates that categorical
 *   psychiatric diagnoses do not reflect underlying brain mechanisms: a
 *   patient meeting diagnostic criteria for Major Depressive Disorder may
 *   have entirely distinct neurobiological profiles, treatment responses, and
 *   prognoses. Yet the categorical framework persists, enforced through
 *   funding mechanisms, publication norms, insurance coding, clinical
 *   training, and professional licensing. The constraint has drifted toward
 *   higher theater (performative revision cycles) and increasing
 *   extractiveness as evidence against categorical nosology accumulates but
 *   institutional resistance hardens. Alternative frameworks (dimensional
 *   assessment via RDoC, transdiagnostic mechanisms, computational
 *   phenotyping) exist but remain marginalized despite significant scientific
 *   support.
 *
 * KEY AGENTS:
 *   - Patients seeking understanding: Primary victims (powerless/trapped) — require diagnosis for treatment access but receive categorical labels that obscure rather than explain their condition
 *   - Neuroscience researchers: Secondary victims (moderate/constrained) — need categories for cohort definition and communication but are constrained to use neurobiologically invalid frames
 *   - Pharmaceutical industry: Primary beneficiary (institutional/arbitrage) — drug approval, clinical trials, and marketing narratives organized around DSM categories
 *   - Insurance administration: Beneficiary (institutional/arbitrage) — categorical codes enable actuarial systems and billing automation
 *   - Open research community: Constrained advocates (organized/constrained) — propose dimensional/transdiagnostic alternatives but face funding and publication barriers requiring DSM translation
 *   - Psychiatric profession: Institutional defender (institutional/arbitrage) — professional identity and licensing tied to categorical mastery; dimensional frameworks perceived as threat to epistemic authority
 *   - DSM editorial apparatus: Theater performer (institutional/arbitrage) — conducts ritualistic revisions that perform scientific updating while preserving core categorical structure
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing psychiatric nosological necessity as immutable, conflating the necessity of some classification with the necessity of categorical classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(psychiatric_nosology_institutional_lock_in, 0.58).
domain_priors:suppression_score(psychiatric_nosology_institutional_lock_in, 0.65).
domain_priors:theater_ratio(psychiatric_nosology_institutional_lock_in, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(psychiatric_nosology_institutional_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(psychiatric_nosology_institutional_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(psychiatric_nosology_institutional_lock_in, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(psychiatric_nosology_institutional_lock_in, tangled_rope).
narrative_ontology:human_readable(psychiatric_nosology_institutional_lock_in, "Psychiatric Nosology Institutional Lock-In").
narrative_ontology:topic_domain(psychiatric_nosology_institutional_lock_in, "mental_health/institutional_epistemology").

domain_priors:requires_active_enforcement(psychiatric_nosology_institutional_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(psychiatric_nosology_institutional_lock_in, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(psychiatric_nosology_institutional_lock_in, diagnostic_bureaucracy).
narrative_ontology:constraint_beneficiary(psychiatric_nosology_institutional_lock_in, insurance_administration).
narrative_ontology:constraint_beneficiary(psychiatric_nosology_institutional_lock_in, psychiatric_establishment).
narrative_ontology:constraint_victim(psychiatric_nosology_institutional_lock_in, patients_seeking_understanding).
narrative_ontology:constraint_victim(psychiatric_nosology_institutional_lock_in, neuroscience_researchers).
narrative_ontology:constraint_victim(psychiatric_nosology_institutional_lock_in, competing_diagnostic_frameworks).
narrative_ontology:constraint_victim(psychiatric_nosology_institutional_lock_in, dimensional_research_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT SEEKING EXPLANATION (SNARE) — Trapped within the DSM/ICD framework with no meaningful exit. The patient cannot access alternative diagnostic understandings (dimensional, neurobiological, trauma-informed) that might explain their experience. The categorical diagnosis provides ritual validation but minimal explanatory power. Full extraction: the framework organizes the patient's suffering to the benefit of institutional actors while preventing the patient from understanding their own condition through competing epistemologies.
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NEUROSCIENCE RESEARCHER (TANGLED ROPE) — Genuinely needs categorical diagnoses for participant recruitment, cohort definition, and communication with clinicians. But the DSM/ICD categories are increasingly recognized as neurobiologically invalid — they don't carve nature at the joints. Researchers are constrained by the need to publish in DSM frames (to reach clinical audiences, obtain funding) while knowing the categories are epistemically incoherent. Mixed coordination (enabling communication) and extraction (enforcing outdated epistemology).
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL COMPANY (ROPE) — Benefits from the categorical framework through FDA approval pathways, clinical trial designs, and marketing narratives organized around DSM diagnoses. A drug 'for depression' or 'for anxiety' is easier to position than a drug that 'dampens amygdala hyperactivation in dimensional emotion dysregulation.' But the company also has genuine coordination benefits: the DSM frame enables communication with prescribers, insurance companies, and patients. The extraction (monopoly on disease definition during drug patent period) coexists with genuine coordination function.
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE ADMINISTRATION (ROPE) — The categorical DSM structure enables actuarial calculation, risk pooling, and billing automation. Insurance companies experience the framework primarily as a coordination mechanism: 'Major Depressive Disorder' codes are essential infrastructure for healthcare financing. Exit options are available (they could reorganize around dimensional assessments) but switching costs are enormous. The institutional perspective sees coordination benefits as dominant; extracted value exists but is not the primary driver.
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN RESEARCH / DIMENSIONAL ADVOCATES (TANGLED ROPE) — Organized researchers (RDoC, Open Science initiatives, computational psychiatry groups) see alternatives: dimensional assessment, transdiagnostic mechanisms, computational phenotyping. They can articulate these alternatives and mobilize evidence against categorical nosology. But they remain constrained: funding prioritizes DSM-aligned research, journals expect DSM frames, clinicians were trained in categorical thinking. The constraint coordinates across institutional actors (ensuring coherence) while extracting from those proposing alternatives (requiring them to translate into DSM language or face marginalization).
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DSM EDITORIAL APPARATUS (PITON) — The ritual of DSM revision (every 5-20 years) produces the theatrical appearance of evidence-driven classification updating. But the fundamental structure remains unchanged: categorical diagnoses, minimal ecological validity, continued resistance to dimensional/neurobiological integration. The revision process is largely performative — it gives the appearance of scientific progress and institutional responsiveness while the core extraction mechanism persists. Theater ratio is high because the apparatus performs scientific validity without achieving it.
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NOSOLOGICAL NECESSITY (MOUNTAIN) — At civilizational scope, some form of disease classification is inherent to medicine: you cannot treat what you cannot name. The question 'what is the nature of psychiatric conditions?' may have a structure-invariant answer grounded in neurobiology, but the necessity of SOME diagnostic frame for clinical communication appears natural and immutable. This perspective risks naturalizing the specific DSM framework by conflating the necessity of classification with the necessity of categorical classification. Engine false summit detection should flag this.
constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(psychiatric_nosology_institutional_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(psychiatric_nosology_institutional_lock_in, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(psychiatric_nosology_institutional_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(psychiatric_nosology_institutional_lock_in, TR),
    TR >= 0.70.

:- end_tests(psychiatric_nosology_institutional_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts from multiple victims. Patients lose access to neurobiologically-grounded understanding of their conditions. Researchers lose the ability to use brain-valid frameworks in funded research. Alternative epistemologies (dimensional, transdiagnostic, computational) are forced into DSM translation to gain legitimacy. The extraction is maintained through funding gatekeeping, publication norms, and professional credentialing. However, extraction is not total (ε ≥ 0.66 as in pure snares) because the framework does provide genuine coordination benefits: it enables pharmaceutical development, clinical communication, and insurance administration. Suppression (0.65): High. Multiple barriers prevent exit from categorical thinking. Financial incentives (funding, drug approval, insurance reimbursement) are structured around DSM diagnoses. Professional training and licensing are organized categorically. Publication systems expect categorical frames. Patient communities may internalize categorical identity. Clinicians lack practical tools for dimensional assessment. The barriers are not absolute (constrained rather than trapped) but are substantial and mutually reinforcing. Theater ratio (0.68): High. The DSM revision cycle (2013 revision, ongoing ICD-11 development) performs evidence-driven scientific updating while the fundamental categorical structure persists unchanged. Neuroscientists publish evidence that categories are neurobiologically invalid, yet categories remain. Clinicians know categories don't predict treatment response, yet categories remain. The revision apparatus generates the appearance of progress without achieving it. Theater has increased from 0.42 (early DSM development, less contestation) to 0.68 (present, as evidence against categories accumulates but institutional resistance hardens).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radical disagreement across perspectives. Patients (trapped) experience snare — the constraint locks them into categorical identity without explanatory power. Researchers (constrained) experience tangled rope — they need the categories but know they're wrong. Pharma (institutional/arbitrage) experiences rope — genuine coordination through a mechanism that also extracts. Insurance (institutional/arbitrage) experiences pure rope — categorical codes are essential infrastructure. The open research community (organized/constrained) experiences tangled rope — they can articulate alternatives but cannot escape institutional requirement to translate into DSM frames. The DSM apparatus (institutional/arbitrage) experiences piton — it performs revision while structure persists. The analytical observer (civilizational) risks false summit (mountain) by naturalizing categorical necessity. The gap reveals that the constraint has different structural functions for different actors: for those with arbitrage options, it's coordination; for those trapped, it's extraction; for those trying to propose alternatives, it's hybrid enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) tracks whether each agent experiences the constraint as extracting from them (high d) or benefiting them (low d). Patients seeking understanding are full targets (d ≈ 0.95): trapped without alternatives, bearing cognitive cost of categorical mislabeling. Neuroscience researchers are partial targets (d ≈ 0.65): constrained by categorical frames but benefit from DSM-structured communication and research infrastructure. Pharmaceutical industry are beneficiaries (d ≈ 0.08): arbitrage options available (could develop dimensional-aligned drugs) but regulatory/marketing systems favor categorical frames, producing net benefit. Insurance administration are beneficiaries (d ≈ 0.12): arbitrage options available (could implement dimensional coding) but infrastructure investment locked into categorical systems produces net benefit. Open research community are partial targets (d ≈ 0.58): organized enough to articulate alternatives but constrained by funding/publication/credentialing systems biased toward categorical frames. The psychiatric profession occupies an ambiguous position (d ≈ 0.35): they are simultaneously defenders of the categorical system (benefiting through professional authority) and trapped within it (their identity constituted through categorical mastery). Institutional actors universally benefit from lock-in (low d → low/negative f(d) → low χ), while patients and researchers universally bear extraction (high d → high f(d) → high χ). This creates the asymmetric extraction signature of tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through institutional decomposition: The same structural phenomenon (categorical psychiatric diagnosis) is legitimately classified as snare (from patient perspective), tangled rope (from researcher perspective), rope (from pharma perspective), piton (from apparatus perspective), and false-summit mountain (from civilizational perspective). The mandatrophy is not 'which type is correct?' but 'which institutional position are you occupying?' The constraint demonstrates why indexical classification is necessary: a single policy choice (maintain categorical psychiatric nosology) distributes extraction and coordination benefits asymmetrically across actors with different power levels and exit options. From the analyst's civilizational view, the constraint risks appearing as natural law — 'psychiatric categories are necessary for clinical communication' — when in fact the necessity of classification has been conflated with the necessity of categorical classification. The evidence-base (neurobiological studies showing categorical invalidity) increasingly demonstrates that the constraint maintains its extractive function through institutional inertia and political economy rather than epistemic necessity. The theater ratio increase over time (0.42 → 0.68) reveals the transition: in early DSM development, categories were plausibly justified by limited neuroscience; now that neuroscience has advanced, the apparatus performs justification (revision cycles) rather than achieving it. This is the diagnostic signature of piton/false-mountain dynamics — increasing theater as evidence against the structure accumulates but institutional resistance hardens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurobiological_validity_threshold,
    'At what level of neurobiological coherence does a categorical psychiatric diagnosis become epistemically indefensible?',
    'Neuroimaging meta-analyses of categorical diagnoses vs dimensional constructs; genetic correlation analysis; computational phenotyping classification accuracy; comparison of categorical vs dimensional predictive validity for treatment response',
    'If threshold is already crossed: current DSM categories are instrumentally useful fictions that should be openly acknowledged as such, shifting classification from false summit (mountain) toward snare/tangled rope. If threshold is not yet crossed: the constraint retains some claim to epistemic legitimacy rather than pure institutional inertia.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neurobiological_validity_threshold, empirical, 'Neurobiological validity threshold for psychiatric categorical diagnoses').

omega_variable(
    institutional_switching_cost_vs_epistemic_gain,
    'What is the magnitude of institutional switching costs (retraining, billing system redesign, research infrastructure change) relative to epistemic gains from dimensional or transdiagnostic frameworks?',
    'Cost-benefit analysis of major nosological transitions (ICD-10 to ICD-11, DSM-IV to DSM-5); measurement of implementation friction; longitudinal tracking of adoption timelines; financial analysis of billing system transitions',
    'If switching costs are low relative to epistemic gains: the constraint is contingent on institutional inertia and could be overcome by coordinated redesign. If switching costs are catastrophic: the constraint is locked in by infrastructure, not just epistemology — becomes more intractable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_switching_cost_vs_epistemic_gain, empirical, 'Institutional switching costs versus epistemic gains in nosological transition').

omega_variable(
    identity_lock_in_psychiatric_professionalism,
    'To what extent do psychiatrists'' professional identity and career legitimacy depend on DSM categorical mastery? Would dimensional classification threaten their epistemic authority?',
    'Qualitative analysis of psychiatric training curricula; survey of psychiatrist attitudes toward dimensional alternatives; analysis of licensing exam content; historical comparison of professional identity formation before/after DSM standardization',
    'If identity lock is high: psychiatrists will resist dimensional alternatives even when evidence supports them, because accepting alternatives requires reconstituting professional identity. If identity lock is low: adoption of alternatives depends primarily on institutional cost, not professional protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_psychiatric_professionalism, conceptual, 'Professional identity dependence on categorical psychiatric classification').

omega_variable(
    patient_agency_in_nosological_capture,
    'Are patients actively captured by categorical diagnoses (internalize them as identity, prefer them to ambiguity), or passively constrained (they would prefer dimensional/explanatory alternatives but lack access)?',
    'Qualitative research on patient preferences for diagnostic frameworks; analysis of peer support communities (do they organize around categorical diagnoses or dimensional experiences?); longitudinal study of identity formation following diagnosis; patient involvement in diagnostic framework design',
    'If active capture: patients collude in their own constraint — classification shifts toward identity_locked exit option, reflecting cognitive fusion with diagnostic categories. If passive constraint: patients are external victims with trapped or constrained exit, primary constraint is institutional enforcement, not internalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_agency_in_nosological_capture, empirical, 'Degree of patient identity capture versus passive constraint in psychiatric categorization').

omega_variable(
    dimensional_alternative_implementation_readiness,
    'Do dimensional frameworks (RDoC, dimensional ICD-11) have sufficient implementation maturity to replace categorical systems, or do they remain aspirational research projects?',
    'Assessment of dimensional framework clinical utility in real-world settings; measurement of practitioner adoption rates; tracking of insurance/billing system adaptations to dimensional coding; analysis of longitudinal research output using dimensional vs categorical frames',
    'If alternatives are implementation-ready: the constraint is maintained by institutional inertia and political economy, not by absence of alternatives — snare aspect dominates. If alternatives remain immature: some institutional lock-in may be functionally necessary until alternatives mature — tangled rope aspect partially justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dimensional_alternative_implementation_readiness, empirical, 'Implementation maturity of dimensional psychiatric classification alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(psychiatric_nosology_institutional_lock_in, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psych_nos_tr_t0, psychiatric_nosology_institutional_lock_in, theater_ratio, 0, 0.42).
narrative_ontology:measurement(psych_nos_tr_t5, psychiatric_nosology_institutional_lock_in, theater_ratio, 5, 0.55).
narrative_ontology:measurement(psych_nos_tr_t10, psychiatric_nosology_institutional_lock_in, theater_ratio, 10, 0.68).
narrative_ontology:measurement(psych_nos_tr_t15, psychiatric_nosology_institutional_lock_in, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(psych_nos_be_t0, psychiatric_nosology_institutional_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(psych_nos_be_t5, psychiatric_nosology_institutional_lock_in, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(psych_nos_be_t10, psychiatric_nosology_institutional_lock_in, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(psych_nos_be_t15, psychiatric_nosology_institutional_lock_in, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(psychiatric_nosology_institutional_lock_in, identity_coordination).
narrative_ontology:boltzmann_floor_override(psychiatric_nosology_institutional_lock_in, 0.12).
narrative_ontology:affects_constraint(psychiatric_nosology_institutional_lock_in, psychiatric_treatment_algorithm_lock_in).
narrative_ontology:affects_constraint(psychiatric_nosology_institutional_lock_in, psychotropic_drug_approval_framework).
narrative_ontology:affects_constraint(psychiatric_nosology_institutional_lock_in, mental_health_insurance_categorization).
narrative_ontology:affects_constraint(psychiatric_nosology_institutional_lock_in, psychiatric_research_funding_gatekeeping).

% DUAL FORMULATION NOTE:
% Psychiatric nosology institutional lock-in is upstream of multiple domain-specific constraints. The categorical framework constrains how psychiatric treatment algorithms are designed (separate story: treatment_algorithm_lock_in, ε ≈ 0.62). The DSM structure determines psychotropic drug approval pathways (separate story: drug_approval_framework, ε ≈ 0.48). Insurance coding systems depend on DSM categories (separate story: insurance_categorization, ε ≈ 0.42). Research funding mechanisms prioritize DSM-aligned studies (separate story: funding_gatekeeping, ε ≈ 0.51). Each downstream story has its own extractiveness value reflecting the specific institutional dynamics, but all are affected by the upstream nosological constraint. The family exhibits increasing extractiveness in implementation layers (nosology → treatment → approval → reimbursement) as institutional stakeholders accumulate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(psychiatric_nosology_institutional_lock_in, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
