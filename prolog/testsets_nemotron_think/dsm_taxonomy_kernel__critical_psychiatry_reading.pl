% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__critical_psychiatry_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The DSM (Diagnostic and Statistical Manual of Mental Disorders) functions
 *   as the authoritative diagnostic taxonomy for psychiatry in the US and
 *   globally. The critical psychiatry reading holds that DSM categories are
 *   not discovered neurobiological entities but are reverse-engineered from
 *   available psychotropic drugs to create billable, marketable diagnostic
 *   bins. This reading sees the DSM-III (1980) 'atheoretical' operational
 *   criteria turn as a strategic accommodation to pharmaceutical industry
 *   needs for standardized trial populations, and subsequent revisions
 *   (DSM-IV, DSM-5, DSM-5-TR) as diagnostic inflation tracking patent
 *   lifecycles and market expansion. The coordination function — a shared
 *   diagnostic language for clinical communication, insurance reimbursement,
 *   and research — is real but subordinate to the extraction function:
 *   pharmaceutical profit via expanded indication, polypharmacy, and lifelong
 *   medication maintenance. Beneficiaries are pharmaceutical capital (direct
 *   profit), industry-funded psychiatrists (KOL payments, research funding,
 *   prestige), and insurance industry (standardized coding for utilization
 *   control). Victims are patients subjected to overprescription, adverse
 *   drug effects, misdiagnosis, and identity capture by diagnostic labels.
 *   The constraint is actively enforced via insurance reimbursement rules,
 *   licensing examinations, hospital privileging, disability determination,
 *   and legal standards of care.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.65).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, '988774c9-6ad5-4a05-aca4-b1bfc98a8e17').
narrative_ontology:cs_kernel_codification('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', formalized).
narrative_ontology:cs_authority_grounding('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', extraction).
narrative_ontology:cs_interpretation_layer_present('988774c9-6ad5-4a05-aca4-b1bfc98a8e17').
narrative_ontology:cs_reading_relation('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', foundational, psychiatric_diagnoses_are_pharma_market_constructs).
narrative_ontology:cs_axiom_status(psychiatric_diagnoses_are_pharma_market_constructs, holdable).
narrative_ontology:cs_axiom_grounding('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', psychiatric_diagnoses_are_pharma_market_constructs, empirically_contingent).
narrative_ontology:cs_axiom('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', secondary, diagnostic_expansion_tracks_patent_lifecycles).
narrative_ontology:cs_axiom_status(diagnostic_expansion_tracks_patent_lifecycles, holdable).
narrative_ontology:cs_axiom_grounding('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', diagnostic_expansion_tracks_patent_lifecycles, empirically_contingent).
narrative_ontology:cs_reference_frame('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', critical_psychiatry_framework).
narrative_ontology:cs_drift_state('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', post_dsm5_pharma_scandals, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('988774c9-6ad5-4a05-aca4-b1bfc98a8e17', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_industry).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, adverse_effect_victims).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, misdiagnosed_populations).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_influence_on_diagnostic_criteria).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, diagnostic_inflation_tracks_market_opportunity).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_categories_serve_commercial_over_clinical_ends).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major psychotropic manufacturers (antidepressants, antipsychotics, stimulants, mood stabilizers) depend on DSM-coded indications for FDA approval, insurance reimbursement, and marketing. They fund key opinion leaders, DSM workgroup members, clinical trials, and patient advocacy groups. Diagnostic expansion directly increases addressable market. They can shift pipelines across diagnoses and jurisdictions.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Academic psychiatrists who receive speaking fees, consulting contracts, research grants, and trial leadership roles from pharma. They dominate DSM workgroups, guideline committees, and residency training. Their careers and professional status are fused to the DSM framework. Exit means losing industry income, leadership positions, and professional identity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__critical_psychiatry_reading, industry_funded_psychiatrists, agenda_setter).

% Insurers require DSM codes for reimbursement, utilization review, and network credentialing. They gain administrative standardization and cost-control leverage but bear premium inflation from diagnostic expansion and polypharmacy. They could adopt alternative coding (ICD-only, dimensional measures) but are locked into DSM by provider contracts and regulatory expectation.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Patients receiving multiple psychotropic medications for DSM diagnoses that may not fit their presentation (e.g., antidepressant + antipsychotic + stimulant for 'treatment-resistant depression' that may be bipolar, trauma, or life circumstance). They bear metabolic syndrome, sexual dysfunction, cognitive blunting, withdrawal syndromes. Exit requires finding a non-DSM clinician, paying out-of-pocket, and navigating disability/school systems without a code.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, overprescribed_patients, payer,
    powerless, biographical, constrained, global).

% Patients with tardive dyskinesia, PSSD (post-SSRI sexual dysfunction), akathisia, metabolic devastation, or benzodiazepine withdrawal — harms directly caused by DSM-indicated prescribing. The DSM framework treats these as 'side effects' of necessary treatment rather than iatrogenic injury from diagnostic overreach. They are often disbelieved by clinicians trained in the DSM paradigm.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, adverse_effect_victims, payer,
    powerless, biographical, trapped, global).

% Groups systematically misdiagnosed by DSM categories: women with trauma diagnosed as borderline personality; Black men diagnosed with schizophrenia instead of mood disorder; children with developmental trauma diagnosed as ADHD/bipolar; grief diagnosed as depression. The diagnostic label gates resources but distorts understanding. Exit means rejecting the label and losing access to disability, accommodations, or specialized care.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, misdiagnosed_populations, payer,
    moderate, generational, constrained, global).

% Psychiatrists and psychologists (e.g., Critical Psychiatry Network, Mad in America affiliates) who reject the biomedical model and DSM's pharma entanglement. They practice formulation-based, trauma-informed, or psychosocial approaches. They are marginalized in academic departments, excluded from guideline committees, and face licensing risk for 'non-standard' care. Their exit from the DSM paradigm is professional exile.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatrists, observer,
    moderate, biographical, constrained, national).

% Autistic, ADHD, and other neurodivergent self-advocates who reject pathologization of their neurotype. They argue DSM categories medicalize natural variation to enforce behavioral conformity. They are excluded from DSM revision processes, insurance policy tables, and clinical guideline development. Their exit is structural — the constraint defines them as lacking insight.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% FDA, EMA, NICE, and similar bodies that approve drugs based on DSM-defined indications. They have begun requiring functional impairment measures and patient-reported outcomes but still anchor to DSM categories. They could mandate dimensional or mechanism-based approval pathways but are constrained by statutory frameworks and industry capture.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, regulatory_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic language enabling clinical communication, insurance reimbursement, disability determination, legal standards of care, and research standardization across psychiatry and adjacent fields.
% TRANSFER_FUNCTION: Moves pharmaceutical profit (via expanded indications, polypharmacy, lifelong maintenance) from patients and payers (insurance, public health systems, out-of-pocket) to pharmaceutical capital and industry-affiliated psychiatrists, using DSM diagnostic authority as the extraction mechanism.
% ABSENT_VOICES: Patients harmed by polypharmacy and diagnostic mislabeling who cannot access non-DSM care; neurodivergent people who reject pathologization but need diagnostic codes for accommodations; clinicians in the Global South whose practice is shaped by DSM-exported guidelines without local validation; low-income patients for whom diagnostic labels are the only gateway to survival resources.
% DISAPPEARANCE_RATIONALE: If DSM diagnostic authority vanished overnight, insurance reimbursement would collapse without ICD-only coding, disability systems would lose gatekeeping criteria, pharmaceutical marketing would lose indication anchors, and clinical training would lose its curricular spine. The psychiatric field would reorganize around dimensional frameworks (HiTOP, RDoC), formulation-based practice, or ICD-11 — a genuine rearrangement, not business as usual.
% FOUNDING_PROBLEM: Pre-DSM-III psychiatry suffered catastrophic diagnostic unreliability: clinicians could not agree on diagnoses (kappa ~0.2), blocking clinical research, treatment development, and insurance reimbursement. The 1980 operational criteria turn solved this by replacing theoretical inference with observable symptom checklists.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (diagnostic unreliability) is attested as solved by the biomedical establishment itself — DSM-5 field trials showed improved reliability for some categories. However, critical psychiatrists (Moncrieff, Whitaker, Frances, double-blind placebo researchers) and NIMH RDoC architects attest that reliability was achieved at the cost of validity, and the arrangement now persists for commercial not clinical reasons. No party outside the pharmaceutical-beneficiary set attests that diagnostic inflation serves the original reliability problem.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__critical_psychiatry_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is moderate-high: the pharmaceutical revenue stream dependent on DSM-coded indications is enormous (hundreds of billions annually), and the diagnostic expansion (e.g., pediatric bipolar, adult ADHD, binge eating disorder, prolonged grief) tracks market opportunity more than epidemiological shift. Suppression (0.65) is substantial: alternatives (HiTOP, PDM, formulation-based approaches, neurodiversity frameworks) are marginalized through insurance non-reimbursement, licensing exam alignment, and institutional gatekeeping. Theater ratio (0.45) reflects genuine coordination value (inter-rater reliability, research standardization) increasingly overwhelmed by performative maintenance of categories that serve commercial ends. Accessibility collapse (0.60): alternatives exist but require exiting insurance-based practice, forgoing hospital privileges, or accepting professional marginalization. Resistance (0.55): critical psychiatry movement, patient advocacy, some NIMH RDoC pushback, but the constraint's institutional embeddedness is deep. Claimed type is tangled_rope: real coordination function (shared diagnostic language) coexists with asymmetric extraction (pharma profit via diagnostic expansion) requiring active enforcement (insurance/licensing/legal).
 *
 * PERSPECTIVAL GAP:
 *   From the biomedical reading's seat, the DSM is a progressive scientific achievement (mountain/rope). From the critical psychiatry seat, it is a market-making device (tangled_rope/snare). From the neurodiversity seat, it is a pathologization engine (snare). The same constraint computes to different types per seat because directionality differs: beneficiaries experience coordination; victims experience extraction; excluded voices experience erasure. The engine computes this divergence from the beneficiary/victim declarations and exit structures authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical capital and industry-funded psychiatrists are structural beneficiaries (d near 0.0-0.2): they collect rents from the constraint's operation. Insurance industry is a secondary beneficiary (d ~0.3): gains administrative standardization but bears cost inflation. Overprescribed patients, adverse effect victims, and misdiagnosed populations are structural targets (d near 0.8-1.0): they bear extraction via polypharmacy, iatrogenic harm, and identity foreclosure. Exit is constrained — patients cannot easily exit the diagnostic system when it governs insurance, disability, legal, and school accommodations. Clinicians who reject DSM face professional marginalization (constrained exit). The engine will compute per-seat χ from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The DSM's founding problem (DSM-III, 1980) was diagnostic unreliability — clinicians couldn't agree on diagnoses, blocking research and treatment. The operational criteria solution solved this coordination problem. By DSM-5 (2013), the founding problem (reliability) was substantially solved, but the arrangement persists and expands — diagnostic inflation serves commercial ends, not clinical validity. The mandate has atrophied: the coordination function is mature, but the extraction function drives continued revision. This is classic mandatrophy — the constraint outlives its founding justification. The critical reading exposes this; the biomedical reading denies it; the neurodiversity reading shows its human cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharma_influence_vs_scientific_progress,
    'To what extent do DSM revisions reflect genuine empirical advances versus pharmaceutical market-shaping?',
    'Historical analysis of DSM revision processes cross-referenced with pharmaceutical patent pipelines, industry funding of key opinion leaders, and clinical trial landscapes at each revision cycle.',
    'If revisions track pharma pipelines, the constraint is extractive (tangled_rope/snare); if they track independent replication, the biomedical reading gains ground and extraction is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharma_influence_vs_scientific_progress, empirical, 'Whether DSM taxonomy evolution is driven by science or market engineering.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of diagnostic alternatives structural (insurance reimbursement, licensing, legal standards) or internalized (clinician training, patient self-pathologization, professional identity fusion)?',
    'Post-exit suppression trajectory: track clinicians who adopt alternative frameworks (e.g., HiTOP, PDM, formulation-based practice) — if suppression persists after leaving institutional settings, internalized component is confirmed.',
    'If internalized, effective suppression is higher than structural measures suggest; the constraint travels with agents after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in psychiatric diagnostic enforcement.').

omega_variable(
    kernel_framing_contestation,
    'Is the DSM kernel a genuine medical taxonomy that has been captured, or was it constructed from inception as a pharmaceutical market-making device?',
    'Genealogical analysis of DSM-III origins (1980): whether the operational criteria / atheoretical framing was a sincere methodological move or a strategic accommodation to pharma''s need for trial-ready diagnostic bins.',
    'If constructed for markets from inception, the constraint is a snare with coordination as cover; if captured later, it is a degraded rope/tangled_rope with mandatrophy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_contestation, conceptual, 'Origin intent vs. capture trajectory of the DSM kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 42).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_crit_tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm_crit_tr_t7, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 7, 0.3).
narrative_ontology:measurement(dsm_crit_tr_t14, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 14, 0.35).
narrative_ontology:measurement(dsm_crit_tr_t21, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 21, 0.38).
narrative_ontology:measurement(dsm_crit_tr_t28, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement(dsm_crit_tr_t35, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 35, 0.43).
narrative_ontology:measurement(dsm_crit_tr_t42, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 42, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm_crit_be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm_crit_be_t7, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(dsm_crit_be_t14, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(dsm_crit_be_t21, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(dsm_crit_be_t28, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(dsm_crit_be_t35, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(dsm_crit_be_t42, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 42, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm_crit_su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm_crit_su_t7, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(dsm_crit_su_t14, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 14, 0.53).
narrative_ontology:measurement(dsm_crit_su_t21, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement(dsm_crit_su_t28, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 28, 0.61).
narrative_ontology:measurement(dsm_crit_su_t35, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 35, 0.63).
narrative_ontology:measurement(dsm_crit_su_t42, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 42, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, information_standard).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.02).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_marketing_practices).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, insurance_reimbursement_structures).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatric_training_standards).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, disability_determination_criteria).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% DSM taxonomy kernel decomposes into three readings: biomedical (mountain/rope claim, low ε), critical psychiatry (tangled_rope, moderate-high ε, pharma beneficiaries, patient victims), neurodiversity (snare, high ε, institutional beneficiaries, neurodivergent victims). The critical reading influences the biomedical reading by exposing its commercial entanglement, and influences the neurodiversity reading by supplying the political-economic mechanism for pathologization. All three coexist as live positions in psychiatric discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__critical_psychiatry_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
