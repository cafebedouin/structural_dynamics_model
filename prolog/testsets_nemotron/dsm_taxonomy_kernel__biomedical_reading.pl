% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__biomedical_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Taxonomy as Objective Neurobiological Disease Map (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   The biomedical reading of the DSM taxonomy kernel asserts that diagnostic
 *   categories (e.g., major depressive disorder, schizophrenia, ADHD)
 *   correspond to discrete neurobiological disease entities discoverable
 *   through empirical research — analogous to diabetes or tuberculosis in
 *   general medicine. This reading instantiates a constraint that coordinates
 *   clinical practice, research funding, drug regulation, insurance
 *   reimbursement, and legal coercion around a shared categorical ontology.
 *   The coordination function is genuine: a common language enables
 *   interoperability across fragmented systems. The extraction function is
 *   equally real: the categories authorize involuntary treatment, create
 *   pharmaceutical markets worth hundreds of billions, and legitimize
 *   institutional control over disfavored populations. The constraint is a
 *   tangled rope because both functions are structurally necessary to its
 *   persistence — the coordination infrastructure IS the extraction
 *   infrastructure. Beneficiaries (psychiatric establishment, pharma,
 *   conformity institutions) control the diagnostic revision process; victims
 *   (diagnosed populations subject to coercion) have no exit. The
 *   claimed_type is tangled_rope from the analytical seat; the engine will
 *   compute per-seat classifications from the structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.65).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Taxonomy as Objective Neurobiological Disease Map (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '80449d33-67a0-4f93-ad4c-d3043619b3c8').
narrative_ontology:cs_kernel_codification('80449d33-67a0-4f93-ad4c-d3043619b3c8', formalized).
narrative_ontology:cs_authority_grounding('80449d33-67a0-4f93-ad4c-d3043619b3c8', extraction).
narrative_ontology:cs_interpretation_layer_present('80449d33-67a0-4f93-ad4c-d3043619b3c8').
narrative_ontology:cs_reading_relation('80449d33-67a0-4f93-ad4c-d3043619b3c8', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_reading_relation('80449d33-67a0-4f93-ad4c-d3043619b3c8', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('80449d33-67a0-4f93-ad4c-d3043619b3c8', foundational, dsm_categories_are_natural_kinds).
narrative_ontology:cs_axiom_status(dsm_categories_are_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('80449d33-67a0-4f93-ad4c-d3043619b3c8', dsm_categories_are_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('80449d33-67a0-4f93-ad4c-d3043619b3c8', secondary, diagnostic_validity_requires_biological_markers).
narrative_ontology:cs_axiom_status(diagnostic_validity_requires_biological_markers, holdable).
narrative_ontology:cs_axiom_grounding('80449d33-67a0-4f93-ad4c-d3043619b3c8', diagnostic_validity_requires_biological_markers, empirically_contingent).
narrative_ontology:cs_reference_frame('80449d33-67a0-4f93-ad4c-d3043619b3c8', kraepelinian_disease_ontology).
narrative_ontology:cs_drift_state('80449d33-67a0-4f93-ad4c-d3043619b3c8', post_rdo_c_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('80449d33-67a0-4f93-ad4c-d3043619b3c8', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, insurance_reimbursement_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, legal_competence_gatekeepers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, people_meeting_diagnostic_thresholds).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, children_in_foster_care_psychiatrized).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, prisoners_subjected_to_forced_medication).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, immigrants_subject_to_psychiatric_screening).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, psychiatric_diagnoses_are_natural_kinds).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, psychotropic_drugs_correct_underlying_pathophysiology).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, diagnostic_reliability_entails_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls diagnostic criteria through DSM revision processes, accreditation of training programs, and editorial gatekeeping of major journals. Derives professional authority, insurance reimbursement codes, and legal expert-witness status from the claim that categories map to neurobiological entities. Can pivot to dimensional models (RDoC, HiTOP) while retaining institutional control.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Uses DSM categories as regulatory gateways for drug approval (FDA requires DSM indication), marketing authorization, and insurance formulary placement. Funds key opinion leaders, CME, and patient advocacy groups that reinforce category validity. Would lose blockbuster indication monopolies if categories dissolved but retains R&D pipelines and direct-to-consumer channels.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    powerful, biographical, mobile, global).

% Schools, prisons, child welfare systems, immigration courts, and employers use DSM diagnoses to authorize containment, medication, special education placement, disability determination, and fitness-for-duty evaluations. The diagnostic label is the administrative key that unlocks coercive power. These institutions would resist decategorization because it removes their legitimating framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, agenda_setter).

% Public and private payers require DSM codes for claims payment. The categorical system enables automated adjudication, utilization review, and cost containment. Moving to dimensional or functional assessment would increase administrative complexity and reduce denial automation. They benefit from the current system's rigidity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, insurance_reimbursement_systems, beneficiary,
    institutional, biographical, constrained, national).

% Courts, guardianship boards, and competency evaluators treat DSM diagnoses as presumptive evidence of incapacity. The categorical framework provides bright-line rules for legal determinations that would otherwise require individualized functional assessment. They benefit from the epistemic shortcut.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, legal_competence_gatekeepers, beneficiary,
    powerful, biographical, constrained, national).

% Receive diagnoses that determine access to care, disability benefits, and legal protections but also authorize involuntary treatment, loss of parental rights, firearm prohibitions, and immigration bars. Cannot opt out of the diagnostic system when institutions impose it. Some gain needed services; others lose liberty. Exit requires rejecting the diagnostic framework entirely — socially and clinically costly.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, people_meeting_diagnostic_thresholds, payer,
    moderate, biographical, constrained, global).

% Detained and medicated solely on the basis of DSM criteria applied by clinicians with institutional authority. No meaningful exit: release requires accepting the diagnostic framework. The category IS the legal warrant for confinement. Experience the constraint as pure extraction — their autonomy is the resource transferred.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_patients, payer,
    powerless, immediate, trapped, local).

% Diagnosed at rates 3-4x general population; medicated with antipsychotics for behavioral control rather than psychosis. The DSM category legitimates pharmacological management of trauma reactions. No capacity to refuse; child welfare system controls their care. Life-long consequences for identity, insurance, and legal status.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, children_in_foster_care_psychiatrized, payer,
    powerless, biographical, trapped, national).

% Diagnosed with DSM categories (often antisocial personality disorder, schizophrenia) to authorize involuntary medication for institutional manageability. The diagnostic threshold is lowered in correctional settings. No exit: refusal extends solitary confinement. The category functions as a chemical restraint warrant.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, prisoners_subjected_to_forced_medication, payer,
    powerless, biographical, trapped, national).

% Screened for DSM categories at borders and in detention; diagnoses trigger mandatory detention, deportation bars, or forced treatment. Cultural expressions of distress are misread as pathology. No exit from the screening regime; the diagnostic framework is the border enforcement tool.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, immigrants_subject_to_psychiatric_screening, payer,
    powerless, biographical, trapped, global).

% Publish structural critiques of DSM validity, pharmaceutical influence, and diagnostic inflation. Systematically excluded from DSM workgroups, major journal editorial boards, and guideline committees. Their exclusion is what maintains the consensus that categories are neurobiologically validated.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_scholars, excluded,
    organized, generational, mobile, global).

% Organize against pathologization of autism, ADHD, and other neurological variations. Demand removal of categories that function as conformity enforcement. Excluded from diagnostic revision processes because their framing (difference not disease) contradicts the biomedical premise. Some achieve accommodation victories; the categorical structure remains.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% Traces the constraint's operation across epistemic, economic, and coercive dimensions. Sees the coordination function (shared clinical language, research aggregation, insurance interoperability) and the extraction function (market creation, coercive authorization, professional monopoly) as structurally intertwined. The constraint persists because both functions are real and the beneficiaries of extraction control the coordination infrastructure.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared categorical language enabling clinical communication, research aggregation, insurance reimbursement, legal adjudication, and regulatory drug approval across disparate institutions and jurisdictions. Solves the coordination problem of how to talk about distress and deviance in standardized, actionable terms.
% TRANSFER_FUNCTION: Moves clinical authority, pharmaceutical markets, insurance reimbursement streams, legal coercion warrants, and professional monopoly rents from diagnosed populations and payers to the psychiatric establishment, pharmaceutical industry, and conformity-enforcing institutions. The diagnostic category is the transfer instrument — it converts human variation into billable, regulatable, detainable units.
% ABSENT_VOICES: People diagnosed in childhood who grow up to reject their labels; cultural healers and indigenous knowledge-keepers whose frameworks were displaced; former patients who experienced diagnosis as iatrogenic harm; historians of psychiatry who trace category construction. They are absent because the diagnostic system's epistemic authority depends on their exclusion — their testimony would falsify the natural-kind claim.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished overnight, the entire edifice of psychiatric reimbursement, involuntary commitment statutes, disability determination, special education law, immigration psychiatric screening, and pharmaceutical indication regulation would lose its operational foundation. Clinicians would need new languages; courts would need new standards; pharma would need new approval pathways; schools would need new accommodation frameworks. The world would rearrange chaotically — proof that arrangements depend on the constraint.
% FOUNDING_PROBLEM: Late 19th/early 20th century psychiatry faced a legitimacy crisis: no reliable diagnostic language, no scientific credibility, no place in medical education, no insurance recognition, no legal standing. The categorical disease model (Kraepelin, then DSM-III) solved this by mimicking the rest of medicine — giving psychiatry the epistemic form of a natural science.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the founding problem is live: 'we still need reliable diagnosis for clinical care and research.' Critical psychiatry scholars (Moncrieff, Double, Timimi) and historians (Shorter, Scull, Kirk & Kutchins) attest the founding problem was institutional legitimacy, not scientific discovery — the disease model was adopted because it worked politically, not because it was validated. The NIMH's own RDoC initiative (Insel 2013) implicitly corroborates by declaring DSM categories 'lack validity' for research. Corroboration from outside the beneficiary set exists and contradicts the establishment's self-account.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__biomedical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint enables massive resource transfers (pharma revenue, insurance payments, disability budgets) and liberty deprivations (involuntary commitment, forced medication, guardianship) that flow from diagnosed populations to beneficiary institutions. The marginal cost of diagnosis is near zero; the transferred value is enormous. Suppression (0.65) is substantial but not total: critical psychiatry and neurodiversity movements exist and publish, but are excluded from the authoritative revision process and guideline committees. Theater ratio (0.25) is moderate-low: the scientific rhetoric (biomarkers coming soon, RDoC will validate) performs a legitimating function but the core coordination (billing codes, legal warrants) is genuinely functional. Accessibility collapse (0.55) is partial — alternatives exist (formulation-based approaches, Open Dialogue, neurodiversity frameworks) but are marginalized by the institutional infrastructure. Resistance (0.58) is significant — survivor movements, critical psychiatry, neurodiversity advocacy — but has not displaced the constraint because beneficiaries control the coordination levers.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment's seat, the constraint is a rope: genuine coordination with minimal coercion (they see the scientific aspiration, not the coercive implementation). From involuntarily committed patients' seat, it is a snare: pure extraction with no coordination benefit (the category is the warrant for their confinement). From neurodiversity advocates' seat, it is a snare wearing a rope's clothes: the coordination story is cover for conformity enforcement. The engine computes this divergence from the structural data — power, exit_options, and role declarations drive the per-seat directionality that yields different effective extraction values and thus different computed types.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment (agenda_setter, institutional, arbitrage exit) sits at the beneficiary end: it controls the constraint and can pivot to dimensional models without losing authority. Pharma (beneficiary, powerful, mobile) benefits enormously but could adapt to new indication frameworks. Conformity institutions (beneficiary/agenda_setter, institutional, arbitrage) depend on the categorical form for legal-administrative operations — they would resist decategorization most fiercely. Diagnosed populations (payers, powerless to moderate, trapped to constrained) bear extraction with minimal exit: involuntary patients are trapped; community-diagnosed people are constrained by insurance/legal systems that require the code. Critical psychiatry and neurodiversity advocates (excluded, organized, mobile/constrained) are kept out of the revision room — their exclusion is the suppression mechanism. The analytical observer sees the full structure without bearing its costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (psychiatry's legitimacy crisis) was real but has been substantially solved — psychiatry is now a medical specialty with insurance recognition, legal standing, and research funding. Yet the categorical disease model persists and expands (DSM-III 265 categories → DSM-5-TR 541 categories). The mandate has atrophied: the coordination function no longer requires the disease ontology (RDoC proves dimensional approaches work for research; HiTOP proves transdiagnostic approaches work for clinical utility). But the extraction function requires the disease ontology — pharma needs categories for FDA indications; courts need categories for bright-line rulings; schools need categories for IEP eligibility. The constraint persists because the beneficiaries of extraction control the coordination infrastructure. This is not a piton (the function hasn't atrophied — it has been captured); it is a tangled rope where the coordination and extraction functions have fused and the extraction function now drives the coordination function's evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_validation_trajectory,
    'Will empirically validated neurobiological biomarkers for DSM categories emerge at scale, retroactively justifying the disease ontology, or will the biomarker search continue to fail as it has for 40 years?',
    'Longitudinal tracking of NIMH RDoC biomarker yields, FDA biomarker qualification decisions, and large-scale consortium genetics/neuroimaging studies (ENIGMA, ABCD, UK Biobank). If biomarkers with clinical utility (PPV > 0.8 for treatment response) emerge for >50% of major categories within 15 years, the biomedical reading''s natural-kind claim gains empirical support.',
    'If biomarkers validate the categories, the constraint shifts toward mountain (natural law) or rope (genuine coordination on discovered structure). If biomarkers fail, the constraint''s extractive character is confirmed and the tangled_rope classification hardens toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biomarker_validation_trajectory, empirical, 'Whether the disease ontology will be empirically vindicated or remains a promissory note.').

omega_variable(
    coordination_extraction_separability,
    'Can the DSM''s coordination function (shared clinical language, research aggregation, billing interoperability) be preserved while decoupling it from the disease ontology that enables extraction (pharma indications, involuntary commitment warrants, disability gatekeeping)?',
    'Natural experiments from jurisdictions implementing ICD-11 dimensional approaches, Open Dialogue services, or neurodiversity-affirming frameworks. If clinical communication, research, and reimbursement function without categorical disease labels, the functions are separable and the extraction is gratuitous.',
    'If separable, the biomedical reading''s claimed coordination justification is falsified — the disease ontology is maintained FOR its extraction function. If inseparable, some extraction is the price of coordination and the tangled_rope classification is structurally necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s two functions are structurally separable or fused.').

omega_variable(
    reading_framing_underdetermination,
    'Does the biomedical reading''s framing (categories as discovered natural kinds) represent the only coherent interpretation of DSM''s epistemic structure, or does the critical psychiatry framing (categories as constructed market infrastructure) fit the same empirical record equally well?',
    'Comparative analysis of predictive success: does the natural-kind framing or the market-construction framing better predict DSM revision trajectories, pharma indication patterns, and category expansion/contraction dynamics? Historical tracing of category construction (e.g., ADHD, PTSD, Bipolar II, ASD) against pharmaceutical patent timelines.',
    'If the critical psychiatry framing has superior explanatory power, the biomedical reading is a self-serving ideology rather than a scientific conclusion — the constraint''s claimed_type (tangled_rope per this reading) masks a snare. If the biomedical framing uniquely predicts empirical outcomes, its claimed_type is more warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the kernel admits multiple equally coherent readings or one reading is structurally privileged.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of critical psychiatry and neurodiversity voices structural (editorial gatekeeping, funding exclusion, credentialing barriers) or internalized (clinicians genuinely believe the disease ontology, patients internalize diagnostic identities)?',
    'Post-exit trajectory studies: do clinicians who leave institutional psychiatry retain the disease ontology? Do diagnosed people who encounter neurodiversity frameworks experience reduced self-stigma? If suppression persists after structural barriers are removed, internalized component is significant.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This would increase effective extraction for identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the psychiatric epistemology constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(dsm__tr_t1987, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2013, 0.24).
narrative_ontology:measurement(dsm__tr_t2022, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2022, 0.25).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(dsm__be_t1987, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1987, 0.58).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.65).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2013, 0.75).
narrative_ontology:measurement(dsm__be_t2022, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2022, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(dsm__su_t1987, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1987, 0.5).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2013, 0.62).
narrative_ontology:measurement(dsm__su_t2022, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2022, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.1).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychiatric_involuntary_commitment_statutes).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_indication_regulatory_framework).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, special_education_idea_eligibility_categories).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, social_security_disability_mental_listings).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dsm_taxonomy_kernel. The biomedical_reading claims categories map to neurobiological entities (high extractiveness, tangled_rope). The critical_psychiatry_reading claims categories are reverse-engineered from pharma treatments (very high extractiveness, snare). The neurodiversity_reading claims categories pathologize natural variation (moderate extractiveness, tangled_rope with different victim set). All three share the DSM taxonomic infrastructure but disagree on its ontology, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.15).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, powerful, 0.25).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, powerless, 0.92).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, moderate, 0.7).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
