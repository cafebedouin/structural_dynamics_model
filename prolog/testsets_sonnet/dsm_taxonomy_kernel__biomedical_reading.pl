% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This story instantiates the biomedical reading of the DSM taxonomy
 *   kernel: the claim that DSM categories map to objective, discoverable
 *   neurobiological disease entities. Under this reading, diagnostic criteria
 *   are treated as provisional descriptions of underlying biological kinds
 *   awaiting biomarker confirmation, and the ongoing absence of validated
 *   biomarkers for most categories is read as a temporary gap in research
 *   rather than evidence against the categories' natural-kind status. This
 *   framing is what licenses involuntary commitment, mandated pharmaceutical
 *   treatment, and legal incapacity determinations on diagnostic grounds —
 *   the categories are treated with the evidentiary weight of discovered
 *   disease, not negotiated convention. Two sibling readings of the same DSM
 *   kernel are generated as separate constraints: the
 *   critical_psychiatry_reading (categories reverse-engineered from available
 *   drug treatments) and the neurodiversity_reading (categories pathologizing
 *   natural variation). Each sibling has its own epsilon, its own
 *   beneficiary/victim structure, and its own classification; they are linked
 *   here only as network edges, not folded into this constraint's metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.71).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.66).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '38e2bead-a1b1-4b3a-bce3-c83c4a47df92').
narrative_ontology:cs_kernel_codification('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', formalized).
narrative_ontology:cs_authority_grounding('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', expertise).
narrative_ontology:cs_interpretation_layer_present('38e2bead-a1b1-4b3a-bce3-c83c4a47df92').
narrative_ontology:cs_reading_relation('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', dsm_taxonomy_kernel__critical_psychiatry_reading, forecloses).
narrative_ontology:cs_reading_relation('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', foundational, dsm_categories_are_discovered_natural_kinds).
narrative_ontology:cs_axiom_status(dsm_categories_are_discovered_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', dsm_categories_are_discovered_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', secondary, diagnostic_labels_warrant_involuntary_intervention).
narrative_ontology:cs_axiom_status(diagnostic_labels_warrant_involuntary_intervention, holdable).
narrative_ontology:cs_axiom_grounding('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', diagnostic_labels_warrant_involuntary_intervention, instrumental).
narrative_ontology:cs_reference_frame('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', discovered_biological_taxonomy).
narrative_ontology:cs_drift_state('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', post_rdoc_launch_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38e2bead-a1b1-4b3a-bce3-c83c4a47df92', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, insurance_reimbursement_systems).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_threshold).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, parents_seeking_care_for_children).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, disability_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, parents_seeking_care_for_children).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_research_program).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, diagnostic_reliability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the DSM through APA committee structures, trains clinicians on its categories, and testifies in courts and legislatures that the categories track real disease entities. Its professional authority, licensing structures, and research funding streams are built on the categories being treated as biologically real rather than provisional or consensus-based.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Designs, markets, and prices psychotropic drugs against DSM diagnostic categories; funds significant psychiatric research and continuing-education content; benefits directly when a category is treated as a fixed neurobiological target requiring lifelong pharmaceutical management rather than a contested or situational label.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Schools, employers, courts, and the military use DSM diagnoses to sort, exclude, mandate treatment for, or excuse individuals from ordinary expectations. The biomedical framing gives these institutions a neutral-seeming scientific warrant for behavioral management decisions they would otherwise have to justify on contested normative grounds.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, generational, arbitrage, national).

% Receive a diagnosis presented as a discovered biological fact about them rather than a clinical judgment call; this framing follows them into employment records, insurance files, custody proceedings, and self-understanding. Exiting the diagnostic label once assigned is structurally difficult even where clinical judgment later changes.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_threshold, payer,
    powerless, biographical, trapped, national).

% Can be detained, medicated against their will, and stripped of legal decision-making capacity on the basis of a DSM category presented in commitment hearings as objective disease rather than contested clinical opinion. Have essentially no exit once the label is invoked in a legal proceeding.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_patients, payer,
    powerless, immediate, trapped, regional).

% Navigate school and insurance systems that require a DSM diagnosis to unlock accommodations, therapy coverage, or medication access for their children. Benefit from the diagnostic category as a practical gateway to services while also bearing the cost of a label their child may carry for life, sometimes regardless of later developmental change.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, parents_seeking_care_for_children, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, parents_seeking_care_for_children, beneficiary).

% Must obtain and defend a DSM diagnosis framed as objective neurobiological fact to qualify for disability benefits, accommodations, or legal protections, and are vulnerable to having that same framing used to deny claims when symptom presentation does not match the expected biological course.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, disability_claimants, payer,
    powerless, biographical, constrained, national).

% Build careers, grants, and journals on the premise that DSM categories correspond to discoverable neurobiological entities; the premise is a precondition for the research program's funding and prestige, independent of whether biomarker validation has succeeded for the categories in question.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, agenda_setter).

% Publish evidence that DSM categories lack the biomarker validation, reliability, and construct validity claimed for them, and argue the categories function as administrative and commercial conveniences rather than discovered diseases. Marginalized within mainstream psychiatric institutions, journals, and funding panels that operate on the biomedical premise.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, global).

% Argue that categories such as autism or ADHD describe natural variation in cognition rather than disease entities, and are largely absent from DSM revision committees despite bearing the direct consequences of category boundaries and thresholds.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodivergent_advocacy_groups, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides clinicians, insurers, courts, and researchers a shared vocabulary for identifying and communicating about patterns of distress and impairment, enabling billing, treatment planning, and cross-study comparison that would otherwise be idiosyncratic to each clinician.
% TRANSFER_FUNCTION: Moves diagnostic authority, treatment revenue, legal capacity determinations, and institutional gatekeeping power from individuals labeled by the categories to the professional, pharmaceutical, and institutional bodies that administer and interpret those categories — under the premise that the categories are discovered facts rather than negotiated conventions.
% ABSENT_VOICES: Critical psychiatry researchers and neurodivergent advocacy groups are structurally underrepresented on DSM revision task forces relative to biologically-oriented psychiatrists and researchers with pharmaceutical industry funding ties; patients diagnosed under contested categories rarely have a formal voice in criteria-setting at all.
% DISAPPEARANCE_RATIONALE: If the biomedical reading of DSM categories were abandoned overnight in favor of an explicitly provisional, consensus-based framing, involuntary commitment standards, insurance reimbursement codes, disability determinations, and pharmaceutical marketing claims tied to 'treating a disease' would all require re-justification on different (and more contestable) normative grounds; large portions of psychiatric research funding structured around biomarker discovery would need to be reframed or defunded.
% FOUNDING_PROBLEM: Clinicians in the mid-20th century faced wildly inconsistent diagnostic practices across institutions and even within the same institution over time; the DSM was built to standardize criteria so clinicians, researchers, and payers could reliably refer to 'the same thing' when discussing a patient's presentation.
% FOUNDING_PROBLEM_CORROBORATION: The APA and biologically-oriented psychiatric researchers attest the categories increasingly correspond to real neurobiological substrates as neuroscience advances. Independent sources outside the psychiatric establishment — including the NIMH's own 2013 statement (in launching RDoC) that DSM categories lack sufficient validity as biological targets, and decades of inter-rater reliability and biomarker-validation literature from psychology and philosophy of science — corroborate that the founding standardization problem was substantially solved by DSM-III's operationalized criteria, but the further claim of discovered biological entities remains empirically unestablished outside the benefiting professional and commercial parties.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71) because the biomedical framing is the operative premise behind involuntary treatment orders, mandated medication, disability gatekeeping, and legal capacity stripping — real, high-stakes transfers of autonomy and resources justified by treating a clinical judgment as a discovered fact. Suppression (0.66) reflects that alternatives to the biomedical framing (RDoC-style dimensional models, critical psychiatry's construct-validity critiques, neurodiversity framings) are actively marginalized within licensing boards, insurance panels, and legal standards of evidence, not merely academically contested. Theater ratio (0.42) captures that a substantial share of 'biological psychiatry' activity — biomarker-search grant proposals, DSM revision committee proceedings presented as scientific consensus-building — increasingly functions to maintain the appearance of an advancing natural-kind research program despite the persistent absence of validated biomarkers for the great majority of DSM categories. Accessibility collapse (0.58) and resistance (0.55) are both moderate-high: institutional alternatives to the biomedical framing exist and are actively pursued (RDoC, critical psychiatry, neurodiversity movements) but have not displaced the dominant framing in clinical, legal, or insurance practice.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment's seat, this reading is a maturing scientific research program awaiting technological catch-up (better neuroimaging, better biomarkers). From the seat of someone involuntarily committed on the basis of a DSM diagnosis, the same category functioned as an unappealable legal fact stripping them of autonomy in a single hearing. The engine computes these as structurally different seats from the same authored data; this story does not adjudicate which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and conformity-requiring institutions sit near the beneficiary end: they collect professional authority, revenue, and administrative convenience from the categories being treated as biological fact, and hold arbitrage-grade exit (they can revise, reframe, or defend the categories as their interests require). Individuals meeting diagnostic thresholds, involuntarily committed patients, and disability claimants sit near the full-target end: they bear loss of legal capacity, forced treatment, and lifelong labeling, with trapped or constrained exit options once a diagnosis enters legal or institutional record. Parents seeking care and biological psychiatry researchers hold dual beneficiary/payer positions reflecting real but asymmetric stakes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inconsistent, unreliable clinical diagnosis across institutions — was substantially real and was substantially solved by DSM-III's operationalized criteria, which is why this constraint is not authored as a pure snare: there is a genuine coordination function (shared clinical vocabulary, comparable research, billing interoperability). What has drifted is the further, stronger claim layered onto that coordination success: that the categories are therefore discovered neurobiological disease entities rather than useful operational conventions. That further claim is where the extraction concentrates (involuntary commitment, forced medication, capacity stripping), which is why tangled_rope rather than rope or snare alone captures the structure — coordination and extraction run through the same DSM apparatus simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_validation_status,
    'Do any current DSM categories possess validated biological markers (genetic, neuroimaging, physiological) sufficient to establish them as discrete natural kinds, as opposed to clinically useful but conventionally-bounded symptom clusters?',
    'Systematic review of biomarker validation literature against NIMH RDoC criteria; tracking whether any DSM-5 category achieves the biomarker specificity and sensitivity thresholds used to validate diseases in other areas of medicine.',
    'If validated biomarkers emerge for a given category, the biomedical reading gains strong support for that category specifically. If, as the NIMH''s own RDoC initiative suggests, none currently meet this bar for the major categories, the biomedical reading''s claim of ''discoverable disease entities'' is closer to an aspirational research program mislabeled as an established fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biomarker_validation_status, empirical, 'Whether DSM categories have achieved biomarker validation as natural disease kinds.').

omega_variable(
    natural_kind_vs_constructed_kernel_ambiguity,
    'Is the DSM taxonomy kernel itself a natural-law-like discovery about human neurobiology, or a constructed administrative and commercial instrument that different parties read differently?',
    'Cross-reading comparison: track whether diagnostic category boundaries shift in ways correlated with insurance reimbursement structures and pharmaceutical patent cycles (supporting constructed reading) versus shifting only in response to new biological evidence (supporting natural-kind reading).',
    'If category boundaries track commercial and administrative pressures more than biological evidence, this reading (which requires emerges_naturally-style discovery framing to be credible) is undermined in favor of the critical_psychiatry_reading sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_kind_vs_constructed_kernel_ambiguity, conceptual, 'Whether the DSM kernel is discovered natural law or constructed administrative/commercial instrument — required because this reading claims beneficiaries exist alongside a naturalness claim.').

omega_variable(
    reliability_vs_validity_conflation,
    'Does the DSM''s genuine achievement of inter-rater reliability (clinicians agreeing on which label to apply) get conflated with construct validity (the label corresponding to a real, discrete underlying entity)?',
    'Philosophy-of-science analysis distinguishing reliability metrics (kappa statistics across DSM revisions) from validity metrics (biomarker correspondence, treatment specificity, prognostic stability); track whether professional and legal discourse treats reliability achievement as validity achievement.',
    'If reliability is systematically presented as validity in clinical, legal, and insurance contexts, that conflation is itself a mechanism by which the coordination achievement (reliability) launders the extractive claim (validity as discovered disease).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reliability_vs_validity_conflation, conceptual, 'Whether diagnostic reliability is being conflated with construct validity to support the disease-entity claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dsm__tr_t9, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement(dsm__tr_t18, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(dsm__tr_t27, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 27, 0.35).
narrative_ontology:measurement(dsm__tr_t36, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 36, 0.39).
narrative_ontology:measurement(dsm__tr_t45, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsm__be_t9, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(dsm__be_t18, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(dsm__be_t27, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 27, 0.64).
narrative_ontology:measurement(dsm__be_t36, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(dsm__be_t45, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 45, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm__su_t9, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 9, 0.48).
narrative_ontology:measurement(dsm__su_t18, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(dsm__su_t27, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 27, 0.6).
narrative_ontology:measurement(dsm__su_t36, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 36, 0.63).
narrative_ontology:measurement(dsm__su_t45, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 45, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the dsm_taxonomy_kernel. biomedical_reading claims the categories are discovered neurobiological disease entities (this file); critical_psychiatry_reading claims the categories are reverse-engineered from available pharmaceutical treatments to construct drug markets; neurodiversity_reading claims the categories pathologize natural neurological variation against institutional norms. Each reading has a distinct beneficiary/victim structure and a distinct epsilon; per the epsilon-invariance principle they are not merged into one story with a measurement parameter. The biomedical_reading's coordination-success claim (diagnostic reliability) is cited by the establishment as evidence for its validity claim, creating an influence edge toward the other two readings' contested terrain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
