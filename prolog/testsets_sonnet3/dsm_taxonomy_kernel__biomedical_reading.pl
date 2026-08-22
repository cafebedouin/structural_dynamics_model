% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This story instantiates the biomedical reading of the DSM taxonomy
 *   kernel: the claim that DSM categories map to objective neurobiological
 *   disease entities discoverable through empirical research, rather than
 *   being administrative constructs (critical psychiatry reading) or
 *   pathologized natural variation (neurodiversity reading). The biomedical
 *   reading's own metrics are authored here: after four decades of
 *   well-funded biomarker search, DSM categories remain operationalized
 *   checklist constructs without robust category-specific biological
 *   validation, while the categories continue to license involuntary
 *   commitment, forced medication, and loss of legal capacity on the premise
 *   that they identify real disease states. The reading's institutional
 *   apparatus (professional licensing, insurance reimbursement, forensic
 *   psychiatry, pharmaceutical indication) depends on treating validity as
 *   settled when it remains a live empirical question even within mainstream
 *   psychiatric epistemology.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: agenda_setter/beneficiary — writes and administers the DSM, draws authority from biological framing
 *   - pharmaceutical_industry: beneficiary — sells treatments indicated for DSM categories, funds validating research
 *   - diagnostic_threshold_patients: payer — receive a durable label presented as objective medical finding
 *   - involuntarily_committed_individuals: payer, trapped — diagnosis is the legal trigger for overriding stated wishes
 *   - critical_psychiatry_clinicians: excluded — marginalized dissent from within the profession
 *   - medical_historians_and_philosophers_of_science: observer — trace the revision history against genuine nosological discovery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c').
narrative_ontology:cs_kernel_codification('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', formalized).
narrative_ontology:cs_authority_grounding('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', expertise).
narrative_ontology:cs_interpretation_layer_present('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c').
narrative_ontology:cs_reading_relation('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', foundational, categorical_diagnoses_track_discrete_biological_kinds).
narrative_ontology:cs_axiom_status(categorical_diagnoses_track_discrete_biological_kinds, holdable).
narrative_ontology:cs_axiom_grounding('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', categorical_diagnoses_track_discrete_biological_kinds, empirically_contingent).
narrative_ontology:cs_axiom('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', secondary, psychiatric_impairment_is_ultimately_a_brain_disease_state).
narrative_ontology:cs_axiom_status(psychiatric_impairment_is_ultimately_a_brain_disease_state, holdable).
narrative_ontology:cs_axiom_grounding('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', psychiatric_impairment_is_ultimately_a_brain_disease_state, empirically_contingent).
narrative_ontology:cs_reference_frame('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', dsm_iii_operationalized_criteria_standard).
narrative_ontology:cs_drift_state('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', post_rdoc_and_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('129c3699-0fdb-4c7e-b1da-ae9cfc8f9b3c', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_patients).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, legal_capacity_stripped_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_research_program).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, categorical_diagnosis_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the DSM, trains clinicians to apply its categories as if they were discovered disease entities, and administers licensing, insurance reimbursement, and forensic evaluation systems built on those categories. Draws professional authority, research funding, and courtroom expert-witness standing from the claim that DSM categories track real neurobiological kinds.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary).

% Sells medications indicated for DSM categories; funds much of the biological psychiatry research that is then cited as evidence for the categories' objective status. Revenue depends on the categories being treated as real, treatable disease entities rather than provisional administrative constructs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Schools, employers, prisons, and the military use DSM diagnoses to sort, medicate, exclude, or mandate treatment for people whose behavior disrupts institutional order. The biomedical framing lets these institutions describe conformity enforcement as medical necessity rather than social control.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, generational, arbitrage, national).

% Receive a DSM diagnosis on meeting checklist criteria, which becomes a durable label attached to their medical, employment, immigration, and legal records. Cannot easily contest the diagnosis's validity because the biomedical framing presents it as an objective finding rather than a clinical judgment call.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_patients, payer,
    powerless, biographical, constrained, national).

% Are held or medicated against their will under statutes that require a DSM diagnosis as the legal trigger. The biomedical reading is load-bearing here: the claim that the diagnosis identifies a real disease state is what licenses overriding their stated wishes.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, involuntarily_committed_individuals, payer,
    powerless, immediate, trapped, national).

% Lose guardianship, custody, contractual, or testamentary capacity on the strength of a DSM diagnosis presented in court as identifying an objective, verifiable brain disease. Have no comparably authoritative counter-evidence available to them, since the same institutions that diagnose also certify capacity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, legal_capacity_stripped_patients, payer,
    powerless, biographical, trapped, national).

% Build careers, grants, and journals around the search for biomarkers and neural correlates for DSM categories. Continued failure to find robust, category-specific biomarkers after decades of well-funded search is treated within the field as a temporary gap in tools rather than as evidence against the categories' validity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, agenda_setter).

% Clinicians and researchers who argue DSM categories are administrative constructs rather than discovered diseases are marginalized within mainstream psychiatric institutions, journals, and licensing bodies that have built their authority on the biomedical claim. Their objections rarely reach the courtroom or the classroom where the diagnosis is operationalized.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_clinicians, excluded,
    moderate, biographical, constrained, national).

% Study the DSM's revision history — categories added, removed, and redefined by committee vote rather than by discovery of new biological evidence — and compare it to genuine cases of nosological discovery in other areas of medicine (e.g., infectious disease etiology).
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, medical_historians_and_philosophers_of_science, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic vocabulary that lets clinicians, insurers, researchers, and courts communicate about patient presentations using a common set of labels, enabling billing, research comparability, and treatment protocol standardization.
% TRANSFER_FUNCTION: Moves diagnostic authority and its downstream consequences (treatment mandates, legal capacity determinations, insurance coverage, institutional sorting) from the diagnosed individual to the diagnosing institution, on the strength of a claim of biological objectivity that the underlying science has not established at the category level.
% ABSENT_VOICES: Critical psychiatry clinicians and patients who reject their diagnosis as a valid disease category are structurally absent from DSM revision committees, which are staffed by clinicians and researchers institutionally invested in the categories' validity; neurodiversity advocates raising a parallel but distinct objection are also outside the room.
% DISAPPEARANCE_RATIONALE: If the biomedical reading of DSM categories were abandoned overnight — if categories were universally treated as provisional administrative constructs rather than discovered diseases — involuntary commitment statutes tied to diagnosis, insurance reimbursement schedules, disability determinations, and pharmaceutical marketing claims would all require restructuring; legal capacity proceedings citing DSM diagnoses as objective medical findings would lose their evidentiary basis.
% FOUNDING_PROBLEM: Clinicians needed a shared, reliable vocabulary to communicate about patient presentations across institutions, insurers, and researchers, replacing earlier idiosyncratic and psychoanalytically-loaded diagnostic language with operationalized, checklist-based criteria (DSM-III, 1980).
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment attests the founding problem (unreliable diagnosis) was solved by DSM-III's operationalized criteria and that biological validation is ongoing but on track. Independent historians of medicine (e.g., accounts of the DSM-III task force's own internal memos) and philosophers of psychiatric science attest that reliability was achieved but validity — the claim that categories correspond to distinct underlying disease processes — remains empirically unestablished decades later; this corroboration comes from outside the psychiatric establishment and outside the pharmaceutical industry.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72) because the biomedical reading's core function — licensing involuntary treatment, pharmaceutical intervention, and legal capacity determinations — imposes severe, often irreversible costs on diagnosed individuals, and this licensing power rests on a claim of biological objectivity the underlying research has not delivered at the category level after 40+ years of well-funded search. Suppression (0.68) reflects that dissenting clinical viewpoints (critical psychiatry) are structurally excluded from DSM revision processes and from courtroom expert testimony, not merely disagreed with. Theater ratio (0.4) captures that a substantial share of activity defending the biomedical claim — biomarker studies that fail to replicate at the category level, task-force revisions presented as scientific refinement — functions more to sustain institutional legitimacy than to establish the claimed validity. Accessibility collapse (0.5) is moderate rather than extreme: alternative diagnostic frameworks (dimensional models, RDoC, critical psychiatry accounts) exist and are actively argued in the literature, so alternatives have not fully collapsed, but they are marginalized in clinical and legal practice. Resistance (0.6) reflects substantial organized pushback from critical psychiatry, patient advocacy, and philosophy-of-science critique.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and conformity-enforcing institutions are structural beneficiaries: they collect professional authority, revenue, and social-control capacity respectively from the biomedical framing being treated as settled, and their exit options are effectively arbitrage-grade (they can shift emphasis between categories, drugs, or diagnostic frameworks while retaining institutional position). Diagnosed individuals, and especially involuntarily committed or capacity-stripped individuals, are structural targets: the biomedical claim is precisely what licenses overriding their preferences, and their exit options range from constrained (contesting a diagnosis on one's record) to trapped (contesting an active commitment order). This maps directly onto the derivation chain — beneficiaries get low d, victims get high d, no override was needed here because the structural relationship is unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unreliable, idiosyncratic diagnosis prior to DSM-III's operationalized criteria — was largely solved by the 1980 revision; inter-rater reliability improved substantially. But the biomedical reading's further claim, that these operationalized categories track distinct discoverable disease entities, was never established and remains contested by historians and philosophers of psychiatric science working outside the psychiatric establishment. Classifying this as tangled_rope rather than snare or mountain reflects that the coordination function (shared diagnostic vocabulary enabling clinical communication, research comparability, insurance processing) is genuine and was that story's actual founding achievement — it is not pure cover. The extraction is asymmetric and layered onto that genuine coordination function: the same categorical apparatus that lets clinicians communicate also licenses involuntary intervention on the strength of an unestablished validity claim. Treating this as a mountain (natural, discovered disease entities) would erase the beneficiary structure and the contested validity question entirely; treating it as a pure snare would erase the genuine reliability gains DSM-III achieved. Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_validity_vs_reliability,
    'Do DSM categories correspond to discrete, biologically distinct disease entities (validity), or have researchers only ever established that clinicians can apply the checklist criteria consistently to the same patient (reliability)?',
    'A robust, replicated, category-specific biomarker (genetic, neuroimaging, or physiological) that discriminates a DSM category from both healthy controls and adjacent categories at clinically useful sensitivity/specificity, replicated across independent research groups and populations.',
    'If validity is established, the biomedical reading is vindicated and its extraction is closer to genuine coordination cost around a real discovered kind. If validity remains unestablished indefinitely, continued treatment of the categories as discovered disease entities is a false-summit pattern: mountain framing sustained by beneficiaries despite the operational reality being closer to administrative construct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_validity_vs_reliability, empirical, 'Whether DSM categories are validated biological kinds or only reliably-applied administrative labels.').

omega_variable(
    dsm_kernel_reading_disagreement_locus,
    'This constraint is one reading (biomedical) of the dsm_taxonomy_kernel. The critical_psychiatry_reading holds the categories are reverse-engineered from available drug treatments; the neurodiversity_reading holds the categories pathologize natural variation against institutional norms. All three readings agree the DSM categories exist and are actively used to sort and treat people — they disagree about WHAT KIND OF THING the categories are (discovered disease vs. market-construction artifact vs. institutionally-imposed pathologization of difference).',
    'The disagreement is not resolvable by a single experiment across all three readings simultaneously; each reading proposes a different kind of corroborating evidence (biomarker validation for biomedical; pharmaceutical industry funding-and-drafting correlation analysis for critical psychiatry; cross-cultural and historical variation in what counts as impairment for neurodiversity). The dsm_taxonomy_kernel node in the network models this as three siblings whose evidentiary programs run in parallel.',
    'Which reading is treated as authoritative determines which victim set (diagnostic-threshold patients broadly, vs. specific market-targeted diagnostic categories, vs. neurologically atypical individuals) receives legal and clinical protection, and which beneficiary set (psychiatric establishment/pharma, vs. narrower pharma-market-construction actors, vs. conformity-enforcing institutions) is understood as extracting.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dsm_kernel_reading_disagreement_locus, conceptual, 'Where the three sibling readings of the DSM kernel actually disagree, and what evidence each would accept as resolving its own claim.').

omega_variable(
    involuntary_commitment_dependency,
    'Is the biomedical objectivity claim causally necessary for current involuntary commitment and capacity-stripping statutes, or could equivalent legal mechanisms operate on a purely functional/risk-based standard without invoking discovered-disease language?',
    'Comparative legal analysis of jurisdictions that have moved (or debated moving) to functional/capacity-based rather than diagnosis-based commitment criteria, tracking whether commitment rates and due-process protections change.',
    'If legal mechanisms are separable from the biomedical validity claim, the extraction attributed to this reading specifically (rather than to civil commitment law generally) may be lower than authored; if inseparable, the current authorship is accurate or conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(involuntary_commitment_dependency, empirical, 'Whether involuntary commitment''s extraction is intrinsic to the biomedical reading or to civil commitment law more broadly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(dsm__tr_t2018, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(dsm__be_t2018, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.64).
narrative_ontology:measurement(dsm__su_t2018, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2018, 0.67).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.1).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_commitment_statutes).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychotropic_pharmaceutical_marketing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dsm_taxonomy_kernel, each authored as a separate ε-invariant constraint per the ε-invariance principle: biomedical_reading (this file, tangled_rope, ε=0.72), critical_psychiatry_reading (categories reverse-engineered from available drugs to construct markets), and neurodiversity_reading (categories pathologize natural variation against institutional norms). All three agree DSM categories are operative and consequential; they diverge on the nature of what the categories track and therefore on beneficiary/victim structure and ε. Each links to the other two via affects_constraints, and each independently links downstream to institutional constraints (involuntary commitment law, pharmaceutical marketing) whose legitimacy partly depends on which reading is socially dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
