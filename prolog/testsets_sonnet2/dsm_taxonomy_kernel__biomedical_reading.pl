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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The biomedical reading of the DSM kernel holds that its diagnostic
 *   categories name objective neurobiological disease entities awaiting
 *   confirmation through empirical (genetic, neuroimaging, biomarker)
 *   research — the categories are discoveries, not conventions. This story
 *   authors ONLY that reading. The theater_ratio rises over the interval as
 *   the operationalized-criteria reliability achievement of DSM-III (real
 *   coordination function) is increasingly used to justify a stronger
 *   biological-validity claim that subsequent research (genome-wide
 *   association studies, the NIMH RDoC pivot) has not delivered — the ratio
 *   of validity-claim performance to validity-claim substance widens. The
 *   claimed type (tangled_rope) and the metrics are authored independently:
 *   the biological-disease claim is stated as a genuine belief structurally
 *   true from inside this reading, while the metrics describe what the
 *   arrangement's operation actually does regardless of whether the
 *   underlying biological claim eventually proves out.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: agenda_setter/beneficiary (institutional/arbitrage) — administers and profits from category stability
 *   - pharmaceutical_industry: beneficiary (institutional/arbitrage) — markets against DSM-coded indications
 *   - diagnostic_threshold_patients: payer (powerless/trapped) — bears involuntary treatment and capacity loss
 *   - clinicians: agenda_setter/payer (organized/constrained) — apply criteria under liability and billing pressure without full ontological endorsement
 *   - biological_psychiatry_researchers: beneficiary/agenda_setter (organized/arbitrage) — careers ride on eventual biomarker discovery
 *   - family_courts_and_disability_boards: agenda_setter (institutional/analytical) — treat diagnosis as settled medical fact in adjudication
 *   - critical_psychiatry_and_service_user_movements: excluded (moderate/constrained) — objection treated as symptomatic rather than evidentiary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.68).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.62).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '153fa32f-5372-491d-b22f-1fb0019428c1').
narrative_ontology:cs_kernel_codification('153fa32f-5372-491d-b22f-1fb0019428c1', formalized).
narrative_ontology:cs_authority_grounding('153fa32f-5372-491d-b22f-1fb0019428c1', expertise).
narrative_ontology:cs_interpretation_layer_present('153fa32f-5372-491d-b22f-1fb0019428c1').
narrative_ontology:cs_reading_relation('153fa32f-5372-491d-b22f-1fb0019428c1', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_reading_relation('153fa32f-5372-491d-b22f-1fb0019428c1', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('153fa32f-5372-491d-b22f-1fb0019428c1', foundational, dsm_categories_are_discoverable_natural_kinds).
narrative_ontology:cs_axiom_status(dsm_categories_are_discoverable_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('153fa32f-5372-491d-b22f-1fb0019428c1', dsm_categories_are_discoverable_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('153fa32f-5372-491d-b22f-1fb0019428c1', secondary, diagnostic_reliability_entails_construct_validity).
narrative_ontology:cs_axiom_status(diagnostic_reliability_entails_construct_validity, holdable).
narrative_ontology:cs_axiom_grounding('153fa32f-5372-491d-b22f-1fb0019428c1', diagnostic_reliability_entails_construct_validity, instrumental).
narrative_ontology:cs_reference_frame('153fa32f-5372-491d-b22f-1fb0019428c1', dsm_iii_operationalized_reliability_consensus).
narrative_ontology:cs_drift_state('153fa32f-5372-491d-b22f-1fb0019428c1', post_rdoc_biomarker_search_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('153fa32f-5372-491d-b22f-1fb0019428c1', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, clinicians).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biomedical_model_of_mental_illness).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, diagnostic_reliability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the DSM through committee process, licenses diagnostic authority to clinicians, and administers the categories through insurance coding, court testimony, and institutional gatekeeping. Its professional legitimacy and reimbursement structure both depend on categories being treated as discovered disease entities rather than provisional constructs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary).

% Markets drugs against DSM-coded indications; each stabilized category becomes a regulatory pathway for approval and a billing target for prescribers. Funds much of the biological research that is then cited as validating the categories it profits from treating.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Once assessed as meeting criteria, can face involuntary commitment, forced medication, loss of custody or employment eligibility, and diminished legal capacity, all justified by the claim that the diagnosis names a real underlying brain disease. Exit is structurally foreclosed: refusing the label or the treatment can itself be read as a symptom (anosognosia) that triggers further intervention.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_patients, payer,
    powerless, biographical, trapped, national).

% Apply DSM criteria daily under liability, insurance, and time pressure; billing codes require a DSM category to reimburse care. Many privately doubt category validity but have no workable alternative vocabulary that clears billing and legal-defensibility requirements, so they administer a system whose ontological claims they may not personally endorse.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, clinicians, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, clinicians, payer).

% Careers, grant pipelines, and journal prestige are built on the premise that DSM categories carve nature at biological joints awaiting discovery (genetic markers, neuroimaging correlates). Persistent failure to find robust biomarkers is typically absorbed as a call for more funding rather than as evidence against the categories.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, biological_psychiatry_researchers, agenda_setter).

% Use DSM diagnoses as load-bearing evidence in custody determinations, competency hearings, and disability adjudication, treating a checklist category as if it were a settled medical fact with the same evidentiary weight as a blood test.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, family_courts_and_disability_boards, agenda_setter,
    institutional, biographical, analytical, national).

% Argue from outside DSM committee structure that categories were reverse-engineered from available treatments or reify culturally contingent norms, but have no seat in DSM revision committees and are frequently characterized as unreliable witnesses to their own conditions when they object to the diagnosis that governs them.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_and_service_user_movements, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared diagnostic vocabulary that lets clinicians, insurers, researchers, and courts communicate about presentations using common terms, enabling billing, treatment protocols, and comparable research across sites.
% TRANSFER_FUNCTION: Moves legal capacity, bodily autonomy, and treatment decisions away from labeled individuals and toward diagnosing institutions; moves public and insurance funds toward pharmaceutical treatment pathways keyed to DSM codes.
% ABSENT_VOICES: People who have been diagnosed and object to the biological-disease framing of their experience are largely absent from DSM revision committees; their testimony is often treated as confirming rather than disconfirming evidence (dismissed as symptomatic of the very condition being described).
% DISAPPEARANCE_RATIONALE: If the biomedical-disease reading of DSM categories were abandoned, involuntary commitment standards, insurance reimbursement structures, disability adjudication, and pharmaceutical marketing claims tied to specific 'disorders' would all require reconstruction on a different evidentiary basis; billing codes, drug approvals, and legal competency frameworks are built directly on the premise this reading asserts.
% FOUNDING_PROBLEM: Clinicians needed a shared, reliable vocabulary to communicate about mental distress across institutions, enable research comparability, and justify insurance reimbursement, replacing the earlier, inconsistent psychoanalytic diagnostic language.
% FOUNDING_PROBLEM_CORROBORATION: The APA and biological psychiatry researchers attest the categories are increasingly validated by emerging neuroscience. Independent replication crises in psychiatric genetics, the NIMH's own 2013 statement that DSM categories lack validity for its research funding purposes (RDoC initiative), and testimony from service-user and critical-psychiatry researchers outside the beneficiary set corroborate that the founding reliability problem was substantially solved by DSM-III's operationalized criteria, while the further claim of discovered neurobiological validity remains unestablished decades later.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.68) because, under this reading, a diagnosis is treated as evidence of an underlying disease state sufficient to override the diagnosed person's stated preferences — enabling involuntary commitment, forced medication, and loss of legal capacity. Suppression (0.62) reflects that alternative framings (dimensional, contextual, socially constructed) are actively excluded from clinical, legal, and insurance practice, not merely disfavored. Accessibility collapse is moderate (0.5) rather than mountain-level because dimensional and psychosocial alternatives remain live in academic psychiatry and in some jurisdictions, even though they are structurally marginalized in practice. Resistance (0.55) reflects organized critical-psychiatry and service-user pushback that has achieved partial institutional recognition (RDoC, ICD-11 dimensional trials) without displacing the categorical biomedical reading's dominant practical role.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment's and biological researchers' seats, the categories are provisional-but-progressing scientific discoveries whose validity will be vindicated by better neuroscience — a mountain-in-waiting. From the diagnosed patient's seat under trapped exit, the same categorical claim is the mechanism by which their testimony about their own experience is discounted and their legal capacity removed. The engine computes these as different seat classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Psychiatric establishment, pharmaceutical industry, and biological psychiatry researchers sit near the beneficiary end: they collect professional legitimacy, revenue, and career capital from the categories being treated as discovered biological facts, and hold arbitrage-grade exit (they can pivot claims, funding targets, or specialty focus without losing standing). Diagnosed patients sit near the full-target end: trapped exit, biographical time horizon, and the highest asymmetry — the same categorical claim that grounds clinicians' authority is what strips patients of the standing to contest their own diagnosis. Clinicians are structurally mixed: they administer the extraction but are also constrained by it, which is why their exit_options are constrained rather than arbitrage despite organized power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inter-rater diagnostic unreliability before DSM-III's operationalized criteria — was substantially solved by the 1980s reliability reforms; the reliability achievement is real coordination and does not evaporate under this reading. Mandatrophy risk is that the reliability achievement is used to retroactively validate a distinct and still-unproven claim (that the reliable categories map onto discovered natural kinds with specific neurobiological substrates). Classifying this as tangled_rope rather than pure mountain or pure snare preserves both facts: coordination (shared vocabulary, reduced diagnostic chaos) is genuine, while the biological-validity claim functions as extractive cover that licenses coercive intervention beyond what the coordination function requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biomarker_discovery_horizon,
    'Will neuroscience eventually identify robust, specific biological substrates corresponding to current DSM categorical boundaries, vindicating the biomedical reading''s core empirical claim?',
    'Longitudinal tracking of genome-wide association study replication, neuroimaging biomarker validation studies, and whether NIMH''s RDoC dimensional framework supersedes or supplements DSM categories in funded research over the next several decades.',
    'If robust biological substrates matching DSM boundaries are found, the biomedical reading''s classification should shift toward mountain (genuine natural kinds, coordination function dominant). If the RDoC pivot away from DSM categories in research funding continues and deepens, the reading''s extractive character (biological-validity claims outrunning evidence) becomes harder to distinguish from pure cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biomarker_discovery_horizon, empirical, 'Whether future neuroscience will vindicate or further undermine the categorical-natural-kind claim central to this reading.').

omega_variable(
    reliability_vs_validity_conflation,
    'Does the demonstrated inter-rater reliability of DSM-III-onward criteria constitute evidence for the distinct claim of biological validity, or are these two independent achievements that the biomedical reading conflates?',
    'Philosophy-of-science analysis distinguishing operational reliability (agreement between raters using the same checklist) from construct validity (the checklist tracking a real underlying natural kind); review of DSM Task Force internal documentation on this distinction.',
    'If conflated, the biomedical reading''s extractiveness score should be read as substantially inflated by a category error that transfers reliability''s genuine coordination credit onto validity''s contested extraction; if genuinely linked, the tangled_rope classification''s coordination component is stronger than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_vs_validity_conflation, conceptual, 'Whether diagnostic reliability and biological validity are being illegitimately conflated within this reading''s own justificatory structure.').

omega_variable(
    committer_framing_choice,
    'Is the biomedical reading correctly framed as a kernel-reading of a shared DSM-taxonomy commitment, or would a decomposition by specific diagnostic category (e.g. schizophrenia vs. ADHD vs. grief-related depression) more accurately track where biological evidence is strong versus absent, since evidentiary support varies enormously across categories within the DSM?',
    'Category-by-category evidentiary review; if biological support varies from near-mountain (e.g. some dementias with clear neuropathology) to near-snare (categories with no replicated biomarkers and high diagnostic inflation), the single kernel-reading framing may itself be under-decomposed per the epsilon-invariance principle.',
    'If categories vary this widely, this single story''s ε (0.68) is an average masking a wide range; a fuller corpus treatment might decompose ''the DSM'' into per-category constraint families rather than treating the whole manual as one kernel with three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Whether the kernel-reading framing at the whole-DSM level is the right grain, or whether further decomposition by diagnostic category is required by the epsilon-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(dsm__tr_t2018, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1994, 0.55).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2013, 0.65).
narrative_ontology:measurement(dsm__be_t2018, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1994, 0.5).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2013, 0.6).
narrative_ontology:measurement(dsm__su_t2018, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, involuntary_psychiatric_commitment_standards).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychotropic_drug_approval_pathway).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the DSM categories' per the epsilon-invariance principle: biomedical_reading (this file, tangled_rope, eps=0.68), critical_psychiatry_reading (categories reverse-engineered from available treatments to build drug markets — expected higher eps, more purely extractive), and neurodiversity_reading (categories pathologize natural variation against institutional norms — different victim framing, different eps). Each reading has its own ε, beneficiary/victim structure, and claimed type; none averages or references the others' values. All three should be linked via affects_constraints to preserve the kernel-family structure for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
