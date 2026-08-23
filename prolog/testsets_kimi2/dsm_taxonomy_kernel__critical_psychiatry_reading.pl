% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__critical_psychiatry_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: dsm_taxonomy_kernel__critical_psychiatry_reading
 *   human_readable: DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This constraint instantiates the critical psychiatry reading of the DSM
 *   taxonomy kernel. The kernel is the Diagnostic and Statistical Manual of
 *   Mental Disorders as an authoritative nosology; the critical reading holds
 *   that DSM categories are reverse-engineered from available pharmaceutical
 *   treatments to construct and expand markets for psychotropic drugs.
 *   Sibling readings include the biomedical reading (categories map to
 *   objective neurobiological disease entities) and the neurodiversity
 *   reading (categories pathologize natural neurological variation). This
 *   reading claims the constraint is a tangled rope: the DSM undeniably
 *   coordinates diagnosis, research, and reimbursement, but it does so while
 *   asymmetrically extracting from patients and concentrating gains in
 *   pharmaceutical capital and industry-funded psychiatrists.
 *
 * KEY AGENTS:
 *   - Pharmaceutical industry: primary beneficiary â captures diagnostic expansion as drug indications
 *   - Industry-funded psychiatrists: secondary beneficiary â receives funding and professional standing from the industry-taxonomy nexus
 *   - DSM authors (APA task force): agenda_setter â maintains the formal taxonomy under institutional and market pressure
 *   - Patients subjected to overprescription: primary payer â bears adverse effects and opportunity costs of missed non-pharmacological care
 *   - Critical psychiatry scholars: analytical observer â documents capture but lacks institutional leverage
 *   - Neurodiversity advocates: excluded voice â structurally absent from revision processes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__critical_psychiatry_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__critical_psychiatry_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__critical_psychiatry_reading, "DSM Taxonomy as Pharmaceutical Market Construction (Critical Psychiatry Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__critical_psychiatry_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__critical_psychiatry_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__critical_psychiatry_reading, 'a16dd8e0-f4a9-4de3-b69d-016a66f2d95b').
narrative_ontology:cs_kernel_codification('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', formalized).
narrative_ontology:cs_authority_grounding('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', extraction).
narrative_ontology:cs_interpretation_layer_present('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b').
narrative_ontology:cs_reading_relation('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', dsm_taxonomy_kernel__neurodiversity_reading, influences).
narrative_ontology:cs_axiom('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', foundational, dsm_categories_are_market_constructions).
narrative_ontology:cs_axiom_status(dsm_categories_are_market_constructions, holdable).
narrative_ontology:cs_axiom_grounding('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', dsm_categories_are_market_constructions, empirically_contingent).
narrative_ontology:cs_axiom('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', foundational, diagnostic_expansion_serves_pharmaceutical_profit).
narrative_ontology:cs_axiom_status(diagnostic_expansion_serves_pharmaceutical_profit, holdable).
narrative_ontology:cs_axiom_grounding('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', diagnostic_expansion_serves_pharmaceutical_profit, empirically_contingent).
narrative_ontology:cs_reference_frame('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', market_constructed_nosology).
narrative_ontology:cs_drift_state('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', contemporary_post_dsm5, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a16dd8e0-f4a9-4de3-b69d-016a66f2d95b', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_receiving_industry_funding).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_overprescribed).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, biomedical_model_of_mental_illness).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmacological_intervention_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Profits from the expansion of diagnosable populations and corresponding pharmacological indications tied to DSM categories. Funds clinical trials, guideline committees, and continuing medical education to align diagnostic expansion with existing drug portfolios. Could exit to other therapeutic areas but has captured psychiatry as a stable revenue pipeline.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Receive direct industry funding for research, speaking fees, and advisory roles while participating in professional structures that legitimize DSM-based prescribing. Their professional standing and income flow depend on maintaining the taxonomy's authority, making exit costly despite awareness of validity problems.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, psychiatrists_receiving_industry_funding, beneficiary,
    powerful, biographical, constrained, national).

% APA task force members who draft and revise DSM categories. They operate under institutional pressure to produce a manual that preserves psychiatry's medical legitimacy and insurance viability, which constrains their ability to radically de-pathologize or reject pharmaceutical framings even when descriptive validity suggests otherwise.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, dsm_authors_psychiatrists, agenda_setter,
    institutional, generational, constrained, national).

% Receive DSM diagnoses that channel them into long-term psychotropic regimens with limited informed consent about iatrogenic effects, withdrawal difficulties, or alternative frameworks. Trapped by insurance requirements, involuntary commitment standards, and the absence of non-DSM clinical infrastructure in most healthcare systems.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, patients_overprescribed, payer,
    powerless, biographical, trapped, national).

% Analyze the historical and economic conditions of DSM construction from outside the APA-industry nexus. They document industry influence, diagnostic inflation, and outcome failures but lack institutional power to alter reimbursement or licensing rules that enforce DSM use.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, critical_psychiatry_scholars, observer,
    analytical, civilizational, analytical, global).

% Assert that DSM categories pathologize natural neurological variation. Excluded from APA revision processes and from insurance frameworks that require DSM codes for accommodation or services. Their inclusion would threaten the disease-model assumptions that justify pharmacological intervention.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__critical_psychiatry_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__critical_psychiatry_reading, pharmaceutical_industry).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__critical_psychiatry_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes psychiatric diagnosis across clinical and research settings, producing consistent communication, insurance reimbursement coding, and epidemiological aggregation for public health planning.
% TRANSFER_FUNCTION: Moves diagnostic legitimacy and corresponding pharmaceutical sales from undifferentiated psychological distress into discrete, billable disease categories, transferring wealth from patients and payers to pharmaceutical manufacturers and industry-aligned prescribing clinicians.
% ABSENT_VOICES: Patients reporting severe adverse effects from polypharmacy, critical psychiatrists challenging diagnostic validity, and neurodiversity advocates rejecting pathologization are structurally underrepresented in APA task force deliberations and industry-funded efficacy trials; their absence is maintained by credential-gated revision processes and research funding priorities.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would collapse insurance reimbursement for mental health, invalidate decades of pharma-indication pipelines, fragment psychiatric research aggregation, and create immediate pressure for alternative nosologies (trauma-informed, dimensional, neurodiversity-based) to fill the clinical and administrative vacuum.
% FOUNDING_PROBLEM: Inter-rater reliability crisis in psychiatric diagnosis during the 1970s, coupled with psychiatry's threatened medical legitimacy relative to neurology and internal medicine.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists and critical psychiatry scholars (e.g., Kirk, Kutchins, Whitaker, Healy) outside the pharmaceutical beneficiary set attest that the reliability crisis was the genuine founding problem; these same external observers attest it has been superseded and the constraint now primarily serves market-consolidation functions.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__critical_psychiatry_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__critical_psychiatry_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__critical_psychiatry_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__critical_psychiatry_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is moderate-high because the DSM's category boundaries have demonstrably expanded alongside pharmaceutical marketing cycles, converting ordinary distress into billable disease. Suppression (0.68) is high because the constraint persists through insurance mandates, licensing requirements, and research funding structures that actively marginalize non-DSM frameworks. Theater ratio (0.45) reflects substantial performative maintenance of 'descriptive neutrality' and 'evidence-based' authority while categories track market availability. Accessibility collapse (0.70) captures the near-total dominance of DSM framing in clinical training, insurance, and legal contexts. Resistance (0.55) reflects active but institutionally underpowered critique from critical psychiatry and survivor movements. Temporal measurements trace rising extraction, theater, and suppression from DSM-III (1980) through DSM-5 (2013) and contemporary contestation.
 *
 * PERSPECTIVAL GAP:
 *   The APA task force and industry-funded psychiatrists experience the DSM as necessary professional infrastructure and medical progress; from these seats the coordination function dominates and the constraint appears as rope or scaffold. Overprescribed patients experience the same structure as a coercive gateway to harmful polypharmacy with no meaningful alternative care pathway; from this seat the constraint appears as snare. The engine computes this divergence from the structural data rather than the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical industry sits near full beneficiary (low d): the constraint subsidizes its market expansion by creating reimbursable indications. Industry-funded psychiatrists sit at low-to-moderate beneficiary d. DSM authors are mixed (moderate d) because they gain authority but pay institutional costs of maintenance. Patients overprescribed sit near full target (high d): they bear the adverse effects and direct costs. Critical scholars are symmetric/analytical (d ~0.5). Neurodiversity advocates are high-d excluded targets of the pathologization logic.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents the mandatrophy error of reading a captured coordination mechanism as pure extraction (snare) â the DSM genuinely solved a 1970s reliability crisis and still enables research aggregation. It also prevents the opposite error of reading active extraction as benign coordination (rope) â the victim set is non-empty, enforcement is active, and the coordination story is used to suppress alternatives. The founding problem (reliability) is dead, confirming the constraint persists beyond its originating mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    industry_influence_quantification,
    'What is the precise magnitude of pharmaceutical industry funding and direct influence on DSM panel composition and category definition?',
    'Mandatory financial disclosure of all APA task force members combined with independent audit of industry-funded working groups and guideline panels.',
    'If influence is demonstrably causal in category expansion, the extractiveness score is validated as structural rather than conspiratorial; if negligible, the critical reading''s extraction claim weakens and the constraint may recompute toward rope or mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industry_influence_quantification, empirical, 'Quantifying pharmaceutical industry capture of DSM revision').

omega_variable(
    coordination_extraction_separability,
    'Can the DSM''s genuine coordination function (diagnostic reliability, research aggregation) be separated from its market-construction function, or are they structurally fused?',
    'Comparative analysis of non-DSM-based healthcare systems and research frameworks that achieve comparable coordination without pharmaceutical-industry capture.',
    'If separable, the constraint is a tangled rope with separable strands; if inseparable, the coordination function itself may be epiphenomenal to extraction, pushing classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable').

omega_variable(
    patient_suppression_mechanism,
    'Is patient compliance with DSM-based overprescription driven primarily by structural barriers (insurance denial for non-DSM care, involuntary treatment) or by internalized belief in the biomedical model?',
    'Longitudinal outcome studies tracking patient behavior and belief after structural exit becomes available (e.g., in jurisdictions with expanded non-pharmacological coverage).',
    'If suppression is primarily internalized, effective extraction exceeds the structural measure because patients carry the constraint after nominal exit; if structural, extraction is bounded by institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patient_suppression_mechanism, empirical, 'Structural versus internalized suppression of patient alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__critical_psychiatry_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_crit_tr_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dsm_crit_tr_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 9, 0.25).
narrative_ontology:measurement(dsm_crit_tr_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 18, 0.3).
narrative_ontology:measurement(dsm_crit_tr_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 27, 0.36).
narrative_ontology:measurement(dsm_crit_tr_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 36, 0.41).
narrative_ontology:measurement(dsm_crit_tr_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, theater_ratio, 45, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm_crit_be_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm_crit_be_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement(dsm_crit_be_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(dsm_crit_be_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 27, 0.65).
narrative_ontology:measurement(dsm_crit_be_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 36, 0.7).
narrative_ontology:measurement(dsm_crit_be_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, base_extractiveness, 45, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm_crit_su_t0, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dsm_crit_su_t9, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 9, 0.48).
narrative_ontology:measurement(dsm_crit_su_t18, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 18, 0.55).
narrative_ontology:measurement(dsm_crit_su_t27, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 27, 0.62).
narrative_ontology:measurement(dsm_crit_su_t36, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 36, 0.65).
narrative_ontology:measurement(dsm_crit_su_t45, dsm_taxonomy_kernel__critical_psychiatry_reading, suppression_requirement, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__critical_psychiatry_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
