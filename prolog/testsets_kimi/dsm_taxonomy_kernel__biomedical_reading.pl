% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: DSM Biomedical Disease Entity Reading
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The biomedical reading of the DSM kernel asserts that psychiatric
 *   diagnostic categories correspond to objective neurobiological disease
 *   entities discoverable through empirical research. This reading functions
 *   as a constraint by legitimizing involuntary treatment, expanding
 *   pharmaceutical markets, and authorizing institutional behavioral control.
 *   It is contested by neurodiversity and critical psychiatry readings that
 *   share the same diagnostic text but instantiate different structural
 *   relationships.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: agenda_setter and beneficiary (institutional/mobile) â defines categories and captures professional jurisdiction
 *   - pharmaceutical_industry: beneficiary (powerful/arbitrage) â captures profits from indication expansion
 *   - institutions_requiring_behavioral_conformity: beneficiary (institutional/mobile) â captures behavioral control and legal cover
 *   - diagnostic_threshold_population: payer (powerless/identity_locked) â bears extraction through involuntary treatment, pharmaceutical regimen, and lost legal capacity
 *   - critical_psychiatry_researchers: excluded (moderate/constrained) â contests ontology but lacks institutional authority
 *   - neurodiversity_advocates: excluded (moderate/constrained) â contests pathologization but excluded from revision processes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.78).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.75).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Disease Entity Reading").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '5517e09c-1fa5-40a5-b50e-da6ff0bd9705').
narrative_ontology:cs_kernel_codification('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', fixed_text).
narrative_ontology:cs_authority_grounding('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', expertise).
narrative_ontology:cs_interpretation_layer_present('5517e09c-1fa5-40a5-b50e-da6ff0bd9705').
narrative_ontology:cs_reading_relation('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', dsm_taxonomy_kernel__critical_psychiatry_reading, influences).
narrative_ontology:cs_axiom('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', foundational, dsm_categories_are_neurobiological_natural_kinds).
narrative_ontology:cs_axiom_status(dsm_categories_are_neurobiological_natural_kinds, holdable).
narrative_ontology:cs_axiom_grounding('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', dsm_categories_are_neurobiological_natural_kinds, empirically_contingent).
narrative_ontology:cs_axiom('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', secondary, coercive_intervention_justified_by_disease_status).
narrative_ontology:cs_axiom_status(coercive_intervention_justified_by_disease_status, holdable).
narrative_ontology:cs_axiom_grounding('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', coercive_intervention_justified_by_disease_status, instrumental).
narrative_ontology:cs_reference_frame('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', biomedical_disease_entity_framework).
narrative_ontology:cs_drift_state('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', post_genomic_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5517e09c-1fa5-40a5-b50e-da6ff0bd9705', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_population).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, biomedical_model_legitimacy).
narrative_ontology:constraint_vindicates(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_intervention_primary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and revises the DSM, defining diagnostic thresholds and disease categories. Controls research funding streams, training curricula, and institutional licensure. Benefits from professional jurisdiction, prestige, and the legitimizing narrative that psychiatric expertise maps symptoms to discoverable biological entities.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, beneficiary).

% Develops and markets psychotropic medications mapped to DSM diagnostic indications. The disease-entity framing expands reimbursable markets and justifies long-term pharmacological management. Profits scale with diagnostic prevalence and the institutional requirement that medication response validates the category.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Schools, correctional facilities, employers, and welfare agencies use DSM categories to classify, exclude, or medically manage non-conforming behavior. Gains streamlined behavioral control, legal cover for exclusion, and a medicalized alternative to overt disciplinary frameworks.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, generational, mobile, national).

% Individuals who meet DSM threshold criteria and receive diagnoses that subject them to involuntary treatment, pharmaceutical regimens, loss of legal capacity, and stigmatization. Diagnosis often becomes identity-fused and legally consequential, making exit from the constraint difficult even after symptomatic periods subside.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnostic_threshold_population, payer,
    powerless, biographical, identity_locked, national).

% Researchers who contest the biomedical disease-entity model and advance psychosocial, political, or trauma-based understandings of distress. Marginalized in funding allocation, peer review, and institutional authority despite accumulating empirical support for their frameworks.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_researchers, excluded,
    moderate, generational, constrained, global).

% Advocates who frame cognitive and neurological differences as natural human variation rather than pathology. Excluded from DSM revision processes and diagnostic policy, their alternatives are suppressed by the disease-entity framing that pathologizes neurobiological diversity.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate psychiatric treatment by grouping observable symptoms into valid disease entities, enabling standardized diagnosis, prognosis, and pharmacological intervention across diverse clinical settings.
% TRANSFER_FUNCTION: Moves authority to define mental normality from individuals and communities to expert psychiatric institutions; moves bodily autonomy and legal capacity from diagnosed individuals to medical and institutional oversight; moves research funding and pharmaceutical profits toward neurobiological intervention models.
% ABSENT_VOICES: Critical psychiatry researchers who contest disease entity ontology, neurodiversity advocates who reject pathologization of cognitive difference, and service users who experience diagnosis as social control rather than medical care are structurally excluded from DSM revision processes and diagnostic policy.
% DISAPPEARANCE_RATIONALE: If the biomedical disease entity claim vanished, involuntary treatment regimes would lose their primary legitimizing framework, pharmaceutical markets dependent on diagnostic indication would contract, institutions would need alternative behavioral management paradigms, and the epistemic architecture of modern psychiatry would require reconstruction.
% FOUNDING_PROBLEM: Severe mental distress, behavioral disruption, and cognitive suffering that impaired individual functioning and threatened social order required systematic classification and intervention to relieve suffering and protect communities.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment and medical institutions attest the problem remains live and best addressed through biomedical framing. Historians of psychiatry, critical psychiatrists, and disability scholars argue the founding problem was genuine human distress but that the biomedical disease-entity framing is a historically contingent solution that has outlived its empirical support; no corroborator outside the benefiting parties fully endorses the objective natural kind claim.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the disease-entity framing authorizes involuntary intervention, lifelong pharmaceutical management, and legal capacity restrictions that far exceed any demonstrated biomarker specificity. Suppression is high (0.75) because the constraint persists by excluding rival frameworks from funding, training, and policy. Theater ratio is moderate (0.45): the surface performance of scientific objectivity is substantial and growing as empirical challenges mount. Accessibility collapse is high (0.70) because alternatives (psychosocial, neurodiversity) are institutionally marginalized once the biomedical frame is accepted. Resistance is moderate (0.55) reflecting growing but structurally weakened contestation. The measurement series run on a single shared time grid showing extraction accumulation from DSM-III era to present.
 *
 * PERSPECTIVAL GAP:
 *   From the psychiatric establishment seat, the constraint appears as legitimate medical science coordinating care for discoverable diseases. From the diagnostic_threshold_population seat, the same structure operates as coercive extraction that fuses identity to pathology and removes legal autonomy. The engine computes this divergence from the structural asymmetry in beneficiary/victim declarations and exit options; the authored claim does not adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and conforming institutions are structural beneficiaries: they collect professional jurisdiction, revenue, and behavioral control, placing them near the beneficiary end of directionality. The diagnostic_threshold_population is the structural target: they bear the costs of involuntary treatment and identity foreclosure, placing them near the full-target end. Critical researchers and neurodiversity advocates are excluded from the beneficiary derivation chain; their opposition is structurally contained rather than incorporated.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was built to address genuine suffering and behavioral disruption (a live founding problem). However, the specific claim that DSM categories map to objective neurobiological entities has outlived its empirical support, hardening into a snare that now primarily serves institutional and pharmaceutical extraction. The divergence between founding_problem_status (contested) and disappearance_verdict (world_rearranges) flags the mandatrophy: the arrangement persists and rearranges the world, but the problem it was built to solve no longer justifies the specific disease-entity architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_entity_empirical_status,
    'Do DSM categories correspond to discoverable neurobiological natural kinds, or are they pragmatic constructs without objective biological correlates?',
    'Large-scale genomic, neuroimaging, and biomarker research that either validates or fails to validate discrete biological boundaries for major DSM categories.',
    'If no consistent biomarkers or natural boundaries are found, the biomedical reading collapses toward constructed snare; if validated, extraction diminishes toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_entity_empirical_status, empirical, 'Empirical status of biomedical disease entity claim').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s suppression primarily structural (legal involuntary treatment, institutional exclusion) or internalized (diagnosed individuals adopting disease identity and compliance)?',
    'Longitudinal studies of service users post-diagnosis tracking internalized stigma, compliance behavior, and identity formation independently of structural coercion.',
    'If internalized suppression dominates, effective extraction exceeds structural measures and the constraint operates through cognitive capture; if purely structural, removal of legal frameworks would liberate targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_family_reading_delta,
    'How would the biomedical reading''s classification change if structurally compared to the neurodiversity and critical psychiatry readings of the same kernel?',
    'Cross-reading comparison of epsilon values, beneficiary/victim structures, and authority groundings across the DSM taxonomy kernel family.',
    'Reveals whether the biomedical reading''s extraction is unique or shared across all kernel instantiations, altering whether the kernel itself is treated as inherently contested or as a neutral text with one bad reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_family_reading_delta, conceptual, 'Sibling reading structural comparison within kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_bio_tr_t0, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsm_bio_tr_t10, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(dsm_bio_tr_t20, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(dsm_bio_tr_t30, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(dsm_bio_tr_t40, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(dsm_bio_tr_t50, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(dsm_bio_be_t0, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dsm_bio_be_t10, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dsm_bio_be_t20, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(dsm_bio_be_t30, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(dsm_bio_be_t40, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(dsm_bio_be_t50, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dsm_bio_su_t0, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dsm_bio_su_t10, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(dsm_bio_su_t20, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(dsm_bio_su_t30, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(dsm_bio_su_t40, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(dsm_bio_su_t50, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel decomposes into three structurally distinct readings. The biomedical reading (this file) treats diagnostic categories as objective neurobiological disease entities coordinated through medical expertise. The neurodiversity reading treats the same categories as an identity-coordination mechanism enforcing behavioral conformity. The critical psychiatry reading treats them as an extraction mechanism reverse-engineered from pharmaceutical markets. They share the same diagnostic text but instantiate different constraints with different epsilon values, beneficiary/victim structures, and authority groundings. Network edges link the upstream biomedical claim to its downstream contestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
