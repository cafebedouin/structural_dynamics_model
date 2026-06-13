% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Biomedical Model: Mental Disorders as Discoverable Disease Entities
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   The biomedical reading of DSM categories asserts that psychiatric
 *   diagnoses map to objective neurobiological disease entities discoverable
 *   through empirical research. This reading frames mental disorders as brain
 *   diseases, treatment as medical intervention, and diagnosis as the
 *   scientific identification of underlying pathology. It coordinates a vast
 *   institutional ecosystem: psychiatric profession, pharmaceutical industry,
 *   insurance systems, schools, and courts all depend on DSM categories as
 *   scientifically grounded. The reading is not false — it has delivered
 *   standardization, coordination, and relief for many people. But it is also
 *   extractive: it routes authority to medical professionals, transforms
 *   human suffering into treatable pathology, medicalizes behavior that
 *   conflicts with institutional norms, and creates pharmaceutical markets by
 *   casting normal human variation as disorder. This story instantiates ONLY
 *   the biomedical reading; the sibling critical_psychiatry and
 *   neurodiversity readings are separate constraint stories linked by
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.76).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.71).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Biomedical Model: Mental Disorders as Discoverable Disease Entities").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7').
narrative_ontology:cs_kernel_codification('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', fixed_text).
narrative_ontology:cs_authority_grounding('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', extraction).
narrative_ontology:cs_interpretation_layer_present('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7').
narrative_ontology:cs_reading_relation('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', dsm_taxonomy_kernel__critical_psychiatry_reading, forecloses).
narrative_ontology:cs_reading_relation('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', dsm_taxonomy_kernel__neurodiversity_reading, coexists_with).
narrative_ontology:cs_axiom('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', foundational, mental_disorders_are_brain_diseases).
narrative_ontology:cs_axiom_status(mental_disorders_are_brain_diseases, holdable).
narrative_ontology:cs_axiom_grounding('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', mental_disorders_are_brain_diseases, empirically_contingent).
narrative_ontology:cs_axiom('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', foundational, diagnostic_criteria_discover_underlying_pathology).
narrative_ontology:cs_axiom_status(diagnostic_criteria_discover_underlying_pathology, holdable).
narrative_ontology:cs_axiom_grounding('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', diagnostic_criteria_discover_underlying_pathology, empirically_contingent).
narrative_ontology:cs_axiom('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', secondary, psychiatric_expertise_is_medical_expertise).
narrative_ontology:cs_axiom_status(psychiatric_expertise_is_medical_expertise, holdable).
narrative_ontology:cs_axiom_grounding('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', psychiatric_expertise_is_medical_expertise, conventional).
narrative_ontology:cs_reference_frame('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', objective_neurobiological_disease_model).
narrative_ontology:cs_drift_state('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', contemporary_post_biomarker_absence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9a4d46a1-163e-4702-9e9c-ccb2ccfa66f7', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutional_compliance_enforcers).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_criteria).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, neurodivergent_populations).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, cultural_minorities_with_different_psychiatric_expression).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).

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
 *   Extractiveness rises from 0.38 (1980, post-DSM-III when diagnostic standardization began) to 0.76 (2025, after decades of pharmaceutical expansion and neurodiversity critique). The rise tracks pharmaceutical market expansion, broadening diagnostic criteria (e.g., major depressive disorder spectrum, ADHD in adults), and increasing mandatory treatment pathways. Theater_ratio is low-to-moderate (0.12 → 0.42) because the biomedical research function is real — neurobiological investigations continue — but growing share of enforcement is devoted to securing diagnostic authority and pharmaceutical markets rather than discovering new disease mechanisms. Suppression is substantial (0.48 → 0.71) because the constraint requires active exclusion of alternative frameworks: critical psychiatry is marginalized in journals and funding, neurodiversity advocates are labeled as denying illness, cultural psychiatry is treated as less rigorous. The shared time grid ensures every metric is authored at every examined point; measurements are 'observed' (historical data on diagnosis rates, pharmaceutical spending, research publication trends, institutional enforcement actions).
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric establishment perceives this constraint as Rope (genuine coordination of scientific knowledge). The pharmaceutical industry perceives it as Rope (market enabling through disease categorization). Individuals meeting diagnostic criteria perceive it as Tangled Rope if they benefit from treatment (coordination + extraction bundled) or as Snare if treatment is coercive (extraction, minimal coordination). Neurodivergent populations perceive it as Snare (pathologization, identity loss, coercion). Critical psychiatrists perceive it as Snare with institutional theater (the disease-discovery research is performative; the real function is legitimizing suppression). The engine computes these divergences from power + exit + beneficiary/victim declarations; this is why per-seat classification matters.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment and pharmaceutical industry are clear beneficiaries (d near 0.0 → negative χ: they benefit). Institutional compliance enforcers benefit partly (schools use DSM to justify medication and special education categorization). Individuals meeting diagnostic criteria sit near symmetric or slightly toward target (d ≈ 0.4–0.6): they receive coordination benefit (disease framework explains suffering, treatment becomes possible) but bear extraction (loss of autonomy, medication mandates, identity fusion with diagnosis). Neurodivergent populations and cultural minorities are targets (d near 1.0): the biomedical reading pathologizes their variation, foreclosing alternatives, with minimal coordination benefit — they are harmed by the pathologization. Critical psychiatry researchers are trapped (d ≈ 0.95): they are excluded from authority structures and funding. The engine computes d from these beneficiary/victim declarations + exit options; the spatial_scope (universal: psychiatric diagnosis spans all borders) and power asymmetries (institutional vs. powerless) amplify effective extraction for the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (incoherent, non-standardized diagnosis) is partially solved (standardization achieved) but the solution bundled in a biomedical framing that persists even as evidence for underlying disease entities remains weak. Decades of neurobiology research have not identified biomarkers for most DSM categories; the disease model is vindicating a proposition (mental illness IS brain disease) rather than discovering a fact. The theater_ratio rise (0.12 → 0.42) indicates the constraint increasingly performs disease-discovery while its actual function is legitimizing institutional control and pharmaceutical extraction. The mandatrophy is partial: the constraint is not dead (it delivers real coordination and some people genuinely benefit), but its justification (empirical discovery of disease entities) has outlived its empirical grounding. This is why the critical and neurodiversity readings emerge as challenges — they point to this gap between founding justification and present function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neurobiological_biomarker_discovery,
    'Do objective neurobiological biomarkers exist that validate the biomedical reading''s claim that DSM categories map to discoverable disease entities?',
    'Longitudinal meta-analysis of biomarker research across all major DSM categories; examination of whether proposed biomarkers replicate across populations and predict treatment response; assessment of biological heterogeneity within diagnostic categories.',
    'If biomarkers are found for most DSM categories, the biomedical reading is vindicated and the critical reading loses empirical grounding. If biomarkers remain elusive despite 45+ years of research, the critical reading gains force: the categories may be pragmatic social constructs, not disease discoveries. If biomarkers are found for some categories but not others, all three readings remain viable — the constraint is a hybrid of genuine disease discovery and constructive categorization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neurobiological_biomarker_discovery, empirical, 'Whether neurobiological discoveries support or undermine the biomedical reading''s core claim.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (external barriers to alternative frameworks) or internalized (individuals with diagnoses have fused identity with diagnosis and resist alternatives)?',
    'Longitudinal studies of service-user perspective shifts: do individuals diagnosed under the biomedical model maintain disease identity when exposed to neurodiversity-affirmative or critical psychiatry framings? Do researchers in critical psychiatry face publication barriers (structural) or self-censor due to fear of being labeled anti-psychiatry (internalized)? Interview studies with excluded populations about perceived barriers.',
    'If suppression is primarily structural, the constraint could be altered by changing institutional access and publishing standards. If primarily internalized, the constraint persists even when structural barriers are removed — individuals carry the suppression with them. If mixed, fixing the constraint requires both architectural change and identity-work with those labeled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'The mechanism maintaining the biomedical reading''s dominance.').

omega_variable(
    separation_of_standardization_from_disease_discovery,
    'Are the achievements of DSM standardization (coherent criteria, research aggregation, clinical communication) structurally separable from the claim that the categories discover underlying disease entities?',
    'Philosophical and historical analysis of whether diagnostic standardization logically requires disease-entity discovery. Examination of whether alternative frameworks (neurodiversity, social construction, phenomenological) can deliver standardization without disease claims. Pilot use of alternative classification systems (ICD-11, dimensional models, neurodiversity-affirmative frameworks) in research and clinical settings.',
    'If separable, standardization can be retained while the disease-discovery claim is dropped — the constraint could be reformed without abandoning coordination benefits. If inseparable, defending standardization requires defending the biomedical reading, and abandoning the reading means losing standardization. If hybrid, some coordination functions require disease claims and others do not — selective retention becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_of_standardization_from_disease_discovery, conceptual, 'Whether the coordination function depends on the extractive biomedical framing.').

omega_variable(
    pharmaceutical_incentive_alignment,
    'To what extent does pharmaceutical industry funding and marketing shape which conditions are diagnosed, how broadly diagnostic criteria are applied, and whether biomedical frameworks are preferred over alternatives?',
    'Analysis of DSM-revision task force funding sources and pharmaceutical company ties. Comparison of diagnostic expansion rates for conditions with available pharmacotherapies vs. conditions without. Examination of marketing messaging from pharmaceutical companies and its alignment with DSM diagnostic scope. Cross-national comparison of diagnosis rates in health systems with vs. without pharmaceutical incentive structures.',
    'If pharmaceutical incentives substantially shape diagnostic expansion and biomedical framing preference, the critical reading gains credence: DSM categories are partly reverse-engineered from available treatments. If incentives are minimal, the biomedical reading is strengthened. If moderate, all three readings remain viable — the constraint is shaped by both scientific inquiry and market incentives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_incentive_alignment, empirical, 'The extent of pharmaceutical influence on DSM categories and the biomedical framing.').

omega_variable(
    identity_fusion_in_diagnostic_labeling,
    'When individuals are diagnosed under the biomedical reading, to what extent does the diagnosis become fused with self-identity, foreclosing alternative self-understandings?',
    'Longitudinal studies of self-identity trajectories following psychiatric diagnosis; comparison of identity fusion rates for medical diagnoses (diabetes, hypertension) vs. psychiatric diagnoses; ethnographic studies of how individuals describe themselves before and after diagnosis; analysis of whether neurodiversity-affirmative frameworks reduce identity fusion.',
    'If identity fusion is severe and persistent, the constraint''s extraction of autonomy is substantial and the exit for individuals meeting criteria (identity_locked) is accurate. If identity fusion is reversible or avoidable with alternative framing, the exit might shift to ''constrained'' and the extraction might be lower. Understanding the mechanism enables resistance strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_diagnostic_labeling, empirical, 'The degree to which psychiatric diagnosis becomes fused with self-identity and forecloses alternatives.').

omega_variable(
    committer_frame__biomedical_vs_critical_foreclosure,
    'Does the biomedical reading''s core premise (DSM categories ARE discoveries of neurobiological disease entities) logically foreclose the critical reading''s premise (DSM categories ARE reverse-engineered from available treatments to construct drug markets), or do both remain viable within different institutional frameworks?',
    'Conceptual analysis: if DSM categories are discoveries, they cannot also be market constructions. But if the discovery process is shaped by market incentives (the biomedical reading does not claim the discovery process is free from incentives), both could be true simultaneously. The question is whether ''shaped by incentives'' is compatible with ''genuine discovery.''',
    'If the biomedical reading forecloses the critical reading, the three readings form a logical triplet where only one can be true. If they coexist (both true simultaneously in different aspects), the constraint family is a hybrid system. This affects how disputes over the readings are resolved: foreclosure suggests empirical adjudication can decide, coexistence suggests negotiation of institutional priorities is necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame__biomedical_vs_critical_foreclosure, conceptual, 'The logical relationship between the biomedical and critical readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(dsm__tr_t1992, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.38).
narrative_ontology:measurement(dsm__be_t1992, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1992, 0.52).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2020, 0.73).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(dsm__su_t1992, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1992, 0.54).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.61).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__biomedical_reading, 0.18).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, psychiatric_involuntary_treatment_authority).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_market_expansion_via_diagnostic_scope).

% DUAL FORMULATION NOTE:
% The dsm_taxonomy_kernel has three readings: biomedical_reading (this story — DSM categories are disease discoveries), critical_psychiatry_reading (DSM categories are market-driven constructions), and neurodiversity_reading (DSM categories pathologize natural variation). These are not different measurements of one constraint; they are structurally distinct claims about the same kernel (the DSM classification system). The biomedical_reading's ε (0.76 at interval end) reflects high extraction coupled with real coordination benefits. The critical_psychiatry_reading has higher ε (purely extractive; no coordination value). The neurodiversity_reading has lower beneficiary/victim asymmetry (neutral or benefit to those labeled). All three share the interval (1980–2025) and some stakeholders but differ in victim sets, beneficiary sets, and extracted values. The network edges indicate that shifts in one reading's institutional position influence the others: if biomarkers are discovered, neurodiversity reading must evolve; if pharmaceutical influence is exposed, critical reading gains credence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__biomedical_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
