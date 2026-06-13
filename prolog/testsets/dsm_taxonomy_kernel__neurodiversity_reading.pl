% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dsm_taxonomy_kernel__neurodiversity_reading, []).

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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Pathologization of Neurodivergence (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   This story instantiates the neurodiversity reading of the contested DSM
 *   taxonomy kernel. The reading asserts that DSM categories pathologize
 *   natural human neurological variation (autism, ADHD, dyslexia, etc.) in
 *   order to enforce institutional behavioral conformity. Neurodivergent
 *   individuals are subjected to coercive normalization (behavioral
 *   modification, medication pressure, denial of accommodation) while
 *   beneficiary institutions (schools, employers, psychiatric authority,
 *   pharmaceutical industry, carceral systems) extract conformity compliance
 *   and economic value from the pathologization frame. The founding problem
 *   (psychiatry's search for medical legitimacy) is dead—the medical model's
 *   biological assumptions have not been empirically vindicated—yet the
 *   constraint persists because institutions depend on it. This is NOT the
 *   biomedical reading (which asserts DSM categories map to objective
 *   neurobiological disease) or the critical psychiatry reading (which
 *   asserts categories are reverse-engineered from pharmaceutical
 *   availability). This reading is specific: the constraint's function is to
 *   pathologize variation as deficiency so that institutions can demand
 *   conformity as treatment rather than accommodate difference. The
 *   claim/metric gap is deliberate: extractiveness is high (0.79) and theater
 *   is rising (0.58 at interval end) because the constraint's justification
 *   increasingly relies on performative science—DSM revision cycles driven by
 *   committee consensus, not discovery; the founding medical model
 *   unvalidated—while its actual function is normalization enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.79).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.76).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Pathologization of Neurodivergence (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, '931aed8d-d71e-4922-bf11-b60dbe6c241c').
narrative_ontology:cs_kernel_codification('931aed8d-d71e-4922-bf11-b60dbe6c241c', fixed_text).
narrative_ontology:cs_authority_grounding('931aed8d-d71e-4922-bf11-b60dbe6c241c', extraction).
narrative_ontology:cs_interpretation_layer_present('931aed8d-d71e-4922-bf11-b60dbe6c241c').
narrative_ontology:cs_reading_relation('931aed8d-d71e-4922-bf11-b60dbe6c241c', dsm_taxonomy_kernel__biomedical_reading, forecloses).
narrative_ontology:cs_reading_relation('931aed8d-d71e-4922-bf11-b60dbe6c241c', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('931aed8d-d71e-4922-bf11-b60dbe6c241c', foundational, neurodivergence_is_natural_variation).
narrative_ontology:cs_axiom_status(neurodivergence_is_natural_variation, holdable).
narrative_ontology:cs_axiom_grounding('931aed8d-d71e-4922-bf11-b60dbe6c241c', neurodivergence_is_natural_variation, deontological).
narrative_ontology:cs_axiom('931aed8d-d71e-4922-bf11-b60dbe6c241c', foundational, institutional_conformity_demands_unjustified).
narrative_ontology:cs_axiom_status(institutional_conformity_demands_unjustified, holdable).
narrative_ontology:cs_axiom_grounding('931aed8d-d71e-4922-bf11-b60dbe6c241c', institutional_conformity_demands_unjustified, deontological).
narrative_ontology:cs_axiom('931aed8d-d71e-4922-bf11-b60dbe6c241c', secondary, medical_model_lacks_biological_validity).
narrative_ontology:cs_axiom_status(medical_model_lacks_biological_validity, holdable).
narrative_ontology:cs_axiom_grounding('931aed8d-d71e-4922-bf11-b60dbe6c241c', medical_model_lacks_biological_validity, empirically_contingent).
narrative_ontology:cs_reference_frame('931aed8d-d71e-4922-bf11-b60dbe6c241c', neurodiversity_self_determination_framework).
narrative_ontology:cs_drift_state('931aed8d-d71e-4922-bf11-b60dbe6c241c', contemporary_neurodiversity_movement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('931aed8d-d71e-4922-bf11-b60dbe6c241c', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_conformity_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_diagnostic_authority).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autism_community).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_community).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, dyslexia_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dsm_taxonomy_kernel__neurodiversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness scores high (0.79 at interval end) because pathologization itself is a form of harm: the individual is told their neurology is disordered, requiring intervention, justifying denial of accommodation. The extraction is the forced choice between medication, behavioral modification, or institutional exclusion. Suppression is high (0.76) because the constraint persists through medical authority, institutional gatekeeping (diagnosis required for accommodations, services, and legal recognition), and internalized pathologization—neurodivergent people are taught to experience their own neurology as deficiency. Theater rising from 0.42 to 0.58 over the interval reflects the growing gap between the diagnostic system's scientific rhetoric (DSM revisions presented as empirical discovery) and the revealed reality of committee-driven category construction and pharmaceutical influence. Accessibility collapse is substantial (0.72): once someone is diagnosed, institutional pathways narrow to normalization or exclusion; alternatives (self-directed neurodiversity frameworks, accommodation models) are not available within institutional contexts. Resistance is substantial (0.68) from neurodiversity advocates and self-advocates, though they face systematic exclusion from diagnostic authority and their evidence is treated as activism, not science.
 *
 * PERSPECTIVAL GAP:
 *   The psychiatric authority and institutional beneficiaries (schools, employers) experience this constraint as genuine coordination with incidental extraction: the DSM enables better treatment matching, informs institutional accommodations, and provides scientific legitimacy to clinical practice. Neurodivergent individuals and advocates experience it as extraction masquerading as coordination: the pathology frame forecloses alternative readings (neurodiversity as variation, not disorder) and justifies coercive normalization. The constraint's persistence depends on authority asymmetry: psychiatry controls what counts as evidence and legitimate framework. From the authority's seat, resistance is noise; from the target's seat, it is truth-telling forbidden. The engine should compute this as a tangled_rope with substantial d divergence across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals sit at the victim end of directionality (d near 1.0): they bear the costs of pathologization (coercive interventions, denial of accommodation, self-pathologization) and have identity-locked exit (their neurology is ineradicable; exit requires either hiding or institutional abandonment). Institutional beneficiaries (schools, employers, psychiatric authority, pharmaceutical industry) sit at the beneficiary end (d near 0.0): they extract conformity compliance and economic value without running the diagnostic apparatus themselves (psychiatrists maintain it, institutions use it). Parents and disability communities sit in the middle (d near 0.5): some benefit from diagnosis (access to services), others are harmed by it (their child is pathologized). The per-seat computation will show divergence: from the psychiatric authority's seat, the DSM is genuine coordination enabling scientific communication and treatment matching; from neurodivergent individuals' seats, it is coercive extraction enforcing conformity. This divergence is structural and should be computed by the engine; the commentary reflects the likely seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This is NOT mandatrophy: the constraint's founding problem is dead (psychiatry's medical legitimacy is unvalidated), but the constraint persists because institutions depend on it to enforce conformity without explicitly negotiating behavioral demands. A true mandate obsolescence would mean the constraint serves no function; here it serves institutional interests very clearly. The theater ratio rising (0.42 to 0.58) suggests increasing performative maintenance—diagnostic revisions dressed as scientific discovery while actually reflecting field politics and pharmaceutical influence—but the performativity is supporting extraction, not replacing it. This is a tangled_rope that is becoming more theatrical, not a piton maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of the measured suppression (0.76) is structural (institutional gatekeeping, denial of alternatives, pharmaceutical/behavioral mandates) versus internalized (neurodivergent individuals'' own internalized pathology narratives that persist after institutional pressure is removed)?',
    'Longitudinal study of individuals who exit institutional contexts (leave school, change employers, decline medication) and measure whether suppression and self-pathologization persist. Comparison with individuals who adopt neurodiversity framings despite institutional pathologization.',
    'If internalized suppression is substantial (>0.4), the constraint''s effective suppression is higher than the structural measure suggests—the target carries pathologization with them after exit, requiring identity reconstruction work. This would suggest the constraint operates partly through identity capture, not just external enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Internalized pathology narrative persistence after institutional exit').

omega_variable(
    medical_model_biological_validity,
    'Do DSM diagnostic categories map to objective, discoverable neurobiological disease entities, or are they socially constructed categories that reflect institutional interests and field politics?',
    'Meta-analysis of biological validity studies (twin studies, neuroimaging, genetic research, pharmacological treatment specificity) and analysis of DSM revision history (how categories were added, removed, expanded across editions; funding sources; committee composition).',
    'If biological validity is low and revision driven by institutional/pharmaceutical interests (high confidence), the founding problem (psychiatry''s medical legitimacy) is definitively dead and the constraint operates purely as normalization enforcement. If biological validity is defensible, the neurodiversity reading becomes weaker and the biomedical reading more plausible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_model_biological_validity, empirical, 'Whether DSM categories have biological validity or are social constructs').

omega_variable(
    pharmaceutical_industry_dependency,
    'How much of the DSM''s current form and revision trajectory is directly influenced by pharmaceutical industry funding, marketing priorities, and the therapeutic technologies available to treat various conditions?',
    'Audit of funding sources for DSM-5 task force members, analysis of pharmaceutical marketing expenditure correlated with diagnostic expansion, comparison of pharmacological availability with category revision timelines.',
    'If pharmaceutical influence is substantial, the constraint operates as a market-creation mechanism disguised as scientific taxonomy—supporting the critical psychiatry reading and strengthening the extraction narrative (pharmaceutical industry as primary beneficiary). If influence is minimal, the constraint is more defensible as genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_industry_dependency, empirical, 'Extent of pharmaceutical industry influence on DSM categories and revisions').

omega_variable(
    institutional_necessity_of_pathology_frame,
    'Do schools, employers, and social institutions require the pathology frame (neurodivergence-as-disorder) to function, or can they accommodate variation under alternative frames (neurodiversity-as-natural-difference)?',
    'Natural experiments from organizations and institutions that have adopted neurodiversity-affirming practices (some tech companies, some autism-specialized schools, some disability-centered organizations): measure whether institutional function, productivity, and participant satisfaction improve, decline, or remain stable relative to pathology-framed peers.',
    'If institutions can function and adapt under neurodiversity frames without loss of coordination, the pathology frame is unnecessary and the constraint operates purely as power maintenance by institutional beneficiaries. If institutions genuinely require the pathology frame (conformity demand is the only sustainable coordination mechanism), the extraction is a side effect of legitimate coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_necessity_of_pathology_frame, conceptual, 'Whether institutional function requires the pathology frame or whether alternatives are structurally viable').

omega_variable(
    neurodiversity_self_determination_boundary,
    'Where is the line between accommodation (respecting neurodivergent self-determination in how they live and work) and medical treatment necessity (intervention required for genuine harm prevention)? Who decides this boundary and on what grounds?',
    'Analysis of neurodivergent self-advocate testimony about what they experience as harmful vs. as difference, combined with examination of which DSM diagnoses have genuine harm-prevention rationales vs. which are primarily normalization tools.',
    'If most DSM diagnoses lack clear harm-prevention rationales and operate primarily as normalization tools, the pathology frame is revealed as unnecessary coercion. If some diagnoses have defensible harm-prevention grounds (e.g., severe self-injury in autism), the boundary question becomes diagnostic: which categories are legitimate medical conditions, which are normalization enforcement?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neurodiversity_self_determination_boundary, preference, 'Whether DSM diagnoses target genuine harm or primarily target difference from norms').

omega_variable(
    kernel_reading_alternative_framings,
    'The neurodiversity reading treats DSM categories as pathologizing variation for institutional conformity. The biomedical reading treats them as mapping to objective disease. The critical psychiatry reading treats them as reverse-engineered from pharmaceutical availability. These three readings are not just different empirical claims—they instantiate different authority structures and different beneficiary sets. Which reading captures the actual structural function of the DSM?',
    'Integrated analysis combining empirical evidence (biological validity, pharmaceutical influence, revision history), institutional analysis (who benefits from which frame, which frame enables which policies), and epistemic analysis (which frame is internally coherent with actual evidence and which requires evidentiary suppression).',
    'This is the master omega: which reading the evidence supports determines the constraint''s type and the appropriate policy response. Neurodiversity reading + high evidence = tangled_rope (genuine coordination function in nosology, but extraction through pathologization). Biomedical reading + high evidence = rope (legitimate medical taxonomy). Critical psychiatry reading + high evidence = snare (pharmaceutical market construction). The three readings may be partially true (some categories map to disease, some are pharmaceutical marketing), which would require decomposition into multiple constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Which kernel reading (neurodiversity/biomedical/critical psychiatry) is the DSM actually instantiating').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t0, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dsm__tr_t5, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement(dsm__tr_t10, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(dsm__tr_t15, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(dsm__tr_t22, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 22, 0.56).
narrative_ontology:measurement(dsm__tr_t30, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dsm__be_t0, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(dsm__be_t5, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(dsm__be_t10, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(dsm__be_t15, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(dsm__be_t22, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 22, 0.77).
narrative_ontology:measurement(dsm__be_t30, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 30, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t0, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(dsm__su_t5, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(dsm__su_t10, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(dsm__su_t15, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(dsm__su_t22, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 22, 0.75).
narrative_ontology:measurement(dsm__su_t30, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 30, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.12).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_diagnostic_expansion).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_self_advocacy_movement).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, special_education_eligibility_gate).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_medication_coercion).

% DUAL FORMULATION NOTE:
% The DSM taxonomy kernel admits at least three structurally distinct readings instantiating different constraints with different ε values, beneficiary/victim sets, and type classifications. This story (neurodiversity reading) asserts pathologization as the extraction mechanism and institutional conformity as the beneficiary function. Sibling stories (biomedical_reading, critical_psychiatry_reading) contest the structural function and would declare different victims, beneficiaries, and ε values. The three are linked via network.affects_constraints because each reading's empirical case affects the credibility and applicability of the others. Do not merge them into one constraint story—ε-invariance requires separate treatment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, institutional, 0.15).
constraint_indexing:directionality_override(dsm_taxonomy_kernel__neurodiversity_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
