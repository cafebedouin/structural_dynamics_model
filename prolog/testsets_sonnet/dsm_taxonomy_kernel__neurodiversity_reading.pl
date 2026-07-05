% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: dsm_taxonomy_kernel__neurodiversity_reading
 *   human_readable: DSM Taxonomy as Institutional Conformity Enforcement (Neurodiversity Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction
 *
 * SUMMARY:
 *   This story instantiates the neurodiversity reading of the DSM taxonomy
 *   kernel: the claim that diagnostic categories for conditions like autism,
 *   ADHD, and oppositional defiant disorder encode institutional behavioral
 *   norms (classroom seating, workplace scheduling, disciplinary compliance)
 *   as if they were objective markers of pathology. Under this reading, the
 *   coordination function is real (institutions need a shared sorting
 *   vocabulary) but the sorting criteria are calibrated to institutional
 *   convenience rather than to functional harm intrinsic to the individual,
 *   producing asymmetric extraction: institutions gain funding, liability
 *   protection, and disciplinary legitimacy, while diagnosed individuals bear
 *   stigma, coercive normalization pressure, and — paradoxically — must
 *   accept the same pathologizing label as the price of accessing legal
 *   accommodation. This is a DISTINCT constraint from the biomedical reading
 *   (which holds ε low, near-mountain, because it treats the categories as
 *   tracking discoverable neurobiological entities) and from the critical
 *   psychiatry reading (which holds ε high but locates the mechanism in
 *   pharmaceutical market construction rather than institutional-conformity
 *   enforcement). The three readings are not the same constraint measured
 *   three ways; they are three different structural claims about what
 *   generates the DSM's persistence, each with its own ε, victim set, and
 *   beneficiary set. See kernel_context and the reading_relations below for
 *   the disambiguation.
 *
 * KEY AGENTS:
 *   - public_school_systems: primary agenda_setter/beneficiary — uses diagnostic sorting for funding and discipline
 *   - employers_requiring_standardized_performance: beneficiary — externalizes workplace design costs onto diagnosed individuals
 *   - carceral_and_disciplinary_institutions: beneficiary — converts social-control problems into medical ones
 *   - autistic_individuals, adhd_diagnosed_individuals, children_labeled_oppositional_defiant: primary payers — bear coercive normalization and stigma
 *   - neurodivergent_adults_denied_accommodation: payer trapped in a double bind — must accept the pathologizing label to access accommodation protections
 *   - neurodiversity_advocates: excluded from DSM revision process
 *   - clinical_researchers: analytical observers, structurally embedded in the same nosological framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.72).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.68).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, tangled_rope).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Institutional Conformity Enforcement (Neurodiversity Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'fa57e29a-7651-45de-9ebe-f8a74ed3f79c').
narrative_ontology:cs_kernel_codification('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', formalized).
narrative_ontology:cs_authority_grounding('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', expertise).
narrative_ontology:cs_interpretation_layer_present('fa57e29a-7651-45de-9ebe-f8a74ed3f79c').
narrative_ontology:cs_reading_relation('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', dsm_taxonomy_kernel__critical_psychiatry_reading, influences).
narrative_ontology:cs_axiom('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', foundational, variation_is_not_intrinsically_pathological).
narrative_ontology:cs_axiom_status(variation_is_not_intrinsically_pathological, holdable).
narrative_ontology:cs_axiom_grounding('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', variation_is_not_intrinsically_pathological, deontological).
narrative_ontology:cs_axiom('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', foundational, diagnostic_thresholds_track_institutional_convenience).
narrative_ontology:cs_axiom_status(diagnostic_thresholds_track_institutional_convenience, holdable).
narrative_ontology:cs_axiom_grounding('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', diagnostic_thresholds_track_institutional_convenience, empirically_contingent).
narrative_ontology:cs_reference_frame('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', categorical_deficit_taxonomy).
narrative_ontology:cs_drift_state('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', post_neurodiversity_movement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('fa57e29a-7651-45de-9ebe-f8a74ed3f79c', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standardized_performance).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_disciplinary_institutions).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, special_education_administrators).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_oppositional_defiant).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_adults_denied_accommodation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses DSM categories (ADHD, ODD, autism spectrum) to sort students into disciplinary and special-education tracks, triggering funding formulas and liability protections. Applies diagnostic labels to manage classroom behavior that deviates from seated-attention norms, and can compel medication or behavioral compliance as a condition of continued mainstream enrollment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, public_school_systems, beneficiary).

% Relies on DSM-anchored fitness-for-duty and accommodation frameworks to determine who is 'impaired' versus merely different, externalizing the cost of workplace design onto the diagnosed individual rather than restructuring rigid schedules or communication norms.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, employers_requiring_standardized_performance, beneficiary,
    institutional, generational, arbitrage, national).

% Uses diagnostic labels (conduct disorder, oppositional defiant disorder) to justify segregation, restraint, and disciplinary escalation for behavior that departs from institutional compliance expectations, converting a social-control problem into a medical one.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, carceral_and_disciplinary_institutions, beneficiary,
    institutional, civilizational, arbitrage, national).

% Administers diagnostic-linked funding and staffing formulas; benefits from stable diagnostic categories that justify budget lines, but is also constrained by the same categories when they fail to match a given child's actual needs.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, special_education_administrators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dsm_taxonomy_kernel__neurodiversity_reading, special_education_administrators, agenda_setter).

% Subjected to diagnostic criteria written around deficits relative to neurotypical social norms rather than around functional harm; face coercive normalization therapies (e.g., compliance-based behavioral interventions), loss of self-determination in treatment decisions, and exclusion from accommodation when their variation does not fit the diagnostic profile as institutions apply it.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, autistic_individuals, payer,
    powerless, biographical, trapped, national).

% Diagnosed largely on the basis of failure to sustain attention within institutional structures (classroom seating, standardized testing, office schedules) rather than intrinsic dysfunction; medication is frequently offered as the condition for continued institutional participation, with limited practical alternative given school and workplace structuring.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, adhd_diagnosed_individuals, payer,
    powerless, biographical, constrained, national).

% Diagnosed for resisting authority structures; the diagnostic label itself becomes the mechanism by which institutional non-compliance is converted into a treatable defect, foreclosing inquiry into whether the authority structure or the environment is the actual source of conflict.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, children_labeled_oppositional_defiant, payer,
    powerless, biographical, trapped, national).

% Must obtain and maintain a DSM-anchored diagnosis to access legal accommodation protections, meaning the same taxonomy that pathologizes their variation is also the sole gateway to relief from institutional conformity demands — a double bind structural to this reading.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_adults_denied_accommodation, payer,
    moderate, biographical, constrained, national).

% Argue diagnostic categories should be reframed as descriptions of variation requiring environmental accommodation rather than disorders requiring correction. Historically excluded from DSM revision committees, which remain dominated by clinicians and institutional stakeholders rather than by those diagnosed.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, excluded,
    organized, generational, constrained, national).

% Study diagnostic reliability, comorbidity patterns, and outcome data; positioned to observe whether diagnostic categories track functional impairment independent of institutional context, but often trained within and dependent on the same nosological framework they might critique.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, clinical_researchers, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides institutions (schools, employers, courts) with a shared vocabulary to identify individuals whose behavior or cognition departs from operational norms, enabling triage of resources (special education funding, accommodation processing, disciplinary tracking) without each institution independently re-deriving behavioral standards.
% TRANSFER_FUNCTION: Moves the cost of institutional inflexibility (rigid schedules, standardized instruction, uniform disciplinary expectations) onto diagnosed individuals in the form of stigma, coercive treatment, and the labor of seeking accommodation, while institutions retain funding, liability protection, and disciplinary legitimacy that flow from the diagnostic sorting.
% ABSENT_VOICES: Neurodivergent individuals and their self-organized advocacy movements have had minimal representation on DSM revision task forces historically; the diagnostic criteria are written substantially by clinicians observing behavior from an institutional-normativity standpoint rather than by the people whose neurology is being classified.
% DISAPPEARANCE_RATIONALE: If DSM categories vanished overnight, schools would lose their primary sorting mechanism for special education funding and disciplinary tracking, employers would lose their fitness-for-duty framework, and courts would lose a basis for competency and accommodation determinations — institutions would need to build alternative (likely more individualized, more expensive) systems for managing behavioral variation, or would revert to unmediated discretionary exclusion.
% FOUNDING_PROBLEM: Clinicians needed a shared diagnostic vocabulary to communicate about patients experiencing genuine distress or functional impairment, and researchers needed stable categories to study prevalence and treatment efficacy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of psychiatry and disability-studies scholars outside the psychiatric establishment (e.g., work documenting the shifting boundaries of ADHD and autism diagnostic criteria across DSM editions) attest that diagnostic thresholds have moved to track institutional and insurance-reimbursement pressures rather than new neurobiological findings; the APA task forces that author revisions are drawn substantially from the same clinical-institutional apparatus that benefits from stable diagnostic categories, so self-report from the benefiting parties alone would not be independent corroboration.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72) because, under this reading, the diagnostic threshold itself is the harm mechanism — pathologization forecloses self-determination in treatment and identity even before any accommodation is granted or denied. Suppression is authored moderately high (0.68) reflecting both structural suppression (institutions can compel behavioral compliance or medication as conditions of continued participation) and internalized suppression (diagnosed individuals often adopt the deficit framing as self-understanding). Theater ratio rises over the interval (0.2 to 0.4) as diagnostic criteria proliferate (DSM-III to DSM-5) while underlying institutional accommodation for genuine neurological variation changes comparatively little — more diagnostic activity, not proportionally more structural accommodation. Suppression_requirement is tracked separately because enforcement machinery (IEP compliance requirements, workplace fitness-for-duty documentation, disciplinary tracking systems) visibly hardened across the interval, independent of the extraction trend.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional beneficiaries (schools, employers, carceral systems) sit near the full-beneficiary end of directionality: they collect funding, legitimacy, and liability protection through the diagnostic sorting mechanism without bearing its costs. Diagnosed individuals sit near the full-target end: trapped or constrained exit, bearing stigma and coercive normalization. The double bind of neurodivergent_adults_denied_accommodation is structurally important — they cannot simply exit the diagnostic framework because it is also their only legal path to accommodation, which is why their exit_options are authored as 'constrained' rather than 'trapped': partial leverage exists but only by accepting the pathologizing frame the reading contests.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) preserves the fact that a genuine coordination problem exists — institutions do need some shared vocabulary to allocate finite accommodation resources — while still naming the asymmetric extraction that this reading holds is built into how that vocabulary was calibrated. Classifying this as a pure snare would deny any coordination function and make the framework unable to explain why diagnosed individuals sometimes actively seek diagnosis (for accommodation access); classifying it as a pure rope would erase the coercive normalization and stigma this reading identifies as the primary harm. The tangled_rope preserves both halves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diagnostic_criteria_norm_dependence,
    'Do DSM behavioral criteria for conditions like ADHD and autism identify functional impairment independent of the institutional context in which the behavior is observed, or are the criteria themselves calibrated against institutional behavioral expectations (classroom attention spans, workplace schedules) such that the ''impairment'' is partly an artifact of the environment being held fixed?',
    'Cross-cultural and cross-institutional prevalence studies: if diagnostic rates for the same underlying neurological profile vary sharply with schooling structure, work environment rigidity, or cultural tolerance for variation, that supports the norm-dependence claim; if rates and functional impairment remain stable across institutional contexts, that supports the biomedical reading instead.',
    'If criteria are substantially norm-dependent, this reading''s high extractiveness score is well-grounded and the beneficiary set (institutions requiring conformity) is correctly identified as capturing value from a contingent rather than natural sorting mechanism. If criteria are substantially context-independent, this reading collapses toward the biomedical reading and the extractiveness score would need revision downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diagnostic_criteria_norm_dependence, empirical, 'Whether diagnostic criteria track institutional norms or context-independent neurobiology.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the DSM taxonomy kernel best understood through a single dominant reading, or do biomedical, critical-psychiatry, and neurodiversity mechanisms operate simultaneously and non-exclusively across different diagnostic categories (e.g., biomedical mechanisms may dominate for some conditions, institutional-conformity mechanisms for others)?',
    'Category-by-category structural analysis: some DSM categories (e.g., certain neurodegenerative conditions) may show strong biomedical grounding while others (e.g., ODD, ADHD) show strong institutional-conformity signatures; a fully disaggregated analysis would need to decompose the kernel further than three readings.',
    'If the mechanisms are category-specific rather than kernel-wide, the three sibling readings may each be more accurately understood as applying to different subsets of DSM categories rather than as three total competing accounts of the same taxonomy, which would motivate further decomposition per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three kernel readings apply uniformly across all DSM categories or only to specific diagnostic subsets.').

omega_variable(
    accommodation_gateway_paradox,
    'Does the fact that legal accommodation protections require a DSM diagnosis undermine or support the neurodiversity reading''s claim that the taxonomy is primarily an instrument of institutional conformity enforcement?',
    'Comparative analysis of accommodation regimes that do not require formal diagnosis (e.g., functional-needs-based accommodation models) versus diagnosis-gated regimes, tracking whether removing the diagnostic gateway increases or decreases actual accommodation delivered.',
    'If diagnosis-gated accommodation delivers materially more support than functional-needs models, the pathologizing framework may be serving an access-enabling function that partially offsets its stigmatizing cost, which would argue for revising the extractiveness score downward or noting a genuine (if imperfect) coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_gateway_paradox, empirical, 'Whether the diagnosis-as-accommodation-gateway function offsets or compounds the constraint''s extractive character.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(dsm__tr_t2017, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2017, 0.38).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(dsm__be_t2017, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2017, 0.7).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(dsm__su_t2017, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__neurodiversity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dsm_taxonomy_kernel__neurodiversity_reading, 0.08).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__biomedical_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dsm_taxonomy_kernel. The biomedical_reading treats DSM categories as tracking objective neurobiological entities (low ε, Mountain-leaning). The critical_psychiatry_reading treats them as reverse-engineered from pharmaceutical treatment availability to construct drug markets (high ε, pharmaceutical manufacturers as primary beneficiary). This neurodiversity_reading treats them as institutional-conformity enforcement mechanisms (high ε, schools/employers/carceral systems as primary beneficiary). Per the ε-invariance principle, these are three distinct constraints sharing a contested kernel, not one constraint measured three ways — each carries its own stable ε and its own beneficiary/victim structure. All three should be cross-linked via affects_constraints in their respective files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
