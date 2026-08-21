% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__neurodiversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: DSM Taxonomy as Pathologization of Neurodiversity
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint story represents the 'neurodiversity reading' of the DSM
 *   taxonomy kernel. From this perspective, the DSM's categories are not
 *   objective disease entities but rather social constructs that pathologize
 *   natural human neurological variation. This pathologization serves to
 *   enforce institutional behavioral norms, extracting conformity from
 *   neurodivergent individuals and benefiting systems that require such
 *   conformity (e.g., schools, workplaces, carceral systems). The constraint
 *   is classified as a Snare due to its high extractiveness (the harm of
 *   pathologization itself) and active suppression of alternative
 *   understandings of neurodiversity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__neurodiversity_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__neurodiversity_reading, 0.78).
domain_priors:theater_ratio(dsm_taxonomy_kernel__neurodiversity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__neurodiversity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__neurodiversity_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__neurodiversity_reading, "DSM Taxonomy as Pathologization of Neurodiversity").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__neurodiversity_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__neurodiversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__neurodiversity_reading, 'dcfece36-2e05-49d2-b269-f5c7616e37b4').
narrative_ontology:cs_kernel_codification('dcfece36-2e05-49d2-b269-f5c7616e37b4', formalized).
narrative_ontology:cs_authority_grounding('dcfece36-2e05-49d2-b269-f5c7616e37b4', lineage).
narrative_ontology:cs_interpretation_layer_present('dcfece36-2e05-49d2-b269-f5c7616e37b4').
narrative_ontology:cs_reading_relation('dcfece36-2e05-49d2-b269-f5c7616e37b4', dsm_taxonomy_kernel__biomedical_reading, coexists_with).
narrative_ontology:cs_reading_relation('dcfece36-2e05-49d2-b269-f5c7616e37b4', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('dcfece36-2e05-49d2-b269-f5c7616e37b4', foundational, neurodiversity_is_natural_human_variation).
narrative_ontology:cs_axiom_status(neurodiversity_is_natural_human_variation, holdable).
narrative_ontology:cs_axiom_grounding('dcfece36-2e05-49d2-b269-f5c7616e37b4', neurodiversity_is_natural_human_variation, deontological).
narrative_ontology:cs_axiom('dcfece36-2e05-49d2-b269-f5c7616e37b4', foundational, pathologization_is_a_form_of_social_control).
narrative_ontology:cs_axiom_status(pathologization_is_a_form_of_social_control, holdable).
narrative_ontology:cs_axiom_grounding('dcfece36-2e05-49d2-b269-f5c7616e37b4', pathologization_is_a_form_of_social_control, instrumental).
narrative_ontology:cs_reference_frame('dcfece36-2e05-49d2-b269-f5c7616e37b4', neurodiversity_as_natural_variation).
narrative_ontology:cs_drift_state('dcfece36-2e05-49d2-b269-f5c7616e37b4', contemporary_dsm_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('dcfece36-2e05-49d2-b269-f5c7616e37b4', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__neurodiversity_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schools, employers, carceral systems, and other institutions benefit from the DSM's categories by having a framework to label and manage behaviors that deviate from their norms, often leading to coercive normalization or denial of accommodation rather than systemic change.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, institutional_systems_of_conformity, beneficiary,
    institutional, generational, constrained, national).

% Benefits from the implicit validation of their own neurological patterns as 'normal' and 'healthy,' reinforcing social structures that privilege neurotypical functioning and often failing to recognize the value of neurodivergent perspectives.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurotypical_majority, beneficiary,
    organized, biographical, mobile, global).

% Are labeled with disorders, leading to stigmatization, medicalization of natural traits, and pressure to conform to neurotypical standards. They bear the costs of pathologization, including loss of self-determination, forced interventions, and denial of appropriate accommodations.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodivergent_individuals, payer,
    powerless, biographical, identity_locked, global).

% Actively resist the pathologizing framework of the DSM, arguing for a social model of disability and the recognition of neurodiversity as a natural form of human variation. They face institutional resistance and struggle for recognition and systemic change.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, neurodiversity_advocates, payer,
    moderate, generational, constrained, global).

% Are the primary authors and enforcers of the DSM taxonomy. While some may genuinely seek to alleviate suffering, their institutional role within the medical model often perpetuates the pathologization of neurodiversity, even when individual practitioners hold more nuanced views.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, psychiatric_professionals, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the expansion of diagnostic categories, which creates markets for psychotropic medications. This industry's influence on diagnostic criteria is a key concern for critics of the DSM.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__neurodiversity_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized language and framework for classifying mental health conditions, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions.
% TRANSFER_FUNCTION: Transfers the authority to define 'normal' and 'pathological' human experience from individuals and communities to medical and institutional systems, resulting in the pathologization of neurodivergent traits and the transfer of resources towards medical interventions.
% ABSENT_VOICES: Neurodivergent individuals and their communities, particularly those who reject the medical model of disability, are often excluded from the core processes of DSM revision and definition, despite being the primary subjects of its classifications. Their perspectives on self-determination and the social model of disability are marginalized.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the medical, educational, and carceral systems would lose their primary tool for classifying and managing 'deviant' behaviors. This would force a fundamental re-evaluation of how society addresses neurological differences, potentially leading to a shift towards accommodation and support rather than pathologization and 'cure.'
% FOUNDING_PROBLEM: To provide a common nomenclature for mental disorders, improve diagnostic reliability, and guide research and treatment in psychiatry.
% FOUNDING_PROBLEM_CORROBORATION: Psychiatric professionals and the biomedical research community largely attest that the founding problem of diagnostic reliability and treatment guidance remains live. Neurodiversity advocates and critical psychiatry scholars, from outside the benefiting parties, argue that while a common language is useful, the current taxonomy has overshot its original purpose, pathologizing normal variation and serving institutional control rather than genuine therapeutic need.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__neurodiversity_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__neurodiversity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__neurodiversity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dsm_taxonomy_kernel__neurodiversity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__neurodiversity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the very act of labeling natural variations as 'disorders' imposes significant costs on individuals, including stigma, medicalization, and pressure for normalization. Suppression is also high, as the medical-institutional complex actively resists alternative frameworks like neurodiversity, often through professional gatekeeping and the control of research funding. The theater ratio is low because the DSM still performs a genuine, albeit contested, coordination function for diagnosis and treatment, even if its underlying premises are challenged.
 *
 * PERSPECTIVAL GAP:
 *   The neurodiversity reading fundamentally diverges from the biomedical reading, which views DSM categories as objective disease entities. This creates a profound perspectival gap where what one group sees as a natural variation, the other sees as a treatable disorder. The engine's classification will highlight this divergence by computing a high extractiveness for the neurodiversity seat, contrasting with a potentially lower extractiveness for the biomedical seat (which would frame the DSM as a necessary coordination tool).
 *
 * DIRECTIONALITY LOGIC:
 *   Neurodivergent individuals are the primary victims, bearing the direct costs of pathologization. Institutional systems of conformity and the neurotypical majority are beneficiaries, as the taxonomy validates their norms and provides tools for managing deviation. Psychiatric professionals act as agenda-setters, wielding the power to define and enforce these categories. The pharmaceutical industry is an indirect beneficiary, profiting from the medicalization of conditions. Neurodiversity advocates are also payers, expending significant effort to resist the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The neurodiversity reading suggests a form of mandatrophy where the DSM's original mandate (diagnostic reliability) has been superseded by a function of social control and pathologization. The classification as a Snare prevents mislabeling this as a neutral coordination mechanism (Rope) or a temporary support (Scaffold), emphasizing the active extraction and suppression inherent in its operation from this perspective. The rising extractiveness over time in the measurements further supports this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine Snare, or is its classification as such entirely dependent on the ''neurodiversity reading'' of the DSM taxonomy kernel?',
    'Compare classifications across all sibling readings (biomedical, critical psychiatry). If other readings yield significantly different classifications (e.g., Rope or Tangled Rope), it confirms the reading-dependence. If all readings converge on Snare, it suggests a more intrinsic extractive structure.',
    'If classification is highly reading-dependent, it highlights the conceptual nature of the constraint and the power of framing. If classification converges, it strengthens the claim of intrinsic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''dsm_taxonomy_kernel''. Sibling readings include ''biomedical_reading'' and ''critical_psychiatry_reading''. This omega documents the dependence of the classification on this specific interpretive frame.').

omega_variable(
    empirical_basis_for_categories,
    'To what extent do DSM categories map to empirically verifiable, distinct neurobiological entities, independent of social context?',
    'Longitudinal neuroimaging studies, genetic research, and biomarker discovery that consistently identify discrete biological markers for DSM categories, robust across diverse populations and cultural contexts.',
    'Strong empirical corroboration would challenge the ''pathologization of natural variation'' premise, potentially lowering perceived extractiveness from this reading. Lack of corroboration would strengthen the social construction argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_basis_for_categories, empirical, 'Assesses the scientific validity of DSM categories as objective biological entities versus social constructs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of neurodiversity primarily structural (institutional barriers, lack of funding for alternative research) or internalized (neurodivergent individuals internalizing stigma and self-pathologizing)?',
    'Post-accommodation trajectory: if neurodivergent individuals continue to experience internal barriers to self-acceptance and flourishing even after structural accommodations are made, it suggests a significant internalized component. If structural changes lead to rapid improvements in well-being and self-concept, it points to primarily structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would necessitate different intervention strategies focused on cultural and psychological liberation, not just policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the context of neurodiversity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__neurodiversity_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1994, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement(dsm__tr_t2013, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2013, 0.18).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__neurodiversity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dsm__be_t1994, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 1994, 0.7).
narrative_ontology:measurement(dsm__be_t2013, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2013, 0.8).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__neurodiversity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(dsm__su_t1994, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 1994, 0.65).
narrative_ontology:measurement(dsm__su_t2013, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2013, 0.75).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__neurodiversity_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
