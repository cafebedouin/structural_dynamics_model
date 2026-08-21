% ============================================================================
% CONSTRAINT STORY: dsm_taxonomy_kernel__biomedical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: dsm_taxonomy_kernel__biomedical_reading
 *   human_readable: DSM Categories as Objective Neurobiological Diseases (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy
 *
 * SUMMARY:
 *   This constraint represents the 'biomedical reading' of the DSM taxonomy
 *   kernel, asserting that DSM categories correspond to objective
 *   neurobiological disease entities discoverable through empirical research.
 *   From this perspective, the DSM provides a scientific, value-neutral
 *   framework for understanding and treating mental illness. This reading
 *   underpins the authority of the psychiatric profession and the
 *   pharmaceutical industry, enabling interventions that can include
 *   involuntary treatment and impact legal capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Diseases (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '2a3394e1-b113-443f-a8c7-48c2d417d4df').
narrative_ontology:cs_kernel_codification('2a3394e1-b113-443f-a8c7-48c2d417d4df', formalized).
narrative_ontology:cs_authority_grounding('2a3394e1-b113-443f-a8c7-48c2d417d4df', expertise).
narrative_ontology:cs_interpretation_layer_present('2a3394e1-b113-443f-a8c7-48c2d417d4df').
narrative_ontology:cs_reading_relation('2a3394e1-b113-443f-a8c7-48c2d417d4df', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('2a3394e1-b113-443f-a8c7-48c2d417d4df', dsm_taxonomy_kernel__critical_psychiatry_reading, forecloses).
narrative_ontology:cs_axiom('2a3394e1-b113-443f-a8c7-48c2d417d4df', foundational, dsm_categories_map_to_objective_disease).
narrative_ontology:cs_axiom_status(dsm_categories_map_to_objective_disease, holdable).
narrative_ontology:cs_axiom_grounding('2a3394e1-b113-443f-a8c7-48c2d417d4df', dsm_categories_map_to_objective_disease, empirically_contingent).
narrative_ontology:cs_axiom('2a3394e1-b113-443f-a8c7-48c2d417d4df', foundational, psychiatric_illness_is_neurobiological).
narrative_ontology:cs_axiom_status(psychiatric_illness_is_neurobiological, holdable).
narrative_ontology:cs_axiom_grounding('2a3394e1-b113-443f-a8c7-48c2d417d4df', psychiatric_illness_is_neurobiological, empirically_contingent).
narrative_ontology:cs_reference_frame('2a3394e1-b113-443f-a8c7-48c2d417d4df', objective_disease_model).
narrative_ontology:cs_drift_state('2a3394e1-b113-443f-a8c7-48c2d417d4df', contemporary_scientific_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2a3394e1-b113-443f-a8c7-48c2d417d4df', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, applies, and enforces DSM categories, asserting their scientific validity as objective disease entities. Benefits from the authority and resources channeled through the medical model of mental illness.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, arbitrage, global).

% Profits significantly from the medicalization of conditions and the widespread prescription of psychotropic drugs, which are directly linked to DSM diagnoses.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Utilizes DSM diagnoses to manage and control non-conforming behaviors in various settings (e.g., schools, workplaces, legal systems), justifying interventions based on a medical framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_conformity, beneficiary,
    institutional, generational, mobile, national).

% Are subject to diagnosis, prescribed treatments (potentially involuntary), social stigma, and in some cases, loss of legal capacity. Their identity often becomes fused with their diagnosis, making exit from the medical system extremely difficult.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, diagnosed_individuals, payer,
    powerless, biographical, identity_locked, local).

% Bear significant emotional, financial, and caregiving burdens. They are often pressured by the medical system to ensure compliance with treatment plans, reinforcing the constraint.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, families_of_diagnosed, payer,
    moderate, biographical, constrained, local).

% Actively challenge the pathologization of natural human neurological variation, arguing for acceptance and accommodation rather than medical intervention. Their perspective is largely marginalized within the dominant biomedical framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    organized, biographical, constrained, global).

% Critique the DSM as a social construct driven by pharmaceutical interests and institutional power, rather than objective science. Their views are often dismissed as unscientific by the mainstream psychiatric establishment.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatry_scholars, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature and diagnostic criteria for mental health conditions, facilitating communication among clinicians, researchers, and insurance providers, and guiding treatment decisions.
% TRANSFER_FUNCTION: Transfers authority, funding, and legitimacy to the psychiatric establishment and pharmaceutical industry, while transferring diagnostic labels, treatment regimens, and social control to individuals who meet diagnostic criteria.
% ABSENT_VOICES: Neurodiversity advocates and critical psychiatry scholars are systematically excluded from the foundational processes of DSM development and official discourse, despite offering robust alternative framings of mental distress and difference.
% DISAPPEARANCE_RATIONALE: If the claim that DSM categories map to objective neurobiological disease entities vanished, the entire framework of psychiatric diagnosis, pharmaceutical treatment, and related legal/social interventions would lose its scientific and medical legitimacy, forcing a fundamental reorganization of mental health care and societal responses to distress.
% FOUNDING_PROBLEM: To bring scientific rigor and consistency to the classification of mental disorders, replacing subjective and inconsistent diagnostic practices with empirically grounded, operationalized criteria.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment maintains that the problem of consistent, scientific classification is still live and evolving. However, neurodiversity advocates, critical psychiatry scholars, and some sociological analyses argue that the original problem has been largely superseded by issues of social control, market creation, and the pathologization of normal variation, with the current system serving these new functions more than its original mandate.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(dsm_taxonomy_kernel__biomedical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dsm_taxonomy_kernel__biomedical_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant costs borne by diagnosed individuals and their families, including financial burdens of treatment, social stigma, and potential loss of autonomy. Suppression (0.90) is severe due to the institutional power of the medical system, legal frameworks supporting involuntary treatment, and the social pressure to conform to diagnostic labels. The theater ratio is low (0.10) because, from this reading's perspective, the scientific and clinical functions are genuinely pursued, and the claims are taken seriously, not as mere performance. Accessibility collapse is high (0.80) as diagnosis often funnels individuals into a narrow set of medicalized interventions, limiting perceived or actual alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The biomedical reading fundamentally diverges from neurodiversity and critical psychiatry perspectives. While this reading asserts objective disease, other readings frame the same categories as pathologizing natural variation or as market-driven constructs. The engine's classification will highlight how this 'scientific' claim, when operationalized, functions as a highly extractive and suppressive snare for those it diagnoses.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment, pharmaceutical industry, and institutions requiring conformity are clear beneficiaries, gaining authority, profit, and social control. Diagnosed individuals and their families are the primary targets, bearing the costs of treatment, stigma, and loss of autonomy. Neurodiversity advocates and critical psychiatry scholars are excluded, their alternative framings suppressed by the dominance of the biomedical model.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare, despite the biomedical reading's claim of scientific rigor and coordination, prevents mislabeling. It highlights that even a system claiming objective scientific grounding can function primarily as an extractive mechanism when it enables involuntary interventions, limits autonomy, and generates significant profits for its beneficiaries, while suppressing alternative understandings of human experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_objectivity_ambiguity,
    'Are DSM categories truly mapping objective neurobiological disease entities, or are they primarily social constructs influenced by cultural norms, professional consensus, and pharmaceutical interests?',
    'Discovery of definitive, specific, and reliable biomarkers for all DSM categories, or a shift in scientific consensus towards a non-categorical, dimensional understanding of mental distress.',
    'If objective entities are confirmed, the extractiveness might be re-evaluated as a necessary cost of treating genuine disease. If primarily social constructs, the classification as a Snare would be strongly reinforced, highlighting the constructed nature of the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dsm_objectivity_ambiguity, conceptual, 'Ambiguity regarding the ontological status of DSM categories.').

omega_variable(
    biomarker_discovery_status,
    'Given decades of research, does the persistent lack of definitive, specific, and reliable biomarkers for most DSM categories undermine the ''objective neurobiological disease'' claim?',
    'Consensus from independent scientific bodies (e.g., neuroscience, genetics) that the absence of biomarkers fundamentally invalidates the current categorical disease model, or conversely, the discovery of such biomarkers.',
    'If the lack of biomarkers is deemed invalidating, the ''empirically contingent'' grounding of the axioms would be challenged, potentially leading to an ''overridden'' status for the axioms and a re-evaluation of the constraint''s legitimacy. If biomarkers are found, the biomedical reading''s claims would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biomarker_discovery_status, empirical, 'Impact of biomarker research on the biomedical disease model.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by diagnosed individuals primarily structural (legal, institutional barriers) or internalized (self-stigma, identity fusion with diagnosis)?',
    'Longitudinal studies tracking individuals after formal exit from the medical system: if suppression persists significantly, it indicates a strong internalized component. If it rapidly diminishes, structural factors are dominant.',
    'If internalized suppression is a major factor, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the burden even after direct institutional pressure lessens. This would reinforce the ''identity_locked'' exit option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for diagnosed individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(dsm__tr_t2020, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(dsm__tr_t2025, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.81).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(dsm__be_t2020, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(dsm__be_t2025, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.83).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.86).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(dsm__su_t2020, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(dsm__su_t2025, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_drug_approval).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, mental_health_insurance_coverage).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
