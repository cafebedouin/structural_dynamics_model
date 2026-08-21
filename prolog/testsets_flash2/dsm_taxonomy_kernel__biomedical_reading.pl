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
 *   human_readable: DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)
 *   domain: medical_epistemology/psychiatric_taxonomy/social_construction_of_illness
 *
 * SUMMARY:
 *   This constraint represents the 'biomedical reading' of the DSM taxonomy
 *   kernel, asserting that psychiatric categories correspond to objective
 *   neurobiological disease entities discoverable through empirical research.
 *   This reading underpins the medical model of mental illness, justifying
 *   pharmaceutical and other biological interventions. The constraint
 *   operates as a snare, extracting autonomy and resources from individuals
 *   diagnosed, while benefiting the psychiatric establishment and
 *   pharmaceutical industry. Its persistence relies on active enforcement
 *   through diagnostic authority and the suppression of alternative framings.
 *
 * KEY AGENTS:
 *   - psychiatric_establishment: Agenda-setter (institutional/constrained)
 *   - pharmaceutical_industry: Beneficiary (institutional/arbitrage)
 *   - individuals_meeting_diagnostic_criteria: Payer (powerless/identity_locked)
 *   - neurodiversity_advocates: Excluded (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dsm_taxonomy_kernel__biomedical_reading, 0.85).
domain_priors:suppression_score(dsm_taxonomy_kernel__biomedical_reading, 0.9).
domain_priors:theater_ratio(dsm_taxonomy_kernel__biomedical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dsm_taxonomy_kernel__biomedical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dsm_taxonomy_kernel__biomedical_reading, snare).
narrative_ontology:human_readable(dsm_taxonomy_kernel__biomedical_reading, "DSM Categories as Objective Neurobiological Disease Entities (Biomedical Reading)").
narrative_ontology:topic_domain(dsm_taxonomy_kernel__biomedical_reading, "medical_epistemology/psychiatric_taxonomy/social_construction_of_illness").

domain_priors:requires_active_enforcement(dsm_taxonomy_kernel__biomedical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dsm_taxonomy_kernel__biomedical_reading, '28b2da35-71e6-4a96-bcd7-07d64f116db4').
narrative_ontology:cs_kernel_codification('28b2da35-71e6-4a96-bcd7-07d64f116db4', formalized).
narrative_ontology:cs_authority_grounding('28b2da35-71e6-4a96-bcd7-07d64f116db4', lineage).
narrative_ontology:cs_interpretation_layer_present('28b2da35-71e6-4a96-bcd7-07d64f116db4').
narrative_ontology:cs_reading_relation('28b2da35-71e6-4a96-bcd7-07d64f116db4', dsm_taxonomy_kernel__neurodiversity_reading, forecloses).
narrative_ontology:cs_reading_relation('28b2da35-71e6-4a96-bcd7-07d64f116db4', dsm_taxonomy_kernel__critical_psychiatry_reading, coexists_with).
narrative_ontology:cs_axiom('28b2da35-71e6-4a96-bcd7-07d64f116db4', foundational, mental_disorders_are_brain_diseases).
narrative_ontology:cs_axiom_status(mental_disorders_are_brain_diseases, holdable).
narrative_ontology:cs_axiom_grounding('28b2da35-71e6-4a96-bcd7-07d64f116db4', mental_disorders_are_brain_diseases, empirically_contingent).
narrative_ontology:cs_axiom('28b2da35-71e6-4a96-bcd7-07d64f116db4', foundational, dsm_categories_are_valid_disease_entities).
narrative_ontology:cs_axiom_status(dsm_categories_are_valid_disease_entities, holdable).
narrative_ontology:cs_axiom_grounding('28b2da35-71e6-4a96-bcd7-07d64f116db4', dsm_categories_are_valid_disease_entities, empirically_contingent).
narrative_ontology:cs_reference_frame('28b2da35-71e6-4a96-bcd7-07d64f116db4', objective_biomedical_taxonomy).
narrative_ontology:cs_drift_state('28b2da35-71e6-4a96-bcd7-07d64f116db4', contemporary_scientific_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('28b2da35-71e6-4a96-bcd7-07d64f116db4', '').
narrative_ontology:cs_kernel_id(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_criteria).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, patients_subject_to_involuntary_treatment).
narrative_ontology:constraint_victim(dsm_taxonomy_kernel__biomedical_reading, individuals_losing_legal_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and promulgates the DSM categories, conducts research to validate them, and benefits from the authority to diagnose and treat conditions framed as objective diseases. Their professional identity and funding are tied to this framework.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment, agenda_setter,
    institutional, generational, constrained, global).

% Develops and markets psychotropic medications for conditions defined by the DSM. Benefits from the expansion of diagnostic categories and the medicalization of distress, creating new markets for their products.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Employers, schools, and legal systems use DSM diagnoses to justify interventions, accommodations, or exclusions, thereby managing non-conforming behaviors within their structures. They benefit from a seemingly objective framework for behavioral control.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, institutions_requiring_behavioral_conformity, beneficiary,
    institutional, biographical, mobile, national).

% Are labeled with diagnoses that can lead to involuntary treatment, loss of legal capacity, social stigma, and lifelong medicalization. Their identity becomes fused with the diagnosis, making exit from the medical system difficult.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_meeting_diagnostic_criteria, payer,
    powerless, biographical, identity_locked, local).

% Are compelled to undergo treatment based on diagnoses, often losing autonomy and bodily integrity. Their ability to resist is severely curtailed by legal and medical authority.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, patients_subject_to_involuntary_treatment, payer,
    powerless, immediate, trapped, local).

% Have their legal rights and decision-making authority removed or diminished due to a psychiatric diagnosis, often leading to guardianship or conservatorship. They are structurally disempowered.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, individuals_losing_legal_capacity, payer,
    powerless, biographical, trapped, local).

% Analyze and critique the DSM framework, arguing against its biomedical reductionism and highlighting its social and economic functions. They operate from an academic and advocacy position, seeking to reframe mental distress.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, critical_psychiatrists_and_scholars, observer,
    organized, generational, analytical, global).

% Argue that many DSM categories pathologize natural human variation. While they advocate for acceptance and support, their perspective is often marginalized within the dominant biomedical discourse, making their voices 'absent' from the core diagnostic process.
narrative_ontology:constraint_stakeholder(dsm_taxonomy_kernel__biomedical_reading, neurodiversity_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dsm_taxonomy_kernel__biomedical_reading, psychiatric_establishment).
narrative_ontology:fixing_cost_class(dsm_taxonomy_kernel__biomedical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized nomenclature for mental health professionals, facilitating communication, research, and treatment planning across diverse clinical settings.
% TRANSFER_FUNCTION: Transfers authority and resources to the psychiatric establishment and pharmaceutical industry by defining conditions as medical diseases, while transferring autonomy and resources away from individuals who receive diagnoses.
% ABSENT_VOICES: Neurodiversity advocates and critical psychiatry scholars are largely excluded from the DSM's core definitional processes; they would argue for a non-pathologizing, social model of distress and a critical examination of pharmaceutical influence.
% DISAPPEARANCE_RATIONALE: If the DSM taxonomy vanished overnight, the entire structure of psychiatric diagnosis, treatment, research funding, and pharmaceutical marketing would collapse. Legal frameworks for involuntary treatment and capacity assessment would lose their basis, and the medicalization of distress would be profoundly challenged, forcing a radical reorganization of mental health care and social support systems.
% FOUNDING_PROBLEM: The need for a common language and classification system for mental disorders to improve communication, facilitate research, and guide treatment decisions in psychiatry.
% FOUNDING_PROBLEM_CORROBORATION: The psychiatric establishment maintains the problem is live, citing ongoing needs for diagnostic clarity. Critical psychiatry and neurodiversity advocates, supported by sociological and historical analyses, argue the original problem has been superseded by the system's extractive functions, and the current taxonomy creates more problems than it solves.
narrative_ontology:disappearance_verdict(dsm_taxonomy_kernel__biomedical_reading, world_rearranges).
narrative_ontology:founding_problem_status(dsm_taxonomy_kernel__biomedical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dsm_taxonomy_kernel__biomedical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) reflects the profound impact of diagnosis on individuals, including involuntary treatment and loss of legal capacity. Suppression (0.90) is severe due to the institutional power of the psychiatric establishment and the legal backing for diagnoses, which actively marginalizes alternative perspectives. The theater ratio (0.20) is low, indicating that while there is genuine scientific activity, a significant portion of the effort is directed towards maintaining the biomedical framing against challenges, rather than purely objective discovery. The rising extractiveness and suppression over time reflect the increasing medicalization of distress and the expansion of diagnostic categories since DSM-III.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the psychiatric establishment, this is a legitimate scientific endeavor providing essential coordination. From the perspective of individuals receiving diagnoses, it is a highly extractive and suppressive system that pathologizes their experiences and limits their autonomy. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The psychiatric establishment and pharmaceutical industry are clear beneficiaries (low d) as they gain authority, funding, and market share. Institutions requiring behavioral conformity also benefit by having a 'scientific' basis for control. Individuals receiving diagnoses, especially those subject to involuntary treatment or loss of legal capacity, are clear targets (high d) due to the direct extraction of autonomy and resources. Neurodiversity advocates are excluded, meaning the constraint actively suppresses their alternative framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a snare prevents mislabeling this as a 'rope' or 'scaffold' by highlighting the asymmetric extraction and active suppression. While a coordination function exists (standardized nomenclature), the high extractiveness and victim declarations reveal its true nature. The 'contested' status of the founding problem further indicates that the original mandate may have atrophied, with the structure now serving primarily extractive ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_validation_status,
    'To what extent do DSM categories map to objective neurobiological disease entities, as claimed by this reading?',
    'Longitudinal empirical research identifying specific, consistent neurobiological markers for each DSM category, independent of symptom clusters.',
    'If strong empirical validation emerges, the extractiveness might be re-evaluated as a necessary cost of treating genuine disease, potentially shifting the classification towards a ''tangled_rope'' or even ''rope'' for some conditions. If validation fails, the ''snare'' classification is reinforced, highlighting the constructed nature of the categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validation_status, empirical, 'The degree to which DSM categories have objective neurobiological correlates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal/institutional barriers) or internalized (self-stigma, identity fusion with diagnosis)?',
    'Post-exit suppression trajectory: if individuals continue to experience self-stigma and limited opportunities after formal disengagement from the psychiatric system, reclassify as partially internalized suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in psychiatric diagnosis.').

omega_variable(
    framing_under_determination,
    'Is the ''biomedical reading'' the only defensible framing of the DSM taxonomy, or do alternative framings (e.g., neurodiversity, critical psychiatry) offer equally coherent, but structurally different, accounts?',
    'Analysis of the logical consistency and explanatory power of sibling readings, and the social/political factors that privilege one framing over others. This is a conceptual choice, not an empirical one.',
    'If alternative framings are equally coherent, the ''snare'' classification of this reading is reinforced by demonstrating its constructed nature and the active suppression of competing interpretations. If this reading were to be universally accepted as the only coherent one, the constraint would move closer to a ''mountain'' (a false summit, given beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'The choice of framing (biomedical vs. neurodiversity vs. critical psychiatry) fundamentally alters the constraint''s perceived structure and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dsm_taxonomy_kernel__biomedical_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm__tr_t1980, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(dsm__tr_t1990, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(dsm__tr_t2000, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(dsm__tr_t2010, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(dsm__tr_t2024, dsm_taxonomy_kernel__biomedical_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dsm__be_t1980, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(dsm__be_t1990, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(dsm__be_t2000, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(dsm__be_t2010, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(dsm__be_t2024, dsm_taxonomy_kernel__biomedical_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dsm__su_t1980, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(dsm__su_t1990, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(dsm__su_t2000, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(dsm__su_t2010, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(dsm__su_t2024, dsm_taxonomy_kernel__biomedical_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dsm_taxonomy_kernel__biomedical_reading, information_standard).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__neurodiversity_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, dsm_taxonomy_kernel__critical_psychiatry_reading).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, pharmaceutical_research_funding_priorities).
narrative_ontology:affects_constraint(dsm_taxonomy_kernel__biomedical_reading, mental_health_insurance_coverage_rules).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dsm_taxonomy_kernel'. Its structural claims differ significantly from the 'neurodiversity_reading' and 'critical_psychiatry_reading', which are modeled as separate constraints due to their distinct epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
