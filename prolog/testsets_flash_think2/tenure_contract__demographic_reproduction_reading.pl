% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint describes the 'demographic reproduction' reading of the
 *   tenure contract, focusing on how tenure peer review, through subjective
 *   criteria like 'fit' and 'collegiality,' functions as a gatekeeping
 *   mechanism that reproduces the demographic composition of dominant faculty
 *   groups. The claimed type is 'snare' because the coordination story
 *   (ensuring academic excellence) is seen as cover for an extractive
 *   function that disproportionately harms underrepresented groups. The
 *   metrics reflect high extraction and suppression, with a rising theater
 *   ratio as justifications become more performative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.82).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'e190dbd0-5563-4bf9-b103-9f7914857e97').
narrative_ontology:cs_kernel_codification('e190dbd0-5563-4bf9-b103-9f7914857e97', formalized).
narrative_ontology:cs_authority_grounding('e190dbd0-5563-4bf9-b103-9f7914857e97', lineage).
narrative_ontology:cs_interpretation_layer_present('e190dbd0-5563-4bf9-b103-9f7914857e97').
narrative_ontology:cs_reading_relation('e190dbd0-5563-4bf9-b103-9f7914857e97', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('e190dbd0-5563-4bf9-b103-9f7914857e97', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('e190dbd0-5563-4bf9-b103-9f7914857e97', foundational, demographic_homogeneity_ensures_stability).
narrative_ontology:cs_axiom_status(demographic_homogeneity_ensures_stability, holdable).
narrative_ontology:cs_axiom_grounding('e190dbd0-5563-4bf9-b103-9f7914857e97', demographic_homogeneity_ensures_stability, conventional).
narrative_ontology:cs_axiom('e190dbd0-5563-4bf9-b103-9f7914857e97', foundational, subjective_criteria_are_objective_proxies).
narrative_ontology:cs_axiom_status(subjective_criteria_are_objective_proxies, holdable).
narrative_ontology:cs_axiom_grounding('e190dbd0-5563-4bf9-b103-9f7914857e97', subjective_criteria_are_objective_proxies, conventional).
narrative_ontology:cs_reference_frame('e190dbd0-5563-4bf9-b103-9f7914857e97', traditional_academic_peer_review).
narrative_ontology:cs_drift_state('e190dbd0-5563-4bf9-b103-9f7914857e97', contemporary_diversity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e190dbd0-5563-4bf9-b103-9f7914857e97', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, junior_faculty_of_color).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, women_in_stem_fields).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, diversity_equity_inclusion_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These faculty members, often from historically overrepresented groups, set and interpret the 'fit' and 'collegiality' criteria in peer review. They benefit from the reproduction of a faculty body that aligns with their existing networks and cultural norms, maintaining their dominant position and influence within the institution and discipline.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary).

% Candidates from underrepresented demographic groups who are subject to tenure review. They bear the cost of subjective evaluation criteria that often disadvantage them, leading to disproportionate rates of denial and forcing them out of academic careers they have invested heavily in.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, identity_locked, national).

% Benefits from a stable, predictable faculty composition, which can be perceived as less disruptive than a rapidly diversifying one. While often publicly committed to diversity, the administration benefits from the inertia of the existing system, which minimizes internal conflict over faculty composition.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, beneficiary,
    institutional, generational, mobile, national).

% These groups and individuals within and outside academia advocate for reforms to tenure processes to address demographic disparities. They bear the cost of continuous advocacy against institutional inertia and resistance, often facing backlash or slow progress.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_equity_inclusion_advocates, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, diversity_equity_inclusion_advocates, payer).

% Faculty members from underrepresented racial and ethnic groups who are currently on the tenure track. They navigate the tenure process, often facing additional service burdens and implicit biases in evaluation, making their path to tenure more precarious than their demographically dominant peers.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, junior_faculty_of_color, payer,
    moderate, biographical, constrained, local).

% Female faculty in STEM disciplines, who often face unique challenges in tenure review, including biases related to gender, work-life balance, and perceptions of 'fit' within male-dominated fields. They contribute to the pool of victims of demographic gatekeeping.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, women_in_stem_fields, payer,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates the selection and retention of faculty who are deemed 'best fit' to contribute to the long-term academic and cultural environment of the institution and discipline.
% TRANSFER_FUNCTION: Transfers long-term job security, status, and institutional resources to a select group of faculty, often reproducing existing demographic patterns, and denies these to others, effectively transferring opportunities and career stability away from underrepresented groups.
% ABSENT_VOICES: Academics who have left the profession due to perceived bias in tenure review, scholars advocating for radical structural changes to academic labor, and those who are systematically excluded from the tenure track due to these gatekeeping mechanisms.
% DISAPPEARANCE_RATIONALE: If tenure peer review as a demographic gatekeeping mechanism vanished overnight, universities would be forced to adopt new, potentially more objective and transparent, evaluation systems. This would likely lead to a significant shift in faculty demographics, a re-evaluation of 'fit' criteria, and a more diverse, though potentially less stable, academic workforce.
% FOUNDING_PROBLEM: To ensure academic excellence and protect intellectual freedom by granting permanent positions to scholars who have demonstrated significant contributions and are committed to the institution, thereby fostering a stable environment for high-risk, long-term inquiry.
% FOUNDING_PROBLEM_CORROBORATION: University administrators and many tenured faculty assert that the founding problem of ensuring academic freedom and excellence is still live. However, critics, including scholars in critical university studies, labor economists, and DEI advocates, argue that the mechanism has been co-opted to serve other ends, citing persistent demographic disparities and the subjective nature of 'fit' criteria as evidence that the original problem is either solved or the solution has been perverted.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.82, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.82) reflects the denial of long-term career stability and institutional resources to qualified candidates from underrepresented groups. Suppression (0.78) is high due to the institutional power of tenured faculty and administration to enforce subjective criteria, effectively limiting career paths for those who do not 'fit' the dominant mold. The rising theater ratio (0.65) indicates that while formal processes exist, the actual function of 'fit' and 'collegiality' increasingly serves to maintain demographic homogeneity rather than purely academic merit. Accessibility collapse is moderate (0.60) as alternative academic careers are limited, and resistance is moderate (0.55) from DEI advocates and affected groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of demographically dominant faculty and university administration, tenure review is a legitimate process for maintaining academic standards and institutional stability. From the perspective of underrepresented faculty and DEI advocates, the same process is a mechanism of exclusion and demographic reproduction. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty and university administration are beneficiaries, as they maintain their positions and institutional stability. Underrepresented faculty candidates, junior faculty of color, and women in STEM fields are victims, bearing the costs of exclusion and career precarity. DEI advocates are observers/payers, expending effort to challenge the system. The 'identity_locked' exit option for underrepresented candidates reflects the deep personal and professional investment in academia, making exit extremely costly.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_vs_bias_in_fit_criteria,
    'To what extent are ''fit'' and ''collegiality'' criteria genuinely meritocratic assessments of academic contribution versus proxies for demographic similarity or cultural alignment?',
    'Quantitative analysis of tenure review outcomes correlated with demographic data, blinded peer review experiments, and qualitative studies of committee deliberations to identify patterns of bias.',
    'If ''fit'' is primarily a proxy for demographic similarity, the extractiveness and suppression metrics are robust. If it''s genuinely meritocratic, the constraint''s classification might shift towards a ''rope'' or ''tangled_rope'' with lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merit_vs_bias_in_fit_criteria, empirical, 'Assessing the true nature of subjective tenure criteria.').

omega_variable(
    tenure_purpose_ambiguity,
    'Is the primary function of tenure to protect academic freedom and foster excellence, or has it become primarily a mechanism for demographic reproduction and institutional inertia?',
    'Longitudinal studies comparing academic output and diversity outcomes in institutions with and without tenure, or with reformed tenure systems. Policy analysis of legal challenges and legislative reforms.',
    'If the latter, the ''snare'' classification is strongly supported. If the former, the constraint might be reclassified as a ''tangled_rope'' or ''rope'' with a higher coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_purpose_ambiguity, conceptual, 'Ambiguity regarding the core purpose of tenure in contemporary academia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__demographic_reproduction_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(tenu_tr_t1998, tenure_contract__demographic_reproduction_reading, theater_ratio, 1998, 0.48).
narrative_ontology:measurement(tenu_tr_t2006, tenure_contract__demographic_reproduction_reading, theater_ratio, 2006, 0.55).
narrative_ontology:measurement(tenu_tr_t2014, tenure_contract__demographic_reproduction_reading, theater_ratio, 2014, 0.6).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__demographic_reproduction_reading, theater_ratio, 2020, 0.63).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__demographic_reproduction_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(tenu_be_t1998, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(tenu_be_t2006, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2006, 0.75).
narrative_ontology:measurement(tenu_be_t2014, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2014, 0.79).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(tenu_su_t1998, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(tenu_su_t2006, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(tenu_su_t2014, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2014, 0.74).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
