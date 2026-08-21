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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes tenure peer review through the lens of
 *   demographic gatekeeping, where criteria like 'fit' and 'collegiality' are
 *   used to reproduce the existing composition of faculty, often at the
 *   expense of underrepresented groups. This is one reading of the broader
 *   'tenure_contract' kernel, which is also interpreted as protecting
 *   academic freedom or enabling institutional extraction. The high
 *   extractiveness and suppression reflect the systemic exclusion and career
 *   costs borne by victims, while the rising theater ratio indicates that
 *   justifications for the process become increasingly performative over
 *   time.
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
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'afc90038-bfa7-4332-a7cc-3573f50fe462').
narrative_ontology:cs_kernel_codification('afc90038-bfa7-4332-a7cc-3573f50fe462', formalized).
narrative_ontology:cs_authority_grounding('afc90038-bfa7-4332-a7cc-3573f50fe462', practice).
narrative_ontology:cs_interpretation_layer_present('afc90038-bfa7-4332-a7cc-3573f50fe462').
narrative_ontology:cs_reading_relation('afc90038-bfa7-4332-a7cc-3573f50fe462', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('afc90038-bfa7-4332-a7cc-3573f50fe462', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('afc90038-bfa7-4332-a7cc-3573f50fe462', foundational, merit_is_objective_and_universal).
narrative_ontology:cs_axiom_status(merit_is_objective_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('afc90038-bfa7-4332-a7cc-3573f50fe462', merit_is_objective_and_universal, empirically_contingent).
narrative_ontology:cs_axiom('afc90038-bfa7-4332-a7cc-3573f50fe462', secondary, collegiality_ensures_productive_environment).
narrative_ontology:cs_axiom_status(collegiality_ensures_productive_environment, holdable).
narrative_ontology:cs_axiom_grounding('afc90038-bfa7-4332-a7cc-3573f50fe462', collegiality_ensures_productive_environment, conventional).
narrative_ontology:cs_reference_frame('afc90038-bfa7-4332-a7cc-3573f50fe462', meritocratic_reproduction_ideal).
narrative_ontology:cs_drift_state('afc90038-bfa7-4332-a7cc-3573f50fe462', contemporary_diversity_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('afc90038-bfa7-4332-a7cc-3573f50fe462', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, marginalized_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the tenure process, setting broad guidelines and approving departmental recommendations. Benefits from a stable faculty but faces external pressure for diversity, often leading to performative actions rather than structural change. The gatekeeping function, while not explicitly intended, contributes to institutional stability and avoids internal conflict from challenging existing power structures.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Participates in peer review committees, evaluating candidates based on criteria like 'fit' and 'collegiality'. Benefits from the reproduction of their own demographic and intellectual composition, maintaining existing power structures and norms within departments. Exit is constrained by career path dependence but they are largely insulated from the negative effects of the gatekeeping.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary,
    powerful, biographical, constrained, national).

% Are evaluated by the tenure system, often facing subjective criteria that disadvantage them. They bear the cost of exclusion, lost career opportunities, and the emotional toll of navigating biased systems. Their identity is often deeply tied to their academic aspirations, making exit difficult despite the high costs.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, identity_locked, national).

% Includes tenured faculty from underrepresented groups who may experience the system as hostile or unsupportive, despite having achieved tenure. They pay through increased service burdens, tokenism, and the emotional labor of navigating and challenging the system from within. Their exit options are constrained by their investment in their academic careers.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, marginalized_scholars, payer,
    moderate, biographical, constrained, national).

% Includes internal and external groups pushing for greater equity and inclusion in academia. They observe the demographic reproduction and advocate for systemic changes to tenure criteria and processes. Their efforts meet resistance, and their exit options are constrained by their commitment to the cause.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_advocates, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the selection and retention of faculty based on academic merit, research productivity, and collegial fit, ensuring a stable and high-quality academic workforce for the institution.
% TRANSFER_FUNCTION: Transfers career opportunities, institutional resources, and academic power from underrepresented and marginalized faculty candidates to demographically dominant faculty groups, perpetuating existing hierarchies.
% ABSENT_VOICES: Numerous talented scholars from underrepresented backgrounds who are pushed out of academia or choose not to pursue tenure-track careers due to the perceived biases and gatekeeping functions of the system. Their experiences and perspectives are largely absent from the internal discourse on tenure reform.
% DISAPPEARANCE_RATIONALE: If tenure peer review vanished overnight, the mechanisms for faculty selection and retention would fundamentally change. While it might open doors for more diverse hiring, it would also introduce new forms of instability, potentially leading to increased political interference in hiring and a loss of job security for existing faculty, forcing a complete reorganization of academic labor markets.
% FOUNDING_PROBLEM: To ensure academic quality, protect intellectual independence, and provide job security for scholars, preventing arbitrary dismissal based on political or institutional whims, thereby fostering high-risk, long-term research.
% FOUNDING_PROBLEM_CORROBORATION: University administrations and many tenured faculty corroborate the 'live' status of the founding problem, emphasizing the ongoing need to protect academic freedom and quality. Critics, including underrepresented scholars and institutional theorists, attest that the problem is 'dead' or subverted, arguing that the system now primarily serves to reproduce existing power structures rather than protect inquiry, citing demographic data and studies on implicit bias.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   The high extractiveness (0.82) reflects the significant career and opportunity costs imposed on underrepresented faculty candidates who are denied tenure due to subjective criteria. Suppression (0.78) is high because the system actively excludes alternatives and challenges to its internal norms, making it difficult for marginalized scholars to gain entry or thrive. The theater ratio (0.65) is substantial because the stated goals of meritocracy and collegiality often serve as a cover for implicit biases and the reproduction of existing power structures, with 'fit' becoming a performative justification for exclusion. Accessibility collapse is moderate (0.60) as alternative academic paths are limited, but non-academic careers exist. Resistance (0.55) is present from diversity advocates and marginalized scholars, but often insufficient to overcome institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of demographically dominant faculty and university administration, the tenure system may be seen as a legitimate mechanism for maintaining quality and collegiality (a 'rope' or 'tangled_rope'). However, from the perspective of underrepresented faculty candidates and marginalized scholars, the same system operates as a 'snare,' actively extracting opportunities and reproducing inequality. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty are clear beneficiaries, as the system reproduces their group's composition and power. University administration also benefits from a stable, predictable (albeit homogenous) faculty, reducing internal conflict. Underrepresented faculty candidates and marginalized scholars are the primary targets, bearing the costs of exclusion and systemic bias. Diversity advocates act as observers and agents of resistance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_of_fit_and_collegiality,
    'To what extent are ''fit'' and ''collegiality'' criteria objective measures of academic contribution versus subjective proxies for demographic or cultural similarity?',
    'Quantitative analysis of tenure decisions correlated with demographic data, blinded peer review studies, and qualitative research on committee deliberations and implicit bias.',
    'If these criteria are found to be highly subjective and correlated with demographic reproduction, it strengthens the ''snare'' classification and calls for structural reform of evaluation processes. If they are demonstrably objective, it weakens the gatekeeping claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjectivity_of_fit_and_collegiality, empirical, 'Ambiguity of subjective tenure criteria.').

omega_variable(
    intended_vs_emergent_gatekeeping,
    'Is the demographic reproduction an intended function of the tenure system, or an emergent, unintended consequence of otherwise neutral processes?',
    'Historical analysis of tenure policy evolution, interviews with decision-makers, and examination of institutional statements versus actual outcomes. This is a conceptual distinction with empirical grounding.',
    'If intended, it points to a more deliberate and entrenched ''snare''. If emergent, it suggests a ''tangled_rope'' where a coordination function has been subverted by unaddressed biases, requiring different intervention strategies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intended_vs_emergent_gatekeeping, conceptual, 'Intentionality of demographic gatekeeping.').

omega_variable(
    academic_freedom_vs_diversity_tension,
    'Does prioritizing demographic diversity in tenure decisions inherently conflict with the principle of academic freedom, or can both be simultaneously upheld?',
    'Philosophical and legal analysis of academic freedom doctrines in relation to equity principles, and empirical studies of institutions that have successfully integrated both goals.',
    'If a fundamental conflict exists, it forces a ''preference'' choice between two valued goods. If they are compatible, it suggests that the ''academic_freedom_reading'' of tenure is not necessarily foreclosed by this ''demographic_reproduction_reading'', but rather influenced by it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_diversity_tension, preference, 'Tension between academic freedom and diversity goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__demographic_reproduction_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(tenu_tr_t1995, tenure_contract__demographic_reproduction_reading, theater_ratio, 1995, 0.48).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__demographic_reproduction_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(tenu_tr_t2005, tenure_contract__demographic_reproduction_reading, theater_ratio, 2005, 0.6).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__demographic_reproduction_reading, theater_ratio, 2010, 0.63).
narrative_ontology:measurement(tenu_tr_t2015, tenure_contract__demographic_reproduction_reading, theater_ratio, 2015, 0.64).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__demographic_reproduction_reading, theater_ratio, 2020, 0.65).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(tenu_be_t1995, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1995, 0.69).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(tenu_be_t2005, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2005, 0.76).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(tenu_be_t2015, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2020, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(tenu_su_t1995, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(tenu_su_t2005, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(tenu_su_t2015, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2020, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three distinct readings of the 'tenure_contract' kernel. Each reading focuses on a different structural function and has a distinct epsilon value and stakeholder configuration. They are linked to capture their interdependencies within the broader academic system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
