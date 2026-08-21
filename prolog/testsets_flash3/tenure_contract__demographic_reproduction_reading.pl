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
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story models tenure peer review as a mechanism for
 *   demographic gatekeeping within higher education. It is one reading of the
 *   broader 'tenure_contract' kernel. The core claim is that subjective
 *   criteria like 'fit' and 'collegiality' are used to reproduce the existing
 *   demographic composition of faculty, rather than objectively evaluating
 *   research and teaching merit. This leads to high extraction from
 *   underrepresented groups and significant suppression of alternative career
 *   paths. The claimed type is 'snare' because the coordination story
 *   (academic quality/freedom) serves as cover for an extractive function
 *   that benefits identifiable groups and harms others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'c7c87c16-2cba-4e1c-8213-652b220d072f').
narrative_ontology:cs_kernel_codification('c7c87c16-2cba-4e1c-8213-652b220d072f', formalized).
narrative_ontology:cs_authority_grounding('c7c87c16-2cba-4e1c-8213-652b220d072f', lineage).
narrative_ontology:cs_interpretation_layer_present('c7c87c16-2cba-4e1c-8213-652b220d072f').
narrative_ontology:cs_reading_relation('c7c87c16-2cba-4e1c-8213-652b220d072f', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('c7c87c16-2cba-4e1c-8213-652b220d072f', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('c7c87c16-2cba-4e1c-8213-652b220d072f', foundational, demographic_homogeneity_as_collegiality).
narrative_ontology:cs_axiom_status(demographic_homogeneity_as_collegiality, holdable).
narrative_ontology:cs_axiom_grounding('c7c87c16-2cba-4e1c-8213-652b220d072f', demographic_homogeneity_as_collegiality, conventional).
narrative_ontology:cs_axiom('c7c87c16-2cba-4e1c-8213-652b220d072f', foundational, subjective_fit_as_quality_indicator).
narrative_ontology:cs_axiom_status(subjective_fit_as_quality_indicator, holdable).
narrative_ontology:cs_axiom_grounding('c7c87c16-2cba-4e1c-8213-652b220d072f', subjective_fit_as_quality_indicator, conventional).
narrative_ontology:cs_reference_frame('c7c87c16-2cba-4e1c-8213-652b220d072f', traditional_peer_review_meritocracy).
narrative_ontology:cs_drift_state('c7c87c16-2cba-4e1c-8213-652b220d072f', contemporary_diversity_equity_inclusion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7c87c16-2cba-4e1c-8213-652b220d072f', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_faculty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reproduction of existing demographic composition, often through subjective criteria like 'fit' and 'collegiality' that favor those already within the dominant group. Their professional identity is deeply tied to the existing system, making exit unthinkable.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Bear the costs of exclusion and biased evaluation. Despite high research productivity, they are often denied tenure based on subjective criteria, forcing them out of academia or into precarious contingent positions. Their career options are severely limited by this gatekeeping.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, trapped, national).

% Administers the tenure process, often defending it as a guarantor of academic quality and freedom. Benefits from a stable, predictable faculty body, even if it means reproducing existing demographics. Faces external pressure for diversity but internal resistance to changing tenure criteria.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of the system's rigidity, often performing much of the teaching labor with little job security, low pay, and no path to tenure. Their precarity is a direct consequence of the tenured system's limited openings and gatekeeping function.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_faculty, payer,
    powerless, immediate, constrained, local).

% Pay tuition that supports the tenured system, but may not benefit from the full diversity of perspectives or innovative research that a more open system might foster. Their direct costs are financial, but indirect costs include a less dynamic learning environment.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, students, payer,
    moderate, biographical, mobile, local).

% Evaluate universities on various metrics, including diversity and faculty qualifications. They can exert pressure for reform but often lack direct enforcement mechanisms over tenure criteria, making their role primarily advisory.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, accreditation_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the selection and retention of faculty, aiming to ensure a stable, high-quality academic workforce and protect academic standards.
% TRANSFER_FUNCTION: Transfers job security, institutional power, and control over academic discourse to a select group of tenured faculty, while transferring precarity and exclusion to underrepresented and contingent faculty.
% ABSENT_VOICES: Prospective faculty from underrepresented backgrounds, who are systematically filtered out by the tenure process, would advocate for objective, transparent evaluation criteria focused solely on research and teaching merit, and for dismantling subjective 'fit' criteria.
% DISAPPEARANCE_RATIONALE: If tenure peer review as demographic gatekeeping vanished, universities would face immediate pressure to diversify faculty, evaluation criteria would shift, and the composition of academic departments would likely change significantly over time, leading to a reorganization of academic power structures.
% FOUNDING_PROBLEM: To protect academic freedom and ensure a stable, qualified faculty immune to political pressures or arbitrary dismissal, allowing for long-term, high-risk research and teaching.
% FOUNDING_PROBLEM_CORROBORATION: University administrations and many tenured faculty attest that the founding problem of academic freedom protection is still live. However, underrepresented faculty, contingent faculty, and critical institutional theorists argue that while academic freedom is a live problem, tenure's current operation has drifted to serve demographic reproduction, with corroboration from empirical studies on faculty hiring and retention patterns.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the system systematically denies opportunities and career progression to qualified individuals based on non-meritocratic criteria, effectively extracting their potential contributions and redirecting resources. Suppression (0.78) is also high due to the limited number of tenure-track positions and the subjective, opaque nature of the review process, which makes challenging decisions extremely difficult. The theater ratio (0.65) reflects the significant performative effort dedicated to justifying the system's fairness and meritocracy, even as its actual function deviates. The increasing trend in extractiveness and theater over the interval reflects a growing divergence between the stated purpose of tenure and its observed demographic outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of demographically dominant faculty, the system ensures quality and collegiality, appearing as a 'rope' or even a 'mountain' of academic standards. From the perspective of underrepresented candidates, it is a clear 'snare' designed to exclude. The engine's classification will highlight this divergence by computing different types for these seats based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty are beneficiaries (d near 0.0) as the system favors their reproduction. Underrepresented faculty candidates and contingent faculty are clear victims (d near 1.0), bearing the costs of exclusion and precarity. University administration acts as an agenda-setter, benefiting from institutional stability but also facing pressure to maintain the system. Students are indirect payers, bearing tuition costs for a system that may not serve their best interests in terms of faculty diversity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'snare' prevents mislabeling this as a coordination mechanism (rope) or a temporary support (scaffold). The 'founding_problem_status' being 'contested' and the high 'theater_ratio' indicate that the original mandate of academic freedom protection has been substantially co-opted or overshadowed by the demographic reproduction function. The system persists not because its original problem is universally live, but because it benefits powerful incumbent groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_of_fit_criteria,
    'To what extent are ''fit'' and ''collegiality'' criteria genuinely predictive of academic success and collaboration, versus proxies for demographic similarity?',
    'Longitudinal studies correlating ''fit'' scores during tenure review with objective post-tenure metrics (e.g., citation counts, grant acquisition, interdisciplinary collaboration) across diverse faculty groups.',
    'If ''fit'' is found to be a weak or biased predictor, it would undermine the legitimacy of current tenure practices and strengthen calls for more objective evaluation, potentially reclassifying the constraint towards a pure snare. If it proves genuinely predictive, the extraction might be re-evaluated as a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjectivity_of_fit_criteria, empirical, 'Ambiguity regarding the objective validity of subjective tenure criteria.').

omega_variable(
    academic_freedom_vs_demographic_reproduction,
    'Does the protection of academic freedom inherently require a system that allows for demographic reproduction, or are these two functions separable?',
    'Conceptual analysis and case studies of alternative tenure models (e.g., post-tenure review, fixed-term renewable contracts with strong academic freedom clauses) in diverse institutions, assessing their impact on both academic freedom and demographic diversity.',
    'If separable, the ''academic_freedom_reading'' of the tenure contract would be seen as a cover for the ''demographic_reproduction_reading'', strengthening the snare classification. If inseparable, the trade-off would be acknowledged as a fundamental tension within the system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academic_freedom_vs_demographic_reproduction, conceptual, 'The conceptual tension between tenure''s stated goal of academic freedom and its observed outcome of demographic reproduction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of positions, opaque process) or internalized (underrepresented candidates self-selecting out due to perceived bias)?',
    'Post-exit career trajectory analysis for underrepresented candidates: if suppression persists after leaving academia, reclassify as partially internalized. Surveys on self-censorship and career choices among diverse PhDs.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the snare more insidious. If primarily structural, policy interventions targeting process transparency and position availability would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for underrepresented faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.6).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.76).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, university_hiring_practices).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_publishing_norms).

% DUAL FORMULATION NOTE:
% This constraint is the 'demographic_reproduction_reading' of the 'tenure_contract' kernel. It highlights the gatekeeping function, distinct from the 'academic_freedom_reading' (which focuses on protecting inquiry) and the 'institutional_extraction_reading' (which focuses on labor market rigidity and contingent faculty exploitation). All three are distinct structural claims about the same underlying institutional arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
