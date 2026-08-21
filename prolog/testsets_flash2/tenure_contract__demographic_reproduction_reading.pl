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
 *   This constraint describes tenure peer review as a mechanism for
 *   demographic gatekeeping within higher education. While ostensibly
 *   designed to protect academic freedom and ensure quality, this reading
 *   highlights how subjective criteria like 'fit' and 'collegiality' are used
 *   to reproduce the existing demographic composition of faculty, often to
 *   the detriment of underrepresented groups. The claimed type is 'snare'
 *   because the coordination story (quality assurance) is seen as cover for
 *   extraction (exclusion and reproduction of privilege). This is one reading
 *   of the 'tenure_contract' kernel.
 *
 * KEY AGENTS:
 *   - demographically_dominant_faculty: Primary beneficiary (institutional/identity_locked) — benefits from preferential evaluation and reproduction of status.
 *   - underrepresented_faculty_candidates: Primary victim (powerless/trapped) — bears the costs of exclusion and biased evaluation.
 *   - university_administration: Agenda setter (institutional/constrained) — administers the process, benefits from stability, but faces pressure regarding equity.
 *   - contingent_faculty: Secondary victim (powerless/constrained) — bears the costs of limited tenured positions and precarious labor.
 *   - diversity_equity_inclusion_advocates: Observer (organized/constrained) — critiques the system and advocates for reform.
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
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'f7c05b95-7cad-4e9a-902d-06fe41b047c7').
narrative_ontology:cs_kernel_codification('f7c05b95-7cad-4e9a-902d-06fe41b047c7', formalized).
narrative_ontology:cs_authority_grounding('f7c05b95-7cad-4e9a-902d-06fe41b047c7', lineage).
narrative_ontology:cs_interpretation_layer_present('f7c05b95-7cad-4e9a-902d-06fe41b047c7').
narrative_ontology:cs_reading_relation('f7c05b95-7cad-4e9a-902d-06fe41b047c7', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('f7c05b95-7cad-4e9a-902d-06fe41b047c7', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('f7c05b95-7cad-4e9a-902d-06fe41b047c7', foundational, demographic_homogeneity_as_quality_proxy).
narrative_ontology:cs_axiom_status(demographic_homogeneity_as_quality_proxy, holdable).
narrative_ontology:cs_axiom_grounding('f7c05b95-7cad-4e9a-902d-06fe41b047c7', demographic_homogeneity_as_quality_proxy, conventional).
narrative_ontology:cs_axiom('f7c05b95-7cad-4e9a-902d-06fe41b047c7', foundational, subjective_fit_as_essential_evaluation).
narrative_ontology:cs_axiom_status(subjective_fit_as_essential_evaluation, holdable).
narrative_ontology:cs_axiom_grounding('f7c05b95-7cad-4e9a-902d-06fe41b047c7', subjective_fit_as_essential_evaluation, conventional).
narrative_ontology:cs_reference_frame('f7c05b95-7cad-4e9a-902d-06fe41b047c7', traditional_peer_review_meritocracy).
narrative_ontology:cs_drift_state('f7c05b95-7cad-4e9a-902d-06fe41b047c7', contemporary_equity_discourse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7c05b95-7cad-4e9a-902d-06fe41b047c7', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administration).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reproduction of existing demographic composition, often through subjective criteria like 'fit' and 'collegiality' during peer review. Their professional identity is deeply tied to the existing structure, making exit unthinkable.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Bear the costs of exclusion and biased evaluation during the tenure process. Their career paths are often terminated or diverted due to subjective criteria that disproportionately affect them. Exit means leaving academia or accepting precarious contingent positions.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, trapped, national).

% Administers the tenure process, often defending it as essential for academic quality and freedom, even when aware of its demographic effects. Benefits from a stable, if demographically homogenous, faculty body. Exit from the system is difficult due to institutional inertia and legal frameworks.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of a system that limits tenured positions, forcing them into precarious, low-pay, high-workload roles. They are often highly qualified but lack the institutional power to challenge the tenure system. Exit means leaving academia entirely.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_faculty, payer,
    powerless, immediate, constrained, local).

% Analyze and critique the tenure system's role in perpetuating demographic disparities. They advocate for reforms but face significant institutional resistance. Their exit options are limited to working within the existing system or leaving academia.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_equity_inclusion_advocates, observer,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the selection and retention of faculty, ostensibly to ensure academic quality and institutional stability, but in practice, it coordinates the reproduction of existing demographic and cultural norms within the faculty.
% TRANSFER_FUNCTION: Transfers long-term job security, institutional power, and academic freedom from a large pool of aspiring scholars to a smaller, often demographically homogenous, tenured faculty, while externalizing costs onto contingent labor.
% ABSENT_VOICES: Prospective faculty from underrepresented groups who are filtered out by the tenure process, and scholars advocating for more equitable and transparent evaluation methods, are largely excluded from shaping the rules of the system.
% DISAPPEARANCE_RATIONALE: If tenure peer review as demographic gatekeeping vanished, the composition of faculty would likely diversify more rapidly, evaluation criteria would shift towards more objective measures, and the power dynamics within universities would fundamentally alter, leading to a significant rearrangement of the academic labor market.
% FOUNDING_PROBLEM: The tenure system was established to protect academic freedom and ensure a stable, high-quality faculty by providing job security against arbitrary dismissal and political interference.
% FOUNDING_PROBLEM_CORROBORATION: University administrations and many tenured faculty attest that the founding problem of protecting academic freedom is still live. However, underrepresented faculty, contingent faculty, and external labor economists argue that the system's primary function has drifted to demographic reproduction and rent-seeking, with the original problem largely solved or superseded by other mechanisms.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because the system effectively transfers long-term security and power to a select group, often based on non-meritocratic factors, while imposing significant costs on those excluded. Suppression (0.78) is high due to the structural barriers to entry, the subjective nature of evaluation, and the limited alternatives for academic careers. Theater ratio (0.65) is substantial because the stated function (merit-based selection, academic freedom) increasingly serves as a cover for the actual function (demographic reproduction). Accessibility collapse is high (0.70) because once the gatekeeping mechanism is understood, alternatives for underrepresented groups within the traditional academic career path are severely limited. Resistance is moderate (0.45) but growing, primarily from organized advocacy groups and individual scholars, but it faces strong institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   Demographically dominant faculty perceive the system as fair and meritocratic, essential for maintaining standards, thus experiencing it as a 'rope' or even a 'mountain' (natural order of merit). Underrepresented candidates and contingent faculty experience it as a 'snare' due to the high extraction and suppression they face. University administration often frames it as a 'tangled rope' – a necessary coordination mechanism with acknowledged but manageable imperfections. The engine's classification as 'snare' reflects the structural reality from the perspective of those most impacted by its gatekeeping function.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty are beneficiaries (d=0.0-0.2) as the system favors their reproduction. Underrepresented candidates and contingent faculty are targets (d=0.8-1.0) as they bear the costs of exclusion and precarity. University administration is an agenda-setter (d=0.3-0.5), benefiting from institutional stability but also bearing some reputational costs and pressure for reform. DEI advocates are observers (d=0.5), analyzing the system without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling demographic gatekeeping as legitimate coordination. By identifying the high extractiveness and suppression, and the significant theater ratio, it highlights that the constraint's persistence is due to active reproduction of privilege rather than a genuine, broadly beneficial coordination function. The 'snare' classification directly challenges the 'academic freedom' justification when it serves to protect demographic closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_of_fit_criteria,
    'To what extent are ''fit'' and ''collegiality'' criteria genuinely predictive of research productivity or positive institutional contribution, versus proxies for demographic or cultural similarity?',
    'Longitudinal studies correlating subjective evaluation scores with objective post-tenure research output and institutional service, controlling for demographic factors. Blinded review processes for ''fit'' criteria.',
    'If these criteria are found to be poor predictors or demographically biased, it would further strengthen the ''snare'' classification by undermining the coordination justification and highlighting the extractive nature of the gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjectivity_of_fit_criteria, empirical, 'Assesses the empirical validity and bias of subjective tenure criteria.').

omega_variable(
    academic_freedom_vs_demographic_reproduction,
    'Does the protection of academic freedom, as currently implemented through tenure, inherently require or enable the demographic reproduction observed, or are these functions separable?',
    'Comparative analysis of tenure systems in different national contexts or institutional models that achieve academic freedom without similar demographic outcomes. Legal and policy reforms separating academic freedom protections from subjective peer review.',
    'If separable, the ''academic_freedom_reading'' would be foreclosed as a justification for the current system, reinforcing the ''snare'' classification. If inseparable, it would highlight a fundamental tension within the ''tenure_contract'' kernel itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_demographic_reproduction, conceptual, 'Examines the structural relationship between academic freedom and demographic reproduction within tenure.').

omega_variable(
    internalized_suppression_among_candidates,
    'Is the suppression experienced by underrepresented faculty candidates primarily structural (biased evaluation, limited positions) or internalized (self-censorship, discouragement from applying to certain institutions/fields)?',
    'Qualitative studies and surveys of underrepresented candidates'' experiences, including post-exit trajectories. Analysis of application patterns and self-selection biases.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than structural measures suggest, as candidates carry the suppression with them, even when structural barriers are nominally reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_among_candidates, empirical, 'Structural vs. internalized suppression mechanism for underrepresented faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.62).
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
narrative_ontology:measurement(tenu_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, university_hiring_practices).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_labor_market_precarity).

% DUAL FORMULATION NOTE:
% This constraint is the 'demographic_reproduction_reading' of the 'tenure_contract' kernel. It focuses on how tenure peer review perpetuates existing demographic compositions, contrasting with readings that emphasize academic freedom or institutional extraction. All three readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
