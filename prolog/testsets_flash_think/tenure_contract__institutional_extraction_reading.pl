% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Academic Tenure as Institutional Rent Extraction
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes academic tenure from the perspective of
 *   institutional rent extraction. It argues that tenure, while historically
 *   justified by academic freedom, now primarily functions as a mechanism for
 *   early career winners (tenured faculty) to extract permanent rents,
 *   creating employment rigidity, hindering resource reallocation, and
 *   loading costs onto contingent labor and students. The claimed type is
 *   Snare, reflecting the high extraction and identifiable victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.75).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Academic Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '35f5805e-fe03-4255-9375-77cda3bc28b1').
narrative_ontology:cs_kernel_codification('35f5805e-fe03-4255-9375-77cda3bc28b1', formalized).
narrative_ontology:cs_authority_grounding('35f5805e-fe03-4255-9375-77cda3bc28b1', extraction).
narrative_ontology:cs_interpretation_layer_present('35f5805e-fe03-4255-9375-77cda3bc28b1').
narrative_ontology:cs_reading_relation('35f5805e-fe03-4255-9375-77cda3bc28b1', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('35f5805e-fe03-4255-9375-77cda3bc28b1', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('35f5805e-fe03-4255-9375-77cda3bc28b1', foundational, tenure_as_permanent_claim_on_resources).
narrative_ontology:cs_axiom_status(tenure_as_permanent_claim_on_resources, holdable).
narrative_ontology:cs_axiom_grounding('35f5805e-fe03-4255-9375-77cda3bc28b1', tenure_as_permanent_claim_on_resources, conventional).
narrative_ontology:cs_axiom('35f5805e-fe03-4255-9375-77cda3bc28b1', foundational, labor_market_rigidity_is_inefficient).
narrative_ontology:cs_axiom_status(labor_market_rigidity_is_inefficient, holdable).
narrative_ontology:cs_axiom_grounding('35f5805e-fe03-4255-9375-77cda3bc28b1', labor_market_rigidity_is_inefficient, empirically_contingent).
narrative_ontology:cs_reference_frame('35f5805e-fe03-4255-9375-77cda3bc28b1', permanent_claim_on_resources).
narrative_ontology:cs_drift_state('35f5805e-fe03-4255-9375-77cda3bc28b1', contemporary_higher_ed_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35f5805e-fe03-4255-9375-77cda3bc28b1', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, university_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, taxpayers_donors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent claims on university resources and employment, benefiting from high job security and often higher salaries. They actively defend the tenure system, shaping its rules and interpretations. Their professional identity is deeply intertwined with their tenured status.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary).

% Bear the costs of employment rigidity, working under precarious contracts with lower pay, fewer benefits, and no job security. They are often excluded from governance and decision-making processes that affect their employment conditions. Their career path is often identity-locked to academia, despite the precarity.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, contingent_faculty, excluded).

% Pay higher tuition fees that indirectly subsidize the costs of employment rigidity and tenured faculty salaries. They experience reduced instructional investment and less flexible curricula due to resource allocation constraints. Their exit options are limited by the need for credentials.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, local).

% Administers the tenure system but bears the costs of its rigidity, struggling to reallocate resources, adapt to changing academic fields, or manage budget constraints. They are often caught between the demands of tenured faculty and the financial pressures of the institution.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter).

% Indirectly fund universities through taxes and donations, bearing the ultimate cost of institutional inefficiencies and inflated tuition. They can exert pressure through political channels or by redirecting philanthropic giving.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, taxpayers_donors, payer,
    organized, generational, mobile, national).

% Represent the evolving needs of knowledge production and pedagogical innovation. They are structurally excluded from direct influence on tenure rules, which often prioritize established fields over emerging ones, hindering necessary resource reallocation.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_disciplines, excluded,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tenure_contract__institutional_extraction_reading, academic_disciplines).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, tenure aimed to coordinate long-term research and teaching by providing job security, attracting top talent, and protecting academic freedom from external pressures. In this reading, this function is largely a cover for extraction.
% TRANSFER_FUNCTION: Transfers job security, stable income, and institutional resources from contingent labor and students (via tuition) to tenured faculty, creating a permanent claim on university budgets.
% ABSENT_VOICES: Contingent faculty, who are directly harmed by the system, are often marginalized in governance. Future generations of scholars, who face increasingly precarious career paths, and academic disciplines requiring flexible resource allocation are also effectively excluded.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, the academic labor market would undergo a radical restructuring. Universities would gain significant flexibility in resource allocation, potentially leading to more dynamic curricula and research, but also raising concerns about job security and academic freedom. The entire structure of academic employment and university finance would be fundamentally altered.
% FOUNDING_PROBLEM: To protect scholars from political interference and institutional retaliation, ensuring intellectual independence and fostering long-term, high-risk research, and to attract and retain top academic talent.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and their advocates argue the founding problem of academic freedom protection remains live. Contingent faculty, student groups, and some university administrators, supported by labor economists and institutional theorists, argue the problem is largely solved or has shifted, and tenure now primarily serves as a mechanism for rent extraction, with its original function significantly diminished or co-opted.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because tenured faculty secure permanent claims on resources and employment, often at the expense of institutional flexibility and other labor groups. Suppression is high (0.75) due to the structural rigidity of the system, which limits alternatives for contingent faculty and prevents significant resource reallocation. Theater ratio is moderate (0.45) as the rhetoric of academic freedom still provides some cover, but a substantial portion of the system's maintenance is dedicated to preserving the extractive structure rather than its original function. Accessibility collapse is high (0.7) for contingent faculty, who face severely limited career progression outside the tenured track. Resistance is moderate (0.6) from various groups, including contingent faculty organizations, student activists, and some university administrators seeking reform.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of tenured faculty, tenure is a necessary protection for intellectual inquiry and a deserved reward for scholarly achievement. From the perspective of contingent faculty, students, and many administrators, it is an outdated, extractive mechanism that perpetuates inequality and stifles institutional adaptation. The engine's classification as a Snare reflects the latter, structurally derived perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are the primary beneficiaries and agenda-setters, directly profiting from the system's stability and control over its evolution. Contingent faculty and students are clear victims, bearing the direct and indirect costs of the system. University administration is a payer, as they manage the financial and operational rigidities imposed by tenure, even as they administer the system. Taxpayers and donors are indirect payers. Academic disciplines, as abstract entities, are excluded from direct influence but are structurally impacted by the system's resistance to change.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_freedom_function_decay,
    'To what extent has the original function of protecting academic freedom atrophied, versus being co-opted or merely diminished by the extractive function?',
    'Empirical studies comparing academic freedom protections in tenured vs. non-tenured systems, and historical analysis of tenure''s impact on controversial research topics over time.',
    'If the academic freedom function is largely intact, the constraint might lean more towards a Tangled Rope; if it''s primarily theatrical cover, the Snare classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_function_decay, empirical, 'Assessing the true functional status of academic freedom protection within the tenure system.').

omega_variable(
    rigidity_source_attribution,
    'What proportion of employment rigidity and resource misallocation in higher education is directly attributable to tenure, versus other institutional factors (e.g., administrative bloat, state funding cuts)?',
    'Comparative institutional analysis across different university governance models and funding structures, isolating the causal impact of tenure policies.',
    'If tenure is a minor contributor to rigidity, its extractiveness might be lower; if it''s a primary driver, the Snare classification is further validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rigidity_source_attribution, empirical, 'Disentangling tenure''s specific contribution to institutional rigidity from other factors.').

omega_variable(
    extraction_beneficiary_breadth,
    'Is the primary extraction solely benefiting tenured faculty, or does it also diffuse to other institutional actors or indirectly benefit the university as a whole (e.g., by maintaining prestige)?',
    'Detailed financial and labor market analysis, tracing the flow of resources and benefits across all university stakeholders, including reputational gains.',
    'If benefits are more diffuse, the constraint might have a stronger (though still extractive) coordination component; if highly concentrated, the Snare classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_breadth, empirical, 'Identifying the full scope of beneficiaries of tenure''s extractive mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__institutional_extraction_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__institutional_extraction_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__institutional_extraction_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__institutional_extraction_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__institutional_extraction_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__institutional_extraction_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__institutional_extraction_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__institutional_extraction_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__institutional_extraction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__institutional_extraction_reading, suppression_requirement, 1990, 0.62).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__institutional_extraction_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__institutional_extraction_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
