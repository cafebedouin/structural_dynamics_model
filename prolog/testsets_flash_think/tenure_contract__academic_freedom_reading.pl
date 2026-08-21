% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Academic Tenure as a Guarantor of Academic Freedom
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'academic freedom' reading of the
 *   broader 'tenure_contract' kernel. From this perspective, tenure is a
 *   vital mechanism that decouples researcher survival from institutional
 *   displeasure or political backlash, thereby enabling high-risk,
 *   truth-seeking inquiry. The constraint is claimed as a Rope, as it
 *   primarily serves a coordination function for the academic enterprise,
 *   protecting its members and benefiting society through independent
 *   knowledge production. The low base extractiveness and suppression reflect
 *   its function in *preventing* extraction and suppression of academics.
 *   However, it is actively enforced against external pressures, which are
 *   identified as 'victims' because the constraint extracts their ability to
 *   control academic output.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.1).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Tenure as a Guarantor of Academic Freedom").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'b75ee6bc-0a01-4beb-8d01-9c37be650b77').
narrative_ontology:cs_kernel_codification('b75ee6bc-0a01-4beb-8d01-9c37be650b77', formalized).
narrative_ontology:cs_authority_grounding('b75ee6bc-0a01-4beb-8d01-9c37be650b77', lineage).
narrative_ontology:cs_interpretation_layer_present('b75ee6bc-0a01-4beb-8d01-9c37be650b77').
narrative_ontology:cs_reading_relation('b75ee6bc-0a01-4beb-8d01-9c37be650b77', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b75ee6bc-0a01-4beb-8d01-9c37be650b77', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('b75ee6bc-0a01-4beb-8d01-9c37be650b77', foundational, academic_freedom_is_foundational).
narrative_ontology:cs_axiom_status(academic_freedom_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('b75ee6bc-0a01-4beb-8d01-9c37be650b77', academic_freedom_is_foundational, deontological).
narrative_ontology:cs_axiom('b75ee6bc-0a01-4beb-8d01-9c37be650b77', foundational, truth_seeking_requires_independence).
narrative_ontology:cs_axiom_status(truth_seeking_requires_independence, holdable).
narrative_ontology:cs_axiom_grounding('b75ee6bc-0a01-4beb-8d01-9c37be650b77', truth_seeking_requires_independence, instrumental).
narrative_ontology:cs_reference_frame('b75ee6bc-0a01-4beb-8d01-9c37be650b77', enlightenment_ideal_of_inquiry).
narrative_ontology:cs_drift_state('b75ee6bc-0a01-4beb-8d01-9c37be650b77', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b75ee6bc-0a01-4beb-8d01-9c37be650b77', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, society_at_large).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from arbitrary dismissal, enabling them to pursue controversial research and teaching without fear of reprisal. Their professional identity is deeply tied to this protection.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    powerful, biographical, identity_locked, national).

% Administers the tenure system, balancing the principles of academic freedom with institutional reputation, funding pressures, and external political demands. They are responsible for defending tenure.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Seek to influence research agendas, curriculum, or faculty hiring/firing decisions. Tenure acts as a barrier to their control, effectively 'extracting' their ability to exert direct influence over academic content.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    institutional, immediate, mobile, national).

% Benefit from high-quality, independent research and teaching that is not swayed by political or economic pressures, fostering a robust intellectual environment.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, mobile, local).

% Benefits from the generation of unbiased knowledge, critical inquiry, and the long-term pursuit of truth, which tenure is designed to safeguard.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, society_at_large, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, society_at_large).

% Lack the protections of tenure, bearing the costs of institutional rigidity and precarity. They are excluded from the primary benefits of this constraint, though they may contribute to the academic mission.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_faculty, excluded,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates independent research and teaching by protecting scholars from short-term political and economic pressures, fostering long-term knowledge creation and intellectual autonomy.
% TRANSFER_FUNCTION: Transfers job security and intellectual autonomy to tenured faculty, in exchange for long-term commitment to the institution and the pursuit of knowledge. It also transfers the cost of potential institutional displeasure or political backlash away from individual researchers.
% ABSENT_VOICES: Contingent faculty, who bear the costs of the system without the benefits of tenure, and external political actors, who would argue for more direct accountability and control over research agendas.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, academic institutions would likely become more susceptible to political and economic pressures, leading to a chilling effect on controversial research and a shift towards more commercially viable or politically palatable topics. The nature of knowledge production would fundamentally change.
% FOUNDING_PROBLEM: To protect scholars from arbitrary dismissal and ensure intellectual independence, particularly after historical instances of political interference in universities and the suppression of unpopular ideas.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom organizations (e.g., AAUP), historical records of political interference, contemporary reports on legislative attempts to influence university curricula, and statements from international human rights bodies corroborate that the threat to academic independence remains live.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.15) and suppression (0.10) are low because, from the perspective of academic freedom, tenure *reduces* the extraction and suppression experienced by scholars. The theater ratio is also low (0.08), indicating that its function is largely genuine, though some performative defense may occur as its legitimacy is challenged. Accessibility collapse is moderate (0.40) as tenure represents a specific, competitive career path. Resistance is low (0.10) from within the tenured academic community, but high from external actors. The increasing trend in suppression_requirement reflects the growing external pressures on academic freedom that tenure must actively resist.
 *
 * PERSPECTIVAL GAP:
 *   While this reading frames tenure as a Rope, other readings (e.g., institutional_extraction_reading, demographic_reproduction_reading) would classify the same 'tenure_contract' kernel very differently, likely as a Snare or Tangled Rope, due to different beneficiaries, victims, and perceived functions. The engine's computation of per-seat classifications will highlight this divergence, as the presence of 'victims' and 'active enforcement' will likely push the computed type towards Tangled Rope, despite the low base metrics and claimed Rope type.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty, students, and society are beneficiaries, as they gain from the protections and outcomes of academic freedom. University administration acts as an agenda-setter, balancing various pressures while upholding the system. External political actors are identified as victims because the constraint extracts their ability to directly control or suppress academic inquiry. Contingent faculty are excluded from the benefits of tenure, highlighting a structural inequity not addressed by this reading's primary function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    function_vs_dysfunction_ambiguity,
    'Is the primary function of tenure to protect academic freedom (as this reading claims), or have its dysfunctions (e.g., resource hoarding, demographic reproduction) become its dominant operational reality?',
    'Longitudinal studies tracking resource allocation shifts, demographic changes in faculty composition, and the actual impact of tenure on controversial research output versus institutional inertia.',
    'If dysfunctions dominate, the constraint would reclassify from Rope to Tangled Rope or Snare, with higher extractiveness and theater ratio, and different beneficiaries/victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_vs_dysfunction_ambiguity, empirical, 'Ambiguity between tenure''s stated purpose and its observed effects.').

omega_variable(
    sibling_reading_institutional_extraction,
    'How would the classification change if the ''institutional_extraction_reading'' of the tenure contract were adopted?',
    'Analyzing the constraint through the lens of resource flow, contingent labor exploitation, and the rigidity of tenured positions preventing reallocation.',
    'This reading would likely classify tenure as a Snare or Tangled Rope, with high extractiveness from contingent faculty and institutional flexibility, benefiting early tenured faculty and administration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_institutional_extraction, conceptual, 'Impact of the institutional extraction reading on classification.').

omega_variable(
    sibling_reading_demographic_reproduction,
    'How would the classification change if the ''demographic_reproduction_reading'' of the tenure contract were adopted?',
    'Analyzing the constraint through the lens of hiring and promotion patterns, diversity metrics, and the subjective criteria used in peer review for ''fit'' and ''collegiality''.',
    'This reading would likely classify tenure as a Tangled Rope, with extraction from underrepresented groups and beneficiaries being the dominant demographic, perpetuating existing power structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_demographic_reproduction, conceptual, 'Impact of the demographic reproduction reading on classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (of external influence) structural (e.g., legal protections) or internalized (e.g., cultural norms of academic independence)?',
    'Post-legal challenge analysis: if academic independence persists after legal protections are weakened, reclassify as partially internalized cultural norm.',
    'If internalized, the constraint''s effective suppression of external influence is higher than the structural measure suggests, as the academic community carries the resistance with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for external influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tenu_tr_t6, tenure_contract__academic_freedom_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(tenu_tr_t12, tenure_contract__academic_freedom_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(tenu_tr_t18, tenure_contract__academic_freedom_reading, theater_ratio, 18, 0.07).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__academic_freedom_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.08).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tenu_be_t6, tenure_contract__academic_freedom_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(tenu_be_t12, tenure_contract__academic_freedom_reading, base_extractiveness, 12, 0.12).
narrative_ontology:measurement(tenu_be_t18, tenure_contract__academic_freedom_reading, base_extractiveness, 18, 0.13).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__academic_freedom_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(tenu_su_t6, tenure_contract__academic_freedom_reading, suppression_requirement, 6, 0.12).
narrative_ontology:measurement(tenu_su_t12, tenure_contract__academic_freedom_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(tenu_su_t18, tenure_contract__academic_freedom_reading, suppression_requirement, 18, 0.16).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__academic_freedom_reading, suppression_requirement, 24, 0.18).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'tenure_contract' kernel. Each reading presents a different structural interpretation of tenure's function, beneficiaries, and victims, leading to different classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
