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
 *   human_readable: Academic Freedom via Tenure Contract
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'academic freedom' reading of the
 *   tenure contract, where tenure primarily functions to protect intellectual
 *   independence and enable high-risk, truth-seeking inquiry. It decouples
 *   researcher survival from institutional displeasure or political backlash.
 *   This reading views tenure as a coordination mechanism that benefits
 *   faculty, students, and public discourse, with minimal extraction from
 *   external political actors who cannot easily suppress inconvenient
 *   research. This is one reading of the 'tenure_contract' kernel, distinct
 *   from 'institutional_extraction_reading' and
 *   'demographic_reproduction_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.1).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Freedom via Tenure Contract").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '15d46f11-02fa-4ff8-9820-9b8eb7fbb55d').
narrative_ontology:cs_kernel_codification('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', formalized).
narrative_ontology:cs_authority_grounding('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', lineage).
narrative_ontology:cs_interpretation_layer_present('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d').
narrative_ontology:cs_reading_relation('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', foundational, intellectual_independence_is_foundational).
narrative_ontology:cs_axiom_status(intellectual_independence_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', intellectual_independence_is_foundational, deontological).
narrative_ontology:cs_axiom('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', foundational, truth_seeking_requires_protection).
narrative_ontology:cs_axiom_status(truth_seeking_requires_protection, holdable).
narrative_ontology:cs_axiom_grounding('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', truth_seeking_requires_protection, instrumental).
narrative_ontology:cs_reference_frame('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', post_1940_aa_statement_of_principles).
narrative_ontology:cs_drift_state('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('15d46f11-02fa-4ff8-9820-9b8eb7fbb55d', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from arbitrary dismissal, enabling them to pursue controversial research and express unpopular views without fear of job loss. This stability allows for long-term, high-risk projects. Exit is constrained by specialized skills and the limited number of tenured positions.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    powerful, biographical, constrained, national).

% Manages the tenure system, upholding academic freedom principles while balancing institutional reputation and funding. They benefit from the prestige of groundbreaking research but bear the cost of defending controversial faculty. Their ability to alter tenure rules is constrained by faculty governance and legal precedent.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from exposure to diverse perspectives and cutting-edge research that might not be possible without academic freedom. They are indirect beneficiaries of the truth-seeking function. Their exit options are to transfer to other institutions.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, mobile, local).

% Benefits from independent, evidence-based research and critical inquiry that informs public policy and societal understanding, unconstrained by political or corporate pressures. This is an abstract beneficiary, representing the epistemic commons.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, public_discourse, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, public_discourse).

% Bear the 'cost' of not being able to easily suppress research or speech that challenges their agendas. They face resistance when attempting to influence academic content or dismiss faculty for political reasons. Their exit options involve shifting focus to other institutions or legislative action.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    institutional, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pursuit of knowledge by protecting researchers from external pressures, ensuring that inquiry is driven by intellectual merit rather than political or financial expediency. It provides a stable environment for long-term, high-risk research.
% TRANSFER_FUNCTION: Transfers intellectual independence and job security to tenured faculty, in exchange for their commitment to truth-seeking and the advancement of knowledge, which benefits students and public discourse. It also transfers the 'cost' of unsuppressible inquiry to external political actors.
% ABSENT_VOICES: Short-term political interests or corporate entities seeking to suppress inconvenient research findings are structurally excluded from directly influencing tenured faculty. They would argue for greater accountability to immediate public or economic demands.
% DISAPPEARANCE_RATIONALE: If tenure vanished, academic research would quickly become more cautious and less critical, aligning with institutional or political agendas to ensure job security. High-risk, long-term, or controversial inquiry would diminish, fundamentally altering the nature of higher education and its contribution to society.
% FOUNDING_PROBLEM: To protect scholars from arbitrary dismissal by university administrators, political figures, or donors, ensuring intellectual freedom and the pursuit of knowledge for its own sake.
% FOUNDING_PROBLEM_CORROBORATION: Academic associations, faculty senates, and historical analyses of academic freedom cases consistently corroborate the ongoing need for tenure to protect intellectual independence. While some critics argue its function has shifted, the core problem of external pressure on research remains.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.15) because, from this reading's perspective, the primary function is protection and coordination, not rent collection. Suppression is also low (0.1) as the constraint's strength lies in its ability to resist external pressures, not to impose them on faculty. Accessibility collapse is high (0.7) because, once tenure is understood, the alternative of unprotected academic employment is seen as significantly less desirable for truth-seeking. Resistance is low (0.05) because the core function is widely accepted within the academic community. The slight upward trend in extractiveness and suppression reflects increasing external political scrutiny and attempts to influence academic institutions over time, requiring more active defense of tenure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of tenured faculty, the constraint is a pure Rope, providing essential protection. From the perspective of external political actors, it might be perceived as a mild Snare, as it prevents them from exerting control. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are direct beneficiaries (low d) due to job security and intellectual freedom. Students and public discourse are indirect beneficiaries (low d) of the high-quality, independent research. University administration is an agenda-setter, balancing institutional needs with academic freedom. External political actors are the 'payers' (high d) in the sense that they are constrained from easily suppressing research or speech they dislike, bearing the 'cost' of academic independence.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling tenure as pure extraction by focusing on its core coordination function for academic freedom. While other readings might highlight extractive aspects, this analysis emphasizes the structural role of protecting inquiry, which remains a live problem. The low theater ratio indicates that the performance of academic freedom is largely genuine, not merely a cover for other functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    truth_seeking_vs_institutional_survival,
    'To what extent does tenure genuinely enable high-risk, truth-seeking inquiry, versus merely protecting faculty from accountability for low productivity or institutional displeasure unrelated to academic freedom?',
    'Longitudinal studies correlating tenure status with research output risk-taking, publication impact, and instances of controversial but ultimately vindicated findings, controlling for field and institutional context.',
    'If tenure primarily protects low productivity, its coordination function for truth-seeking is weaker, potentially shifting its classification towards a Piton or Tangled Rope from an institutional perspective. If it strongly correlates with high-risk inquiry, its Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_seeking_vs_institutional_survival, empirical, 'Assessing the actual impact of tenure on research risk-taking and intellectual independence.').

omega_variable(
    academic_freedom_vs_other_readings,
    'Is the ''academic freedom'' function of tenure the dominant structural reality, or is it a cover story for ''institutional extraction'' or ''demographic reproduction''?',
    'Comparative analysis of the structural properties (extraction, suppression, beneficiary/victim sets) across all three readings of the ''tenure_contract'' kernel. The engine''s cross-reading classification will highlight the dominant structural pattern.',
    'If the ''institutional_extraction_reading'' or ''demographic_reproduction_reading'' shows significantly higher extractiveness and suppression, this ''academic_freedom_reading'' might be reclassified as a less dominant or even theatrical aspect of the overall tenure system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academic_freedom_vs_other_readings, conceptual, 'Ambiguity regarding the primary structural function of the tenure contract amidst competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1940, tenure_contract__academic_freedom_reading, theater_ratio, 1940, 0.02).
narrative_ontology:measurement(tenu_tr_t1960, tenure_contract__academic_freedom_reading, theater_ratio, 1960, 0.01).
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__academic_freedom_reading, theater_ratio, 1980, 0.01).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__academic_freedom_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__academic_freedom_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1940, tenure_contract__academic_freedom_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(tenu_be_t1960, tenure_contract__academic_freedom_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__academic_freedom_reading, base_extractiveness, 1980, 0.07).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__academic_freedom_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__academic_freedom_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1940, tenure_contract__academic_freedom_reading, suppression_requirement, 1940, 0.05).
narrative_ontology:measurement(tenu_su_t1960, tenure_contract__academic_freedom_reading, suppression_requirement, 1960, 0.03).
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__academic_freedom_reading, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__academic_freedom_reading, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__academic_freedom_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'tenure_contract' kernel. This 'academic_freedom_reading' focuses on the protective function for intellectual independence, while 'institutional_extraction_reading' and 'demographic_reproduction_reading' highlight other structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
