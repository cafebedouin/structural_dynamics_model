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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint story describes the tenure contract as a mechanism for
 *   academic freedom, decoupling researcher survival from institutional
 *   displeasure or political backlash to enable high-risk inquiry. It is one
 *   reading of the 'tenure_contract' kernel, focusing on its function in
 *   protecting intellectual independence. The metrics reflect a relatively
 *   low-extraction, low-suppression coordination mechanism from this
 *   perspective, though with a slight increase in extractiveness and theater
 *   in recent decades due to external pressures.
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Primary beneficiaries, gaining intellectual autonomy and job security.
 *   - Students: Indirect beneficiaries, gaining from diverse and high-quality research.
 *   - Public Discourse: Abstract beneficiary, gaining from unfettered truth-seeking.
 *   - University Administrators: Agenda-setters, tasked with upholding tenure protections.
 *   - Political Actors Seeking Control: Payers, constrained in their ability to suppress research.
 *   - Institutional Administrators Seeking Conformity: Payers, facing reduced control over faculty.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.05).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Freedom via Tenure Contract").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '5fd80198-cfc0-46f3-943e-0b677017d1f8').
narrative_ontology:cs_kernel_codification('5fd80198-cfc0-46f3-943e-0b677017d1f8', formalized).
narrative_ontology:cs_authority_grounding('5fd80198-cfc0-46f3-943e-0b677017d1f8', lineage).
narrative_ontology:cs_interpretation_layer_present('5fd80198-cfc0-46f3-943e-0b677017d1f8').
narrative_ontology:cs_reading_relation('5fd80198-cfc0-46f3-943e-0b677017d1f8', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fd80198-cfc0-46f3-943e-0b677017d1f8', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('5fd80198-cfc0-46f3-943e-0b677017d1f8', foundational, intellectual_independence_is_foundational).
narrative_ontology:cs_axiom_status(intellectual_independence_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('5fd80198-cfc0-46f3-943e-0b677017d1f8', intellectual_independence_is_foundational, deontological).
narrative_ontology:cs_axiom('5fd80198-cfc0-46f3-943e-0b677017d1f8', foundational, unfettered_inquiry_advances_knowledge).
narrative_ontology:cs_axiom_status(unfettered_inquiry_advances_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('5fd80198-cfc0-46f3-943e-0b677017d1f8', unfettered_inquiry_advances_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('5fd80198-cfc0-46f3-943e-0b677017d1f8', post_1940_aa_statement_of_principles).
narrative_ontology:cs_drift_state('5fd80198-cfc0-46f3-943e-0b677017d1f8', contemporary_political_polarization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('5fd80198-cfc0-46f3-943e-0b677017d1f8', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, public_discourse).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, political_actors_seeking_control).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators_seeking_conformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from arbitrary dismissal, enabling them to pursue controversial research and express unpopular views without fear of job loss. This fosters intellectual independence and long-term research projects.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    powerful, biographical, mobile, national).

% Benefit from exposure to diverse perspectives and cutting-edge research that might otherwise be suppressed. They gain from a richer educational environment and the advancement of knowledge.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, constrained, local).

% Benefits from the unfettered pursuit of truth and the dissemination of knowledge, even when it challenges established norms or powerful interests. This contributes to a more informed and robust public sphere.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, public_discourse, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, public_discourse).

% Responsible for upholding tenure protections, even when faculty research or speech creates institutional displeasure or political pressure. They manage the legal and reputational risks associated with academic freedom.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Bear the cost of being unable to easily suppress research or speech that challenges their agendas. They face resistance when attempting to influence academic outcomes through political pressure or funding threats.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, political_actors_seeking_control, payer,
    powerful, immediate, trapped, regional).

% Bear the cost of reduced control over faculty hiring, firing, and research direction. They cannot easily enforce institutional conformity or silence internal dissent, which can complicate management.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, institutional_administrators_seeking_conformity, payer,
    institutional, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pursuit of knowledge by protecting researchers from short-term pressures, allowing for long-term, high-risk, and potentially controversial inquiry essential for advancing understanding.
% TRANSFER_FUNCTION: Transfers job security and intellectual autonomy to tenured faculty, in exchange for their commitment to truth-seeking and the advancement of knowledge, even at institutional or political cost.
% ABSENT_VOICES: Short-term political interests and institutional factions seeking to control research outcomes are structurally excluded from directly influencing tenured faculty's work. They would argue for greater accountability to immediate public or institutional demands.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, academic research would rapidly reorient towards safer, less controversial topics, and faculty would self-censor to protect their employment. The quality and independence of higher education would diminish, and public discourse would suffer from a lack of critical, evidence-based inquiry.
% FOUNDING_PROBLEM: The need to protect scholars from arbitrary dismissal by university authorities or external political pressures, ensuring intellectual independence and the pursuit of truth.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom organizations, faculty unions, and historical analyses of academic suppression consistently corroborate the ongoing need for tenure to protect intellectual independence. Legal precedents and public statements from diverse educational bodies outside direct beneficiaries also support this view.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   From the academic freedom perspective, tenure's extractiveness is low (0.15) because it primarily transfers security and autonomy to faculty, rather than extracting from them. Suppression is also low (0.05) as its purpose is to *prevent* suppression of inquiry. Theater ratio is low (0.1) as the core function of protecting academic freedom remains active, though some performative aspects may exist in defending it. Accessibility collapse is low (0.2) because alternatives to tenure (e.g., short-term contracts) exist but do not offer the same protections. Resistance is low (0.1) from those who benefit, but higher from those who seek to control academic output.
 *
 * PERSPECTIVAL GAP:
 *   External political actors and institutional administrators seeking conformity experience tenure as a constraint that limits their power and imposes costs, leading to a higher effective extraction from their seats. Tenured faculty, students, and public discourse, however, experience it as a beneficial coordination mechanism. The engine will compute these divergent classifications based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (d=0.0-0.1) as the constraint directly subsidizes their intellectual independence. Students and public discourse are also beneficiaries, albeit indirect (d=0.1-0.2). Political actors and institutional administrators seeking conformity are targets (d=0.8-0.9) as the constraint actively prevents them from achieving their goals of control or conformity. University administrators, while enforcing the constraint, also benefit from the legitimacy it confers on the institution, placing them closer to symmetric (d=0.4-0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading of tenure is not experiencing mandatrophy; its founding problem (protecting academic freedom) is still live, and the constraint continues to perform this function. The classification as a 'rope' from this perspective prevents mislabeling it as pure extraction, which would ignore its genuine coordination function in stabilizing truth-seeking. The slight increase in extractiveness and theater over time reflects external pressures on academic freedom, not an internal decay of the mandate itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    academic_freedom_vs_institutional_extraction,
    'Is the primary function of tenure to protect academic freedom, or has it become a mechanism for institutional rent extraction by early winners?',
    'Comparative analysis of resource allocation trends (e.g., tenured vs. contingent faculty salaries, research funding distribution) and the impact of tenure on institutional flexibility and innovation over time.',
    'If primarily extraction, the constraint would reclassify towards ''snare'' or ''tangled_rope'' for many seats, with significantly higher effective extractiveness. If academic freedom remains dominant, it retains its ''rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(academic_freedom_vs_institutional_extraction, empirical, 'Distinguishing the academic freedom function from potential extractive functions of tenure.').

omega_variable(
    academic_freedom_vs_demographic_reproduction,
    'Does tenure''s peer review process primarily evaluate research merit, or does it function as a mechanism for demographic reproduction and gatekeeping?',
    'Longitudinal studies of faculty hiring and promotion patterns, disaggregated by demographic characteristics, and analysis of ''fit'' and ''collegiality'' criteria in tenure decisions.',
    'If primarily gatekeeping, the constraint would show higher suppression and extractiveness for excluded groups, potentially reclassifying as a ''snare'' for those seats. If merit-based, it retains its ''rope'' classification for academic freedom.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_demographic_reproduction, empirical, 'Assessing whether tenure''s evaluation criteria are meritocratic or reproduce existing demographics.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of external political influence structural (legal protections) or internalized (academic norms)?',
    'Analysis of legislative attempts to curtail academic freedom and the institutional responses. If legal challenges are frequent and successful, suppression is structural. If self-censorship is prevalent even without direct threats, it''s internalized.',
    'If internalized, the effective suppression of external influence is higher than the structural measure suggests, as faculty carry the suppression with them. If purely structural, the constraint''s resilience is tied directly to legal and institutional defenses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for external political influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1940, tenure_contract__academic_freedom_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(tenu_tr_t1960, tenure_contract__academic_freedom_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__academic_freedom_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__academic_freedom_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__academic_freedom_reading, theater_ratio, 2024, 0.1).

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
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__academic_freedom_reading, suppression_requirement, 2000, 0.03).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__academic_freedom_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, university_research_funding_allocation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'tenure_contract' kernel, focusing on its role in academic freedom. The other readings ('institutional_extraction_reading', 'demographic_reproduction_reading') model different structural functions and outcomes of the same underlying contract.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
