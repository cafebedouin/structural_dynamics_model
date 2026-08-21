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
 *   This constraint story instantiates the 'academic freedom' reading of the
 *   tenure contract kernel. From this perspective, tenure is a vital
 *   mechanism that protects faculty from external political and institutional
 *   pressures, thereby enabling robust truth-seeking and high-risk inquiry.
 *   It functions as a coordination mechanism that stabilizes academic careers
 *   and fosters intellectual independence. The metrics reflect this
 *   protective function, showing low base extractiveness (from faculty) and
 *   high suppression (of external interference).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.2).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.7).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Freedom via Tenure Contract").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '2ab161e7-9016-49c9-98a8-2fcc26654c5f').
narrative_ontology:cs_kernel_codification('2ab161e7-9016-49c9-98a8-2fcc26654c5f', formalized).
narrative_ontology:cs_authority_grounding('2ab161e7-9016-49c9-98a8-2fcc26654c5f', practice).
narrative_ontology:cs_interpretation_layer_present('2ab161e7-9016-49c9-98a8-2fcc26654c5f').
narrative_ontology:cs_reading_relation('2ab161e7-9016-49c9-98a8-2fcc26654c5f', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ab161e7-9016-49c9-98a8-2fcc26654c5f', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('2ab161e7-9016-49c9-98a8-2fcc26654c5f', foundational, academic_freedom_is_foundational_for_truth).
narrative_ontology:cs_axiom_status(academic_freedom_is_foundational_for_truth, holdable).
narrative_ontology:cs_axiom_grounding('2ab161e7-9016-49c9-98a8-2fcc26654c5f', academic_freedom_is_foundational_for_truth, deontological).
narrative_ontology:cs_axiom('2ab161e7-9016-49c9-98a8-2fcc26654c5f', secondary, long_term_research_requires_job_security).
narrative_ontology:cs_axiom_status(long_term_research_requires_job_security, holdable).
narrative_ontology:cs_axiom_grounding('2ab161e7-9016-49c9-98a8-2fcc26654c5f', long_term_research_requires_job_security, instrumental).
narrative_ontology:cs_reference_frame('2ab161e7-9016-49c9-98a8-2fcc26654c5f', post_1940_aaup_principles).
narrative_ontology:cs_drift_state('2ab161e7-9016-49c9-98a8-2fcc26654c5f', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2ab161e7-9016-49c9-98a8-2fcc26654c5f', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, non_tenured_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from arbitrary dismissal, enabling them to pursue controversial research and express unpopular views without fear of job loss. They benefit from long-term stability and intellectual autonomy.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    powerful, generational, constrained, national).

% Bear the costs of pre-tenure insecurity and intense scrutiny, often working under precarious conditions for years with no guarantee of permanent protection. They aspire to tenure's benefits but are currently subject to its gatekeeping.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, non_tenured_faculty, payer,
    powerless, biographical, constrained, national).

% Responsible for upholding tenure contracts and defending academic freedom, even when it conflicts with institutional interests or external pressures. They manage the process and bear the reputational and financial costs of defending controversial faculty.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a robust intellectual environment, access to diverse perspectives, and research unconstrained by political or corporate influence. They are indirect beneficiaries of the truth-seeking function.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, mobile, local).

% Are constrained in their ability to directly influence or suppress academic research and teaching that might challenge their agendas. They bear the 'cost' of not being able to exert direct control over faculty employment and speech.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    institutional, immediate, trapped, national).

% Monitor the state of academic freedom and advocate for policies that protect it, recognizing its role in societal progress and democratic discourse. They analyze the constraint's operation and its impact on the broader intellectual commons.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, public_knowledge_advocates, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates academic careers and research agendas by providing long-term job security, allowing faculty to pursue high-risk, long-term, or controversial inquiry without fear of reprisal, thereby fostering intellectual independence and truth-seeking.
% TRANSFER_FUNCTION: Transfers job security and intellectual autonomy to faculty members, in exchange for their long-term commitment to research, teaching, and service, and in return for the societal benefit of unconstrained inquiry. It also transfers the cost of defending academic freedom to the institution and society.
% ABSENT_VOICES: External political actors and special interest groups who would prefer to control academic narratives are structurally excluded from direct influence over tenured faculty. They would argue for greater accountability to immediate public or political demands.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, academic institutions would fundamentally reorganize. Faculty would face increased precarity, research agendas would become more susceptible to external pressures, and the nature of academic inquiry would shift towards short-term, less controversial topics, severely impacting the pursuit of truth and intellectual independence.
% FOUNDING_PROBLEM: Political and institutional interference in academic inquiry, leading to the suppression of inconvenient truths, intellectual stagnation, and the chilling effect on faculty speech and research.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom organizations (e.g., AAUP), historical accounts of faculty dismissals for political reasons, and contemporary reports on legislative and donor interference in university affairs corroborate that the threat to academic freedom remains live, even if its manifestations evolve.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness is low (0.2) because, from this reading, tenure primarily serves to protect faculty from extraction and interference, acting as a subsidy for intellectual independence. Suppression is high (0.7) because tenure actively suppresses the ability of external actors and even internal administration to arbitrarily dismiss faculty or dictate research agendas. Theater ratio is low (0.1) as the core function of protecting academic freedom remains genuine, though it faces ongoing challenges. Resistance is moderate-high (0.6) reflecting the continuous external pressure and attempts to undermine tenure's protections.
 *
 * PERSPECTIVAL GAP:
 *   The 'academic freedom' reading emphasizes the protective and enabling aspects of tenure, leading to a classification as a Rope. Sibling readings, such as 'institutional extraction' or 'demographic reproduction', would highlight different structural aspects (e.g., resource hoarding, gatekeeping) and thus yield different base extractiveness values and classifications. The engine's per-seat classification would show tenured faculty as net beneficiaries, while external political actors would experience high effective extraction due to their inability to control academic output.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries, gaining job security and intellectual autonomy (low d). Students are also beneficiaries, gaining from unconstrained research and teaching (low d). External political actors and institutional administrators, who seek greater control, are the targets of tenure's protective function (high d). Non-tenured faculty are payers, bearing the costs of precarity in the hope of future benefits (moderate d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid instantiation of the ''academic_freedom_reading'' of the ''tenure_contract'' kernel?',
    'Comparison with historical documents (e.g., AAUP 1940 Statement of Principles) and contemporary defenses of academic freedom by relevant organizations.',
    'If not, the entire analysis of this reading''s structural properties would be misattributed, requiring re-evaluation under a different kernel or as a standalone constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this story''s identity as a specific reading of the tenure kernel.').

omega_variable(
    structural_delta_institutional_extraction,
    'How does the structural function of tenure as a protector of academic freedom (this reading) differ from its function as a mechanism for institutional extraction (sibling reading)?',
    'Empirical analysis of resource allocation within universities, faculty salaries vs. contingent labor costs, and the impact of tenure on institutional flexibility and innovation.',
    'If the extractive function is dominant, the base extractiveness of the ''tenure_contract'' kernel would be higher, and this reading''s classification as a Rope would be challenged by the engine''s computation of the ''institutional_extraction_reading'' as a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_institutional_extraction, empirical, 'Distinguishes academic freedom from institutional rent-seeking.').

omega_variable(
    structural_delta_demographic_reproduction,
    'How does the structural function of tenure as a meritocratic protector of truth-seeking (this reading) differ from its function as a mechanism for demographic reproduction (sibling reading)?',
    'Statistical analysis of faculty hiring, promotion, and tenure rates across demographic groups, and qualitative studies of ''fit'' and ''collegiality'' criteria in tenure decisions.',
    'If the demographic reproduction function is dominant, the ''tenure_contract'' kernel''s base extractiveness would be higher for marginalized groups, and this reading''s classification would be challenged by the engine''s computation of the ''demographic_reproduction_reading'' as a Snare or Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_demographic_reproduction, empirical, 'Distinguishes academic freedom from gatekeeping and demographic bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1940, tenure_contract__academic_freedom_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(tenu_tr_t1960, tenure_contract__academic_freedom_reading, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__academic_freedom_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__academic_freedom_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__academic_freedom_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__academic_freedom_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1940, tenure_contract__academic_freedom_reading, base_extractiveness, 1940, 0.15).
narrative_ontology:measurement(tenu_be_t1960, tenure_contract__academic_freedom_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__academic_freedom_reading, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__academic_freedom_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__academic_freedom_reading, base_extractiveness, 2010, 0.21).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__academic_freedom_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1940, tenure_contract__academic_freedom_reading, suppression_requirement, 1940, 0.6).
narrative_ontology:measurement(tenu_su_t1960, tenure_contract__academic_freedom_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__academic_freedom_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__academic_freedom_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__academic_freedom_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__academic_freedom_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
