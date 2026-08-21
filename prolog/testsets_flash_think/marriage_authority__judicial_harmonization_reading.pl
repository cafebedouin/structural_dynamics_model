% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization of Marriage Authority
 *   domain: legal/constitutional/social
 *
 * SUMMARY:
 *   This constraint describes the process by which a nation's Supreme Court,
 *   through case-by-case review, imposes a constitutional floor on diverse
 *   personal law codes governing marriage and family. This leads to a de
 *   facto harmonization of laws without formal legislative action (e.g., a
 *   Uniform Civil Code). It is a reading of the broader 'marriage_authority'
 *   kernel, focusing on the institutional mechanism of judicial review as the
 *   primary driver of legal evolution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.65).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.8).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/social").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '98cc0bf1-3fa3-48bc-a290-14f9e2590511').
narrative_ontology:cs_kernel_codification('98cc0bf1-3fa3-48bc-a290-14f9e2590511', formalized).
narrative_ontology:cs_authority_grounding('98cc0bf1-3fa3-48bc-a290-14f9e2590511', lineage).
narrative_ontology:cs_interpretation_layer_present('98cc0bf1-3fa3-48bc-a290-14f9e2590511').
narrative_ontology:cs_reading_relation('98cc0bf1-3fa3-48bc-a290-14f9e2590511', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('98cc0bf1-3fa3-48bc-a290-14f9e2590511', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('98cc0bf1-3fa3-48bc-a290-14f9e2590511', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('98cc0bf1-3fa3-48bc-a290-14f9e2590511', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_axiom('98cc0bf1-3fa3-48bc-a290-14f9e2590511', foundational, constitutional_supremacy_over_personal_law).
narrative_ontology:cs_axiom_status(constitutional_supremacy_over_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('98cc0bf1-3fa3-48bc-a290-14f9e2590511', constitutional_supremacy_over_personal_law, deontological).
narrative_ontology:cs_axiom('98cc0bf1-3fa3-48bc-a290-14f9e2590511', secondary, judicial_review_as_harmonization_tool).
narrative_ontology:cs_axiom_status(judicial_review_as_harmonization_tool, holdable).
narrative_ontology:cs_axiom_grounding('98cc0bf1-3fa3-48bc-a290-14f9e2590511', judicial_review_as_harmonization_tool, conventional).
narrative_ontology:cs_reference_frame('98cc0bf1-3fa3-48bc-a290-14f9e2590511', constitutional_supremacy_doctrine).
narrative_ontology:cs_drift_state('98cc0bf1-3fa3-48bc-a290-14f9e2590511', contemporary_application_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('98cc0bf1-3fa3-48bc-a290-14f9e2590511', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, citizens_seeking_uniformity).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, communities_losing_autonomy).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, parties_to_overridden_laws).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court and lower courts interpret constitutional principles to establish a floor for marriage and family law, overriding conflicting local or communal codes. They benefit from expanded authority and a more coherent legal landscape.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals who benefit from the harmonization of laws, particularly those whose rights were previously restricted by diverse or discriminatory personal law codes. They gain legal certainty and expanded rights.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, citizens_seeking_uniformity, beneficiary,
    moderate, biographical, mobile, national).

% Religious or ethnic communities whose traditional or customary marriage laws are overridden or modified by judicial rulings. They bear the cost of diminished self-governance and cultural erosion.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, communities_losing_autonomy, payer,
    organized, generational, constrained, local).

% Individuals directly affected by the invalidation or modification of specific marriage arrangements under communal or personal laws. They face legal uncertainty, disruption of established norms, and potential loss of status or rights under the new framework.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, parties_to_overridden_laws, payer,
    powerless, immediate, trapped, local).

% The legislative branch, which is bypassed in this process of legal harmonization. While it could enact a Uniform Civil Code, it often defers to judicial action due to political sensitivities, effectively ceding authority to the courts.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legislature, excluded,
    institutional, generational, constrained, national).

% Academics and legal experts who analyze the evolution of marriage law through judicial review, its impact on legal pluralism, and its implications for constitutional theory and comparative family law.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Harmonizes diverse personal law codes across a nation by establishing a constitutional floor for marriage and family rights, reducing legal fragmentation and ensuring a baseline of uniform application.
% TRANSFER_FUNCTION: Transfers ultimate authority over marriage law interpretation from diverse local/communal bodies to the Supreme Court; transfers legal certainty and expanded rights to some citizens, while imposing new obligations or invalidating existing arrangements for others.
% ABSENT_VOICES: Legislatures, whose role in codifying a comprehensive Uniform Civil Code is largely bypassed, and some religious/communal leaders who advocate for the preservation of distinct personal laws but lack direct representation in the judicial process.
% DISAPPEARANCE_RATIONALE: If judicial review of marriage authority vanished, personal law codes would likely remain fragmented and potentially discriminatory, leading to inconsistent rights and legal uncertainty across jurisdictions. The constitutional floor would cease to be enforced, and the legal landscape would revert to greater pluralism without a harmonizing force.
% FOUNDING_PROBLEM: Inconsistent application of fundamental rights across diverse personal law systems, leading to inequality, legal uncertainty, and potential discrimination in marriage and family matters for certain groups of citizens.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, legal aid groups, and a significant body of legal scholarship (from outside the judiciary itself) corroborate the ongoing problem of inconsistent rights and the need for harmonization, even if they debate the optimal mechanism.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it performs a genuine coordination function (harmonizing disparate laws) but also involves significant asymmetric extraction (overriding communal autonomy, imposing new legal frameworks) and requires active enforcement by the judiciary. Extractiveness is substantial (0.65) due to the costs imposed on communities and individuals whose traditional laws are superseded. Suppression is high (0.80) because judicial rulings are binding and actively enforced, limiting alternatives. Theater ratio is low (0.10) as the judiciary's role is primarily substantive and functional, not performative. The 'scaffold-like' nature of this process, as a transitional pathway to harmonization, is acknowledged in the omegas, but the structural properties of active enforcement and extraction align better with Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and citizens seeking uniformity perceive this process as upholding constitutional rights and ensuring equality, a necessary coordination function. Conversely, communities whose traditional laws are overridden, and individuals directly impacted by these changes, experience it as an extractive imposition on their autonomy and established norms. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is a primary beneficiary and agenda-setter, gaining authority and shaping the legal landscape. Citizens seeking uniformity also benefit from expanded and consistent rights. Communities losing autonomy and parties to overridden laws are the primary targets, bearing the costs of legal change and diminished self-governance. The legislature is largely excluded, as its role in codifying law is bypassed by judicial action.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_vs_legislative_legitimacy,
    'Is judicial harmonization a legitimate and sustainable path to legal uniformity, or does it suffer from a democratic deficit compared to legislative action?',
    'Empirical studies on public acceptance and compliance with judicially-imposed changes versus legislatively-enacted reforms; analysis of political stability and long-term societal integration.',
    'If a democratic deficit is confirmed, the constraint''s legitimacy (and thus its long-term stability) is weaker, potentially increasing resistance and requiring higher suppression. If sustainable, it reinforces the judiciary''s role as a harmonizing force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_vs_legislative_legitimacy, conceptual, 'Legitimacy of judicial vs. legislative paths to legal uniformity.').

omega_variable(
    transitional_vs_permanent_scaffold,
    'Is this judicial harmonization process truly a ''scaffold'' (transitional support towards a stable, harmonized state) or has it become a permanent, ongoing mode of governance?',
    'Analysis of whether the pace of new judicial interventions slows significantly over time, or if legislative bodies begin to codify the constitutional floor, indicating a shift from judicial ''scaffold'' to legislative ''steady state''.',
    'If permanent, the ''scaffold'' framing is misleading, and the constraint''s classification might drift towards a more stable, extractive type (e.g., Snare or entrenched Tangled Rope) if the extraction persists without a clear end-state in sight. If transitional, it retains its Scaffold-like function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_vs_permanent_scaffold, empirical, 'Whether judicial harmonization is a temporary or permanent mode of governance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1960, marriage_authority__judicial_harmonization_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority__judicial_harmonization_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__judicial_harmonization_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__judicial_harmonization_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__judicial_harmonization_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1960, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(marr_be_t1975, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(marr_be_t1990, marriage_authority__judicial_harmonization_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__judicial_harmonization_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1960, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(marr_su_t1975, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(marr_su_t1990, marriage_authority__judicial_harmonization_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2005, 0.77).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__judicial_harmonization_reading, suppression_requirement, 2025, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, focusing on the judicial mechanism of harmonization. Its ε value differs significantly from other readings, which focus on communal autonomy, legislative action, or specific rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
