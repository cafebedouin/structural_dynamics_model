% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet System for Marriage Authority
 *   domain: legal/political/comparative_law
 *
 * SUMMARY:
 *   This constraint describes a system of legal pluralism where marriage
 *   authority is deliberately fragmented among different religious or
 *   communal legal codes, rather than centralized under a single state law.
 *   This 'federalist millet' reading frames the fragmentation as a
 *   consociational mechanism to prevent majoritarian domination and ensure
 *   political stability in a diverse society. It is distinct from a 'communal
 *   autonomy' reading by emphasizing the elite-bargain and anti-tyranny
 *   function over pure community self-governance. The system is maintained by
 *   political elites who see legislative paralysis on personal law as a
 *   feature, not a bug.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.25).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet System for Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/comparative_law").

domain_priors:requires_active_enforcement(marriage_authority__federalist_millet_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'b8994309-4262-4ffa-9c41-e129db884d12').
narrative_ontology:cs_kernel_codification('b8994309-4262-4ffa-9c41-e129db884d12', formalized).
narrative_ontology:cs_authority_grounding('b8994309-4262-4ffa-9c41-e129db884d12', lineage).
narrative_ontology:cs_interpretation_layer_present('b8994309-4262-4ffa-9c41-e129db884d12').
narrative_ontology:cs_reading_relation('b8994309-4262-4ffa-9c41-e129db884d12', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8994309-4262-4ffa-9c41-e129db884d12', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8994309-4262-4ffa-9c41-e129db884d12', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8994309-4262-4ffa-9c41-e129db884d12', marriage_authority__judicial_harmonization_reading, coexists_with).
narrative_ontology:cs_axiom('b8994309-4262-4ffa-9c41-e129db884d12', foundational, fragmented_authority_prevents_majoritarian_tyranny).
narrative_ontology:cs_axiom_status(fragmented_authority_prevents_majoritarian_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('b8994309-4262-4ffa-9c41-e129db884d12', fragmented_authority_prevents_majoritarian_tyranny, instrumental).
narrative_ontology:cs_axiom('b8994309-4262-4ffa-9c41-e129db884d12', foundational, legislative_paralysis_is_stability_feature).
narrative_ontology:cs_axiom_status(legislative_paralysis_is_stability_feature, holdable).
narrative_ontology:cs_axiom_grounding('b8994309-4262-4ffa-9c41-e129db884d12', legislative_paralysis_is_stability_feature, instrumental).
narrative_ontology:cs_reference_frame('b8994309-4262-4ffa-9c41-e129db884d12', consociational_power_sharing_framework).
narrative_ontology:cs_drift_state('b8994309-4262-4ffa-9c41-e129db884d12', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b8994309-4262-4ffa-9c41-e129db884d12', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, political_elites).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, individual_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities benefit from the ability to govern marriage and family matters according to their own traditions, protecting their cultural and religious identity from majoritarian imposition. Their exit options are constrained by their embeddedness in the national political structure.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, constrained, national).

% These elites designed and maintain the fragmented system as a consociational mechanism to prevent tyranny of the majority and ensure political stability. They manage the legislative paralysis as a feature, not a bug, of the system.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, political_elites, agenda_setter,
    institutional, generational, mobile, national).

% Individuals navigate a complex and often inconsistent landscape of personal laws, which can lead to legal uncertainty or unequal treatment depending on their community affiliation. Their ability to choose a different legal regime is limited.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, individual_citizens, payer,
    moderate, biographical, constrained, local).

% Advocate for a uniform civil code and the elimination of personal law pluralism, viewing it as an impediment to national unity and individual equality. They are structurally excluded from the elite bargain that maintains the current system.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secular_reformers, excluded,
    organized, generational, constrained, national).

% Seek to reform personal laws to ensure gender equality, often through judicial intervention. They find the fragmented system resistant to comprehensive reform and are not part of the original consociational bargain.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, gender_equality_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of diverse religious and cultural communities within a single state by granting them autonomy over personal law, thereby preventing majoritarian conflict over sensitive identity issues.
% TRANSFER_FUNCTION: Transfers authority over marriage and family law from a centralized state legislature to various community-specific legal systems, in exchange for political stability and minority inclusion.
% ABSENT_VOICES: Secular reformers and gender equality advocates are largely absent from the foundational bargain and ongoing maintenance of this system. They would argue for universal individual rights and a uniform civil code, which the current system is designed to resist.
% DISAPPEARANCE_RATIONALE: If this fragmented authority disappeared, the delicate balance of power between communities would collapse, likely leading to significant political instability, inter-communal conflict, and a crisis of national identity as majoritarian laws would immediately apply to all, or a vacuum of authority would emerge.
% FOUNDING_PROBLEM: The problem of preventing majoritarian domination and ensuring political stability in a deeply pluralistic society with diverse religious and cultural communities, particularly concerning highly sensitive personal laws like marriage.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and constitutional scholars, as well as leaders of minority communities, corroborate that the problem of managing diversity and preventing majoritarian overreach remains live. While secular reformers contest the solution, the existence of the underlying problem is widely acknowledged outside the immediate beneficiaries.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the primary function is coordination and stability, not rent extraction. Any 'cost' to individuals (e.g., legal complexity) is seen as a necessary trade-off for the broader political good. Suppression is moderate (0.25) as the system actively resists attempts at centralization or uniformization, but does not typically coerce individuals within their communities. Theater ratio is low (0.1) as the system genuinely performs its function of maintaining political stability, with minimal performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   Political elites and minority communities view this system as a successful, low-extraction rope that ensures stability and protects identity. Individual citizens and excluded groups (secular reformers, gender equality advocates) experience it as a more constraining, less equitable arrangement, though not necessarily highly extractive in a financial sense. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities are primary beneficiaries (d=0.0-0.1) as their traditions are protected. Political elites are also beneficiaries (d=0.0-0.1) as the system ensures their stability. Individual citizens are payers (d=0.4-0.5) due to navigating legal complexities and potential inequalities, but are not 'victims' in the extractive sense. Secular reformers and gender equality advocates are excluded (d=0.8-0.9) as the system's design actively works against their goals.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_community_autonomy,
    'Is the persistence of fragmented marriage authority primarily due to an elite political bargain for stability, or genuine, bottom-up communal demand for self-governance?',
    'Sociological studies of community preferences vs. elite political negotiations; analysis of legislative history and constitutional debates.',
    'If primarily an elite bargain, the ''rope'' classification holds, but the ''beneficiary'' status of minority communities might be re-evaluated for potential ''payer'' aspects (e.g., costs of internal community enforcement). If genuine communal demand, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_community_autonomy, empirical, 'Distinguishing the primary driver of legal pluralism.').

omega_variable(
    legislative_paralysis_feature_or_bug,
    'Is the legislative paralysis on personal law a deliberate, functional feature of the consociational system, or an unintended, dysfunctional bug that entrenches inequalities?',
    'Analysis of policy outcomes, judicial interventions, and comparative studies of similar systems. Examination of whether the ''paralysis'' prevents tyranny or merely prevents necessary reform.',
    'If a functional feature, the ''rope'' classification is robust. If a dysfunctional bug, the ''extractiveness'' and ''suppression'' metrics might be higher for individual citizens, potentially shifting the classification towards ''tangled_rope'' for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legislative_paralysis_feature_or_bug, conceptual, 'Assessing the true nature of legislative inaction in personal law.').

omega_variable(
    federalist_millet_vs_communal_autonomy_framing,
    'Does this constraint primarily function as a federalist anti-tyranny mechanism (this reading), or as a system for pure communal self-governance (communal_autonomy_reading)?',
    'Analysis of the constitutional text''s explicit intent, judicial interpretations, and the historical context of its adoption. Does the state merely enforce community norms, or does it actively structure the pluralism for political ends?',
    'If the communal autonomy framing is dominant, the ''political_elites'' role as agenda_setter might be diminished, and the ''beneficiary'' status of ''minority_religious_communities'' might be seen as more direct and less mediated by state design. The core classification would likely remain ''rope'' but with different emphasis on the coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalist_millet_vs_communal_autonomy_framing, conceptual, 'Distinguishing between two closely related framings of legal pluralism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1947, marriage_authority__federalist_millet_reading, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(marr_tr_t1960, marriage_authority__federalist_millet_reading, theater_ratio, 1960, 0.07).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority__federalist_millet_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority__federalist_millet_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__federalist_millet_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1947, marriage_authority__federalist_millet_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(marr_be_t1960, marriage_authority__federalist_millet_reading, base_extractiveness, 1960, 0.12).
narrative_ontology:measurement(marr_be_t1980, marriage_authority__federalist_millet_reading, base_extractiveness, 1980, 0.13).
narrative_ontology:measurement(marr_be_t2000, marriage_authority__federalist_millet_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__federalist_millet_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1947, marriage_authority__federalist_millet_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(marr_su_t1960, marriage_authority__federalist_millet_reading, suppression_requirement, 1960, 0.22).
narrative_ontology:measurement(marr_su_t1980, marriage_authority__federalist_millet_reading, suppression_requirement, 1980, 0.23).
narrative_ontology:measurement(marr_su_t2000, marriage_authority__federalist_millet_reading, suppression_requirement, 2000, 0.24).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__federalist_millet_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority' kernel, emphasizing its role as a consociational anti-tyranny mechanism through fragmented legal authority. It is linked to other readings that emphasize communal autonomy, secularization, gender rights, or judicial harmonization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
