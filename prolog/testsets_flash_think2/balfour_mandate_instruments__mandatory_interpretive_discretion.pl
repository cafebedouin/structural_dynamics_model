% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: British Mandatory Interpretive Discretion over Palestine Mandate
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint is the `mandatory_interpretive_discretion` reading of the
 *   `balfour_mandate_instruments` kernel. It focuses on how British
 *   unilateral interpretive authority, rather than the specific content of
 *   the mandate, constituted the primary operational constraint, creating
 *   strategic uncertainty for both Arab and Zionist communities. Sibling
 *   readings include `jewish_national_home_primacy` and
 *   `dual_obligation_indigenous_rights`. The constraint operated as a snare,
 *   extracting certainty and fixed rights from both communities by
 *   maintaining British policy flexibility and control through interpretive
 *   ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.7).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.8).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.7).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "British Mandatory Interpretive Discretion over Palestine Mandate").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '0b72f797-72ca-4307-bf3b-8d0480c34ed9').
narrative_ontology:cs_kernel_codification('0b72f797-72ca-4307-bf3b-8d0480c34ed9', fixed_text).
narrative_ontology:cs_authority_grounding('0b72f797-72ca-4307-bf3b-8d0480c34ed9', extraction).
narrative_ontology:cs_interpretation_layer_present('0b72f797-72ca-4307-bf3b-8d0480c34ed9').
narrative_ontology:cs_reading_relation('0b72f797-72ca-4307-bf3b-8d0480c34ed9', balfour_mandate_instruments__jewish_national_home_primacy, coexists_with).
narrative_ontology:cs_reading_relation('0b72f797-72ca-4307-bf3b-8d0480c34ed9', balfour_mandate_instruments__dual_obligation_indigenous_rights, coexists_with).
narrative_ontology:cs_axiom('0b72f797-72ca-4307-bf3b-8d0480c34ed9', foundational, british_interpretive_supremacy).
narrative_ontology:cs_axiom_status(british_interpretive_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('0b72f797-72ca-4307-bf3b-8d0480c34ed9', british_interpretive_supremacy, conventional).
narrative_ontology:cs_axiom('0b72f797-72ca-4307-bf3b-8d0480c34ed9', secondary, policy_flexibility_as_governance).
narrative_ontology:cs_axiom_status(policy_flexibility_as_governance, holdable).
narrative_ontology:cs_axiom_grounding('0b72f797-72ca-4307-bf3b-8d0480c34ed9', policy_flexibility_as_governance, instrumental).
narrative_ontology:cs_reference_frame('0b72f797-72ca-4307-bf3b-8d0480c34ed9', unilateral_interpretive_authority).
narrative_ontology:cs_drift_state('0b72f797-72ca-4307-bf3b-8d0480c34ed9', post_un_partition_plan, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0b72f797-72ca-4307-bf3b-8d0480c34ed9', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_communities_palestine).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities_palestine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wielded unilateral interpretive power over the Mandate instruments, allowing for policy flexibility and strategic ambiguity. Benefited from maintaining control and leveraging competing claims to British advantage, avoiding external review.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to shifting British policies regarding land, immigration, and political rights, leading to strategic uncertainty and an inability to secure fixed legal or political status. Faced land expropriation and political marginalization due to discretionary interpretations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_communities_palestine, payer,
    powerless, generational, trapped, regional).

% Subject to shifting British policies regarding immigration, land purchase, and the development of a 'Jewish National Home,' leading to strategic uncertainty and an inability to secure fixed guarantees for their proto-state aspirations. Faced restrictions on immigration and land acquisition based on discretionary interpretations.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_communities_palestine, payer,
    organized, generational, constrained, regional).

% Nominally oversaw the British Mandate but lacked effective enforcement power to challenge or review British interpretive discretion, rendering it a largely symbolic oversight body.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandates_commission, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To administer the territory of Palestine and prepare for self-governance under international supervision, balancing the competing claims and aspirations of the Arab and Zionist communities.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and policy flexibility to the British Mandatory Power, at the cost of legal certainty, fixed rights, and self-determination for the local communities.
% ABSENT_VOICES: An independent international arbitration body with binding authority, or a unified local representative body with recognized sovereignty, would have challenged the unilateral interpretive authority and demanded fixed legal frameworks.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion had vanished overnight, the underlying Mandate text would have immediately become subject to direct, unmediated contestation by the communities, likely leading to immediate conflict or international arbitration, fundamentally altering the political landscape and accelerating the end of the Mandate.
% FOUNDING_PROBLEM: To administer territories detached from the Ottoman Empire after WWI, balancing the promise of a Jewish National Home with the rights of existing non-Jewish communities, under international supervision, in a context of rising nationalist aspirations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars widely corroborate the dual and often contradictory nature of the mandate's founding problem, noting the inherent tension between the Balfour Declaration and the League of Nations Covenant's self-determination principles. The specific problem of British administration of the mandate ended with their withdrawal in 1948.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70) because British discretion allowed for policy shifts (e.g., land regimes, White Papers) that consistently altered the baseline for both communities, preventing either from securing their long-term aspirations. Suppression is also high (0.80) due to the lack of external review and the inability of local communities to challenge British interpretations effectively. Theater ratio is low (0.20) as the discretion was actively and consequentially exercised, not merely performative. Resistance is high (0.70) as both communities actively lobbied and, at times, violently resisted British policies.
 *
 * PERSPECTIVAL GAP:
 *   From the British perspective, their interpretive discretion was a necessary tool for maintaining order and balancing competing claims in a complex geopolitical environment. From the perspective of both Arab and Zionist communities, this discretion was a source of profound uncertainty and a mechanism for British control and extraction, preventing the realization of their self-determination.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are the primary beneficiaries, gaining policy flexibility and maintaining control through a 'divide and rule' strategy. Both Arab and Zionist communities are victims, as they are subjected to the strategic uncertainty and shifting policies that prevent them from achieving their respective goals. The League of Nations Mandates Commission is an observer, lacking real power to influence the interpretive discretion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_text_ambiguity,
    'To what extent was the ambiguity in the original Mandate text (e.g., ''national home'' vs. ''civil and religious rights'') an inherent structural feature versus a deliberate British policy choice to enable discretion?',
    'Historical analysis of drafting documents and diplomatic correspondence preceding the Mandate''s ratification, comparing initial proposals with final text.',
    'If inherent, the constraint''s extractiveness is partly a consequence of the foundational document''s flaws; if deliberate, it highlights the British role in constructing the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_text_ambiguity, empirical, 'Origin of interpretive ambiguity in the Mandate instruments.').

omega_variable(
    external_review_feasibility,
    'Could an effective external review mechanism (e.g., a stronger League of Nations court) have genuinely constrained British interpretive discretion, or was the geopolitical reality of the time insurmountable?',
    'Counterfactual historical analysis comparing the Palestine Mandate with other mandates or international agreements where stronger oversight mechanisms were present and effective.',
    'If feasible, the absence of such a mechanism points to a structural flaw in the international system that enabled the snare; if infeasible, it underscores the inherent power dynamics of colonial administration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_review_feasibility, conceptual, 'Potential for external checks on British interpretive authority.').

omega_variable(
    discretion_as_divide_and_rule,
    'Was British interpretive discretion primarily a tool for ''divide and rule'' to maintain colonial control, or a genuine attempt to balance irreconcilable claims?',
    'Analysis of British internal policy documents and communications, comparing stated intentions with observed outcomes and the differential impact on Arab and Zionist communities.',
    'If primarily ''divide and rule,'' the constraint''s extractiveness and suppression are higher, as the coordination narrative is a cover; if a genuine attempt, the complexity of the coordination problem is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_as_divide_and_rule, empirical, 'Motivation behind British interpretive discretion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0, 0.15).
narrative_ontology:measurement(balf_tr_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 5, 0.16).
narrative_ontology:measurement(balf_tr_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 10, 0.17).
narrative_ontology:measurement(balf_tr_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 15, 0.18).
narrative_ontology:measurement(balf_tr_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 20, 0.19).
narrative_ontology:measurement(balf_tr_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 28, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(balf_be_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(balf_be_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(balf_be_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(balf_be_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(balf_be_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 28, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t0, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(balf_su_t5, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(balf_su_t10, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(balf_su_t15, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(balf_su_t20, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(balf_su_t28, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 28, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, israeli_palestinian_conflict_legitimacy_claims).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'balfour_mandate_instruments' kernel, focusing on British interpretive discretion. It is structurally linked to sibling readings that emphasize the 'jewish_national_home_primacy' and 'dual_obligation_indigenous_rights' aspects of the Mandate, as all derive from the same foundational text and historical context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
