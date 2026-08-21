% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__substantial_effects_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__substantial_effects_limited_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: commerce_clause_text__substantial_effects_limited_reading
 *   human_readable: Commerce Clause: Substantial Effects Doctrine (Limited Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the Commerce Clause,
 *   asserting that federal power extends to intrastate activities only if
 *   they have a 'substantial effect' on interstate commerce AND are genuinely
 *   economic, requiring a jurisdictional nexus and non-pretextual economic
 *   regulation. This reading seeks to limit the expansive federal power seen
 *   in the mid-20th century while avoiding the narrowness of an originalist
 *   interpretation. It creates a hybrid beneficiary structure, allowing
 *   federal regulation where economically justified but protecting state
 *   police powers from federal overreach. The constraint is a 'tangled rope'
 *   because it provides a coordination function (clarifying federal-state
 *   boundaries) but also involves asymmetric extraction (federal preemption
 *   of state authority in certain areas, and the burden on states to defend
 *   their non-economic regulations).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__substantial_effects_limited_reading, 0.45).
domain_priors:suppression_score(commerce_clause_text__substantial_effects_limited_reading, 0.6).
domain_priors:theater_ratio(commerce_clause_text__substantial_effects_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_text__substantial_effects_limited_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__substantial_effects_limited_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__substantial_effects_limited_reading, "Commerce Clause: Substantial Effects Doctrine (Limited Reading)").
narrative_ontology:topic_domain(commerce_clause_text__substantial_effects_limited_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__substantial_effects_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__substantial_effects_limited_reading, 'addfc4a1-9bd5-4f5f-b40a-2892106a108a').
narrative_ontology:cs_kernel_codification('addfc4a1-9bd5-4f5f-b40a-2892106a108a', fixed_text).
narrative_ontology:cs_authority_grounding('addfc4a1-9bd5-4f5f-b40a-2892106a108a', lineage).
narrative_ontology:cs_interpretation_layer_present('addfc4a1-9bd5-4f5f-b40a-2892106a108a').
narrative_ontology:cs_reading_relation('addfc4a1-9bd5-4f5f-b40a-2892106a108a', commerce_clause_text__expansive_federal_reading, coexists_with).
narrative_ontology:cs_reading_relation('addfc4a1-9bd5-4f5f-b40a-2892106a108a', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_axiom('addfc4a1-9bd5-4f5f-b40a-2892106a108a', foundational, economic_activity_limitation).
narrative_ontology:cs_axiom_status(economic_activity_limitation, holdable).
narrative_ontology:cs_axiom_grounding('addfc4a1-9bd5-4f5f-b40a-2892106a108a', economic_activity_limitation, conventional).
narrative_ontology:cs_axiom('addfc4a1-9bd5-4f5f-b40a-2892106a108a', foundational, jurisdictional_nexus_requirement).
narrative_ontology:cs_axiom_status(jurisdictional_nexus_requirement, holdable).
narrative_ontology:cs_axiom_grounding('addfc4a1-9bd5-4f5f-b40a-2892106a108a', jurisdictional_nexus_requirement, conventional).
narrative_ontology:cs_reference_frame('addfc4a1-9bd5-4f5f-b40a-2892106a108a', post_new_deal_limited_federalism).
narrative_ontology:cs_drift_state('addfc4a1-9bd5-4f5f-b40a-2892106a108a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('addfc4a1-9bd5-4f5f-b40a-2892106a108a', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, federal_government).
narrative_ontology:constraint_beneficiary(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, states_police_power).
narrative_ontology:constraint_victim(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_activity).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, federalism_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__substantial_effects_limited_reading, limited_government_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ability to regulate genuinely economic intrastate activities that substantially affect interstate commerce, enabling national policy coherence. Bears the cost of justifying a jurisdictional nexus and demonstrating non-pretextual economic regulation, preventing overreach into traditional state police powers.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from a uniform national market and federal regulation of economic activities that cross state lines or have significant national impact, reducing regulatory fragmentation. They are not directly subject to the constraint's limits but benefit from its clarity.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, national_economic_actors, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of federal preemption in areas deemed to have a substantial effect on interstate commerce. Benefits from the constraint's limits on federal power, preserving a sphere for traditional state police power regulation (e.g., health, safety, morals) against federal overreach.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, states_police_power, payer,
    institutional, generational, constrained, national).

% Is protected from federal regulation if it is genuinely non-economic and lacks a direct jurisdictional nexus to interstate commerce. However, it bears the cost of constant judicial scrutiny to prove its non-economic nature and lack of substantial effect, often facing federal challenges.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, intrastate_non_economic_activity, payer,
    powerless, immediate, trapped, local).

% Acts as the primary arbiter of the Commerce Clause's scope, defining the boundaries between federal and state power under this reading. Its decisions enforce the jurisdictional nexus and non-pretextual economic regulation requirements, shaping the constraint's application.
narrative_ontology:constraint_stakeholder(commerce_clause_text__substantial_effects_limited_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the division of regulatory authority between the federal government and individual states, allowing federal regulation of genuinely economic activities with national impact while reserving non-economic, local matters to the states.
% TRANSFER_FUNCTION: Transfers regulatory authority over certain intrastate economic activities from states to the federal government, while transferring protection from federal overreach to states for non-economic activities.
% ABSENT_VOICES: Advocates for a purely localist, pre-New Deal understanding of commerce would argue that even 'genuinely economic' intrastate activity should be beyond federal reach. Conversely, proponents of an expansive federal power would argue that the 'jurisdictional nexus' and 'non-pretextual' tests are artificial barriers to effective national governance. Both are excluded from the current judicial consensus that defines this reading.
% DISAPPEARANCE_RATIONALE: If this reading of the Commerce Clause vanished, the balance of federal and state power would immediately shift. Without the 'substantial effects' test, federal power would either contract dramatically (if an originalist reading prevailed) or expand without limit (if an expansive reading took hold), leading to a fundamental reorganization of regulatory authority and economic activity across the nation.
% FOUNDING_PROBLEM: The original Commerce Clause was intended to prevent states from erecting trade barriers and to ensure a unified national market, but its precise scope regarding intrastate activity was not fully defined, leading to disputes over federal regulatory authority.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and constitutional historians attest to the ongoing tension between federal and state power, confirming that the founding problem of defining the Commerce Clause's scope remains live. Judicial opinions and legislative debates from outside the federal government's immediate beneficiaries consistently highlight this persistent challenge.
narrative_ontology:disappearance_verdict(commerce_clause_text__substantial_effects_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__substantial_effects_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__substantial_effects_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_text__substantial_effects_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__substantial_effects_limited_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__substantial_effects_limited_reading_tests).
:- end_tests(commerce_clause_text__substantial_effects_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the federal government's ability to preempt state law in significant economic areas, but it is limited by the 'economic activity' and 'jurisdictional nexus' requirements. Suppression (0.6) is also moderate, as states must actively defend their regulatory space against federal challenges, but they are not entirely 'trapped' due to the judicial limits on federal power. Theater ratio (0.2) is low, as the judicial scrutiny of federal legislation is a genuine function, not mere performance, though some arguments for federal power may be pretextual. Accessibility collapse (0.4) is moderate, as states still have avenues to challenge federal overreach, and resistance (0.3) is present from states and advocates for limited federal power.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this reading is a necessary coordination mechanism for a modern national economy. From the states' perspective, it is a constant battle to preserve their traditional police powers against federal encroachment, even with the stated limits. The engine's classification will reflect this divergence, with the federal seat experiencing it as a rope-like coordination and the state seats experiencing it as a more snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national economic actors are beneficiaries, gaining from a coherent national regulatory framework. States and intrastate non-economic activities are payers, bearing the cost of federal preemption and the burden of proving their activities fall outside federal scope. The Supreme Court acts as an agenda-setter, defining and enforcing the boundaries of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_distinction,
    'Is the judicial distinction between ''economic'' and ''non-economic'' activity a stable and principled boundary, or is it inherently fluid and subject to judicial discretion?',
    'Longitudinal analysis of Supreme Court jurisprudence: if the distinction consistently shifts based on political composition rather than clear legal principles, it suggests fluidity.',
    'If fluid, the constraint''s limits on federal power are less predictable and more susceptible to political influence, increasing extractiveness for states and potentially shifting the classification towards a snare for those whose activities are reclassified as ''economic''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_distinction, conceptual, 'The stability and objectivity of the economic/non-economic distinction.').

omega_variable(
    pretextual_regulation_detection,
    'How effectively can courts discern genuinely ''non-pretextual economic regulation'' from federal attempts to regulate traditional state police powers under the guise of commerce?',
    'Empirical study of federal legislation challenged under the Commerce Clause: if a high percentage of challenged laws are found to be pretextual, it suggests effective judicial gatekeeping; if few are, it suggests judicial deference or difficulty in detection.',
    'If detection is ineffective, federal power effectively expands beyond the stated limits, increasing extractiveness for states and potentially pushing the constraint towards a more snare-like classification. If effective, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pretextual_regulation_detection, empirical, 'The efficacy of judicial review in preventing pretextual federal regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__substantial_effects_limited_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2015, 0.19).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_text__substantial_effects_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2005, 0.42).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_text__substantial_effects_limited_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(comm_su_t2015, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_text__substantial_effects_limited_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__substantial_effects_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__substantial_effects_limited_reading, commerce_clause_text__originalist_narrow_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause text, each with different structural properties and classifications. They form a constraint family, with each reading influencing the others by defining the boundaries of federal power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
