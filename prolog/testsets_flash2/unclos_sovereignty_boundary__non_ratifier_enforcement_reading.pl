% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint describes the assertion by major naval powers that
 *   freedom of navigation (FON) principles constitute customary international
 *   law, enforceable by naval presence, irrespective of UNCLOS ratification.
 *   This reading allows naval powers to operate in EEZs without being bound
 *   by UNCLOS provisions they have not ratified, effectively extracting
 *   unhindered access from coastal states that might otherwise assert
 *   stricter controls. The constraint is claimed as a 'rope' by its
 *   proponents (a coordination mechanism), but its metrics reflect a
 *   'tangled_rope' due to active enforcement and asymmetric benefits.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '83eb8e86-a054-4c7d-8548-d0d7815d2507').
narrative_ontology:cs_kernel_codification('83eb8e86-a054-4c7d-8548-d0d7815d2507', formalized).
narrative_ontology:cs_authority_grounding('83eb8e86-a054-4c7d-8548-d0d7815d2507', extraction).
narrative_ontology:cs_interpretation_layer_present('83eb8e86-a054-4c7d-8548-d0d7815d2507').
narrative_ontology:cs_reading_relation('83eb8e86-a054-4c7d-8548-d0d7815d2507', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('83eb8e86-a054-4c7d-8548-d0d7815d2507', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('83eb8e86-a054-4c7d-8548-d0d7815d2507', foundational, freedom_of_navigation_is_customary_law).
narrative_ontology:cs_axiom_status(freedom_of_navigation_is_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('83eb8e86-a054-4c7d-8548-d0d7815d2507', freedom_of_navigation_is_customary_law, conventional).
narrative_ontology:cs_axiom('83eb8e86-a054-4c7d-8548-d0d7815d2507', secondary, naval_presence_legitimizes_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_legitimizes_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('83eb8e86-a054-4c7d-8548-d0d7815d2507', naval_presence_legitimizes_enforcement, instrumental).
narrative_ontology:cs_reference_frame('83eb8e86-a054-4c7d-8548-d0d7815d2507', unrestricted_maritime_mobility).
narrative_ontology:cs_drift_state('83eb8e86-a054-4c7d-8548-d0d7815d2507', contemporary_maritime_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83eb8e86-a054-4c7d-8548-d0d7815d2507', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to navigate freely through all international waters, including Exclusive Economic Zones (EEZs) of non-ratifying states, based on customary international law. They enforce this through naval presence and operations, benefiting from unhindered global mobility for their fleets and trade.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% These states attempt to assert exclusive rights over their EEZs, including restricting foreign naval activities, citing sovereign rights. They bear the cost of having their claims challenged by naval powers, often lacking the military capacity to enforce their preferred interpretation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    moderate, biographical, constrained, national).

% Benefits from the principle of freedom of navigation, as it ensures predictable and unhindered passage through key maritime routes, reducing transit times and costs. They are largely passive beneficiaries, relying on naval powers to maintain the status quo.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% States that have ratified UNCLOS and generally adhere to its provisions. They observe the contest between naval powers and non-ratifiers, sometimes aligning with one side or the other depending on specific geopolitical interests, but their primary commitment is to the treaty framework.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states, observer,
    institutional, generational, analytical, global).

% The primary judicial organ of the UN, which could adjudicate disputes over customary international law and UNCLOS interpretation. Its rulings would provide legal clarity but are often not universally accepted or enforceable without political will.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global understanding of maritime transit rights, allowing for predictable movement of goods and military assets across oceans, even in the absence of universal treaty ratification.
% TRANSFER_FUNCTION: Transfers the right to unrestricted passage from coastal states (who might otherwise assert exclusivity) to major naval powers and the international shipping industry, enforced by naval presence.
% ABSENT_VOICES: Smaller coastal states with limited naval capacity, who might prefer stricter EEZ controls for resource management or security, are often marginalized in the enforcement of customary law by powerful naval actors.
% DISAPPEARANCE_RATIONALE: If the principle of freedom of navigation as customary international law vanished, major naval powers would face significant challenges to global mobility, coastal states would likely assert more restrictive EEZ claims, leading to increased maritime disputes, trade disruptions, and potential military confrontations.
% FOUNDING_PROBLEM: The need for clear and universally accepted rules governing maritime navigation to prevent conflicts and facilitate global trade and communication, predating formal treaty codification.
% FOUNDING_PROBLEM_CORROBORATION: Major naval powers consistently assert the live status of this problem, citing the need for global stability and trade. Coastal states, while often disagreeing on the interpretation, generally acknowledge the underlying need for maritime order, even if they contest the means of its enforcement. International legal scholars also corroborate the historical and ongoing nature of this problem.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because naval powers gain significant strategic and economic advantages from unhindered access, while coastal states bear the cost of diminished sovereign control over their EEZs. Suppression (0.75) is substantial, as the enforcement relies on the superior military capabilities of naval powers, effectively suppressing alternative interpretations or enforcement by coastal states. Theater ratio is low (0.20) because the naval presence is a genuine, functional enforcement mechanism, not merely performative. The increasing trend in extractiveness and suppression reflects the growing contestation over maritime zones and the intensification of enforcement efforts.
 *
 * PERSPECTIVAL GAP:
 *   Naval powers perceive this as a necessary coordination mechanism for global stability and trade, a 'rope' ensuring common good. Coastal states, however, experience it as an extractive 'snare' or 'tangled_rope,' where their sovereign claims are overridden by superior force. The engine's classification will likely reflect this divergence, computing a more extractive type for coastal states.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers are clear beneficiaries and agenda-setters (low d), as they define and enforce the terms of navigation. Coastal states asserting EEZ exclusivity are targets (high d), bearing the costs of challenged sovereignty. The international shipping industry is a beneficiary (low d), gaining from predictable routes. UNCLOS ratifying states and the ICJ are observers, their directionality depending on their specific alignment or analytical role.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring freedom of navigation) is still live, but its application has drifted from pure coordination to a mechanism for powerful states to maintain strategic advantage. The classification as 'tangled_rope' prevents mislabeling it as a pure 'rope' by acknowledging the asymmetric extraction and active enforcement required to sustain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_scope_ambiguity,
    'To what extent do specific freedom of navigation practices (e.g., military exercises in EEZs) genuinely constitute customary international law, universally binding even on non-ratifiers, versus being assertions of power by naval states?',
    'Analysis of state practice and opinio juris (states'' belief that a practice is legally obligatory) from a broad range of states, including those with limited naval power, and ICJ advisory opinions.',
    'If practices are found to be primarily power assertions, the constraint''s extractiveness and suppression would be re-evaluated upwards, potentially reclassifying it closer to a ''snare''. If genuinely customary, its coordination function would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_scope_ambiguity, empirical, 'Ambiguity regarding the legal basis and universal acceptance of specific FON practices as customary international law.').

omega_variable(
    unclos_ratification_impact,
    'How would the constraint''s structure and enforcement change if major naval powers (e.g., the US) were to ratify UNCLOS?',
    'Legal analysis of the implications of ratification on existing FON operations, and observation of actual changes in state practice post-ratification (if it occurs).',
    'Ratification could either formalize and legitimize current practices (reducing perceived extraction for some) or impose new obligations that constrain naval powers, potentially shifting the balance of benefits and costs and altering the constraint''s type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unclos_ratification_impact, conceptual, 'Uncertainty about the impact of UNCLOS ratification by key non-ratifying naval powers on the constraint''s operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.5).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.6).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel, focusing on the customary international law aspect of freedom of navigation and its enforcement by non-ratifying naval powers. It is linked to sibling readings that emphasize strict EEZ boundaries or historical rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
