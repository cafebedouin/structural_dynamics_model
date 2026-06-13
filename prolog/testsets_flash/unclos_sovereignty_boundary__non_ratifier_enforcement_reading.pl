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
 *   This constraint represents the reading of international maritime law
 *   where freedom of navigation (FON) principles are considered customary
 *   international law, binding on all states regardless of their ratification
 *   of the United Nations Convention on the Law of the Sea (UNCLOS). It
 *   asserts that major naval powers can enforce these principles through
 *   naval presence, even in areas claimed as Exclusive Economic Zones (EEZs)
 *   by coastal states that interpret UNCLOS as granting them exclusive
 *   rights. This reading decouples the enforcement of FON from specific
 *   treaty obligations, allowing non-ratifying states (or states acting
 *   outside UNCLOS provisions) to assert these rights.
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
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'f9d1aac4-3d0f-46b1-b111-5abf32b2ed68').
narrative_ontology:cs_kernel_codification('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', distributed).
narrative_ontology:cs_authority_grounding('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', extraction).
narrative_ontology:cs_interpretation_layer_present('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68').
narrative_ontology:cs_reading_relation('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', foundational, freedom_of_navigation_is_customary_law).
narrative_ontology:cs_axiom_status(freedom_of_navigation_is_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', freedom_of_navigation_is_customary_law, conventional).
narrative_ontology:cs_axiom('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', foundational, naval_presence_is_legitimate_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_is_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', naval_presence_is_legitimate_enforcement, instrumental).
narrative_ontology:cs_reference_frame('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', unimpeded_global_maritime_access).
narrative_ontology:cs_drift_state('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', contemporary_maritime_disputes_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f9d1aac4-3d0f-46b1-b111-5abf32b2ed68', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, fishing_fleets_of_coastal_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert and enforce freedom of navigation principles as customary international law, independent of UNCLOS ratification. They conduct 'freedom of navigation operations' (FONOPs) to challenge what they perceive as excessive maritime claims, ensuring global access for their naval and commercial vessels. They benefit from strategic mobility and reduced legal constraints.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, generational, arbitrage, global).

% Claim exclusive rights within their 200-nautical-mile EEZs, including restrictions on foreign military activities or resource exploration. They view naval presence by non-ratifying powers as an infringement on their sovereignty and a threat to their economic interests. Their options are diplomatic protest, limited naval deterrence, or legal challenges, all of which incur significant costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    organized, generational, constrained, national).

% Benefits from the unimpeded passage through international waters and claimed EEZs, reducing transit times and legal complexities. They rely on the enforcement of freedom of navigation to maintain efficient global supply chains, even if it creates friction between states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% Are directly impacted by foreign fishing vessels operating within their claimed EEZs under the assertion of freedom of navigation. They face competition for resources and potential ecological damage, with limited recourse due to the power imbalance. Their livelihood is tied to local waters, making exit difficult.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, fishing_fleets_of_coastal_states, payer,
    moderate, biographical, identity_locked, local).

% Adjudicate disputes related to maritime law, including interpretations of UNCLOS and customary international law. Their rulings can influence the perceived legitimacy and enforceability of this constraint, though their authority is often challenged by major naval powers.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_tribunals, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures predictable and unimpeded passage for commercial and military vessels through international waters and claimed EEZs, facilitating global trade and strategic mobility by establishing a common understanding of maritime rights.
% TRANSFER_FUNCTION: Transfers the right to unimpeded passage and resource access from coastal states (who claim exclusive EEZ rights) to major naval powers and the global shipping industry, backed by naval enforcement.
% ABSENT_VOICES: Small island developing states and landlocked states with limited maritime access, who often rely on strict interpretations of UNCLOS for their resource claims, are often marginalized in debates dominated by naval powers and major coastal states. They would advocate for stronger protections for national maritime zones.
% DISAPPEARANCE_RATIONALE: If this reading of customary international law vanished, major naval powers would lose a key justification for their FONOPs, leading to increased challenges to their maritime movements. Coastal states would likely assert more aggressive control over their EEZs, potentially disrupting global shipping and increasing the risk of maritime conflicts. The global maritime order would become significantly more fragmented and contested.
% FOUNDING_PROBLEM: The need to balance sovereign rights of coastal states with the freedom of navigation essential for global commerce and military mobility, particularly in the absence of universal treaty ratification.
% FOUNDING_PROBLEM_CORROBORATION: Major naval powers consistently assert the problem is live, citing the need for global stability and trade. Coastal states, while disputing the solution, acknowledge the underlying tension between sovereign claims and international transit. International legal scholars provide corroboration of the historical and ongoing nature of this tension, independent of the benefiting parties.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (ensuring global maritime trade and transit) but also involves asymmetric extraction. Major naval powers benefit from unimpeded access and enforce these rules, while coastal states attempting to assert exclusive EEZ rights bear the cost of having their claims challenged and potentially overridden by naval presence. Extractiveness (0.65) is substantial due to the imposition of costs on coastal states. Suppression (0.75) is high, as it relies on the credible threat of naval force to deter challenges. Resistance (0.70) is also high, reflecting ongoing diplomatic protests, naval standoffs, and legal challenges from coastal states. Theater ratio (0.20) is low, as the enforcement is direct and functional, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   Major naval powers experience this as a Rope, a necessary coordination mechanism for global trade and security. Coastal states asserting EEZ exclusivity experience it as a Snare, an imposition on their sovereign rights backed by force. The engine will compute this divergence from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers are primary beneficiaries (d near 0.0) as they gain unimpeded access without being bound by UNCLOS ratification for enforcement. The global shipping industry also benefits from predictable transit. Coastal states asserting EEZ exclusivity are victims (d near 1.0) as their claims are challenged and their resources potentially exploited by others under the guise of FON. Fishing fleets of coastal states are also victims, as their exclusive access to resources within their claimed EEZs is undermined.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_status_ambiguity,
    'To what extent are ''freedom of navigation'' principles genuinely customary international law, independent of UNCLOS ratification, versus a policy preference asserted by naval powers?',
    'Analysis of state practice and opinio juris from non-naval powers, and rulings from international tribunals on non-UNCLOS-ratified states'' maritime claims.',
    'If genuinely customary law, the constraint is a Rope for naval powers and a Tangled Rope for coastal states. If primarily a policy assertion, it is a Snare for coastal states, as the coordination story is cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status_ambiguity, conceptual, 'Ambiguity of customary international law status for freedom of navigation.').

omega_variable(
    enforcement_legitimacy_ambiguity,
    'Is enforcement by naval presence a legitimate mechanism for customary international law, or does it constitute coercion that undermines the ''customary'' claim?',
    'Analysis of international legal scholarship and state responses to naval operations in disputed zones, particularly from states not directly involved in the dispute.',
    'If legitimate, the enforcement is a coordination cost. If coercive, it is a suppression mechanism, increasing the constraint''s effective extractiveness for coastal states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy_ambiguity, conceptual, 'Legitimacy of naval enforcement for customary international law.').

omega_variable(
    kernel_reading_identification,
    'This constraint is the ''non_ratifier_enforcement_reading'' of the ''unclos_sovereignty_boundary'' kernel. What would change if the ''strict_eez_reading'' or ''historical_rights_reading'' were adopted?',
    'Analysis of legal precedent and state practice under alternative readings.',
    'The ''strict_eez_reading'' would shift major naval powers into the victim category and coastal states into beneficiaries, reclassifying to Snare for naval powers. The ''historical_rights_reading'' would introduce a new set of beneficiaries (states with historical claims) and victims (states whose EEZ claims are overridden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identification of this constraint as a specific reading of the UNCLOS sovereignty boundary kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement(uncl_tr_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(uncl_be_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(uncl_su_t20, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_trade_routes_security).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel. Its structural properties differ significantly from other readings, particularly regarding beneficiaries and victims, necessitating separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
