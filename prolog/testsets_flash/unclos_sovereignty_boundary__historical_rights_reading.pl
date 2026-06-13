% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__historical_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__historical_rights_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__historical_rights_reading
 *   human_readable: Historical Rights Override of UNCLOS EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint describes the assertion that historical usage and
 *   occupation create sovereign rights that predate and override the
 *   Exclusive Economic Zone (EEZ) provisions of the United Nations Convention
 *   on the Law of the Sea (UNCLOS). This reading is actively enforced by
 *   claimant states, leading to significant geopolitical friction and
 *   resource extraction from other coastal states and international actors.
 *   It is a Tangled Rope because it attempts to coordinate historical claims
 *   with modern law, but does so with significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, 0.65).
domain_priors:suppression_score(unclos_sovereignty_boundary__historical_rights_reading, 0.75).
domain_priors:theater_ratio(unclos_sovereignty_boundary__historical_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__historical_rights_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__historical_rights_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__historical_rights_reading, "Historical Rights Override of UNCLOS EEZ").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__historical_rights_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__historical_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__historical_rights_reading, '48b038ec-4f96-4561-ad42-c9ab8b204d2b').
narrative_ontology:cs_kernel_codification('48b038ec-4f96-4561-ad42-c9ab8b204d2b', distributed).
narrative_ontology:cs_authority_grounding('48b038ec-4f96-4561-ad42-c9ab8b204d2b', extraction).
narrative_ontology:cs_interpretation_layer_present('48b038ec-4f96-4561-ad42-c9ab8b204d2b').
narrative_ontology:cs_reading_relation('48b038ec-4f96-4561-ad42-c9ab8b204d2b', unclos_sovereignty_boundary__strict_eez_reading, coexists_with).
narrative_ontology:cs_reading_relation('48b038ec-4f96-4561-ad42-c9ab8b204d2b', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coexists_with).
narrative_ontology:cs_axiom('48b038ec-4f96-4561-ad42-c9ab8b204d2b', foundational, historical_precedence_over_codified_law).
narrative_ontology:cs_axiom_status(historical_precedence_over_codified_law, holdable).
narrative_ontology:cs_axiom_grounding('48b038ec-4f96-4561-ad42-c9ab8b204d2b', historical_precedence_over_codified_law, conventional).
narrative_ontology:cs_axiom('48b038ec-4f96-4561-ad42-c9ab8b204d2b', foundational, effective_occupation_creates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_creates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('48b038ec-4f96-4561-ad42-c9ab8b204d2b', effective_occupation_creates_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('48b038ec-4f96-4561-ad42-c9ab8b204d2b', pre_unclos_customary_maritime_law).
narrative_ontology:cs_drift_state('48b038ec-4f96-4561-ad42-c9ab8b204d2b', contemporary_unclos_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('48b038ec-4f96-4561-ad42-c9ab8b204d2b', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__historical_rights_reading, international_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that assert historical rights to maritime areas, often overlapping with other states' UNCLOS-defined EEZs. They actively enforce these claims through naval patrols, resource extraction, and diplomatic pressure, benefiting from expanded territorial control and resource access.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, expansive_claimant_states, agenda_setter,
    institutional, generational, constrained, regional).

% States whose UNCLOS-defined EEZs are encroached upon by historical claims. They bear the cost of lost resource access, increased security risks, and diplomatic friction. Their options are to challenge claims legally, militarily, or diplomatically, all of which are costly.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, eez_holding_coastal_states, payer,
    institutional, generational, constrained, regional).

% Operate vessels through contested waters, facing increased insurance premiums, potential harassment, and rerouting costs due to overlapping claims. They pay through operational friction and reduced efficiency.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_shipping_companies, payer,
    organized, immediate, constrained, global).

% Local and international fishing vessels that are denied access to traditional fishing grounds or face harassment from claimant states' patrols. They bear direct economic losses from reduced catch and increased operational risk.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, fishing_fleets, payer,
    moderate, biographical, constrained, local).

% International legal bodies tasked with interpreting and applying UNCLOS. They observe and adjudicate disputes but lack direct enforcement power over states that reject their jurisdiction or the premise of UNCLOS itself in favor of historical claims.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, unclos_arbitration_bodies, observer,
    institutional, generational, analytical, global).

% Benefits from the general principle of stable maritime boundaries, even if contested. The existence of a framework for dispute resolution, however imperfect, provides a baseline for international order, reducing the likelihood of open conflict.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__historical_rights_reading, international_community, beneficiary,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for claimant states to assert and potentially negotiate historical rights, preventing immediate, unmanaged conflict over contested maritime zones, while also allowing for resource exploitation under their asserted jurisdiction.
% TRANSFER_FUNCTION: Transfers control over maritime resources (fishing, mineral rights, energy) and strategic access from states adhering strictly to UNCLOS EEZ definitions to states asserting historical claims, often backed by naval presence.
% ABSENT_VOICES: Small island nations and landlocked states, who have less historical maritime presence or naval power, are effectively marginalized in disputes where historical claims override established international law. They would advocate for strict adherence to UNCLOS to protect their limited maritime entitlements.
% DISAPPEARANCE_RATIONALE: If the concept of historical rights overriding UNCLOS EEZ provisions vanished, claimant states would lose their primary legal justification for expansive claims. This would likely lead to a re-stabilization of maritime boundaries according to UNCLOS, reducing friction for coastal states and shipping, but potentially increasing pressure on claimant states to find new justifications or accept reduced influence.
% FOUNDING_PROBLEM: The need to reconcile traditional, often vaguely defined, historical claims to maritime areas with modern, codified international law (UNCLOS) that establishes clear, but sometimes conflicting, boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The problem is live, as evidenced by ongoing disputes in various maritime regions (e.g., South China Sea, Arctic). International legal scholars and UNCLOS arbitration bodies, acting as external observers, corroborate that this tension remains a central challenge to maritime governance, distinct from the self-serving claims of the expansive states.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__historical_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__historical_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__historical_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unclos_sovereignty_boundary__historical_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__historical_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__historical_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because claimant states gain significant resource access and strategic control at the expense of others. Suppression (0.75) is also high, as these claims are often backed by naval power and diplomatic pressure, actively suppressing alternative interpretations or enforcement of UNCLOS. The theater ratio (0.20) is relatively low, as the enforcement actions are genuinely aimed at asserting and maintaining control, not merely for show. The metrics show a trend of increasing extractiveness and suppression as claimant states become more assertive over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of expansive claimant states, this is a legitimate assertion of long-standing rights, a form of coordination that acknowledges historical realities. From the perspective of EEZ-holding coastal states and international actors, it is an extractive imposition that undermines established international law. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Expansive claimant states are the primary beneficiaries and agenda-setters, actively shaping and enforcing this interpretation. EEZ-holding coastal states, international shipping, and fishing fleets are victims, bearing the costs of lost access and increased risk. The international community is a diffuse beneficiary, gaining a (contested) framework for managing disputes, even if the framework itself is extractive for some.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_claim_legitimacy,
    'What constitutes ''historical usage and occupation'' sufficient to establish sovereign rights, and is this standard universally recognized or selectively applied?',
    'International court rulings establishing clear precedents for historical claims, or a new international convention that codifies criteria for such claims.',
    'If the standard is clarified and universally accepted, the constraint''s legitimacy would increase, potentially reducing resistance. If it remains ambiguous or selectively applied, it will continue to function as a tool for asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_claim_legitimacy, conceptual, 'Ambiguity in the definition and recognition of historical claims.').

omega_variable(
    unclos_customary_law_status,
    'To what extent have UNCLOS EEZ provisions achieved the status of customary international law, binding even on non-signatories or those asserting historical claims?',
    'A definitive ruling by the International Court of Justice on the customary status of specific UNCLOS provisions, or widespread, consistent state practice affirming their binding nature.',
    'If UNCLOS EEZ provisions are widely recognized as customary law, the ''historical_rights_reading'' would face increased legal challenge and its suppression costs would rise. If not, its claims would retain more legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unclos_customary_law_status, empirical, 'The legal status of UNCLOS provisions as customary international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__historical_rights_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1995, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(uncl_tr_t2005, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(uncl_tr_t2015, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__historical_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1995, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(uncl_be_t2005, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(uncl_be_t2015, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__historical_rights_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(uncl_su_t1995, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(uncl_su_t2005, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(uncl_su_t2015, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__historical_rights_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__historical_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__historical_rights_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'unclos_sovereignty_boundary' kernel, focusing on historical rights. It directly influences and is influenced by other readings of the same kernel, particularly the 'strict_eez_reading' and 'non_ratifier_enforcement_reading', as they represent competing interpretations of maritime sovereignty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
