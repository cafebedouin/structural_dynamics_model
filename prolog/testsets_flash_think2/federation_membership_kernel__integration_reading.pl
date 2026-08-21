% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__integration_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_kernel__integration_reading
 *   human_readable: EU Free Movement: Integrationist Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story models the 'integrationist reading' of free
 *   movement within the European Union, where supranational authority (ECJ)
 *   interprets the scope of free movement expansively to maximize labor
 *   mobility and equal treatment, viewing it as fundamental to EU citizenship
 *   and single market completion. This reading often leads to ECJ rulings
 *   overriding national labor market protections and imposing costs on
 *   receiving state welfare systems without direct fiscal compensation. The
 *   metrics reflect the increasing extractiveness and suppression inherent in
 *   this expansive interpretation over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, 0.78).
domain_priors:suppression_score(federation_membership_kernel__integration_reading, 0.85).
domain_priors:theater_ratio(federation_membership_kernel__integration_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(federation_membership_kernel__integration_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__integration_reading, "EU Free Movement: Integrationist Reading").
narrative_ontology:topic_domain(federation_membership_kernel__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_kernel__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__integration_reading, 'c6b9f463-b559-4885-9330-c3d4cdae184a').
narrative_ontology:cs_kernel_codification('c6b9f463-b559-4885-9330-c3d4cdae184a', fixed_text).
narrative_ontology:cs_authority_grounding('c6b9f463-b559-4885-9330-c3d4cdae184a', lineage).
narrative_ontology:cs_interpretation_layer_present('c6b9f463-b559-4885-9330-c3d4cdae184a').
narrative_ontology:cs_reading_relation('c6b9f463-b559-4885-9330-c3d4cdae184a', federation_membership_kernel__member_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('c6b9f463-b559-4885-9330-c3d4cdae184a', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('c6b9f463-b559-4885-9330-c3d4cdae184a', foundational, free_movement_as_absolute_right).
narrative_ontology:cs_axiom_status(free_movement_as_absolute_right, holdable).
narrative_ontology:cs_axiom_grounding('c6b9f463-b559-4885-9330-c3d4cdae184a', free_movement_as_absolute_right, deontological).
narrative_ontology:cs_axiom('c6b9f463-b559-4885-9330-c3d4cdae184a', foundational, supremacy_of_eu_law).
narrative_ontology:cs_axiom_status(supremacy_of_eu_law, holdable).
narrative_ontology:cs_axiom_grounding('c6b9f463-b559-4885-9330-c3d4cdae184a', supremacy_of_eu_law, conventional).
narrative_ontology:cs_reference_frame('c6b9f463-b559-4885-9330-c3d4cdae184a', ever_closer_union_principle).
narrative_ontology:cs_drift_state('c6b9f463-b559-4885-9330-c3d4cdae184a', contemporary_eu_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6b9f463-b559-4885-9330-c3d4cdae184a', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__integration_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, eu_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__integration_reading, multinational_corporations).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, local_labor_in_receiving_states).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, sending_state_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_kernel__integration_reading, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The European Commission and the European Court of Justice (ECJ) actively interpret and enforce free movement rights, expanding their scope to deepen EU integration, often overriding national legal frameworks. They benefit from increased authority and legitimacy.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, eu_institutions, agenda_setter,
    institutional, generational, analytical, continental).

% Benefit directly from the right to live, work, and access social benefits in any EU member state, enhancing their economic and personal opportunities. They are the primary subjects of the right being expanded.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, mobile_eu_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Benefit from a larger, more flexible labor pool across the EU, enabling them to optimize labor costs and talent acquisition. They can leverage free movement to their advantage, often without bearing the social costs.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Bear the costs of increased competition for jobs, potential wage depression, and strain on local public services in regions with high immigration. Their ability to resist is limited by supranational law.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, local_labor_in_receiving_states, payer,
    powerless, immediate, trapped, local).

% Bear the fiscal costs of providing social benefits and public services to new arrivals, often without corresponding fiscal transfers or compensation from the EU budget. Their national policy autonomy is constrained by ECJ rulings.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, receiving_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Experience brain drain and loss of skilled labor, which can hinder their economic development and strain their own social systems, particularly in less affluent member states. They externalize the costs of educating and training citizens who then leave.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, sending_state_economies, payer,
    institutional, generational, constrained, national).

% Are compelled to implement and uphold ECJ rulings on free movement, even when these conflict with national policy preferences regarding welfare access or labor market protection. They are the primary enforcers of a system they often contest.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__integration_reading, member_state_governments, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_kernel__integration_reading, member_state_governments, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__integration_reading, eu_institutions).
narrative_ontology:fixing_cost_class(federation_membership_kernel__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To facilitate the completion of the EU single market by ensuring the free movement of workers, goods, services, and capital, and to establish a common European citizenship with equal rights across member states.
% TRANSFER_FUNCTION: Transfers labor, human capital, and associated welfare costs from sending member states to receiving member states, while transferring regulatory authority over these matters from national governments to EU institutions (ECJ).
% ABSENT_VOICES: National populist movements and local labor unions, who would advocate for stronger national borders, protection of domestic labor markets, and greater national control over welfare provision, are often marginalized in the supranational legal discourse.
% DISAPPEARANCE_RATIONALE: If the integrationist reading of free movement vanished, the EU single market would fragment, EU citizenship would lose a core constitutive element, and the entire federal project would be fundamentally reconfigured, leading to a massive reorganization of economic and social relations across the continent.
% FOUNDING_PROBLEM: The original problem was to foster economic integration and prevent future conflicts in post-war Europe by creating a common market and a sense of shared European identity.
% FOUNDING_PROBLEM_CORROBORATION: The EU institutions themselves, along with many academic federalism scholars, corroborate the ongoing relevance of the founding problem. However, national governments and opposition parties often contest the *current interpretation* as an overreach beyond the original intent.
narrative_ontology:disappearance_verdict(federation_membership_kernel__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__integration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_kernel__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__integration_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.78) reflects the significant costs borne by specific member state populations and systems (local labor, welfare systems, sending economies) due to the expansive interpretation, without commensurate benefits or compensation. Suppression (0.85) is high because national legal and political avenues to resist or limit these effects are consistently overridden by ECJ jurisprudence, requiring active enforcement of supranational law. The theater ratio is low (0.10) as the ECJ's enforcement and interpretation are highly functional and effective in achieving the integrationist goals, not merely performative. Accessibility collapse is high (0.75) as national alternatives to the ECJ's interpretation are largely foreclosed. Resistance is moderate (0.60) as member states and national political actors frequently voice opposition, but often lack effective means to alter the legal trajectory.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU institutions, this expansive interpretation is a necessary and legitimate evolution towards an 'ever closer union,' solving problems of market fragmentation and inequality. From the perspective of national governments, local labor, and welfare systems, it represents an imposition of costs and a loss of sovereign control, experienced as extraction rather than pure coordination. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   EU institutions (ECJ, Commission) are clear beneficiaries, gaining authority and advancing integration. Mobile EU citizens and multinational corporations also benefit from enhanced mobility and market flexibility. Conversely, local labor in receiving states, receiving state welfare systems, and sending state economies bear significant costs, making them targets of extraction. Member state governments are in a dual role, acting as both enforcers and payers, as their national sovereignty is constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecj_interpretive_legitimacy,
    'Is the ECJ''s expansive interpretation of free movement a legitimate evolution of treaty law or an instance of judicial overreach that undermines national sovereignty?',
    'A constitutional convention or treaty revision that explicitly redefines the scope of free movement and the ECJ''s interpretive powers, or a sustained period of non-compliance by a critical mass of member states.',
    'If deemed overreach, the constraint''s legitimacy would collapse, leading to reclassification as a Snare or Tangled Rope with higher suppression. If affirmed as legitimate, its Rope-like coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecj_interpretive_legitimacy, conceptual, 'Ambiguity regarding the ECJ''s role in shaping free movement law.').

omega_variable(
    fiscal_compensation_mechanism,
    'To what extent could a robust EU-level fiscal compensation mechanism mitigate the costs borne by receiving state welfare systems, thereby rebalancing the constraint?',
    'Implementation and evaluation of a significant EU fiscal transfer mechanism specifically designed to offset welfare costs associated with free movement.',
    'If effective, such a mechanism would reduce the extractiveness from receiving state welfare systems, potentially shifting the constraint closer to a pure Rope by addressing the asymmetric cost burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_compensation_mechanism, empirical, 'Impact of fiscal transfers on the perceived extractiveness of free movement.').

omega_variable(
    labor_market_impact_differentiation,
    'Are the negative impacts on local labor markets in receiving states primarily due to free movement, or are they exacerbated by other factors such as automation, global trade, or national labor policies?',
    'Detailed econometric studies disaggregating the causal factors of labor market changes in affected regions, controlling for non-migration variables.',
    'If free movement is a minor factor, the extractiveness attributed to it would decrease. If it''s a primary driver, the classification as a Tangled Rope would be further solidified due to clear victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_differentiation, empirical, 'Disentangling the specific impact of free movement on local labor markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__integration_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1993, federation_membership_kernel__integration_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(fede_tr_t1998, federation_membership_kernel__integration_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(fede_tr_t2003, federation_membership_kernel__integration_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(fede_tr_t2008, federation_membership_kernel__integration_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(fede_tr_t2013, federation_membership_kernel__integration_reading, theater_ratio, 2013, 0.1).
narrative_ontology:measurement(fede_tr_t2018, federation_membership_kernel__integration_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(fede_tr_t2023, federation_membership_kernel__integration_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(fede_be_t1993, federation_membership_kernel__integration_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(fede_be_t1998, federation_membership_kernel__integration_reading, base_extractiveness, 1998, 0.62).
narrative_ontology:measurement(fede_be_t2003, federation_membership_kernel__integration_reading, base_extractiveness, 2003, 0.68).
narrative_ontology:measurement(fede_be_t2008, federation_membership_kernel__integration_reading, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement(fede_be_t2013, federation_membership_kernel__integration_reading, base_extractiveness, 2013, 0.76).
narrative_ontology:measurement(fede_be_t2018, federation_membership_kernel__integration_reading, base_extractiveness, 2018, 0.77).
narrative_ontology:measurement(fede_be_t2023, federation_membership_kernel__integration_reading, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1993, federation_membership_kernel__integration_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(fede_su_t1998, federation_membership_kernel__integration_reading, suppression_requirement, 1998, 0.68).
narrative_ontology:measurement(fede_su_t2003, federation_membership_kernel__integration_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(fede_su_t2008, federation_membership_kernel__integration_reading, suppression_requirement, 2008, 0.8).
narrative_ontology:measurement(fede_su_t2013, federation_membership_kernel__integration_reading, suppression_requirement, 2013, 0.83).
narrative_ontology:measurement(fede_su_t2018, federation_membership_kernel__integration_reading, suppression_requirement, 2018, 0.84).
narrative_ontology:measurement(fede_su_t2023, federation_membership_kernel__integration_reading, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_single_market_completion).
narrative_ontology:affects_constraint(federation_membership_kernel__integration_reading, eu_citizenship_rights).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership_kernel', focusing on the integrationist interpretation of free movement. Other readings (member_sovereignty_reading, welfare_coordination_reading) offer alternative structural analyses of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
