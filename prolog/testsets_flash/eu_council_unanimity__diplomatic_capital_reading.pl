% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity: Diplomatic Capital Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story models the EU Council's unanimity requirement from
 *   the perspective that it is a consensus-building mechanism. This reading
 *   emphasizes the coordination costs (extended negotiations, diplomatic
 *   effort) as a necessary investment for achieving stronger policy
 *   legitimacy and reducing downstream defection. It is one reading of the
 *   broader 'eu_council_unanimity' kernel, which is also interpreted as a
 *   'sovereignty_guarantor_reading' and a 'veto_trap_reading'. This story
 *   focuses on the positive-sum aspects of the negotiation process.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.25).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity: Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__diplomatic_capital_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '4891784c-082e-4a9e-a1a4-327eb5212fd3').
narrative_ontology:cs_kernel_codification('4891784c-082e-4a9e-a1a4-327eb5212fd3', formalized).
narrative_ontology:cs_authority_grounding('4891784c-082e-4a9e-a1a4-327eb5212fd3', lineage).
narrative_ontology:cs_interpretation_layer_present('4891784c-082e-4a9e-a1a4-327eb5212fd3').
narrative_ontology:cs_reading_relation('4891784c-082e-4a9e-a1a4-327eb5212fd3', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('4891784c-082e-4a9e-a1a4-327eb5212fd3', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('4891784c-082e-4a9e-a1a4-327eb5212fd3', foundational, consensus_enhances_legitimacy).
narrative_ontology:cs_axiom_status(consensus_enhances_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4891784c-082e-4a9e-a1a4-327eb5212fd3', consensus_enhances_legitimacy, instrumental).
narrative_ontology:cs_axiom('4891784c-082e-4a9e-a1a4-327eb5212fd3', foundational, negotiation_builds_buy_in).
narrative_ontology:cs_axiom_status(negotiation_builds_buy_in, holdable).
narrative_ontology:cs_axiom_grounding('4891784c-082e-4a9e-a1a4-327eb5212fd3', negotiation_builds_buy_in, empirically_contingent).
narrative_ontology:cs_reference_frame('4891784c-082e-4a9e-a1a4-327eb5212fd3', post_maastricht_consensus_building).
narrative_ontology:cs_drift_state('4891784c-082e-4a9e-a1a4-327eb5212fd3', contemporary_multi_crisis_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('4891784c-082e-4a9e-a1a4-327eb5212fd3', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_policy_legitimacy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, eu_institutions_excluding_council).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, deliberative_democracy_principle).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Member states benefit from the enhanced legitimacy and durability of policies forged through unanimous consent, even if it requires significant diplomatic effort and compromise. They retain a strong voice in shaping collective decisions.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% The rotating presidency is responsible for brokering consensus among member states. This role demands significant diplomatic skill and investment of political capital to achieve unanimous decisions, which then lend greater weight to the presidency's legacy.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_council_presidency, agenda_setter,
    institutional, immediate, constrained, continental).

% Citizens benefit from more stable and legitimate EU policies that are less prone to reversal or non-compliance due to broad member state buy-in. They experience the costs of slower decision-making indirectly.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, continental).

% Institutions like the European Commission and Parliament may experience delays and frustrations due to the unanimity requirement, as it slows down legislative processes and can dilute ambitious proposals. They bear the cost of extended negotiation times.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_institutions_excluding_council, payer,
    institutional, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all member states have a voice and buy-in in critical policy decisions, fostering collective ownership and reducing the likelihood of non-compliance or defection from agreed-upon policies.
% TRANSFER_FUNCTION: Transfers diplomatic capital and negotiation effort from individual member states and the Council Presidency into collective policy legitimacy and durability. It also transfers time and opportunity costs from other EU institutions.
% ABSENT_VOICES: While all member states are present, the voices of smaller states might be less influential in setting the initial agenda, even if their consent is ultimately required. External actors or non-governmental organizations advocating for faster, more decisive action might feel excluded from the internal consensus-building process.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, the EU's decision-making process would accelerate, but policies might face greater resistance, non-compliance, or even outright rejection from dissenting member states. The perceived legitimacy and stability of EU governance would be fundamentally altered, leading to a more fragmented and less cohesive union.
% FOUNDING_PROBLEM: To ensure that collective decisions in sensitive areas of European integration were genuinely supported by all sovereign member states, preventing the imposition of policies that could undermine national interests or democratic legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Many political scientists specializing in EU governance, as well as statements from various national governments (particularly smaller ones), corroborate that the problem of ensuring national buy-in and legitimacy for collective action remains live. They point to the increased durability of unanimously adopted policies compared to those passed by qualified majority voting.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the 'cost' is primarily diplomatic effort and time, which yields a 'benefit' of enhanced policy durability and legitimacy for all participants. Suppression is low (0.15) as the mechanism relies on active negotiation and persuasion rather than coercion; states are not 'forced' to agree but incentivized to find common ground. Theater ratio is very low (0.05) as the negotiation process is genuinely functional, not performative. The slight increase in extractiveness and suppression over time reflects the growing complexity and number of member states in the EU, making consensus harder to achieve.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of EU member states, the unanimity rule is a valuable tool for ensuring their interests are respected and policies are robust. From the perspective of other EU institutions (like the Commission), it can be seen as a bottleneck that slows down progress. However, this reading emphasizes the overall benefit to the collective legitimacy, making it a Rope from most seats, with the 'payer' seats experiencing higher coordination costs.
 *
 * DIRECTIONALITY LOGIC:
 *   EU member states are primary beneficiaries, as they gain policy legitimacy and avoid being outvoted on critical issues. The Council Presidency, while an agenda-setter, also benefits from the enhanced authority of unanimous decisions. Other EU institutions bear some costs in terms of slower processes, making them payers. There are no clear 'victims' in this reading, as the process is designed to be mutually beneficial, albeit costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (ensuring broad buy-in and legitimacy) is still live. The iterative negotiation process, while sometimes slow, continues to serve its function of building consensus. This classification as a Rope prevents mislabeling the coordination costs as pure extraction, recognizing the genuine, if sometimes inefficient, function it performs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_benefit_balance_of_negotiation,
    'Does the increased policy legitimacy and durability consistently outweigh the diplomatic costs and delays imposed by the unanimity requirement?',
    'Empirical studies comparing policy implementation rates, compliance levels, and public acceptance for policies adopted by unanimity versus qualified majority voting (QMV) in similar contexts.',
    'If the costs consistently outweigh the benefits, the extractiveness of the constraint would be re-evaluated upwards, potentially shifting its classification towards a Tangled Rope or even a Snare if the ''consensus'' becomes a cover for minoritarian blocking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_balance_of_negotiation, empirical, 'Assessing the net value of unanimity''s coordination function.').

omega_variable(
    unanimity_vs_veto_trap_framing,
    'Is the observed behavior of member states primarily driven by a desire for consensus and legitimacy (diplomatic_capital_reading), or by the strategic use of veto threats for national gain (veto_trap_reading)?',
    'Detailed case studies of negotiation processes, analysis of voting records, and interviews with diplomats to discern underlying motivations and the actual impact of blocking positions.',
    'If the veto_trap_reading is more accurate, the constraint''s extractiveness would be significantly higher, and its classification would shift from Rope to Snare, as the coordination story would be revealed as cover for pure extraction by blocking minorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unanimity_vs_veto_trap_framing, conceptual, 'Distinguishing genuine consensus-building from strategic blocking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1993, 0.2).
narrative_ontology:measurement(eu_c_be_t2000, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(eu_c_be_t2007, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2007, 0.23).
narrative_ontology:measurement(eu_c_be_t2014, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2014, 0.24).
narrative_ontology:measurement(eu_c_be_t2024, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1993, 0.1).
narrative_ontology:measurement(eu_c_su_t2000, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(eu_c_su_t2007, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2007, 0.13).
narrative_ontology:measurement(eu_c_su_t2014, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2014, 0.14).
narrative_ontology:measurement(eu_c_su_t2024, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eu_council_unanimity' kernel. This 'diplomatic_capital_reading' emphasizes the consensus-building and legitimacy-enhancing aspects, contrasting with the 'sovereignty_guarantor_reading' (focus on protection against majoritarian coercion) and the 'veto_trap_reading' (focus on minoritarian extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
