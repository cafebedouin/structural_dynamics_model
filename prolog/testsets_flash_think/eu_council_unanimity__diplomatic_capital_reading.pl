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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity: Diplomatic Capital Reading
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story analyzes the EU Council's unanimity requirement
 *   from the 'diplomatic capital' reading, which views it as a necessary,
 *   albeit costly, mechanism for consensus-building that strengthens policy
 *   legitimacy and durability. It is one reading of the broader
 *   'eu_council_unanimity' kernel, which is also interpreted as a
 *   'sovereignty guarantor' or a 'veto trap'. This reading focuses on the
 *   positive coordination function and the investment in political buy-in.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.15).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.2).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.2).
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
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'cf2ec823-28ba-4b73-8b14-625bee3bc32a').
narrative_ontology:cs_kernel_codification('cf2ec823-28ba-4b73-8b14-625bee3bc32a', formalized).
narrative_ontology:cs_authority_grounding('cf2ec823-28ba-4b73-8b14-625bee3bc32a', lineage).
narrative_ontology:cs_interpretation_layer_present('cf2ec823-28ba-4b73-8b14-625bee3bc32a').
narrative_ontology:cs_reading_relation('cf2ec823-28ba-4b73-8b14-625bee3bc32a', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf2ec823-28ba-4b73-8b14-625bee3bc32a', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('cf2ec823-28ba-4b73-8b14-625bee3bc32a', foundational, consensus_enhances_legitimacy).
narrative_ontology:cs_axiom_status(consensus_enhances_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('cf2ec823-28ba-4b73-8b14-625bee3bc32a', consensus_enhances_legitimacy, conventional).
narrative_ontology:cs_axiom('cf2ec823-28ba-4b73-8b14-625bee3bc32a', secondary, negotiation_as_policy_investment).
narrative_ontology:cs_axiom_status(negotiation_as_policy_investment, holdable).
narrative_ontology:cs_axiom_grounding('cf2ec823-28ba-4b73-8b14-625bee3bc32a', negotiation_as_policy_investment, instrumental).
narrative_ontology:cs_reference_frame('cf2ec823-28ba-4b73-8b14-625bee3bc32a', post_maastricht_integration_model).
narrative_ontology:cs_drift_state('cf2ec823-28ba-4b73-8b14-625bee3bc32a', contemporary_eu_governance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cf2ec823-28ba-4b73-8b14-625bee3bc32a', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_citizens).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, legitimacy_through_consensus_doctrine).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, shared_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each member state holds a veto, forcing extensive negotiation to reach consensus on major policy decisions. This process ensures national interests are considered and policies gain broad political buy-in, but requires significant diplomatic effort and time investment.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Proposes legislation and facilitates negotiations, but does not hold a vote in the Council. Experiences the unanimity rule as a constraint on the speed and ambition of policy development, but also recognizes its role in ensuring the legitimacy and durability of adopted policies.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_commission, observer,
    institutional, biographical, analytical, continental).

% Benefit from the stability and perceived legitimacy of EU policies that have broad member state consensus. Indirectly bear the costs of slower decision-making or compromised policy ambition, but generally value the democratic buy-in and reduced risk of policy reversal.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that all member states have buy-in for major policy decisions, preventing unilateral defection and strengthening the legitimacy and durability of collective action across the Union.
% TRANSFER_FUNCTION: Transfers diplomatic capital, negotiation effort, and potential policy ambition from individual member states into collective policy legitimacy, stability, and reduced downstream non-compliance.
% ABSENT_VOICES: Advocates for faster decision-making or more centralized EU power might argue for qualified majority voting (QMV) in more areas, but their proposals are often sidelined by the political commitment to unanimity in sensitive domains, which prioritizes consensus over speed.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished, decision-making would accelerate, but policies would likely face increased resistance, non-compliance, and potential defection from states that feel coerced, leading to a less stable and legitimate Union where collective action is harder to sustain.
% FOUNDING_PROBLEM: How to achieve deep political integration and collective action among sovereign states without undermining their fundamental national interests or perceived legitimacy, ensuring that all members feel their voice is heard and respected.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently attest to the ongoing need for consensus to ensure buy-in and prevent fragmentation. Political scientists and international relations scholars, from outside the direct beneficiaries, corroborate that the legitimacy derived from unanimity is a key factor in the EU's stability, even while acknowledging its costs in terms of efficiency.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because while the negotiation process is costly in terms of time and diplomatic effort, this reading frames these as necessary investments that yield a net benefit in policy legitimacy and reduced defection. Suppression is low (0.20) as the rule forces negotiation rather than coercing states into unwanted outcomes. Theater ratio is very low (0.05) because the unanimity rule is a core, functional aspect of EU decision-making, not a performative one. Accessibility collapse is moderate (0.40) as alternatives like Qualified Majority Voting (QMV) exist, but the political choice for unanimity in certain areas is deliberate. Resistance is low (0.10) because while states may resist specific policy proposals, they generally adhere to the unanimity rule itself as a legitimate process.
 *
 * PERSPECTIVAL GAP:
 *   The 'sovereignty_guarantor_reading' would emphasize the protection of national interests, while the 'veto_trap_reading' would highlight the inefficiencies and potential for minoritarian extraction. This 'diplomatic_capital_reading' acknowledges the costs but frames them as an investment in collective legitimacy, leading to a Rope classification where other readings might yield a Mountain (sovereignty guarantor) or a Snare (veto trap). The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states are the primary agenda-setters and beneficiaries, as they directly engage in the negotiation process and gain from the enhanced legitimacy of policies. EU citizens are indirect beneficiaries, gaining from stable and broadly accepted policies. The EU Commission acts as an observer, facilitating the process but not directly subject to the unanimity vote itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_benefit_framing_ambiguity,
    'Is the cost of iterative negotiation under unanimity a genuine investment in legitimacy (as this reading claims), or an inefficient burden that stifles effective policy-making?',
    'Comparative analysis of policy outcomes and implementation rates in areas governed by unanimity versus QMV, alongside surveys of member state satisfaction and compliance.',
    'If the costs are found to consistently outweigh the legitimacy benefits, the extractiveness metric would increase, potentially shifting the classification towards a Tangled Rope or even a Snare, as the ''cost'' becomes ''extraction''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_framing_ambiguity, conceptual, 'Ambiguity in framing negotiation costs as investment vs. burden.').

omega_variable(
    legitimacy_measurement_challenge,
    'How can the ''strengthened policy legitimacy'' claimed by this reading be empirically measured and attributed directly to the unanimity requirement?',
    'Development of robust, cross-national metrics for policy legitimacy, public trust, and compliance, controlling for other factors influencing policy success.',
    'If legitimacy gains are found to be negligible or attributable to other factors, the primary coordination benefit of this constraint would be undermined, increasing its effective extractiveness and potentially reclassifying it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_measurement_challenge, empirical, 'Empirical challenge in measuring legitimacy gains from unanimity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(eu_c_su_t40, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(eu_c_su_t50, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'eu_council_unanimity' kernel, each with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
