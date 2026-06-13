% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity (Sovereignty Guarantor Reading)
 *   domain: institutional/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty guarantor' reading of the EU
 *   Council's unanimity rule, where it is understood as a fundamental
 *   protection against majoritarian coercion, ensuring each state's consent
 *   on matters implicating national sovereignty. This reading emphasizes the
 *   legitimate exercise of veto power as a defense of national interests,
 *   particularly for smaller member states. The constraint is claimed as a
 *   Rope, reflecting its genuine coordination function in building trust and
 *   ensuring voluntary participation, with moderate extraction primarily due
 *   to coordination costs rather than systematic rent-seeking.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.3).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '9273c214-fac8-4fc7-9bdc-2d565416f0b0').
narrative_ontology:cs_kernel_codification('9273c214-fac8-4fc7-9bdc-2d565416f0b0', formalized).
narrative_ontology:cs_authority_grounding('9273c214-fac8-4fc7-9bdc-2d565416f0b0', lineage).
narrative_ontology:cs_interpretation_layer_present('9273c214-fac8-4fc7-9bdc-2d565416f0b0').
narrative_ontology:cs_reading_relation('9273c214-fac8-4fc7-9bdc-2d565416f0b0', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('9273c214-fac8-4fc7-9bdc-2d565416f0b0', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('9273c214-fac8-4fc7-9bdc-2d565416f0b0', foundational, state_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(state_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9273c214-fac8-4fc7-9bdc-2d565416f0b0', state_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('9273c214-fac8-4fc7-9bdc-2d565416f0b0', foundational, collective_action_requires_consent).
narrative_ontology:cs_axiom_status(collective_action_requires_consent, holdable).
narrative_ontology:cs_axiom_grounding('9273c214-fac8-4fc7-9bdc-2d565416f0b0', collective_action_requires_consent, conventional).
narrative_ontology:cs_reference_frame('9273c214-fac8-4fc7-9bdc-2d565416f0b0', post_westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('9273c214-fac8-4fc7-9bdc-2d565416f0b0', contemporary_eu_integration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9273c214-fac8-4fc7-9bdc-2d565416f0b0', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states view unanimity as their primary defense against being outvoted by larger states on matters of national sovereignty. The veto power ensures their interests are protected in critical policy areas, even if it slows down decision-making.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, national).

% While benefiting from overall EU stability, these states often bear the costs of delayed or blocked decisions due to a single state's veto. They see unanimity as a potential impediment to effective governance, but acknowledge its role in maintaining state buy-in.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer,
    powerful, biographical, constrained, national).

% The institutions administer the unanimity rule, facilitating negotiations and seeking consensus. They are constrained by the need to secure unanimous consent on sensitive issues, which can lead to legislative gridlock but ensures broad legitimacy for adopted policies.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from the protection of national interests and the stability that comes from states feeling their sovereignty is respected. They may indirectly pay the cost of slower decision-making or less ambitious policy outcomes in areas requiring unanimity.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective action on sensitive issues, particularly those touching national sovereignty, proceeds only with the explicit consent of all member states, thereby preventing majoritarian coercion and preserving the voluntary nature of the union.
% TRANSFER_FUNCTION: Transfers the power to block collective action from the majority to any single member state on issues requiring unanimity, effectively distributing veto power as a safeguard.
% ABSENT_VOICES: Advocates for a more efficient, federalized EU might argue that the unanimity rule excessively empowers individual states, hindering the collective good. Their voices are present in broader political discourse but are structurally overridden by the unanimity requirement itself on specific issues.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight, especially on core sovereignty issues, many smaller states would perceive a fundamental shift in the balance of power, potentially leading to increased distrust, withdrawal threats, or a re-evaluation of their commitment to the EU. The institutional design of the EU would fundamentally change.
% FOUNDING_PROBLEM: To prevent any single member state from being coerced into collective action against its fundamental sovereign interests, particularly after historical experiences of larger powers dominating smaller ones in international agreements.
% FOUNDING_PROBLEM_CORROBORATION: Small member states consistently attest to the live status of this problem, citing ongoing concerns about national interest protection. Academic analyses of international relations and institutional design, from outside the direct beneficiaries, corroborate the historical and ongoing relevance of this foundational protection for voluntary unions.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) because while unanimity can slow down decision-making and lead to compromises that are not optimal for all, it does not systematically extract resources from any specific group for the benefit of another in this reading. Suppression is low (0.1) as the rule is a self-imposed procedural safeguard, not an actively enforced coercion against dissent. Theater ratio is low (0.05) as the function of protecting sovereignty is genuinely performed. Accessibility collapse is high (0.7) because for issues requiring unanimity, there are no alternative paths to collective action within the EU framework. Resistance is low (0.05) because the rule is widely accepted as a foundational principle, even by those who find it cumbersome.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of small member states, this is a pure Rope, a vital safeguard. From the perspective of larger states or those advocating for deeper integration, it might be seen as a 'veto trap' (another reading of this kernel), leading to gridlock. This story focuses on the 'sovereignty guarantor' perspective, where the veto is a legitimate right, not an extractive tool.
 *
 * DIRECTIONALITY LOGIC:
 *   Small member states are clear beneficiaries, as the rule directly protects their sovereign interests. All member states are also beneficiaries in that the rule underpins the voluntary nature and stability of the union. Large member states are payers in terms of potential policy delays or compromises, but they also benefit from the overall stability. EU institutions are agenda-setters, administering the rule. EU citizens are indirect beneficiaries of national sovereignty protection, but also indirect payers of any inefficiencies.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_legitimacy_ambiguity,
    'Is the exercise of a veto under the unanimity rule a legitimate defense of national sovereignty, or an opportunistic act of minoritarian extraction?',
    'Analysis of specific veto instances: if vetoes consistently align with core national interests and are not traded for unrelated concessions, it supports the sovereignty defense. If vetoes are used as bargaining chips for unrelated gains, it supports the extraction reading.',
    'If vetoes are primarily extractive, the constraint''s effective extractiveness would be higher, shifting its classification towards a Tangled Rope or Snare from the perspective of other states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_legitimacy_ambiguity, empirical, 'Distinguishing legitimate sovereignty defense from opportunistic extraction in veto use.').

omega_variable(
    coordination_cost_vs_extraction,
    'What proportion of the ''cost'' associated with unanimity (e.g., delayed decisions, diluted policies) is an unavoidable coordination cost, and what is a consequence of strategic blocking for unrelated gains?',
    'Detailed case studies of negotiation processes and outcomes, quantifying the value of concessions made to secure unanimous consent versus the value of the policy outcome itself.',
    'A higher proportion of strategic blocking would increase the effective extractiveness, challenging the ''Rope'' classification and pushing it towards ''Tangled Rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction, empirical, 'Quantifying the true nature of costs associated with unanimity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 1993, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t1993, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 1993, 0.05).
narrative_ontology:measurement(eu_c_tr_t2003, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2003, 0.05).
narrative_ontology:measurement(eu_c_tr_t2013, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2013, 0.05).
narrative_ontology:measurement(eu_c_tr_t2023, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 2023, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t1993, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 1993, 0.25).
narrative_ontology:measurement(eu_c_be_t2003, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2003, 0.28).
narrative_ontology:measurement(eu_c_be_t2013, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2013, 0.3).
narrative_ontology:measurement(eu_c_be_t2023, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 2023, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t1993, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 1993, 0.1).
narrative_ontology:measurement(eu_c_su_t2003, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2003, 0.1).
narrative_ontology:measurement(eu_c_su_t2013, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2013, 0.1).
narrative_ontology:measurement(eu_c_su_t2023, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 2023, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the EU Council's unanimity rule. This 'sovereignty guarantor' reading emphasizes protection against majoritarian coercion, while other readings focus on extraction or consensus-building.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
