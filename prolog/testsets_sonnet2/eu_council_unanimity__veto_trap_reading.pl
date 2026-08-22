% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__veto_trap_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule as Veto-Trap Extraction Mechanism
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This story is the veto_trap reading of the eu_council_unanimity kernel:
 *   the same procedural rule that requires all member states to consent
 *   before certain Council measures pass, read specifically as a structural
 *   vulnerability exploited by individual states for extraction. Under this
 *   reading, the rule's coordination story (sovereignty protection) is cover
 *   for a persistent pattern in which a single state's credible refusal to
 *   consent forces the coalition majority to buy its cooperation through
 *   side-payments, carve-outs, and diluted measures. This is deliberately ONE
 *   of three readings of the same kernel; the sovereignty_guarantor_reading
 *   and diplomatic_capital_reading are separate constraint stories with their
 *   own ε values, beneficiary/victim structures, and classifications — they
 *   are not folded into this one, per the ε-invariance principle. Where this
 *   reading finds high extraction concentrated on a single beneficiary seat,
 *   the sibling readings find low or diffuse extraction; that is not a
 *   contradiction to resolve but the expected structural delta between
 *   readings of a contested kernel.
 *
 * KEY AGENTS:
 *   - blocking_member_state: Primary beneficiary (moderate/arbitrage) — extracts concessions via credible non-consent
 *   - coalition_majority_states: Primary target (organized/constrained) — pays through dilution and side-payments
 *   - sanctioned_third_party_targets: Secondary target (powerful/trapped) — receives weakened measures with no seat at the table
 *   - eu_commission_agenda: Structural payer (institutional, non-agent) — pre-filters proposals to avoid triggering blocks
 *   - eu_legal_scholars: Analytical observer — documents the pattern across votes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.79).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.62).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule as Veto-Trap Extraction Mechanism").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, '8f996fe7-469b-43a7-b932-08330e5ad758').
narrative_ontology:cs_kernel_codification('8f996fe7-469b-43a7-b932-08330e5ad758', formalized).
narrative_ontology:cs_authority_grounding('8f996fe7-469b-43a7-b932-08330e5ad758', extraction).
narrative_ontology:cs_interpretation_layer_present('8f996fe7-469b-43a7-b932-08330e5ad758').
narrative_ontology:cs_reading_relation('8f996fe7-469b-43a7-b932-08330e5ad758', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f996fe7-469b-43a7-b932-08330e5ad758', eu_council_unanimity__diplomatic_capital_reading, influences).
narrative_ontology:cs_axiom('8f996fe7-469b-43a7-b932-08330e5ad758', foundational, consent_leverage_is_rent_not_right).
narrative_ontology:cs_axiom_status(consent_leverage_is_rent_not_right, holdable).
narrative_ontology:cs_axiom_grounding('8f996fe7-469b-43a7-b932-08330e5ad758', consent_leverage_is_rent_not_right, empirically_contingent).
narrative_ontology:cs_axiom('8f996fe7-469b-43a7-b932-08330e5ad758', secondary, unanimity_function_has_decoupled_from_stated_purpose).
narrative_ontology:cs_axiom_status(unanimity_function_has_decoupled_from_stated_purpose, holdable).
narrative_ontology:cs_axiom_grounding('8f996fe7-469b-43a7-b932-08330e5ad758', unanimity_function_has_decoupled_from_stated_purpose, empirically_contingent).
narrative_ontology:cs_reference_frame('8f996fe7-469b-43a7-b932-08330e5ad758', post_maastricht_consensus_norm).
narrative_ontology:cs_drift_state('8f996fe7-469b-43a7-b932-08330e5ad758', post_2014_sanctions_regime_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8f996fe7-469b-43a7-b932-08330e5ad758', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, sanctioned_third_party_targets).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_commission_agenda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, small_member_state_negotiators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a single vote equal in formal weight to any other member state under the unanimity rule for the relevant policy area (e.g. sanctions, taxation, enlargement, foreign policy). Withholds consent from a measure the rest of the Council supports, signaling it will not lift the block absent specific concessions: a side-payment, a carve-out for a domestic industry, an opt-out clause, or an unrelated policy trade. Because the measure legally cannot proceed without its consent, its threat to hold out is fully credible at zero cost to itself until the majority capitulates or the measure dies.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, beneficiary,
    moderate, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter).

% Represent a large preference-weighted majority of member states and population but cannot convert that majority into a legally binding outcome. Must either concede resources or policy content to the blocking state, dilute the measure into something weaker than the majority's actual preference, or accept indefinite delay. Exit from the negotiation means abandoning the policy entirely, which is often costlier than paying the extraction.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    organized, biographical, constrained, continental).

% External states or entities that would be subject to a proposed sanctions package or foreign-policy measure. When a member state blocks or waters down the measure in exchange for concessions, the sanctioned target experiences a weaker or delayed constraint than the coalition majority intended — an indirect beneficiary of the veto trap's dilution effect, though not a party to the negotiation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, sanctioned_third_party_targets, payer,
    powerful, immediate, trapped, global).

% The Commission's proposed policy program is systematically narrowed to whatever survives serial veto negotiation. Ambitious proposals are pre-filtered or watered down in drafting specifically to avoid triggering a block, meaning the extraction shapes the agenda before a vote is ever taken, not only at the point of a formal veto.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission_agenda, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__veto_trap_reading, eu_commission_agenda).

% Officials in smaller or lower-GDP member states who have learned that credible blocking threats are the most reliable lever their state has for extracting transfers or exemptions it could not obtain through weighted voting. They actively cultivate a reputation for following through on blocks to keep the threat credible in future rounds.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, small_member_state_negotiators, beneficiary,
    moderate, biographical, arbitrage, national).

% Study patterns of veto use, side-payments, and policy dilution across Council votes on sanctions, taxation, and treaty amendments. Document the frequency with which unanimity requirements produce concession bargains rather than genuine consensus, informing debates on qualified-majority voting reform.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_legal_scholars, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, unanimity coordinates member states around measures that bind all of them by ensuring no state is subject to a collective decision it never agreed to — a genuine coordination problem when sovereignty stakes are high.
% TRANSFER_FUNCTION: Moves policy content, side-payments, and negotiating leverage from the coalition majority (and, indirectly, from third-party sanctions targets) to whichever member state credibly threatens to withhold consent, in the form of concessions, carve-outs, or diluted measures.
% ABSENT_VOICES: Third-party states or entities targeted by a proposed sanctions package have no seat in the Council negotiation at all — their exposure is decided entirely by the internal bargain between the blocking state and the majority, with no voice of their own in how much dilution they receive.
% DISAPPEARANCE_RATIONALE: If unanimity were replaced by qualified majority voting in the relevant policy areas overnight, blocking states would lose their extraction leverage immediately, sanctions and foreign-policy measures would pass closer to majority preference, and the entire practice of pre-negotiation side-payments to avoid a veto would collapse — the Commission's agenda-setting behavior and member-state negotiating strategy would both reorganize substantially.
% FOUNDING_PROBLEM: Unanimity was built to prevent a majority of member states from imposing binding obligations on a state whose core sovereignty interests were directly implicated, particularly in foreign policy, taxation, and treaty change, where consent was treated as a precondition for legitimate collective action.
% FOUNDING_PROBLEM_CORROBORATION: Blocking states and their negotiators attest the sovereignty-protection problem remains fully live. Independent political scientists and several Commission officials attest, based on patterns of concession-extraction documented in sanctions and tax-harmonization votes, that the mechanism has substantially shifted from protecting genuine sovereignty interests toward routine leverage-extraction largely disconnected from any live sovereignty concern in the specific blocked measure.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__veto_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_council_unanimity__veto_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.79 at interval end) reflects the accumulated evidence, drawn from sanctions and tax-harmonization votes, that unanimity produces systematic concession bargains rather than one-off holdouts over genuine sovereignty stakes. Suppression (0.62) is moderate-high: the coalition majority is not physically coerced, but is structurally barred from any legal path around a single state's non-consent, which functions as a powerful suppressive mechanism on its preferred outcome. Theater ratio (0.40) captures that a meaningful share of Council deliberation now performs consensus-building ritual around a bargain whose real content is a private side-payment, not the ostensible sovereignty concern. All three temporal series share one time grid (T=0 to T=24) per the alignment rule.
 *
 * PERSPECTIVAL GAP:
 *   Under the veto_trap reading, the blocking state's own account (protecting a vital national interest) and the coalition majority's account (paying ransom for a policy it could pass on the merits) look like the same negotiation viewed from opposite ends — the engine computes each seat's classification from the structural data (power, exit, beneficiary/victim role) rather than from either party's stated justification, which is exactly how a coordination cover story is distinguished from the underlying extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking_member_state is the structural beneficiary: it collects concessions at effectively zero direct cost, and its exit options are best described as arbitrage — it can play the same leverage move in future rounds, so d sits near the full-beneficiary end. The coalition_majority_states bear the transfer and cannot exit the negotiation without abandoning the substantive goal, placing them near the full-target end. sanctioned_third_party_targets are trapped and bear costs (or receive under-strength restrictions) without ever being a party to the bargain that determined those costs — a pure victim seat with no negotiating presence at all. small_member_state_negotiators are grouped with beneficiaries because, under this reading, they are agents actively deploying the same mechanism the blocking_member_state exemplifies, not passive recipients of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting sovereignty from majoritarian coercion) is authored as contested rather than flatly dead, because in some specific votes the blocking state's sovereignty concern is genuinely live. But the R5 corroboration shows that outside observers (political scientists, some Commission officials) increasingly find the mechanism decoupled from any live sovereignty concern in the specific measure being blocked — it has become a generalized leverage tool independent of the substantive stake. This is the mandatrophy signature: a mandate (sovereignty protection) that has partially outlived its founding function while the procedural form persists unchanged and is now used for a different purpose (extraction) than the one that justified it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_trap_vs_sovereignty_guarantor_boundary,
    'For any given blocked measure, is the blocking state exercising a genuine sovereignty veto over a decision that materially implicates its core national interest, or is it exercising a generalized leverage tool disconnected from the specific measure''s sovereignty stakes?',
    'Case-by-case analysis of blocked measures against the blocking state''s declared and revealed sovereignty interest in the specific policy area, cross-referenced against the concessions actually extracted (unrelated side-payments vs. narrow carve-outs tailored to the stated sovereignty concern).',
    'If sovereignty stakes are consistently present and the concessions are narrowly tailored to those stakes, this reading''s high-ε classification overstates the extraction and the sovereignty_guarantor_reading is closer to descriptively accurate for that measure. If concessions are consistently unrelated side-payments, this reading''s classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_trap_vs_sovereignty_guarantor_boundary, empirical, 'Whether veto use tracks genuine sovereignty stakes or generalized leverage extraction, on a case-by-case basis.').

omega_variable(
    diplomatic_capital_reading_overlap,
    'Does the iterative negotiation process that produces a final concession bargain also generate genuine legitimacy gains for the resulting policy (the diplomatic_capital_reading''s claim), such that some of what this reading counts as pure extraction is partly offset by legitimacy value the coalition majority itself receives?',
    'Comparative study of policy compliance and domestic ratification smoothness for measures passed via unanimity-with-concessions versus counterfactual qualified-majority-passed measures in comparable domains.',
    'If legitimacy gains are substantial and shared by the coalition majority, part of the measured extraction may be better modeled as a joint coordination cost rather than a pure one-directional transfer, which would argue for treating this reading''s ε as an upper bound rather than a settled value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diplomatic_capital_reading_overlap, conceptual, 'Whether legitimacy gains from iterative consensus-building offset the extraction this reading identifies.').

omega_variable(
    third_party_target_exclusion_severity,
    'How much of the sanctions-dilution effect experienced by third-party targets is attributable specifically to unanimity-driven veto trading, versus other factors (diplomatic relationships, economic interdependence, general EU foreign-policy caution)?',
    'Comparative analysis of sanctions severity in unanimity-governed EU foreign policy versus qualified-majority-governed EU trade measures against comparable targets.',
    'A high isolated effect would strengthen the claim that third-party targets are a genuine victim class of this specific mechanism; a low isolated effect would suggest the sanctions_target victim designation overstates this constraint''s causal role relative to other EU foreign-policy dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(third_party_target_exclusion_severity, empirical, 'How much sanctions dilution is specifically attributable to the veto-trap mechanism versus other causes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eu_c_tr_t4, eu_council_unanimity__veto_trap_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__veto_trap_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__veto_trap_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__veto_trap_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__veto_trap_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(eu_c_be_t4, eu_council_unanimity__veto_trap_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__veto_trap_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__veto_trap_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__veto_trap_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__veto_trap_reading, base_extractiveness, 24, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(eu_c_su_t4, eu_council_unanimity__veto_trap_reading, suppression_requirement, 4, 0.49).
narrative_ontology:measurement(eu_c_su_t8, eu_council_unanimity__veto_trap_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__veto_trap_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(eu_c_su_t16, eu_council_unanimity__veto_trap_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__veto_trap_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.1).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_qualified_majority_voting_reform).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the eu_council_unanimity kernel, decomposed per the ε-invariance principle rather than represented as one constraint with an observable parameter. sovereignty_guarantor_reading and diplomatic_capital_reading are separate files with their own ε values and classifications. All three should be treated as a constraint family; each links to the other two via affects_constraints. This reading also links to eu_qualified_majority_voting_reform, since the extraction pattern documented here is the primary structural argument cited by reform advocates for replacing unanimity with qualified majority voting in the affected policy areas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
