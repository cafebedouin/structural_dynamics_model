% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity Requirement (Diplomatic Capital / Legitimacy-Building Reading)
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   This story reads the EU Council's unanimity requirement through the
 *   diplomatic-capital lens: unanimity is expensive coordination machinery
 *   that pays for itself in legitimacy. Long negotiation rounds, package
 *   deals, and successive presidency compromise texts are the cost; the
 *   payoff is that adopted policy carries the visible, negotiated consent of
 *   every member state, which the reading holds lowers post-adoption
 *   defection and domestic backlash relative to policies imposed by
 *   qualified-majority vote. This is emphatically ONE reading among three
 *   live readings of the same kernel (eu_council_unanimity). The
 *   sovereignty_guarantor_reading treats unanimity as a foundational
 *   protection against majoritarian coercion of sovereign consent — a
 *   deontological floor, not a cost-benefit trade. The veto_trap_reading
 *   treats the identical procedural rule as a structural vulnerability that
 *   lets a single minoritarian state extract concessions via credible
 *   blocking threats — high ε, identifiable extractors and extracted-from
 *   parties. This story does not adjudicate between the three; it authors
 *   only the diplomatic-capital reading's own ε, beneficiary structure, and
 *   classification, on the understanding that the other two readings are
 *   separate constraint files with their own metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.22).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.28).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity Requirement (Diplomatic Capital / Legitimacy-Building Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional_design/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, '1f50609f-3de2-4c41-bff2-438b08a743f2').
narrative_ontology:cs_kernel_codification('1f50609f-3de2-4c41-bff2-438b08a743f2', formalized).
narrative_ontology:cs_authority_grounding('1f50609f-3de2-4c41-bff2-438b08a743f2', practice).
narrative_ontology:cs_interpretation_layer_present('1f50609f-3de2-4c41-bff2-438b08a743f2').
narrative_ontology:cs_reading_relation('1f50609f-3de2-4c41-bff2-438b08a743f2', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f50609f-3de2-4c41-bff2-438b08a743f2', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('1f50609f-3de2-4c41-bff2-438b08a743f2', foundational, negotiated_consent_produces_durable_compliance).
narrative_ontology:cs_axiom_status(negotiated_consent_produces_durable_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1f50609f-3de2-4c41-bff2-438b08a743f2', negotiated_consent_produces_durable_compliance, empirically_contingent).
narrative_ontology:cs_axiom('1f50609f-3de2-4c41-bff2-438b08a743f2', secondary, coordination_cost_and_payoff_are_broadly_shared_not_class_asymmetric).
narrative_ontology:cs_axiom_status(coordination_cost_and_payoff_are_broadly_shared_not_class_asymmetric, holdable).
narrative_ontology:cs_axiom_grounding('1f50609f-3de2-4c41-bff2-438b08a743f2', coordination_cost_and_payoff_are_broadly_shared_not_class_asymmetric, empirically_contingent).
narrative_ontology:cs_reference_frame('1f50609f-3de2-4c41-bff2-438b08a743f2', treaty_based_sovereign_pooling).
narrative_ontology:cs_drift_state('1f50609f-3de2-4c41-bff2-438b08a743f2', post_2004_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1f50609f-3de2-4c41-bff2-438b08a743f2', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, small_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, council_secretariat).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, downstream_compliance_regimes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, national_publics).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, consensus_durability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit at the same table as the largest economies and can shape final text through the iterative negotiation unanimity forces, rather than being outvoted on matters that touch core national interests. Their leverage comes entirely from the requirement that every capital sign off; without it their preferences would be aggregated away under weighted voting.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, small_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Absorb longer negotiation timelines and occasional watered-down text in exchange for policies that all twenty-seven capitals actually implement without domestic backlash or later defection. They pay in negotiating time and compromise; they gain policies that survive changes of government.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer).

% Designs and manages the iterative negotiation process — successive presidency compromise texts, informal trilogues, package deals — that converts unanimity's veto threat into eventual agreement. Its institutional value proposition rests on being able to produce durable consensus text; a shift to majority voting would reduce its centrality to the process.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, council_secretariat, agenda_setter,
    institutional, civilizational, analytical, continental).

% Benefit from policies their own government visibly negotiated and accepted rather than had imposed by a majority of other states, which reduces domestic perception of the EU as illegitimate imposition and lowers the political cost of compliance at home.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, national_publics, beneficiary,
    organized, generational, constrained, national).

% Not an actor but a cost bearer in the structural sense: negotiation rounds extend the calendar time before agreed policy takes effect, a real cost the diplomatic-capital reading treats as the price of durability rather than as extraction from any party.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, policy_implementation_timelines, payer,
    analytical, biographical, analytical, continental).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__diplomatic_capital_reading, policy_implementation_timelines).

% Compare implementation and defection rates between unanimity-reached and QMV-reached EU decisions to assess whether the consensus-durability thesis holds empirically.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, external_analysts, observer,
    analytical, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces every member state's government to be genuinely brought inside the agreement before it is adopted, converting a one-shot majority vote into an iterative negotiation that surfaces objections early and buys domestic political cover for compliance.
% TRANSFER_FUNCTION: Moves negotiating time and textual compromise from all parties into the agreement itself; no systematic transfer of resources from one fixed group to another — the cost (slower decisions, diluted text) and the payoff (durable buy-in, lower defection) are both broadly shared across participating states rather than flowing from a victim class to a beneficiary class.
% ABSENT_VOICES: Sub-national regions and civil society groups whose interests are folded into a single national position have no direct seat; their objections, if any, are absorbed or overridden inside domestic coordination before the state reaches the Council table. This is a real absence but is orthogonal to the unanimity mechanism itself, which operates state-to-state.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight in favor of pure majority voting, the iterative pre-negotiation process that currently absorbs objections before a vote would lose its point — states with minority preferences would have less incentive to negotiate in good faith since they could simply be outvoted, and post-adoption compliance would likely become more contested in states that lost the vote.
% FOUNDING_PROBLEM: Sovereign states forming a supranational body needed a decision rule that would not require any state to be bound by policy it had no part in shaping, in order to secure initial and continued participation in an unprecedented pooling of sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Council Secretariat process reviews and several member-state foreign ministries attest that unanimity-reached agreements (e.g., sanctions renewals, treaty amendments) show lower rates of non-implementation than comparable QMV-adopted directives; independent political-science literature on EU compliance (outside any Council institution) is mixed and some studies attribute durability more to domestic ratification requirements than to unanimity itself — so the corroboration is real but not unanimous among external academics.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.22) because under this reading there is no fixed group that captures value from another through the mechanism — the costs (negotiation time, textual dilution) and the payoff (durability, buy-in) are both broadly distributed across the same set of participating states, which is the rope signature rather than the tangled-rope or snare signature. Suppression is moderate-low (0.28): a state can decline to agree, which is a real alternative-preserving feature, though the diplomatic and reputational cost of being the sole holdout functions as informal pressure. Theater ratio is low and drifts only slightly upward (0.10 to 0.15) — the process is substantially functional, not performative; the slight rise reflects some routinization of compromise-package theater in later-stage negotiations as the EU has grown from 15 to 27 members.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder in this reading carries a victim role. Small states benefit through voice they would lack under weighted majority voting; large states pay in negotiating time and diluted text but benefit from durable, low-defection outcomes; national publics benefit from lower perceived illegitimacy of EU policy; the Secretariat is the agenda-setting administrator of the negotiation machinery itself. This symmetric-ish structure is why the reading claims rope rather than tangled_rope: coordination function is present and no structural payer class is isolated from the benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing sovereign buy-in for an unprecedented pooling of authority — is authored as contested rather than flatly dead: enlargement to 27 states and repeated single-state blocking episodes (e.g., on sanctions renewals, budget frameworks) have made some observers argue the mechanism now serves theatrical consensus-performance more than genuine buy-in-building. This reading holds the founding problem substantially live, distinguishing it from a piton reading in which the mechanism would be inertial performance with no live coordination payoff.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_reading_indeterminacy,
    'Is the EU Council unanimity requirement genuinely best characterized as legitimacy-building coordination cost, as sovereignty-protecting deontological floor, or as extraction-enabling veto trap — and can a single procedural rule sustain all three readings simultaneously across different policy domains?',
    'Comparative case analysis across policy domains (foreign policy/sanctions vs. tax vs. treaty change) tracking whether blocking episodes correlate with genuine sovereignty concerns, extraction-seeking behavior, or good-faith negotiation friction; would require coding a large sample of actual unanimity votes and the stated vs. revealed motives of blocking states.',
    'If veto-trap dynamics dominate empirically (frequent minoritarian extraction via blocking threats with concessions extracted rather than genuine consensus built), this reading''s low-ε classification is descriptively wrong for the domains where that dominates, and the veto_trap_reading would be the operative constraint there instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_reading_indeterminacy, conceptual, 'Whether the diplomatic-capital reading, sovereignty-guarantor reading, or veto-trap reading is the empirically dominant structural characterization of unanimity''s actual operation, and whether this varies systematically by policy domain.').

omega_variable(
    consensus_durability_empirical_status,
    'Do unanimity-reached EU decisions actually show lower defection and non-implementation rates than QMV-reached decisions, once domestic ratification requirements and issue salience are controlled for?',
    'Comparative compliance studies (e.g., infringement proceeding rates, transposition delays) between unanimity and QMV legal instruments, controlling for policy domain and salience, ideally using pre-registered political science methodology outside Council-affiliated institutions.',
    'If the durability effect is weak or absent once confounds are controlled, the diplomatic-capital reading''s core empirical premise (negotiation cost buys downstream compliance) is undermined and ε should be revised upward toward a pure-cost characterization without the offsetting legitimacy payoff.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consensus_durability_empirical_status, empirical, 'Whether the claimed legitimacy-durability payoff of unanimity is empirically real or a post-hoc justification for a costly procedure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t8, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(eu_c_tr_t16, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(eu_c_tr_t32, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eu_c_be_t8, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 8, 0.19).
narrative_ontology:measurement(eu_c_be_t16, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 16, 0.2).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement(eu_c_be_t32, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 32, 0.21).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(eu_council_unanimity__diplomatic_capital_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the eu_council_unanimity kernel, decomposed per the ε-invariance principle because the same procedural rule (Council unanimity requirement) supports structurally distinct claims with different ε values: this reading (diplomatic_capital_reading, ε≈0.22, rope) treats it as legitimacy-purchasing coordination cost with no fixed beneficiary/victim class; sovereignty_guarantor_reading treats it as a deontological protection independent of cost-benefit framing; veto_trap_reading treats it as extraction-enabling structural vulnerability with high ε and identifiable extractor/extracted-from parties. The diplomatic-capital reading structurally influences the veto-trap reading's operating environment: the more the negotiation process succeeds at building genuine consensus, the less room exists for pure extraction via blocking threat, and vice versa — hence 'influences' rather than 'coexists_with' or 'forecloses' toward that sibling. Against the sovereignty_guarantor_reading, this reading coexists: a state can simultaneously value unanimity for the legitimacy-building process AND treat it as a sovereignty floor, since neither premise contradicts the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
