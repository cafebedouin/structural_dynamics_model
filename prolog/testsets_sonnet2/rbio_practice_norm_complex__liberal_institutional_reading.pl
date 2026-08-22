% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: Rules-Based International Order — Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This story instantiates the liberal institutional reading of the
 *   rules-based international order (RBIO) kernel: the norms are universal in
 *   principle, consent-based through treaty ratification and institutional
 *   membership, and revisable through legitimate multilateral processes such
 *   as UN Charter amendment, treaty renegotiation, or new customary law
 *   formation. On this reading, the well-documented selectivity of
 *   enforcement — intervention in some atrocities and not others, sanctions
 *   on some norm violators and not others — is a capacity and political-will
 *   problem (the UNSC cannot act everywhere it should, coalitions are costly
 *   to assemble) rather than evidence that the legitimacy claim itself is
 *   false. Economic conditionality attached to loans or sanctions relief is
 *   read as a contractual bargain: states and populations retain the formal
 *   option of non-compliance and its costs, so the arrangement is not
 *   coercion in the strict sense the sovereignty-maximalist or
 *   hegemonic-extraction readings would apply. This is DELIBERATELY one
 *   reading among three of the same underlying kernel
 *   (rbio_practice_norm_complex); the sibling readings
 *   (hegemonic_extraction_reading, sovereignty_maximalist_reading) are
 *   separate constraint files with their own epsilon values and are not
 *   synthesized here.
 *
 * KEY AGENTS:
 *   - intervening_states: primary agenda-setters and beneficiaries of enforcement discretion
 *   - unsc_permanent_members: veto-holders whose consent is required for authorized action
 *   - multinational_reconstruction_contractors: downstream beneficiaries of post-intervention markets
 *   - international_financial_institutions: administer conditionality as contract terms
 *   - sanctioned_state_civilian_populations: bear humanitarian costs with no direct voice
 *   - targeted_state_governments: formally sovereign, substantively constrained
 *   - small_states_outside_unsc: consent-bearing but enforcement-dependent on sponsorship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.42).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.38).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "Rules-Based International Order — Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9').
narrative_ontology:cs_kernel_codification('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', formalized).
narrative_ontology:cs_authority_grounding('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', lineage).
narrative_ontology:cs_interpretation_layer_present('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9').
narrative_ontology:cs_reading_relation('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', foundational, enforcement_selectivity_is_capacity_limited_not_illegitimate).
narrative_ontology:cs_axiom_status(enforcement_selectivity_is_capacity_limited_not_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', enforcement_selectivity_is_capacity_limited_not_illegitimate, empirically_contingent).
narrative_ontology:cs_axiom('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', foundational, state_consent_via_treaty_ratification_constitutes_genuine_legitimation).
narrative_ontology:cs_axiom_status(state_consent_via_treaty_ratification_constitutes_genuine_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', state_consent_via_treaty_ratification_constitutes_genuine_legitimation, conventional).
narrative_ontology:cs_reference_frame('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', un_charter_consent_based_multilateralism).
narrative_ontology:cs_drift_state('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', post_iraq_war_and_sanctions_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('321d2ce4-24e8-45b0-b3ff-3590d8d4aeb9', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, multinational_reconstruction_contractors).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, small_states_outside_unsc).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene, authorize, and staff multilateral bodies; determine which crises trigger UNSC referral or coalition action; supply the military and diplomatic capacity that makes enforcement possible at all. Present the system as neutral rule-application while retaining veto or near-veto control over which rules get applied where.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary).

% Hold veto power over any Security Council authorization, meaning enforcement against their own conduct or their allies' conduct can be blocked unilaterally. Frame this as a structural safeguard against precipitous action rather than as selective immunity.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Win reconstruction, security, and development contracts that follow intervention and conditionality agreements. Their revenue depends on continued demand for post-intervention services; they have no formal role in the legitimacy debate but structurally benefit from its outcomes.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, multinational_reconstruction_contractors, beneficiary,
    organized, biographical, mobile, global).

% Administer conditionality attached to loans and sanctions relief, treating economic terms as contractual bargains freely entered rather than coercive impositions. Set technical criteria that borrowing states must meet, with significant discretion over how strictly those criteria are applied.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, international_financial_institutions, agenda_setter).

% Bear the humanitarian cost of sanctions regimes and economic conditionality imposed on their governments — shortages, currency collapse, restricted medical imports — regardless of whether they had any voice in the conduct that triggered the measures. Cannot exit the jurisdiction or the consequences.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, sanctioned_state_civilian_populations, payer,
    powerless, immediate, trapped, national).

% Formally retain sovereign standing and a vote in multilateral fora but face sanctions, conditionality, or intervention when judged to violate RBIO norms. Their consent to the underlying framework was often given decades earlier under different geopolitical conditions, and withdrawal from the framework carries severe costs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_state_governments, payer,
    moderate, biographical, constrained, national).

% Vote in the General Assembly and participate in treaty processes but have no veto and limited capacity to initiate enforcement action on their own behalf when they are the ones harmed. Depend on great-power sponsorship to have grievances taken up at all.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, small_states_outside_unsc, payer,
    powerless, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, small_states_outside_unsc, excluded).

% Administer the day-to-day machinery of multilateral processes — treaty monitoring, fact-finding missions, technical reporting — without the political authority to compel compliance from powerful states. Document norm violations that then depend on political actors for any enforcement follow-through.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_secretariat_and_technical_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, diffuse).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, negotiated set of norms — sovereignty, non-aggression, human rights baselines, trade rules — that states have consented to through treaty ratification and institutional membership, reducing the transaction costs of ad hoc bilateral bargaining over every dispute and enabling collective responses to genuine atrocity and aggression.
% TRANSFER_FUNCTION: Moves enforcement capacity, reconstruction contracts, and conditional financing toward intervening and creditor states and their firms, while moving compliance costs, sanctions burdens, and sovereignty constraints toward target states and, disproportionately, their civilian populations.
% ABSENT_VOICES: Civilian populations under sanctions have no seat in the Security Council or in conditionality negotiations conducted between governments and international financial institutions; small non-P5 states have formal votes but no capacity to compel enforcement of norms in their own favor without great-power sponsorship.
% DISAPPEARANCE_RATIONALE: From this reading's perspective, if RBIO norms disappeared overnight the world would rearrange substantially — the shared vocabulary for legitimate versus illegitimate state conduct, the machinery for authorizing collective responses to atrocity, and the negotiated baseline for trade and human rights would all vanish, likely replaced by purely bilateral power bargaining. Sibling readings dispute whether anything of substance would actually change, given how selectively the norms are already enforced.
% FOUNDING_PROBLEM: The post-1945 international system needed a framework to prevent great-power war, provide legitimate mechanisms for collective security action, and establish baseline human rights and trade norms that all states could consent to and revise through negotiation rather than unilateral force.
% FOUNDING_PROBLEM_CORROBORATION: Liberal international relations scholars and many mid-sized democracies attest the founding problem remains live and the framework has adapted through treaty revision and new institutions (ICC, WTO dispute mechanisms). Scholars of the Global South and target-state governments — outside the intervening-state beneficiary set — attest the founding problem has been substantially supplanted by selective enforcement serving P5 interests; this corroboration gap is itself the subject of the kernel contest.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, contested).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).
:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 rather than high, because this reading's own lights hold that consent (treaty ratification, UN membership) and revisability (amendment processes, new customary law) are genuinely operative, not merely nominal — the extraction that exists flows through selective enforcement and conditionality rather than through the norm structure itself being a sham. Suppression is moderate (0.38): the framework relies on real coercive capacity (sanctions, and in extreme cases force) but this reading holds that the coercion is bounded by legal process and reversible through renegotiation, unlike the sovereignty-maximalist reading's characterization of humanitarian intervention as pretextual coercion. Theater ratio is moderate-low (0.30) — this reading holds that multilateral deliberation (Security Council debate, treaty conferences, ICJ proceedings) is substantially functional, not purely performative, though it acknowledges some erosion as enforcement gaps widened after 2003.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (intervening states, P5 members, IFIs) experience the arrangement as legitimate coordination they helped build and continue to consent to; the payer seats (sanctioned civilian populations, targeted governments, non-UNSC small states) experience the same structure as asymmetric extraction they cannot exit. This reading's central claim is that this divergence reflects a capacity gap in enforcement, not a legitimacy defect in the norms — but the engine computes seat-level classification from the structural directionality data regardless of which narrative frame is asserted, which is precisely the test this story exists to run.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and P5 members sit near the full-beneficiary end: they set the agenda, retain veto leverage, and face no comparable external constraint on their own conduct. Reconstruction contractors and IFIs benefit indirectly but substantially from the downstream flows the framework generates. Sanctioned civilian populations sit at the full-target end: trapped, powerless, immediate time horizon, bearing costs from decisions in which they had no vote. Targeted governments and small non-UNSC states occupy an intermediate position — formally sovereign and consenting parties to the framework, but structurally unable to compel its even-handed application in their favor.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy by holding that the founding problem (preventing great-power war, providing legitimate collective-security mechanisms, establishing negotiable human-rights and trade baselines) remains substantially live — the framework has been revised through new treaties, the ICC, and WTO mechanisms, which this reading treats as evidence of ongoing functional renewal rather than institutional fossilization. Whether this self-assessment survives contact with the corroboration requirement (attestation from outside the beneficiary set) is exactly the open question the R5 fields are designed to surface, and here the corroboration is genuinely split.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_legitimacy_selectivity,
    'Is enforcement selectivity (acting on some atrocities/violations and not others) best explained by genuine capacity and coalition-building constraints, or does the pattern of selectivity track P5 interests too consistently to be a capacity artifact?',
    'Comparative case analysis of UNSC referral and non-referral decisions against P5 member interests and non-P5 alignment; if selectivity correlates strongly and consistently with P5 patron relationships rather than atrocity severity, the capacity explanation weakens substantially.',
    'If selectivity tracks interest rather than capacity, this reading''s core distinguishing claim collapses toward the hegemonic_extraction_reading, and the effective extraction of the standing arrangement is higher than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_vs_legitimacy_selectivity, empirical, 'Whether enforcement selectivity is genuinely capacity-limited or interest-tracking.').

omega_variable(
    conditionality_as_contract_or_coercion,
    'Is economic conditionality attached to loans and sanctions relief a genuinely consensual contract term, given the asymmetric bargaining position of states facing balance-of-payments crises or sanctions pressure?',
    'Analysis of whether borrowing/sanctioned states retain a realistic non-compliance option with tolerable costs, versus conditionality functioning as a take-it-or-leave-it imposition under duress.',
    'If realistic non-compliance options do not exist, the ''contract terms'' framing this reading relies on is a legitimating fiction, and directionality for targeted governments and their populations should shift further toward the full-target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_as_contract_or_coercion, conceptual, 'Whether conditionality is contractual consent or coercion under a different name.').

omega_variable(
    revisability_in_practice,
    'How often has the RBIO framework actually been revised through the legitimate multilateral processes this reading points to (Charter amendment, new binding treaty law), versus merely reinterpreted by powerful states without formal amendment?',
    'Count and characterize formal UN Charter amendments and major treaty revisions since 1945 against instances of powerful-state reinterpretation of existing norms (e.g., expanded self-defense doctrines) that bypassed formal amendment.',
    'If formal revision is rare and most change occurs through unilateral reinterpretation by powerful states, the ''revisable through legitimate multilateral process'' claim central to this reading is weaker than authored, pushing this reading''s classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revisability_in_practice, empirical, 'Whether norm change occurs through legitimate multilateral revision or unilateral reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(rbio_tr_t2003, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2003, 0.28).
narrative_ontology:measurement(rbio_tr_t2014, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2014, 0.29).
narrative_ontology:measurement(rbio_tr_t2025, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1960, 0.3).
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(rbio_be_t2003, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(rbio_be_t2014, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(rbio_be_t2025, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(rbio_su_t2003, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2003, 0.35).
narrative_ontology:measurement(rbio_su_t2014, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2014, 0.36).
narrative_ontology:measurement(rbio_su_t2025, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__liberal_institutional_reading, 0.1).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rbio_practice_norm_complex kernel, decomposed per the epsilon-invariance principle because the three readings assign substantially different epsilon values to the same standing arrangement (this reading: 0.42; hegemonic_extraction_reading: expected substantially higher, treating selectivity as proof of extractive intent; sovereignty_maximalist_reading: expected to treat the entire humanitarian-intervention apparatus as extractive pretext). All three should be linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
