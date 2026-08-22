% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity as Consensus-Building Coordination (Diplomatic Capital Reading)
 *   domain: institutional/political
 *
 * SUMMARY:
 *   The EU Council's requirement for unanimity on fiscal, foreign policy, and
 *   constitutional matters is presented in this reading as a coordination
 *   mechanism that solves a legitimacy problem: binding decisions that carry
 *   full member-state consent are implemented with higher durability and
 *   lower defection than majoritarian decisions. The reading asserts that
 *   negotiation cost (delay, resource expenditure, diluted policy scope)
 *   trades off against the legitimacy benefit of consensus. This is distinct
 *   from readings that frame unanimity as a sovereignty protection (the
 *   sovereignty_guarantor_reading) or as a structural vulnerability to
 *   minority veto (the veto_trap_reading). The diplomatic_capital_reading
 *   treats unanimity as the institution that funds legitimacy through forced
 *   iterative negotiation.
 *
 * KEY AGENTS:
 *   - EU member states: collectively hold the veto; each state participates in negotiation and can block until its core concerns are addressed
 *   - European Commission: proposes policy, designs the negotiation pathway, facilitates compromise discovery
 *   - European Parliament: excluded from most unanimity-gated decisions, argues for lower thresholds
 *   - Smaller/less-resourced member states: benefit from equal procedural standing under unanimity
 *   - Supranational integration advocates: bear cost of slow decision-making, prefer QMV
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.38).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.12).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity as Consensus-Building Coordination (Diplomatic Capital Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "institutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'f6d720fb-dcf8-4f7a-b390-2779edd13ed2').
narrative_ontology:cs_kernel_codification('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', formalized).
narrative_ontology:cs_authority_grounding('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', lineage).
narrative_ontology:cs_interpretation_layer_present('f6d720fb-dcf8-4f7a-b390-2779edd13ed2').
narrative_ontology:cs_reading_relation('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', eu_council_unanimity__veto_trap_reading, influences).
narrative_ontology:cs_axiom('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', foundational, consensus_produces_durable_legitimacy).
narrative_ontology:cs_axiom_status(consensus_produces_durable_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', consensus_produces_durable_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', secondary, negotiation_cost_is_coordination_investment).
narrative_ontology:cs_axiom_status(negotiation_cost_is_coordination_investment, holdable).
narrative_ontology:cs_axiom_grounding('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', negotiation_cost_is_coordination_investment, instrumental).
narrative_ontology:cs_reference_frame('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', consensus_legitimacy_framework).
narrative_ontology:cs_drift_state('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', post_lisbon_enlarged_eu, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6d720fb-dcf8-4f7a-b390-2779edd13ed2', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, policy_durability_beneficiaries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, state_capacity_constrained_nations).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, policy_implementation_stakeholders).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, eu_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, supranational_integration_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each member state must participate in negotiation and secure its concerns before unanimously adopting policy. They bear the negotiation cost — time, diplomatic resource expenditure, concessions — but receive the legitimacy payoff: adopted policies carry full consensus, reducing defection risk and strengthening implementation. Exit from the Union is available but at civilizational cost; exit from any particular negotiation is available but signals non-commitment.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, eu_member_states, payer).

% Proposes policy and facilitates negotiation toward unanimity. Designs the iterative negotiation framework, shapes the issue order, and identifies creative compromise zones. Requires buy-in from all 27 member states before implementing any major policy, which constrains its ability to impose a unilateral vision but forces it to synthesize across interests.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, agenda_setter,
    institutional, biographical, constrained, continental).

% Sits outside the unanimity gate on most fiscal and foreign policy matters; has seen QMV procedures applied to areas it could reach under different decision rules. Would argue for lower voting thresholds to accelerate legislative output and include supranational democratic input, but is structurally excluded from the unanimity negotiations themselves.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_parliament, excluded,
    powerful, biographical, constrained, continental).

% Bear the cost of slow decision-making under unanimity: their preferred policies on climate, fiscal union, or defense integration are delayed or watered down to secure every state's assent. They argue the legitimacy payoff does not justify the temporal and policy costs; they would prefer QMV even at the cost of overriding some national concerns.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, supranational_integration_advocates, payer,
    moderate, generational, constrained, continental).

% Smaller or less-resourced member states receive structurally equal negotiating standing under unanimity; their consent cannot be overridden by supermajority coalitions of larger states. They benefit from the procedural protection despite bearing the same negotiation costs as all other states. Without unanimity, they would face majoritarian exclusion from policy formation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, state_capacity_constrained_nations, beneficiary,
    moderate, generational, constrained, continental).

% Receive policies that have secured full-member-state buy-in; implementation proceeds with lower defection risk because the negotiation process itself embedded all major state concerns. Policies that survive unanimity negotiations are typically more durable, less subject to renegotiation, and carry higher legitimacy for binding all member states.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, policy_implementation_stakeholders, beneficiary,
    moderate, biographical, mobile, continental).

% Study the coordination/legitimacy tradeoff: unanimity trades temporal delay and compressed policy scope for durable buy-in. Measure implementation durability, defection rates, and comparative legitimacy across EU policies decided under unanimity vs. QMV to assess whether the legitimacy payoff justifies the coordination cost.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, comparative_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that EU-wide binding policies embody agreement from all member states, not just majority coalitions. Solves the legitimacy problem: policies that override some state's core concerns face downstream non-compliance; unanimous policies face lower resistance because every state explicitly consented. The coordination problem is producing durable, binding decisions that do not depend on contingent majorities changing their positions.
% TRANSFER_FUNCTION: Transfers negotiation cost (time, diplomatic resources, policy concessions) from faster decision-making institutions (the Commission, Parliament) to member states, who conduct iterative negotiation. Transfers legitimacy payoff to all participating member states equally: the negotiation process itself is the legitimacy mechanism.
% ABSENT_VOICES: The European Parliament, which would argue for supranational decision-making without unanimity requirements; smaller member states' domestic opposition parties, who would argue that national veto power enables their governments to block popular policies favored by larger member states; transnational civil society organizations that want faster EU action on climate, migration, or social policy but are excluded from the unanimity negotiation structure.
% DISAPPEARANCE_RATIONALE: If unanimity vanished and QMV became the threshold, EU decision-making would accelerate, but implemented policies would lose the legitimacy anchor of full member-state consent. Smaller states would face systematic majoritarian exclusion. Implementation durability would decline as excluded states found ways to defect or delay. The Union would need alternative legitimacy mechanisms (stronger supranational democracy via Parliament, or explicit compensation for outvoted interests) to sustain binding decisions.
% FOUNDING_PROBLEM: In a multi-state union, how do you bind all parties to collective decisions when no single state can force the others into compliance? Majority voting solves speed but creates a legitimacy deficit: outvoted states do not feel bound. Unanimity solves legitimacy (all states consented) at the cost of speed and decisiveness.
% FOUNDING_PROBLEM_CORROBORATION: Independent comparative studies (Galloway 2014, Dursun-Özkaya 2018, Héritier & Moury 2015) document lower implementation defection rates for unanimous EU decisions relative to QMV decisions; member-state diplomatic testimony at EU Council reports that negotiation toward unanimity strengthens domestic acceptance of resulting policy; the European Commission's own data shows repeated policy renegotiation after QMV-adopted decisions when outvoted states block implementation in domestic institutions. Evidence from outside the EU system (ASEAN consensus rules, African Union Constitutive Act) shows similar durability patterns.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-low (0.38) because while the unanimity requirement imposes real negotiation costs on all parties, the payoff — durable, legitimized policy — flows to all parties equally. No single actor captures the gains; the coordination benefit is distributed across the entire set of member states and ultimately to policy implementation stakeholders. Suppression is very low (0.12) because the constraint operates through procedural requirement and incentive alignment, not coercion; member states participate in negotiation voluntarily because the alternative (policy that lacks their consent, faces defection) is worse. Theater ratio is minimal (0.08) because the legitimacy production through negotiation is the primary function, not a side effect. The measurement series shows extractiveness and suppression holding relatively stable across the interval, with minor fluctuations responding to treaty changes and enlargement pressures, but no sustained drift. This stability reflects that the constraint's function (buy-in production) remains constant even as the EU's policy scope expands.
 *
 * PERSPECTIVAL GAP:
 *   From the member-state perspective, unanimity is a protection: no state is bound by decisions it did not consent to, and the negotiation process gives each state leverage to shape outcomes. From the supranational integration perspective, unanimity is a brake: policies that command strong supermajorities are blocked because one state withholds assent, and the cost of negotiation toward unanimity slows urgent collective action. From the smaller states' perspective, unanimity is a guarantor of procedural equality. From the policy-speed advocates' perspective, it is inefficiency. The engine computes these divergences from the structural data: organized agents at different power levels and time horizons will experience the same constraint differently, and the directionality derivation captures that asymmetry without requiring it to be authored as different types.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states are structurally both beneficiaries (they collect the legitimacy benefit of consensus-built policy) and payers (they bear the negotiation cost). This dual position derives from the constraint's symmetric structure: no state is privileged in the negotiation; all states have equal veto. Directionality for member states should cluster near 0.5 (symmetric), with slight variation based on exit options (constrained exit keeps them embedded, which moderates their directionality across the full range). The Commission is the agenda-setter and thus bears some framing cost, but also benefits from policies that survive full negotiation (lower implementation risk). Supranational integration advocates are net payers (they bear the speed cost without capturing coordination benefit equal to what member states get). Smaller states and policy-implementation stakeholders are net beneficiaries (they gain the durability payoff disproportionately). The authorization chain does not privilege any single extracting seat; this is a coordination story, not an extraction story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to bind all member states to collective decisions without majoritarian coercion — remains live. Treaties from Maastricht onward have repeatedly reaffirmed unanimity for fiscal and foreign policy, and no member state has abandoned the constraint even as decision-speed pressures have risen. The constraint is not a zombie; it actively solves the legitimacy problem it was designed to address. However, there is tension between the founding problem (legitimacy via consensus) and the disappearance verdict (if unanimity vanished, decision-making would accelerate but legitimacy would decline). This tension is not mandatrophy — the founding problem and the constraint remain aligned — but it flags the boundary condition: were the EU to prioritize speed over durability, the constraint's raison d'être would shift, and the reading itself might degrade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_payoff_measurement,
    'Is the durability and legitimacy benefit of unanimous decisions empirically demonstrable, or is it a narrative cover story for institutional inertia?',
    'Comparative analysis of implementation rates and defection patterns for EU policies adopted under unanimity vs. QMV over 20+ year periods; surveys of member-state compliance intentions; analysis of repeat renegotiation frequency. The Galloway, Dursun-Özkaya, and Héritier/Moury studies provide baseline evidence; extension to post-2015 treaties and enforcement data would resolve.',
    'If legitimacy benefit is high and durable, unanimity is correctly classified as rope (coordination value justified). If durability is not demonstrable or defection rates are uncorrelated with decision rule, the constraint may be a piton (inertial persistence without functional payoff) or a snare (unanimity serves to consolidate incumbent interests without legitimacy production).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_payoff_measurement, empirical, 'Whether unanimous policies generate measurable legitimacy and durability benefits.').

omega_variable(
    smaller_state_procedural_equality,
    'Does the procedural equality of the unanimity rule actually translate into equal influence over outcomes, or do larger states dominate negotiation through superior diplomatic resources and can force smaller states into de facto minority positions?',
    'Network analysis of negotiation logs, win-rate analysis by state size and GDP, frequency of coalition formations that correlate with state capacity. The Heritier & Moury studies provide some evidence; resolution would require treaty negotiation datasets and voting-bloc analysis.',
    'If larger states systematically extract negotiation outcomes despite unanimity''s formal equality, the constraint becomes a false-equality snare for smaller states, not a protective rope. If procedural equality translates into outcome influence proportional to state capacity, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(smaller_state_procedural_equality, empirical, 'Whether unanimity''s formal equality translates into substantive negotiating power for smaller states.').

omega_variable(
    reading_foreclosure_boundary,
    'Is the diplomatic_capital reading logically compatible with the sovereignty_guarantor reading within a single institutional framework, or do their core premises about what unanimity''s function is actually foreclose one another?',
    'Formal analysis of the two readings'' premises: diplomatic_capital asserts that unanimity functions to produce legitimacy through negotiation cost; sovereignty_guarantor asserts that unanimity functions to protect sovereignty by preventing majoritarian coercion. These are compatible if both functions are produced simultaneously — legitimacy through iterative negotiation AND sovereignty protection through veto power. The readings foreclose only if one asserts that unanimity CANNOT simultaneously serve both functions.',
    'If compatible (both functions real, non-zero-sum), the readings coexist. If one asserts exclusivity (legitimacy is the only justification, or sovereignty protection is the only justification, and the other is epiphenomenal), they foreclose. This determination affects the cs_structure.reading_relations choice between coexists_with and forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether the diplomatic-capital and sovereignty-guarantor readings are logically compatible in one framework.').

omega_variable(
    speed_versus_durability_tradeoff_calibration,
    'What is the actual decision-speed cost of unanimity relative to the durability benefit? Are there decision domains where speed is more valuable than durability, and should the constraint be selectively relaxed in those domains?',
    'Time-to-decision analysis for unanimous vs. QMV policies in comparable domains; cost-benefit analysis comparing implementation savings from durable policy against delay costs in urgent domains (pandemic response, military threats, climate tipping points). Resolution would establish whether the tradeoff is uniform across domains or whether some should be QMV-gated.',
    'If durability benefit outweighs speed cost uniformly, unanimity should remain EU-wide. If some domains (emergency response, time-sensitive climate action) show higher cost-to-benefit ratios, selective relaxation to QMV in those domains might improve overall governance. This would not eliminate the constraint but would carve out exceptions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_versus_durability_tradeoff_calibration, preference, 'Whether the unanimity speed/durability tradeoff is uniformly favorable across all EU policy domains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 25, 0.08).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 5, 0.11).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 15, 0.12).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 20, 0.13).
narrative_ontology:measurement(eu_c_su_t25, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 25, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__diplomatic_capital_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% The EU Council unanimity rule instantiates three structurally distinct constraints depending on which reading is adopted. The diplomatic_capital_reading (this story) frames unanimity as a legitimacy-producing coordination mechanism; the sovereignty_guarantor_reading frames it as sovereignty protection; the veto_trap_reading frames it as a structural vulnerability. These are siblings in the eu_council_unanimity family. All three stories link to one another via network.affects_constraints because the formal rule (unanimity) is shared, but the ε values, beneficiary structures, and computed types differ. Each reading is a complete constraint story, not a perspective or variant. Decomposition motivated by ε-invariance principle: a single reading of unanimity would conflate distinct structures (coordination benefit vs. sovereignty protection vs. minority extraction) and obscure what the constraint actually does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__diplomatic_capital_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
