% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__veto_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_council_unanimity__veto_trap_reading
 *   human_readable: EU Council Unanimity Rule as Veto Trap
 *   domain: political_economy/institutional_design
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement in sensitive policy domains
 *   (taxation, foreign policy, social security, treaty change) is formally a
 *   sovereignty protection mechanism. This reading — the veto_trap_reading —
 *   documents how the rule operates in practice as a structural vulnerability
 *   that enables minoritarian extraction: states with credible blocking
 *   threats systematically extract concessions, opt-outs, budget rebates, and
 *   policy carve-outs from the majority coalition and the Commission's
 *   agenda. The constraint exhibits genuine coordination function (forcing
 *   consensus) AND asymmetric extraction (blocking states are net
 *   beneficiaries, majority states are net payers), satisfying the
 *   tangled_rope structural signature. Active enforcement is required because
 *   the unanimity rule is maintained by treaty and defended by blocking
 *   states who would lose their leverage under QMV.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.68).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.72).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, tangled_rope).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity Rule as Veto Trap").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "political_economy/institutional_design").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'd5ae1aab-febd-4030-aa60-7a5614bcc2eb').
narrative_ontology:cs_kernel_codification('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', formalized).
narrative_ontology:cs_authority_grounding('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', lineage).
narrative_ontology:cs_interpretation_layer_present('d5ae1aab-febd-4030-aa60-7a5614bcc2eb').
narrative_ontology:cs_reading_relation('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', eu_council_unanimity__sovereignty_guarantor_reading, forecloses).
narrative_ontology:cs_reading_relation('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', foundational, veto_as_credible_threat_enables_systematic_extraction).
narrative_ontology:cs_axiom_status(veto_as_credible_threat_enables_systematic_extraction, holdable).
narrative_ontology:cs_axiom_grounding('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', veto_as_credible_threat_enables_systematic_extraction, empirically_contingent).
narrative_ontology:cs_axiom('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', foundational, sovereignty_protection_mandate_has_atrophied_relative_to_extraction_function).
narrative_ontology:cs_axiom_status(sovereignty_protection_mandate_has_atrophied_relative_to_extraction_function, holdable).
narrative_ontology:cs_axiom_grounding('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', sovereignty_protection_mandate_has_atrophied_relative_to_extraction_function, empirically_contingent).
narrative_ontology:cs_reference_frame('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', maastricht_sovereignty_shield).
narrative_ontology:cs_drift_state('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', post_lisbon_rule_of_law_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5ae1aab-febd-4030-aa60-7a5614bcc2eb', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, blocking_member_state).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__veto_trap_reading, veto_leveraging_governments).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority_states).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, commission_policy_agenda).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, veto_as_extraction_mechanism).
narrative_ontology:constraint_vindicates(eu_council_unanimity__veto_trap_reading, minoritarian_rent_seeking_in_council).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the unanimity requirement to credibly threaten blocking legislation that affects its interests, extracting concessions, opt-outs, budget rebates, or policy carve-outs in exchange for consent. Can escalate to actual veto if demands are not met. The threat is credible because the rule requires its consent.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_member_state, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, blocking_member_state, agenda_setter).

% A set of member states that have learned to coordinate blocking threats to extract side-payments or policy concessions. They benefit from the unanimity rule because it gives them a structural choke-point on legislation they dislike or want modified. Their exit option is mobile because they can credibly threaten to leave negotiations or form blocking minorities.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, veto_leveraging_governments, beneficiary,
    institutional, biographical, mobile, national).

% Must negotiate with blocking states to pass legislation, systematically conceding policy substance, budget resources, or legal flexibility to secure unanimity. The cost of delay and the value of passing legislation create pressure to pay the extraction price. Exit is constrained because leaving the Council framework means exiting the EU or accepting policy paralysis.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority_states, payer,
    organized, biographical, constrained, regional).

% Proposes legislation that must survive the unanimity gauntlet. The Commission waters down proposals preemptively to avoid vetoes, and invests political capital in side-deals to secure consent. Its agenda is the primary extraction target — the value transferred from its preferred policy to the blocking state's position.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, commission_policy_agenda, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, commission_policy_agenda, agenda_setter).

% Co-legislator under ordinary legislative procedure but excluded from unanimity domains (taxation, foreign policy, social security). Would object to the extraction dynamic but has no formal seat at the Council unanimity table. Its identity is fused with supranational parliamentarism, making exit from the frame unthinkable.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, european_parliament, excluded,
    institutional, generational, identity_locked, continental).

% Bear the downstream consequences of watered-down legislation, delayed action, or policy opt-outs negotiated to buy unanimity. They have no voice in Council negotiations and no exit from the resulting policies short of leaving the EU.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_citizens_policy_recipients, excluded,
    powerless, biographical, trapped, continental).

% Studies the unanimity rule's operation across policy domains and historical periods, documenting the pattern of concessions extracted by blocking states and the cumulative policy distortion. Sees the full structural asymmetry that participants experience partially.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, institutional_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Forces all member states to consent to collective action in sensitive domains, theoretically ensuring no state is bound against its will and building consensus through iterative negotiation.
% TRANSFER_FUNCTION: Moves policy substance, budget resources, legal flexibility, and political capital from the coalition majority and Commission agenda to blocking states, in exchange for their consent to legislation they would otherwise oppose.
% ABSENT_VOICES: European Parliament (excluded from unanimity domains), EU citizens who bear policy consequences, smaller member states that lack the structural leverage to extract concessions but still pay the cost of delayed or weakened legislation.
% DISAPPEARANCE_RATIONALE: If the unanimity requirement vanished overnight, qualified majority voting would become the default in its current domains. Legislation would pass faster with less watering-down, blocking states would lose their extraction choke-point, the Commission would propose bolder policies, and the EU's legislative output would shift toward majority preferences. The institutional equilibrium would fundamentally reorganize.
% FOUNDING_PROBLEM: Post-war European integration needed a rule that prevented majoritarian coercion of sovereign states in core sovereignty domains (taxation, foreign policy, social security) — ensuring no state would be forced into collective action it fundamentally opposed.
% FOUNDING_PROBLEM_CORROBORATION: Founding treaties and early community documents attest the sovereignty-protection rationale. Contemporary political scientists (e.g., Hagemann & De Clerck-Sachsse 2007; Novak 2013) and the European Parliament's own resolutions document the shift from sovereignty shield to extraction mechanism. The Commission's repeated proposals to extend QMV attest the founding problem is read as substantially solved by the institutional center.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the historical record shows systematic concessions extracted across multiple policy domains: UK budget rebate (1984), Polish veto threats on energy/climate (2000s), Hungarian/Polish vetoes on rule-of-law conditionality (2020), Maltese/Cypriot vetoes on tax harmonization (ongoing). Suppression (0.72) is high because the rule's persistence depends on treaty-level entrenchment and active defense by beneficiary states — alternatives (QMV) are structurally suppressed. Theater ratio (0.41) is moderate and rising: the consensus-building ritual increasingly masks extraction. Accessibility collapse (0.35) is moderate — QMV alternatives exist and are used in other domains, but treaty change to extend them is blocked by the same unanimity rule. Resistance (0.58) is substantial: Commission, Parliament, and majority states push for QMV extension but face structural blockage.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking state's seat, the unanimity rule appears as legitimate sovereignty protection and consensus-building — they experience the coordination function. From the majority coalition's seat, the same rule operates as an extraction mechanism — they experience the transfer function. The institutional analyst sees both simultaneously. The engine computes this seat divergence from the declared structural relationships; the claimed_type (tangled_rope) asserts the structural truth that both functions are real and simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   Blocking states sit at the beneficiary end (d ≈ 0.15): they collect concessions, control the agenda through veto threats, and have arbitrage-grade exit (can credibly threaten to block or leave negotiations). Coalition majority states sit at the target end (d ≈ 0.85): they pay concessions, suffer policy delay/dilution, and have constrained exit (leaving the unanimity framework requires treaty change they cannot achieve alone). The Commission sits near target (d ≈ 0.75): its agenda is the primary extraction target. Parliament and citizens are excluded — their structural position is not captured by directionality toward this constraint but by exclusion from it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereignty protection against majoritarian coercion) is contested: the sovereignty_guarantor_reading holds it is still live; this reading and the diplomatic_capital_reading hold it has been substantially solved by the acquis, mutual trust, and the Court's jurisprudence. The arrangement persists because the extraction function now sustains it — blocking states defend unanimity not for sovereignty but for leverage. This is mandatrophy: the mandate (sovereignty protection) has atrophied relative to the extraction function, but the constraint remains because beneficiaries capture its gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_boundary,
    'What fraction of observed concessions under unanimity are genuine consensus-building costs vs. pure minoritarian extraction?',
    'Counterfactual analysis of QMV domains: compare concession patterns in unanimity vs. QMV legislation controlling for policy salience. Natural experiments from policy areas that switched from unanimity to QMV (e.g., some internal market measures).',
    'If most concessions are extraction, the constraint is more snare-like; if most are coordination costs, it is more rope-like. The tangled_rope classification turns on this boundary being genuinely ambiguous.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether the coordination function is a genuine operational necessity or a cover story for extraction.').

omega_variable(
    sovereignty_claim_authenticity,
    'Do blocking states genuinely invoke sovereignty concerns, or is sovereignty language a strategic cover for material extraction?',
    'Discourse analysis of Council negotiating records: correlate stated justifications for veto threats with material concessions extracted. Compare cases where sovereignty is invoked vs. where material interests are explicit.',
    'If sovereignty claims are strategic cover, the sovereignty_guarantor_reading is structurally false for those cases. If genuine, the constraint family has a real sovereignty-protection component that coexists with extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_claim_authenticity, conceptual, 'Whether the sovereignty framing is authentic or instrumental.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the eu_council_unanimity kernel admit the veto_trap_reading as a structurally coherent framing, or is the ''veto trap'' an observer imposition on a genuinely consensual practice?',
    'Compare the three readings'' predictive power: which reading''s structural claims (extraction, consensus-building, sovereignty protection) best predict observed veto behavior, concession patterns, and institutional reform dynamics across the 1993-2024 interval?',
    'If veto_trap_reading has superior predictive power, it is the dominant structural description and the kernel is best understood as an extraction mechanism. If sovereignty_guarantor_reading predicts better, the kernel is a sovereignty protection with occasional abuse. The engine''s foreclosure computation will test this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s contested framings represent genuine structural ambiguity or observer projection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 1993, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t1993, eu_council_unanimity__veto_trap_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t1999, eu_council_unanimity__veto_trap_reading, theater_ratio, 1999, 0.22).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t2004, eu_council_unanimity__veto_trap_reading, theater_ratio, 2004, 0.28).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t2009, eu_council_unanimity__veto_trap_reading, theater_ratio, 2009, 0.33).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t2014, eu_council_unanimity__veto_trap_reading, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t2019, eu_council_unanimity__veto_trap_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_tr_t2024, eu_council_unanimity__veto_trap_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t1993, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1993, 0.35).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t1999, eu_council_unanimity__veto_trap_reading, base_extractiveness, 1999, 0.42).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t2004, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2004, 0.48).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t2009, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t2014, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2014, 0.61).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t2019, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2019, 0.65).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_be_t2024, eu_council_unanimity__veto_trap_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t1993, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1993, 0.45).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t1999, eu_council_unanimity__veto_trap_reading, suppression_requirement, 1999, 0.52).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t2004, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2004, 0.58).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t2009, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2009, 0.63).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t2014, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2014, 0.68).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t2019, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(eu_council_unanimity__veto_trap_su_t2024, eu_council_unanimity__veto_trap_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.12).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_qmv_extension_proposals).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_budget_rebate_mechanisms).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_rule_of_law_conditionality).

% DUAL FORMULATION NOTE:
% Part of the eu_council_unanimity constraint family (3 readings). This reading (veto_trap) and the sovereignty_guarantor_reading have divergent ε values (0.68 vs ~0.15) because they assess different structural realities: extraction vs. protection. The diplomatic_capital_reading sits between (~0.35). All three are linked by the shared kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_council_unanimity__veto_trap_reading, institutional, 0.15).
constraint_indexing:directionality_override(eu_council_unanimity__veto_trap_reading, organized, 0.85).
constraint_indexing:directionality_override(eu_council_unanimity__veto_trap_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
