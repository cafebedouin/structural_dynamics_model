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
 *   human_readable: EU Council Unanimity as Veto Trap
 *   domain: institutional_design/international_relations
 *
 * SUMMARY:
 *   The EU Council's unanimity requirement—every member state must consent to
 *   binding decisions—was designed as a sovereignty safeguard in the 1950s
 *   when the integration project was contested. By the 1990s-2000s, consensus
 *   on the desirability of integration had largely emerged, but unanimity
 *   persisted as a structural feature. Under this reading (the
 *   veto_trap_reading), unanimity operates as a minoritarian extraction
 *   mechanism: a single member state can credibly threaten to block policies
 *   the coalition majority wants, forcing the majority to buy off the veto
 *   holder through side-payments, opt-outs, budgetary reallocations, or
 *   policy exemptions. The constraint benefits the blocking minority and
 *   harms the coalition majority. This reading sits in tension with two
 *   siblings: the sovereignty_guarantor_reading (which justifies unanimity as
 *   protecting national sovereignty against majoritarian coercion—a live
 *   justification when the founding problem persists) and the
 *   diplomatic_capital_reading (which frames unanimity as forcing iterative
 *   negotiation that strengthens legitimacy—a coordination story). This JSON
 *   instantiates only the veto_trap_reading, which treats veto use as
 *   extraction and high ε as structural rather than pathological.
 *
 * KEY AGENTS:
 *   - blocking_minority_state: holds credible veto power; uses it to extract concessions from the majority
 *   - coalition_majority: wants to pass policy; must systematically overpay the veto holder to avoid blockage
 *   - eu_commission: mediates negotiations; absorbs transaction costs; proposes compromises
 *   - small_or_economically_weak_states: possess formal veto but lack credible threat power; pay extraction cost without compensation
 *   - eu_parliament: represents supranational democratic preference; structurally excluded from veto negotiations
 *   - international_observers: witness whether unanimity is a sovereignty safeguard or a structural trap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__veto_trap_reading, 0.72).
domain_priors:suppression_score(eu_council_unanimity__veto_trap_reading, 0.68).
domain_priors:theater_ratio(eu_council_unanimity__veto_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(eu_council_unanimity__veto_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__veto_trap_reading, snare).
narrative_ontology:human_readable(eu_council_unanimity__veto_trap_reading, "EU Council Unanimity as Veto Trap").
narrative_ontology:topic_domain(eu_council_unanimity__veto_trap_reading, "institutional_design/international_relations").

domain_priors:requires_active_enforcement(eu_council_unanimity__veto_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__veto_trap_reading, 'babdc764-0efa-483c-bdcd-8c2dead6f8b6').
narrative_ontology:cs_kernel_codification('babdc764-0efa-483c-bdcd-8c2dead6f8b6', formalized).
narrative_ontology:cs_authority_grounding('babdc764-0efa-483c-bdcd-8c2dead6f8b6', lineage).
narrative_ontology:cs_interpretation_layer_present('babdc764-0efa-483c-bdcd-8c2dead6f8b6').
narrative_ontology:cs_reading_relation('babdc764-0efa-483c-bdcd-8c2dead6f8b6', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('babdc764-0efa-483c-bdcd-8c2dead6f8b6', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('babdc764-0efa-483c-bdcd-8c2dead6f8b6', foundational, veto_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(veto_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('babdc764-0efa-483c-bdcd-8c2dead6f8b6', veto_as_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('babdc764-0efa-483c-bdcd-8c2dead6f8b6', foundational, founding_problem_obsolescence).
narrative_ontology:cs_axiom_status(founding_problem_obsolescence, holdable).
narrative_ontology:cs_axiom_grounding('babdc764-0efa-483c-bdcd-8c2dead6f8b6', founding_problem_obsolescence, empirically_contingent).
narrative_ontology:cs_reference_frame('babdc764-0efa-483c-bdcd-8c2dead6f8b6', consensus_building_legitimacy).
narrative_ontology:cs_drift_state('babdc764-0efa-483c-bdcd-8c2dead6f8b6', veto_weaponization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('babdc764-0efa-483c-bdcd-8c2dead6f8b6', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__veto_trap_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, coalition_majority).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_policy_integration_agenda).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, eu_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__veto_trap_reading, small_or_economically_weak_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds credible veto power over EU Council decisions. Uses the veto or threat of veto to extract concessions, opt-outs, budgetary side-payments, or policy carve-outs from the coalition majority. Their material interest may be parochial (domestic budget protection, sectoral exemption) or ideological (blocking direction of integration). The veto is costless to initiate and carries no procedural penalty; exercising it is purely a negotiating tactic.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, blocking_minority_state, agenda_setter,
    institutional, generational, constrained, continental).

% Wants to pass a policy that would advance the broader EU agenda (fiscal union, labor mobility, environmental standards). Must negotiate with every member state individually to avoid blockage. Systematically overpays the blocking minority through side-deals, opt-outs (like UK's rebate, Poland's judicial independence carve-out), or budget reallocations that the majority would not grant if choice were unrestricted.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, coalition_majority, payer,
    institutional, generational, constrained, continental).

% Proposes harmonizing policies to member states but cannot unilaterally enact them. Must satisfy all unanimity requirements, which means accommodating veto holders' demands. Plays the role of mediator and broker, absorbing the transaction cost of sequential negotiation and often proposing watered-down compromises that weaker member states can live with.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_commission, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__veto_trap_reading, eu_commission, payer).

% Possess formal veto power equal to large states, but lack credible threat power because the majority can more easily isolate them. They have fewer coalition options, weaker exit (EU membership costs less to lose for large states), and less bargaining material to withhold. They pay the extraction cost without the blocking power to demand compensation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, small_or_economically_weak_states, payer,
    moderate, generational, constrained, continental).

% Represents supranational democratic preference across all EU citizens. Cannot override Council unanimity and has no formal role in blocking decisions. Would oppose many of the side-deals made to secure veto-holder consent, but is structurally excluded from the negotiation where those deals form. Their voice, representing majority citizen preference across the EU, is written out of the constraint.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, eu_parliament, excluded,
    organized, generational, constrained, continental).

% The EU's institutional commitment to deepening union through harmonization of standards, labor mobility, environmental regulation, and fiscal coordination. Unanimity blocks it repeatedly, forcing compromises that slow integration and embed exceptions that undermine the policy's integrity. The agenda is not an agent but a victim of the constraint's operation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, supranational_integration_agenda, payer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(eu_council_unanimity__veto_trap_reading, supranational_integration_agenda).

% Scholars, analysts, and external states watching whether EU institutional design enables effective collective action or traps it in minoritarian extraction. They observe that unanimity was intended as a sovereignty safeguard but operates as a structural vulnerability once members lose ideological consensus and pursue narrow national advantage.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__veto_trap_reading, international_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__veto_trap_reading, blocking_minority_state).
narrative_ontology:fixing_cost_class(eu_council_unanimity__veto_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures no member state is forced into a collective commitment against its explicit consent—a safeguard against majoritarian coercion of peripheral or ideologically distinct members into policies they oppose.
% TRANSFER_FUNCTION: Transfers policy concessions, budgetary side-payments, exemptions (opt-outs), and ideological victories from the coalition majority to the blocking minority state, mediated through the threat of veto.
% ABSENT_VOICES: EU Parliament (supranational democratic preference across all citizens) and non-blocking small member states (who lack credible threat power and cannot extract concessions) are structurally excluded from the negotiation where veto-triggered deals form. Their opposition would matter if they were seated, but the constraint is designed so they are not.
% DISAPPEARANCE_RATIONALE: If unanimity vanished overnight and the EU shifted to qualified majority voting (or supermajority voting), the blocking minority would lose its extraction tool; policies the majority prefers would pass without side-deals; budgetary reallocations negotiated to buy off vetoes would cease; opt-outs that privileged particular states would face pressure to harmonize. The entire institutional equilibrium would reorganize around majority preference, not minority veto.
% FOUNDING_PROBLEM: Early EU (1950s-1980s) required unanimous consent to protect smaller or ideologically distinct member states from being coerced into a federal project they had not explicitly endorsed. Unanimity was a check on majoritarian overreach and a guarantee of national sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: By the 1990s-2000s, member states largely converged on the integration project's desirability (even if disagreeing on speed and direction). Unanimity became less a protection against coerced membership and more a tool for individual members to extract rents from the majority's desire to deepen union. Scholars of EU institutional design (Tsebelis, Garrett, Moravcsik) and policy analysts document that veto use shifted from existential sovereignty defense to parochial bargaining. The blocking minority no longer claims unanimity is a sovereignty safeguard; they assert their right to extract concessions as compensation for moving toward policies they would prefer not to support—a different rationale, and one that corroborates the founding problem is dead.
narrative_ontology:disappearance_verdict(eu_council_unanimity__veto_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__veto_trap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__veto_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(eu_council_unanimity__veto_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__veto_trap_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.72 at interval end) because the constraint systematically transfers value from the majority to the blocking minority through credible threats: the veto is costless to initiate, carries no procedural penalty, and creates a negotiation dynamic where the majority has more to lose (from blockage) than the blocking minority has to lose (from exercising it). Suppression is moderately high (0.68) because the constraint forces the majority into concessions they would not grant if the choice set were unrestricted—the majority's policy preference is suppressed by the institutional requirement to satisfy the minority. Theater rises from 0.28 to 0.42 because as the constraint matures, more of the 'consensus building' rhetoric (the diplomatic_capital justification) becomes performative—the real negotiation is increasingly transparent as extraction, not deliberation. The measurement series captures a historical trajectory: extractiveness rises steeply to t=20 (where vetoes proliferate as members learn the tactic), then plateaus (the constraint's extractive capacity is exhausted—all low-hanging concessions have been extracted). Suppression follows a similar curve. Theater rises throughout because the EU increasingly frames the process as 'consensus building' while the underlying dynamics are nakedly extractive. At t=30, the constraint has reached equilibrium: the blocking minority has extracted what it can, the majority has accepted the structural cost, and the process is now theatrical—formal negotiation masking settled extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the blocking minority's seat, unanimity is a legitimate power to protect national interest through credible negotiating leverage—they do not experience themselves as extractive, but as exercising hard-won institutional power. From the coalition majority's seat, unanimity is an institutional trap that forces overpayment for policies the majority prefers; they experience the extraction acutely. From the EU Parliament's excluded seat, the constraint is a democratic deficit—supranational preference is being traded away in backroom deals to satisfy individual states. The engine should compute different types for these seats: the blocking minority may see rope (coordination through negotiation), the majority sees snare (forced extraction), and the Parliament sees a pure power dynamic where it has no standing. The CLAIM here is snare (this reading holds that extraction is the structural mechanism); the metrics support that claim, but only from the majority seat, not uniformly across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The blocking minority state is the beneficiary (collects concessions, opt-outs, budgetary transfers, ideological victories—d approaches 0.0). The coalition majority is the victim (pays the concessions, accepts the delays, tolerates the exceptions—d approaches 1.0). The EU Parliament is the structural loser: it is excluded from the negotiation where the deals form, so it has no directionality toward the constraint in the normal sense, but its preference is systematically suppressed in favor of the blocking minority's concession-extraction. Small weak states have formal veto power (d should be low, like large blocking states) but lack credible threat power; their exit options are more constrained (leaving the EU costs them more than it costs large states). This creates an asymmetry within the formal peers: large states exploit veto power as an extraction tool; small states have the same formal power but cannot use it effectively. No override is needed for the large blocking minority (the structural derivation captures their benefit correctly). Small weak states are a different structural position and might warrant an override if the story's focus narrows to their predicament—but this broad story treats them as constrained payers without special override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting member states from coercion into an unwanted integration project) is dead: member states now want deeper integration (or at least do not credibly object to it on sovereignty grounds). Yet unanimity persists, and its primary function has shifted from sovereignty protection to extractive rent-seeking. The constraint exhibits classic mandatrophy: the original mandate has outlived its function, but the institutional structure persists because the beneficiaries (those who can use the veto to extract) have no incentive to change it. The solution (moving to qualified majority or supermajority voting) is blocked by the beneficiaries themselves—the ones with veto power have no reason to surrender it. The constraint is thus a candidate for the mandatrophy_resolved flag if the story author believes the shift in mandate is irreversible; here, it is not flagged because the mandate dispute is live in the sibling readings (the sovereignty_guarantor_reading still claims the founding problem is live). The mandatrophy dynamic is the diagnostic fact that distinguishes this reading from its siblings: this reading says the founding problem is dead and the constraint is now pure extraction; the sovereignty_guarantor reading says the founding problem is live and unanimity is a legitimate safeguard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_liveness,
    'Is the EU founding problem (protection of member states from majoritarian coercion into unwanted integration) still live, or is it dead?',
    'Survey member states on whether they experience unanimity as a sovereignty safeguard (problem live) or as a constraint they would gladly lift (problem dead). Analyze voting patterns: if vetoes cluster around sovereignty concerns, the problem is live; if vetoes cluster around sectoral/budgetary narrow interests, the problem is dead.',
    'If the founding problem is live, the sovereignty_guarantor_reading is correct and unanimity is a legitimate safeguard (low ε). If it is dead, the veto_trap_reading is correct and unanimity is pure extractive structure (high ε). This omega is the core of the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Whether unanimity is a sovereignty safeguard (founding problem live) or minoritarian extraction (founding problem dead).').

omega_variable(
    veto_credibility_vs_threat_value,
    'Do blocking minorities actually extract concessions, or do they exercise veto as a last resort when consensus has genuinely failed?',
    'Game-theoretic analysis of EU negotiation data: if veto threats are systematically used as first-mover negotiating tactics before consensus efforts are exhausted, extraction is the mechanism; if vetoes occur only when negotiation reaches an impasse, the mechanism is consensus-building with veto as a backstop.',
    'If veto is a negotiating tactic (extraction), ε remains high and snare is correct. If veto is a true last resort (consensus mechanism), ε drops substantially and rope or diplomatic_capital_reading becomes more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_credibility_vs_threat_value, empirical, 'Whether the veto is primarily a negotiating extraction tool or a consensus-building backstop.').

omega_variable(
    side_deal_efficiency_loss,
    'Do the side-payments, opt-outs, and budgetary reallocations required to satisfy veto holders create a less efficient or less coherent EU policy regime than qualified majority voting would?',
    'Comparative institutional analysis: model EU policy outcomes under unanimity vs. qualified majority voting in a counterfactual framework, controlling for member state preferences. Measure policy coherence (number of exceptions, exemptions, carve-outs), transaction costs (negotiation time and resources), and distributional efficiency (whether resources flow to the most efficient users or to the most powerful veto holders).',
    'If unanimity produces substantially worse outcomes (more incoherent policy, higher transaction costs, regressive distribution), it strengthens the case that extraction is the dominant mechanism and snare is correct. If outcomes are roughly equivalent, unanimity''s coordination benefit (forcing consensus) may justify the transaction cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(side_deal_efficiency_loss, empirical, 'Whether unanimity''s transaction costs and policy incoherence are proportional to the coordination benefits it produces.').

omega_variable(
    alternative_readings_foreclosure,
    'Can all three readings (veto_trap, sovereignty_guarantor, diplomatic_capital) coexist in one party''s framework, or does this reading''s core premise foreclose one or both siblings?',
    'Examine whether a single actor (e.g., a EU member state) can consistently hold all three frames, or whether accepting one reading logically requires rejecting another. If a state claims unanimity is a sovereignty safeguard AND that veto threats are extractive AND that the process builds legitimacy, are those claims coherent or in tension?',
    'If the readings are logically incompatible (e.g., claiming unanimity is a legitimate safeguard while also treating it as extractive rentseeking), then at least one reading forecloses another. If they are compatible (e.g., unanimity protects sovereignty AND is used for extraction AND does build some legitimacy), they coexist. The kernel''s logical structure determines the reading_relations values in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether the veto_trap reading and its siblings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__veto_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__veto_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(eu_c_tr_t5, eu_council_unanimity__veto_trap_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__veto_trap_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(eu_c_tr_t15, eu_council_unanimity__veto_trap_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__veto_trap_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(eu_c_tr_t25, eu_council_unanimity__veto_trap_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__veto_trap_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__veto_trap_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(eu_c_be_t5, eu_council_unanimity__veto_trap_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__veto_trap_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(eu_c_be_t15, eu_council_unanimity__veto_trap_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__veto_trap_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(eu_c_be_t25, eu_council_unanimity__veto_trap_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__veto_trap_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__veto_trap_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(eu_c_su_t5, eu_council_unanimity__veto_trap_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__veto_trap_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(eu_c_su_t15, eu_council_unanimity__veto_trap_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__veto_trap_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(eu_c_su_t25, eu_council_unanimity__veto_trap_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__veto_trap_reading, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__veto_trap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(eu_council_unanimity__veto_trap_reading, 0.18).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, eu_council_unanimity__diplomatic_capital_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__veto_trap_reading, qualified_majority_voting_eu_council).

% DUAL FORMULATION NOTE:
% This constraint is one reading (veto_trap_reading) of the kernel eu_council_unanimity. The sibling readings (sovereignty_guarantor_reading, diplomatic_capital_reading) are separate constraint stories with different ε values, beneficiary/victim structures, and claimed types. All three readings share the same institutional kernel (the unanimity rule) but instantiate it structurally and normatively differently. The network links capture the constraint family structure: each reading influences and contests the others. Decomposition is necessary because the observable (whether the rule operates as sovereignty protection, coordination mechanism, or extraction tool) changes ε and type classification, violating ε-invariance if forced into one story. See commentary.kernel_context for the kernel contest and how the readings relate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
