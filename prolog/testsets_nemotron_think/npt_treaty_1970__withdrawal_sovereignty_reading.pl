% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereign Prerogative
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story models the NPT Article X withdrawal right as read
 *   by states and scholars who treat sovereign exit as a legitimate,
 *   unqualified prerogative — treaty obligations are binding only so long as
 *   the security environment permits. Under this reading, the withdrawal
 *   clause is not an emergency valve but a standing option that threshold
 *   states (Japan, South Korea, Iran, Brazil, etc.) hold as leverage: the
 *   credible threat of exit extracts security guarantees, technology
 *   transfers, or political concessions from the P5 and the broader regime.
 *   The regime stability norm becomes a victim because its credibility
 *   depends on the perception that withdrawal is exceptional; when exit
 *   becomes a routine bargaining chip, compliance incentives for NNWS erode.
 *   The P5 are dual-positioned: they benefit from the oligopoly the regime
 *   enforces (beneficiary) but also bear enforcement costs and face cascade
 *   risk when withdrawal threats materialize (payer). The coordination
 *   function is genuine — Article X enabled universal adherence by giving
 *   states an exit assurance — but the extraction layer has thickened over
 *   decades as threshold states learned to monetize the option.
 *
 * KEY AGENTS:
 *   - threshold_states: Primary beneficiary (moderate/constrained) — hold withdrawal option as leverage, extract concessions
 *   - nuclear_weapon_states_p5: Dual beneficiary/payer (institutional/constrained) — benefit from nonproliferation oligopoly, pay enforcement costs and cascade risk
 *   - non_nuclear_weapon_states_nnws: Primary victim (organized/constrained) — bear compliance costs while regime credibility erodes from withdrawal threats
 *   - regime_stability_norm: Victim (abstract) — credibility undermined when exit threat becomes normalized bargaining tool
 *   - verification_regime_iaea: Victim (institutional/constrained) — mandate and access degraded when states withdraw or threaten to
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure of option-value extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.55).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereign Prerogative").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '17472dc5-13a8-41c4-a86b-ed614a64f3ee').
narrative_ontology:cs_kernel_codification('17472dc5-13a8-41c4-a86b-ed614a64f3ee', formalized).
narrative_ontology:cs_authority_grounding('17472dc5-13a8-41c4-a86b-ed614a64f3ee', lineage).
narrative_ontology:cs_interpretation_layer_present('17472dc5-13a8-41c4-a86b-ed614a64f3ee').
narrative_ontology:cs_reading_relation('17472dc5-13a8-41c4-a86b-ed614a64f3ee', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('17472dc5-13a8-41c4-a86b-ed614a64f3ee', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('17472dc5-13a8-41c4-a86b-ed614a64f3ee', foundational, sovereign_withdrawal_right_unqualified).
narrative_ontology:cs_axiom_status(sovereign_withdrawal_right_unqualified, holdable).
narrative_ontology:cs_axiom_grounding('17472dc5-13a8-41c4-a86b-ed614a64f3ee', sovereign_withdrawal_right_unqualified, deontological).
narrative_ontology:cs_axiom('17472dc5-13a8-41c4-a86b-ed614a64f3ee', foundational, security_environment_conditions_obligation_bindingness).
narrative_ontology:cs_axiom_status(security_environment_conditions_obligation_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('17472dc5-13a8-41c4-a86b-ed614a64f3ee', security_environment_conditions_obligation_bindingness, instrumental).
narrative_ontology:cs_reference_frame('17472dc5-13a8-41c4-a86b-ed614a64f3ee', npt_as_voluntary_security_bargain).
narrative_ontology:cs_drift_state('17472dc5-13a8-41c4-a86b-ed614a64f3ee', post_2003_north_korea_withdrawal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('17472dc5-13a8-41c4-a86b-ed614a64f3ee', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_p5).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, verification_regime_iaea).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_includes_treaty_exit).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, security_environment_determines_obligation_bindingness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent nuclear capacity (Japan, South Korea, Iran, Brazil, Turkey, etc.) that hold the Article X withdrawal option as leverage. They extract security guarantees, civilian nuclear cooperation, and political concessions by credibly threatening exit. They bear costs if they actually withdraw (sanctions, isolation) but the option value is positive. Exit is constrained — withdrawal triggers immediate regime cascade and security deterioration.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, payer).

% The five recognized NWS (US, Russia, UK, France, China) that set the regime agenda, control the UNSC enforcement machinery, and benefit from the nonproliferation oligopoly. They pay enforcement costs (diplomatic, financial, military) and face existential cascade risk if withdrawal thresholds are crossed. Their exit is arbitrage-grade — they could abrogate the treaty but the regime serves their interest. They administer the constraint through the NPT review process and IAEA governance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_p5, beneficiary).

% The 180+ NNWS that comply with full-scope safeguards and foreswear nuclear weapons permanently. They bear the compliance costs (foregone deterrent, safeguards intrusiveness, technology restrictions) while the regime's credibility erodes from withdrawal threats. Their exit is constrained — they have no nuclear option and withdrawal would isolate them without strategic gain. They resist through NAM/G77 coalitions at RevCons and the TPNW.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_nnws, payer,
    organized, generational, constrained, global).

% The normative expectation that NPT membership is a durable commitment, not a conditional option. This norm is degraded each time a threshold state credibly threatens withdrawal for concessions — the norm bears the cost of extraction but collects no rents. It has no exit; it either persists or collapses. Listed as non-agent entity for structural completeness.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    moderate, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% The IAEA safeguards system that loses mandate continuity, access rights, and institutional credibility when states withdraw or threaten withdrawal. It bears the operational costs of verification gaps and political pressure from both P5 and NNWS. Its exit is constrained — it is the treaty's institutional embodiment and cannot withdraw without dissolving the regime.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, verification_regime_iaea, payer,
    institutional, generational, constrained, global).

% The analytical seat that sees the full structure: the genuine coordination function (universal nonproliferation norm), the extraction layer (threshold state option value), and the victim structure (NNWS compliance costs, regime stability degradation). Neither collects nor pays; computes the classification.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal framework preventing horizontal nuclear proliferation by offering non-nuclear states a credible exit option (Article X) that made the treaty joinable — states could adhere knowing they could withdraw if their security environment deteriorated catastrophically.
% TRANSFER_FUNCTION: Moves political leverage and material concessions (security guarantees, nuclear technology, sanctions relief) from the P5 and the broader regime to threshold states, via the credible threat of treaty withdrawal. The regime stability norm and NNWS compliance incentives are the depleted reservoir.
% ABSENT_VOICES: Future generations who inherit a weakened or collapsed nonproliferation regime; states that would have proliferated but for the regime's existence (the counterfactual compliance cohort); the global public that bears existential risk from cascade proliferation. These voices are structurally excluded — they have no seat at the NPT review conferences.
% DISAPPEARANCE_RATIONALE: If Article X withdrawal right vanished overnight (treated as non-justiciable or subject to UNSC veto), threshold states would lose their primary leverage, the P5 would face fewer withdrawal crises but also fewer adhesion incentives for new members, NNWS would gain regime credibility but lose the exit assurance that made the treaty universal. The nuclear order would reorganize around security guarantees and alliance structures rather than treaty law.
% FOUNDING_PROBLEM: How to achieve universal adherence to a nonproliferation treaty when states feared permanent foreclosure of the nuclear option in a deteriorating security environment. Article X was the entry condition for states that would not join a suicide pact.
% FOUNDING_PROBLEM_CORROBORATION: The negotiation record (1965-1968) corroborates that Article X was demanded by non-nuclear states (Ireland, Sweden, Brazil, Mexico) as a sovereignty safeguard. The P5 accepted it reluctantly. Contemporary threshold states attest the problem is live (security environment unchanged); NNWS and disarmament advocates attest the problem is solved (regime is universal, withdrawal threat is now abuse). No consensus outside the benefiting parties.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the withdrawal option has become a tradeable asset: threshold states extract tangible concessions (security assurances, civilian nuclear cooperation, sanctions relief) by credibly threatening exit. The coordination function (nonproliferation regime stability) is real but shrinking — theater_ratio (0.42) reflects that a growing share of diplomatic activity manages withdrawal threats rather than prevents proliferation. Suppression (0.55) is moderate: the treaty's enforcement machinery (IAEA safeguards, UNSC resolutions, export controls) is real but calibrated to horizontal proliferation, not to the meta-threat of withdrawal itself. Accessibility_collapse (0.62) is elevated because the withdrawal option collapses the alternative of 'permanent non-nuclear status' into a conditional commitment. Resistance (0.71) is high: NNWS coalitions (NAM, G77) actively contest the asymmetry at every RevCon, and the TPNW emerged partly as counter-resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the threshold state seat, the constraint is a rope: a coordination mechanism (security through nonproliferation) with a fair exit option they negotiated for. From the NNWS seat, it is a snare: they comply permanently while threshold states defect conditionally. From the P5 seat, it is a tangled_rope: they coordinate the oligopoly but must continuously pay to suppress withdrawal cascades. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) represents the analytical seat's synthesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states sit at d ≈ 0.2 (beneficiary end): they hold the option, collect concessions, and face constrained but real exit (they could withdraw). The P5 sit at d ≈ 0.45 (moderate): they benefit from the regime but bear enforcement costs and existential cascade risk. NNWS sit at d ≈ 0.75 (target end): they pay compliance costs, cannot credibly threaten exit (no nuclear option), and suffer regime erosion. The regime_stability_norm and IAEA are structural victims with no exit — they absorb the degradation. The analytical observer sits at d = 0.0 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear proliferation while allowing peaceful use) remains live but the withdrawal_sovereignty reading treats Article VI disarmament as contingent, not reciprocal. This reading resolves mandatrophy by declaring the security environment — not the treaty text — as the binding condition. The arrangement persists because the P5 prefer a flawed regime to no regime, and threshold states prefer the option value to actual withdrawal (which triggers sanctions). No party benefits enough to reform Article X; no party is hurt enough to abandon the treaty. This is the piton dynamic within the tangled_rope: the extraction layer (option value) has atrophied the coordination function but the constraint persists through institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the NPT a single constraint with competing readings, or are the oligopoly_enforcement, reciprocal_disarmament, and withdrawal_sovereignty readings structurally distinct constraints linked by the treaty text?',
    'Decompose each reading into its own constraint story with independent ε, stakeholders, and classification; compare engine outputs. If ε values differ substantially (as BGS spectral vs eigenvector), the kernel label conflates multiple constraints.',
    'If distinct constraints, the withdrawal_sovereignty reading''s ε ≈ 0.68 (tangled_rope) is not a measurement variant of the oligopoly reading''s ε ≈ 0.15 (mountain/rope) — they are different objects. The corpus must track them separately with network.affects_constraints links.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the NPT kernel decomposes into multiple ε-invariant constraints').

omega_variable(
    withdrawal_as_coordination_vs_extraction,
    'Does Article X''s withdrawal right serve a genuine coordination function (allowing states to join a risky regime with an exit option) or is it primarily an extraction mechanism (threshold states leveraging exit threat for concessions)?',
    'Historical analysis of NPT negotiation record: was Article X demanded by non-nuclear states as entry condition, or inserted by nuclear states as pressure valve? Counterfactual: would fewer states have joined without Article X?',
    'If genuine coordination function, tangled_rope classification holds (coordination + asymmetric extraction). If pure cover for extraction, reclassifies to snare. The coordination function is the gate for tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_as_coordination_vs_extraction, empirical, 'Whether Article X withdrawal right has a bona fide coordination function').

omega_variable(
    regime_stability_as_victim,
    'Is ''regime stability norm'' a legitimate victim stakeholder, or is it a reified abstraction that collects no rents and bears no costs?',
    'Trace concrete harms when withdrawal threats materialize: NNWS compliance erosion, verification gaps, cascade proliferation risk. If harms are diffuse and unattributable, regime stability may be a vindicated proposition, not a victim.',
    'If regime stability is a proposition not a payer, victims list shrinks to NNWS and IAEA — still sufficient for snare/tangled_rope gates but changes the extraction map.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_stability_as_victim, conceptual, 'Whether the regime stability norm is a structural victim or a vindicated proposition').

omega_variable(
    suppression_mechanism_treaty_exit,
    'Is the suppression of alternatives (nuclear acquisition, security guarantees) structural (treaty verification, UNSC enforcement, export controls) or internalized (NNWS self-restraint from normative internalization)?',
    'Post-withdrawal trajectory analysis: if a state withdraws and proliferates, does suppression persist via external barriers (sanctions, interdiction) or collapse? If external barriers hold, suppression is structural; if they evaporate, internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint operates through normative capture as well as enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_treaty_exit, empirical, 'Structural vs internalized suppression in NPT compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_ws_tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt_ws_tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(npt_ws_tr_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(npt_ws_tr_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2003, 0.37).
narrative_ontology:measurement(npt_ws_tr_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(npt_ws_tr_t2023, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2023, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_ws_be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(npt_ws_be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(npt_ws_be_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement(npt_ws_be_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2003, 0.58).
narrative_ontology:measurement(npt_ws_be_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(npt_ws_be_t2023, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_ws_su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(npt_ws_su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(npt_ws_su_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(npt_ws_su_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2003, 0.52).
narrative_ontology:measurement(npt_ws_su_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2015, 0.54).
narrative_ontology:measurement(npt_ws_su_t2023, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__withdrawal_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% NPT kernel family (3 stories): oligopoly_enforcement_reading (low ε, coordination dominant) → reciprocal_disarmament_reading (moderate ε, reciprocity contested) → withdrawal_sovereignty_reading (high ε, extraction dominant). Upstream readings are cited as legitimacy cover for downstream extraction; this reading's sovereignty framing is enabled by the oligopoly reading's refusal to enforce Article VI reciprocity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, moderate, 0.2).
constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, institutional, 0.45).
constraint_indexing:directionality_override(npt_treaty_1970__withdrawal_sovereignty_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
