% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_reachability_boundary__dropping_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Nuclear Deterrence as Coordination Equilibrium (Dropping-Probability Reading)
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   Since 1945, the probability of a war among great powers rising to
 *   unrestrained, civilization-scale exchange has dropped sharply — no
 *   nuclear weapon has been used in anger since Nagasaki, and multiple
 *   historical crises (Cuban Missile Crisis, 1983 Able Archer, various
 *   false-alarm incidents) that could plausibly have escalated to total war
 *   did not. This reading holds that the drop reflects an actively maintained
 *   coordination equilibrium among nuclear powers (mutual assured
 *   destruction, extended deterrence, arms control regimes, crisis-management
 *   protocols) rather than a permanent structural closure of the possibility.
 *   The equilibrium requires continuous investment — force modernization,
 *   doctrine maintenance, alliance credibility signaling,
 *   crisis-communication infrastructure — and its coordination function is
 *   real (genuine mutual restraint solving a genuine security dilemma) but
 *   rides alongside asymmetric extraction: nuclear-armed elites and allied
 *   security planners gain strategic leverage and industrial revenue from
 *   maintaining the equilibrium, while global populations bear the residual
 *   catastrophic tail-risk with no voice in doctrine.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_leadership: Primary agenda-setter and beneficiary (institutional/arbitrage) — sets doctrine, gains leverage
 *   - populations_under_nuclear_threat: Primary victim (powerless/trapped) — bears existential tail-risk with no voice
 *   - defense_industrial_complex: Secondary beneficiary (powerful/mobile) — profits from perceived fragility requiring renewal
 *   - arms_control_negotiators_and_analysts: Analytical observer (analytical/analytical) — studies equilibrium stability without controlling it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.62).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Nuclear Deterrence as Coordination Equilibrium (Dropping-Probability Reading)").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '995d4898-cd7a-41b4-8ba1-72e8017ab29b').
narrative_ontology:cs_kernel_codification('995d4898-cd7a-41b4-8ba1-72e8017ab29b', distributed).
narrative_ontology:cs_authority_grounding('995d4898-cd7a-41b4-8ba1-72e8017ab29b', distributed).
narrative_ontology:cs_reading_relation('995d4898-cd7a-41b4-8ba1-72e8017ab29b', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('995d4898-cd7a-41b4-8ba1-72e8017ab29b', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('995d4898-cd7a-41b4-8ba1-72e8017ab29b', foundational, deterrence_is_maintained_equilibrium_not_structural_closure).
narrative_ontology:cs_axiom_status(deterrence_is_maintained_equilibrium_not_structural_closure, holdable).
narrative_ontology:cs_axiom_grounding('995d4898-cd7a-41b4-8ba1-72e8017ab29b', deterrence_is_maintained_equilibrium_not_structural_closure, empirically_contingent).
narrative_ontology:cs_axiom('995d4898-cd7a-41b4-8ba1-72e8017ab29b', foundational, coordination_function_and_asymmetric_extraction_coexist_in_same_structure).
narrative_ontology:cs_axiom_status(coordination_function_and_asymmetric_extraction_coexist_in_same_structure, holdable).
narrative_ontology:cs_axiom_grounding('995d4898-cd7a-41b4-8ba1-72e8017ab29b', coordination_function_and_asymmetric_extraction_coexist_in_same_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('995d4898-cd7a-41b4-8ba1-72e8017ab29b', post_1945_mutual_assured_destruction_baseline).
narrative_ontology:cs_drift_state('995d4898-cd7a-41b4-8ba1-72e8017ab29b', post_cold_war_multipolar_proliferation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('995d4898-cd7a-41b4-8ba1-72e8017ab29b', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states_leadership).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, alliance_bloc_security_planners).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_industrial_complex).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states_excluded_from_deterrence_bargaining).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, future_generations_bearing_residual_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, alliance_bloc_security_planners).
narrative_ontology:constraint_vindicates(total_war_reachability_boundary__dropping_reading, mutual_assured_destruction_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the deterrence architecture: force posture, doctrine, alert levels, and escalation signaling. Derives geopolitical leverage, alliance leadership, and domestic legitimacy from possessing and credibly threatening to use these weapons. Can adjust the coordination game's terms unilaterally in ways non-nuclear actors cannot.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states_leadership, beneficiary).

% Shelter under extended deterrence umbrellas, gaining security without independently acquiring nuclear arsenals. Pay through basing commitments, alliance dues, and subordination of independent security policy to the umbrella-holder's strategic calculus; cannot exit the arrangement without incurring the cost of independent deterrence or accepting exposure.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, alliance_bloc_security_planners, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, alliance_bloc_security_planners, payer).

% Manufactures, modernizes, and maintains delivery systems and warheads; profits scale with perceived instability of the deterrence equilibrium since credibility maintenance requires continuous investment. Has strong incentive to frame the equilibrium as fragile and requiring perpetual renewal.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_industrial_complex, beneficiary,
    powerful, biographical, mobile, national).

% Bear the tail-risk of civilization-ending failure with zero input into doctrine, targeting, or alert posture. Cannot opt out of being targeted by virtue of geography or alliance membership; their consent was never sought and cannot meaningfully be withdrawn. Absorb the low-probability-but-catastrophic cost that makes the equilibrium 'coordinated' for elites and existential for everyone else.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, biographical, trapped, global).

% Live within the security architecture the nuclear powers construct but have no seat in setting its terms, no veto over escalation doctrine, and limited recourse beyond diplomatic protest or attempts at proliferation (itself destabilizing). Their objections to first-use ambiguity or modernization programs are heard, if at all, only through multilateral forums the nuclear powers can stall.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_states_excluded_from_deterrence_bargaining, excluded,
    moderate, generational, constrained, national).

% Inherit whatever residual probability of catastrophic failure the current equilibrium carries forward, plus any material and doctrinal path-dependencies (stockpiles, delivery infrastructure, targeting doctrine) locked in by present choices, without any voice in those choices.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, future_generations_bearing_residual_risk, payer,
    powerless, civilizational, trapped, global).

% Study the equilibrium's stability, negotiate reduction treaties, and monitor compliance. Can influence but not unilaterally alter the coordination game; their assessments feed into but do not control doctrine.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_negotiators_and_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states_leadership).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rival nuclear powers each refrain from first use and from destabilizing force postures because retaliation is assured; this mutual restraint is a genuine, non-trivial solution to the problem that any single actor's unrestrained escalation would trigger catastrophic reciprocal destruction.
% TRANSFER_FUNCTION: Moves existential tail-risk from the decision-making elites who set doctrine and gain strategic leverage from credibility onto global populations, non-nuclear states, and future generations who bear the risk but have no say in doctrine, alert posture, or modernization decisions.
% ABSENT_VOICES: Global civilian populations, non-nuclear states, and future generations are structurally absent from doctrine-setting; they would object to first-use ambiguity, hair-trigger alert postures, and modernization programs that raise stakes, but are represented only indirectly through multilateral forums the nuclear powers can slow-walk or ignore.
% DISAPPEARANCE_RATIONALE: If the deterrence equilibrium collapsed overnight (arsenals eliminated or credibility evaporated), the entire architecture of alliance commitments, extended-deterrence guarantees, force posture, and strategic doctrine would have to be rebuilt from different premises; conversely, if it merely ceased to be enforced through active signaling and modernization, the probability of destabilizing miscalculation would shift measurably. The arrangement is not a background fact of nature — actors' postures are actively organized around maintaining it.
% FOUNDING_PROBLEM: The founding problem was preventing great-power war among actors capable of civilization-ending destruction, in a security environment where each side's rational fear of the other's first strike could itself trigger preemptive escalation (the security dilemma raised to existential stakes).
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states' own strategic doctrine documents assert the problem remains live (great-power competition, multipolar nuclear proliferation). Independent arms-control scholars, historians of near-miss incidents (multiple documented false-alarm and near-launch events during the Cold War and after), and non-nuclear-state diplomats corroborate that the coordination function is real but argue the current force postures and modernization programs exceed what stable deterrence requires — that some of the doctrine's persistence now serves institutional and industrial interests rather than the founding problem alone.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_reachability_boundary__dropping_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_reachability_boundary__dropping_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real coordination function (mutual restraint genuinely reduces the probability of catastrophic exchange relative to no coordination at all) combined with a substantial transfer: strategic leverage and industrial revenue accrue to those setting doctrine while existential risk is borne by populations with zero doctrinal input. Suppression (0.62) reflects that alternative security architectures (e.g., full disarmament, alternative collective security regimes) are actively foreclosed by the incumbent powers' doctrine and by the structural difficulty of verified multilateral disarmament — not because no coordination alternative could exist, but because the current equilibrium's maintainers have strong incentive to suppress serious consideration of exit paths. Theater ratio (0.28) is moderate: much of the doctrinal and signaling activity (deterrence patrols, force posture announcements) serves genuine coordination function, but a rising share (0.10 to 0.28 across the interval) reflects modernization programs and doctrine reviews that primarily serve institutional and industrial continuity rather than marginal deterrence value. Accessibility collapse (0.35) is moderate-low, deliberately below what a mountain claim would require — alternative security arrangements (regional collective security, verified disarmament regimes, no-first-use treaties) remain conceptually and diplomatically available, they are merely politically suppressed, which is the key structural fact distinguishing this reading from the contraction_reading's implicit claim of near-total foreclosure.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon states' leadership seat, the equilibrium is genuine, hard-won coordination requiring continuous vigilant maintenance — a rope they are responsibly tending. From the seat of populations under nuclear threat, the same structure is an imposed risk-bearing arrangement they never consented to and cannot exit — extraction of their safety margin for others' strategic leverage. The engine computes these divergent seat-level readings from the same structural data; this story does not adjudicate which seat is 'right' — it authors the structural facts (who sets terms, who bears risk, what exit looks like from each position) that generate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states' leadership sits near the beneficiary end: they set the terms of the coordination game, extract strategic leverage from credibility, and possess exit options (arbitrage — the capacity to alter the equilibrium's terms unilaterally) unavailable to any other seat. Alliance bloc planners are near-symmetric: real security benefit from the umbrella, real cost in subordinated autonomy and basing commitments — a genuine coordination beneficiary who also pays. The defense industrial complex benefits without bearing risk-exposure proportional to its gain, giving it a directionality skewed toward beneficiary despite formally 'private' status. Populations under nuclear threat and future generations sit at the full-target end: high exposure, zero decision input, trapped exit (geography and citizenship determine target status; no consent was sought). Non-nuclear excluded states occupy an intermediate position — moderate power, constrained exit, structurally shut out of the bargaining table that sets the terms they must live under.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than either a mountain (contraction_reading's implicit frame) or pure snare prevents two symmetric mislabeling errors. Calling it a mountain would mistake an actively maintained, resource-intensive, politically contestable equilibrium for a permanent structural fact — obscuring that continued suppression of alternative security architectures, and continued investment in credibility maintenance, are choices being made, not physics. Calling it a pure snare would deny the real coordination achievement: the probability of great-power total war genuinely has fallen relative to a world without any mutual-restraint mechanism, and dismantling the equilibrium without a replacement coordination structure would not obviously improve outcomes for the very populations who bear its costs. The tangled_rope classification holds both facts: a real coordination function operating through the same structure that extracts asymmetric benefit and imposes asymmetric risk — which is exactly the diagnostic tangled_rope exists to make precise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_contraction_distinction,
    'Has the space of physically/technologically feasible total war outcomes actually contracted (contraction_reading), or has only the probability of reaching a still-feasible outcome dropped due to maintained coordination (this reading)?',
    'Analysis of whether emerging technologies (missile defense, hypersonics, AI-assisted first-strike calculus, cyber-enabled command-and-control disruption) could restore first-strike-advantage conditions that the current equilibrium''s stability depends on being absent. If such restoration is technologically plausible, the ''contraction'' framing is wrong and the ''dropping probability, stable reachability'' framing is right.',
    'If reachability has genuinely contracted (a technological/structural change removed winnable total war from the feasible set), this constraint over-attributes causal weight to active coordination maintenance where a mountain-like structural closure is doing more of the work. If reachability is unchanged and only probability has dropped through maintained coordination, this reading is correct and the contraction_reading over-claims permanence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_contraction_distinction, empirical, 'Whether the underlying feasible-outcome space has structurally contracted or only the probability of reaching it has dropped.').

omega_variable(
    equilibrium_versus_atrophy_distinction,
    'Is the current low-probability state an actively maintained coordination equilibrium (this reading) or a degraded/atrophied capability state that persists mostly through institutional inertia and could reverse sharply with technological change (contingent_reachability_reading, piton framing)?',
    'Examine whether force modernization, doctrine review, and crisis-management infrastructure investment are tracking genuine emerging threats (equilibrium maintenance) or are primarily self-perpetuating institutional/industrial activity disconnected from marginal deterrence need (atrophy with theatrical maintenance).',
    'If investment is genuinely threat-tracking, this reading''s tangled_rope classification (real coordination + real extraction) holds. If investment is substantially disconnected from threat and mostly self-perpetuating, the piton reading gains support and effective extraction is overstated here relative to theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equilibrium_versus_atrophy_distinction, conceptual, 'Whether the equilibrium is actively maintained coordination or inertial atrophy with theatrical upkeep.').

omega_variable(
    counterfactual_probability_baseline,
    'What would the probability of great-power total war actually be in the absence of the specific nuclear-deterrence coordination mechanisms (MAD, extended deterrence, arms control), holding other factors (economic interdependence, international institutions, nuclear taboo norms) constant?',
    'Comparative historical and game-theoretic modeling isolating the marginal contribution of nuclear deterrence specifically versus other war-inhibiting factors (economic interdependence, democratic peace effects, international institutions, the normative ''nuclear taboo'').',
    'If deterrence''s marginal contribution to reduced total-war probability is smaller than nuclear-weapon-states'' doctrine claims, the coordination-function justification is weaker than authored here and the extraction-to-coordination ratio should be revised upward (more snare-like); if larger, the tangled_rope''s coordination component is more robustly established.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_probability_baseline, empirical, 'Isolating deterrence''s true marginal causal contribution to reduced total-war probability versus confounding factors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_reachability_boundary__dropping_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_reachability_boundary__dropping_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_reachability_boundary__dropping_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(tota_tr_t1991, total_war_reachability_boundary__dropping_reading, theater_ratio, 1991, 0.3).
narrative_ontology:measurement(tota_tr_t2003, total_war_reachability_boundary__dropping_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(tota_tr_t2014, total_war_reachability_boundary__dropping_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__dropping_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.72).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1975, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement(tota_be_t2003, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2003, 0.48).
narrative_ontology:measurement(tota_be_t2014, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2014, 0.55).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.78).
narrative_ontology:measurement(tota_su_t1975, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.42).
narrative_ontology:measurement(tota_su_t2003, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2003, 0.5).
narrative_ontology:measurement(tota_su_t2014, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2014, 0.58).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contingent_reachability_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_nonproliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, extended_deterrence_alliance_commitments).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the total_war_reachability_boundary kernel. contraction_reading claims nuclear weapons removed winnable total war from the feasible set entirely (a stronger, more mountain-adjacent claim about permanent structural narrowing). contingent_reachability_reading claims the current low-probability state is technology-dependent and could reverse (a piton-adjacent framing emphasizing atrophy and reversibility). This reading (dropping_reading) claims the probability dropped through an actively maintained coordination equilibrium that remains a rope-riding-tangled-rope structure — real coordination function, real asymmetric extraction, requiring active enforcement to hold. The three readings are not measurement variants of one constraint; each has a distinct ε, distinct beneficiary/victim structure, and distinct persistence logic, linked here via affects_constraints rather than merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
