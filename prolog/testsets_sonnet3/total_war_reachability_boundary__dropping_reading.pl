% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Nuclear Deterrence as Coordination Equilibrium With Defection Risk
 *   domain: international_relations/strategic_studies/nuclear_deterrence
 *
 * SUMMARY:
 *   This reading treats total war among great powers as an arrangement whose
 *   probability has genuinely dropped since 1945 but whose reachability has
 *   never left the feasible set — the drop is a property of a maintained
 *   coordination equilibrium (mutual deterrence), not a structural
 *   closing-off of the possibility the way, say, interstellar travel is
 *   closed off by light-speed limits. On this reading deterrence is a
 *   tangled_rope: it solves a real coordination problem (preventing
 *   first-strike incentives between rival nuclear powers) while
 *   simultaneously extracting involuntary risk exposure from populations,
 *   non-nuclear frontline states, and future generations who never consented
 *   to and do not benefit from the doctrine. The oscillation in the
 *   measurement series (extractiveness spiking during the Cuban Missile
 *   Crisis era, easing after the Cold War's end, rising again amid renewed
 *   great-power competition and modernization cycles) reflects that this is a
 *   maintained equilibrium under continuous renegotiation, not a fixed floor.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — set doctrine, collect leverage
 *   - extended_deterrence_allies: beneficiary/payer (powerful/constrained) — shelter under the umbrella, host the risk
 *   - defense_industrial_base: beneficiary (organized/arbitrage) — profits from credibility-maintenance cycle
 *   - populations_under_nuclear_threat: payer (powerless/trapped) — bears uncompensated tail risk
 *   - non_nuclear_frontline_states: payer (moderate/constrained) — absorbs crisis instability from others' signaling games
 *   - future_generations_facing_residual_risk: payer (powerless/trapped, civilizational horizon) — inherits residual probability mass with no voice
 *   - arms_control_treaty_bodies: observer (institutional/analytical) — verifies but depends on state cooperation
 *   - disarmament_advocacy_movements: excluded (moderate/constrained) — argue the equilibrium is avoidable, pressed from outside the room
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.71).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Nuclear Deterrence as Coordination Equilibrium With Defection Risk").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/strategic_studies/nuclear_deterrence").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '12ffc649-50e6-4a7f-9e8b-883a1c8e85c2').
narrative_ontology:cs_kernel_codification('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', distributed).
narrative_ontology:cs_authority_grounding('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', distributed).
narrative_ontology:cs_reading_relation('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', total_war_reachability_boundary__contingent_reachability_reading, influences).
narrative_ontology:cs_axiom('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', foundational, deterrence_is_maintained_equilibrium_not_structural_closure).
narrative_ontology:cs_axiom_status(deterrence_is_maintained_equilibrium_not_structural_closure, holdable).
narrative_ontology:cs_axiom_grounding('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', deterrence_is_maintained_equilibrium_not_structural_closure, empirically_contingent).
narrative_ontology:cs_axiom('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', secondary, coordination_and_extraction_coexist_in_same_structure).
narrative_ontology:cs_axiom_status(coordination_and_extraction_coexist_in_same_structure, holdable).
narrative_ontology:cs_axiom_grounding('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', coordination_and_extraction_coexist_in_same_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', cold_war_bipolar_mad_equilibrium).
narrative_ontology:cs_drift_state('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', post_2014_multipolar_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('12ffc649-50e6-4a7f-9e8b-883a1c8e85c2', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, defense_industrial_base).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_frontline_states).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, future_generations_facing_residual_risk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain arsenals, doctrines, and command-and-control infrastructure that jointly constitute deterrence. Set the terms of what counts as stabilizing versus destabilizing behavior, negotiate arms-control regimes at their own initiative, and derive diplomatic leverage, alliance leadership, and domestic political capital from possessing the capability. Can modernize, expand, or draw down forces largely on their own timetable; bear none of the direct human cost of a strategic exchange occurring on someone else's territory.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states, beneficiary).

% Rely on a nuclear patron's umbrella instead of developing independent arsenals, gaining security at lower cost and avoiding proliferation stigma. In exchange, host forces, align foreign policy with the patron's strategic posture, and accept that their territory becomes a plausible target or battlespace in an exchange they did not choose to enter. Formal exit (developing independent deterrents or leaving the alliance) is legally possible but politically and economically prohibitive.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, beneficiary,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(total_war_reachability_boundary__dropping_reading, extended_deterrence_allies, payer).

% Designs, builds, and sustains delivery systems, warheads, and modernization programs. Revenue and institutional survival depend on continued perceived credibility-maintenance requirements; has no structural stake in the underlying probability of war itself, only in the continuation of the procurement cycle that credibility-talk justifies.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, defense_industrial_base, beneficiary,
    organized, biographical, arbitrage, national).

% Live within the blast, fallout, and climatic-disruption radius of any large exchange between nuclear states, regardless of consent to the doctrines that put them there. Cannot individually exit the risk pool; migration reduces but does not eliminate exposure given the global reach of nuclear winter effects. Bear the entire tail risk that deterrence is priced to manage without collecting any of the deterrence rent.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, populations_under_nuclear_threat, payer,
    powerless, biographical, trapped, global).

% Sit geographically between nuclear rivals or adjacent to contested flashpoints without possessing their own deterrent leverage. Absorb crisis instability, basing pressure, and escalation risk generated by great-power signaling games they have limited voice in shaping; alignment choices are constrained by geography and existing security guarantees rather than freely chosen.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, non_nuclear_frontline_states, payer,
    moderate, generational, constrained, regional).

% Inherit whatever residual probability of catastrophic exchange the current equilibrium leaves in place, plus the accumulated fissile material, aging command systems, and doctrinal precedents set now. Have no representation in present arms-control or modernization decisions and cannot exit a risk fixed before their existence.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, future_generations_facing_residual_risk, payer,
    powerless, civilizational, trapped, global).

% Monitor compliance, verify reductions, and produce risk assessments used to argue that the equilibrium is stable or eroding. Depend on the cooperation of nuclear states for access and can be sidelined when a state judges verification inconvenient to its modernization plans.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, arms_control_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Argue the entire equilibrium is an avoidable, manufactured risk rather than a stable coordination solution, and that unilateral or verified multilateral disarmament is achievable. Rarely seated in the closed-door deterrence-planning process itself; influence is exerted through public pressure, treaty advocacy (e.g., the Treaty on the Prohibition of Nuclear Weapons), and litigation rather than direct participation in doctrine-setting.
narrative_ontology:constraint_stakeholder(total_war_reachability_boundary__dropping_reading, disarmament_advocacy_movements, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_reachability_boundary__dropping_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(total_war_reachability_boundary__dropping_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mutual possession of survivable second-strike capability solves a genuine collective-action problem among rival great powers: absent credible retaliation, a first-strike advantage would incentivize preemption, so each side's deterrent stabilizes the other's restraint. This is a real Schelling-point equilibrium, not pure theater — crisis stability metrics (warning time, survivability, command redundancy) track an actual coordination function.
% TRANSFER_FUNCTION: Moves diplomatic leverage, alliance cohesion, and procurement revenue toward nuclear weapon states, their armed services, and defense contractors; moves involuntary, uncompensated tail-risk exposure onto populations who never consented to the doctrine, non-nuclear states caught in the geography of confrontation, and future generations who inherit the residual probability mass.
% ABSENT_VOICES: Populations in target-dense regions, non-nuclear frontline states, and future generations have no seat in doctrine-setting or force-modernization decisions; disarmament movements press from outside the process through treaties and public advocacy rather than inside it. Their absence means the 'stability' consensus is disproportionately produced by the parties who benefit from continued credibility, not audited against the preferences of those who bear the tail risk.
% DISAPPEARANCE_RATIONALE: If mutual deterrence relationships vanished overnight without replacement (not merely disarmament by treaty, but sudden collapse of the credibility structure), the strategic calculus of every nuclear-adjacent state would reorganize: alliance structures built on extended deterrence would need new security guarantees, conventional force postures would be renegotiated, and the specific stabilizing function deterrence performs (preventing preemptive first strikes) would need a substitute or absence would itself become destabilizing. The arrangement is load-bearing, not decorative.
% FOUNDING_PROBLEM: The founding problem was the credible prevention of a first-strike incentive between nuclear-armed rivals during the early Cold War: without assured retaliation, whichever side struck first could plausibly disarm the other, making restraint irrational. Mutual assured destruction was constructed to make first strikes irrational instead.
% FOUNDING_PROBLEM_CORROBORATION: Strategic studies scholars outside the defense-procurement community (e.g. independent arms-control researchers, some retired military officers turned critics) attest the core first-strike-prevention problem remains partially live given multipolar proliferation, but argue the scale and modernization pace of current arsenals now exceeds what stability requires — i.e. the coordination function persists at a fraction of current force levels, and the remainder is credibility-maintenance and industrial-base momentum rather than the founding problem itself. Nuclear weapon states and their defense establishments attest the full current posture remains necessary; this is corroboration from inside the beneficiary set and is weighted accordingly.
narrative_ontology:disappearance_verdict(total_war_reachability_boundary__dropping_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_reachability_boundary__dropping_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_reachability_boundary__dropping_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that a meaningful share of the arrangement's operation is not explained by the coordination function alone: force levels, modernization tempo, and doctrinal expansiveness (e.g. counterforce postures beyond minimal deterrence) exceed what pure first-strike prevention requires, and the surplus reads as credibility-rent and industrial-base capture layered onto genuine coordination. Suppression (0.71) is high because the equilibrium depends on continuously suppressed alternatives — unilateral disarmament, no-first-use pledges, arms-race exits — being treated as unthinkable within the doctrine-setting community; this is a raw structural property of how the arrangement is defended, not scaled by any context dimension. Theater ratio (0.28) is moderate-low: most of the apparatus (survivable second-strike forces, verification regimes) performs a real function, but a rising share (0.10 in 1945 to 0.28 in 2025) is signaling and modernization theater whose primary purpose is maintaining perceived credibility rather than marginal deterrent value. Accessibility collapse (0.42) is only moderate — unlike a genuine mountain, workable alternative equilibria (minimal deterrence, no-first-use, multilateral verified reduction) remain conceivable and have been proposed by credentialed strategists; they are suppressed, not physically foreclosed. Resistance (0.55) is substantial: disarmament movements, some arms-control scholarship, and periodic domestic political pushback against modernization budgets constitute real, sustained resistance to the arrangement's expansiveness, even though it has not dislodged the core structure.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the arrangement is stabilizing coordination it actively manages and from which it draws real strategic benefit — the engine should compute this seat close to the rope end conditional on the coordination facts alone. From the powerless/trapped payer seats (populations under threat, future generations), the identical structure computes as high effective extraction: full target directionality, no meaningful exit, tail risk borne without consent. The tangled_rope classification is exactly what holds both computations as true simultaneously without collapsing one into the other — the coordination function is real AND the extraction is real, on the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and defense industrial base sit near the beneficiary end of directionality (d low): they set terms, capture leverage/revenue, and enjoy the most mobile exit options (arbitrage — able to renegotiate posture on their own schedule). Extended deterrence allies are mixed but nearer symmetric-to-beneficiary (constrained exit, real security benefit, some risk absorption). Populations under nuclear threat and future generations sit at the full-target end (d high): trapped exit, zero say in doctrine, full exposure to tail risk with no offsetting benefit captured. Non-nuclear frontline states sit closer to target than beneficiary: moderate power lets them exert some diplomatic pressure, but constrained exit and geography leave them absorbing risk generated by others' signaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing an early-Cold-War first-strike incentive) is contested as live versus dead: a reduced, minimal-deterrence posture would likely satisfy the original coordination requirement, but the current force levels, modernization programs, and doctrinal scope exceed that minimum. Classifying this as tangled_rope rather than snare prevents mislabeling a structure with a genuine, still-partially-live coordination function as pure extraction; classifying it as tangled_rope rather than rope (or mountain) prevents treating the surplus credibility-rent and involuntary risk transfer as costless or natural. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) is itself diagnostic: the arrangement is genuinely load-bearing for present alliance structures even as its scale has drifted past what the founding problem requires — a partial-zombie signature rather than a clean live-function or clean-capture case.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_versus_capture_scale,
    'How much of the current nuclear force posture and modernization tempo is required by the genuine first-strike-prevention coordination function, versus how much is credibility-rent and defense-industrial capture layered on top?',
    'Comparative force-structure analysis against minimal-deterrence proposals from independent strategic studies literature; historical natural experiments where force levels were reduced (e.g. post-INF, post-New START) without observable degradation in crisis stability.',
    'If the coordination-required floor is much lower than current posture, the extraction share of this tangled_rope is larger than currently authored and the classification should weight more heavily toward the extractive pole; if current posture tracks close to the coordination-required floor, the arrangement is closer to a genuine rope with modest extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_versus_capture_scale, empirical, 'What fraction of current deterrence posture is coordination-necessary versus rent.').

omega_variable(
    reachability_versus_probability_distinctness,
    'Is ''total war remains reachable but has dropped in probability'' a stable, well-defined structural claim distinct from the contraction reading''s ''total war left the feasible set,'' or do the two readings converge asymptotically as probability approaches a floor indistinguishable from zero?',
    'Formal decision-theoretic modeling of the deterrence game''s equilibrium stability under stress (crisis instability studies, near-miss historical incident analysis) to establish whether a nonzero defection-probability floor is empirically distinguishable from structural infeasibility.',
    'If the two readings are empirically indistinguishable at current probability estimates, treating them as separate constraints (tangled_rope vs mountain) may be a framing artifact rather than a structural fact — though per the ε-invariance principle each reading still authors its own stable ε from its own premises regardless of empirical convergence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reachability_versus_probability_distinctness, conceptual, 'Whether the dropping-probability and contraction readings are structurally distinct or converge under scrutiny.').

omega_variable(
    involuntary_risk_bearer_consent_status,
    'Do populations under nuclear threat, non-nuclear frontline states, and future generations have any meaningful mechanism of consent to or voice within the deterrence equilibrium, given that democratic mandates for nuclear-armed states nominally represent their own citizens but not foreign populations or future persons?',
    'Comparative analysis of nuclear policy decision processes against any existing international mechanisms (UN General Assembly resolutions, treaty ratification patterns, ICJ advisory opinions) that could constitute a proxy consent channel.',
    'If no meaningful consent mechanism exists for the majority of risk-bearers, the victim/payer classification for these groups is strongly corroborated and the extraction component of the tangled_rope reading is robust; if meaningful proxy consent exists, the extraction framing for those seats should be weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(involuntary_risk_bearer_consent_status, conceptual, 'Whether risk-bearing non-nuclear populations have any consent mechanism within the deterrence arrangement.').


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
narrative_ontology:measurement(tota_tr_t2005, total_war_reachability_boundary__dropping_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(tota_tr_t2015, total_war_reachability_boundary__dropping_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(tota_tr_t2025, total_war_reachability_boundary__dropping_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(tota_be_t1962, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1962, 0.68).
narrative_ontology:measurement(tota_be_t1975, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(tota_be_t1991, total_war_reachability_boundary__dropping_reading, base_extractiveness, 1991, 0.38).
narrative_ontology:measurement(tota_be_t2005, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement(tota_be_t2015, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(tota_be_t2025, total_war_reachability_boundary__dropping_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1962, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1962, 0.85).
narrative_ontology:measurement(tota_su_t1975, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1975, 0.72).
narrative_ontology:measurement(tota_su_t1991, total_war_reachability_boundary__dropping_reading, suppression_requirement, 1991, 0.55).
narrative_ontology:measurement(tota_su_t2005, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(tota_su_t2015, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(tota_su_t2025, total_war_reachability_boundary__dropping_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_reachability_boundary__dropping_reading, 0.12).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contraction_reading).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, contingent_reachability_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_reachability_boundary kernel. contraction_reading claims nuclear weapons removed winnable total war from the feasible set entirely (mountain-adjacent framing: the strategic space itself contracted). contingent_reachability_reading claims the current low-probability state is a piton — an atrophied capability that could reverse with technological change (e.g. effective missile defense, first-strike-enabling precision/AI advances). This reading (dropping_reading) claims total war's probability dropped through an actively maintained coordination equilibrium (deterrence) that remains subject to defection risk, making it a tangled_rope rather than a mountain or a piton. All three readings share the same underlying kernel — the boundary of reachability for great-power total war since 1945 — but author structurally distinct ε, beneficiary/victim sets, and classifications from that shared kernel, per the ε-invariance principle: they are linked via affects_constraints rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
