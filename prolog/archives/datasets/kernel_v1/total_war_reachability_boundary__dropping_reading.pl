% ============================================================================
% CONSTRAINT STORY: total_war_reachability_boundary__dropping_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_reachability_boundary__dropping_reading
 *   human_readable: Total War Reachability Boundary (Dropping Reading): Deterrence as Coordination Equilibrium
 *   domain: international_relations/nuclear_deterrence/strategic_studies
 *
 * SUMMARY:
 *   The total war reachability boundary represents a constraint on the
 *   strategic feasibility of nuclear-armed conflict. This story instantiates
 *   the 'dropping reading' of a contested kernel: the claim that total war
 *   between nuclear powers has DROPPED IN PROBABILITY while remaining
 *   REACHABLE from a strategic standpoint. Under this reading, deterrence is
 *   a coordination equilibrium (a rope/tangled rope) rather than a natural
 *   law—actors remain locked in a mutual vulnerability relationship that they
 *   actively maintain through deterrent threat, not through physics alone.
 *   The probability of total war has fallen (relative to the early Cold War
 *   or to pre-nuclear era fears) because deterrence institutions and
 *   doctrines have matured and because actors have learned the costs of
 *   brinkmanship. But reachability—the structural fact that actors retain the
 *   capacity to wage total war if they chose defection—persists. The
 *   constraint thus exhibits coordination (deterrence credibility benefits
 *   all parties) coupled with extraction (all populations remain under
 *   existential threat). This reading coexists with two sibling readings: the
 *   contraction reading (total war left the feasible set entirely, a true
 *   mountain) and the contingent reachability reading (reachability is
 *   atrophying—a piton—and could reverse with technology). This story
 *   generates the dropping reading as a clean ε-invariant constraint; the
 *   sibling readings are separate stories.
 *
 * KEY AGENTS:
 *   - Nuclear-armed great powers (institutional/arbitrage): Beneficiaries of deterrence credibility and first-mover strategic advantage; experience deterrence as coordination
 *   - Global population under nuclear threat (powerless/trapped): Victims of the existential threat; cannot exit or modify the deterrence system
 *   - Non-nuclear states (powerless/trapped): Dependent on great-power deterrence for security; lack agency in the deterrence equilibrium
 *   - Alliance members and extended deterrence recipients (moderate/constrained): Protected by nuclear umbrella but constrained by dependence on nuclear powers' strategic decisions
 *   - Deterrence credibility system (organized/constrained): Functions as both coordinator (benefits all through mutual vulnerability) and extractor (imposes threat on all)
 *   - Cold War deterrence infrastructure (institutional/arbitrage): Maintains performative apparatus; functions at diminished capacity relative to original design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, 0.58).
domain_priors:suppression_score(total_war_reachability_boundary__dropping_reading, 0.65).
domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(total_war_reachability_boundary__dropping_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_reachability_boundary__dropping_reading, tangled_rope).
narrative_ontology:human_readable(total_war_reachability_boundary__dropping_reading, "Total War Reachability Boundary (Dropping Reading): Deterrence as Coordination Equilibrium").
narrative_ontology:topic_domain(total_war_reachability_boundary__dropping_reading, "international_relations/nuclear_deterrence/strategic_studies").

domain_priors:requires_active_enforcement(total_war_reachability_boundary__dropping_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_reachability_boundary__dropping_reading, '3c20e3e1-fd15-4a7e-924c-c20c592926d5').
narrative_ontology:cs_kernel_codification('3c20e3e1-fd15-4a7e-924c-c20c592926d5', formalized).
narrative_ontology:cs_authority_grounding('3c20e3e1-fd15-4a7e-924c-c20c592926d5', extraction).
narrative_ontology:cs_interpretation_layer_present('3c20e3e1-fd15-4a7e-924c-c20c592926d5').
narrative_ontology:cs_reading_relation('3c20e3e1-fd15-4a7e-924c-c20c592926d5', total_war_reachability_boundary__contingent_reachability_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c20e3e1-fd15-4a7e-924c-c20c592926d5', total_war_reachability_boundary__contraction_reading, coexists_with).
narrative_ontology:cs_axiom('3c20e3e1-fd15-4a7e-924c-c20c592926d5', foundational, reachability_stability_under_deterrence).
narrative_ontology:cs_axiom_status(reachability_stability_under_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('3c20e3e1-fd15-4a7e-924c-c20c592926d5', reachability_stability_under_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('3c20e3e1-fd15-4a7e-924c-c20c592926d5', foundational, deterrence_as_coordination_with_defection_risk).
narrative_ontology:cs_axiom_status(deterrence_as_coordination_with_defection_risk, holdable).
narrative_ontology:cs_axiom_grounding('3c20e3e1-fd15-4a7e-924c-c20c592926d5', deterrence_as_coordination_with_defection_risk, instrumental).
narrative_ontology:cs_reference_frame('3c20e3e1-fd15-4a7e-924c-c20c592926d5', mutual_vulnerability_stability).
narrative_ontology:cs_drift_state('3c20e3e1-fd15-4a7e-924c-c20c592926d5', post_cold_war_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3c20e3e1-fd15-4a7e-924c-c20c592926d5', '').
narrative_ontology:cs_kernel_id(total_war_reachability_boundary__dropping_reading, total_war_reachability_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_reachability_boundary__dropping_reading, deterrence_credibility_system).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, global_population_under_threat).
narrative_ontology:constraint_victim(total_war_reachability_boundary__dropping_reading, non_nuclear_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL POPULATION (SNARE) — Trapped in a strategic arrangement they did not choose and cannot exit. The reachability of total war remains; deterrence holds only through continued commitment by nuclear powers. No exit option and maximum extraction from the existential threat.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NUCLEAR STATES (SNARE) — Structurally dependent on the deterrence equilibrium for security but unable to enforce or adjust its terms. Trapped by great-power competition and unable to exit the deterrence system. Full extraction without agency.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DETERRENCE CREDIBILITY SYSTEM (TANGLED ROPE) — Coordinates strategic stability through mutual vulnerability (coordination function: both sides benefit from predictable escalation barriers). Simultaneously extracts from all populations under the threat umbrella. Active enforcement through weapons modernization, strategic doctrine, and crisis management infrastructure. Beneficiaries (nuclear states) experience this as coordination; victims experience extraction.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NUCLEAR-ARMED GREAT POWERS (ROPE) — Experience deterrence as pure coordination: maintaining credibility through second-strike capability and clear doctrines is beneficial to all parties in the system. The constraint appears as a mutual agreement on escalation boundaries. Exit available through negotiated arms reduction treaties, but deterrence itself is seen as stabilizing.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIANCE MEMBERS (TANGLED ROPE) — Benefit from extended deterrence (nuclear umbrella protection) while constrained by dependence on nuclear powers' strategic decisions. Simultaneously protected and vulnerable. Cannot exit the alliance without losing deterrence benefits; cannot modify deterrence rules unilaterally.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COLD WAR DETERRENCE INFRASTRUCTURE (PITON) — The theatrical apparatus of deterrence (crisis communication protocols, nuclear command-and-control drills, strategic doctrine announcements) persists through institutional inertia. Much of the overt signaling is performative; actual deterrence credibility depends on technical capabilities and implicit mutual understanding. Theater ratio is moderate because some infrastructure is functional, but much maintains Cold War forms despite changed strategic context.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NEOREALIST VIEW (MOUNTAIN) — From a structural realism perspective, deterrence is an emergent feature of anarchy in the international system: given nuclear weapons' destructive capacity and verification limits, mutual vulnerability is an inescapable structural fact. Total war reachability cannot be eliminated without eliminating nuclear weapons themselves. This perspective naturalizes the deterrence boundary as an immutable property of the post-nuclear era. Engine false summit detector will flag this as naturalization of what is actually a contingent institutional choice about deterrence doctrine.
constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_reachability_boundary__dropping_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_reachability_boundary__dropping_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_reachability_boundary__dropping_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_reachability_boundary__dropping_reading, TR),
    TR >= 0.70.

:- end_tests(total_war_reachability_boundary__dropping_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, declining over the interval. The constraint extracts existential risk from the global population in exchange for (purported) strategic stability. The extraction rate has declined from the Cold War's peak (0.72) because deterrence credibility has increased—actors are now more confident in the coordination equilibrium, reducing fear of inadvertent escalation. However, extraction remains substantial because the reachability boundary has NOT contracted; total war remains possible. Suppression (0.65): Moderate-high and rising. Suppression of alternatives to the deterrence system has increased over time. First, the acquisition of nuclear weapons by additional states (France, China, India, Pakistan) has reinforced the deterrence framework globally—actors see no alternative to mutual vulnerability. Second, arms control agreements and non-proliferation norms suppress exit paths for states that might otherwise build nuclear arsenals. Third, the rise of NATO and alliance structures suppresses unilateral defection from deterrence. The rising suppression trajectory reflects institutional maturation of the deterrence system, not increasing coercion—suppression works through incentive alignment, not force. Theater ratio (0.48): Moderate and rising. The theatrical components of deterrence (nuclear posture statements, strategic doctrine announcements, exercises, crisis communication protocols) have increased as a fraction of total deterrence activity. During the Cold War, theater ratio was lower (0.35) because actual weapons deployments and alert postures were operationally critical. In the post-Cold War era, actual deployments have become less frequent and less necessary for credibility; instead, deterrence credibility relies more on doctrinal clarity and signaling. The rising theater reflects the piton perspective's observation that much Cold War infrastructure persists in ceremonial form.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between beneficiaries and victims. Nuclear-armed powers experience the deterrence boundary as coordination (Rope): both sides benefit from credible mutual vulnerability, and the constraint solves the security dilemma. The global population experiences the same constraint as pure extraction (Snare): they bear the existential risk with no ability to consent or exit. The analytical observer from a neorealist position risks classifying this as a Mountain—an emergent property of anarchy—but the structural data reveals active institutional enforcement and beneficiary preservation, disqualifying the mountain classification. The dropping reading's perspectival contribution is to hold that probability reduction (which makes deterrence appear more stable) does NOT imply reachability reduction—the boundary remains a contingent institutional coordination, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-armed powers derive d ≈ 0.10–0.20 (beneficiaries with arbitrage options): they gain deterrence benefits and can exit through negotiated arms reduction or strategic pivot. The analytical context computes d ≈ 0.72 (observer position in the crisis without agency). Trapped populations derive d ≈ 0.95 (maximum exposure, no exit): they bear existential risk and have no structural path to refuse the constraint. Constrained alliance members derive d ≈ 0.65: they benefit from deterrence protection but are locked into dependence on nuclear powers' strategic choices. The chi formula computes effective extraction as ε × f(d) × σ(S). For the global population (d=0.95, S=global): chi ≈ 0.58 × 1.4 × 1.2 ≈ 0.97 (experienced extraction is near-total). For nuclear powers (d=0.15, S=global): chi ≈ 0.58 × 0.05 × 1.2 ≈ 0.04 (experienced extraction is minimal or negative). This 40x gap in experienced chi drives the perspectival classification range from Snare (powerless) to Rope (institutional).
 *
 * MANDATROPHY ANALYSIS:
 *   The dropping reading resolves mandatrophy by showing that deterrence is neither pure coordination (Rope) nor pure extraction (Snare), but a hybrid where coordination and extraction are locked together. Nuclear powers coordinate on mutual vulnerability (beneficial to all if no state defects). But the coordination is maintained through threat and suppression of alternatives (extraction). The probability drop has NOT resolved mandatrophy—if anything, it has deepened the constraint by normalizing deterrence as permanent. If probability had dropped TO ZERO (reachability also contracted), the constraint would resolve to Rope or Mountain. The dropping reading's claim is that probability reduction WITHOUT reachability reduction means the mandatrophy is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_probability_decoupling,
    'Does maintaining reachability without raising probability require continuous enforcement effort, or does the physical constraint of nuclear weapons inherently provide stability?',
    'Empirical tracking of deterrence stability: testing whether reduced enforcement (fewer exercises, lower alert postures, weaker command-and-control) correlates with increased reachability probability. Comparison with periods of higher enforcement intensity.',
    'If enforcement-dependent: deterrence is a tangled_rope requiring active enforcement (current classification confirmed). If physics-dependent: deterrence approaches mountain classification (reachability boundary is a structural law, not a managed constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reachability_vs_probability_decoupling, empirical, 'Whether reachability stability depends on enforcement effort or physics alone').

omega_variable(
    defection_incentive_under_crisis,
    'Under severe crisis (existential threat to a nuclear power), would defection from the deterrence equilibrium become rational, or are the escalation dynamics themselves sufficient to prevent defection?',
    'Game-theoretic analysis of crisis scenarios; historical case studies of near-miss incidents (Cuban Missile Crisis, Kargil, Taiwan Strait); assessment of whether payoff matrices show defection-proof equilibria or contingent stability.',
    'If defection-proof: deterrence is a self-enforcing coordination game (Rope). If contingent: deterrence depends on actors honoring commitments even when defection appears rational (Tangled Rope). If defection is incentivized: the constraint is fundamentally unstable and classified wrongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defection_incentive_under_crisis, empirical, 'Whether deterrence equilibrium is defection-proof under crisis').

omega_variable(
    technological_reachability_drift,
    'Does technological change (hypersonic weapons, AI-enabled decision-making, anti-satellite capabilities, quantum computing) shift the reachability boundary itself, making total war more or less reachable?',
    'Monitoring technological development in strategic weapon systems; assessment of how new capabilities alter mutual vulnerability assumptions; evaluation of whether new technologies create bypasses around deterrence or reinforce it.',
    'If reachability increases: the dropping reading''s core claim weakens (probability dropping may be temporary). If reachability decreases: dropping reading is confirmed. If reachability oscillates: deterrence stability is contingent on technological parity, shifting classification toward Piton (inertial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_reachability_drift, empirical, 'Whether technological change alters total war reachability').

omega_variable(
    reading_contest_framing_choice,
    'Is the dropping reading''s core claim—that probability dropped but reachability remains—grounded in material strategic facts or in a choice about how to frame the deterrence relationship?',
    'Distinction between reachability (could actors implement total war and survive it?) and probability (would rational actors choose to do so?). The dropping reading decouples these; other readings may couple them. Assess which decoupling captures the actual strategic structure.',
    'If decoupling reflects material reality: dropping reading stands as a distinct constraint type (Tangled Rope). If decoupling is framing choice: all three readings coexist, and reading_relations should reflect coexistence rather than competition. This omega documents the kernel-level under-determination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_framing_choice, conceptual, 'Whether reachability/probability decoupling is material or framing-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_reachability_boundary__dropping_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twrb_drop_theater_t0, total_war_reachability_boundary__dropping_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(twrb_drop_theater_t25, total_war_reachability_boundary__dropping_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(twrb_drop_theater_t50, total_war_reachability_boundary__dropping_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(twrb_drop_extr_t0, total_war_reachability_boundary__dropping_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(twrb_drop_extr_t25, total_war_reachability_boundary__dropping_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(twrb_drop_extr_t50, total_war_reachability_boundary__dropping_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(twrb_drop_supp_t0, total_war_reachability_boundary__dropping_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(twrb_drop_supp_t25, total_war_reachability_boundary__dropping_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(twrb_drop_supp_t50, total_war_reachability_boundary__dropping_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_reachability_boundary__dropping_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, nuclear_escalation_ladder).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, mutual_assured_destruction_credibility).
narrative_ontology:affects_constraint(total_war_reachability_boundary__dropping_reading, first_strike_stability_dilemma).

% DUAL FORMULATION NOTE:
% The total war reachability boundary decomposes into three structurally distinct constraint stories, each instantiating a different reading of a single contested kernel. The dropping reading (this story) claims deterrence is a Tangled Rope with defection risk remaining. The contraction reading (separate story) claims total war has left the feasible set (Mountain). The contingent reachability reading (separate story) claims reachability is atrophying as a Piton. Network links reflect how these readings stand to each other: the dropping reading influences the other two by establishing reachability as the baseline against which contraction and atrophy are measured.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_reachability_boundary__dropping_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
