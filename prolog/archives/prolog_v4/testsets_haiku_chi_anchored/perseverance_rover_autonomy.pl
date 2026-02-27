% ============================================================================
% CONSTRAINT STORY: perseverance_rover_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_perseverance_rover_autonomy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: perseverance_rover_autonomy
 *   human_readable: Perseverance Rover Autonomy Constraint
 *   domain: technological/space_exploration
 *
 * SUMMARY:
 *   Perseverance's autonomous decision-making capability presents a
 *   multi-layered constraint on human control and operational authority. The
 *   20-minute one-way light delay creates a technical fact: ground operators
 *   cannot issue real-time commands and receive responses within a Martian
 *   sol. This hard constraint enables a coordination benefit (rovers can
 *   explore efficiently without waiting for communication cycles) but
 *   simultaneously extracts authority from human operators, who become
 *   supervisors of pre-programmed objectives rather than controllers of
 *   moment-to-moment decisions. The constraint exhibits the full range of DR
 *   types depending on perspective: immutable physical law (speed of light),
 *   legitimate coordination mechanism (efficient exploration), temporary
 *   scaffolding (until human presence on Mars), degraded institutional
 *   doctrine (autonomy treated as absolute when contingencies exist), mixed
 *   coordination-extraction hybrid (for operators and rovers), and pure
 *   extraction (for mission control's decision authority). The theater ratio
 *   (0.61) indicates that operational discourse naturalizes technical
 *   constraints into mission philosophy more than the actual mission
 *   requirements necessitate — 90% of the rover's cycle is spent in overnight
 *   thermal dormancy where autonomy provides no advantage, yet autonomy
 *   doctrine treats it as continuous necessity. The extractiveness trend
 *   (0.28→0.38 over 1000 sols) reflects institutional accumulation: as the
 *   mission extends, risk-averse guardrails are reinforced, suppressing
 *   operator authority further.
 *
 * KEY AGENTS:
 *   - Mission Control Operators: Primary victims (powerless/trapped) — bear responsibility without moment-to-moment control authority; trapped by light-delay physics and operational doctrine
 *   - Perseverance Rover: Distributed decision-maker (moderate/constrained) — enabled to explore but constrained by pre-programmed safety architecture; cannot deviate from guardrails
 *   - Planetary Science Objectives: Primary beneficiaries (institutional/arbitrage) — gain from rapid geological sampling and efficient traverse rates; bear no extraction costs
 *   - Future Human Mars Missions: Organized actors (organized/constrained) — see autonomy as transitional scaffolding; sunset clause embedded in architecture
 *   - Radio Communication Infrastructure: Institutional infrastructure (institutional/arbitrage) — maintains operational doctrine of absolute autonomy; acts as piton through inertia
 *   - Analytical Observer: Civilizational frame (analytical/analytical) — risks naturalizing institutional policy into laws of physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(perseverance_rover_autonomy, 0.38).
domain_priors:suppression_score(perseverance_rover_autonomy, 0.52).
domain_priors:theater_ratio(perseverance_rover_autonomy, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(perseverance_rover_autonomy, extractiveness, 0.38).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(perseverance_rover_autonomy, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(perseverance_rover_autonomy, tangled_rope).
narrative_ontology:human_readable(perseverance_rover_autonomy, "Perseverance Rover Autonomy Constraint").
narrative_ontology:topic_domain(perseverance_rover_autonomy, "technological/space_exploration").

domain_priors:requires_active_enforcement(perseverance_rover_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, mars_exploration_program).
narrative_ontology:constraint_beneficiary(perseverance_rover_autonomy, planetary_science_objectives).
narrative_ontology:constraint_victim(perseverance_rover_autonomy, mission_control_operator_agency).
narrative_ontology:constraint_victim(perseverance_rover_autonomy, real_time_decision_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISSION CONTROL OPERATOR (SNARE) — Ground control cannot override autonomous decisions in real time due to 20-minute light-delay. Operators are trapped in a subordinate role: they plan waypoints and objectives but the rover executes without direct human authorization. Extraction is asymmetric: operators bear responsibility for outcomes while lacking moment-to-moment control. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PERSEVERANCE ROVER AGENCY (TANGLED ROPE) — The rover experiences the autonomy constraint as coordination infrastructure: it is enabled to explore efficiently (coordination benefit) but constrained by pre-programmed safety guardrails, wheel sensors, and geological risk parameters (extraction cost). The rover cannot deviate from its decision architecture even when local conditions suggest alternative strategies. d≈0.58, f(d)≈0.76, σ=1.0 → χ≈0.29.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLANETARY SCIENCE OBJECTIVES (ROPE) — The autonomy requirement directly serves science goals: rapid geological sampling, efficient traverse rates, and opportunistic target selection improve discovery capacity. The science objectives benefit from the constraint without bearing extraction costs — the rover does the work, not the science teams. Autonomy is perceived as enabling, not constraining. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FUTURE HUMAN MARS MISSIONS (SCAFFOLD) — Perseverance's autonomy architecture is temporary scaffolding for robotic exploration that will be supplanted by human operators on-site. Once humans are present on Mars, real-time control returns possible and the autonomy constraint sunset occurs. The constraint is intentionally designed as transitional: building robotic autonomy technology until human presence renders it unnecessary. d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.12. Has sunset clause by design.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RADIO COMMUNICATION INFRASTRUCTURE (PITON) — The 20-minute one-way light delay creates a technical bottleneck that has become reified into operational doctrine: 'rovers must be autonomous.' But the infrastructure is partially theater — Perseverance spends 90% of its operational cycle in overnight thermal management mode, during which autonomy provides no advantage. The autonomy constraint persists through institutional inertia even though shorter daylight-only planning cycles could reduce it. theater_ratio=0.61 captures this degradation: operational doctrine treats autonomy as absolute necessity when it is actually a periodically-necessary coordination mechanism. d≈0.10, f(d)≈-0.04, σ=1.0 → χ≈-0.02.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/physical perspective, the light-delay constraint is an immutable law: photons travel at 3×10^8 m/s, Mars orbits at ~225 million km from Earth, creating a ~20-minute one-way delay. No engineering can eliminate this. However, the base properties (ε=0.38, suppression=0.52) indicate that contingent institutional choices (autonomy architecture, operational doctrine, risk-aversion thresholds) are the actual constraint. The mountain perspective risks naturalizing policy into physics. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(perseverance_rover_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(perseverance_rover_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(perseverance_rover_autonomy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(perseverance_rover_autonomy, TR),
    TR >= 0.70.

:- end_tests(perseverance_rover_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts human operator agency (they cannot direct moment-to-moment decisions) but the extraction is justified by legitimate science coordination benefits (rapid exploration, autonomous risk response). The extraction is not as severe as a pure snare because mission control still designs objectives, analyzes results, and plans future waypoints. The moderate value reflects that extraction and coordination coexist. Suppression (0.52): Moderate-high. Significant barriers to operator intervention include the physical light delay (ineliminable), conservative safety guardrails (partially contingent), and institutional doctrine treating autonomy as absolute (contingent). Operators cannot override decisions, cannot request real-time clarification, and cannot interrupt execution cycles. But suppression is not maximal because operators do have planning authority and can redesign objectives post-analysis. Theater ratio (0.61): Moderate-high. Operational culture emphasizes autonomy as a fundamental principle, yet the rover spends 90% of its cycle in dormancy where autonomy provides zero benefit. The autonomy doctrine is partly theater — reified as absolute necessity when actual mission constraints are more granular. ArXiv equivalent in operational practice: papers describing 'rover autonomy' often present it as philosophically central when actual requirement analysis shows it is locally necessary (during traverse) but globally overstated (during thermal cycles). The theater has increased over the mission (0.35→0.61) as institutional risk-aversion has accumulated in guardrail density.
 *
 * PERSPECTIVAL GAP:
 *   Mission control operators experience pure extraction (Snare): they are trapped in subordinate roles by both physics (light delay) and policy (operational doctrine). Planetary science experiences pure coordination (Rope): they are enabled by autonomy without bearing extraction costs. The rover experiences mixed coordination and extraction (Tangled Rope): enabled to explore, constrained by guardrails. Future human missions experience temporary scaffolding (Scaffold): autonomy is intentionally transitional. The communication infrastructure maintains a degraded ritual (Piton): autonomy doctrine persists through inertia. The civilizational analyst risks seeing immutable law (Mountain): speed of light creates autonomy necessity. The gap is not merely observational — it reflects genuine conflict in how the constraint is valued: operators see loss of authority, scientists see gain in capability, and institutional doctrine naturalizes one into the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Mission control operators: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. They are structural targets of the constraint with no exit path. Perseverance rover: Victim of guardrails + constrained → d≈0.58, f(d)≈0.76. Moderate-high extraction. Enabled for exploration but constrained in decision-making. Planetary science objectives: Beneficiaries + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They gain exploration capability without bearing constraint costs. Future human missions: Organized + constrained → d≈0.35, f(d)≈0.32. Low-moderate extraction. They have agency (organization level) and an exit path (sunset clause); constrained only during the scaffolding period. Radio communication infrastructure: Institutional + arbitrage → d≈0.10, f(d)≈-0.04. Net beneficiary due to piton classification (theater dominance, not extraction dominance). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival risk; the false summit detector will flag it because base properties indicate contingent institutional architecture, not immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through explicit decomposition of what 'autonomy' means in mission context. The mandatrophy question is: 'Is autonomy a coordination mechanism enabling science (Rope) or an extraction mechanism removing operator authority (Snare)?' The answer is: both, at different levels. The constraint functions as Rope for science objectives (coordination benefit: rapid exploration). It functions as Snare for mission control (extraction of decision authority). It functions as Scaffold for future human missions (temporary, intentional). The mandatrophy is resolved by recognizing that the constraint's classification depends on structural position relative to the autonomy boundary, not on whether autonomy is 'good' or 'bad' in general. The false summit risk (mountain perspective) naturalizes what is actually a policy choice (operators cannot override) into what appears to be a law of physics (light delay). The engine's task is to flag when institutional doctrine collapses contingent architecture into apparent necessity. Here: the light delay is true physics, but the operational doctrine of absolute autonomy (no human override, no delayed-veto protocols, no partial-loop planning) is contingent engineering choice. The constraint story disambiguates: the base properties show that extractiveness and suppression are high enough that autonomy cannot be a pure mountain. The perspectival range (Snare through Mountain) reveals that the mountain view requires false summit correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_necessity_threshold,
    'What communication delay threshold genuinely requires autonomous rover operation vs allows human-loop planning cycles?',
    'Comparative mission analysis: Curiosity rover (similar delay) vs future missions with faster planning cycles; time-to-discovery metrics with different autonomy levels; failure analysis of constraint violations',
    'If threshold < 10 min: current autonomy level is extractive policy choice. If threshold > 30 min: autonomy is a genuine technical necessity. Current implementation at 20-min delay sits in the ambiguity zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_necessity_threshold, empirical, 'Whether 20-minute light delay genuinely necessitates rover autonomy').

omega_variable(
    risk_parameters_human_calibrated,
    'Are Perseverance''s autonomy guardrails (wheel slip thresholds, cliff detection distances, thermal risk cutoffs) optimized for robotic safety or are they conservatively set due to liability aversion?',
    'Comparison of actual failure rates vs predicted failure rates from guardrail parameters; analysis of Opportunity and Spirit failures relative to their autonomy thresholds; expert assessment of how different risk parameters would have changed mission outcomes',
    'If human-calibrated conservatively: suppression could be reduced without safety loss (constraint is partly theater). If physically necessary: current suppression reflects genuine epistemic limits of distributed decision-making.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(risk_parameters_human_calibrated, empirical, 'Whether autonomy guardrails are physically necessary or conservatively risk-averse').

omega_variable(
    operator_authority_partial_recovery,
    'Could planning cycles be restructured to allow mission control to veto or modify rover decisions post-execution, creating a partial human-loop that partially recovers operator agency?',
    'Simulation of delayed-veto protocols: rover executes autonomously, then receives ground command to abort or redirect before next cycle begins. Testing whether this recovers meaningful operator authority without compromising safety.',
    'If feasible: constraint type shifts from Snare toward Tangled Rope for operators (they recover partial authority). If infeasible: operator trap is genuine technical constraint, not just policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_authority_partial_recovery, empirical, 'Whether delayed veto protocols could partially recover operator authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(perseverance_rover_autonomy, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pra_tr_t0, perseverance_rover_autonomy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pra_tr_t500, perseverance_rover_autonomy, theater_ratio, 500, 0.55).
narrative_ontology:measurement(pra_tr_t1000, perseverance_rover_autonomy, theater_ratio, 1000, 0.61).

% Extraction over time
narrative_ontology:measurement(pra_be_t0, perseverance_rover_autonomy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pra_be_t500, perseverance_rover_autonomy, base_extractiveness, 500, 0.34).
narrative_ontology:measurement(pra_be_t1000, perseverance_rover_autonomy, base_extractiveness, 1000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(perseverance_rover_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(perseverance_rover_autonomy, mars_communication_latency_barrier).
narrative_ontology:affects_constraint(perseverance_rover_autonomy, human_mars_exploration_timeline).

% DUAL FORMULATION NOTE:
% Perseverance autonomy is downstream of the physical constraint (Mars orbital distance → light delay) but represents a distinct architectural constraint. The upstream physical limit has ε≈0.05 (mountain: speed of light immutable). The autonomy constraint has ε=0.38 (tangled rope: institutional choices amplify extraction beyond bare physics). This decomposition mirrors the BGS pattern: universal law (light speed) vs contested implementation (rover doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
