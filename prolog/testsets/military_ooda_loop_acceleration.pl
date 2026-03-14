% ============================================================================
% CONSTRAINT STORY: military_ooda_loop_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_military_ooda_loop_acceleration, []).

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
 *   constraint_id: military_ooda_loop_acceleration
 *   human_readable: Military OODA Loop Acceleration Constraint
 *   domain: military/strategic_doctrine
 *
 * SUMMARY:
 *   The military OODA loop (Observe-Orient-Decide-Act) acceleration
 *   constraint emerges from the structural tension between technological
 *   capability advancement and strategic stability requirements. As sensor
 *   systems, communication networks, and decision-support AI improve,
 *   military doctrine under competitive pressure pushes toward faster
 *   decision cycles. This compression extracts from technologically
 *   disadvantaged nations, destabilizes arms control verification, and
 *   compresses human judgment windows below safe thresholds. The constraint
 *   is a tangled rope because it simultaneously coordinates genuine tactical
 *   improvement (faster responsiveness to battlefield conditions) and
 *   extracts through forced adoption of risky automation before human-system
 *   reliability is established. Extractiveness has increased from 0.35 to
 *   0.58 over the measurement interval as automation systems matured and were
 *   deployed operationally, while theater ratio remained relatively low (0.25
 *   to 0.38), indicating the constraint's primary function is real (not
 *   performative) but driven by genuine competitive dynamics rather than
 *   institutional theater.
 *
 * KEY AGENTS:
 *   - Technologically Advanced Military: Primary beneficiary (institutional/arbitrage) — captures strategic advantage through sensor and processing superiority; experiences OODA loop acceleration as coordination gain
 *   - Technologically Disadvantaged Military: Primary victim (powerless/trapped) — forced to accelerate decision cycles beyond safe automation thresholds to avoid strategic defeat; cannot exit without accepting strategic vulnerability
 *   - Defense Contractors: Beneficiary (institutional/arbitrage) — profit from demand for advanced sensors, AI processing, and automated command systems; experience constraint as pure market coordination
 *   - Arms Control Verification Regime: Secondary victim (powerful/constrained) — compressed decision cycles reduce verification window; forced to work within timescales set by military operational requirements
 *   - Strategic Stability (Abstract Collective Good): Victim (powerless/trapped) — compressed decision cycles increase accidental escalation risk; no advocacy or exit mechanism
 *   - Scientific/Intelligence Community: Mixed (organized/constrained) — coordinate with military on capability assessment but experience extraction through demand for faster analysis without resource increase
 *   - Analytical Observer: Risk naturalizer (analytical/analytical) — perspective that frames OODA acceleration as inevitable consequence of technology rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(military_ooda_loop_acceleration, 0.58).
domain_priors:suppression_score(military_ooda_loop_acceleration, 0.62).
domain_priors:theater_ratio(military_ooda_loop_acceleration, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(military_ooda_loop_acceleration, extractiveness, 0.58).
narrative_ontology:constraint_metric(military_ooda_loop_acceleration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(military_ooda_loop_acceleration, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(military_ooda_loop_acceleration, tangled_rope).
narrative_ontology:human_readable(military_ooda_loop_acceleration, "Military OODA Loop Acceleration Constraint").
narrative_ontology:topic_domain(military_ooda_loop_acceleration, "military/strategic_doctrine").

domain_priors:requires_active_enforcement(military_ooda_loop_acceleration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(military_ooda_loop_acceleration, technologically_advanced_militaries).
narrative_ontology:constraint_beneficiary(military_ooda_loop_acceleration, defense_contractors).
narrative_ontology:constraint_beneficiary(military_ooda_loop_acceleration, military_intelligence_agencies).
narrative_ontology:constraint_victim(military_ooda_loop_acceleration, slower_decision_cycle_nations).
narrative_ontology:constraint_victim(military_ooda_loop_acceleration, strategic_stability).
narrative_ontology:constraint_victim(military_ooda_loop_acceleration, arms_control_verification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TECHNOLOGICALLY DISADVANTAGED MILITARY (SNARE) — Trapped in an escalatory arms race. Cannot exit without strategic vulnerability. Forced to accelerate sensor-processing pipelines, compress command cycles, and automate decision-making even when reliability is uncertain. Experiences maximum extraction: forced adoption of risky doctrine to avoid being decisively defeated during their slower decision window.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER MILITARY POWER (TANGLED ROPE) — Constrained by procurement timelines and industrial capacity, but also benefits from genuine capability improvement. Experiences both coordination (faster decision cycles improve tactical responsiveness) and extraction (forced to adopt technologies before they mature, creating vulnerability to systemic failures). High suppression from competitive pressure, but some agency in force doctrine development.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGICALLY ADVANCED MILITARY (ROPE) — Experiences the constraint as coordination: their technological superiority creates genuine benefits through faster decision-making. They can arbitrage out by maintaining lead; accelerating OODA loops reinforces their advantage. Net beneficiary of the constraint — it rewards their investment in sensors, processing, and automation.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEFENSE CONTRACTOR (ROPE) — Pure coordination from this perspective: accelerating OODA loops drives demand for sensor systems, AI-enabled processing, network infrastructure, and automation hardware. No perceived extraction — the constraint aligns perfectly with market incentives. Beneficiary with full arbitrage: can shift investment between advanced militaries to maximize revenue.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ARMS CONTROL VERIFICATION REGIME (TANGLED ROPE) — Constrained by the need to maintain credibility and access. Benefits from coordination function: faster decision-making creates transparency demands that verification regimes can exploit. But also experiences extraction: OODA loop acceleration enables covert capability accumulation (faster deployment cycles compress verification windows). Suppression from political pressure to not slow military operations.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STRATEGIC STABILITY (SNARE) — Trapped as an abstract collective good. Compressed decision cycles increase accidental escalation risk: faster OODA loops reduce human judgment windows and increase reliance on automated threat assessment. No exit option for this abstract constraint bearer. Experiences full extraction as decision compression outpaces verification and diplomatic channels.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: SCIENTIFIC/INTELLIGENCE COMMUNITY (TANGLED ROPE) — Organized but constrained by classification restrictions and access requirements. Coordinates with military on capability assessment but experiences extraction through demand for faster analysis without commensurate resource increase. Benefits from constraint through career advancement and institutional prestige; constrained by the need to maintain security clearances and institutional relationships.
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ACTION-REACTION CYCLE (MOUNTAIN) — From a civilizational perspective, OODA loop acceleration follows inevitably from information technology advancement and competitive military dynamics. The constraint appears to be a natural law: whenever sensor and communication capabilities improve, decision cycles must compress or the faster actor gains decisive advantage. This perspective risks naturalizing what is actually a contingent institutional choice (whether to trust automation over human judgment).
constraint_indexing:constraint_classification(military_ooda_loop_acceleration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(military_ooda_loop_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(military_ooda_loop_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(military_ooda_loop_acceleration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(military_ooda_loop_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(military_ooda_loop_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from technologically disadvantaged nations through forced adoption of unreliable automation, and from strategic stability through compressed decision windows. However, it is not maximum extraction (which would be >0.70) because genuine technological improvement also produces real coordination benefits — faster decision-making does improve tactical responsiveness when system reliability is adequate. The rise from 0.35 to 0.58 over 20 years reflects maturation of automation systems from experimental to operational deployment, increasing the real stakes. Suppression (0.62): Moderate-high. Technologically disadvantaged nations face serious barriers: procurement costs are high, domestic technology development is slow, and falling behind creates strategic vulnerability. But suppression is not total — alternative strategies exist (defensive doctrine, alliances, decentralized command structures) that avoid the arms race entirely. Theater ratio (0.38): Low-moderate. The constraint's primary function is real (improved decision-making speed does matter in conflict), not performative. Theater ratio is not zero because military organizations engage in doctrinal presentation and justification of automation that exceeds actual capability, but the underlying competitive pressure is genuine.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. Technologically advanced militaries see rope — pure coordination benefit from faster decision cycles that their systems enable. Defense contractors see rope — market coordination perfectly aligned with their business model. Technologically disadvantaged militaries see snare — forced choice between adopting unreliable automation or accepting strategic defeat. Strategic stability sees snare — escalation risk is involuntary and unavoidable. Arms control sees tangled rope — genuine tension between faster decision transparency and compressed verification windows. The analytical observer risks seeing mountain — OODA acceleration as inevitable consequence of information technology — but the structural data reveals this as naturalization of a contingent competitive dynamic. If nations agreed (via treaty) to limit automation or impose verification timelines exceeding OODA cycles, the constraint would change fundamentally. Its appearance as 'natural law' is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary beneficiaries (technologically advanced militaries and defense contractors) experience low or negative effective extraction because they control the escalation path and profit from acceleration. The primary victims (disadvantaged militaries and strategic stability) experience high extraction because they absorb the costs of forced automation and escalation risk. Mid-tier military powers experience constrained exit and mixed benefits, producing moderate extraction. The directionality derivation depends entirely on structural position: those who can arbitrage out (technologically advanced) have low d; those who are trapped (disadvantaged nations, abstract collective goods) have high d. No override values are needed — the derivation chain from beneficiary/victim declarations produces accurate d values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved by recognizing that OODA loop acceleration is NOT pure extraction (snare) from the perspective of advanced militaries and contractors — it genuinely coordinates improved tactical responsiveness. But it IS pure extraction from the perspective of disadvantaged militaries, which experience forced adoption of unreliable systems with no coordination benefit. The tangled rope classification (middle perspective, moderate power, constrained exit) correctly identifies that the same structural mechanism produces both coordination and extraction depending on position. The false summit (mountain perspective) reveals the naturalization risk: framing OODA acceleration as inevitable consequence of physics/technology obscures that it is a choice by technologically advanced nations to compress decision cycles beyond verified safe thresholds, extracting from those unable to keep pace. The constraint persists because technologically advanced nations benefit and have power to enforce the terms. It could be broken by treaty (mutual agreement to limit automation and extend verification timelines), but the treaty would require technologically advanced parties to accept strategic disadvantage. This is the classic tangled rope structure: coordination benefit for one side, extraction burden for the other, maintained through power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_judgment_vs_automation_threshold,
    'At what decision-cycle compression point does human judgment become impossible and must be replaced by automated threat assessment?',
    'Empirical testing of decision latencies: measurement of minimum time required for human commanders to evaluate automated recommendations vs. time required for decision automation itself. Comparison with actual engagement timescales in recent conflicts.',
    'If threshold is reached only at sub-second cycles: human judgment remains viable. If threshold is already breached: the constraint generates genuine escalation risk independent of policy choice. Classification shifts from contingent institutional choice to physical/cognitive limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_judgment_vs_automation_threshold, empirical, 'Cognitive threshold for human judgment in compressed decision cycles').

omega_variable(
    technological_lead_sustainability,
    'Can any military sustain technological lead in sensor/processing systems indefinitely, or does the lag-time for others to acquire similar technology make the constraint''s extraction mechanism temporary?',
    'Historical analysis of military technology diffusion timelines; assessment of whether advanced systems become commodity-grade within 5-10 years; evaluation of whether technological lead translates to sustained strategic advantage.',
    'If leads are temporary: constraint generates extraction only during transition periods. If leads are durable: OODA loop acceleration creates permanent power asymmetry. If diffusion accelerates: constraint weakens as gap closes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lead_sustainability, empirical, 'Sustainability of technological military leads').

omega_variable(
    escalation_risk_quantification,
    'Does OODA loop acceleration measurably increase accidental escalation risk, or does faster decision-making with better information actually reduce miscalculation?',
    'Game-theoretic modeling of conflict dynamics with different decision cycle times; analysis of near-miss incidents and their relationship to decision compression; comparison of escalation patterns in fast-cycle vs slow-cycle conflict scenarios.',
    'If acceleration increases risk: constraint becomes destructive even for the technologically advanced. If acceleration reduces risk: constraint provides genuine stability benefit. If effect is neutral: narrative about escalation risk becomes cover story for rent-seeking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_risk_quantification, empirical, 'Relationship between OODA loop speed and escalation risk').

omega_variable(
    verification_window_compression,
    'How much has OODA loop acceleration compressed the verification window available to arms control regimes, and is the compression structural or reversible through diplomatic agreement?',
    'Quantification of deployment timescales before/after acceleration initiatives; assessment of whether verification protocols have adapted proportionally; analysis of whether treaties can impose verification timelines that exceed OODA cycle requirements.',
    'If compression is structural: arms control becomes impossible. If compression is reversible: constraint is policy choice rather than technological inevitability. If regimes adapt: extraction mechanism is mediated by institutional design.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_window_compression, empirical, 'Arms control verification window compression').

omega_variable(
    automation_failure_modes,
    'What are the actual failure modes when automated threat assessment becomes responsible for engagement decisions, and how frequently do they occur in field conditions?',
    'Analysis of automated system performance in realistic conditions (sensor noise, spoofing, environmental factors); comparison of simulation performance vs field reliability; documentation of instances where automation made classification errors.',
    'If failure rates are low: automation is justified. If failure rates are significant: the constraint generates extraction through forcing acceptance of unacceptable risk. If failures are catastrophic but rare: the constraint creates tail risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_failure_modes, empirical, 'Failure modes and rates in automated military threat assessment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(military_ooda_loop_acceleration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ooda_tr_t0, military_ooda_loop_acceleration, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ooda_tr_t10, military_ooda_loop_acceleration, theater_ratio, 10, 0.32).
narrative_ontology:measurement(ooda_tr_t20, military_ooda_loop_acceleration, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ooda_tr_t5, military_ooda_loop_acceleration, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ooda_tr_t15, military_ooda_loop_acceleration, theater_ratio, 15, 0.35).

% Extraction over time
narrative_ontology:measurement(ooda_be_t0, military_ooda_loop_acceleration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ooda_be_t10, military_ooda_loop_acceleration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ooda_be_t20, military_ooda_loop_acceleration, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ooda_be_t5, military_ooda_loop_acceleration, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(ooda_be_t15, military_ooda_loop_acceleration, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(military_ooda_loop_acceleration, enforcement_mechanism).
narrative_ontology:affects_constraint(military_ooda_loop_acceleration, arms_control_verification_window).
narrative_ontology:affects_constraint(military_ooda_loop_acceleration, human_judgment_in_conflict).
narrative_ontology:affects_constraint(military_ooda_loop_acceleration, military_technology_diffusion).

% DUAL FORMULATION NOTE:
% OODA loop acceleration is upstream of specific arms control failures and tactical automation incidents. The constraint family includes: (1) OODA acceleration itself (this story, ε=0.58, tangled rope coordination+extraction mix), (2) arms control verification window compression (ε=0.72, downstream snare as verification becomes impossible), (3) human judgment atrophy in compressed cycles (ε=0.45, tangled rope as automation reduces decision-maker experience), (4) military technology diffusion lag (ε=0.35, rope as disadvantaged nations catch up over 10-15 years). Each has its own extractiveness and classification; together they form a constraint family where OODA acceleration is the causal root.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
