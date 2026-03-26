% ============================================================================
% CONSTRAINT STORY: new_start_expiration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_new_start_expiration, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: new_start_expiration
 *   human_readable: New START Treaty Expiration and Nuclear Constraint Collapse
 *   domain: geopolitical/nuclear_security
 *
 * SUMMARY:
 *   The New START treaty's expiration (February 2026 initial framework,
 *   extended deadline approach in 2035) creates a structural collapse of the
 *   primary verification mechanism constraining US and Russian strategic
 *   nuclear arsenals. For three decades, START agreements provided bilateral
 *   verification protocols, warhead caps, deployed missile limits, and data
 *   transparency. The expiration removes these institutional scaffolding,
 *   creating a constraint landscape where mutual suspicion replaces
 *   verification, and arms race dynamics replace negotiated limits. This
 *   constraint exhibits the snare classification globally (non-nuclear states
 *   have no exit options) while appearing as rope to the primary
 *   beneficiaries (US and Russian military establishments). The constraint
 *   demonstrates how the same structural phenomenon — absence of negotiated
 *   limits on nuclear forces — can classify as snare (for the powerless),
 *   rope (for the beneficiaries), tangled rope (for strategic planners),
 *   piton (for the degraded non-proliferation regime), and false mountain
 *   (for analysts tempted to naturalize anarchy). The theater ratio increase
 *   reflects growing performative compliance with non-binding
 *   confidence-building measures while actual strategic forces accelerate
 *   modernization.
 *
 * KEY AGENTS:
 *   - US Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — treaty expiration enables $1.5+ trillion nuclear modernization pipeline, ICBM replacement, SLBM conversion to MIRVs
 *   - Russian Military Establishment: Co-beneficiary (institutional/arbitrage) — justifies asymmetric modernization, hypersonic warhead development, reduces verification burden
 *   - Global Non-Nuclear States: Primary victims (powerless/trapped) — face unconstrained nuclear threat with zero negotiating power or exit options
 *   - Allied States and NATO Members: Secondary victims (moderate/constrained) — experience strategic vulnerability and reduced protection credibility
 *   - US and Russian Strategic Command: Mixed actors (powerful/constrained) — both benefit from clarity on maximum plausible forces but face mutual escalation risk
 *   - International Non-Proliferation Regime (NPT, IAEA): Institutional degradation (institutional/arbitrage) — maintains theatrical verification while enforcement capacity collapses
 *   - Analytical Observers: Risk of false summit (analytical/analytical) — tempted to naturalize constraint as immutable feature of anarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(new_start_expiration, 0.68).
domain_priors:suppression_score(new_start_expiration, 0.72).
domain_priors:theater_ratio(new_start_expiration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(new_start_expiration, extractiveness, 0.68).
narrative_ontology:constraint_metric(new_start_expiration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(new_start_expiration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(new_start_expiration, snare).
narrative_ontology:human_readable(new_start_expiration, "New START Treaty Expiration and Nuclear Constraint Collapse").
narrative_ontology:topic_domain(new_start_expiration, "geopolitical/nuclear_security").

domain_priors:requires_active_enforcement(new_start_expiration).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(new_start_expiration, us_military_industrial_complex).
narrative_ontology:constraint_beneficiary(new_start_expiration, russian_military_establishment).
narrative_ontology:constraint_victim(new_start_expiration, global_nuclear_stability).
narrative_ontology:constraint_victim(new_start_expiration, non_nuclear_states).
narrative_ontology:constraint_victim(new_start_expiration, civilian_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES (SNARE) — Nations without nuclear weapons and civilian populations worldwide cannot exit the constraint imposed by unconstrained US-Russia nuclear buildup. They are maximally extracted from (bearing existential risk) with zero alternatives. No verification mechanism, no enforcement option, no way to negotiate. Maximum d → maximum f(d) → maximum experienced extraction.
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED AND NON-ALIGNED STATES (SNARE) — Countries with security commitments to either power bloc face constrained exit: NATO members cannot meaningfully withdraw from US umbrella protection; non-aligned states face nuclear umbrella shopping with limited bargaining power. Suppression is total — no state can unilaterally halt the arms race. High extraction of strategic autonomy.
constraint_indexing:constraint_classification(new_start_expiration, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US MILITARY-INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary. Treaty expiration enables unrestricted nuclear modernization, expanded arsenal, and $1+ trillion modernization budgets. Experiences constraint collapse as coordination success: the removal of verification requirements and warhead caps solves their production and deployment goals. High arbitrage options (shift production, seek foreign contracts, lobby for expanded budgets). Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RUSSIAN MILITARY ESTABLISHMENT (ROPE) — Co-beneficiary. Treaty expiration provides strategic flexibility, reduces verification burdens, and justifies asymmetric modernization investments. Like the US counterpart, experiences constraint removal as enabling coordination: both powers can now pursue deterrence strategies without negotiated limits. Arbitrage options through arms sales, alliance building, hypersonic and MIRV development. Net beneficiary.
constraint_indexing:constraint_classification(new_start_expiration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: US AND RUSSIAN STRATEGIC COMMAND (TANGLED ROPE) — Military strategists see both coordination function and asymmetric extraction. Coordination function: the absence of verified limits creates mutual clarity about maximum plausible arsenals, enabling deterrence calculations without intelligence uncertainty. Extraction: strategic mobility increases for whoever modifies arsenals faster, creating arms race dynamics. Constrained exit — military planners cannot unilaterally commit to new limits without strategic risk. Requires active enforcement of doctrinal commitments.
constraint_indexing:constraint_classification(new_start_expiration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NPT AND IAEA (PITON) — The non-proliferation treaty framework persists as institutional inertia despite weakened enforcement capacity. Nuclear verification is now theatrical — IAEA inspections continue but lack enforcement when great powers ignore inspection findings. The regime's primary mechanism (great power modeling of restraint via START) has collapsed, yet the institutional infrastructure remains. Theater ratio high: ongoing inspections and treaty meetings performed even as the underlying constraint mechanism (great power commitment) has dissolved. Piton emerges from institutional inertia, not functional enforcement.
constraint_indexing:constraint_classification(new_start_expiration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (FALSE SUMMIT) — Risk of naturalizing constraint as 'inherent to great power dynamics.' Some analysts claim unconstrained nuclear competition is immutable law of anarchy: states lack central authority to enforce limits, so constraints always fail. This perspective risks false summit classification. However, structural data contradicts mountain classification: the constraint is contingent institutional arrangement (treaty design, verification protocols, political will), not a law of nature. START treaty existed for 30+ years, proving that negotiated limits ARE possible. The analytical observer must resist naturalization.
constraint_indexing:constraint_classification(new_start_expiration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(new_start_expiration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(new_start_expiration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(new_start_expiration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(new_start_expiration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(new_start_expiration, TR),
    TR >= 0.70.

:- end_tests(new_start_expiration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint's removal of verified limits on nuclear forces creates substantial asymmetric benefits for the two powers. The primary beneficiaries (US and Russian military establishments) capture strategic mobility, budget expansion, and deployment flexibility worth tens of billions in military advantage. The extraction is not total (0.90+) because both powers retain mutual vulnerability — neither can achieve decisive first-strike superiority. The trajectory shows extractiveness rising from 0.35 (under START verification) to 0.68 (post-expiration) as arms race dynamics accelerate. Suppression (0.72): High. For non-nuclear states, suppression is total — no alternative exists to living under the threat of unconstrained nuclear arsenals. For allied states, suppression is severe — NATO members have constrained exit (cannot withdraw from US protection) and cannot negotiate independent constraints. For Russia, suppression comes from the need to match US modernization pace. The suppression value reflects genuine barriers to exiting the constraint: proliferation is economically irrational (high cost); deterrence without nuclear weapons is unreliable; unilateral disarmament is suicidal. Theater ratio (0.58): Moderate-high. The non-proliferation regime persists as institutional performance even as its core mechanism (great power modeling) has collapsed. IAEA inspections continue, NPT review conferences meet, confidence-building statements are issued — all performative given the absence of meaningful enforcement capacity. The theater ratio is lower than piton levels (0.70+) because some functional verification still occurs through national technical means; the theater is not yet complete inversion. The rise from 0.42 to 0.58 reflects increasing gap between performative compliance and actual strategic behavior.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across structural positions. The US military-industrial complex sees constraint removal as coordination success (rope) — they solve the problem of modernizing arsenals without negotiation burdens. Russian military sees similar coordination benefit. But the global non-nuclear majority sees pure extraction (snare) — they bear existential risk with zero alternatives. Allied states occupy the middle (tangled rope) — they benefit from security guarantees but lose deterrence stability guarantees. Strategic commanders occupy a paradoxical position: they experience both coordination (clarity about maximum forces simplifies deterrence calculations) and extraction (arms race dynamics create mutual escalation trap). The piton perspective reveals institutional inertia: the non-proliferation regime performs verification theater despite losing enforcement leverage. The analytical observer risks false summit by naturalizing what is actually a contingent institutional choice (bilateral verification model can be replaced with multilateral or different trust architectures). The perspectival gap is the entire constraint's analytical content: it demonstrates how indexical classification reveals hidden structural conflicts that binary 'is this good/bad' framings miss.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. Beneficiaries (US and Russian military establishments) have low d values (0.15-0.25) derived from arbitrage exit options and beneficiary status — they can escape the constraint's burden while capturing its benefits. Constrained actors (allied states, non-aligned nations) have moderate d values (0.50-0.65) derived from constrained exit options and victim status — they cannot freely exit but have some negotiating leverage. Trapped actors (non-nuclear states, civilian populations) have high d values (0.85-0.95) derived from trapped exit options and total victim status — they bear maximum extraction with zero alternatives. Strategic commanders occupy the highest complexity: they are technically beneficiaries of clarity but operationally trapped in arms race dynamics, producing moderate d values (0.45-0.55). The engine derives d from these structural declarations and applies f(d) sigmoid to produce experienced extractiveness chi. Beneficiaries with arbitrage options experience low χ; trapped agents experience high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the mandatrophy trap by distinguishing between the two primary beneficiaries' genuine coordination function and the global victims' extraction. The temptation to mislabel: 'Nuclear deterrence is coordination between the superpowers — START was a cooperative agreement.' The trap: this framing naturalizes deterrence as mutually beneficial and overlooks the massive extraction from non-nuclear states. The resolution: recognize that coordination at one scale (bilateral US-Russia deterrence management) overlaps with extraction at another scale (global nuclear asymmetry). The constraint is snare-primary (for the majority) with rope-secondary (for the beneficiaries). The piton perspective (institutional inertia of the NPT) prevents false claim that the constraint is a mountain (natural law of anarchy). The tangled rope perspective (for strategic command) prevents false claim that coordination completely explains the constraint. The analytical observer perspective identifies the false summit risk — analysts tempted to say 'unconstrained competition is inevitable in anarchic systems' are naturalizing what is actually a reversible institutional choice. The mandatrophy is resolved by showing that all classifications are correct from their respective perspectives, and the engine's job is to measure which perspective each agent occupies, not to choose a single classification for the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_collapse_reversibility,
    'Can verification mechanisms and data-sharing protocols be rapidly re-established if political will returns, or has institutional knowledge degraded irreversibly?',
    'Analysis of IAEA and NTM (national technical means) inspection capacity; comparison with Cold War re-engagement models; assessment of data continuity from previous monitoring regimes',
    'If reversible: constraint could be re-established through new treaty with lower negotiation costs. If irreversible: any future constraint faces higher startup costs and lower initial confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_collapse_reversibility, empirical, 'Whether verification protocols can be restored post-collapse').

omega_variable(
    breakout_detection_timing,
    'What is the detectability window for unilateral strategic deployment (MIRV conversion, SLBM rearmament, warhead production acceleration)? Can the other power respond within strategically relevant timeframes?',
    'Technical assessment of NTM detection latency; historical timelines for similar transitions; comparison with Cold War crisis-response decision cycles',
    'If detection window < 12 months: hair-trigger escalation risk intensifies. If detection window > 24 months: constrained arms race possible without immediate crisis. This determines whether the constraint emerges naturally as a crisis machine (mountain framing) or as a behavioral cage requiring enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(breakout_detection_timing, empirical, 'Time window for detecting strategic breakout').

omega_variable(
    deterrence_stability_cliff,
    'Does nuclear deterrence exhibit a discontinuous transition (phase change) as verification confidence degrades, or does stability degrade gradually?',
    'Game-theoretic analysis of second-strike credibility under uncertainty; historical case studies of nuclear crises; comparison with China and India nuclear threshold behavior',
    'If discontinuous: stability collapses catastrophically at some ε threshold (mountain framing). If gradual: escalation risk increases linearly with arsenal asymmetry (snare framing). This determines whether the constraint is ''naturally'' unstable or ''made'' unstable by choices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_stability_cliff, conceptual, 'Whether deterrence stability exhibits phase transition').

omega_variable(
    multipolar_constraint_emergence,
    'Will China and other nuclear powers negotiate new multilateral constraints to replace US-Russia START, or will the collapse catalyze unconstrained multipolar competition?',
    'Diplomatic track analysis; statements from Chinese, French, British, Indian strategic communities; feasibility assessments of N-party verification protocols',
    'If multilateral constraints emerge: the constraint landscape shifts from bilateral snare to n-party tangled rope. If unconstrained competition: snare classification deepens globally. This determines whether the constraint is terminal or transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolar_constraint_emergence, preference, 'Likelihood of multilateral constraint replacement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(new_start_expiration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nstart_tr_t0, new_start_expiration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nstart_tr_t5, new_start_expiration, theater_ratio, 5, 0.5).
narrative_ontology:measurement(nstart_tr_t10, new_start_expiration, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(nstart_be_t0, new_start_expiration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nstart_be_t5, new_start_expiration, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(nstart_be_t10, new_start_expiration, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(new_start_expiration, enforcement_mechanism).
narrative_ontology:affects_constraint(new_start_expiration, nuclear_proliferation_incentive_cascade).
narrative_ontology:affects_constraint(new_start_expiration, hypersonic_warhead_verification_gap).
narrative_ontology:affects_constraint(new_start_expiration, global_strategic_stability_index).

% DUAL FORMULATION NOTE:
% The New START expiration decomposes into distinct constraints at different scales: (1) bilateral US-Russia deterrence management (institutional level, potentially rope), (2) global non-nuclear state security (global level, clearly snare), (3) non-proliferation enforcement capacity (institutional degradation, piton). These are linked via affects_constraints: the bilateral coordination failure downstream affects global proliferation incentives and NPT credibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(new_start_expiration, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
