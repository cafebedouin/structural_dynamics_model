% ============================================================================
% CONSTRAINT STORY: start_treaty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_start_treaty, []).

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
 *   constraint_id: start_treaty
 *   human_readable: START Treaty Expiration and Nuclear Arsenal Constraint Removal
 *   domain: political/military/geopolitics
 *
 * SUMMARY:
 *   The New START treaty (Strategic Arms Reduction Treaty), signed in 2010
 *   and set to expire in 2026, provides the only legally binding constraints
 *   on the deployed strategic nuclear arsenals of the United States and
 *   Russia. Its expiration removes, for the first time in half a century
 *   (since 1972 SALT I), any formal limit on the number of deployed
 *   intercontinental ballistic missiles, submarine-launched ballistic
 *   missiles, and deployed strategic bombers. The constraint operates at the
 *   intersection of strategic stability, non-proliferation legitimacy, and
 *   mutual vulnerability. The constraint exhibits the full range of indexical
 *   classification depending on observer position: nuclear weapons states
 *   perceive it as a coordination mechanism (Rope) that enables strategic
 *   predictability; non-nuclear states and the global stability commons
 *   experience it as an extraction mechanism they cannot resist (Snare); the
 *   non-proliferation regime sees it as hybrid coordination and asymmetric
 *   security advantage (Tangled Rope); arms control establishments maintain
 *   the treaty as a degraded ritual (Piton) whose verification function has
 *   atrophied while its symbolic function persists; and some strategic
 *   thinkers rationalize nuclear deterrence as immutable law (false
 *   Mountain). The constraint's extractiveness has increased over its
 *   interval (0.32 → 0.68) as confidence in its verification mechanisms has
 *   declined and as multipolar nuclear proliferation has complicated the
 *   bilateral framework. Theater ratio remains stable (0.58-0.62) because the
 *   treaty's operational activity has always been dominated by symbolic
 *   confidence-building (site inspections, data exchanges, notification
 *   protocols) rather than constraint enforcement.
 *
 * KEY AGENTS:
 *   - United States: Primary beneficiary (institutional/arbitrage) — captures strategic flexibility upon expiration; built credible deterrent under constraint; treaty expiration removes disadvantage
 *   - Russian Federation: Primary beneficiary (institutional/arbitrage) — similarly positioned; constraint removal enables response to perceived NATO encroachment
 *   - Non-Nuclear States and Global South: Primary victim (powerless/trapped) — dependent on strategic stability and non-proliferation regime legitimacy; lack mechanism to influence nuclear powers
 *   - Allied Nations (NATO, Japan, South Korea): Secondary victim (moderate/constrained) — nominally protected by extended deterrence; treaty expiration increases uncertainty about umbrella stability
 *   - Non-Proliferation Regime (IAEA, NPT signatories, Arms Control Advocates): Organized actor (organized/constrained) — benefits from treaty's role in legitimizing non-nuclear weapons state compliance; sees expiration as weakening leverage
 *   - Global Nuclear Stability and Accident Prevention: Abstract victim (powerless/trapped) — benefits from verification protocols and notification procedures; loses mechanisms for preventing unintended escalation
 *   - Strategic Deterrence Establishment: Institutional beneficiary (institutional/arbitrage) — maintains bureaucratic infrastructure for compliance; sees treaty expiration as freeing strategic planning from constraints
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a political choice as immutable strategic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(start_treaty, 0.68).
domain_priors:suppression_score(start_treaty, 0.75).
domain_priors:theater_ratio(start_treaty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(start_treaty, extractiveness, 0.68).
narrative_ontology:constraint_metric(start_treaty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(start_treaty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(start_treaty, snare).
narrative_ontology:human_readable(start_treaty, "START Treaty Expiration and Nuclear Arsenal Constraint Removal").
narrative_ontology:topic_domain(start_treaty, "political/military/geopolitics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(start_treaty, nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(start_treaty, military_industrial_complex).
narrative_ontology:constraint_victim(start_treaty, non_nuclear_states).
narrative_ontology:constraint_victim(start_treaty, global_nuclear_stability).
narrative_ontology:constraint_victim(start_treaty, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-NUCLEAR STATES (SNARE) — Trapped in a system where their security depends entirely on the restraint of nuclear powers. Treaty expiration removes the only binding constraint on arsenal expansion. They have no exit option and no mechanism to influence the constraint. d≈0.92, f(d)≈1.39, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(start_treaty, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS (SNARE) — NATO members and US allies are nominally protected by extended nuclear deterrence but have no control over the arsenals upon which their security depends. Treaty expiration increases uncertainty about arsenal sizes. Limited exit: some could pursue independent nuclear programs, but at enormous political and economic cost. d≈0.78, f(d)≈1.10, σ=1.1 → χ≈0.57.
constraint_indexing:constraint_classification(start_treaty, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: RUSSIAN FEDERATION (ROPE) — Benefits from treaty expiration by removing numerical limits on nuclear forces. Sees the constraint as a coordination mechanism for signaling relative strategic posture. Without the treaty, Russia has arbitrage: can maintain deterrence through uncertainty about actual arsenal size. d≈0.15, f(d)≈0.02, σ=1.1 → χ≈0.01. Nearly zero effective extraction — Russia is the beneficiary.
constraint_indexing:constraint_classification(start_treaty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: UNITED STATES (ROPE) — Benefits from treaty expiration by removing numerical constraints. Perceives the constraint as coordination for crisis stability and mutual transparency. Sees expiration as freeing strategic flexibility. d≈0.12, f(d)≈-0.06, σ=1.1 → χ≈-0.04. Negative effective extraction — US is the beneficiary.
constraint_indexing:constraint_classification(start_treaty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: NON-PROLIFERATION REGIME (TANGLED ROPE) — Organized actors (IAEA, NPT signatories, arms control advocates) see both a coordination function (START provided verification protocols, confidence-building measures that reduced accident risk) and asymmetric extraction (nuclear weapons states retain permanent security advantage despite non-proliferation commitments). Treaty expiration weakens both the coordination function and the legitimacy of the extraction. requires_active_enforcement = true; beneficiaries = [nuclear_weapons_states], victims = [non_nuclear_states]. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(start_treaty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GLOBAL NUCLEAR STABILITY (SNARE) — Abstract collective representing the shared interest in reducing accidental escalation risk and constraining arsenal growth. START provided verification, notification of launches, and mutual transparency. Expiration removes these mechanisms entirely. The stability commons has no voice and no exit. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.74.
constraint_indexing:constraint_classification(start_treaty, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From civilizational timescale, START treaty maintained a ritualized mutual surveillance system that was heavily theater: both sides maintained arsenals far beyond deterrence requirements; verification protocols were performative (not catching hidden warheads due to classified architecture); the treaty's functional role was marginal compared to its symbolic function. theater_ratio = 0.58 reflects that arms control verification (spot checks, data exchanges) comprises ~40-50% of the treaty's operational activity, while strategic stability depends on background conditions (survivable second-strike capability) that the treaty does not significantly alter. The piton perspective sees the treaty as an inertial institution: maintained by bureaucratic commitment and international norms, but with degraded functional verification. Expiration reveals that the performative role was the primary function all along.
constraint_indexing:constraint_classification(start_treaty, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: FALSE SUMMIT (MOUNTAIN FRAMING) — Some strategic thinkers frame nuclear deterrence and arms racing as natural law: that two competing powers with nuclear capability will inevitably expand arsenals absent binding constraint, and that any arms control is temporary theater. This perspective naturalizes what is actually a political choice. The claim 'nuclear deterrence requires unbound arsenals' is not a law of physics; it is a claim about strategic behavior under conditions of mistrust. The engine detects this as a false summit: extractiveness=0.68, suppression=0.75 contradict the mountain gates (ε ≤ 0.25, suppression ≤ 0.05). The mountain classification reveals the rationalization, not the reality.
constraint_indexing:constraint_classification(start_treaty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(start_treaty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(start_treaty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(start_treaty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(start_treaty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(start_treaty, TR),
    TR >= 0.70.

:- end_tests(start_treaty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High. The constraint's expiration asymmetrically benefits nuclear weapons states by removing formal limitations while leaving non-nuclear states exposed to the resulting instability. Non-nuclear states cannot negotiate equivalent bargaining power. The extractiveness increased from 0.32 at treaty inception (when both sides saw mutual benefit in verified stability) to 0.68 near expiration (when strategic drift and proliferation concerns undermined the coordination function). Suppression (0.75): Very High. Non-nuclear states cannot exit — they are locked into a system where their security depends entirely on the forbearance of nuclear powers. Even nations capable of developing nuclear weapons face international sanctions and diplomatic isolation if they do so. Verification protocols, now degrading, provided the only mechanism for monitoring compliance, and those mechanisms are no longer operative post-expiration. Theater Ratio (0.58): Moderate. The treaty's operational content comprises verification activities (on-site inspections, data exchanges, notification of launches), which are intrinsically performative — they verify declared forces but cannot detect hidden warheads or undeclared programs. ~60% of treaty activity is verification ritual; ~40% is genuine constraint enforcement (accounting, declarations, monitoring). The theater ratio has remained stable because the performative function was always central to the treaty's legitimacy, regardless of its functional sufficiency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full span of classification across eight distinct observer positions. The nuclear weapons states (US and Russia) see Rope — they experience the constraint as coordination that enables mutual deterrence and crisis stability, with genuine strategic benefits for both sides. Allied nations see Snare — they depend on US extended deterrence without influence over the umbrella. The non-proliferation regime sees Tangled Rope — it gains coordination benefits from the treaty's legitimization of non-nuclear status, but it also maintains the asymmetric extraction of permanent nuclear privilege for the five treaty-recognized weapons states. Arms control establishments see Piton — they maintain the institutional ritual of verification and compliance, but the functional role of verification has degraded as Russian invasions proceeded despite treaty compliance, and as cyber threats and strategic ambiguity have undermined transparency. The global stability commons sees Snare — the abstract collective interest in preventing accidental escalation and constraining arsenal growth has no voice and no exit mechanism. The analytical observer risks seeing Mountain — naturalizing nuclear deterrence dynamics as immutable law, which obscures the contingent political choices that enabled treaty creation and now enable treaty exit.
 *
 * DIRECTIONALITY LOGIC:
 *   United States and Russia: Beneficiary + arbitrage → d≈0.12-0.15, f(d)≈-0.06 to 0.02. Negative or near-zero effective extraction — both are net beneficiaries. Allied nations: Victim + constrained → d≈0.78, f(d)≈1.10. Significant extraction; these nations are exposed by decisions they cannot control. Non-nuclear states: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction; no exit mechanism. Non-proliferation regime: Mixed (organized/constrained) → d≈0.52, f(d)≈0.68. The regime acts as both monitor of the constraint and advocate for expansion of non-nuclear compliance, creating ambiguous positioning. Global stability commons: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction; the abstract collective has no representation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The classification as Snare is stable across the structural data. Base extractiveness (0.68) and suppression (0.75) both exceed the snare thresholds (ε ≥ 0.46, suppression ≥ 0.60). The chi formula χ = 0.68 × f(d) × σ(S) produces effective extraction ≥ 0.66 for the victim perspectives (d ≈ 0.78-0.95), confirming snare classification. The mandatrophy is resolved by recognizing that the constraint functions as a Snare FOR non-nuclear states and the stability commons, while functioning as a Rope (net positive) for the nuclear powers themselves. This is not a contradiction — it is the structural feature of any constraint that involves asymmetric power. The nuclear weapons states legitimately see the constraint as beneficial coordination (Rope); the non-nuclear states legitimately see the same constraint as extraction they cannot resist (Snare). No higher-order classification (e.g., 'is it really a snare or a rope?') can override this perspectival difference. The mandatrophy is fully resolved by the indexed classification system itself: different agents classify the same constraint differently because they occupy structurally different positions relative to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_sufficiency_threshold,
    'Did START treaty verification protocols actually constrain strategic behavior, or were they primarily symbolic confidence-building measures?',
    'Declassified intelligence assessments comparing detected violations vs actual arsenal compositions; analysis of whether verification uncertainty exceeded strategic uncertainties from other sources; post-expiration behavioral changes measured against pre-expiration baseline',
    'If verification was functionally sufficient: treaty expiration removes a real constraint on arms race, and the snare classification is stronger. If verification was theater: treaty expiration removes a symbolic constraint, and the piton perspective is primary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_sufficiency_threshold, empirical, 'Whether START verification protocols materially constrained strategic behavior').

omega_variable(
    nuclear_stability_multipolarity,
    'Can mutual deterrence stability between two nuclear powers be extended to a multipolar nuclear environment (US, Russia, China, India, Pakistan, Israel, North Korea, potentially France independently)?',
    'Game-theoretic analysis of n-party deterrence; empirical study of crisis stability in multipolar nuclear environments; comparison of bilateral vs multilateral arms control frameworks; historical analysis of near-misses in bipolar vs multipolar eras',
    'If stability is extendable: treaty expiration increases instability proportionally with number of poles. If multipolar deterrence is inherently unstable: treaty expiration may matter less than proliferation itself. Shapes whether constraint should decompose into separate stories for bipolar vs multipolar dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_stability_multipolarity, conceptual, 'Whether bilateral deterrence stability generalizes to multipolar nuclear systems').

omega_variable(
    escalation_ladder_discreteness,
    'Is there a discrete threshold above which nuclear arsenal size materially increases escalation risk, or is escalation risk continuous across all arsenal sizes?',
    'Analysis of first-strike vulnerability as a function of arsenal size and deployment posture; modeling of command-and-control failure modes at different inventory scales; empirical study of historical escalation incidents and their relationship to arsenal transparency',
    'If threshold exists: START constraints operated near critical threshold, and expiration has discontinuous effect (snare classification confirmed). If continuous: expiration is incremental, and tangled_rope perspectives gain weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(escalation_ladder_discreteness, empirical, 'Relationship between arsenal size and escalation risk').

omega_variable(
    breakout_first_mover_advantage,
    'In a post-START environment, how much strategic advantage accrues to the first nation to break out and expand its arsenal beyond declared levels?',
    'Game-theoretic modeling of breakout scenarios; analysis of detection lag and verification capacity without treaty; historical study of arms race dynamics when one side unilaterally abandoned constraints',
    'If first-mover advantage is large: creates prisoner''s dilemma incentive structure, transforming snare into a trap where even willing cooperators are forced to defect. If advantage is marginal: leaves room for multilateral negotiation or constraint reimposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breakout_first_mover_advantage, empirical, 'First-mover strategic advantage in post-treaty breakout scenarios').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(start_treaty, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(start_tr_t0, start_treaty, theater_ratio, 0, 0.62).
narrative_ontology:measurement(start_tr_t10, start_treaty, theater_ratio, 10, 0.6).
narrative_ontology:measurement(start_tr_t20, start_treaty, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(start_be_t0, start_treaty, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(start_be_t10, start_treaty, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(start_be_t20, start_treaty, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(start_treaty, enforcement_mechanism).
narrative_ontology:affects_constraint(start_treaty, nuclear_proliferation_incentive).
narrative_ontology:affects_constraint(start_treaty, extended_deterrence_credibility).
narrative_ontology:affects_constraint(start_treaty, crisis_stability_architecture).

% DUAL FORMULATION NOTE:
% START treaty expiration is a constraint family member related to broader nuclear governance constraints. The treaty itself is a coordination mechanism (Rope from the perspective of both nuclear powers), but its expiration creates a new constraint (this story) that affects downstream nuclear stability. The upstream constraint (the treaty as coordination) had ε≈0.05; the downstream constraint (post-expiration instability) has ε≈0.68. These are distinct stories: one describes the treaty's function while active, the other describes the structural consequences of its removal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
