% ============================================================================
% CONSTRAINT STORY: reputational_cascade_failure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reputational_cascade_failure, []).

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
 *   constraint_id: reputational_cascade_failure
 *   human_readable: The Social Liquidity Trap
 *   domain: social/economic/informational
 *
 * SUMMARY:
 *   The social liquidity trap is a self-reinforcing reputational cascade
 *   triggered by unverified negative information that spreads faster than
 *   correction or verification can occur. Once negative signal enters a
 *   networked reputation system, individual agents face a rational defection
 *   incentive: cutting ties with the target is low-cost social signaling,
 *   while defending the target carries reputational risk. This creates a
 *   coordination failure where individually rational choices (defect)
 *   collectively destroy information integrity. The constraint exhibits
 *   tangled-rope structure: it combines coordination function (networks
 *   enable reputation aggregation and trust) with extraction mechanism
 *   (asymmetric information amplification, suppression of nuance, and
 *   publicity traps that prevent credible refutation). The constraint's
 *   theater ratio (0.64) reflects that much of the activity in reputation
 *   systems is performative signaling rather than substantive verification —
 *   platforms optimize for engagement rather than accuracy, users perform
 *   trust rather than verify competence, and institutions maintain
 *   credentialing theater despite low verification capacity. The base
 *   extractiveness (0.58) reflects moderate but significant asymmetry: the
 *   initial claim spreaders capture attention and engagement value while the
 *   target and epistemic commons bear concentration costs and reputational
 *   contamination.
 *
 * KEY AGENTS:
 *   - Reputation Target: Primary victim (powerless/trapped) — cannot exit or refute without amplifying the claim; maximum extraction
 *   - Epistemic Commons: Secondary victim (moderate/trapped) — reputation signal fidelity declines; abstract collective with no agency or exit
 *   - Network Participants: Secondary actors (moderate/mobile) — face coordination failure; rational individual defection damages collective reputation infrastructure
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — designs for engagement; viral cascades drive metrics; benefits from negative content amplification
 *   - Initial Claim Spreaders: Primary beneficiary (institutional/arbitrage) — capture initial attention and credibility from breaking story; enjoy status boost from 'knowing first'
 *   - Institutional Reputation Framework: Organized agents (organized/constrained) — building verification, credential, and legal alternative pathways to replace viral reputation (scaffold logic)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform design choices and human cognitive limits as inherent laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reputational_cascade_failure, 0.58).
domain_priors:suppression_score(reputational_cascade_failure, 0.68).
domain_priors:theater_ratio(reputational_cascade_failure, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reputational_cascade_failure, extractiveness, 0.58).
narrative_ontology:constraint_metric(reputational_cascade_failure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(reputational_cascade_failure, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reputational_cascade_failure, tangled_rope).
narrative_ontology:human_readable(reputational_cascade_failure, "The Social Liquidity Trap").
narrative_ontology:topic_domain(reputational_cascade_failure, "social/economic/informational").

domain_priors:requires_active_enforcement(reputational_cascade_failure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, initial_claim_spreaders).
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, attention_capture_mechanisms).
narrative_ontology:constraint_victim(reputational_cascade_failure, reputation_target).
narrative_ontology:constraint_victim(reputational_cascade_failure, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPUTATION TARGET (SNARE) — Once unverified negative information enters circulation, the target faces maximum extraction with no exit. Social disinvestment is self-reinforcing: each defector increases the incentive for others to defect (coordination failure). The target cannot credibly communicate refutation without amplifying the claim further (publicity trap). d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — Reputation systems depend on reliable signal transfer. Cascade failures corrupt the epistemic substrate: true information and false information become indistinguishable. The commons has no agent to defend it and no way to exit the contamination. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: NETWORK PARTICIPANT (TANGLED ROPE) — Individual agents face mixed incentives. Coordination benefit: social networks solve collective action (trust, reputation aggregation). Extraction cost: rational defection when negative signal appears (cutoff cost is low). Suppression of counterargument is high — contrarians face social cost, alternative framings are filtered. d≈0.65, f(d)≈1.02, σ=1.2 → χ≈0.61.
constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Social platforms coordinate information flow and reputation aggregation. They experience the constraint as a coordination solution: viral negativity is high-engagement content. The platform benefits from the cascade mechanism (amplification, metric capture). Suppression is built into algorithmic feed (negative content outranks corrections). d≈0.10, f(d)≈0.08, σ=1.2 → χ≈0.04. Net beneficiary — effective extraction negative.
constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL REPUTATION FRAMEWORK (SCAFFOLD) — Institutional solutions (verification requirements, editorial oversight, credential systems, legal recourse) are building alternative reputation pathways that bypass viral cascade. These frameworks have high activation cost but low extraction. Theater ratio is declining as blockchain-based reputation, decentralized review, and cryptographic credential verification mature. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.35. Sunset clause: as institutional alternatives mature (estimated 10-15 years), the viral cascade loses force.
constraint_indexing:constraint_classification(reputational_cascade_failure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY REPUTATION INSTITUTION (PITON) — Traditional reputation systems (credentials, memberships, endorsements, vetting boards) persist through institutional inertia despite low functional verification capacity. Theater ratio=0.64 reflects that credentialing is substantially performative in the face of information velocity — credentials lag reality, verification is costly and stale. The institution maintains the ritual (gating, accreditation) without enforcement power in attention economy. χ is low because arbitrage path (leaving system and using viral signals instead) is increasingly viable.
constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, reputational asymmetry is inherent to social information: true information requires effort to verify, false information spreads freely. This perspective risks seeing the cascade as an immutable law of information ecology. However, the structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts deep mountain classification. The engine's false summit detector will flag that the 'inherent asymmetry' framing naturalizes contingent platform design (algorithmic amplification, engagement optimization) that could be otherwise.
constraint_indexing:constraint_classification(reputational_cascade_failure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputational_cascade_failure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reputational_cascade_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reputational_cascade_failure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reputational_cascade_failure, TR),
    TR >= 0.70.

:- end_tests(reputational_cascade_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint combines real coordination failure (network reputation mechanisms are genuinely useful) with significant extraction asymmetry (unverified information spreads faster than truth, making refutation self-defeating; platform algorithmic amplification favors viral negativity). The value reflects that extraction is not total but substantial and directional. Over the measurement interval, extractiveness rises from 0.32 to 0.58 as cascade acceleration increases (positive feedback in social disinvestment). Suppression (0.68): High. Counterargument suppression occurs through multiple channels: platform algorithm demotion of replies/quotes relative to retweets, social cost of public disagreement, confirmation bias filtering in feed selection, and the loss of nuance in cascading transmission (complex argument reduces to meme). However, suppression is not total — some alternative narratives emerge in secondary networks and institutional channels. Theater ratio (0.64): Moderate-high, and rising. Much reputation system activity is performative: public credentialing without verification, status signaling through network affiliation, institutional vetting that cannot track information velocity. As the cascade accelerates, theater increases because users perform protective distancing (public condemnation of target) rather than invest in actual verification. At t=6, the platform ecosystem is substantially theatrical — the game is played for audience rather than for truth.
 *
 * PERSPECTIVAL GAP:
 *   The reputational cascade demonstrates sharp perspectival divergence. The reputation target (Snare) sees total extraction and no exit. The network participant (Tangled Rope) sees mixed incentives but experiences the pressure toward defection as overwhelming. The platform operator (Rope) sees a functioning coordination mechanism — their metrics are working perfectly. The institutional reputation framework (Scaffold) sees a temporary problem being solved by alternative structures. The epistemic commons (Snare) suffers silent damage with no advocate. The legacy institution (Piton) continues credentialing theater while watching its authority erode. The analytical observer (Mountain attempt) risks endorsing the 'natural law' framing of platform designers and neuroscientists. The constraint's true type is Tangled Rope because it has both coordination function (reputation aggregation solves collective action in trust) and extraction mechanism (asymmetric information, suppression of nuance, publicity trap). The snare perspectives are real — the target and commons are extractively victimized — but they coexist with genuine coordination benefits that the rope and scaffold perspectives capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Reputation Target: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot exit or refute. Epistemic Commons: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction — suffers contamination with no defense. Network Participant: Victim/Mixed + mobile → d≈0.65, f(d)≈1.02. Moderate-high extraction but agent has some exit capacity (can refuse to defect, can seek alternative networks). Platform Operator: Beneficiary + arbitrage → d≈0.10, f(d)≈0.08. Net beneficiary — can exit platform reputation system entirely; benefits from cascade dynamics. Initial Claim Spreaders: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Net beneficiary — capture status from 'breaking' story; can move on to next story. Institutional Reputation Framework: Organized + constrained → d≈0.45, f(d)≈0.48. Low-moderate extraction; organized agents see exit path via alternative infrastructure. Legacy Institution: Institutional + arbitrage → d≈0.10, f(d)≈0.08. Low extraction due to arbitrage (users leaving for platforms); piton classification comes from theater gate.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The reputational cascade resolves the mandatrophy by demonstrating that the constraint is NOT pure extraction (Snare) despite having victimized agents. The coordination function is genuine: networks aggregate reputation and solve the trust problem better than individual verification could. The extraction is also genuine: asymmetric information, algorithmic amplification, and publicity traps transfer value from target and commons to platform and claim spreaders. The constraint is Tangled Rope precisely because both functions operate simultaneously. Eliminating the coordination function would destroy reputation systems entirely — the solution is not to shut down networks but to reduce the extraction through redesign (algorithm transparency, correction amplification, verification-first ranking). The scaffold perspective confirms that institutional alternatives (blockchain reputation, decentralized review, legal recourse) are maturing as exit paths. The false summit detector flags the 'natural law' perspective: the cascade is not inherent to reputation — it is contingent on specific platform design choices (engagement optimization, algorithmic amplification, loss of nuance in cascading). The constraint's mandatrophy is resolved by recognizing that the same structural mechanism (high-speed information diffusion in networks) produces coordination benefit and extraction cost simultaneously. Policy levers exist (algorithm design, platform liability, institutional credentialing) that could shift the χ value without destroying the rope function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_lag_threshold,
    'What threshold for claim complexity and verification cost distinguishes legitimate reputation signal lag from extractive cascade amplification?',
    'Empirical comparison of verification timelines for true vs false claims across domains; correlation between claim complexity and replication success rates',
    'If threshold is low (simple claims): cascade indicates strong network signaling efficiency (Rope). If threshold is high (complex claims): cascade indicates extraction mechanism (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verifiability_lag_threshold, empirical, 'Verification lag threshold for reputation claims').

omega_variable(
    algorithmic_amplification_necessity,
    'Does the cascade occur primarily due to human cognitive biases (confirmation bias, availability heuristic) or does algorithmic amplification (engagement optimization, filter bubbles) drive the self-reinforcement?',
    'A/B testing platforms with vs without algorithmic amplification; comparison of cascade dynamics on algorithm-free vs algorithm-optimized networks',
    'If human bias dominant: cascade is structural property of any reputation system (Mountain or inevitable Snare). If algorithmic dominant: cascade is design choice (Tangled Rope); sunsets possible via algorithm redesign (Scaffold path viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_amplification_necessity, empirical, 'Role of algorithmic amplification vs cognitive bias in cascade dynamics').

omega_variable(
    counterargument_suppression_mechanism,
    'Is suppression of counterargument primarily due to network topology (retweets>quotes, loss of nuance), platform design (shadowbanning, demotion of corrections), or social psychology (costly disagreement)?',
    'Content analysis of correction ratios; measurement of visibility loss for counterarguments; experimental manipulation of platform affordances for dispute framing',
    'If topology: institutional/scaffold solutions (federated networks, quote-first design) reduce suppression (Rope path). If platform: regulatory intervention or algorithm change reduces extraction (Snare→Tangled Rope). If psychology: suppression is quasi-irreducible (Mountain-like component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterargument_suppression_mechanism, empirical, 'Mechanism driving suppression of counterarguments').

omega_variable(
    exit_velocity_from_cascade,
    'Can a reputation target credibly exit a cascade via public response, institutional verification, or network isolation without amplifying the original claim?',
    'Historical case analysis of successful reputation recovery; measurement of information decay vs persistence across platforms and time horizons',
    'If exit is possible: target is not fully trapped (exit_options→constrained rather than trapped; Tangled Rope instead of Snare). If exit is impossible: target is trapped by publicity paradox (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_velocity_from_cascade, empirical, 'Ability of reputation target to credibly exit cascade').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reputational_cascade_failure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(repcf_tr_t0, reputational_cascade_failure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(repcf_tr_t3, reputational_cascade_failure, theater_ratio, 3, 0.51).
narrative_ontology:measurement(repcf_tr_t6, reputational_cascade_failure, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(repcf_be_t0, reputational_cascade_failure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(repcf_be_t3, reputational_cascade_failure, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(repcf_be_t6, reputational_cascade_failure, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reputational_cascade_failure, information_standard).
narrative_ontology:affects_constraint(reputational_cascade_failure, platform_engagement_optimization).
narrative_ontology:affects_constraint(reputational_cascade_failure, institutional_credentialing_decay).
narrative_ontology:affects_constraint(reputational_cascade_failure, truth_decay_asymmetry).

% DUAL FORMULATION NOTE:
% The reputational cascade is downstream of platform design (engagement optimization) and upstream of institutional trust decay. Separate constraint stories model the platform's algorithmic amplification mechanism (ε higher, pure snare extraction) and the epistemic commons contamination (ε high, snare victim), while this story captures the hybrid tangled-rope structure of the cascade as a coordination-extraction hybrid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reputational_cascade_failure, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
