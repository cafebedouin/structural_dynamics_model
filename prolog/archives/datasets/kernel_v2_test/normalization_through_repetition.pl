% ============================================================================
% CONSTRAINT STORY: normalization_through_repetition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_normalization_through_repetition, []).

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
 *   constraint_id: normalization_through_repetition
 *   human_readable: Normalization Through Repetition in Political Discourse
 *   domain: political_philosophy/rhetorical_analysis/ideological_discourse
 *
 * SUMMARY:
 *   Normalization through repetition exploits a fundamental feature of human
 *   epistemic calibration: we use frequency of assertion as a heuristic for
 *   consensus and acceptability. When claims are repeated without challenge,
 *   observers—especially passive lurkers who consume discourse without
 *   participating—update their sense of what is normal, acceptable, or widely
 *   believed. This mechanism has a genuine coordination function: shared
 *   vocabulary and common reference points require repetition to stabilize.
 *   But it also enables asymmetric extraction when bad faith actors
 *   coordinate to repeat claims faster than correction can propagate,
 *   shifting the Overton window without persuasion. The constraint exhibits
 *   rising extractiveness and suppression over the measurement interval
 *   (2010-2022) as social media platforms amplified repetition effects
 *   through algorithmic recommendation and as coordinated inauthentic
 *   behavior became more sophisticated. Theater ratio is moderate and rising:
 *   some discourse engagement is genuine deliberation, but an increasing
 *   fraction is performative repetition designed to manipulate calibration
 *   rather than exchange ideas.
 *
 * KEY AGENTS:
 *   - Lurker Observers: Primary victims (powerless/trapped) — passive consumers whose epistemic calibration is hijacked by frequency heuristics; cannot distinguish coordinated repetition from genuine consensus; no voice to challenge
 *   - Good Faith Participants: Secondary victims (moderate/constrained) — active participants who benefit from coordination (shared vocabulary) but bear extraction cost when baseline assumptions shift; can challenge but face asymmetric effort and social cost
 *   - Bad Faith Actors: Primary beneficiaries (institutional/arbitrage) — coordinated amplification networks that exploit repetition mechanism to shift norms without persuasion; experience constraint as pure coordination tool
 *   - Platform Moderators: Mixed position (institutional/constrained) — benefit from engagement metrics driven by repetition but bear reputational cost when normalization enables extremism; constrained by business model
 *   - Counter-Narrative Coalition: Organized resistance (organized/mobile) — fact-checkers, media literacy educators, platform safety teams building alternative verification pathways with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both legitimate coordination function (norm formation requires repetition) and extractive overlay (coordinated manipulation of calibration)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(normalization_through_repetition, 0.58).
domain_priors:suppression_score(normalization_through_repetition, 0.62).
domain_priors:theater_ratio(normalization_through_repetition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(normalization_through_repetition, extractiveness, 0.58).
narrative_ontology:constraint_metric(normalization_through_repetition, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(normalization_through_repetition, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(normalization_through_repetition, tangled_rope).
narrative_ontology:human_readable(normalization_through_repetition, "Normalization Through Repetition in Political Discourse").
narrative_ontology:topic_domain(normalization_through_repetition, "political_philosophy/rhetorical_analysis/ideological_discourse").

domain_priors:requires_active_enforcement(normalization_through_repetition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(normalization_through_repetition, bad_faith_actors).
narrative_ontology:constraint_beneficiary(normalization_through_repetition, coordinated_amplification_networks).
narrative_ontology:constraint_victim(normalization_through_repetition, epistemic_commons).
narrative_ontology:constraint_victim(normalization_through_repetition, good_faith_participants).
narrative_ontology:constraint_victim(normalization_through_repetition, lurker_observers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LURKER OBSERVER (SNARE) — Passive consumers of discourse who calibrate their sense of what is normal/acceptable based on frequency of assertion. Cannot distinguish between genuine consensus and coordinated repetition. No voice to challenge, no exit from the information environment. Maximum extraction: their epistemic calibration is hijacked without their awareness or consent.
constraint_indexing:constraint_classification(normalization_through_repetition, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GOOD FAITH PARTICIPANT (TANGLED ROPE) — Active participants who benefit from discourse coordination (shared vocabulary, common reference points) but bear extraction cost when repetition-without-challenge shifts baseline assumptions. Can challenge claims but face social cost and asymmetric effort (debunking requires more energy than assertion). Constrained exit: can leave specific forums but not the broader discourse environment.
constraint_indexing:constraint_classification(normalization_through_repetition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BAD FAITH ACTOR (ROPE) — Coordinated networks that benefit from the repetition mechanism. Experience the constraint as pure coordination: synchronized messaging creates the appearance of consensus without requiring actual persuasion. Net beneficiary with arbitrage exit: can shift to new claims when old ones are challenged, or exit to different platforms.
constraint_indexing:constraint_classification(normalization_through_repetition, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNTER-NARRATIVE COALITION (SCAFFOLD) — Organized fact-checking networks, media literacy educators, and platform moderation teams building alternative verification pathways. See the normalization mechanism as temporary: distributed verification, context labels, and algorithmic de-amplification are creating structural resistance to repetition-based manipulation. Sunset logic: as media literacy improves and platforms implement friction for coordinated inauthentic behavior, the pure repetition strategy loses effectiveness.
constraint_indexing:constraint_classification(normalization_through_repetition, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM MODERATOR (TANGLED ROPE) — Institutional actors who benefit from engagement (repetition drives activity) but bear reputational cost when normalization enables extremism. Constrained by business model: cannot fully suppress repetition without reducing engagement metrics. Mixed experience: coordination function (enabling discourse) coexists with extraction (platform becomes vector for manipulation).
constraint_indexing:constraint_classification(normalization_through_repetition, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, some degree of repetition-based norm formation is inherent to language and culture (genuine coordination function). But the constraint also enables asymmetric extraction when bad faith actors exploit the mechanism faster than correction can propagate. The analytical view recognizes both the legitimate coordination role (shared vocabulary requires repetition) and the extractive overlay (coordinated manipulation of calibration).
constraint_indexing:constraint_classification(normalization_through_repetition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(normalization_through_repetition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(normalization_through_repetition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(normalization_through_repetition, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(normalization_through_repetition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(normalization_through_repetition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Bad faith actors capture substantial benefit by shifting discourse norms without persuasion, while lurkers and good faith participants bear the cost of distorted calibration. The value reflects that extraction is significant but not total—some repetition serves legitimate coordination, and counter-narratives do propagate. Rising trajectory (0.35→0.58 over 12 years) reflects increasing sophistication of coordinated amplification and algorithmic amplification effects. Suppression (0.62): Moderate-high. Challenging normalized claims faces asymmetric cost: debunking requires more effort than assertion, challengers face social penalty (pile-ons, ostracism), and algorithmic feeds may not surface challenges to users who saw the original claims. Rising trajectory (0.40→0.62) reflects increasing structural barriers as claims become entrenched and as platform algorithms create filter bubbles. Theater ratio (0.48): Moderate. Roughly half of discourse engagement is performative repetition designed to manipulate calibration rather than genuine deliberation. Rising trajectory (0.30→0.48) reflects shift from organic discourse to coordinated messaging campaigns. The theater is not as high as pure astroturfing (which would be 0.7+) because substantial genuine engagement persists.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same repetition mechanism appears as pure coordination (rope) to beneficiaries, mixed coordination-extraction (tangled rope) to moderate participants and the analytical observer, pure extraction (snare) to powerless lurkers, and temporary problem with sunset (scaffold) to organized resistance. Bad faith actors see only the coordination function—synchronized messaging creates consensus appearance without persuasion cost. Lurkers see only extraction—their calibration is manipulated without their awareness. Good faith participants see both—they benefit from shared vocabulary but bear cost when repetition shifts norms. The analytical observer recognizes that some repetition is inherent to language and culture (legitimate coordination) while also seeing the extractive overlay when coordination is weaponized. The counter-narrative coalition sees a temporary problem being solved by media literacy and platform interventions. No single perspective captures the full structure—the presheaf over observation positions is the complete description.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the extraction flow. Lurker observers are full victims with trapped exit—they bear maximum extraction (high d) because their calibration is hijacked without awareness or consent, and they have no mechanism to exit the information environment or challenge claims. Good faith participants are partial victims with constrained exit—they experience moderate extraction (mid-range d) because they benefit from coordination (shared vocabulary) but bear cost when repetition shifts baselines; they can challenge but face asymmetric effort. Bad faith actors are full beneficiaries with arbitrage exit—they experience negative effective extraction (low d, inverted chi) because the constraint subsidizes their goals (norm-shifting without persuasion) and they can exit to new claims or platforms when challenged. Platform moderators are mixed—they benefit from engagement but bear reputational cost, producing mid-range d. The counter-narrative coalition has mobile exit and organized power, producing low d despite being structurally opposed to the extraction. The analytical observer has analytical exit and recognizes both coordination and extraction, producing mid-range d that reflects the mixed structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that tangled rope classification at the analytical level is structurally correct: the mechanism has both genuine coordination function (norm formation, shared vocabulary) and asymmetric extraction (manipulation of calibration by bad faith actors). The coordination function is not mere cover—repetition genuinely serves to stabilize shared meaning. But the extraction is also real—coordinated actors exploit the mechanism faster than correction propagates. The key structural feature is the challenge asymmetry: assertion is cheap, debunking is expensive, creating an exploitable gradient. The constraint is not a false summit (not a snare disguised as coordination) because the coordination function persists even when extraction is removed. It is not pure rope because identifiable victims (lurkers, epistemic commons) bear costs that beneficiaries do not. The tangled rope classification captures this irreducible duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    repetition_threshold_calibration,
    'What frequency of unchallenged assertion is sufficient to shift lurker perception of consensus? Does the threshold vary by claim type or community context?',
    'Experimental manipulation of assertion frequency in controlled discourse environments; measurement of lurker belief updating as function of repetition count and challenge rate',
    'If threshold is low (3-5 repetitions): normalization is highly efficient extraction mechanism. If threshold is high (20+ repetitions): mechanism requires sustained coordination and is more vulnerable to disruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repetition_threshold_calibration, empirical, 'Repetition frequency threshold for shifting perceived consensus').

omega_variable(
    challenge_asymmetry_magnitude,
    'How much more costly is challenging a normalized claim than making the original assertion? Does the asymmetry increase with claim entrenchment?',
    'Measurement of social cost (downvotes, pile-ons, ostracism) and cognitive cost (research burden, argumentation complexity) for challengers vs asserters across discourse contexts',
    'If asymmetry is severe (10x+ cost): suppression is structural and constraint is closer to snare. If asymmetry is moderate (2-3x cost): constraint remains tangled rope with genuine coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(challenge_asymmetry_magnitude, empirical, 'Cost asymmetry between assertion and challenge').

omega_variable(
    coordination_vs_manipulation_boundary,
    'Where is the boundary between legitimate norm formation through repetition (cultural transmission, language evolution) and extractive manipulation of calibration mechanisms?',
    'Conceptual analysis of intent, coordination structure, and feedback responsiveness. Empirical markers: presence of coordinated amplification networks, resistance to counter-evidence, strategic timing of repetition.',
    'If boundary is clear and detectable: can distinguish rope (legitimate coordination) from tangled rope (mixed) from snare (pure extraction). If boundary is fuzzy: classification depends heavily on observer perspective and intent attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_manipulation_boundary, conceptual, 'Boundary between legitimate norm formation and extractive manipulation').

omega_variable(
    platform_algorithm_amplification,
    'Do recommendation algorithms amplify repetition effects by creating filter bubbles where unchallenged claims circulate without encountering counter-narratives?',
    'Comparison of normalization rates in algorithmically-curated vs chronological feeds; measurement of cross-ideological exposure rates; A/B testing of algorithmic interventions',
    'If algorithms substantially amplify: platform design is a structural component of the extraction mechanism and platforms are partial beneficiaries. If algorithms are neutral or dampening: extraction is primarily social rather than technical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithm_amplification, empirical, 'Whether platform algorithms amplify repetition-based normalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(normalization_through_repetition, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norm_rep_theater_t0, normalization_through_repetition, theater_ratio, 0, 0.3).
narrative_ontology:measurement(norm_rep_theater_t3, normalization_through_repetition, theater_ratio, 3, 0.35).
narrative_ontology:measurement(norm_rep_theater_t6, normalization_through_repetition, theater_ratio, 6, 0.4).
narrative_ontology:measurement(norm_rep_theater_t9, normalization_through_repetition, theater_ratio, 9, 0.45).
narrative_ontology:measurement(norm_rep_theater_t12, normalization_through_repetition, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(norm_rep_extract_t0, normalization_through_repetition, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(norm_rep_extract_t3, normalization_through_repetition, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(norm_rep_extract_t6, normalization_through_repetition, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(norm_rep_extract_t9, normalization_through_repetition, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(norm_rep_extract_t12, normalization_through_repetition, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(norm_rep_suppress_t0, normalization_through_repetition, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(norm_rep_suppress_t3, normalization_through_repetition, suppression_requirement, 3, 0.48).
narrative_ontology:measurement(norm_rep_suppress_t6, normalization_through_repetition, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(norm_rep_suppress_t9, normalization_through_repetition, suppression_requirement, 9, 0.6).
narrative_ontology:measurement(norm_rep_suppress_t12, normalization_through_repetition, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(normalization_through_repetition, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of tribal_marker_vs_analytical_work: when discourse moves from analytical engagement to tribal signaling, repetition becomes more effective as a normalization mechanism because challenges are interpreted as outgroup hostility rather than epistemic correction. The upstream constraint creates the conditions under which repetition-without-challenge becomes structurally likely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
