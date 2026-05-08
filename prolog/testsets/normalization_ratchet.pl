% ============================================================================
% CONSTRAINT STORY: normalization_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_normalization_ratchet, []).

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
 *   constraint_id: normalization_ratchet
 *   human_readable: Normalization Ratchet in Public Discourse
 *   domain: political_philosophy/rhetoric/epistemology
 *
 * SUMMARY:
 *   The normalization ratchet describes how repetition of claims in public
 *   discourse shifts perceived consensus independent of actual agreement or
 *   evidence. The mechanism is structural: lurkers (passive observers) see
 *   claim frequency but not challenge frequency, leading to availability
 *   heuristic bias. Algorithmic amplification exacerbates this by
 *   prioritizing engagement (replies, quote-tweets) over accuracy, creating a
 *   feedback loop where controversial claims get more visibility. The
 *   constraint exhibits both genuine coordination function (public discourse
 *   requires some mechanism for surfacing claims and testing ideas) and
 *   asymmetric extraction (bad-faith actors can flood the zone at lower cost
 *   than good-faith actors can refute). The theater_ratio (0.58) reflects
 *   that much challenge activity is performative: debaters know they are
 *   unlikely to change the flooder's mind but engage anyway to signal to
 *   lurkers that the claim is contested. The extractiveness has increased
 *   over the interval (0.32 → 0.48) as platforms have optimized for
 *   engagement, lowering the cost of flooding and raising the cost of
 *   sustained rebuttal.
 *
 * KEY AGENTS:
 *   - Lurker Populations: Primary victim (powerless/trapped) — passive observers whose consensus perception is manipulated by claim frequency without awareness of challenge frequency
 *   - Epistemic Commons: Primary victim (powerless/identity_locked) — the shared resource of reliable public knowledge, degraded by signal-to-noise contamination
 *   - Good-Faith Debaters: Secondary victim (moderate/constrained) — participants who engage to test ideas but bear asymmetric exhaustion costs from repetitive refutation
 *   - Bad-Faith Flooders: Primary beneficiary (institutional/arbitrage) — actors who repeat claims to shift the Overton window, experiencing the constraint as a coordination tool
 *   - Platform Engagement Metrics: Primary beneficiary (institutional/arbitrage) — algorithmic systems that benefit from controversy loops and time-on-platform increases
 *   - Counter-Flooding Coalition: Organized agents (organized/mobile) — fact-checkers, community notes, moderation teams building structural countermeasures with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function and extractive mechanism; classification reflects mixed structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(normalization_ratchet, 0.48).
domain_priors:suppression_score(normalization_ratchet, 0.62).
domain_priors:theater_ratio(normalization_ratchet, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(normalization_ratchet, extractiveness, 0.48).
narrative_ontology:constraint_metric(normalization_ratchet, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(normalization_ratchet, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(normalization_ratchet, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(normalization_ratchet, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(normalization_ratchet, tangled_rope).
narrative_ontology:human_readable(normalization_ratchet, "Normalization Ratchet in Public Discourse").
narrative_ontology:topic_domain(normalization_ratchet, "political_philosophy/rhetoric/epistemology").

domain_priors:requires_active_enforcement(normalization_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(normalization_ratchet, bad_faith_flooders).
narrative_ontology:constraint_beneficiary(normalization_ratchet, platform_engagement_metrics).
narrative_ontology:constraint_victim(normalization_ratchet, epistemic_commons).
narrative_ontology:constraint_victim(normalization_ratchet, lurker_populations).
narrative_ontology:constraint_victim(normalization_ratchet, good_faith_debaters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LURKER POPULATION (SNARE) — Passive observers who consume discourse without participating. Trapped by information asymmetry: see high-frequency claims without seeing the ratio of claims to challenges. No exit from the perception shift because algorithmic feeds prioritize engagement over accuracy. Maximum extraction: their consensus perception is manipulated without their awareness or consent.
constraint_indexing:constraint_classification(normalization_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — The shared resource of reliable public knowledge. Identity-locked because the commons cannot 'exit' discourse — its function is constituted through participation. Experiences pure extraction: flooded with claims that shift perceived consensus independent of evidence, degrading the signal-to-noise ratio for all participants. High suppression: no mechanism to filter bad-faith repetition from good-faith inquiry at scale.
constraint_indexing:constraint_classification(normalization_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: GOOD-FAITH DEBATER (TANGLED ROPE) — Participants who engage to test ideas and refine understanding. Constrained by time and energy costs: refuting the same claim repeatedly is exhausting, but silence enables normalization. Benefits from the discourse ecosystem (idea refinement, coalition building) but also bears extraction (attention drain, burnout risk). Mixed experience: genuine coordination function exists alongside asymmetric cost.
constraint_indexing:constraint_classification(normalization_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BAD-FAITH FLOODER (ROPE) — Actors who repeat claims to shift the Overton window regardless of truth value. Arbitrage exit: can switch platforms, identities, or tactics costlessly. Experiences the constraint as pure coordination: the repetition mechanism is a tool for achieving their goal (consensus shift). Net beneficiary: extraction flows toward them in the form of normalized framing and reduced challenge frequency.
constraint_indexing:constraint_classification(normalization_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM ENGAGEMENT METRICS (ROPE) — Algorithmic systems optimizing for user engagement. Arbitrage exit: platforms can modify algorithms or migrate users across features. Benefits from the ratchet: repetitive claims generate replies, quote-tweets, and thread engagement, all of which increase time-on-platform. Experiences the constraint as coordination: the controversy loop is a feature, not a bug.
constraint_indexing:constraint_classification(normalization_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COUNTER-FLOODING COALITION (SCAFFOLD) — Organized groups (fact-checkers, community notes, moderation teams, epistemic security researchers) building structural countermeasures. Mobile exit: can shift platforms or tactics as the ratchet evolves. Sees the constraint as temporary: ratio-tracking tools, claim-frequency transparency, and algorithmic de-amplification of repetition are emerging. Sunset logic: as these tools mature, the normalization ratchet loses its asymmetric advantage.
constraint_indexing:constraint_classification(normalization_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (public discourse requires some mechanism for surfacing claims) and the extractive mechanism (repetition without challenge shifts consensus independent of truth). The ratchet is not a natural law — it is a contingent feature of platforms that prioritize engagement over accuracy. Analytical classification reflects the mixed structure: real coordination need + asymmetric extraction.
constraint_indexing:constraint_classification(normalization_ratchet, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(normalization_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(normalization_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(normalization_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(normalization_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(normalization_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The asymmetry between flooding cost and refutation cost creates genuine extraction from good-faith participants and lurkers. However, the extraction is not maximal because some coordination function exists (discourse does surface claims for evaluation) and organized countermeasures are emerging. The value reflects the current state where algorithmic amplification has increased extraction but has not yet reached the saturation point where all discourse is flooded. Suppression (0.62): High. Significant barriers to countering normalization include: algorithmic amplification of engagement over accuracy, time/energy asymmetry (refuting takes more effort than asserting), publication bias (platforms don't surface challenge frequency), and exhaustion effects (good-faith debaters burn out). But suppression is not total — community notes, ratio tracking, and moderation tools provide some counterpressure. Theater ratio (0.58): Moderate-high. Much challenge activity is performative: debaters engage not to convince the flooder (known to be futile) but to signal to lurkers that the claim is contested. The signaling is necessary (silence enables normalization) but the debate itself is theater (the flooder is not arguing in good faith). The theater has increased as platforms have made engagement more visible than resolution.
 *
 * PERSPECTIVAL GAP:
 *   The normalization ratchet demonstrates how structural position determines classification. Bad-faith flooders see pure coordination (Rope): the repetition mechanism is a tool for achieving their goal. Platform engagement metrics also see coordination (Rope): the controversy loop increases engagement, which is the optimization target. Good-faith debaters see mixed coordination and extraction (Tangled Rope): the discourse ecosystem enables idea refinement but also imposes asymmetric exhaustion costs. Lurkers and the epistemic commons see pure extraction (Snare): their consensus perception is manipulated without awareness or consent. The counter-flooding coalition sees a temporary problem with a sunset (Scaffold): ratio-tracking tools and algorithmic de-amplification are emerging countermeasures. The analytical observer sees the mixed structure (Tangled Rope): genuine coordination need (discourse requires claim surfacing) alongside asymmetric extraction (repetition without challenge shifts consensus independent of truth). The perspectival gap is not 'which type is correct?' but 'which structural position are you measuring from?'
 *
 * DIRECTIONALITY LOGIC:
 *   Bad-faith flooders are primary beneficiaries: they experience low directionality (d ≈ 0.10) because extraction flows toward them in the form of normalized framing and reduced challenge frequency. Their institutional power and arbitrage exit options further reduce experienced extraction. Platform engagement metrics are also beneficiaries (d ≈ 0.15): controversy loops increase time-on-platform, which is their optimization target. Good-faith debaters are secondary victims with moderate directionality (d ≈ 0.60): they bear asymmetric costs (exhaustion, time drain) but also benefit from the discourse ecosystem (idea refinement, coalition building). Lurker populations are primary victims with high directionality (d ≈ 0.90): they bear the full cost of perception manipulation with no agency or exit. The epistemic commons is also a primary victim (d ≈ 0.92): an abstract collective that cannot organize or exit, bearing maximum extraction in the form of signal-to-noise degradation. The counter-flooding coalition has lower directionality (d ≈ 0.45) because they are organized and mobile, with agency to build countermeasures.
 *
 * MANDATROPHY ANALYSIS:
 *   The normalization ratchet resolves mandatrophy by showing that the constraint has both a genuine coordination function (public discourse requires mechanisms for surfacing claims and testing ideas) and an extractive mechanism (bad-faith actors can flood the zone at lower cost than good-faith actors can refute). The coordination function is real: without some way to repeat and amplify claims, novel ideas would never gain traction. The extraction is also real: the asymmetry between assertion cost and refutation cost allows manipulation of perceived consensus independent of evidence. The tangled_rope classification at the analytical level reflects this mixed structure. The constraint is not 'really' a rope (pure coordination) or 'really' a snare (pure extraction) — it is both, and the classification depends on the observer's structural position. The lurker sees a snare because they bear the cost with no agency. The flooder sees a rope because they capture the benefit. The good-faith debater sees a tangled rope because they experience both coordination and extraction. All three classifications are legitimate perspectival readings of the same structural data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    challenge_visibility_threshold,
    'What ratio of claim-frequency to challenge-frequency is required to prevent normalization in lurker populations?',
    'Controlled experiments varying claim/challenge ratios in synthetic discourse environments; lurker perception surveys measuring consensus shift as a function of ratio',
    'If threshold is low (e.g., 1:3 challenge ratio sufficient): counter-flooding is tractable. If threshold is high (e.g., 1:1 parity required): asymmetry is structural and extraction is higher than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(challenge_visibility_threshold, empirical, 'Claim-to-challenge ratio threshold for preventing normalization').

omega_variable(
    algorithmic_amplification_contribution,
    'How much of the normalization effect is due to algorithmic amplification vs organic repetition?',
    'Comparison of normalization rates on algorithmically-curated platforms vs chronological-feed platforms; A/B testing of feed algorithms with and without engagement-based amplification',
    'If algorithmic: platform design changes can mitigate (scaffold perspective strengthened). If organic: the ratchet is a deeper feature of human attention and the extraction is more structural (snare perspective strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_contribution, empirical, 'Proportion of normalization effect attributable to algorithmic amplification').

omega_variable(
    good_faith_exhaustion_rate,
    'At what repetition frequency do good-faith debaters stop challenging claims due to exhaustion?',
    'Longitudinal tracking of debater engagement: measure reply frequency, thread participation, and dropout rates as a function of claim repetition frequency',
    'If exhaustion threshold is low: suppression is higher than measured, and the tangled_rope classification for good-faith debaters may understate extraction. If threshold is high: debaters have more resilience and the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(good_faith_exhaustion_rate, empirical, 'Repetition frequency at which good-faith challenge rate drops due to exhaustion').

omega_variable(
    community_notes_effectiveness,
    'Do community notes and ratio-tracking tools actually reduce normalization, or do they merely shift the battlefield?',
    'Before/after studies of platforms implementing community notes; comparison of normalization rates on platforms with vs without ratio transparency',
    'If effective: scaffold perspective confirmed — the sunset is real. If ineffective: flooders adapt faster than countermeasures, and the scaffold is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_notes_effectiveness, empirical, 'Whether community notes reduce normalization or merely displace it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(normalization_ratchet, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(norm_ratchet_theater_t0, normalization_ratchet, theater_ratio, 0, 0.35).
narrative_ontology:measurement(norm_ratchet_theater_t3, normalization_ratchet, theater_ratio, 3, 0.45).
narrative_ontology:measurement(norm_ratchet_theater_t6, normalization_ratchet, theater_ratio, 6, 0.52).
narrative_ontology:measurement(norm_ratchet_theater_t10, normalization_ratchet, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(norm_ratchet_extract_t0, normalization_ratchet, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(norm_ratchet_extract_t3, normalization_ratchet, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(norm_ratchet_extract_t6, normalization_ratchet, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(norm_ratchet_extract_t10, normalization_ratchet, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(normalization_ratchet, information_standard).

% DUAL FORMULATION NOTE:
% The normalization ratchet is downstream of transmissibility_asymmetry (the structural fact that assertion is cheaper than refutation). The upstream constraint is a mountain (immutable property of information transmission). The normalization ratchet is the contingent institutional arrangement (platform design, algorithmic amplification) that exploits this asymmetry for extractive purposes. The two constraints have different ε values and different classifications, but they are linked: the mountain enables the tangled_rope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
