% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interuption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_conversational_dogmas_interruption, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: conversational_dogmas_interuption
 *   human_readable: Conversational Dogmas: Interruption vs. Strong Civility Norms
 *   domain: social/technological/discourse
 *
 * SUMMARY:
 *   Conversational norms around interruption and civility constitute a
 *   constraint that exhibits genuine coordination function (orderly
 *   turn-taking prevents collision) while simultaneously enabling extraction
 *   (turn-taking rules protect established voices and silence emerging ones).
 *   The constraint's classification varies sharply by observer position:
 *   those already at the table experience rope (coordination benefit); those
 *   fighting for audibility experience snare (enforced silence); organized
 *   digital communities building alternative platforms experience scaffold
 *   (sunset through technological transition); institutions defending
 *   traditional discourse formats experience piton (performative civility
 *   degradation). The critical tension: the same rule that enables
 *   coordination also enables gatekeeping. Turn-taking prevents chaos AND
 *   prevents interruption-as-correction. Strong civility norms protect order
 *   AND protect privilege. This hybrid structure makes the constraint a
 *   paradigmatic tangled rope — both functions are real, both are structural,
 *   and neither can be removed without damaging the other.
 *
 * KEY AGENTS:
 *   - Formal Institutional Speakers: Primary beneficiary (institutional/arbitrage) — command uninterrupted time; civility norms protect their speaking slot
 *   - Marginalized Voices: Primary victim (powerless/trapped) — systematically talked over; forced to wait for floor that rarely comes; no exit without surrendering participation
 *   - Urgent Claim Holders: Secondary victim (moderate/constrained) — need interruption for time-sensitive corrections; face social/professional penalties for norm violation
 *   - High-Status Discourse Participants: Beneficiary (powerful/arbitrage) — can violate interruption norms with minor penalty; their violations are reframed as 'passion' or 'expertise'
 *   - Digital Platform Coalition: Organized agents (organized/mobile) — building alternative discourse infrastructures (Reddit, Discord, Twitter threads) with asynchronous reply and lower turn-enforcement; enabling exit from traditional civility norms
 *   - Institutional Deliberative Bodies: Institutional actor (institutional/arbitrage) — enforce strong civility norms through procedural rules; maintain traditional formats despite evidence of silencing effects
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes constraint as hybrid coordination-extraction; identifies piton elements (performative ritualization)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(conversational_dogmas_interuption, 0.52).
domain_priors:suppression_score(conversational_dogmas_interuption, 0.65).
domain_priors:theater_ratio(conversational_dogmas_interuption, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(conversational_dogmas_interuption, extractiveness, 0.52).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(conversational_dogmas_interuption, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(conversational_dogmas_interuption, tangled_rope).
narrative_ontology:human_readable(conversational_dogmas_interuption, "Conversational Dogmas: Interruption vs. Strong Civility Norms").
narrative_ontology:topic_domain(conversational_dogmas_interuption, "social/technological/discourse").

domain_priors:requires_active_enforcement(conversational_dogmas_interuption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, formal_institutional_speakers).
narrative_ontology:constraint_beneficiary(conversational_dogmas_interuption, high_status_discourse_participants).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, marginal_voices).
narrative_ontology:constraint_victim(conversational_dogmas_interuption, urgent_claim_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED INTERLOCUTOR (SNARE) — Subject to strong civility norms that prevent interruption, yet systematically talked over and never granted the floor. Cannot exit discourse spaces without surrendering participation entirely. Bears maximum extraction: enforced silence while dominant voices command attention. No alternatives; participation requires submission to norms that structurally prevent them from being heard.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: URGENT CLAIM HOLDER (TANGLED ROPE) — Needs to interrupt to inject time-sensitive corrections or critical information, but faces social/professional penalties for violating civility norms. Constrained exit: can walk away from specific conversations but not from discourse participation itself. Experiences both the coordination benefit (conversation structure prevents chaos) and the extraction (prevented from urgent contribution). Asymmetric: the civility norm benefits those with already-established voice; harms those fighting for baseline audibility.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FORMAL INSTITUTIONAL SPEAKER (ROPE) — Benefits from strong civility norms that protect their uninterrupted speaking time. Experiences the constraint as pure coordination: orderly turn-taking enables structured communication. Low experienced extraction because they define the baseline (their speaking style IS the standard civility). Can exit by choosing informal contexts or by leveraging their status to redefine norms.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL PLATFORM COALITION (SCAFFOLD) — Alternative discourse platforms (Twitter, Reddit, Discord, TikTok) create lower-friction interruption and asynchronous reply mechanisms that bypass strong civility norms entirely. Organized agents (platform designers, online communities) are building sunset clause into traditional conversational constraints: real-time discourse is shifting to comment threads, reaction systems, and multithreaded conversations where 'interruption' becomes parallel contribution. Theater ratio low for these platforms; function is distribution and simultaneity, not order. High exit mobility — marginalized voices can relocate to digital spaces with different norms.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: DELIBERATIVE DEMOCRACY RITUAL (PITON) — Academic and civic institutions have ritualized 'civility norms' as markers of legitimate discourse, decoupled from actual function. Town halls, academic conferences, and media panels maintain strong turn-taking structures despite evidence that they systematically silence marginalized perspectives. The ritual persists through institutional inertia: civility appears as a marker of respectability, so institutions enforce it even when (or especially when) it prevents meaningful participation. Theater ratio high (0.65+): the performance of 'civil discourse' has become the goal, replacing actual inclusive dialogue. The piton classification derives from this performative degradation — the constraint maintains itself through theatrical persistence, not through demonstrated coordination function.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, the constraint exhibits genuine coordination function (turn-taking prevents collision and chaos) AND asymmetric extraction (turn-taking rules structurally favor those already at the table). Neither function can be removed without losing the other. The constraint is not a false mountain (not a law of conversation itself) nor a pure rope (not neutral across power distributions). It is a hybrid: real coordination wrapped around real extraction. The asymmetry is not incidental — it is structural: norms that enable orderly conversation also enable gatekeeping.
constraint_indexing:constraint_classification(conversational_dogmas_interuption, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(conversational_dogmas_interuption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interuption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(conversational_dogmas_interuption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(conversational_dogmas_interuption, TR),
    TR >= 0.70.

:- end_tests(conversational_dogmas_interuption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The civility norm extracts from those seeking baseline audibility while coordinating turn-order. The extraction is not total (some marginalized speakers do gain floor) but systematic (recovery rates are status-stratified). The metric reflects both functions: coordination value (0.2) plus asymmetric gatekeeping (0.32). Suppression (0.65): High. Multiple barriers prevent interruption from marginalized speakers: social penalties, professional consequences, internalized norm compliance, and institutional enforcement. These are not identical to the extraction itself but reinforce it — suppression enables the extraction to persist despite its visibility. Theater ratio (0.58): Moderate-high. Academic and civic institutions have increasingly ritualized 'civil discourse' as a marker of legitimacy, decoupled from actual inclusion outcomes. Panels and town halls maintain strict turn-taking formats despite evidence they silence marginalized perspectives. The performance has become the goal — institutions enforce civility rules even when doing so visibly prevents legitimate participation. The ratio increased over the interval (0.35 → 0.58) as institutions doubled down on ritual enforcement rather than reforming norms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a six-way perspectival divergence. Formal institutional speakers perceive rope (orderly turn-taking enables structured communication; they experience no extraction). Marginalized voices perceive snare (enforced silence; no exit without surrendering participation). The digital platform coalition perceives scaffold (alternative norms emerging with sunset logic; exit mobility high). Institutions perceive piton (ritual civility persists through inertia; performative degradation visible). Urgent claim holders perceive tangled rope (both benefit and harm; constrained exit). The analytical observer perceives tangled rope at civilizational scale (coordination and extraction are inseparable; neither is optional). No single classification is 'correct' — the presheaf over observation site is the true structure. However, the piton classification is concerning: institutions maintain strong civility enforcement despite rising evidence it silences legitimate voices. This is characteristic of piton degradation — maintaining the ritual rather than the function.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map directly to power asymmetries in the constraint. Formal institutional speakers benefit from norms that protect their uninterrupted time; marginalized voices bear costs of enforced silence. The directionality pipeline computes d from these structural facts: beneficiaries with arbitrage options (high-status speakers can exit to informal contexts or leverage status to redefine norms) experience low d → negative/low χ; victims with trapped or constrained options experience high d → high χ. The analytical observer sees both functions simultaneously — d ≈ 0.50 (symmetric cost-benefit) — but this masks the asymmetry between those who designed the rules (beneficiaries) and those subject to them (victims). The perspectival gap is not measurement error; it is the structural reality of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint resolves mandatrophy by decomposing the 'civility norm' into two structurally distinct claims: (1) Turn-taking prevents collision (coordination function, genuine at all scales). (2) Strong enforcement of turn-taking rules is necessary to achieve that coordination (empirical claim, falsifiable). The tangled rope classification accepts (1) as structural and questions (2). The emergence of digital platforms with looser turn-enforcement and lower chaos rates suggests (2) is false — coordination is possible with much less strict norms. This supports the scaffold perspective: the sunset is real. Organizations clinging to strong civility norms are maintaining piton (ritual without function) rather than adjusting to lower-enforcement models that achieve coordination with less silencing. The mandatrophy is resolved by recognizing that 'civility' conflates two distinct claims: orderly turn-taking (real function) and strict enforcement (potentially contingent). The constraint is NOT a false mountain (turn-taking is not an inherent law) but a real hybrid: genuine coordination layered with gatekeeping extraction. The institutional response to platform emergence will determine whether the constraint transitions to scaffold (norms reform, theater decreases) or remains piton (ritual enforcement tightens despite evidence of harm).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    turn_taking_necessity_threshold,
    'Is strong turn-taking enforcement necessary to prevent discourse chaos, or does chaos only emerge above a critical participant density?',
    'Comparative analysis of online communities: small forums vs large platforms; moderated channels vs free-for-all; metrics on comprehensibility and participation patterns as function of turn-enforcement strictness and participant count',
    'If necessary at all scales: justifies mountain-adjacent (natural law) classification for turn-taking. If only at high density: turns-taking norms are contingent on scale, not inherent — supports snare/tangled rope classification and reveals civility-norms-as-gatekeeping.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(turn_taking_necessity_threshold, empirical, 'Empirical threshold for turn-taking necessity by participant density').

omega_variable(
    marginalized_voice_recovery_rate,
    'What fraction of marginalized speakers who attempt interruption in strong-civility contexts are subsequently granted the floor (deferred speech)? Does this rate differ significantly between high-status and low-status interrupters?',
    'Discourse analysis of recorded conversations (academic, civic, media); categorization of interruptions by interrupter status and speaker response; measurement of deferred speech grant rates',
    'If recovery rate is near-equal across status: supports rope classification (turn-taking is neutral mechanism). If recovery is status-stratified: confirms snare/tangled rope — norms enforce silence on those least able to command return floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_voice_recovery_rate, empirical, 'Floor-grant rate for interruptions by interrupter status').

omega_variable(
    alternative_discourse_adoption_trajectory,
    'Are digital platforms with looser interruption norms actually reducing participation inequality, or are they reproducing the same stratification through different mechanisms (e.g., algorithmic visibility instead of turn-taking)?',
    'Longitudinal analysis of voice distribution on platforms with different interruption norms; comparison of speaker visibility curves (cumulative speaking time, reply counts, reach) across platforms and across speaker status levels',
    'If digital platforms reduce inequality: scaffold classification is correct — the sunset is real and functional. If inequality persists: suggests the constraint is deeper than turn-taking norms (extraction mechanism migrated to new infrastructure) — tangled rope persists across platforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_discourse_adoption_trajectory, empirical, 'Whether alternative platforms reduce or reproduce participation inequality').

omega_variable(
    institutional_enforcement_motivation,
    'Do institutions enforce strong civility norms because they genuinely believe in their coordination function, or because they perceive norms as protecting institutional control and legitimacy?',
    'Institutional discourse analysis: how institutions respond when civility norms demonstrably prevent legitimate participation (e.g., academic panels that silence relevant expertise). Do they reform norms or tighten enforcement?',
    'If reform-oriented: supports piton classification (institutions would update norms if they recognized degradation). If tighten-enforcement: confirms that civility is actually a gatekeeping mechanism benefiting institutions — stronger snare/tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_enforcement_motivation, conceptual, 'Whether institutional enforcement is motivated by coordination or control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(conversational_dogmas_interuption, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(convdogma_tr_t0, conversational_dogmas_interuption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(convdogma_tr_t10, conversational_dogmas_interuption, theater_ratio, 10, 0.5).
narrative_ontology:measurement(convdogma_tr_t20, conversational_dogmas_interuption, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(convdogma_be_t0, conversational_dogmas_interuption, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(convdogma_be_t10, conversational_dogmas_interuption, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(convdogma_be_t20, conversational_dogmas_interuption, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(conversational_dogmas_interuption, information_standard).
narrative_ontology:boltzmann_floor_override(conversational_dogmas_interuption, 0.55).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, academic_journal_peer_review_gatekeeping).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, media_representation_inequality).
narrative_ontology:affects_constraint(conversational_dogmas_interuption, online_harassment_norm_enforcement).

% DUAL FORMULATION NOTE:
% The conversational dogma constraint decomposes into two distinct mechanisms: (a) Turn-order enforcement (logistics of preventing speaking collision) — epsilon ~0.15, rope-dominated, coordination function preserved across all scales. (b) Status-stratified norm application (enforcement asymmetry that silences low-status interrupters more than high-status) — epsilon ~0.52, tangled rope, extraction function. These are typically conflated as single 'civility norm' but have different ε values and different structural dynamics. Digital platforms demonstrate that (a) is possible with minimal (b) — supporting scaffold perspective. Traditional institutions maintain (b) even as (a) weakens — supporting piton perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(conversational_dogmas_interuption, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
