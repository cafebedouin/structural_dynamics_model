% ============================================================================
% CONSTRAINT STORY: visibility_as_incitement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_visibility_as_incitement, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: visibility_as_incitement
 *   human_readable: Visibility as Incitement: Forced Witnessing as Resistance Catalyst
 *   domain: political_economy/ethics_of_creation/systems_of_extraction
 *
 * SUMMARY:
 *   The visibility-as-incitement constraint captures a structural inversion
 *   in authoritarian violence: forced witnessing of executions, designed to
 *   suppress resistance through terror, instead makes the regime's violence
 *   legible and therefore actionable. Tala's 60-second count — a cognitive
 *   ritual imposed by the trauma of forced attendance — becomes the mechanism
 *   through which the pattern of violence becomes recognizable as a pattern
 *   rather than experienced as isolated incidents. The constraint exhibits
 *   the full range of DR types depending on the observer's structural
 *   position and time horizon. The regime apparatus sees coordination (Rope)
 *   — forced witnessing communicates the cost of dissent. The forced witness
 *   experiences maximum extraction (Snare) — compelled participation in
 *   ritualized violence with no exit. Institutional stability is the primary
 *   victim (Snare from a different position) — the visibility mechanism that
 *   was meant to suppress instead catalyzes resistance. The resistance
 *   network sees mixed coordination and extraction (Tangled Rope) — shared
 *   trauma enables organizing. The international human rights network sees a
 *   temporary problem with a sunset (Scaffold) — documentation and norm
 *   diffusion will eventually prohibit the practice. The analytical observer
 *   risks naturalizing the dynamic as an immutable law (Mountain / false
 *   summit) — but the inversion depends on contingent institutional choices
 *   (public vs secret violence, ritualized vs dispersed executions).
 *
 * KEY AGENTS:
 *   - Forced Witnesses: Primary victims (powerless/trapped at immediate horizon, powerless/identity_locked at biographical horizon) — compelled attendance creates trauma and legibility simultaneously
 *   - Institutional Stability: Abstract victim (institutional/constrained) — the regime's long-term viability is undermined by its own suppression mechanism
 *   - Regime Apparatus: Primary beneficiary in intended function (institutional/arbitrage) — designed the constraint to suppress resistance through terror
 *   - Resistance Network: Secondary beneficiary in actual effect (moderate/constrained) — forced witnessing creates common knowledge that enables coordination
 *   - Executed Dissidents: Direct victims (powerless/trapped) — bear the ultimate cost of the regime's violence
 *   - International Human Rights Network: Organized observers (organized/mobile) — document and delegitimize the practice across borders
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent dynamics as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(visibility_as_incitement, 0.78).
domain_priors:suppression_score(visibility_as_incitement, 0.85).
domain_priors:theater_ratio(visibility_as_incitement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(visibility_as_incitement, extractiveness, 0.78).
narrative_ontology:constraint_metric(visibility_as_incitement, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(visibility_as_incitement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(visibility_as_incitement, snare).
narrative_ontology:human_readable(visibility_as_incitement, "Visibility as Incitement: Forced Witnessing as Resistance Catalyst").
narrative_ontology:topic_domain(visibility_as_incitement, "political_economy/ethics_of_creation/systems_of_extraction").

domain_priors:requires_active_enforcement(visibility_as_incitement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(visibility_as_incitement, regime_apparatus).
narrative_ontology:constraint_victim(visibility_as_incitement, institutional_stability).
narrative_ontology:constraint_victim(visibility_as_incitement, forced_witnesses).
narrative_ontology:constraint_victim(visibility_as_incitement, executed_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FORCED WITNESS (SNARE) — Compelled attendance at executions with no exit option. Immediate time horizon: the 60-second count begins now. Trapped: physical coercion, legal mandate, community surveillance. Maximum extraction: forced to internalize regime logic through ritualized violence. The constraint's intended function — suppress resistance through terror — operates at full force from this position.
constraint_indexing:constraint_classification(visibility_as_incitement, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: WITNESS WHO COUNTS (SNARE) — Same agent, biographical time horizon. Identity-locked: the witness's self-concept has fused with the counting ritual. Tala counts to sixty and the pattern becomes legible. The constraint still extracts maximally but the extraction mechanism has inverted: forced visibility intended to terrorize instead catalyzes recognition of the system's internal logic. The snare remains a snare — high extraction, high suppression — but the victim now sees the mechanism.
constraint_indexing:constraint_classification(visibility_as_incitement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESISTANCE NETWORK (TANGLED ROPE) — Organized but not institutional. Constrained exit: can operate underground but faces severe costs if exposed. Generational time horizon: building capacity across decades. Experiences the constraint as mixed: forced witnessing creates shared trauma (extraction) but also creates legibility of the regime's violence (coordination). The visibility mechanism that was meant to suppress instead provides common knowledge of the system's brutality, enabling coordination among those who have witnessed.
constraint_indexing:constraint_classification(visibility_as_incitement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME APPARATUS / INTENDED FUNCTION (ROPE) — The constraint as designed. Institutional power, arbitrage exit (regime actors can exempt themselves or leave), immediate time horizon (suppress resistance now). From this perspective, forced witnessing is pure coordination: communicate the cost of dissent, align population behavior through shared terror. Low extraction because the regime is the beneficiary. This perspective represents the constraint's claimed function, not its actual effect.
constraint_indexing:constraint_classification(visibility_as_incitement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL STABILITY / ACTUAL EFFECT (SNARE) — The regime's long-term stability is the primary victim. Biographical time horizon: the regime persists across leadership changes but is undermined by its own suppression mechanism. Constrained exit: the regime cannot simply stop the executions without admitting weakness. The forced witnessing creates common knowledge of the regime's violence, which catalyzes resistance rather than suppressing it. High extraction: the constraint consumes the regime's legitimacy. The regime apparatus sees rope; institutional stability experiences snare.
constraint_indexing:constraint_classification(visibility_as_incitement, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Civilizational time horizon, universal scope. Risks naturalizing the visibility-incitement dynamic as an immutable property of authoritarian regimes: 'Terror always backfires.' But the structural data contradicts this — the constraint's inversion depends on specific conditions (forced attendance, ritualized repetition, legibility of the pattern). The mountain classification is a false summit: what appears as a law of political dynamics is actually a contingent institutional arrangement. The regime could reduce visibility (secret executions, dispersed violence) and the incitement mechanism would break.
constraint_indexing:constraint_classification(visibility_as_incitement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INTERNATIONAL HUMAN RIGHTS NETWORK (SCAFFOLD) — Organized actors with mobile exit options (can operate across borders). Generational time horizon: building norms and institutions that make forced witnessing unacceptable. Sees the constraint as temporary: the visibility mechanism that currently incites resistance will eventually be prohibited by international law and norm diffusion. Sunset logic: as documentation of forced witnessing spreads (video, testimony, forensic evidence), the practice becomes internationally illegitimate and domestically unsustainable. Low extraction because this perspective has agency and sees an exit path.
constraint_indexing:constraint_classification(visibility_as_incitement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(visibility_as_incitement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(visibility_as_incitement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(visibility_as_incitement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(visibility_as_incitement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(visibility_as_incitement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The regime extracts compliance through terror, but the extraction mechanism inverts over time — forced witnessing creates legibility that undermines the regime's stability. The high value reflects both the immediate extraction (forced participation in violence) and the long-term extraction (regime legitimacy consumed by its own suppression mechanism). Suppression (0.85): Very high. Forced attendance is legally mandated, physically coerced, and socially enforced. Exit options are minimal — witnesses cannot refuse without becoming targets themselves. The suppression is structural (legal/physical barriers) and internalized (trauma, identity fusion with the counting ritual). Theater ratio (0.68): High. The execution ritual is substantially performative — the regime stages public violence to communicate power, but the communication mechanism (forced witnessing) has inverted from its intended function. The theater has increased over the interval as the regime has escalated the ritualization (mandatory attendance, formalized procedures, repeated executions) in an attempt to restore the suppression function, but the escalation only increases legibility. The measurements show extraction and theater both rising as the regime doubles down on a mechanism that is actively undermining its stability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a critical structural inversion: the regime apparatus sees Rope (coordination through terror), but institutional stability experiences Snare (extraction of legitimacy). The gap is not just perspectival disagreement — it represents a genuine structural contradiction. The constraint's intended function (suppress resistance) and its actual effect (catalyze resistance) are opposite. The forced witness perspective shows the mechanism of inversion: at immediate time horizon, the witness is trapped in the terror (Snare). At biographical time horizon with identity lock, the witness has internalized the counting ritual, and the counting makes the pattern legible (still Snare, but the extraction mechanism has inverted). The resistance network sees Tangled Rope because they experience both the extraction (shared trauma) and the coordination (common knowledge of regime violence). The scaffold perspective (international human rights network) represents the constraint's eventual resolution — as documentation spreads, the practice becomes unsustainable. The mountain perspective is a false summit — the visibility-incitement dynamic is not a law of authoritarian regimes but a contingent feature of regimes that choose public over secret violence.
 *
 * DIRECTIONALITY LOGIC:
 *   The forced witness at immediate time horizon is a full victim with trapped exit options — maximum directionality (d ≈ 0.95), maximum experienced extraction. The same witness at biographical time horizon with identity_locked exit still experiences maximum extraction but the mechanism has shifted — the identity lock is cognitive (fusion with the counting ritual) rather than purely structural. Institutional stability is a victim with constrained exit (the regime cannot simply stop without admitting weakness) — high directionality (d ≈ 0.85). The regime apparatus is a beneficiary with arbitrage exit — low directionality (d ≈ 0.05) in the intended function perspective, but this perspective represents the constraint as designed, not as experienced. The resistance network is a mixed case — victim of the violence (extraction) but beneficiary of the legibility (coordination) — moderate directionality (d ≈ 0.55). The international human rights network is a beneficiary (documentation enables their mission) with mobile exit — low directionality (d ≈ 0.20). The analytical observer uses canonical analytical directionality (d ≈ 0.73).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by demonstrating that high extraction (0.78) can coexist with genuine coordination function when the coordination is unintended. The regime designed forced witnessing as pure extraction (Snare) — suppress resistance through terror. But the visibility mechanism creates common knowledge of the regime's violence, which enables resistance coordination (Tangled Rope from the resistance network perspective). The coordination is real — witnesses who have counted to sixty together have shared knowledge of the pattern — but it is not the coordination the regime intended. The mandatrophy question 'Is this coordination or extraction?' is answered: it is both, and the mixture depends on the observer's position. The regime sees coordination (intended function). Institutional stability sees extraction (actual effect). The resistance network sees both (shared trauma + organizing capacity). The analytical challenge is not to choose one type but to map the presheaf: which perspectives see which types, and why do the perspectives diverge? The divergence reveals the structural inversion — a suppression mechanism that incites what it was designed to suppress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legibility_threshold,
    'What threshold of forced witnessing repetition is required for the pattern to become legible and catalyze resistance rather than suppress it?',
    'Historical analysis of execution regimes: correlation between witnessing frequency, duration, and subsequent resistance activity. Comparison of regimes with public vs secret executions. Tala''s 60-second count is one data point — how many iterations before the count becomes recognition?',
    'If threshold is low (few repetitions): forced witnessing almost always backfires, and the snare classification for institutional stability is robust. If threshold is high (many repetitions required): most regimes can sustain terror without incitement, and the rope classification for regime apparatus is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legibility_threshold, empirical, 'Repetition threshold for visibility to catalyze resistance').

omega_variable(
    suppression_visibility_tradeoff,
    'Can regimes reduce visibility (secret executions, dispersed violence) without losing the suppression function, or is public terror structurally necessary for deterrence?',
    'Comparative analysis of authoritarian regimes: effectiveness of secret vs public violence in suppressing dissent. Game-theoretic modeling of common knowledge requirements for deterrence.',
    'If public visibility is necessary: the regime faces an inescapable tradeoff (suppress now, incite later), and the constraint is a structural trap. If secret violence works: the visibility-incitement dynamic is contingent on regime choice, and the mountain classification is confirmed as false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_visibility_tradeoff, empirical, 'Whether suppression requires public visibility').

omega_variable(
    witness_identity_lock_mechanism,
    'What specific identity-fusion mechanism binds the witness who counts? Is it trauma bonding, cognitive dissonance resolution, or narrative coherence seeking?',
    'Psychological and ethnographic study of forced witnesses: longitudinal tracking of identity changes, narrative framing, and resistance participation. Analysis of testimony from witnesses who became resisters vs those who did not.',
    'If trauma bonding: the identity lock is a side effect of the violence, not the counting ritual. If cognitive dissonance: the witness must construct meaning from the forced participation, and the counting is a coherence mechanism. If narrative seeking: the counting provides a frame that makes the violence legible as a pattern rather than random cruelty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(witness_identity_lock_mechanism, conceptual, 'Mechanism of identity fusion in forced witnesses').

omega_variable(
    incitement_coordination_boundary,
    'At what point does the visibility mechanism shift from individual incitement (Tala decides to hunt) to collective coordination (resistance network forms)?',
    'Network analysis of resistance formation: identification of common knowledge thresholds, coordination cascades, and tipping points. Comparison of individual vs collective action following forced witnessing events.',
    'If individual incitement is sufficient: the constraint operates as a snare for institutional stability even without organized resistance. If collective coordination is required: the tangled rope perspective (resistance network) is the critical transition, and the constraint''s effect depends on whether witnesses can communicate and organize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incitement_coordination_boundary, empirical, 'Threshold for visibility to enable collective coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(visibility_as_incitement, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vis_incite_theater_initial, visibility_as_incitement, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vis_incite_theater_early, visibility_as_incitement, theater_ratio, 3, 0.58).
narrative_ontology:measurement(vis_incite_theater_current, visibility_as_incitement, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(vis_incite_extract_initial, visibility_as_incitement, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(vis_incite_extract_early, visibility_as_incitement, base_extractiveness, 3, 0.71).
narrative_ontology:measurement(vis_incite_extract_current, visibility_as_incitement, base_extractiveness, 6, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(visibility_as_incitement, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of brilliance_as_structural_trap. The upstream constraint (brilliance as trap) describes how exceptional capability becomes a binding mechanism. The visibility-as-incitement constraint describes how forced witnessing of the trap's operation (executions of brilliant dissidents) creates legibility that catalyzes resistance. The two constraints share a structural pattern: a mechanism designed to extract or suppress instead creates the conditions for its own subversion. The upstream constraint operates at the individual level (the brilliant dissident is trapped by their own capability). The downstream constraint operates at the collective level (forced witnesses coordinate through shared legibility of the regime's violence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(visibility_as_incitement, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
