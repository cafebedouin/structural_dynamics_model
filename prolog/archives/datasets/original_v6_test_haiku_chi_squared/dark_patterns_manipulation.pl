% ============================================================================
% CONSTRAINT STORY: dark_patterns_manipulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dark_patterns_manipulation, []).

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
 *   constraint_id: dark_patterns_manipulation
 *   human_readable: Dark Patterns (Interface Coercion)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Dark patterns represent a structural mechanism through which digital
 *   platforms extract user value (attention, data, purchasing behavior, time)
 *   by exploiting cognitive biases and asymmetric information. Unlike crude
 *   extraction mechanisms (surveillance, data theft), dark patterns work
 *   through deliberate UX design that steers user behavior toward
 *   platform-beneficial outcomes while maintaining plausible deniability
 *   ('users chose to click the button'; 'the default was transparent'). The
 *   constraint exhibits all six DR types depending on the observer's
 *   structural position. From the user's perspective, trapped by network
 *   effects and switching costs, dark patterns are a snare with no exit. From
 *   the platform's perspective, they are a coordination mechanism that solves
 *   the problem of converting attention into revenue. From a regulatory
 *   perspective, they are a hybrid coordination/extraction problem with a
 *   sunset pathway (GDPR/DMA transparency and consent requirements). From the
 *   psychological theory perspective, they risk naturalizing extractive
 *   design choices as inevitable features of human cognition. The
 *   extractiveness metric has increased from 0.35 to 0.62 over the
 *   observation interval (14 years, roughly 2010-2024) as dark patterns have
 *   become more sophisticated and as user awareness has paradoxically
 *   increased platforms' investment in subtler manipulation (dark pattern
 *   arms race). Theater ratio has increased from 0.38 to 0.55, reflecting
 *   that platforms increasingly rebrand extraction as
 *   safety/discovery/engagement rather than acknowledging attention
 *   monetization.
 *
 * KEY AGENTS:
 *   - End Users: Primary victim (powerless/trapped) — unable to exit despite harm; exploit cognitive limits that individuals cannot overcome alone
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture user attention, data, and purchasing behavior; experience dark patterns as a functional coordination mechanism
 *   - Privacy-Conscious Consumer Coalition: Secondary victim (moderate/constrained) — organized enough to demand alternatives but locked in by network effects; benefit from free services but bear extraction cost
 *   - Regulatory Coalition (EU/GDPR/DMA): Organized constraint (organized/constrained) — impose transparency and consent requirements; benefit from reduced manipulation but constrained by enforcement and platform arbitrage
 *   - Attention Economy Theory: Degraded institutional frame (piton) — once explained platform business models; now theater that obscures dark pattern mechanisms
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design choices as inevitable cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dark_patterns_manipulation, 0.62).
domain_priors:suppression_score(dark_patterns_manipulation, 0.68).
domain_priors:theater_ratio(dark_patterns_manipulation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dark_patterns_manipulation, extractiveness, 0.62).
narrative_ontology:constraint_metric(dark_patterns_manipulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dark_patterns_manipulation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dark_patterns_manipulation, snare).
narrative_ontology:human_readable(dark_patterns_manipulation, "Dark Patterns (Interface Coercion)").
narrative_ontology:topic_domain(dark_patterns_manipulation, "technological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dark_patterns_manipulation, platform_operators).
narrative_ontology:constraint_victim(dark_patterns_manipulation, end_users).
narrative_ontology:constraint_victim(dark_patterns_manipulation, consumer_autonomy).
narrative_ontology:constraint_victim(dark_patterns_manipulation, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped between switching costs, network effects, and information asymmetry. Cannot exit platform ecosystems without abandoning social/commercial infrastructure. Dark patterns exploit cognitive limitations (default effects, sunk costs, loss aversion) that users cannot overcome through individual effort. d≈0.93, f(d)≈1.38, σ=1.2 → χ≈1.03.
constraint_indexing:constraint_classification(dark_patterns_manipulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS CONSUMER COALITION (TANGLED ROPE) — Organized enough to demand alternatives but constrained by network effects and platform lock-in. Benefits from the surveillance infrastructure through free/low-cost services; bears extraction cost through data exfiltration and manipulative UX. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(dark_patterns_manipulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences dark patterns as a pure coordination mechanism: steering user attention, engagement, and purchasing behavior toward profitable outcomes. The constraint solves the platform's coordination problem of how to monetize user attention. Suppression of user agency is the functional mechanism. d≈0.02, f(d)≈-0.18, σ=1.2 → χ≈-0.11. Net beneficiary.
constraint_indexing:constraint_classification(dark_patterns_manipulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION EU/GDPR/DMA (TANGLED ROPE) — Organized to constrain dark patterns through law (GDPR consent, Digital Markets Act transparency requirements). Benefits from reduced manipulation in regulated markets; constrained by enforcement costs and platform regulatory arbitrage (non-EU jurisdictions). d≈0.58, f(d)≈0.72, σ=1.1 → χ≈0.49.
constraint_indexing:constraint_classification(dark_patterns_manipulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ATTENTION ECONOMY THEORY (PITON) — The 'attention economy' framing of digital platforms was once functional (explained business models); now largely theater. Platforms no longer explain their business as attention monetization but rebrand as social goods, safety platforms, or discovery engines. The attention economy theory persists through academic inertia and residual institutional usage but has been displaced by darker framings. theater_ratio=0.55 reflects moderate performativity: the attention economy description is not entirely false but increasingly incomplete — it avoids naming the predatory coercion mechanisms (dark patterns, addictive design, behavioral targeting).
constraint_indexing:constraint_classification(dark_patterns_manipulation, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational perspective: dark patterns exploit fundamental cognitive architecture of human decision-making. Loss aversion, default effects, sunk cost fallacy, and present bias are features of human neurobiology, not bugs of platform design. From this view, steering behavior toward platform benefit is inevitable once platforms exist. However, the structural data (ε=0.62, suppression=0.68) contradicts this — the severity of extraction is driven by contingent platform design choices (dark patterns are engineered, not inherent), not by immutable cognitive limits. This is a false summit: naturalizing extractive design choices as inevitable psychological laws.
constraint_indexing:constraint_classification(dark_patterns_manipulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dark_patterns_manipulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dark_patterns_manipulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dark_patterns_manipulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dark_patterns_manipulation, TR),
    TR >= 0.70.

:- end_tests(dark_patterns_manipulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Dark patterns systematically steer user behavior toward outcomes that benefit the platform (increased engagement, continued use, data sharing, purchases) at direct cost to user welfare. The extraction is measurable: users report regretting clicks, spending more time/money than intended, sharing more data than they would under transparent choice. However, it is not maximal (0.70+) because users retain some agency — dark patterns influence but do not eliminate choice. The metric reflects structured manipulation that works through cognitive exploitation rather than total coercion. Suppression (0.68): High. Suppression operates through multiple mechanisms: (a) information asymmetry — users don't understand how platforms achieve steering effects; (b) cognitive barriers — biases that even informed users cannot overcome (default effects, loss aversion); (c) exit barriers — network effects and switching costs prevent users from choosing alternative platforms; (d) legal theater — platforms frame dark patterns as 'user choice' or 'design defaults' despite knowing their coercive effect. Theater ratio (0.55): Moderate. The 'attention economy' framing (platforms optimize user engagement) has become increasingly performative as dark patterns have become more coercive. Platforms now rebrand as safety/discovery engines or social goods rather than openly acknowledging attention extraction. However, the theater is not maximal (0.70+) because some legitimate coordination function remains — platforms do optimize legitimate user interests (discovery, connection, relevance) alongside extraction. The theater has increased over time as user awareness of dark patterns has forced platforms into euphemistic rebranding.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival divide between users and platforms. Users see a snare (trapped, no exit, extraction). Platforms see a rope (coordination mechanism solving attention monetization). Regulatory observers see a tangled rope (mixed coordination/extraction with a policy sunset). The gap is primarily driven by directionality (d values 0.93 for users vs 0.02 for platforms) and exit options (trapped vs arbitrage). The analytical observer risks collapsing this gap by framing dark patterns as inevitable features of human cognition (mountain), thereby naturalizing what is actually a contingent institutional choice. This false summit is the key diagnostic insight: dark patterns are not inevitable; they are engineered. Alternative platforms have demonstrated that user engagement can be sustained with transparent UX design (Signal, DuckDuckGo, some open-source tools). The snare classification is justified not because cognitive biases are immutable but because platforms have deliberately chosen to exploit them rather than working around them.
 *
 * DIRECTIONALITY LOGIC:
 *   End users: Victim + trapped → d≈0.93, f(d)≈1.38. Maximum extraction. Cannot exit due to network effects and switching costs; cannot overcome cognitive biases through individual effort. Platform operators: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.18. Net beneficiary. Can exit or renegotiate; experience the constraint as functionally solving their revenue problem. Privacy-conscious coalitions: Victim + constrained → d≈0.72, f(d)≈1.12. High extraction but not maximum; organized enough to demand alternatives and to partially mitigate through privacy tools, but constrained by lock-in. Regulatory coalition: Mixed victim/beneficiary + constrained → d≈0.58, f(d)≈0.72. Can impose rules (arbitrage for this purpose) but constrained by enforcement and regulatory arbitrage (platforms operating outside jurisdiction). Attention economy theory: Institutional + constrained → d≈0.48, f(d)≈0.60. Piton classification comes from theater gate (0.55), not from high directionality. The theory's degradation reflects that it can no longer explain modern dark patterns without cognitive dissonance. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival (observer naturalizes); the false summit detector flags this because extractiveness is driven by contingent design choices, not immutable laws.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION VALIDATION: Dark patterns satisfy the snare criteria unambiguously from the user perspective: ε=0.62 (high extraction), suppression=0.68 (high — multiple mechanisms prevent escape), χ≈1.03 (effective extraction from powerless/trapped user). No ambiguity between pure extraction and coordination — dark patterns explicitly trade user welfare for platform benefit. The classification is not mandatrophic because beneficiary/victim roles are clear (platforms benefit, users bear costs), and the mechanisms are asymmetrically extractive (not coordination-hybrid). However, the snare classification is perspectival — it is true only from the user's view. From the platform's view, the same mechanisms are rope (solving the engagement/monetization coordination problem). The system does not resolve this into a single type; rather, it documents the perspectival gap as evidence that this is a genuine snare viewed by those harmed by it (users, regulatory bodies) and a rope viewed by beneficiaries (platforms). The mandatrophy is resolved by showing that the multiplicity of types is itself the data: snare from below, rope from above, is precisely the pattern that characterizes structural extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_bias_vs_design_choice,
    'To what extent is dark pattern effectiveness driven by immutable cognitive biases vs. contingent UX design choices?',
    'A/B testing alternative transparent UI designs that preserve platform functionality but eliminate dark patterns; measurement of user behavior shift; comparison with platforms using ethical defaults',
    'If immutable bias dominates: mountain classification is justified, dark patterns are inevitable. If design choice dominates: snare classification is justified, alternatives exist, extraction is contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_bias_vs_design_choice, empirical, 'Immutable cognitive bias vs. engineered design choice').

omega_variable(
    exit_feasibility_threshold,
    'What network effect size threshold converts dark patterns from ''costly to exit'' to ''exit is structurally impossible''?',
    'Historical analysis of platform migration patterns; measurement of switching costs as function of network size; comparison of exit rates before/after critical mass threshold',
    'If threshold is below current platform sizes: users are genuinely trapped (powerless perspective justified, snare classification). If threshold is higher: exit is constrained but possible (moderate perspective justified, tangled rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_feasibility_threshold, empirical, 'Network effect threshold for structural exit impossibility').

omega_variable(
    regulatory_effectiveness_lag,
    'Can regulatory responses (GDPR, DMA, consent requirements) outpace dark pattern innovation, or is regulatory coercion necessarily lagged?',
    'Timeline comparison between regulatory intervention and next-generation dark pattern deployment; analysis of regulatory arbitrage (non-EU platforms, gray-zone designs)',
    'If regulation can match innovation pace: scaffold classification with real sunset is justified. If regulatory lag is structural: regulation is tangled rope or piton, not scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness_lag, empirical, 'Whether regulation can outpace dark pattern innovation').

omega_variable(
    transparency_sufficiency,
    'Does algorithmic transparency (as required by DMA/GDPR) actually reduce dark pattern effectiveness, or does informed users still fall prey to manipulation?',
    'Measurement of user behavior after transparency disclosure; comparison of dark pattern effectiveness pre- vs. post-transparency; studies of informed vs. uninformed users',
    'If transparency sufficient: suppression metric should decrease with disclosure; regulatory intervention can reduce χ. If insufficient: transparency is theater; suppression remains high despite disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_sufficiency, empirical, 'Whether algorithmic transparency reduces dark pattern effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dark_patterns_manipulation, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dark_tr_t0, dark_patterns_manipulation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(dark_tr_t7, dark_patterns_manipulation, theater_ratio, 7, 0.48).
narrative_ontology:measurement(dark_tr_t14, dark_patterns_manipulation, theater_ratio, 14, 0.55).

% Extraction over time
narrative_ontology:measurement(dark_be_t0, dark_patterns_manipulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dark_be_t7, dark_patterns_manipulation, base_extractiveness, 7, 0.49).
narrative_ontology:measurement(dark_be_t14, dark_patterns_manipulation, base_extractiveness, 14, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dark_patterns_manipulation, resource_allocation).
narrative_ontology:affects_constraint(dark_patterns_manipulation, attention_economy_rent_extraction).
narrative_ontology:affects_constraint(dark_patterns_manipulation, behavioral_targeting_information_asymmetry).
narrative_ontology:affects_constraint(dark_patterns_manipulation, platform_switching_costs).

% DUAL FORMULATION NOTE:
% Dark patterns decompose into three structurally distinct constraints: (1) cognitive bias exploitation (ε≈0.55, snare from user perspective), (2) information asymmetry in platform mechanics (ε≈0.48, tangled rope between platforms and regulators), (3) network effects enabling exit barriers (ε≈0.65, snare for switching users). These are linked: dark patterns work because cognitive biases are exploitable AND users cannot switch AND platforms can hide mechanics. Each story gets its own ε and network of perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dark_patterns_manipulation, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
