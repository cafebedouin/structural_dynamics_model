% ============================================================================
% CONSTRAINT STORY: digital_advertising_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_advertising_extraction, []).

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
 *   constraint_id: digital_advertising_extraction
 *   human_readable: Digital Advertising Extraction and Behavioral Capture
 *   domain: technology/economics/behavioral
 *
 * SUMMARY:
 *   Digital advertising platforms have evolved into the primary extraction
 *   architecture of the internet: behavioral data collection, algorithmic
 *   targeting, and attention capture create asymmetric value flows where
 *   users' autonomy and informational commons are extracted to fund free
 *   services. The constraint exhibits high extractiveness (0.68) and
 *   suppression (0.72) because users face genuine traps (material dependency
 *   on platforms for communication and opportunity) and cognitive locks
 *   (identity fusion with platform participation). The extractiveness
 *   trajectory shows steady increase over the interval, reflecting
 *   acceleration of behavioral profiling sophistication and reduction of
 *   alternative pathways. Theater ratio (0.58) reflects performative consent
 *   mechanisms (privacy policies, cookie banners) that provide regulatory
 *   cover without enabling informed choice. From the powerless user's
 *   perspective, this is a classical Snare: trapped, behavioral autonomy
 *   extracted, suppression mechanisms prevent exit. From the platform's
 *   perspective, it appears as Rope: coordination of supply and demand,
 *   market efficiency. The perspective gap reveals the structural asymmetry —
 *   the same mechanism is Rope to the beneficiary and Snare to the victim.
 *
 * KEY AGENTS:
 *   - Users (Powerless/Trapped): Primary victim — materially dependent on platforms for communication, employment, and social connection; behavioral autonomy continuously extracted
 *   - Users (Moderate/Identity-Locked): Secondary victim — structurally mobile but identity fused with platform membership; exit would require psychological identity reconstruction
 *   - Small Advertisers (Moderate/Constrained): Mixed victim-beneficiary — genuine coordination benefit (targeting) alongside asymmetric extraction (algorithmic opacity, account suspension risk, cost inflation)
 *   - Advertising Platforms (Institutional/Arbitrage): Primary beneficiary — extract behavioral surplus, arbitrage between user privacy expectations and advertiser demand, maintain opacity to preserve extraction margins
 *   - Data Brokers (Institutional/Arbitrage): Secondary beneficiary — extract and resell behavioral profiles; operate in regulatory shadows
 *   - Privacy Regulation Coalition (Organized/Constrained): Agent of sunset — GDPR, DMA, and privacy advocates building regulatory frameworks that reduce extraction mechanisms over generational timescale
 *   - Privacy Policy Ritual (Institutional/Constrained): Degraded institution — formal consent mechanisms maintain legal cover while enabling de facto non-consent; theater persists because functional transparency would collapse extraction
 *   - Analytical Observer (Analytical/Analytical): Sees full architecture from civilizational perspective; behavioral extraction mechanisms become more sophisticated faster than regulation closes gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_advertising_extraction, 0.68).
domain_priors:suppression_score(digital_advertising_extraction, 0.72).
domain_priors:theater_ratio(digital_advertising_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_advertising_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(digital_advertising_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(digital_advertising_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_advertising_extraction, snare).
narrative_ontology:human_readable(digital_advertising_extraction, "Digital Advertising Extraction and Behavioral Capture").
narrative_ontology:topic_domain(digital_advertising_extraction, "technology/economics/behavioral").

domain_priors:requires_active_enforcement(digital_advertising_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_advertising_extraction, advertising_platforms).
narrative_ontology:constraint_beneficiary(digital_advertising_extraction, data_brokers).
narrative_ontology:constraint_victim(digital_advertising_extraction, users_attention_autonomy).
narrative_ontology:constraint_victim(digital_advertising_extraction, advertisers_sme).
narrative_ontology:constraint_victim(digital_advertising_extraction, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: USER ATTENTION (SNARE) — Trapped by necessity of digital participation. Cannot escape platform dependency (email, social connection, job search all require platform presence). Behavioral data extracted continuously; algorithmic targeting removes genuine choice. No real alternatives; exit cost is social/economic isolation. Maximum extraction with maximum suppression.
constraint_indexing:constraint_classification(digital_advertising_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IDENTITY-LOCKED USER (SNARE) — Structurally mobile (could delete account) but identity fused with platform participation. Social identity, professional presence, relational continuity constituted through platform membership. Exit requires abandoning identity constructed within the platform ecosystem. Behavioral autonomy extraction is cognitive; suppression is enforced by identity frame rather than material barriers.
constraint_indexing:constraint_classification(digital_advertising_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: SMALL ADVERTISER (TANGLED ROPE) — Mixed experience: platforms provide genuine coordination (targeting small customer bases, demand-side efficiency) alongside asymmetric extraction (algorithmic opacity, account suspension risk, cost inflation via bidding wars). Can exit by switching platforms or using direct marketing, but cost is high (audience loss, learning curve). Moderate extraction with real suppression from lock-in.
constraint_indexing:constraint_classification(digital_advertising_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVERTISING PLATFORM (ROPE) — Experiences the constraint as pure coordination: matching advertisers to users, enabling market efficiency. Extract behavioral surplus value but frame it as market function. Can arbitrage between user privacy expectations and advertiser demand. Net beneficiary with high degrees of freedom.
constraint_indexing:constraint_classification(digital_advertising_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY REGULATION (SCAFFOLD) — Organized agents (GDPR, DMA, privacy advocates) see extractive data practices as a solvable problem with sunset logic. Regulatory frameworks establish consent requirements, data portability, and algorithmic transparency that reduce extraction mechanisms. Suppression declines as technical compliance measures mature. High theater (compliance theater, dark patterns) but genuine extraction reduction over generational timescale.
constraint_indexing:constraint_classification(digital_advertising_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVACY POLICY THEATER (PITON) — Formal consent mechanisms (privacy policies, cookie banners, consent walls) are largely performative. Users do not read policies; dark patterns optimize for accept; technical language obscures actual data flows. The ritual persists because it provides regulatory cover, not because it enables informed consent. Theater ratio is high (0.58 baseline, driven by performative compliance). Degraded institution maintained by legal requirement, not functional verification of consent.
constraint_indexing:constraint_classification(digital_advertising_extraction, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — Sees the full architecture: behavioral extraction at scale, algorithmic opacity, suppression mechanisms preventing informed exit, multi-layer extraction (attention, data, psychological nudging). From civilizational view, the asymmetry is structural and deepening. Extraction mechanisms become more sophisticated (machine learning behavioral profiling) faster than regulation closes loopholes. Classification remains Snare despite attempts to frame as coordination or natural market efficiency.
constraint_indexing:constraint_classification(digital_advertising_extraction, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_advertising_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_advertising_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_advertising_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_advertising_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_advertising_extraction, TR),
    TR >= 0.70.

:- end_tests(digital_advertising_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting sustained behavioral autonomy extraction. The value increased from 0.35 to 0.68 over the interval as machine learning targeting became more sophisticated and behavioral profiling granularity increased. Users perceive diminishing agency over time as algorithmic systems become better at predicting and nudging behavior. Suppression (0.72): High, reflecting multiple overlapping barriers to exit. Material dependency (communication, employment, social opportunity), cognitive lock-in (identity fusion), network effects (switching cost), and designed addictive mechanisms all elevate suppression. The measurement trajectory (0.43→0.50→0.58) for theater ratio indicates that performative consent mechanisms have become more elaborate over the interval, not more functional — dark patterns, consent walls, and pseudo-transparency increase the theatrical coating while extraction mechanisms themselves remain opaque. The snare classification is driven by high extractiveness and suppression with no genuine coordination benefit to the victims (users); the beneficiaries (platforms) experience the same architecture as rope. The perspectival gap is the diagnostic signal of asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between the platform (Rope) and the user (Snare) — they are experiencing the same mechanism but with opposite directionalities. The platform sees coordination (matching supply to demand, enabling market efficiency, providing free services). The user sees extraction (attention captured, behavioral data harvested, choice removed by algorithmic nudging). The small advertiser occupies the middle ground (Tangled Rope) — they genuinely benefit from targeting coordination but also experience extraction through opacity and account vulnerability. The analytical observer (Snare) aligns with the powerless user but sees the full structural architecture, recognizing that the platform's rope perception is available only because platforms control the information asymmetry and can choose what extraction mechanisms are visible or invisible. The privacy regulation perspective (Scaffold) sees the constraint as solvable through sunset mechanisms (regulatory frameworks), but this assumes effective compliance — if GDPR/DMA merely shift extraction to dark patterns and data intermediaries rather than reducing it, the scaffold is aspirational rather than structural. The privacy policy perspective (Piton) observes that the formal consent ritual is mostly theater — users don't read policies, dark patterns optimize for acceptance, technical language obscures actual data flows. The ritual persists because it provides legal protection to platforms, not because it enables informed consent.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain flows from beneficiary/victim declarations through power and exit options. Users with trapped exit experience d ≈ 0.90-0.95, producing maximum f(d) ≈ 1.35-1.42 and thus maximum effective extraction chi. Users with identity_locked exit experience d ≈ 0.85-0.90 (high because victim status, but structurally mobile), producing f(d) ≈ 1.15-1.28 — still extremely high extraction but slightly lower than trapped because the binding mechanism is cognitive (can theoretically break with frame shift) rather than material (cannot break with resources alone). Platforms as beneficiaries with arbitrage exit experience d ≈ 0.05-0.15 (low because beneficiary status + high exit freedom), producing f(d) ≈ -0.12 to 0.05 — negative or near-zero experienced extraction, consistent with their rope perception. Small advertisers with constrained exit experience d ≈ 0.50-0.60 (mixed victim-beneficiary status), producing moderate chi, consistent with tangled rope. The scope modifier σ(S) applies at global scope (1.2), amplifying extractiveness across all indices — the constraint operates at planetary scale, making verification of alternative arrangements impossible for any single user or advertiser to escape. This scale multiplier is critical to understanding why local or national regulatory interventions face such difficulty: the coordination lock is global, so local opt-out is economically irrational even when technically possible.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that platforms genuinely provide coordination benefits (matching advertisers to users, reducing search costs, enabling markets) while simultaneously and structurally extracting behavioral surplus from users who experience no benefit and cannot exit. The constraint is not 'Is this extraction or coordination?' but 'At what point does the extraction become so asymmetric that the coordination framing becomes a cover story?' The answer lies in the disparity between platform control (high algorithmic opacity, dark patterns, account suspension power) and user agency (no transparency, no real consent, no exit). The tangled rope classification for small advertisers confirms that coordination benefits CAN be real — they experience genuine matching efficiency. But for users, the coordination benefit is fictional: they do not benefit from being matched to ads; the platform benefits from their data. The theater ratio (0.58) indicates that roughly 58% of the visible constraint mechanism is performative (consent rituals, privacy policies, compliance theater) while 42% is functional (actual targeting systems, data infrastructure). This suggests that reducing theater through regulatory enforcement (GDPR's transparency requirements, consent mechanisms) can expose the underlying extraction architecture — users who break through the theater to understand actual data flows typically attempt exit despite barriers. The privacy regulation scaffold perspective is credible only if regulatory pressure continues to erode the extraction mechanisms rather than being captured by platform influence (regulatory arbitrage, revolving-door employment, lobbying success). If regulation becomes capturadas part of the snare, it transforms into piton (degraded ritual) rather than scaffold (genuine sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_autonomy_threshold,
    'At what level of algorithmic targeting does user behavior cease to be self-directed choice and become responsive to extraction mechanisms?',
    'Neuroscience/behavioral economics analysis of attention capture, addictive design metrics, and autonomy-impairing notification patterns. Comparative A/B testing of informed vs uninformed user behavior.',
    'If threshold is high: much current behavior attributable to coordination benefits (matching to preferences). If threshold is low: most targeted behavior is extracted, not chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_autonomy_threshold, empirical, 'Threshold for behavioral autonomy loss to algorithmic extraction').

omega_variable(
    privacy_regulation_effectiveness,
    'Do GDPR/DMA compliance measures actually reduce data extraction or merely shift extraction mechanisms (dark patterns, consent walls, data intermediaries)?',
    'Longitudinal measurement of data collection volumes, consent rates, dark pattern prevalence, and user behavior changes post-regulation. Audit of platform compliance vs technical data hoarding.',
    'If effective: scaffold perspective valid, sunset mechanisms exist. If ineffective: extraction continues, suppression merely adapts, regulatory theater is piton not scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privacy_regulation_effectiveness, empirical, 'Whether regulation reduces extraction or shifts mechanisms').

omega_variable(
    alternative_platform_viability,
    'Do decentralized or regulation-compliant alternatives (Signal, DuckDuckGo, federated social networks) offer genuine exit or are they marginal platforms lacking network effects?',
    'Adoption curve analysis, cost comparison, feature parity assessment, network effect threshold measurement for each alternative class.',
    'If viable: exit_options upgrade from trapped to constrained or mobile. If marginal: trapped classification holds, suppression reflects genuine structural dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Viability of alternative platforms as genuine exit options').

omega_variable(
    identity_lock_mechanism_scale,
    'What proportion of platform users are materially trapped vs identity-locked? Are identity-lock and material dependency separable in practice?',
    'Qualitative interviews with users in different demographic and socioeconomic categories about perceived barriers to exit. Post-deletion follow-up on whether identity-lock persists after material exit.',
    'If primarily identity-locked: intervention targets identity frame, not regulation. If primarily materially trapped: regulation reducing economic dependency will reduce suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_scale, empirical, 'Proportion and interaction of material vs cognitive traps').

omega_variable(
    extraction_surplus_distribution,
    'What fraction of extracted value is retained by platform (profit), what is returned to advertisers (efficiency gain), and what is lost to rent-seeking (algorithmic opacity tax)?',
    'Financial analysis of platform margins, advertiser ROI data, and comparison with hypothetical transparent-pricing market equilibrium.',
    'If most extraction is redistributed: snare classification may be overstated (asymmetric but coordinated). If most is retained: snare confirmed, extraction is pure rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_surplus_distribution, empirical, 'Distribution of extracted surplus across stakeholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_advertising_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digad_tr_t0, digital_advertising_extraction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(digad_tr_t5, digital_advertising_extraction, theater_ratio, 5, 0.5).
narrative_ontology:measurement(digad_tr_t10, digital_advertising_extraction, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(digad_be_t0, digital_advertising_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digad_be_t5, digital_advertising_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(digad_be_t10, digital_advertising_extraction, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(digad_be_t2, digital_advertising_extraction, base_extractiveness, 2, 0.43).
narrative_ontology:measurement(digad_be_t7, digital_advertising_extraction, base_extractiveness, 7, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_advertising_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_advertising_extraction, 0.25).
narrative_ontology:affects_constraint(digital_advertising_extraction, attention_economy_extraction).
narrative_ontology:affects_constraint(digital_advertising_extraction, algorithmic_opacity_lock).
narrative_ontology:affects_constraint(digital_advertising_extraction, behavioral_autonomy_suppression).

% DUAL FORMULATION NOTE:
% Digital advertising extraction decomposes into three structurally distinct constraints: (1) resource allocation coordination (matching supply/demand, genuine market function with ε≈0.25) bundled with (2) behavioral autonomy extraction through algorithmic targeting (ε≈0.65) and (3) information asymmetry exploitation through opacity (ε≈0.72). The family treats the coordination and extraction as separate constraints; the present story bundles them as a snare where extraction dominates coordination. See attention_economy_extraction for pure-coordination view and algorithmic_opacity_lock for opacity-focused extraction analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_advertising_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
