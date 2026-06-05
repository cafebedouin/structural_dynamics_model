% ============================================================================
% CONSTRAINT STORY: floating_wall_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_floating_wall_2026, []).

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
 *   constraint_id: floating_wall_2026
 *   human_readable: The East China Sea Maritime Militia Barrier
 *   domain: geopolitical/maritime
 *
 * SUMMARY:
 *   China's deployment of massive fishing vessel 'flotillas' forming 400+
 *   kilometer floating walls near the Sino-Japanese median line represents a
 *   novel form of maritime coercion that blurs the distinction between
 *   civilian activity, state agency, and military operations. The constraint
 *   exhibits tangled rope structure: it serves a genuine coordination
 *   function for China (projecting state presence without direct military
 *   escalation, mobilizing dispersed fishing interests into collective
 *   action) while simultaneously extracting access denial and political
 *   concessions from Japan and regional observers. The fishing vessels are
 *   ostensibly independent operators pursuing commercial activity, yet their
 *   coordinated formation into barriers at strategic moments indicates state
 *   direction. Japanese authorities face a structural dilemma: the militia
 *   barrier is not a military asset (subject to conventional deterrence) but
 *   also not purely civilian activity (subject to normal maritime law). The
 *   theater_ratio reflects this ambiguity — the flotilla is simultaneously a
 *   functioning coercive mechanism and a performance of civilian maritime
 *   activity. Extractiveness has increased over the interval as operational
 *   sophistication and frequency of deployments have grown, suggesting
 *   accumulated learning in the coercion mechanism.
 *
 * KEY AGENTS:
 *   - Chinese State & Militia Operators: Primary beneficiary (institutional/arbitrage) — extends state power projection capacity without direct military escalation; gains access denial and political concessions
 *   - Japanese Fishing Fleet: Primary victim (powerless/trapped) — cannot access traditional fishing grounds without confrontation risk; lacks recourse mechanisms
 *   - Japanese Coast Guard & Maritime Authority: Secondary victim (moderate/constrained) — constrained by rules of engagement and escalation risk; extracts political cost but insufficient to break barrier
 *   - ASEAN Maritime States: Tertiary victims (organized/constrained) — limited by economic dependence on China; face coercive spillover but maintain coordination benefits with China
 *   - International Maritime Law Framework: Institutional actor (institutional/arbitrage) — nominally applies but degraded in enforcement; theater persists while practice diverges
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies dual coordination-extraction function; sees constraint as hybrid rather than pure coercion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(floating_wall_2026, 0.58).
domain_priors:suppression_score(floating_wall_2026, 0.72).
domain_priors:theater_ratio(floating_wall_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(floating_wall_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(floating_wall_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(floating_wall_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(floating_wall_2026, tangled_rope).
narrative_ontology:human_readable(floating_wall_2026, "The East China Sea Maritime Militia Barrier").
narrative_ontology:topic_domain(floating_wall_2026, "geopolitical/maritime").

domain_priors:requires_active_enforcement(floating_wall_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(floating_wall_2026, chinese_state_capacity).
narrative_ontology:constraint_beneficiary(floating_wall_2026, chinese_maritime_interests).
narrative_ontology:constraint_victim(floating_wall_2026, japanese_maritime_access).
narrative_ontology:constraint_victim(floating_wall_2026, regional_freedom_of_navigation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JAPANESE FISHING FLEET (SNARE) — Cannot access traditional fishing grounds without confrontation risk. Militia wall presents as physical/economic barrier with no legitimate coordination benefit. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.72. Trapped exit + victim status produces maximum effective extraction.
constraint_indexing:constraint_classification(floating_wall_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JAPANESE COAST GUARD (SNARE) — Constrained by operational rules of engagement, political escalation risk, and resource limits. Must manage confrontations without conventional military response. Militia barrier extracts political concessions and operational freedom. d≈0.88, f(d)≈1.33, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(floating_wall_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ASEAN MARITIME STATES (TANGLED ROPE) — Constrained by economic dependence on Chinese markets and desire to avoid confrontation. But also benefit from Chinese willingness to coordinate on certain marine resource issues and infrastructure projects. Mixed extraction and coordination. d≈0.65, f(d)≈0.95, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHINESE STATE & MILITIA (ROPE) — Primary beneficiary. Militia system serves coordination function: mobilizing fishing vessels into floating barriers extends state presence without direct military escalation. Provides low-cost, deniable coercive capability. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary perspective; sees constraint as coordination mechanism for power projection.
constraint_indexing:constraint_classification(floating_wall_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL MARITIME LAW (PITON) — UNCLOS and freedom of navigation norms nominally apply but are substantially theatrical in enforcement. The legal framework persists as international theater while ground reality (militia barrier) operates in space of ambiguous sovereignty. theater_ratio=0.68 indicates performative dimension. Law exists; enforcement is degraded. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(floating_wall_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the militia barrier combines coordination (extending state capacity into disputed waters) with extraction (denying Japanese access). The constraint exhibits genuine dual function: it solves China's problem of projecting power in ambiguous sovereignty zones while simultaneously extracting access denial from Japan. Both functions are structural, not secondary. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59. Engine confirms tangled_rope classification at analytical level.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(floating_wall_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(floating_wall_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(floating_wall_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(floating_wall_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(floating_wall_2026, TR),
    TR >= 0.70.

:- end_tests(floating_wall_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The militia barrier denies Japanese maritime access and compels political concessions (recognition of Chinese claims, coordination on disputed waters). However, extraction is not maximal because: (1) alternative fishing grounds exist at higher cost rather than impossible access, (2) Japan retains coast guard presence and can monitor/document violations, (3) international attention creates some cost to China. The value reflects significant but not absolute extraction. Suppression (0.72): High. Multiple barriers to Japanese response: (a) militia vessels are nominally civilian, making military response costly politically, (b) China maintains deniability of operational control, (c) escalation risks economic/military consequences disproportionate to fishing access, (d) international law enforcement is weak in disputed waters. Theater ratio (0.68): High. The flotilla operates as simultaneous performance of civilian maritime activity and military coercion. The 'fishing vessel' framing is essential to the coercive mechanism — treating them as military assets would expose China to different legal/diplomatic costs. The theater has increased over time as operational sophistication makes the performance more elaborate (sustained coordination, timed deployments, engagement protocols).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence between victim and beneficiary perspectives. Japanese fishing fleet sees pure snare (extraction without coordination benefit). Chinese state sees rope/coordination mechanism for extending state capacity at low cost. ASEAN sees tangled rope (constrained by Chinese economic/political leverage but also benefiting from infrastructure and market access). International maritime law sees piton (framework persists theatrically while enforcement degrades). The analytical observer at civilizational scale sees genuine dual function (tangled rope confirmed) — China is simultaneously solving its own problem (low-cost state projection) and extracting from Japan (access denial). The perspectival gap is not resolvable by choosing a 'correct' view; it reflects the actual structural ambiguity: the constraint IS both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Japanese fishing fleet: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Chinese state: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Japanese coast guard: Victim + constrained → d≈0.88, f(d)≈1.33. High extraction but constrained by escalation risk. ASEAN: Mixed + constrained → d≈0.65, f(d)≈0.95. Moderate extraction offset by coordination benefits. International law: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Framework persists without enforcement. Analytical: observer → d≈0.68, f(d)≈1.02. Confirms tangled_rope at civilizational scale.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by identifying genuine dual function. The critical mandatrophy question is: 'Is this coordination with extraction, or extraction masquerading as coordination?' Structural analysis confirms both are real: (1) Coordination function: China mobilizes dispersed fishing interests (thousands of independent vessels) into collective action for state projection. This solves a real coordination problem — individual vessels cannot exert state presence, but coordinated flotilla achieves it. The coordination is genuine, not theater. (2) Extraction function: The same coordinated action denies Japanese maritime access and compels political concessions. This extraction is not byproduct; it is primary goal. Both functions are structural, not secondary. The constraint cannot be reclassified as pure coordination (rope) because suppression is too high (0.72) and victims exist with trapped/constrained exit. It cannot be reclassified as pure snare because beneficiaries (Chinese state) genuinely benefit from coordination mechanism, not merely from extraction. Theater ratio (0.68) indicates significant performative dimension but not Piton-level degradation (≥0.70). The tangled_rope classification holds: 0.40 ≤ χ ≤ 0.90 (χ≈0.59 at analytical level), base extraction ε≥0.30 (ε=0.58), suppression ≥0.40 (suppression=0.72), beneficiaries present, victims present, active enforcement required. All gates satisfied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_state_control_threshold,
    'At what operational autonomy level does the maritime militia transition from plausible deniable proxy to overt state tool?',
    'Analysis of command structure, fuel provisioning, tactical coordination patterns, communication infrastructure; comparison with known state-controlled military operations',
    'If fully state-controlled: snare classification from all perspectives becomes dominant (pure coercion). If semi-autonomous: tangled_rope holds (coordination + extraction hybrid). If truly independent: classification shifts toward rope (coordination problem).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_state_control_threshold, empirical, 'Degree of state control over maritime militia operational autonomy').

omega_variable(
    japanese_escalation_redline,
    'Does Japan have a credible military escalation option that would cause militia wall formation to cease, and at what threshold?',
    'Gaming of military scenarios; assessment of Japanese political tolerance for naval conflict; historical analysis of previous confrontation patterns and their resolution',
    'If escalation threshold is clear and credible: victims transition from trapped to constrained exit. If no credible escalation option: trapped exit remains; snare strengthens. If threshold is ambiguous: constrained exit becomes more likely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(japanese_escalation_redline, empirical, 'Existence and credibility of Japanese military escalation threshold').

omega_variable(
    fishing_vessel_coordination_mechanism,
    'Are the 2,000 fishing vessels organized through economic incentives (subsidies, protection guarantees, preferential access) or through direct command? How much is coordination vs coercion?',
    'Analysis of financial flows to militia participants; interviews with fishing crews; comparison of militia participation rates to economic incentive structures; assessment of penalties for non-participation',
    'If primarily incentive-driven: coordination component (rope/tangled_rope) is genuine; suppression is lower. If command-driven: pure coercion (snare) dominates; theater may be higher (performance of civilian purpose). If mixed: current tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fishing_vessel_coordination_mechanism, empirical, 'Balance between economic incentive and command structures in militia participation').

omega_variable(
    median_line_legitimacy_contest,
    'Is the Sino-Japanese median line itself a legitimate boundary or a contested claim? If contested, is the militia barrier defending a claim or establishing one?',
    'Legal analysis of EEZ delimitation principles; historical record of maritime agreements; assessment of which party initiated the median line as boundary; examination of third-party recognition',
    'If Japan legitimately controls median line: militia barrier is clear extraction (snare). If boundary is genuinely contested: militia barrier becomes coordination mechanism for establishing claim (rope becomes viable). If both parties have plausible claims: constraint becomes symmetric (perspectives converge).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(median_line_legitimacy_contest, conceptual, 'Legitimacy status of the Sino-Japanese median line boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(floating_wall_2026, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fwall_tr_t0, floating_wall_2026, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fwall_tr_t7, floating_wall_2026, theater_ratio, 7, 0.62).
narrative_ontology:measurement(fwall_tr_t14, floating_wall_2026, theater_ratio, 14, 0.68).

% Extraction over time
narrative_ontology:measurement(fwall_be_t0, floating_wall_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fwall_be_t7, floating_wall_2026, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(fwall_be_t14, floating_wall_2026, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(floating_wall_2026, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(floating_wall_2026, 0.35).
narrative_ontology:affects_constraint(floating_wall_2026, sino_japanese_eez_delimitation).
narrative_ontology:affects_constraint(floating_wall_2026, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(floating_wall_2026, chinese_coast_guard_institutional_capacity).

% DUAL FORMULATION NOTE:
% The floating wall constraint is downstream of the Sino-Japanese maritime boundary dispute but represents a distinct structural innovation in coercive mechanism design. EEZ delimitation claims (ε≈0.15, mountain/rope) provide the upstream legitimacy claim; the floating wall (ε=0.58, tangled_rope) operationalizes coercion through civilian-military hybrid means. The network link captures how unresolved boundary disputes create conditions for novel constraint architectures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(floating_wall_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
