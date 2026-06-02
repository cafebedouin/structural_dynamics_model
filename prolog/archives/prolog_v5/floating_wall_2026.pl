% ============================================================================
% CONSTRAINT STORY: floating_wall_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   China has deployed massive floating barriers of fishing vessels (up to
 *   2,000 boats forming 400+ km lines) near the Sino-Japanese median line in
 *   the East China Sea on at least two occasions (2016 and 2020), with
 *   preparations for repeat operations. The mechanism is: mobilize civilian
 *   fishing fleet through state direction (fuel subsidies, port controls,
 *   crew conscription via village leadership), concentrate them in contested
 *   waters to exclude Japanese fishing vessels and establish de facto
 *   exclusion zone, maintain plausible deniability by framing as 'civilian
 *   fishing activity,' sustain the blockade for weeks or months until
 *   political objectives are achieved or domestic pressure mounts. This
 *   constraint exhibits all the characteristics of a tangled rope: it serves
 *   a genuine coordination function (organizing China's maritime state to
 *   project power into contested waters), it extracts costs from multiple
 *   parties (Japanese fishers, freedom of navigation norm, alternative
 *   dispute resolution mechanisms), it requires active enforcement (state
 *   fuel subsidies, mandatory participation, coordinated command), and it
 *   operates through a hybrid of coordination and coercion. The theater ratio
 *   is high (0.68) because the entire operation depends on civilian framing
 *   that obscures state military strategy. The extractiveness is
 *   moderate-high (0.58) because the mechanism is effective but fragile — it
 *   works only against weaker opponents without military capacity to break
 *   the blockade, and only in a context of legal ambiguity about maritime
 *   boundaries.
 *
 * KEY AGENTS:
 *   - Chinese Maritime State Apparatus: Primary beneficiary (institutional/arbitrage) — uses floating wall as tool for territorial assertion without direct military escalation, solves coordination problem of maritime power projection
 *   - Chinese Fishing Vessel Operators & Crews: Complex status (powerless-to-moderate/trapped) — nominally mobilized as 'civilian' actors but subject to state direction, fuel subsidies, and mandatory participation; benefit from subsidies but bear deployment costs and collision risk
 *   - Japanese Coastal Fishing Communities: Primary victim (powerless/trapped) — excluded from traditional fishing grounds, cannot compete with 2,000-vessel formations, no exit except abandonment or years of exclusion
 *   - Japan's Maritime Self-Defense Force: Organized secondary actor (organized/constrained) — can monitor and escalate but constrained by political cost of military response against 'civilians'
 *   - Freedom of Navigation Regime / UNCLOS Framework: Abstract victim (powerless/trapped) — each blockade weakens precedent, cannot organize or enforce
 *   - UNCLOS-Based Maritime Governance Coalition: Organized reform actor (organized/constrained) — sees floating wall as deviation from norms, building alternative dispute resolution mechanisms with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — identifies the constraint as a tangled rope serving both genuine maritime coordination and asymmetric extraction
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
narrative_ontology:constraint_beneficiary(floating_wall_2026, chinese_maritime_state_apparatus).
narrative_ontology:constraint_beneficiary(floating_wall_2026, chinese_fishing_interests).
narrative_ontology:constraint_victim(floating_wall_2026, japanese_fishing_communities).
narrative_ontology:constraint_victim(floating_wall_2026, freedom_of_navigation_regime).
narrative_ontology:constraint_victim(floating_wall_2026, east_china_sea_maritime_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JAPANESE FISHING COMMUNITIES (SNARE) — Excluded from traditional fishing grounds by overwhelming numerical blockade. Cannot compete with 2,000-vessel formations. No exit option except abandoning ancestral fisheries or accepting years of exclusion. Experience maximal extraction with no offsetting coordination benefit. Suppression is total within the regional scope.
constraint_indexing:constraint_classification(floating_wall_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FREEDOM OF NAVIGATION REGIME (SNARE) — Abstract legal order cannot organize or exit. Each blockade weakens precedent for unobstructed passage. Successive floating walls accumulate extraction from the commons. No beneficiary status, no exit pathway, purely targeted for suppression. Theater-high: each wall performs 'civilian fishing' while executing coordinated state strategy.
constraint_indexing:constraint_classification(floating_wall_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: JAPAN'S MSDF (TANGLED ROPE) — Organized state actor with significant capacity. Experiences both extraction and coordination: the floating wall forces Japan into closer maritime monitoring and alliance coordination with U.S. Navy, but also restricts MSDF's own operational freedom in the zone. Can escalate or de-escalate, but constrained by political cost of armed intervention against 'fishing vessels.' Mixed experience: some imposed costs, some coordination benefits, but asymmetric — net extraction relative to pre-barrier baseline.
constraint_indexing:constraint_classification(floating_wall_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHINESE MARITIME STATE APPARATUS (ROPE) — Primary beneficiary. Mobilizes fishing fleet as tool for territorial assertion without direct military commitment. Solves the coordination problem of projecting state power into contested waters while maintaining plausible deniability ('civilian fishing activity'). Experiences constraint as enabling mechanism: the floating wall coordinates dispersed fishing vessels into unified barrier. Net beneficiary with minimal extraction cost — the constraint serves this actor's strategic interests directly.
constraint_indexing:constraint_classification(floating_wall_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: UNCLOS COALITION (SCAFFOLD) — International maritime law bodies, regional maritime dialogue mechanisms, and allied states view the floating wall as a temporary deviation from UNCLOS norms. The constraint has an implicit sunset: as maritime dispute resolution mechanisms (trilateral China-Japan-South Korea talks, expanded UNCLOS dispute mechanisms, increased naval presence norms) mature, the floating wall's exclusionary effect declines. Suppression is high in the immediate term, but the perspective classifies as scaffold because there is genuine forward progress toward alternative governance pathways and exit mechanisms for constrained actors.
constraint_indexing:constraint_classification(floating_wall_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: POST-WWII TERRITORIAL SETTLEMENT (PITON) — The floating wall exploits the degraded functional capacity of the post-WWII East China Sea settlement: the 1945 Cairo Declaration and 1952 San Francisco Treaty left the median line status ambiguous (Japan claims ECS median; China claims continental shelf extension). The settlement persists as a framework but its verification mechanism has atrophied — no enforcement body, no updated survey data, no binding arbitration. The floating wall is enabled by the inertial maintenance of an ambiguous legal structure that no longer functions. Theater-high: both sides cite different versions of the same treaties.
constraint_indexing:constraint_classification(floating_wall_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the floating wall serves a dual function: (1) coordination mechanism for China's maritime state in asserting jurisdictional claims; (2) extraction mechanism targeting Japanese fishing and freedom of navigation norm. Both functions are real. The constraint is neither a pure coordination problem (rope) nor a pure extraction mechanism (snare) — it is hybrid. Theater is high (civilian framing of state strategy), suppression is high (2,000-vessel blockade with military backing), extractiveness is moderate-high (0.58) because the mechanism works only under specific geopolitical conditions (power asymmetry, legal ambiguity, weak enforcement).
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
 *   Base extractiveness (0.58): High-moderate. The floating wall extracts significant costs from Japanese fishing communities (geographic exclusion from ancestral grounds) and the international maritime freedom norm (each deployment weakens the precedent for unobstructed passage). However, extractiveness is not extreme (0.70+) because the mechanism is temporary (weeks to months per deployment), subject to escalation risk if sustained too long, and dependent on legal ambiguity that may eventually be resolved. The temporal measurement shows extractiveness rising from 0.35 (early 2010s, isolated incidents) to 0.58 (2020s, established tactic) as the Chinese state refined the mechanism and normalized its use. Suppression (0.72): High. A 2,000-vessel blockade creates overwhelming numerical suppression of alternative exit options for Japanese fishers. The blockade is backed by implicit state military presence (naval support, weather coordination, safe passage guarantees). State direction of the fishing fleet (via subsidies, port controls, crew conscription) suppresses fishers' individual exit options from the mobilization. Theater ratio (0.68): High. The entire operation depends on maintaining the fiction that 2,000 fishing vessels independently chose to concentrate in a 400km line at a specific time for purposes of 'fishing' rather than state-directed blockade. The theater serves the constraint's function by enabling plausible deniability — Japan cannot respond with military force against 'civilians' without triggering international condemnation. Theater has increased from 0.48 to 0.68 over the interval as the operation became normalized and the civilian cover more practiced.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. Chinese maritime state apparatus sees Rope (coordination mechanism solving the problem of asserting maritime jurisdiction without direct military escalation). UNCLOS coalition sees Scaffold (temporary deviation from norms, with resolution pathways emerging as maritime governance mechanisms mature). Japanese fishing communities see Snare (pure exclusion with no offsetting benefits and no exit). The analytical observer sees Tangled Rope (genuine coordination function AND asymmetric extraction co-present). The post-WWII settlement system sees Piton (degraded legal framework persisting through inertia, its ambiguity exploited by the floating wall). This divergence is not due to measurement ambiguity — it reflects the constraint's true hybrid structure. The same structural phenomenon (coordinated fishing vessel deployment) solves a real coordination problem (maritime power projection) while simultaneously extracting from multiple parties (fishers, norm regime, alternative governance mechanisms). The constraint works precisely BECAUSE it serves both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The floating wall's directionality varies sharply across agent positions. Chinese maritime state apparatus: beneficiary + arbitrage exit → d ≈ 0.10 → low/negative f(d) → they experience it as enabling (negative χ, net benefit). Japanese fishing communities: victim + trapped exit → d ≈ 0.95 → high f(d) ≈ 1.42 → they experience maximum extraction. Japan's MSDF: victim (constrained response options) + organized power + constrained exit → d ≈ 0.65 → moderate f(d) ≈ 1.00 → moderate extraction. UNCLOS coalition: victim (norm weakening) + organized power + constrained exit (can build alternatives but slowly) → d ≈ 0.60 → moderate f(d) ≈ 0.85 → moderate extraction with visible exit pathway (Scaffold classification reflects this — organized agents see an exit path even if it takes decades). The constraint's stability depends on this directionality gap: the beneficiary experiences it as coordination while victims experience extraction. If the beneficiary's d shifted upward (if China bore costs of the operation) or the victims' d shifted downward (if they gained exit options), the classification would change.
 *
 * MANDATROPHY ANALYSIS:
 *   The floating wall resolves the mandatrophy — the fundamental question of whether it is a legitimate coordination mechanism or an illegitimate extraction mechanism — by being BOTH. At the analytical level, it is a tangled rope: genuine coordination function (maritime power projection solving China's problem of asserting jurisdiction without military escalation) + asymmetric extraction (imposing costs on fishers, weakening norm regime). Neither pure Rope nor pure Snare is adequate. The mandatrophy resolution comes from recognizing that 'is this coordination or extraction?' is answerable only relativistically — it depends on whose experience you are measuring. For Chinese maritime state: Rope (genuine coordination). For Japanese fishers: Snare (pure extraction). For UNCLOS coalition: Scaffold (temporary, with exit pathways). The analytical observer who insists on a single answer naturalizes one agent's perspective as universal truth — false summit risk. The constraint story honors the multiperspectival reality: the same mechanism serves coordination function for beneficiaries and extraction for victims, and this dual nature is what makes it a Tangled Rope rather than a pure form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    plausible_deniability_threshold,
    'At what scale of coordinated fishing vessel deployment does ''civilian fishing activity'' cross into recognized state military operation, triggering different legal regimes?',
    'International maritime law precedent review; analysis of past flotilla actions (Falklands, Hormuz, South China Sea); threshold identification from UNCLOS state practice',
    'If threshold < 1,000 vessels: floating wall is classified as state military operation (Snare for all perspectives). If threshold > 3,000: civilian framing prevails and classification shifts toward Rope for beneficiary perspectives. Current ambiguity enables the tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(plausible_deniability_threshold, conceptual, 'Threshold for civilian vs military classification of coordinated vessel flotillas').

omega_variable(
    median_line_legal_status,
    'Does the East China Sea median line constitute binding maritime boundary under UNCLOS Article 74, or is the continental shelf extension claim (China''s position) legally valid?',
    'UNCLOS dispute panel ruling; geological survey analysis of continental shelf configuration; state acceptance of binding arbitration mechanism',
    'If median line valid: floating wall is territorial exclusion (Snare). If continental shelf valid: floating wall is rights enforcement (Rope for China). Current legal ambiguity is the structural enabler of the constraint — resolution would degrade it or eliminate it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(median_line_legal_status, empirical, 'Legal validity of East China Sea median line vs continental shelf claim').

omega_variable(
    escalation_pathway_risk,
    'What is the probability that a floating wall deployment triggers accidental naval collision or deliberate military response that escalates beyond maritime militia operations?',
    'Historical incident frequency analysis; MSDF incident reports; modeling of collision risk from increased vessel density; analysis of rules of engagement ambiguity',
    'If escalation probability is low (< 5% per deployment): floating wall persists as a stable constraint. If high (> 20%): the mechanism becomes self-defeating (escalation cost exceeds benefit) and the constraint collapses. Current assessment: medium (10-15%), making floating walls a recurring tactic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(escalation_pathway_risk, empirical, 'Probability of military escalation from maritime militia flotilla operations').

omega_variable(
    fishing_economics_dependency,
    'How economically dependent are the Chinese fishing vessels on the government subsidies and market protections that make the floating wall mobilization feasible?',
    'Analysis of Chinese fishing fleet economics; subsidy structure documentation; profitability comparison with/without state support; vulnerability to subsidy reduction',
    'If dependency is high (>70%): floating wall is maintained by continuous state extraction from non-fishers. If low (<30%): vessels participate voluntarily and constraint is genuinely coordinated. Current evidence: high dependency (state fuel subsidies, forced crew participation, port-of-origin mandates). High dependency shifts the classification toward pure extraction (Snare from fishing communities perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fishing_economics_dependency, empirical, 'Economic dependence of Chinese fishing fleet on state subsidies enabling floating wall mobilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(floating_wall_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fwall_tr_t0, floating_wall_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(fwall_tr_t3, floating_wall_2026, theater_ratio, 3, 0.62).
narrative_ontology:measurement(fwall_tr_t6, floating_wall_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(fwall_be_t0, floating_wall_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fwall_be_t3, floating_wall_2026, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(fwall_be_t6, floating_wall_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(floating_wall_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(floating_wall_2026, south_china_sea_fishing_blockade).
narrative_ontology:affects_constraint(floating_wall_2026, east_china_sea_median_line_ambiguity).
narrative_ontology:affects_constraint(floating_wall_2026, unclos_dispute_resolution_capacity).

% DUAL FORMULATION NOTE:
% The floating wall constraint is enabled by (and feeds back into) the East China Sea median line legal ambiguity — the constraint would be impossible if UNCLOS median line status were clearly established. Network link documents this causal dependency. Downstream constraints (South China Sea fishing blockades, UNCLOS dispute capacity) are affected by the floating wall's success or failure in establishing precedent for maritime exclusion through civilian vessel deployment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(floating_wall_2026, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
