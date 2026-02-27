% ============================================================================
% CONSTRAINT STORY: streaming_bundling_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_streaming_bundling_mandate, []).

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
 *   constraint_id: streaming_bundling_mandate
 *   human_readable: Mandatory Streaming Bundling Mandate
 *   domain: economic/media_regulation
 *
 * SUMMARY:
 *   A mandatory streaming bundling mandate represents a regulatory
 *   intervention that forces streaming platforms to offer content exclusively
 *   through bundled packages, prohibiting standalone subscriptions. This
 *   constraint exhibits classic tangled rope dynamics: it solves a genuine
 *   coordination problem (reducing consumer choice paralysis, stabilizing
 *   industry revenue) while simultaneously enforcing asymmetric extraction
 *   (from niche creators and budget-conscious consumers). The constraint
 *   redistributes rent from direct-to-consumer platforms and independent
 *   creators toward legacy cable operators and large media conglomerates
 *   seeking to reverse cord-cutting. The theater ratio (0.52) reflects that
 *   bundling's efficiency claims are partially functional (cross-promotion
 *   and discoverability) and partially performative (protecting incumbent
 *   market position). The extractiveness (0.58) shows meaningful but not
 *   dominant extraction — the mandate is not a pure snare, because some
 *   consumers benefit from bundling (reduced choice paralysis, lower
 *   per-title costs for heavy users), and some platforms benefit from
 *   improved revenue stability. However, the suppression (0.68) is
 *   substantial: regulatory prohibition on unbundling removes alternatives
 *   entirely, creating trapped exit for those harmed.
 *
 * KEY AGENTS:
 *   - Niche Content Creators: Primary victim (powerless/trapped) — independent studios, vertical creators, and smaller platforms forced into low-visibility bundles with limited direct consumer access
 *   - Budget-Conscious Consumers: Primary victim (powerless/trapped) — users who previously selected specific channels are legally prohibited from à la carte purchasing, forced to buy bundles
 *   - Direct-to-Consumer Platforms: Secondary victim (moderate/constrained) — Netflix, Disney+, Apple TV+ lose pricing power and market differentiation through forced bundling with competitors
 *   - Legacy Cable Operators: Primary beneficiary (institutional/arbitrage) — Comcast, Charter, AT&T replicate traditional cable model, improve subscriber retention, cross-sell opportunities
 *   - Regulatory Body: Mixed (powerful/arbitrage) — enforces mandate, experiences hybrid coordination-extraction logic, has power to modify or repeal
 *   - Large Content Studios: Secondary beneficiary (institutional/constrained) — Warner Bros., Paramount, Disney use bundling for revenue stability and antitrust-adjacent control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(streaming_bundling_mandate, 0.58).
domain_priors:suppression_score(streaming_bundling_mandate, 0.68).
domain_priors:theater_ratio(streaming_bundling_mandate, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(streaming_bundling_mandate, extractiveness, 0.58).
narrative_ontology:constraint_metric(streaming_bundling_mandate, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(streaming_bundling_mandate, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(streaming_bundling_mandate, tangled_rope).
narrative_ontology:human_readable(streaming_bundling_mandate, "Mandatory Streaming Bundling Mandate").
narrative_ontology:topic_domain(streaming_bundling_mandate, "economic/media_regulation").

domain_priors:requires_active_enforcement(streaming_bundling_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(streaming_bundling_mandate, legacy_cable_operators).
narrative_ontology:constraint_beneficiary(streaming_bundling_mandate, bundled_content_aggregators).
narrative_ontology:constraint_victim(streaming_bundling_mandate, niche_content_providers).
narrative_ontology:constraint_victim(streaming_bundling_mandate, budget_conscious_consumers).
narrative_ontology:constraint_victim(streaming_bundling_mandate, direct_to_consumer_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NICHE CONTENT CREATOR (SNARE) — Small streaming producers (independent documentary studios, niche comedy channels, vertical content creators) cannot exit the mandate: regulatory law forces them into bundles they didn't choose, fragmenting their audience and destroying direct relationships. They bear suppression (no alternative to bundling), extraction (forced into discount-tier bundles), and have no recourse. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(streaming_bundling_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDGET-CONSCIOUS CONSUMER (SNARE) — Consumers who previously subscribed only to specific channels (e.g., HBO Max for one show, Apple TV+ for another) are forced to buy bundles, paying for content they don't want. Trapped: legal prohibition on à la carte options. Suppressed: no alternative. Extracted from: forced overpayment. d≈0.90, f(d)≈1.35, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(streaming_bundling_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: DIRECT-TO-CONSUMER PLATFORM (TANGLED ROPE) — Netflix, Disney+, or Apple TV+ experience the mandate as both coordination failure and extraction. Coordination function: bundling reduces consumer choice paralysis and can increase discoverability (consumers try bundled content they wouldn't select alone). Extraction: forced into bundles with competitors, losing pricing power and differentiation. Exit options constrained: cannot leave the national market without losing scale. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(streaming_bundling_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGACY CABLE OPERATOR (ROPE) — Large telecom/cable operators (Comcast, Charter, AT&T) see the mandate as pure coordination: bundling replicates their traditional cable model, reduces churn, and enables cross-selling. They have arbitrage exit: can lobby for carve-outs, can shift to mobile/broadband if streaming fails. Bundling solves their coordination problem (how to retain subscribers). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(streaming_bundling_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY BODY (TANGLED ROPE) — The agency mandating bundling experiences hybrid logic: coordination function (reduce consumer confusion, stabilize industry revenue), but also enforced extraction (mandates protect legacy operators against disruption, artificially sustaining their position). The regulator has powerful agents and arbitrage options (can repeal, can modify), but the mandate itself creates asymmetry — those regulated cannot exit. d≈0.40, f(d)≈0.40, σ=1.0 → χ≈0.23.
constraint_indexing:constraint_classification(streaming_bundling_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONTENT INDUSTRY INCUMBENT (PITON) — Established studios (Warner Bros., Paramount, Disney) see the mandate as degraded protection. They once required bundling to enforce windowing (theatrical → cable → streaming). The mandate continues bundling logic after its economic function has decayed — studios now use bundling performatively to maintain antitrust appearances rather than for real market control. theater_ratio ≈ 0.52 (mid-range): bundling is partially functional (cross-promotion), partially performative (anticompetitive theater). χ ≈ 0.30 from their perspective (constrained + powerful = moderate extraction, but weak exit option).
constraint_indexing:constraint_classification(streaming_bundling_mandate, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: OPEN INTERNET COALITION (SCAFFOLD) — Consumer advocacy groups, independent creators, and tech platforms organized globally see the mandate as temporary coordination failure with a sunset. Technology enables unbundling (algorithms improve discoverability, mobile platforms fracture traditional distribution), and regulatory opposition is mobilizing. The coalition has mobile exit (international platforms, alternative distribution), time horizon for sunset is generational (10-15 years for regulatory regime to shift). d≈0.35, f(d)≈0.35, σ=1.2 → χ≈0.15.
constraint_indexing:constraint_classification(streaming_bundling_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(streaming_bundling_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(streaming_bundling_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(streaming_bundling_mandate, TR),
    TR >= 0.70.

:- end_tests(streaming_bundling_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mandate extracts from niche creators and budget consumers through suppression of unbundling (complete prohibition), but extraction is not maximal because: (1) some consumers benefit from bundling (reduced decision paralysis, lower per-title cost for heavy users), (2) large platforms retain scale advantages despite bundling, (3) international competition limits rent capture (consumers with VPNs or international accounts can arbitrage). The measurement trajectory shows increasing extractiveness over 6 years (0.32→0.58), indicating that bundling's initial efficiency benefits (cross-promotion, discovery) decay while its protective function hardens (legacy operators consolidate subscriber bases through mandatory bundling). Suppression (0.68): High. Regulatory prohibition on unbundling removes the primary alternative. Consumers and creators cannot opt out; enforcement is automatic through regulatory mandate. No legal à la carte pathway exists. Theater ratio (0.52): Moderate. Bundling is partially functional (genuine cross-promotion and algorithmic discovery improve consumer experience for some users, reduce decision paralysis) and partially performative (maintains antitrust appearance, protects incumbents against disruption, creates artificial rent protection). The ratio has increased over time (0.38→0.52) as the efficiency benefits plateau while the protective theater hardens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Niche creators and budget consumers perceive pure snare (trapped exit, suppression, extraction). Direct-to-consumer platforms perceive tangled rope (mixed coordination via bundling's genuine discovery function, mixed extraction via forced participation and lost pricing power). Legacy operators perceive pure rope (genuine coordination problem solved: subscriber retention, cross-selling). The regulatory body perceives tangled rope (coordination function justified, but enforcement creates asymmetry). Content studios perceive piton (bundling was once functional market control; now degraded to performative antitrust theater). The coalitions opposing the mandate perceive scaffold (temporary coordination failure, sunset visible through technological unbundling and regulatory opposition). The analytical observer risks mountain classification (bundling is 'inherent to media economics'), but the structural data reveals this as false summit: extractiveness increased over time, theater ratio is moderate (not nearly invisible), and beneficiaries are identifiable institutional actors, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Niche creators: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit, cannot organize, no alternatives. Budget consumers: Victim + trapped → d≈0.90, f(d)≈1.35. Near-maximum extraction — legal prohibition on preferred option. Direct-to-consumer platforms: Victim + constrained (can theoretically exit market or fight regulation, but practically constrained by scale economics and regulatory hostility) → d≈0.65, f(d)≈0.95. Substantial extraction but not maximal because they have power and exit optionality. Legacy cable operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — they lobbied for the mandate, can exit by shifting to broadband, capture rent through subscriber consolidation. Content studios: Beneficiary + constrained (benefit from bundling's revenue stability, but constrained by regulatory pressure and platform fragmentation) → d≈0.30, f(d)≈0.28. Low-moderate extraction; they benefit but lack full freedom to reshape the constraint. Regulatory body: Mixed, powerful agents → d≈0.40, f(d)≈0.40. Moderate extraction — enforces asymmetry but has arbitage to modify mandate. Open internet coalition: Organized + mobile → d≈0.35, f(d)≈0.35. Low extraction; coalition has international mobility and regulatory counter-pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The mandatrophy is resolved by decomposing bundling into its genuine coordination function and its extractive enforcement. Bundling DOES solve coordination problems: (1) reduces consumer choice paralysis (paradox of choice), (2) enables cross-promotion and discovery (consumers sample content they wouldn't select independently), (3) stabilizes revenue for platforms (predictable subscriber bases improve investment). These are legitimate coordination benefits. Simultaneously, bundling ENABLES extraction: (1) forces budget consumers to overpay, (2) locks out niche creators from direct channels, (3) protects legacy incumbents from disruption. The classification is tangled rope, not snare, because the coordination function is structurally real — measurable improvements in consumer experience and platform stability — not theater. The theater ratio (0.52) confirms this: bundling is neither purely functional (rope would have theater ≤0.30) nor purely performative (snare would have theater ≥0.70). The mandate is genuine mixed coordination-extraction. The mandatrophy surfaces when naive classification mistakes bundling for pure consumer benefit (rope from beneficiary only) or pure market control (snare from victim only). Deferential realism's full perspectival analysis reveals the hybrid: each agent's classification is correct from their structural position, and the constraint permits both interpretations because it genuinely performs both functions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bundling_efficiency_claim,
    'Does bundling genuinely reduce consumer decision fatigue and increase content discovery, or is this a post-hoc rationalization for rent extraction?',
    'Randomized field experiments: measure consumer satisfaction, churn rates, and content discovery metrics in bundled vs unbundled markets; compare to pre-streaming cable bundling data',
    'If true: bundling is coordination (rope from regulator and operator perspectives strengthens). If false: bundling is pure extraction theater (snare classification hardens across all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundling_efficiency_claim, empirical, 'Whether bundling genuinely solves consumer discovery or is extractive theater').

omega_variable(
    direct_to_consumer_viability,
    'Without the mandate, would niche and independent content creators have sustainable direct-to-consumer models, or do economies of scale require some form of bundling?',
    'Historical analysis of unbundled streaming markets (European, Asian markets with lighter bundling mandates); measurement of independent creator revenue and subscriber acquisition costs in mixed regulatory environments',
    'If viable: mandate is pure extraction (snare classification hardens for creators). If not viable: mandate is mixed coordination-extraction (tangled rope classification is correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_to_consumer_viability, empirical, 'Whether independent creators can thrive without bundling requirement').

omega_variable(
    international_regulatory_convergence,
    'Will other jurisdictions adopt similar bundling mandates, or will this regime remain isolated and subject to repeal?',
    'Tracking of regulatory filings in EU, UK, Canada, Australia; analysis of corporate lobbying positions; correlation with digital services tax and antitrust enforcement trends',
    'If converges: mandate becomes entrenched (piton classification hardens). If isolated: mandate is temporary (scaffold classification hardens, sunset becomes more visible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_regulatory_convergence, conceptual, 'Whether bundling mandates will become global norm or remain isolated').

omega_variable(
    technology_unbundling_pressure,
    'Will algorithmic recommendations, AI-driven curation, and emerging platforms (TikTok, YouTube Shorts, Discord communities) make traditional bundling obsolete regardless of regulation?',
    'Trend analysis: track shifts in content consumption patterns among younger cohorts; measure algorithmic discovery vs traditional browsing; monitor platform investment in recommendation systems',
    'If yes: mandate becomes vestigial (piton classification hardens, theater_ratio rises as bundling becomes performative). If no: mandates will remain economically relevant (tangled rope or snare holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_unbundling_pressure, empirical, 'Whether emerging technology makes bundling economically obsolete').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(streaming_bundling_mandate, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stream_bundle_tr_t0, streaming_bundling_mandate, theater_ratio, 0, 0.38).
narrative_ontology:measurement(stream_bundle_tr_t3, streaming_bundling_mandate, theater_ratio, 3, 0.45).
narrative_ontology:measurement(stream_bundle_tr_t6, streaming_bundling_mandate, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(stream_bundle_be_t0, streaming_bundling_mandate, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(stream_bundle_be_t3, streaming_bundling_mandate, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(stream_bundle_be_t6, streaming_bundling_mandate, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(streaming_bundling_mandate, resource_allocation).
narrative_ontology:affects_constraint(streaming_bundling_mandate, cord_cutting_acceleration).
narrative_ontology:affects_constraint(streaming_bundling_mandate, platform_fragmentation_pressure).
narrative_ontology:affects_constraint(streaming_bundling_mandate, independent_creator_viability).

% DUAL FORMULATION NOTE:
% Mandatory streaming bundling is downstream of regulatory capture dynamics (legacy operators lobbying for favorable rules) and upstream of consumer response mechanisms (subscription stacking, VPN usage, piracy substitution). The constraint family links three structural claims: (1) regulatory capture hypothesis (legacy operators lobby for bundling protection), (2) bundling extraction hypothesis (consumers overpay, creators lose access), (3) technology substitution hypothesis (algorithms and emerging platforms unbundle regardless). Each has distinct ε and classification; they affect the bundling constraint and are affected by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
