% ============================================================================
% CONSTRAINT STORY: streaming_bundling_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: streaming_bundling_mandate
 *   human_readable: Mandatory Streaming Bundling Regulation
 *   domain: economic/media_regulation
 *
 * SUMMARY:
 *   Mandatory streaming bundling represents a regulatory mandate forcing
 *   streaming services to offer content exclusively as part of bundled
 *   packages, prohibiting standalone subscriptions. This constraint exhibits
 *   the classic tangled rope structure: it solves a genuine coordination
 *   problem (fragmented streaming market, consumer choice paralysis, bundle
 *   economics) while simultaneously extracting from creators, consumers, and
 *   competing platforms. Incumbent cable and telecom providers emerge as
 *   primary beneficiaries, as mandatory bundling resurrects their
 *   cross-subsidization model and eliminates direct competition from
 *   standalone platforms. The constraint's extractiveness (0.58) reflects
 *   moderate but persistent transfer of economic rents from creators and
 *   consumers to bundle operators. Suppression (0.65) is significant but not
 *   total—creators can relocate internationally, consumers can use VPNs or
 *   account-sharing, and platforms can compete through international
 *   gateways. The theater ratio (0.48) indicates substantial regulatory
 *   enforcement overhead (compliance mechanisms, price monitoring, bundle
 *   composition audits) but genuine coordination function, distinguishing
 *   this from pure performative regulation. The constraint demonstrates how
 *   regulatory intervention intended to stabilize markets can simultaneously
 *   function as incumbent protection and creator suppression.
 *
 * KEY AGENTS:
 *   - Incumbent Cable/Telecom Providers: Primary beneficiary (institutional/arbitrage) — mandated bundling resurrects their cross-subsidization business model and eliminates standalone platform competition
 *   - Content Creators: Primary victim (powerless/trapped) — forced into bundle distribution, lose direct consumer access and pricing control, revenue flows through operator aggregators
 *   - Consumers Seeking Unbundling: Primary victim (powerless/trapped) — locked into unwanted content bundles, cannot purchase services independently, absorb full bundle pricing
 *   - Standalone Streaming Platforms: Secondary victim (organized/constrained) — face mandated consolidation but also benefit from regulated distribution standardization; have partial exit options
 *   - Public Interest Advocates: Moderate agent (moderate/constrained) — see bundling mandate as temporary intervention with sunset logic; have advocacy leverage within regulatory system
 *   - Legacy Regulatory System: Institutional actor (institutional/arbitrage) — maintains bundle enforcement through regulatory theater; benefits from appearance of market control despite circumvention mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(streaming_bundling_mandate, 0.58).
domain_priors:suppression_score(streaming_bundling_mandate, 0.65).
domain_priors:theater_ratio(streaming_bundling_mandate, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(streaming_bundling_mandate, extractiveness, 0.58).
narrative_ontology:constraint_metric(streaming_bundling_mandate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(streaming_bundling_mandate, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(streaming_bundling_mandate, tangled_rope).
narrative_ontology:human_readable(streaming_bundling_mandate, "Mandatory Streaming Bundling Regulation").
narrative_ontology:topic_domain(streaming_bundling_mandate, "economic/media_regulation").

domain_priors:requires_active_enforcement(streaming_bundling_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(streaming_bundling_mandate, incumbent_cable_providers).
narrative_ontology:constraint_beneficiary(streaming_bundling_mandate, bundled_service_aggregators).
narrative_ontology:constraint_victim(streaming_bundling_mandate, content_creators).
narrative_ontology:constraint_victim(streaming_bundling_mandate, consumer_choice).
narrative_ontology:constraint_victim(streaming_bundling_mandate, competitive_platforms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NICHE CONTENT CREATOR (SNARE) — Forced into mandatory bundling with no option to distribute independently. Cannot exit or negotiate terms. Extraction maximum: income flows through bundle aggregators with no direct consumer relationship, revenue sharing determined by bundle operator, and discovery suppressed by bundle optimization logic.
constraint_indexing:constraint_classification(streaming_bundling_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNBUNDLING-SEEKING CONSUMER (SNARE) — Locked into bundles they do not want; cannot purchase individual services; faces full cost of unwanted content subscription. Exit only through legal departure from jurisdiction or accepting forced expenditure. Maximum suppression and extraction.
constraint_indexing:constraint_classification(streaming_bundling_mandate, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETING STANDALONE PLATFORMS (TANGLED ROPE) — Face mandated consolidation that eliminates their market model but also creates coordination opportunity: bundling creates standardized distribution channel and reduces customer acquisition costs through regulatory mandate rather than marketing. Constrained exit (could relocate services internationally or build alternative platforms) but also benefits from simplified market structure. Mixed extraction and coordination.
constraint_indexing:constraint_classification(streaming_bundling_mandate, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CABLE/TELECOM PROVIDERS (ROPE) — Primary beneficiaries with arbitrage options (can exit by pivoting to pure streaming or maintaining traditional cable). Mandate protects their bundling business model, resurrects cross-subsidization, and eliminates direct competition from standalone platforms. Experience constraint as coordination mechanism: regulatory mandate solves their customer retention problem. Net beneficiary.
constraint_indexing:constraint_classification(streaming_bundling_mandate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC INTEREST ADVOCACY GROUPS (SCAFFOLD) — View mandate as temporary regulatory intervention with sunset logic: bundling restrictions are meant to test market structures and protect consumer welfare during transition. See enforcement mechanisms (price caps, unbundling review clauses) as scaffolding that declines over time as market stabilizes. Constrained exit (must work within regulatory system) but genuine agency through advocacy and policy revision cycles.
constraint_indexing:constraint_classification(streaming_bundling_mandate, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY BROADCASTING REGULATION (PITON) — Bundling mandate is a vestigial application of cable-era regulation to digital markets. Theater ratio high (regulatory compliance theater, bundling enforcement mechanisms, price-cap reviews) but functional utility degraded: consumers already circumvent via VPNs, shared accounts, and international platforms. Mandate persists through regulatory inertia and incumbent lobbying pressure rather than market function. Institutional actors maintain the fiction of effective regulation despite widespread non-compliance.
constraint_indexing:constraint_classification(streaming_bundling_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From analytical distance, the mandate exhibits both genuine coordination (standardized bundle interface reduces fragmentation, simplifies consumer choice architecture) and significant extraction (creator suppression, consumer welfare loss, competitive constraint). Effective extraction chi remains moderate-high even at analytical distance because suppression mechanics (forced bundling, price-fixing via bundle composition) are structural, not perspectival.
constraint_indexing:constraint_classification(streaming_bundling_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(streaming_bundling_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(streaming_bundling_mandate, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): Moderately high. The mandate transfers economic rent from creators and consumers to bundle operators through forced consolidation and cross-subsidization. However, extraction is not maximal (≥0.66 snare threshold) because (1) creators retain exit options through international relocation, (2) consumers have circumvention methods (VPNs, account-sharing), and (3) competitive platforms can operate internationally and undercut the mandate. The trajectory over 6 years shows increasing extractiveness as creators' exit costs rise and regulatory enforcement tightens. Suppression (0.65): Significant but not total. Mandatory bundling creates substantial barriers to standalone distribution and independent creator access. However, suppression is not absolute—international platforms, VPN access, and shared accounts provide partial workarounds. The suppression reflects regulatory enforcement intensity rather than absolute coercion. Theater ratio (0.48): Moderate. Bundling enforcement requires compliance infrastructure (price caps, composition audits, technical standards), but the coordination function is genuine—bundling does solve fragmentation and consumer choice problems. The theater ratio is lower than legacy regulation (piton) but higher than pure coordination mechanisms (rope), reflecting that enforcement overhead is necessary but coordination is real.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between beneficiaries and victims. Incumbent providers see mandatory bundling as coordination mechanism that solves their market retention problem (rope experience). Creators and unbundling-seeking consumers experience pure extraction with no exit (snare). Standalone platforms see mixed coordination and extraction (tangled rope). Public advocates see temporary intervention with sunset potential (scaffold). Legacy regulatory system sees its own degraded enforcement function maintained through institutional inertia (piton). The gap is not merely a difference in power—it reflects fundamentally different structural relationships to the extraction flow. The same mandate is rope for beneficiaries, snare for powerless victims, and tangled rope for moderate competitors. This distributional asymmetry is the core diagnostic feature of tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the bundling mandate. Incumbent providers are beneficiaries with arbitrage options (can pivot to pure streaming or maintain traditional models)—they experience low d (0.10-0.20), producing negative effective extraction. Creators are victims trapped by bundling requirements—they experience high d (0.85-0.95), producing high extraction chi. Consumers seeking unbundling are trapped victims—high d (0.90-1.00), maximum experienced extraction. Standalone platforms are constrained victims but also benefit from regulatory standardization—moderate d (0.50-0.65), moderate extraction. Public advocates experience moderate d (0.45-0.55) through constrained advocacy access within regulatory system. The analytical observer experiences d ≈ 0.65 (moderate structural extraction balanced against genuine coordination function) from global perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PARTIALLY RESOLVED. The constraint avoids false classification through two mechanisms: (1) Explicit identification of beneficiaries (incumbent providers) and victims (creators, consumers, competitive platforms) prevents treating it as pure rope coordination. The redistribution is measurable and directional. (2) Recognition that mandatory bundling solves a genuine coordination problem (fragmented market, consumer paralysis)—it is not pure snare extraction. The constraint genuinely bundles coordination value with extraction rent, which is the defining feature of tangled rope. However, REMAINING AMBIGUITY: Does the mandate represent permanent incumbent capture (snare trajectory with theater) or temporary market stabilization (scaffold with sunset)? The omega variable on regulatory capture persistence directly addresses this. If the mandate sunsets at 10-15 years, it is genuinely scaffold with declining enforcement. If it persists indefinitely maintained by lobbying pressure, it is capture-driven snare in institutional disguise. The theater ratio trajectory (0.35→0.48 over 6 years) suggests rising enforcement complexity without declining intensity, leaning toward persistence rather than sunset. This tension defines the mandatrophy: is this tangled rope becoming piton (degraded ritual) or tangled rope trending toward snare (pure extraction disguised as coordination)?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creator_migration_threshold,
    'At what economic threshold do content creators permanently relocate production to unregulated jurisdictions?',
    'Historical analysis of production location changes in response to bundling mandates; correlation between mandate severity and creator exit rates; tracking of content production capacity shifts to foreign markets',
    'If threshold < 2 years: rapid creator exodus degrades bundle quality and triggers mandate revision. If threshold > 5 years: creators remain trapped, extraction persists, and mandate sustains through institutional inertia despite degraded content quality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_migration_threshold, empirical, 'Threshold at which content creators permanently relocate to unregulated jurisdictions').

omega_variable(
    consumer_circumvention_viability,
    'Do alternative access methods (VPN, account sharing, international platforms, offline content) provide sufficiently viable circumvention that the mandate''s suppression mechanism effectively fails?',
    'Measurement of circumvention adoption rates; analysis of regulatory enforcement capacity vs circumvention technology arms race; economic modeling of compliance costs vs circumvention costs',
    'If circumvention highly viable: mandate''s suppression is illusory, extraction is unsuccessful, and piton classification confirmed (ritual maintained despite functional collapse). If circumvention is rare: suppression mechanism works, victims remain trapped, snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_circumvention_viability, empirical, 'Whether alternative access methods provide effective circumvention of bundling mandate').

omega_variable(
    bundle_composition_externality,
    'Does forcing unwanted content into bundles create genuine market inefficiency (deadweight loss) or do consumers still derive value from expanded choice within bundles?',
    'Consumer surplus analysis comparing bundled vs unbundled pricing; willingness-to-pay studies; measurement of bundle penetration and actual usage patterns',
    'If significant deadweight loss: consumer extraction is real and snare classification confirmed. If deadweight loss is small: bundling creates value through cross-subsidization and tangled rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bundle_composition_externality, empirical, 'Whether forced bundling creates deadweight loss or genuine value through cross-subsidization').

omega_variable(
    international_platform_competition,
    'Do platforms operating from unregulated jurisdictions undercut the mandate and restore unbundled access to regulated markets?',
    'Monitoring of foreign-based platform access rates in regulated markets; enforcement action frequency against international platforms; tracking of consumer adoption of international services',
    'If international competition effective: mandate fails to suppress standalone streaming, extraction mechanism breaks down. If international platforms are successfully blocked: mandate contains the escape route, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_platform_competition, empirical, 'Whether international platforms restore unbundled access despite regulatory mandate').

omega_variable(
    regulatory_capture_persistence,
    'Does the mandate function as permanent incumbent protection or as temporary transition mechanism with genuine sunset logic?',
    'Analysis of regulatory revision cycles; measurement of lobbying expenditure by incumbents vs competitive platforms; tracking of stated vs actual sunset timelines',
    'If capture permanent: mandate is snare for creators and consumers, rope for incumbents, piton for regulatory system. If sunset is real: scaffold classification is correct and mandate will degrade over generational timescale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_persistence, preference, 'Whether mandate is permanent capture or temporary transition with genuine sunset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(streaming_bundling_mandate, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stream_bundle_tr_t0, streaming_bundling_mandate, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stream_bundle_tr_t3, streaming_bundling_mandate, theater_ratio, 3, 0.42).
narrative_ontology:measurement(stream_bundle_tr_t6, streaming_bundling_mandate, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(stream_bundle_be_t0, streaming_bundling_mandate, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stream_bundle_be_t3, streaming_bundling_mandate, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(stream_bundle_be_t6, streaming_bundling_mandate, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(streaming_bundling_mandate, resource_allocation).
narrative_ontology:affects_constraint(streaming_bundling_mandate, content_creator_licensing).
narrative_ontology:affects_constraint(streaming_bundling_mandate, consumer_price_discrimination).
narrative_ontology:affects_constraint(streaming_bundling_mandate, platform_market_concentration).

% DUAL FORMULATION NOTE:
% Mandatory bundling is downstream of broader regulatory capture dynamics in telecommunications/media convergence and upstream of specific creator licensing agreements and consumer welfare impacts. The constraint family decomposes into three distinct structural claims: (1) bundling_as_coordination (low ε, rope-dominant) modeling the genuine market fragmentation problem it solves; (2) bundling_as_rent_extraction (moderate ε, tangled rope, this story) modeling the redistribution from creators/consumers to incumbents; (3) bundling_enforcement_theater (high ε, piton-dominant) modeling the regulatory compliance mechanisms. This story focuses on decomposition (2) as the primary structural claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(streaming_bundling_mandate, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
