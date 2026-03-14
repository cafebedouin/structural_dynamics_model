% ============================================================================
% CONSTRAINT STORY: platform_data_portability_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_data_portability_mandate, []).

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
 *   constraint_id: platform_data_portability_mandate
 *   human_readable: Platform Data Portability Mandate (GDPR, DMA, Digital Markets Act)
 *   domain: digital_economy/regulatory_governance
 *
 * SUMMARY:
 *   The platform data portability mandate (GDPR Article 20, DMA Article 6,
 *   equivalent national laws) represents an attempt to reduce lock-in and
 *   enable competition by requiring platforms to export user data in
 *   portable, machine-readable formats. The mandate operates through seven
 *   distinct structural perspectives, revealing tensions between its stated
 *   coordination goal (reducing switching costs) and its actual effects
 *   (preserving asymmetric network effects while adding compliance burden).
 *   The constraint exhibits increasing theater ratio over time (0.42 → 0.62),
 *   indicating growing divergence between formal compliance and functional
 *   portability — platforms export data while simultaneously making that data
 *   difficult to operationalize outside their ecosystem. Extractiveness has
 *   risen from 0.35 to 0.56 as implementation has revealed that exportable
 *   data without accompanying algorithmic, reputational, and social graph
 *   access remains largely non-portable. The mandate is thus neither pure
 *   coordination (Rope) nor pure extraction (Snare), but rather a hybrid that
 *   redistributes who bears which costs.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — promised portability but face identity lock, social graph lock, and information asymmetry about exercise of rights
 *   - Large Incumbent Platforms: Primary beneficiaries (powerful/arbitrage) — forced to open data exports but retain algorithmic, network effect, and reputation lock-in; experience mandate as coordination with controlled extraction
 *   - Competitive Entrants: Secondary beneficiary (organized/mobile) — experience mandate as pure coordination; reduced switching costs enable market entry
 *   - Regulatory Authority: Institutional enforcer (institutional/constrained) — implements mandate with explicit sunset logic; intends suppression to decline as competitive alternatives mature
 *   - Mid-Market Platforms: Secondary victim (moderate/constrained) — face genuine coordination benefits from standardized data formats but absorb disproportionate compliance burden
 *   - Compliance Infrastructure: Performative ritual (institutional/arbitrage) — data export implementations satisfy letter of law while circumventing spirit through technical choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_data_portability_mandate, 0.52).
domain_priors:suppression_score(platform_data_portability_mandate, 0.65).
domain_priors:theater_ratio(platform_data_portability_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_data_portability_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(platform_data_portability_mandate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_data_portability_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_data_portability_mandate, tangled_rope).
narrative_ontology:human_readable(platform_data_portability_mandate, "Platform Data Portability Mandate (GDPR, DMA, Digital Markets Act)").
narrative_ontology:topic_domain(platform_data_portability_mandate, "digital_economy/regulatory_governance").

domain_priors:requires_active_enforcement(platform_data_portability_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_data_portability_mandate, data_subject_individuals).
narrative_ontology:constraint_beneficiary(platform_data_portability_mandate, competitive_entrants).
narrative_ontology:constraint_victim(platform_data_portability_mandate, platform_switching_friction).
narrative_ontology:constraint_victim(platform_data_portability_mandate, regulatory_compliance_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Despite the mandate's promise of data freedom, the user remains trapped in the platform ecosystem through switching costs (data fragmentation, network effects, habit), identity lock (social graph, reputation), and information asymmetry about how to actually exercise portability rights. The mandate extracts promise of freedom without delivering substantive exit capacity. Maximum extraction from a powerless, trapped agent with no realistic alternatives.
constraint_indexing:constraint_classification(platform_data_portability_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LARGE PLATFORM INCUMBENT (TANGLED ROPE) — Genuine coordination function: the mandate forces data standards, API contracts, and interoperability definitions that enable ecosystem coordination at scale. But these coordination benefits are asymmetrically distributed — the incumbent retains algorithmic lock-in, network effects, and user habit lock while forced to open data export. Active enforcement required; benefits flow to the platform through coordination while costs concentrate on competitors and powerless users.
constraint_indexing:constraint_classification(platform_data_portability_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETITIVE ENTRANT (ROPE) — Organized challengers (well-funded startups, regional platforms, regulated financial institutions) experience the mandate as pure coordination — data portability removes the lock-in barrier that prevented competitive entry. This is coordination with benefit-cost symmetry from the entrant's perspective: lower switching costs enable genuine competition. No extraction experienced; the entrant perceives legitimate access to the playing field.
constraint_indexing:constraint_classification(platform_data_portability_mandate, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (SCAFFOLD) — The mandate is explicitly temporary architecture for transitioning from platform moat-based competition to API-based competition. Sunset mechanism: as competitive entrants build alternative ecosystems (financial data aggregators, social media federations, messaging interoperability), the regulatory mandate's coercive force becomes unnecessary — markets self-coordinate through open standards. The authority enforces actively now (high suppression) but intends suppression to decline over the compliance window as competitive alternatives mature.
constraint_indexing:constraint_classification(platform_data_portability_mandate, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COMPLIANCE THEATER (PITON) — Platforms implement technically correct data export formats (JSON, CSV) and APIs that nominally satisfy the mandate but often export data in forms unusable without the platform's ecosystem. The ritual of 'data portability' persists while the actual function (enabling user choice) remains limited. Theater ratio high because compliance is performative — formal adherence to the letter while circumventing the spirit through technical choices (data fragmentation, proprietary schemas, export delays). This is degraded regulation maintained through institutional inertia and ambiguous metrics.
constraint_indexing:constraint_classification(platform_data_portability_mandate, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MID-MARKET PLATFORM (TANGLED ROPE) — Regional and smaller platforms (financial data aggregators, job boards, niche social networks) face genuine coordination gain from standardized data formats but also face compliance burden that erodes margins. They can exit the regulated market (constrained cost is real) but do not because the aggregator's business model depends on data inflow. Mixed coordination and extraction — they benefit from user lock-in prevented by competitors' data export, but suffer compliance costs not borne equally by incumbents with integrated infrastructure.
constraint_indexing:constraint_classification(platform_data_portability_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, the mandate appears to reflect an immutable property of digital information: data cannot be both portably extracted and asymmetrically locked — there is no technical solution to prevent duplicative access once export is enabled. The observer mistakes a contingent policy choice (mandatory export) for a law of information physics. However, the structural data reveals this is a false summit: the implementation details (export timing, schema standardization, API stability) are entirely contingent regulatory choices, not immutable properties. The appearance of natural law masks the selection of which incompatibility gets regularized versus which gets permitted.
constraint_indexing:constraint_classification(platform_data_portability_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_data_portability_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_data_portability_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_data_portability_mandate, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_data_portability_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_data_portability_mandate, TR),
    TR >= 0.70.

:- end_tests(platform_data_portability_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The mandate initially had lower extractiveness (0.35) when viewed as a regulatory intervention against lock-in. But implementation has revealed that exportable data without algorithmic, social, and reputational infrastructure is minimally useful — users who download their data largely cannot operationalize it on alternative platforms. The rising trajectory (0.35 → 0.56) reflects Goodhart drift: as platforms optimize compliance to the letter (format standards, export speed) rather than the spirit (enabling actual switching), the mandate's extractive character becomes visible. The extraction is not from the typical sense of value theft, but from the promise of freedom — users are promised portability they cannot practically use. Suppression (0.65): High. Barriers to using exported data include: (1) lack of receiving platform infrastructure to accept and integrate the data, (2) loss of algorithmic curation and recommendations upon export, (3) social graph incompleteness (can export contacts but not network effects), (4) procedural friction in the export process itself, (5) information asymmetry about how to exercise rights. Theater ratio (0.58, rising to 0.62): Moderate-high and rising. The mandate creates performative compliance — platforms implement correct formats and APIs while designing around functional portability through technical choices (export delays, schema ambiguity, lack of data completeness guarantees). The theater has increased over time as compliance infrastructure has matured but switching costs have not declined accordingly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same regulatory intervention produces radically different classification outcomes from different structural positions. The individual user sees a Snare — promised freedom that proves non-functional due to switching costs. The incumbent platform sees a Tangled Rope — genuine coordination benefit (standardized data formats enable ecosystem coordination) combined with controlled extraction through preserved lock-ins. The entrant sees a Rope — pure coordination solving the barrier to entry. The regulator sees a Scaffold — explicitly temporary coercion with sunset logic as competitive alternatives mature. The mid-market platform sees a Tangled Rope from a different angle — coordination benefits plus disproportionate compliance burden. The analytical observer risks seeing a Mountain — that digital platforms inherently create lock-in and portability cannot overcome this. But the structural data reveals this as a false summit: the lock-in mechanisms are contingent policy choices (algorithmic curation, social graph integration, reputation portability), not laws of information physics. The mandate's failure to reduce switching costs derives from incomplete scope (exporting data is not exporting algorithms, relationships, or recommendations), not from natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent platforms, regulatory authority) benefit from mandate's coordination aspects: standardized formats, API contracts, and interoperability definitions enable efficient ecosystem scaling. The incumbent's arbitrage exit option and powerful status derive low d ≈ 0.15 → negative χ contribution. Victims (users, mid-market platforms) bear costs: users face switching cost persistence and procedural friction; mid-market platforms face compliance burden. Users' trapped status with powerless power derives maximum d → maximum χ. Mid-market platforms' constrained exit (market dependency) derives moderate-high d → moderate-high χ. The regulatory authority's role as enforcer gives it institutional power with constrained exit relative to the market (it must maintain the mandate but cannot force compliance without resource), deriving lower d than the regulated parties. The compliance theater perspective receives institutional power with arbitrage exit because compliance infrastructure (consulting, auditing, export-as-a-service) is a market opportunity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate resolves the mandatrophy by distinguishing coordination function from extraction mechanism. The genuine coordination function exists: standardized data formats, API contracts, and interoperability definitions enable ecosystem coordination and reduce friction for competitive entrants. But this coordination benefit concentrates on powerful platforms and organized entrants, while the extraction (compliance burden, false promise of portability) concentrates on powerless users and mid-market platforms. The mandate is neither purely extractive (it does enable some genuine competition) nor purely coordinative (it preserves asymmetric lock-ins). The rising theater ratio reveals Goodhart drift: platforms optimize compliance metrics (export format standards, export speed) rather than mandate intent (actual user switching). The tangled rope classification is diagnostically correct — the constraint is structurally hybrid with asymmetric benefit distribution. The false natural law perspective (Mountain) is the tempting but incorrect frame: 'lock-in is inherent to platforms.' The mandate's limited success derives not from the inevitability of digital networks but from the regulatory scope covering data export while leaving algorithmic, social, and reputational lock-in mechanisms untouched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_vs_privacy_tension,
    'Does mandatory data export inherently conflict with privacy protection, or can privacy be maintained through technical separation and consent workflows?',
    'Analysis of data breach correlation post-export mandates; identification of whether portability export formats reduce ability to prevent unauthorized access; testing of consent mechanisms as genuine privacy controls vs performative consent.',
    'If inherent conflict: mandate trades privacy for competition (snare severity increases). If separable: privacy framework can co-exist with portability (tangled rope severity decreases, coordination function strengthens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portability_vs_privacy_tension, empirical, 'Whether mandatory data export and privacy protection are technically compatible').

omega_variable(
    usable_portability_threshold,
    'What percentage of users must successfully use portability to constitutively shift from snare (trapped users) to rope (mobile users)?',
    'Population surveys of actual data export behavior; tracking of users who download data vs users who successfully switch platforms; comparison of export completion rates across demographic groups.',
    'If < 5% users switch: mandate remains snare despite formal rights (theater without function). If > 25% users switch: mandate becomes genuine rope (competitive friction reduced). Threshold drives classification sensitivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usable_portability_threshold, empirical, 'Portability usage rate threshold for classification change').

omega_variable(
    incumbent_accommodation_strategy,
    'Do large platforms accommodate portability through integrated alternatives (single-sign-on, federated identity) that preserve their network effects while formally complying, or do they resist portability through technical friction?',
    'Comparative analysis of platform data export infrastructure; tracking of how many entrants successfully use exported data vs how many build on incumbent federation APIs; measurement of switching cost change over time for users exporting vs users not exporting.',
    'If incumbent accommodates: tangled rope is stable (coordination + controlled extraction). If incumbent resists: snare severity increases and mandate''s enforcement costs rise (regulatory war).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_accommodation_strategy, empirical, 'Whether incumbent platforms accommodate or resist portability').

omega_variable(
    competitive_entrant_data_lock,
    'Once entrants acquire user data via portability, do they create their own lock-in (preventing further switching), making the mandate a one-time friction reduction rather than enabling genuine competition?',
    'Longitudinal tracking of switching costs from entrant platforms; measurement of data export friction in entrant platforms; analysis of whether regulatory scope expands to entrants over time.',
    'If entrants lock-in equivalently: mandate is temporary scaffold (friction shifts but recurs at new entrant). If entrants remain interoperable: mandate enables genuine competitive transformation (rope at scale). This distinction drives network-level classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_entrant_data_lock, empirical, 'Whether entrant platforms create equivalent lock-in').

omega_variable(
    compliance_cost_concentration,
    'Do compliance costs concentrate on small platforms (making mandate a form of regulatory burden extraction) or distribute proportionally (making it a legitimate coordination investment)?',
    'Cost accounting across platform size tiers; measurement of engineering effort and infrastructure required per user; analysis of whether small platforms absorb percentage-wise higher costs than incumbents.',
    'If concentrated: mid-market platforms experience snare-like extraction masked as coordination (tangled rope severity increases). If proportional: tangled rope is genuinely hybrid (lower extractiveness, higher coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_cost_concentration, empirical, 'Whether compliance costs concentrate on small platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_data_portability_mandate, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdpm_tr_t0, platform_data_portability_mandate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pdpm_tr_t3, platform_data_portability_mandate, theater_ratio, 3, 0.5).
narrative_ontology:measurement(pdpm_tr_t6, platform_data_portability_mandate, theater_ratio, 6, 0.58).
narrative_ontology:measurement(pdpm_tr_t9, platform_data_portability_mandate, theater_ratio, 9, 0.62).

% Extraction over time
narrative_ontology:measurement(pdpm_be_t0, platform_data_portability_mandate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pdpm_be_t3, platform_data_portability_mandate, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(pdpm_be_t6, platform_data_portability_mandate, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(pdpm_be_t9, platform_data_portability_mandate, base_extractiveness, 9, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_data_portability_mandate, resource_allocation).
narrative_ontology:affects_constraint(platform_data_portability_mandate, algorithmic_transparency_mandate).
narrative_ontology:affects_constraint(platform_data_portability_mandate, social_graph_interoperability).
narrative_ontology:affects_constraint(platform_data_portability_mandate, platform_network_effects).

% DUAL FORMULATION NOTE:
% The data portability mandate decomposes into distinct constraints: (1) data export technical standard (lower ε, pure coordination when implemented correctly), (2) user interface friction for exercising rights (higher ε, snare-like extraction through procedural obscurity), (3) downstream platform lock-in (independent ε, separate constraint). This story focuses on the mandate's aggregate effect across all three. The upstream constraints are the technical and legal requirements; the downstream constraints are the network effects that portability fails to overcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_data_portability_mandate, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
