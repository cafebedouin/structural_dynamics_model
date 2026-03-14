% ============================================================================
% CONSTRAINT STORY: backwards_compatibility_ratchet
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_backwards_compatibility_ratchet, []).

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
 *   constraint_id: backwards_compatibility_ratchet
 *   human_readable: Backwards Compatibility Ratchet
 *   domain: technology/software_architecture
 *
 * SUMMARY:
 *   The backwards compatibility ratchet is a structural constraint that
 *   accumulates over the lifecycle of networked platforms and protocols. Once
 *   a system gains an installed base, the compatibility debt becomes a
 *   one-way ratchet: adding new features and APIs is cheap; removing old ones
 *   creates coordination problems for downstream users. This constraint
 *   exhibits the full taxonomy of DR types depending on perspective. From the
 *   view of platform innovators attempting architectural redesign, it appears
 *   as a Snare: the installed base lock-in is immutable and extraction is
 *   maximal. From the view of incumbent vendors, it appears as Rope: backward
 *   compatibility is experienced as a coordination mechanism that benefits
 *   all parties and maintains a stable platform. From the view of standards
 *   bodies with explicit sunset policies, it appears as Scaffold: a temporary
 *   constraint with an engineered exit. The theater ratio (0.35) reflects
 *   that much backward compatibility maintenance is increasingly
 *   performative: the system maintains deprecated APIs that serve tiny
 *   populations while the majority of traffic flows through new pathways. The
 *   extractiveness (0.58) reflects the asymmetry between the cost of
 *   maintaining old pathways and the benefit of network stability.
 *
 * KEY AGENTS:
 *   - Platform Innovators: Primary victim (powerless/trapped) — cannot redesign architecture without maintaining indefinite legacy support; architectural debt compounds over time
 *   - Incumbent Vendors: Primary beneficiary (institutional/arbitrage) — control the compatibility ratchet; new competitors face massive onboarding costs to support legacy APIs
 *   - Downstream Ecosystem: Secondary victim (moderate/constrained) — benefits from stability but forced to maintain defensive coding patterns and cannot adopt breaking improvements
 *   - Standards Bodies: Organized actors (organized/mobile) — implement staged deprecation and sunset policies; have agency through versioning and migration pathways
 *   - Legacy Compatibility Layer: Institutional actor (institutional/arbitrage) — maintains deprecated APIs through inertia; sees own maintenance as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice (network effects) as immutable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(backwards_compatibility_ratchet, 0.58).
domain_priors:suppression_score(backwards_compatibility_ratchet, 0.52).
domain_priors:theater_ratio(backwards_compatibility_ratchet, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(backwards_compatibility_ratchet, extractiveness, 0.58).
narrative_ontology:constraint_metric(backwards_compatibility_ratchet, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(backwards_compatibility_ratchet, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(backwards_compatibility_ratchet, tangled_rope).
narrative_ontology:human_readable(backwards_compatibility_ratchet, "Backwards Compatibility Ratchet").
narrative_ontology:topic_domain(backwards_compatibility_ratchet, "technology/software_architecture").

domain_priors:requires_active_enforcement(backwards_compatibility_ratchet).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(backwards_compatibility_ratchet, legacy_system_operators).
narrative_ontology:constraint_beneficiary(backwards_compatibility_ratchet, incumbent_vendors).
narrative_ontology:constraint_victim(backwards_compatibility_ratchet, system_innovators).
narrative_ontology:constraint_victim(backwards_compatibility_ratchet, downstream_platform_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM INNOVATORS (SNARE) — Trapped by installed base lock-in. Cannot remove legacy APIs, protocols, or data formats without fragmenting the ecosystem. Architectural improvements are impossible without maintaining full backward compatibility debt. Maximum extraction: innovation cycles slow by orders of magnitude; architectural redesign is economically impossible. The innovator bears the suppression cost indefinitely.
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOWNSTREAM ECOSYSTEM (TANGLED ROPE) — Benefits from stability and API continuity (genuine coordination function: predictability for third-party developers). But constrained by inability to adopt breaking changes — forced to maintain defensive coding against deprecated pathways. Moderate extraction because the ecosystem both benefits (coordination) and bears costs (architectural burden).
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT VENDOR (ROPE) — Benefits from control of the compatibility ratchet. New competitors must implement full legacy support; switching costs for users increase as legacy depth accumulates. The vendor experiences the constraint as coordination — maintaining stable interfaces for customers. Net beneficiary through arbitrage: can fork the standard when beneficial while maintaining backward-compat facade.
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STANDARDS BODY (SCAFFOLD) — Organized agents (IETF, W3C, language evolution committees) implement staged deprecation policies: v1 backward-compat mandatory, v2+ allows breaking changes after 5-10 year sunset clause. Creates migration pathways that reduce extraction over time. Low effective extraction because the organized actors have agency and explicit sunset logic.
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY COMPATIBILITY LAYER (PITON) — Maintains deprecated APIs that no longer serve coordination: the original use case has moved to new interfaces, but the old paths persist through institutional inertia. Theater ratio 0.35 reflects that compatibility maintenance is increasingly performative: most traffic uses new pathways, but compatibility must be maintained for the tiny installed base on outdated clients. The system sees its own compatibility theater as necessary but degraded.
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NETWORK EFFECT VIEW (MOUNTAIN) — From a civilizational perspective, backward compatibility is a natural consequence of network effects: once an interface gains adopters, the cost of change (coordination problem) is immutable across all parties. This perspective sees the ratchet as inherent to networked systems. However, the structural data contradicts this — breaking changes ARE possible (Windows 11 dropped support, Python 3 broke 2.x code); the immutability is contingent on institutional choice, not physical law. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(backwards_compatibility_ratchet, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(backwards_compatibility_ratchet_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(backwards_compatibility_ratchet, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(backwards_compatibility_ratchet, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(backwards_compatibility_ratchet, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(backwards_compatibility_ratchet, TR),
    TR >= 0.70.

:- end_tests(backwards_compatibility_ratchet_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The ratchet mechanism creates genuine lock-in: moving to a new platform or version requires rewriting downstream code, creating friction that accumulates over platform generations. The extractiveness is not total (0.90+) because breaking changes ARE technically possible and ARE sometimes executed (Python 3, Windows breaking changes, major JavaScript frameworks). The institution chooses perpetual compatibility; the cost is not immutable. Suppression (0.52): Moderate. Significant barriers to exit include ecosystem lock-in, switching costs, and the coordination problem of simultaneous migration. But suppression is not total — organizations CAN migrate when sufficiently motivated (proven by successful breaking changes). Theater ratio (0.35): Low-moderate. Backward compatibility maintenance is increasingly performative — the system maintains deprecated APIs that serve tiny installed bases while the majority of traffic flows through new interfaces. As platforms mature, the theater ratio rises because old pathways accumulate without corresponding traffic. Claimed type (Tangled Rope): The constraint has both a genuine coordination function (API stability enables third-party development) and asymmetric extraction (incumbent vendors benefit from high switching costs). The coordination is real; the extraction is real. The tangled rope classification is justified by both mechanisms operating simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon produces radically different classifications depending on the observer's position. The platform innovator sees pure extraction (Snare) — the ratchet locks them into perpetual maintenance. The incumbent vendor sees coordination (Rope) — backward compatibility is experienced as a customer-satisfaction mechanism. The standards body sees a temporary problem with engineering solutions (Scaffold) — deprecation policies and sunset windows are real mechanisms for reducing extraction over time. The legacy layer sees its own degraded function (Piton) — maintaining deprecated APIs that no longer serve their original purpose. The civilizational observer risks seeing immutable natural law (Mountain) — 'network effects make breaking changes impossible' — but the structural data reveals this as false naturalization: breaking changes DO happen and ARE chosen by institutions; the immutability is contingent on institutional preference for backward compatibility, not an inherent property of networks.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural relationship to the compatibility ratchet. Platform innovators are victims (powerless/trapped) — they must maintain all legacy pathways regardless of cost, giving them high d (maximum experienced extraction). Incumbent vendors are beneficiaries with arbitrage options (institutional/arbitrage) — they can fork the standard when beneficial while maintaining the facade of backward compatibility, giving them low d (negative experienced extraction, i.e., net benefit). Downstream ecosystem operators have constrained exit (moderate/constrained, split beneficiary-victim) — they benefit from API stability but are constrained by inability to adopt breaking improvements, giving them moderate d around 0.50. Standards bodies are organized with mobile exit (organized/mobile) — they can create sunset policies and migration pathways, reducing effective extraction to the scaffold level. The legacy compatibility layer is institutional arbitrage (institutional/arbitrage) — it maintains deprecated APIs for tiny populations through inertia, experiencing low effective extraction because it carries out the incumbent vendor's strategy without direct cost.
 *
 * MANDATROPHY ANALYSIS:
 *   RATCHET MECHANISM: The mandatrophy resolves by recognizing that backward compatibility is a one-way institutional choice. The 'ratchet' metaphor is precise: forward direction (adding new APIs) is cheap; reverse direction (removing old APIs) is catastrophically expensive. This asymmetry is not inherent to networks — it is engineered into the institutional constraint. The constraint prevents mislabeling by requiring separate analysis of: (1) genuine coordination cost (API stability DOES solve the problem of ecosystem fragmentation), and (2) extractive rent (incumbent control over the compatibility ratchet DOES create lock-in advantage). Both are present. The tangled rope classification captures this duality: real coordination function + asymmetric extraction mechanism. The mountain perspective is a false summit: 'network effects make it immutable' confuses institutional choice with physical necessity. Standards bodies prove the ratchet is reversible — Python 3, Java versions, Go editions all executed breaking changes despite massive installed bases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_threshold,
    'At what installed base size does backward compatibility become actually immutable rather than merely institutionally enforced?',
    'Historical analysis of successful vs failed breaking-change migrations (Python 2→3, Windows versions, Java versions). Measurement of correlation between installed base percentage and migration failure rate.',
    'If threshold is high (>80% adoption required): network effects are genuinely immutable, mountain classification is justified. If threshold is low (<20%): immutability is institutional choice, not natural law; constraint is tangled rope with ratchet mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_threshold, empirical, 'Installed base threshold for immutable compatibility').

omega_variable(
    deprecation_cost_visibility,
    'Do organizations genuinely internalize the long-term cost of backward compatibility debt, or is the cost externalized and invisible in accounting?',
    'Internal cost accounting at major platforms: separation of feature-development costs from compatibility-maintenance costs. Analysis of budget reallocation when compatibility sunset policies are enforced.',
    'If costs are invisible: organizations perpetuate the ratchet through rational ignorance rather than deliberate choice; suppression increases because decision-makers don''t see the constraint. If costs are visible: ratchet represents deliberate choice; tangled rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deprecation_cost_visibility, empirical, 'Visibility and internalization of backward compatibility costs').

omega_variable(
    alternative_versioning_model,
    'Would parallel major versions (v1 and v2 fully separate) actually reduce innovation friction more than staged deprecation?',
    'Counterfactual modeling of dual-stack platforms; analysis of Go (single major version, strict backward compat) vs Rust (multiple edition system) vs JavaScript (Web Platform vs Node versions) innovation rates.',
    'If parallel versioning reduces friction: current ratchet is suboptimal institutional choice, not natural law. If parallel versioning increases costs: ratchet reflects genuine coordination constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_versioning_model, empirical, 'Whether parallel versioning reduces backward compatibility friction').

omega_variable(
    extraction_rent_vs_coordination_cost,
    'What fraction of the measured extractiveness (0.58) is genuine coordination cost (preventing users from facing broken code) vs extractive rent (incumbent lock-in advantage)?',
    'Comparative analysis: platforms with strict sunset policies vs perpetual backward compat. Measurement of user switching costs correlated to compatibility-debt depth.',
    'If >50% is coordination: tangled rope classification is justified. If <30% is coordination: constraint is primarily snare, and the ''coordination'' framing is cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_rent_vs_coordination_cost, empirical, 'Ratio of coordination cost to extractive rent in backward compatibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(backwards_compatibility_ratchet, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcr_tr_t0, backwards_compatibility_ratchet, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bcr_tr_t5, backwards_compatibility_ratchet, theater_ratio, 5, 0.26).
narrative_ontology:measurement(bcr_tr_t10, backwards_compatibility_ratchet, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(bcr_be_t0, backwards_compatibility_ratchet, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bcr_be_t5, backwards_compatibility_ratchet, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(bcr_be_t10, backwards_compatibility_ratchet, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(backwards_compatibility_ratchet, information_standard).
narrative_ontology:affects_constraint(backwards_compatibility_ratchet, technical_debt_accumulation).
narrative_ontology:affects_constraint(backwards_compatibility_ratchet, platform_lock_in).
narrative_ontology:affects_constraint(backwards_compatibility_ratchet, ecosystem_fragmentation).

% DUAL FORMULATION NOTE:
% The backwards compatibility ratchet operates at the intersection of three structurally distinct constraints: (1) platform governance (who decides when to break compatibility), (2) ecosystem coordination (how downstream users coordinate migration), and (3) technical architecture (which deprecated pathways persist). This story treats the ratchet as a unified institutional constraint; each component could be decomposed separately with different ε values if more granular analysis is required.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(backwards_compatibility_ratchet, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
