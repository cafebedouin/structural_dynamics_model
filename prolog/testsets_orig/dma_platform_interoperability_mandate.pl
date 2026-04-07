% ============================================================================
% CONSTRAINT STORY: dma_platform_interoperability_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dma_platform_interoperability_mandate, []).

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
 *   constraint_id: dma_platform_interoperability_mandate
 *   human_readable: DMA Platform Interoperability Mandate
 *   domain: digital_markets/regulatory_enforcement
 *
 * SUMMARY:
 *   The DMA platform interoperability mandate (EU Digital Markets Act
 *   Articles 5-6) mandates that 'gatekeepers' (dominant platforms) provide
 *   interop APIs enabling users and smaller platforms to access core
 *   functionality and port social data. The constraint exhibits a tension
 *   between genuine coordination function (solving the multi-platform
 *   ecosystem fragmentation problem) and extraction mechanism (asymmetric
 *   implementation costs, regulatory theater, persistent network effects).
 *   This is a canonical tangled_rope: the mandate creates both coordination
 *   benefits (ecosystem growth, user agency) and extraction burdens
 *   (implementation asymmetry, enforcement gaps, switching costs). The
 *   theater ratio (0.58) reflects that formal compliance is easier to
 *   demonstrate than functional interop — platforms can declare API
 *   availability while restricting access through rate-limiting,
 *   underdocumentation, or technical barriers.
 *
 * KEY AGENTS:
 *   - Dominant Platform Operators: Primary victims (institutional/constrained) — bear asymmetric implementation costs, lose extraction mechanism, but retain market power through network effects and quality differentials
 *   - Smaller Competing Platforms: Primary beneficiaries (moderate/constrained) — gain API access to dominant platform user base but face substantial technical integration burdens
 *   - Users Locked in Ecosystems: Secondary victims (powerless/trapped) — nominally gain portability rights but remain locked by network effects; data interop without social graph portability is insufficient for switching
 *   - Digital Ecosystem Standards Bodies: Organized beneficiaries (organized/mobile) — coordinate technical standards, develop interop specifications, derive legitimacy from ecosystem coordination role
 *   - EU Digital Markets Regulators: Inter-institutional enforcer (institutional/constrained) — demonstrate regulatory power, enforce compliance, but face verification capacity limits; constrained by technical complexity and political pressure
 *   - Consumer Switching Capacity: Abstract beneficiary (powerless/mobile in theory, trapped in practice) — nominally benefits from portability but actual switching remains blocked by network effects despite technical interop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dma_platform_interoperability_mandate, 0.52).
domain_priors:suppression_score(dma_platform_interoperability_mandate, 0.65).
domain_priors:theater_ratio(dma_platform_interoperability_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dma_platform_interoperability_mandate, extractiveness, 0.52).
narrative_ontology:constraint_metric(dma_platform_interoperability_mandate, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(dma_platform_interoperability_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dma_platform_interoperability_mandate, tangled_rope).
narrative_ontology:human_readable(dma_platform_interoperability_mandate, "DMA Platform Interoperability Mandate").
narrative_ontology:topic_domain(dma_platform_interoperability_mandate, "digital_markets/regulatory_enforcement").

domain_priors:requires_active_enforcement(dma_platform_interoperability_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dma_platform_interoperability_mandate, smaller_platforms).
narrative_ontology:constraint_beneficiary(dma_platform_interoperability_mandate, digital_ecosystem_openness).
narrative_ontology:constraint_beneficiary(dma_platform_interoperability_mandate, consumer_switching_capacity).
narrative_ontology:constraint_victim(dma_platform_interoperability_mandate, dominant_platform_operators).
narrative_ontology:constraint_victim(dma_platform_interoperability_mandate, implementation_burden_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMPLIANT USER (SNARE) — Users whose data and social graphs are accessible via interop mandates but who remain locked into dominant platform ecosystems by network effects. Interop creates extraction mechanisms for the dominant platform: users can theoretically leave but cannot take their social networks, which remain fragmented across the original platform. Maximum suppression — exit is nominally permitted but structurally unfeasible.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: SMALLER COMPETING PLATFORM (TANGLED ROPE) — Constrained by technical implementation costs and market access barriers. The interop mandate simultaneously benefits (enables connection to dominant platform's user base) and extracts (bears 70% of interop infrastructure cost while dominant platform bears 30%). Genuine mixed coordination-extraction dynamic with active enforcement requirements.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: ECOSYSTEM COORDINATION (ROPE) — Organized actors (industry standards bodies, developer consortia, open-source communities) experience interop as a genuine coordination mechanism enabling ecosystem growth. No significant extraction experienced; the coordination benefit is real and mutual. Standards spillover and collective technical development create positive-sum coordination value.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: DOMINANT PLATFORM (SCAFFOLD) — Experiences the mandate as temporary regulatory constraint with known sunset. Pre-interop, dominant platform captured the full ecosystem value. Post-interop, it loses extraction mechanism but retains market position through technical quality, user base, and switching costs. The constraint decays as interop becomes normalized and regulatory appetite moves to new domains. Theater ratio reflects initial performative compliance before genuine integration.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY COMPLIANCE (PITON) — The interop mandate generates substantial theatrical compliance: API endpoints declared but underspecified, data flows permitted but rate-limited, technical standards written but with many exemptions. Regulatory inspectors cannot verify true interop functionality without running actual data transfers at scale. The mandate persists through institutional inertia (regulators continue monitoring compliance) despite low functional verification of real ecosystem benefits.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: REGULATORY ENFORCER (TANGLED ROPE / INTER-INSTITUTIONAL) — EU regulators benefit from interop mandate enforcement (demonstrates regulatory power, builds case law for future digital markets intervention) while bearing implementation verification costs. Constrained by political pressure to show results vs. technical capacity to verify actual ecosystem impact. Different structural relationship than dominant platform — enforcer has institutional power but cannot exit the enforcement burden.
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (ecosystem interoperability genuinely solves collective action problem) layered with extraction mechanism (asymmetric cost burden and enforcement gap). The mandate solves a real problem while creating new ones. Civilizational scope reveals that the constraint's long-term effect depends on whether interop norms internalize (becoming infrastructure) or remain performative (becoming theater).
constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dma_platform_interoperability_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dma_platform_interoperability_mandate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dma_platform_interoperability_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dma_platform_interoperability_mandate, TR),
    TR >= 0.70.

:- end_tests(dma_platform_interoperability_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mandate creates measurable extraction for dominant platforms through compliance costs, API maintenance, data provision, and lost lock-in rents. But the extraction is not total because dominant platforms retain substantial market power through network effects, brand, and quality differentials. The mandate reduces but does not eliminate their extraction capacity. Suppression (0.65): High. Multiple barriers prevent genuine user switching despite interop mandates: network effects (value of platform depends on network size), switching costs (rebuilding social graphs), data fragmentation (users must maintain presence across multiple platforms to maximize network), and technical friction (interop APIs have underdocumentation, rate-limiting, compatibility issues). Theater ratio (0.58): Moderate-high. Compliance is primarily demonstrated through API endpoint declarations and formal data transfer protocols rather than verified user switching or competitive entry. Regulators can verify API existence but not functional interop quality without scale testing that is expensive and rare.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence. Dominant platforms see a constraining regulatory intervention (Scaffold from their view — temporary, with eventual adaptation) that reduces extraction. Smaller platforms see genuine opportunity (Rope/Tangled Rope) mixed with implementation burden. Users see nominal portability rights (Rope aspirationally) undermined by persistent network effects (Snare structurally). Regulators see successful intervention (Scaffold with sunset as interop norms mature) but face verification challenges (Piton from the enforcement perspective — theater of compliance checking). The ecosystem coordination perspective sees pure coordination (Rope) — interop is solving a real collective action problem. The gap reveals that the constraint solves one problem (ecosystem fragmentation) while leaving another unsolved (network effects lock-in).
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platforms as constrained institutional actors benefiting pre-mandate but victimized post-mandate: d ≈ 0.65-0.75. Smaller platforms as moderate/constrained actors nominally benefiting: d ≈ 0.45-0.55. Users as trapped powerless agents: d ≈ 0.90-0.95 (nominally benefiting from portability rights but structurally trapped by network effects). Regulators as institutional actors with constrained enforcement: d ≈ 0.55-0.65. The asymmetry is real: beneficiaries experience lower d (less extraction pressure relative to benefit), victims experience higher d (more extraction burden relative to capacity). The mandate shifts who bears costs without removing the fundamental cost structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate resolves mandatrophy by distinguishing genuine coordination function from extraction mechanism. The coordination function is real: interop solves ecosystem fragmentation, enables standards development, creates positive-sum ecosystem growth. The extraction mechanism is also real: asymmetric implementation costs, persistent network effects despite portability, regulatory theater. The constraint is not 'coordination pretending to be extraction' (snare mislabeled as rope) but genuinely both. The classification as tangled_rope captures this hybrid: the mandate has active enforcement (required by DMA), genuine beneficiaries (smaller platforms, ecosystem standards), and genuine victims (users locked by network effects, dominant platforms forced to share data). The mandatrophy does not reduce to a single type — it is legitimately mixed coordination and extraction depending on observable and agent position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_nominal_interop,
    'Does the declared interop mandate produce genuine cross-platform data portability and service switching, or is it primarily theatrical compliance?',
    'Empirical measurement: (a) percentage of users successfully porting social graphs to competing platforms; (b) actual technical errors and rate-limiting in declared interop APIs; (c) platform engineering cost allocation between genuine integration vs. compliance infrastructure',
    'If actual interop high: rope/scaffold classifications dominate, constraint is coordination mechanism. If theatrical: piton/snare classifications dominate, constraint is enforcement theater with extraction underneath.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_vs_nominal_interop, empirical, 'Whether interop mandate produces functional switching or theatrical compliance').

omega_variable(
    extraction_asymmetry_magnitude,
    'What is the actual cost asymmetry in implementing interop? Do dominant platforms bear materially more or less cost than smaller competitors?',
    'Audit of engineering hours, infrastructure expenditure, and opportunity cost allocated to interop by each platform. Compare cost as percentage of total platform R&D budget.',
    'If dominant platform bears 60%+ of costs: extraction is minimal, rope/scaffold dominate. If smaller platforms bear 60%+: extraction is severe, snare characteristics strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_asymmetry_magnitude, empirical, 'Cost allocation asymmetry in interop implementation').

omega_variable(
    network_effects_persistence,
    'Do interop mandates actually enable user switching despite persistent network effects, or do network effects remain the dominant lock-in mechanism?',
    'Measurement of multi-platform adoption patterns post-mandate; correlation between interop API availability and actual user migration; analysis of whether users maintain multiple platform memberships vs. switching entirely',
    'If network effects overcome interop: snare dynamics persist despite mandate, user lock-in remains despite technical interoperability. If network effects decline: rope/scaffold dynamics dominate, genuine switching becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_persistence, empirical, 'Whether interop overcomes network effects lock-in').

omega_variable(
    regulatory_verification_capacity,
    'Can DMA regulators actually verify compliance with interop mandates, or is verification capacity inherently limited to checking formal API declarations?',
    'Audit of EU digital markets enforcement: what metrics are actually tracked, what exemptions are granted, what false positives/negatives occur in compliance determination',
    'If verification high: mandates are functional, piton theater is limited. If verification low: piton dynamics dominate, enforcement becomes primarily theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_verification_capacity, empirical, 'Regulatory capacity to verify actual interop compliance').

omega_variable(
    competitive_emergence,
    'Does the interop mandate enable new competitive platforms to emerge and capture market share, or does it primarily redistribute benefits among existing players?',
    'Market share tracking of new entrants post-mandate; analysis of whether interop barriers decline faster than switching barriers decline; measurement of user satisfaction and choice diversification',
    'If new competitors emerge: rope/scaffold outcome, competitive market structure emerges. If no new entrants: tangled_rope/snare outcome, existing power structures persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_emergence, empirical, 'Whether interop mandate enables new competitive entry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dma_platform_interoperability_mandate, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dma_tr_t0, dma_platform_interoperability_mandate, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dma_tr_t2, dma_platform_interoperability_mandate, theater_ratio, 2, 0.5).
narrative_ontology:measurement(dma_tr_t4, dma_platform_interoperability_mandate, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(dma_be_t0, dma_platform_interoperability_mandate, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dma_be_t2, dma_platform_interoperability_mandate, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(dma_be_t4, dma_platform_interoperability_mandate, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dma_platform_interoperability_mandate, global_infrastructure).
narrative_ontology:affects_constraint(dma_platform_interoperability_mandate, digital_monopoly_rent_extraction).
narrative_ontology:affects_constraint(dma_platform_interoperability_mandate, network_effects_lock_in).
narrative_ontology:affects_constraint(dma_platform_interoperability_mandate, api_governance_standardization).

% DUAL FORMULATION NOTE:
% The interop mandate is downstream of digital monopoly analysis but represents a distinct structural constraint with its own extractiveness profile. The upstream constraint (digital monopoly rent extraction) has ε ≈ 0.70 (pure extraction). The interop mandate transforms this by introducing coordination function while maintaining significant extraction through network effects persistence, creating the tangled_rope ε ≈ 0.52 classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dma_platform_interoperability_mandate, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
