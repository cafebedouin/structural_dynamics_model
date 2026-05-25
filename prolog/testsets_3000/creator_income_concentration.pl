% ============================================================================
% CONSTRAINT STORY: creator_income_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creator_income_concentration, []).

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
 *   constraint_id: creator_income_concentration
 *   human_readable: Creator Income Concentration
 *   domain: economic/digital_labor/cultural_production
 *
 * SUMMARY:
 *   Creator income concentration describes the structural mechanism by which
 *   digital platforms concentrate compensation toward a small fraction of
 *   creators while maintaining the appearance of open opportunity. The
 *   constraint exhibits high extractiveness (0.62) and high suppression
 *   (0.68), indicating a genuine snare mechanism from the powerless creator
 *   perspective. The system combines platform network effects (real
 *   coordination function), algorithmic curation (opaque mechanism that
 *   concentrates or distributes depending on design choices), and career path
 *   dependence (creators accrue audience capital on specific platforms). The
 *   theater ratio (0.58) reflects that platforms maintain legitimacy
 *   narratives around meritocracy and opportunity while engineering
 *   algorithms that concentrate outcomes. Emerging creators face trapped
 *   structural position: platform dependence is mandatory for reach,
 *   algorithm mechanics are opaque, and alternative distribution channels
 *   have high friction. Income concentration increases over the measurement
 *   interval (0.35 → 0.62), indicating that extraction mechanisms are
 *   accumulating rather than correcting. Theater ratio also increases,
 *   suggesting platforms are investing more in legitimacy maintenance as
 *   concentration becomes visible.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victims (powerless/trapped) — lack independent audience, capital, or exit options; bear full cost of platform income concentration mechanism
 *   - Middle-Tier Creators: Secondary victims (moderate/constrained) — have developed partial audience independence and some platform alternatives, but remain substantially constrained by audience lock-in and switching costs
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract value through commission structure, data ownership, and algorithmic curation that directs attention to promoted creators; can restructure or exit mechanism with no friction
 *   - Algorithmic Curators: Secondary beneficiaries (institutional/arbitrage) — benefit from concentration through promotion of favored creators, access to creator data, and attention concentration
 *   - Capital Holders: Tertiary beneficiaries (institutional/arbitrage) — fund platforms and benefit from creator labor extraction; no direct exposure to creator economics
 *   - Creator Cooperative Movement: Organized actors (organized/constrained) — building alternatives (Patreon-like models, creator DAOs, open-source platforms); perceive concentration as temporary coordination problem being solved
 *   - Traditional Media Institutions: Institutional actors (institutional/arbitrage) — once controlled creator distribution; now reduced to legitimacy theater and IP portfolio management
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform concentration as emergent property of attention economies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creator_income_concentration, 0.62).
domain_priors:suppression_score(creator_income_concentration, 0.68).
domain_priors:theater_ratio(creator_income_concentration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creator_income_concentration, extractiveness, 0.62).
narrative_ontology:constraint_metric(creator_income_concentration, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(creator_income_concentration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creator_income_concentration, snare).
narrative_ontology:human_readable(creator_income_concentration, "Creator Income Concentration").
narrative_ontology:topic_domain(creator_income_concentration, "economic/digital_labor/cultural_production").

domain_priors:requires_active_enforcement(creator_income_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creator_income_concentration, platform_operators).
narrative_ontology:constraint_beneficiary(creator_income_concentration, algorithmic_curators).
narrative_ontology:constraint_beneficiary(creator_income_concentration, capital_holders).
narrative_ontology:constraint_victim(creator_income_concentration, non_featured_creators).
narrative_ontology:constraint_victim(creator_income_concentration, middle_tier_creators).
narrative_ontology:constraint_victim(creator_income_concentration, emerging_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Trapped by platform dependence, algorithm opacity, and lack of alternative distribution channels. Bears full cost of income concentration with no exit mechanism. Career viability depends on algorithms they cannot see or influence. Maximum extraction experienced.
constraint_indexing:constraint_classification(creator_income_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MIDDLE-TIER CREATOR (TANGLED ROPE) — Constrained by platform switching costs, audience lock-in, and income dependency, but benefits from some platform coordination functions (audience discovery, payment infrastructure). Mixed extraction and coordination — significant agency but substantial costs to exit.
constraint_indexing:constraint_classification(creator_income_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Net beneficiary. Experiences the constraint as coordination of creator labor and audience attention. Can exit or restructure terms with minimal friction. Benefits flow predictably toward platform; creator income concentration is profitable feature, not costly bug.
constraint_indexing:constraint_classification(creator_income_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREATOR COOPERATIVE MOVEMENT (SCAFFOLD) — Organized agents (creator unions, open-source platforms, creator DAOs) are building alternative distribution and payment mechanisms with explicit sunset logic. See the concentration as a temporary coordination failure being replaced by decentralized models. Low experienced extraction because they perceive and are building alternatives.
constraint_indexing:constraint_classification(creator_income_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL MEDIA INSTITUTION (PITON) — Once-dominant distribution mechanism now largely performative. Studio gatekeeping and talent management exist primarily as theatrical legitimacy mechanisms, no longer as real bottlenecks. Maintained through institutional inertia and IP portfolio value rather than functional necessity. Theater ratio high because legitimacy theater persists despite reduced functional distribution control.
constraint_indexing:constraint_classification(creator_income_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical perspective, Pareto/power-law distributions of attention and income are emergent properties of attention economies with low-cost replication. Creator income concentration appears as a consequence of scale-free network dynamics — an inherent law of attention markets. However, this naturalizes what is structurally a contingent institutional arrangement: platform algorithm design, payment structure, and discovery mechanisms that concentrate rather than distribute.
constraint_indexing:constraint_classification(creator_income_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creator_income_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(creator_income_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(creator_income_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(creator_income_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(creator_income_concentration, TR),
    TR >= 0.70.

:- end_tests(creator_income_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The income concentration mechanism extracts from creators through multiple channels: platform commission structures (typically 30-50%), algorithmic demotion (creators not in algorithmic favor lose visibility), and data extraction (creator content generates training data and behavioral insights). The platform is the beneficiary of all three extraction mechanisms. Measurement progression (0.35 → 0.62) shows extraction accumulating as platforms optimize discovery algorithms toward concentration and as platform dependency deepens. Suppression (0.68): Very high. Barriers to creator exit include: mandatory platform presence for reach (no viable alternative distribution), algorithmic opacity preventing optimization, audience lock-in (audiences expect creators on major platforms), career path dependence (building an audience takes years), and lack of portability (audience cannot move with creator). Theater ratio (0.58): Moderate-high. Platforms maintain extensive legitimacy theater around meritocracy, opportunity, and creator support, while engineering algorithms that concentrate outcomes. The theater serves suppression by preventing creators from recognizing the concentration as extractive rather than inevitable. Claimed type (Snare): High extraction, high suppression, asymmetric enforcement. Platform maintains the constraint actively (algorithmic design choices), suppresses alternatives (exclusive agreements, algorithm dampening of external links), and derives clear benefit from concentration.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how identical structural mechanisms appear as different types depending on the agent's structural position. Platform operators see pure coordination (Rope) — they are solving the real problem of connecting creators with audiences. Creator cooperatives see a temporary problem with alternatives emerging (Scaffold) — blockchain, open-source platforms, and union organizing are building exit paths. Traditional media see their own degradation (Piton) — gatekeeping legitimacy persists but no longer controls distribution. Middle-tier creators see mixed extraction and coordination (Tangled Rope) — platforms genuinely enable reach but extract heavily. Emerging creators see pure extraction (Snare) — the system takes their labor output and channels income toward others with no alternative. The analytical observer risks seeing concentration as an immutable law of attention economies (Mountain) — power laws are inevitable, so concentration must be inevitable. But the structural data reveals this as a false summit: algorithm design choices, payment structures, and platform policies that concentrate rather than distribute are contingent institutional arrangements, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position in the extraction flow. Emerging creators occupy d ≈ 0.95 (full targets): powerless agents facing trapped exit with no alternatives, bearing maximum experienced extraction through f(d) ≈ 1.42. Middle-tier creators occupy d ≈ 0.70 (victims with some agency): moderate power and constrained exit options produce d ≈ 0.70, f(d) ≈ 1.00, moderate experienced extraction. Platform operators occupy d ≈ 0.10 (full beneficiaries): institutional power and arbitrage exit options produce low d, negative f(d), clear benefit flow. The perspectival gap reflects these different d values: emerging creators see snare (high extraction), middle-tier creators see tangled rope (mixed), platforms see rope (coordination benefit). No override needed — structural derivation accurately captures different agent positions.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED by structural metrics and perspectival convergence. The constraint meets all three Snare gates: extractiveness (0.62 ≥ 0.46), suppression (0.68 ≥ 0.60), effective extraction χ ≥ 0.66 for powerless creators (ε=0.62 × f(d≈0.95)≈1.42 × σ(global≈1.2) ≈ 1.05, though χ is capped at effective range). Mandatrophy resolved by: (1) Clear victim identification (emerging creators, non-featured creators bear measurable extraction), (2) Asymmetric enforcement (platform actively engineers concentration through algorithm design), (3) Suppression of alternatives (exclusive agreements, algorithm dampening of external platforms), (4) Network closure (creators cannot exit without losing audience). The platform benefits from concentration, not from creators' improved outcomes. This is not a coordination mechanism being misread as extraction — coordination functions exist (audience discovery, payment processing) but are subordinate to the extraction mechanism. The extraction is active and intentional (algorithm design that concentrates), not incidental. False summit risk: the Mountain perspective naturalizes concentration as inevitable law of attention. Structural data contradicts this — comparison of platforms with different algorithm designs shows different concentration distributions, proving concentration is contingent on design choices, not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity_mechanism,
    'Is income concentration driven by genuine algorithmic constraints on discovery or by opaque ranking that serves platform interests?',
    'Algorithm transparency analysis: comparison of actual discovery mechanics against platform stated objectives; study of whether transparent ranking systems reduce concentration',
    'If genuine constraint: concentration reflects attention limitations, less extractive. If opaque design: concentration reflects engineered outcome, more extractive and more suppressive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_mechanism, empirical, 'Whether concentration is algorithmic constraint or engineered outcome').

omega_variable(
    creator_awareness_and_exit,
    'Do creators understand the concentration mechanism and their exit options, or is the suppression substantially internalized through cognitive capture?',
    'Survey of creator knowledge of algorithm mechanics, alternative platforms, and perceived viability of exit; correlation with actual exit behavior when alternatives become visible',
    'If awareness low and trapped: suppress value near 0.68. If awareness high but exit costs prohibitive: constrained agents outnumber trapped; snare classification weakens to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_awareness_and_exit, empirical, 'Creator understanding and perceived exit viability').

omega_variable(
    network_effect_lock_in_degree,
    'How much of platform lock-in is network effect necessity versus switching cost extraction?',
    'Historical analysis of platform fragmentation and migration events; comparison of migration rates when alternatives offer feature parity without network effect advantages; study of niche platforms with lower concentration',
    'If network effect dominant: lock-in is genuine coordination benefit, tangled_rope stronger. If switching costs dominant: lock-in is extractive, snare stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_lock_in_degree, empirical, 'Network effect necessity versus artificial lock-in').

omega_variable(
    cooperative_platform_viability,
    'Can decentralized creator cooperatives and open platforms actually replace platform concentration, or is the scaffold sunset aspirational?',
    'Analysis of existing cooperative platform adoption, creator switching behavior, and comparative transaction costs; longitudinal tracking of whether cooperative platforms reduce individual creator income concentration',
    'If viable: scaffold perspective confirmed, extractiveness declining as alternatives mature. If unviable: scaffold is aspirational only, snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_platform_viability, empirical, 'Viability of creator cooperative alternatives').

omega_variable(
    suppression_internalization,
    'Is measured suppression (0.68) structural (platform design barriers) or substantially internalized through creator identity fusion and aspirational thinking?',
    'Post-exit analysis: if creators exit platforms but retain belief in inevitability of concentration, suppression is internalized. Measurement of creator narratives about career viability before and after exposure to alternative models.',
    'If internalized: constraint persists in creator belief systems even absent platform mechanisms; effective suppression higher than structural measure. Identity-locked exit option applies to subset of creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creator_income_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cic_tr_t0, creator_income_concentration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cic_tr_t5, creator_income_concentration, theater_ratio, 5, 0.5).
narrative_ontology:measurement(cic_tr_t10, creator_income_concentration, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cic_be_t0, creator_income_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cic_be_t5, creator_income_concentration, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cic_be_t10, creator_income_concentration, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creator_income_concentration, resource_allocation).
narrative_ontology:affects_constraint(creator_income_concentration, platform_lock_in).
narrative_ontology:affects_constraint(creator_income_concentration, algorithmic_opacity).
narrative_ontology:affects_constraint(creator_income_concentration, creative_labor_exploitation).

% DUAL FORMULATION NOTE:
% Creator income concentration is downstream of and interconnected with platform lock-in (structural network effect dependence), algorithmic opacity (mechanism that concentrates or distributes depending on design), and creative labor exploitation (extraction mechanism). Each of these constraints has its own ε value and perspectival structure; network links show structural dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
