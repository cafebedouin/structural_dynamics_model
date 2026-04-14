% ============================================================================
% CONSTRAINT STORY: open_format_standardization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_format_standardization, []).

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
 *   constraint_id: open_format_standardization
 *   human_readable: Open Format Standardization as Coordination and Extraction
 *   domain: technology/standards/governance
 *
 * SUMMARY:
 *   Open format standardization exemplifies how a coordination mechanism can
 *   contain embedded extraction. The constraint addresses a genuine
 *   collective action problem: vendors developing proprietary formats
 *   prevents data portability and interoperability. Standards organizations
 *   (W3C, OASIS, ISO) create mechanisms for vendors to coordinate around
 *   shared formats, reducing friction for end users and enabling competition.
 *   Yet the same standardization apparatus can be captured by dominant
 *   vendors to legitimize proprietary extensions, extract switching costs
 *   through 'standards compliance' theater, and lock users into ecosystems
 *   labeled 'open.' The constraint exhibits all characteristics of a Tangled
 *   Rope at institutional level: genuine coordination benefit (vendors can
 *   interoperate) coupled with asymmetric extraction (incumbents set de facto
 *   standards, lock in compliance requirements). At the end-user level, the
 *   constraint appears as pure Snare: users escape proprietary lock-in only
 *   to face conversion costs and new lock-in to open-source implementations.
 *   The theater ratio has risen from 0.35 to 0.55 over the 15-year interval,
 *   indicating increasing performative content: standards organizations
 *   produce detailed specifications for formats that few vendors implement,
 *   or standardize de facto formats already controlled by single vendors,
 *   creating appearance of openness without material increase in
 *   interoperability.
 *
 * KEY AGENTS:
 *   - Locked-In End Users: Primary victims (powerless/trapped) — bear conversion costs when migrating from proprietary to open formats; trapped in ecosystems by data lock-in
 *   - Proprietary Format Incumbents: Primary beneficiaries and secondary victims (institutional/constrained) — benefit from standardization legitimacy while extracting through extension and de facto control; constrained by regulatory pressure
 *   - Standards Organizations: Secondary beneficiaries (institutional/arbitrage) — solve coordination problems; capture prestige and influence through standardization authority
 *   - Interoperability-Dependent Vendors: Secondary victims (moderate/constrained) — need standards to compete but face implementation burden and asymmetric cost
 *   - Open Source Coalition: Organized challengers (organized/mobile) — see standards as temporary scaffolding toward genuinely open ecosystems; have clear exit strategy as implementations mature
 *   - Legacy Standards Bureaucracy: Institutional actor (powerful/mobile) — maintains standards processes for obsolete formats through inertia; theater high but extraction low
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_format_standardization, 0.38).
domain_priors:suppression_score(open_format_standardization, 0.48).
domain_priors:theater_ratio(open_format_standardization, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_format_standardization, extractiveness, 0.38).
narrative_ontology:constraint_metric(open_format_standardization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(open_format_standardization, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_format_standardization, tangled_rope).
narrative_ontology:human_readable(open_format_standardization, "Open Format Standardization as Coordination and Extraction").
narrative_ontology:topic_domain(open_format_standardization, "technology/standards/governance").

domain_priors:requires_active_enforcement(open_format_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_format_standardization, open_standards_advocates).
narrative_ontology:constraint_beneficiary(open_format_standardization, interoperability_dependent_vendors).
narrative_ontology:constraint_victim(open_format_standardization, proprietary_format_incumbents).
narrative_ontology:constraint_victim(open_format_standardization, end_users_locked_into_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN END USER (SNARE) — Users trapped in proprietary ecosystems cannot migrate data without significant loss or conversion cost. The standardization constraint appears to them as a pure extraction mechanism: they bear the cost of vendor lock-in while vendors capture switching costs. Exit is theoretically possible but economically prohibitive.
constraint_indexing:constraint_classification(open_format_standardization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTEROPERABILITY-DEPENDENT VENDOR (TANGLED ROPE) — Mid-tier vendors need open standards to compete with incumbents but face coordination costs in implementing standards and asymmetric extraction by dominant platforms that set de facto standards. Mixed experience: genuine coordination benefit (can compete) alongside asymmetric cost (must conform to others' specs).
constraint_indexing:constraint_classification(open_format_standardization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STANDARDS ORGANIZATION (ROPE) — Benefits from coordination function: W3C, OASIS, ISO standards bodies experience the constraint as solving genuine collective action problems (interoperability across vendors, data portability). Low extraction overhead when standards are truly open. Arbitrage option via selective participation and influence.
constraint_indexing:constraint_classification(open_format_standardization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROPRIETARY FORMAT INCUMBENT (TANGLED ROPE) — Dominant vendors like Microsoft (Office formats), Adobe (PDF), or Apple (proprietary video codecs) simultaneously benefit from standardization (legitimacy, ecosystem participants) and extract through format extensions, non-disclosure of implementation details, and de facto standard-setting. Constrained by regulatory pressure and open-source alternatives; otherwise mobile.
constraint_indexing:constraint_classification(open_format_standardization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE COALITION (SCAFFOLD) — Organized open-source projects and activists see open format standardization as temporary institutional scaffolding enabling transition from proprietary lock-in to genuinely open ecosystems. Mobile agents with clear sunset logic: as open formats mature and achieve critical mass (LibreOffice, GIMP, Blender), the need for standardization constraints decreases. Theater high but declining as implementations proliferate.
constraint_indexing:constraint_classification(open_format_standardization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY STANDARDS BUREAUCRACY (PITON) — Older standardization bodies (legacy ISO working groups for obsolete formats, discontinued W3C Task Forces) maintain processes and governance structures that have atrophied in function. Theater_ratio high: committees continue producing standards no one implements. Effective extraction low because the standards lack enforcement mechanism. Institutional inertia preserves structures long after their utility declined.
constraint_indexing:constraint_classification(open_format_standardization, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / TECHNICAL LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, some friction in format translation is inherent to data serialization: perfect lossless conversion between all formats is mathematically impossible (lossy compression, domain-specific encodings, semantic mismatch). This perspective naturalizes standardization constraints as immutable technical limits. However, the structural data (active vendor coordination, political standardization choices, patent controversies) reveals this as false naturalization.
constraint_indexing:constraint_classification(open_format_standardization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_format_standardization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_format_standardization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_format_standardization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_format_standardization, TR),
    TR >= 0.70.

:- end_tests(open_format_standardization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through conversion costs, implementation complexity for non-incumbents, and theater (maintaining appearance of standardization without full interoperability). However, extractiveness is not severe because: (1) genuine coordination benefit exists (vendors do interoperate better than with purely proprietary formats), (2) open-source alternatives reduce lock-in severity, (3) end users have some exit options (switch to open formats, demand support). Suppression (0.48): Moderate. Users face barriers to exit through data conversion costs, learning curve for new tools, and compatibility issues with existing workflows. But suppression is not extreme: tools exist to convert formats, open-source alternatives reduce vendor dependence, and network effects favor open formats as they reach critical mass. Theater ratio (0.55): Moderate-high and rising. Standardization bodies produce voluminous specifications and engage in elaborate governance processes, but implementation varies widely. De facto standards (Office Open XML, PDF) maintain proprietary extensions that are 'standardized' in name but proprietary in practice. Theater has increased as standardization has become more about legitimacy signaling than enabling actual interoperability. The rising trajectory (0.35 → 0.55) reflects increasing divergence between formal standard scope and genuine interoperability achieved.
 *
 * PERSPECTIVAL GAP:
 *   The constraint appears radically different depending on structural position. From the incumbent's perspective (institutional/arbitrage), standardization is Rope: a coordination mechanism that enables controlled ecosystem expansion. From the end-user's perspective (powerless/trapped), it is Snare: escape from one lock-in directly into another. From the open-source perspective (organized/mobile), it is Scaffold: temporary institutional support for transition to open ecosystems. From the legacy standards bureaucracy perspective (powerful/mobile), it is Piton: governance structures and processes maintained through institutional inertia despite atrophied function. The analytical perspective risks seeing Mountain (technical limits on format interoperability) but this is false naturalization: the limitations are institutional and strategic choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect structural position within the extraction flow. Incumbents (beneficiaries with arbitrary-exit options) derive low d values because they control the standardization process and extract benefits. Trapped end-users derive high d values because they bear conversion costs and cannot exit without economic loss. Mid-tier vendors derive moderate d values because they benefit from coordination access but bear asymmetric costs from dominant-vendor standard-setting. The open-source coalition derives moderate d values from the mobile-exit perspective: they see clear exit path (open implementations reaching critical mass) and thus experience the constraint as temporary rather than extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that 'open format standardization' conflates two distinct constraints with different ε values: (1) format interoperability coordination (ε ≈ 0.15, Rope), and (2) incumbent-controlled standard-setting extraction (ε ≈ 0.52, Tangled Rope or Snare). The first solves a genuine coordination problem; the second is captured by dominant vendors. A complete analysis requires decomposition into separate stories. The tangled_rope classification at institutional level captures both simultaneously: genuine coordination function alongside asymmetric extraction. The rising theater ratio over the interval indicates drift toward Piton characteristics as standardization becomes more performative — visible standards governance without corresponding interoperability gains. The constraint's extractiveness remains moderate because open-source alternatives reduce the binding force of proprietary standards; if open-source alternatives disappeared or were captured, extractiveness would rise sharply toward Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_capture_via_standard_setting,
    'Is the standardization constraint primarily a coordination mechanism enabling interoperability, or primarily an extraction mechanism enabling dominant vendors to lock in their de facto standards as ''open'' while maintaining proprietary extensions?',
    'Analysis of standard adoption rates vs proprietary extension adoption; correlation between formal standard compliance and actual vendor interoperability; examination of patent licensing terms embedded in standards',
    'If primarily coordination: constraint classifies as Rope from institutional perspective and Tangled Rope from vendor perspectives. If primarily extraction: constraint classifies as Snare from end-user perspective and Tangled Rope from all institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_capture_via_standard_setting, empirical, 'Whether standardization is coordination or dominant-vendor extraction').

omega_variable(
    open_format_definition_ambiguity,
    'What constitutes an ''open format''? Is it open if specifications are published but implementation is patented? If implementations exist but reference code is proprietary? If the standard is open but de facto control rests with one vendor?',
    'Comparative analysis of format openness dimensions: specification transparency, patent licensing, reference implementations, barrier to entry for new implementations, vendor lock-in despite formal openness',
    'If ''open'' requires patent-free + reference implementation + low vendor lock-in: many standards currently labeled ''open'' (MPEG formats, some W3C specs with Defensive Patent License) classify as Tangled Rope or Snare. If ''open'' requires only published specification: classification as Rope is over-generous.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_format_definition_ambiguity, conceptual, 'Definition of openness in open format standards').

omega_variable(
    migration_cost_irreversibility,
    'Are migration costs from proprietary to open formats actually reversible and voluntary, or do they lock users into open formats with their own switching costs (vendor dependence on open-format implementations)?',
    'Longitudinal tracking of format migration: real costs incurred; success/failure rates; measurement of lock-in to new ''open'' vendor platforms; comparison of switching costs before and after migration',
    'If migration is reversible and low-cost: the constraint enables user choice (Rope). If migration costs are high and lock users into open-format platforms: the constraint replaces proprietary lock-in with different lock-in (cycle detection — Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migration_cost_irreversibility, empirical, 'Whether format migration costs are reversible or create new lock-in').

omega_variable(
    standards_capture_by_incumbents,
    'Do incumbent vendors use standardization committees to extend their de facto formats into formal standards, thereby converting proprietary extensions into ''standard'' features that smaller vendors must implement to achieve interoperability?',
    'Analysis of standardization committee composition and voting patterns; comparison of incumbent feature proposals vs challenger proposals; measurement of implementation burden for non-incumbent extensions; tracking of which extensions become mandatory vs optional',
    'If standards capture occurs: standardization is an extraction mechanism disguised as coordination (Snare or Tangled Rope depending on agent perspective). If committee independence is genuine: constraint is primarily coordination (Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(standards_capture_by_incumbents, empirical, 'Incumbent vendor capture of standardization processes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_format_standardization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ofmt_tr_t0, open_format_standardization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ofmt_tr_t5, open_format_standardization, theater_ratio, 5, 0.48).
narrative_ontology:measurement(ofmt_tr_t10, open_format_standardization, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ofmt_tr_t15, open_format_standardization, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(ofmt_be_t0, open_format_standardization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ofmt_be_t5, open_format_standardization, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(ofmt_be_t10, open_format_standardization, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ofmt_be_t15, open_format_standardization, base_extractiveness, 15, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_format_standardization, information_standard).
narrative_ontology:boltzmann_floor_override(open_format_standardization, 0.05).
narrative_ontology:affects_constraint(open_format_standardization, vendor_lock_in_digital_formats).
narrative_ontology:affects_constraint(open_format_standardization, data_portability_regulatory_mandate).

% DUAL FORMULATION NOTE:
% Open format standardization is upstream of vendor lock-in constraints but structurally distinct. The coordination function (enabling interoperability) has ε ≈ 0.15 (Rope); the incumbent capture mechanism (extracting through standard-setting control) has ε ≈ 0.52 (Tangled Rope). These are often conflated in policy discourse but represent different structural phenomena with different remedies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_format_standardization, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
