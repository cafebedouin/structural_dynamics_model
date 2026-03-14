% ============================================================================
% CONSTRAINT STORY: api_surface_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_api_surface_stability, []).

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
 *   constraint_id: api_surface_stability
 *   human_readable: API Surface Stability Constraint in Software Ecosystems
 *   domain: software_engineering/platform_governance
 *
 * SUMMARY:
 *   API surface stability in software ecosystems represents a structural
 *   tension between the need for ecosystem coordination (stable interfaces
 *   enable third-party innovation) and the extraction mechanism embedded in
 *   that stability (developers become locked into platform choices and forced
 *   to accept breaking changes on the maintainer's schedule). The constraint
 *   exhibits the full spectrum of Deferential Realism types: from the
 *   downstream developer's perspective, API changes are imposed without
 *   consent (Snare); from the maintainer's perspective, stability is a
 *   coordination mechanism (Rope); from commercial vendors, the constraint
 *   provides both coordination value and extraction opportunity (Tangled
 *   Rope); from standards bodies, it is a temporary coordination function
 *   with sunset properties (Scaffold); from legacy compatibility layers, it
 *   is largely performative (Piton); and from the civilizational analytical
 *   view, it risks appearing as a natural law when it is actually an
 *   institutional choice. The constraint's extractiveness has increased from
 *   0.35 to 0.58 over the measurement interval, reflecting the accumulation
 *   of ecosystem lock-in effects as platforms mature: early-stage platforms
 *   have low extraction (many alternatives exist), but successful platforms
 *   develop thick ecosystems that make exit costly. Theater ratio has
 *   remained moderate (0.35–0.48), suggesting that API governance is
 *   substantially functional rather than purely performative — the versioning
 *   frameworks, deprecation notices, and compatibility tools serve real
 *   coordination functions, not just ritual.
 *
 * KEY AGENTS:
 *   - Downstream Developers: Primary victims (powerless/trapped) — locked into platform choices by ecosystem dependencies and switching costs; forced to accept breaking changes on maintainer schedule
 *   - Organized Developer Coalitions: Secondary victims (organized/constrained) — collective action through foundations and standards provides voice but limited exit leverage
 *   - Platform Maintainers: Primary beneficiaries (institutional/arbitrage) — control versioning decisions; can choose deprecation timelines and migration burdens; benefit from ecosystem coordination value
 *   - Commercial Platform Vendors: Mixed (institutional/constrained) — capture extraction through breaking changes and upgrade cycles; also provide genuine coordination; constrained by competitive pressure and customer churn risk
 *   - Standards Bodies: Organized actors (organized/mobile) — coordinate protocol stability and versioning frameworks; mobile exit options enable sunset clause logic
 *   - Legacy Compatibility Layers: Degraded institutional (institutional/arbitrage) — maintenance of deprecated versions persists through inertia; increasingly performative as ecosystem evolves
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent design choices as immutable requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(api_surface_stability, 0.58).
domain_priors:suppression_score(api_surface_stability, 0.65).
domain_priors:theater_ratio(api_surface_stability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(api_surface_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(api_surface_stability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(api_surface_stability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(api_surface_stability, tangled_rope).
narrative_ontology:human_readable(api_surface_stability, "API Surface Stability Constraint in Software Ecosystems").
narrative_ontology:topic_domain(api_surface_stability, "software_engineering/platform_governance").

domain_priors:requires_active_enforcement(api_surface_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(api_surface_stability, platform_maintainers).
narrative_ontology:constraint_beneficiary(api_surface_stability, early_adopters).
narrative_ontology:constraint_victim(api_surface_stability, downstream_developers).
narrative_ontology:constraint_victim(api_surface_stability, ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM DEVELOPER (SNARE) — Trapped by dependency lock-in. Breaking API changes force costly refactoring; migration to alternative platforms incurs switching costs and loses established integrations. No meaningful exit without bearing full burden of deprecation lag and rewrite costs. Experiences pure extraction through forced maintenance labor.
constraint_indexing:constraint_classification(api_surface_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED DEVELOPER COALITION (TANGLED ROPE) — Collective action through open-source foundations and standards bodies provides voice in API governance (coordination function). However, constrained exit: switching frameworks incurs ecosystem switching costs; leverage is limited because platform maintainers control versioning. Mixed experience of both coordination benefit and asymmetric extraction.
constraint_indexing:constraint_classification(api_surface_stability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM MAINTAINER (ROPE) — Benefits from ecosystem coordination: stable APIs attract developers, reducing fragmentation. Has arbitrage options: can version APIs, maintain legacy branches, or deprecate gradually. Experiences the constraint as enabling coordination rather than extractive — the stability boundary is a collective good that benefits maintainers through ecosystem health.
constraint_indexing:constraint_classification(api_surface_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL PLATFORM VENDOR (TANGLED ROPE) — Coordinates ecosystem activity (genuine coordination function: ensures interoperability, enables third-party innovation). Also extracts through API versioning: breaking changes force paid upgrades or consulting. Capital requirements create switching costs for enterprise customers. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(api_surface_stability, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: STANDARDS BODY (SCAFFOLD) — Coordinating role through protocol standardization and versioning frameworks. Mobile exit options: standards bodies can shift focus to new platforms or retire obsolete standards. Temporary support function with implicit sunset: as protocols mature and stabilize, the standards-setting constraint naturally winds down. Theater ratio reflects the performative aspects of standardization processes.
constraint_indexing:constraint_classification(api_surface_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY COMPATIBILITY LAYER (PITON) — Maintenance of deprecated API versions is largely performative: minimal new functionality, primarily ritual adherence to backward-compatibility promises. The layer persists through institutional inertia — enterprises depend on it, but the actual maintenance burden is low and decreasing. Theater derives from the ceremonial nature of long-term compatibility maintenance for obsolete interfaces.
constraint_indexing:constraint_classification(api_surface_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical/logical perspective, API surface stability reflects an inherent constraint: any interface exposed to external users creates a commitment that limits future implementation freedom. The stability requirement follows from logical necessity: breaking interfaces logically breaks dependent code. This perspective risks naturalizing what is actually a contingent institutional choice — other design patterns (protocol versioning, interface deprecation, capability-based security) could distribute the stability burden differently.
constraint_indexing:constraint_classification(api_surface_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(api_surface_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(api_surface_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(api_surface_stability, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(api_surface_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(api_surface_stability, TR),
    TR >= 0.70.

:- end_tests(api_surface_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The platform maintainer captures significant value through version control: breaking changes force labor from downstream developers (refactoring, testing, migration). However, extraction is not as severe as pure monopoly rent (which would justify 0.75+) because: (1) some platforms maintain longer deprecation windows, reducing immediate friction; (2) open-source alternatives create countervailing pressure; (3) some developers have exit options if the platform is not critical to their business. The increase from 0.35 to 0.58 over the measurement interval reflects ecosystem maturation — as more critical third-party libraries accumulate, lock-in deepens and extraction rises. Suppression (0.65): High. Multiple barriers prevent alternatives: (1) ecosystem complementarity (libraries, integrations, institutional knowledge); (2) switching costs (rewrite effort, risk of new platform immaturity); (3) organizational inertia (training investments, code bases, vendor relationships); (4) network effects (adoption concentrates on platforms with the largest ecosystems). These barriers are real external constraints, not psychological factors. Theater ratio (0.48): Moderate-low. API governance is substantially functional — semantic versioning, deprecation schedules, and compatibility documentation serve real coordination purposes. However, some performative elements exist: (1) deprecation timelines often exceed actual maintenance burden (ritualized obsolescence); (2) backward-compatibility promises sometimes conflict with technical necessity, creating theater around impossible constraints; (3) version numbering conventions have evolved beyond technical utility into marketing signals. The low-to-moderate theater indicates the constraint is more coordination than degradation, but not purely functional.
 *
 * PERSPECTIVAL GAP:
 *   The platform maintainer and downstream developer experience radically different constraints. The maintainer sees Rope — a coordination mechanism that enables ecosystem growth and third-party innovation. The developer sees Snare — imposed stability requirements that limit their exit options. The organized coalition sees Tangled Rope — genuine coordination value (standards prevent fragmentation) mixed with asymmetric extraction (standards codify the status quo and disadvantage challengers). The standards body sees Scaffold — a temporary coordination function that will sunset as protocols stabilize and new platforms emerge. The legacy compatibility layer sees Piton — the ritual of maintaining deprecated interfaces persists more from institutional inertia than functional necessity. The civilizational observer risks seeing Mountain — stability as an immutable law of software engineering — but the structural data reveals this as a false summit: other design patterns (capability-based versioning, parallel protocol support, gradual migration strategies) could distribute the stability burden differently. The perspectival gaps reveal that API stability is not a naturally emerging coordination problem but an institutional choice that concentrates decision-making power in the maintainer.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform maintainers (beneficiaries with arbitrage options) experience low-to-negative effective extraction — they control versioning and can extract value from lock-in while maintaining ecosystem health. Downstream developers (victims with trapped/constrained exit) experience high effective extraction — they bear the burden of forced migrations and have limited alternatives. The extraction is amplified by global scope (platform decisions affect worldwide developer populations) and institutional enforcement (maintainers control release schedules and can impose breaking changes unilaterally). Organized coalitions (organized power with constrained exit) experience moderate extraction — they gain coordination voice through standards bodies but cannot unilaterally choose exit. The directionality spreads across the spectrum: from negative (beneficiaries with full arbitrage) through high-positive (powerless victims) to moderate-positive (organized actors with constrained exit). This distribution is the signature of Tangled Rope: genuine coordination function (standards prevent fragmentation) overlaid on asymmetric extraction (decisions concentrate in maintainer hands, developers bear migration costs).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that API stability serves genuine coordination functions while enabling extraction. The mandatrophy question — 'Is this pure coordination or pure extraction?' — has a coherent answer: it is hybrid. The coordination function is real: without stable API surfaces, ecosystem fragmentation would explode, third-party libraries would become unmaintainable, and developers would face incompatibility chaos. The extraction is also real: maintainers use version control to impose labor burdens on downstream developers, lock-in deepens as ecosystems grow, and the cost of migration becomes irreversible for many organizations. The Tangled Rope classification indicates this hybrid structure. The false mountain perspective (Perspective 7) is diagnosed as naturalization: API stability is treated as an immutable law ('interfaces always create stability requirements'), when it is actually a contingent choice about how to distribute burden and power. Other design patterns — protocol versioning, capability-based security, or intentionally-designed migration pathways — could reduce extraction while preserving coordination. The mandatrophy is not a paradox; it is a clarification that coordination and extraction are not mutually exclusive. This constraint exhibits both, in measurable proportions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    breaking_change_severity_threshold,
    'What constitutes a ''breaking change'' vs. a permissible API evolution?',
    'Empirical semantic versioning compliance analysis; measurement of actual developer friction from purportedly non-breaking changes; correlation between change classification and downstream migration costs',
    'If threshold is strict: many necessary improvements classified as breaking, increasing extraction. If threshold is loose: genuine instability persists, reducing constraint effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breaking_change_severity_threshold, empirical, 'Threshold for classifying breaking changes vs API evolution').

omega_variable(
    exit_cost_distribution_ambiguity,
    'Are downstream developers trapped or constrained? Do viable alternative platforms exist at comparable cost, or is the platform choice genuinely irreversible for most use cases?',
    'Migration feasibility studies: cost analysis of moving production systems to alternative platforms; survey of actual switching attempts and their success rates; measurement of ecosystem complementarity (how many critical third-party libraries would need reimplementation)',
    'If truly trapped (irreversible): snare classification dominates; pure extraction drives development. If constrained (high-cost exit): tangled_rope dominates; extraction is hybrid with coordination benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_distribution_ambiguity, empirical, 'Whether downstream developers are trapped or constrained').

omega_variable(
    standardization_enforcement_mechanism,
    'Does API stability arise from genuine technical coordination (protocols require compatibility) or from governance choices (vendors could support multiple breaking versions simultaneously)?',
    'Comparative analysis: platforms that support parallel incompatible APIs (e.g., Python 2/3 era, Node.js major versions) vs. those maintaining strict backward compatibility; cost-benefit measurement of multi-version support vs. forced migration',
    'If technical necessity: mountain classification is justified. If governance choice: the constraint is contingent (Tangled Rope or Snare), and alternative versioning architectures could distribute stability burden differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standardization_enforcement_mechanism, conceptual, 'Whether API stability is technical necessity or governance choice').

omega_variable(
    ecosystem_lock_in_mechanism,
    'Is ecosystem lock-in (third-party libraries, integrations, institutional knowledge) the primary mechanism that traps developers, or can platforms establish migration-friendly ecosystems?',
    'Case studies of successful platform migrations (Windows→Linux, deprecated-language migrations); measurement of ecosystem library porting costs; analysis of platforms that maintain high switching ease despite ecosystem size',
    'If lock-in is inevitable: extraction is inherent to any successful platform. If lock-in is engineered: it reflects design choices that could be unmade through open-source strategies or polyglot ecosystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_lock_in_mechanism, empirical, 'Whether ecosystem lock-in is inevitable or engineered').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(api_surface_stability, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apis_tr_t0, api_surface_stability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(apis_tr_t3, api_surface_stability, theater_ratio, 3, 0.42).
narrative_ontology:measurement(apis_tr_t6, api_surface_stability, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(apis_be_t0, api_surface_stability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(apis_be_t3, api_surface_stability, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(apis_be_t6, api_surface_stability, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(api_surface_stability, information_standard).
narrative_ontology:boltzmann_floor_override(api_surface_stability, 0.12).
narrative_ontology:affects_constraint(api_surface_stability, platform_lock_in).
narrative_ontology:affects_constraint(api_surface_stability, semantic_versioning_enforcement).
narrative_ontology:affects_constraint(api_surface_stability, ecosystem_dependency_management).

% DUAL FORMULATION NOTE:
% API surface stability is upstream of platform lock-in (ecosystems create lock-in through API dependency accumulation) and semantic versioning enforcement (the versioning framework is an institutional mechanism for implementing stability). These constraints are decomposed because lock-in has independent extractiveness (ε ≈ 0.75, pure snare) while versioning is more hybrid (ε ≈ 0.58, tangled rope). API surface stability is the parent constraint that structures both downstream phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(api_surface_stability, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
