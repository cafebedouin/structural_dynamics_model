% ============================================================================
% CONSTRAINT STORY: tragedy_of_the_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tragedy_of_the_commons, []).

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
 *   constraint_id: tragedy_of_the_commons
 *   human_readable: The Tragedy of the Commons
 *   domain: economic/social
 *
 * SUMMARY:
 *   The tragedy of the commons occurs when individual rationality produces
 *   collective irrationality: each user gains more by extracting one
 *   additional unit than by restraining, so all users extract maximally,
 *   depleting the shared resource faster than it regenerates. This constraint
 *   exhibits structural properties of both coordination failure (rope:
 *   functional commons governance prevents tragedy) and extraction asymmetry
 *   (snare: free riders benefit while restrainers bear costs; tangled_rope:
 *   mixed coordination and extraction). The tragedy is not a natural law but
 *   a contingent institutional arrangement—it emerges when excludability is
 *   absent (open access), the resource is rival (one user's consumption
 *   reduces availability for others), and governance institutions are weak
 *   (no monitoring, enforcement, or norm-setting). The analytical
 *   perspective's mountain classification is a false summit: it naturalizes
 *   what is actually a solvable institutional problem. Historical commons
 *   that persist sustainably (Swiss alpine meadows, Indonesian subak
 *   irrigation, Caribbean lobster trap fisheries) demonstrate that the
 *   tragedy is preventable through community enforcement, property rights
 *   allocation, and resource monitoring—the constraint is tangled_rope or
 *   scaffold, not immutable natural law.
 *
 * KEY AGENTS:
 *   - Subsistence Users: Primary victims (powerless/trapped) — depend entirely on commons for survival; face extraction through overuse by others; no exit option
 *   - Short-Term Extractors: Primary beneficiaries (moderate/mobile) — capture immediate gains from overexploitation; can exit once resource is depleted by moving to new resource or economic activity
 *   - Sustainable Users: Secondary actors (moderate/constrained) — practice voluntary restraint through social norms and collective identity; face free-rider extraction from those who don't restrain
 *   - Community Managers: Organized actors (organized/mobile) — governance bodies, cooperatives, or collective authorities; can implement monitoring, enforcement, and allocation rules
 *   - Regulatory Authority: Institutional actor (institutional/arbitrage) — state or regional government; can impose quotas, licenses, or enclosure; maintains exit through policy reform
 *   - Historical Enclosure Movement: Institutional pattern (institutional/arbitrage) — private property regimes that replaced commons; persist performatively even when original tragedy mechanism is resolved
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks formalizing contingent institutional patterns as immutable natural laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tragedy_of_the_commons, 0.58).
domain_priors:suppression_score(tragedy_of_the_commons, 0.65).
domain_priors:theater_ratio(tragedy_of_the_commons, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tragedy_of_the_commons, extractiveness, 0.58).
narrative_ontology:constraint_metric(tragedy_of_the_commons, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(tragedy_of_the_commons, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tragedy_of_the_commons, tangled_rope).
narrative_ontology:human_readable(tragedy_of_the_commons, "The Tragedy of the Commons").
narrative_ontology:topic_domain(tragedy_of_the_commons, "economic/social").

domain_priors:requires_active_enforcement(tragedy_of_the_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tragedy_of_the_commons, short_term_extractors).
narrative_ontology:constraint_victim(tragedy_of_the_commons, collective_resource_base).
narrative_ontology:constraint_victim(tragedy_of_the_commons, long_term_users).
narrative_ontology:constraint_victim(tragedy_of_the_commons, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSISTENCE USER (SNARE) — Trapped by dependence on the commons for survival. Cannot exit without abandonment of livelihood. Faces maximum extraction: must compete directly against all other users for diminishing resource. No alternative. Experiences pure extraction with high suppression of alternatives.
constraint_indexing:constraint_classification(tragedy_of_the_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SUSTAINABLE USER (TANGLED ROPE) — Constrained by community norms and resource interdependence but also benefits from functioning commons. Faces extraction through overuse by others, but also coordinates with other moderate users on conservation practices. Mixed experience: genuine coordination function (voluntary restraint norms) combined with asymmetric extraction (free riders exploit restraint).
constraint_indexing:constraint_classification(tragedy_of_the_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMMUNITY MANAGER (ROPE) — Organized agent (commons governance body, cooperative, collective) can exit through privatization or alternative management schemes. Experiences constraint as coordination problem: orchestrating monitoring, enforcement, and benefit-sharing solves collective action failure. Benefits from functional commons while maintaining exit option through reform.
constraint_indexing:constraint_classification(tragedy_of_the_commons, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: REGULATORY AUTHORITY (SCAFFOLD) — Can exit through enclosure, privatization, or centralized management. Temporary coordination through quotas, licenses, or seasonal closures. Low effective extraction because the institution has structural exit and sunset: once privatization is complete or resource is enclosed, the tragedy mechanism no longer operates. Theater ratio remains moderate because enforcement is functional rather than performative.
constraint_indexing:constraint_classification(tragedy_of_the_commons, scaffold,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENCLOSURE MOVEMENT (PITON) — Historical response to commons collapse that persists as institutional inertia. Private property regimes were imposed as solution, but many enforced enclosures now persist performatively, constraining use even when the original tragedy mechanism no longer exists (resource abundance, technological substitution, or successful community management). Theater ratio increases when enclosure regime is maintained long after functional need.
constraint_indexing:constraint_classification(tragedy_of_the_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universal/civilizational perspective, the tragedy appears as an immutable consequence of rational individual behavior under open access and excludable resources. The logic seems to be a natural law: whenever individual incentives diverge from collective good under these conditions, tragedy must result. However, this naturalizes what is actually a contingent institutional arrangement. The mountain classification is a false summit — the structural data reveals that commons tragedies are prevented or managed through enforced norms, governance institutions, and property regimes (all social constructs, not natural laws).
constraint_indexing:constraint_classification(tragedy_of_the_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tragedy_of_the_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tragedy_of_the_commons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tragedy_of_the_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tragedy_of_the_commons, TR),
    TR >= 0.70.

:- end_tests(tragedy_of_the_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint extracts from the resource base and from restrainers through free-rider exploitation. Subsistence users bear maximum extraction cost (loss of livelihood sustainability). However, extractiveness is not total (χ ≤ 0.90) because community governance and enclosure regimes do prevent some tragedies—the constraint is preventable, not absolute. The 0.58 value reflects that extractiveness increases over time (0.35 → 0.58) as overuse accelerates and resource depletion sets in. Suppression (0.65): High. Barriers to resource conservation include: (1) individual incentive structure (extra unit always benefits extractor more than restraint benefits community); (2) monitoring costs (difficult to detect who overextracts in large commons); (3) enforcement costs (sanctioning free riders requires collective action); (4) information asymmetries (users may not know regeneration rate). However, suppression is not total—successful commons demonstrate that monitoring and reputation mechanisms can overcome these barriers. Theater ratio (0.38): Moderate-low. The constraint's enforcement is largely functional rather than performative: quota systems, seasonal closures, and monitoring are real mechanisms with measurable effects on extraction. Theater increases over time as enclosure regimes persist beyond functional need, but remains below 0.70 (piton threshold) for active commons management.
 *
 * PERSPECTIVAL GAP:
 *   The tragedy manifests identically to different observers—resource depletion—but the structural mechanisms and classification types differ dramatically based on the observer's position. The subsistence user sees a snare: they are trapped and maximally extracted. The sustainable user sees tangled_rope: genuine coordination function (collective norms reduce extraction) combined with asymmetric extraction (free riders exploit restraint). The community manager sees rope: a solvable coordination problem with governance institutions providing the solution. The regulatory authority sees scaffold: a temporary problem fixable through enclosure or centralized allocation with a sunset (once resource is privatized or enclosed, the tragedy mechanism no longer operates). The enclosure movement perspective sees piton: historical solution that persists institutionally long after functional need. The analytical observer risks seeing a mountain (natural law: rational individuals always deplete commons) but the structural data reveals this as a false summit—the extractiveness depends on institutional design, not natural law. The perspectival gap reflects that the tragedy is not a single constraint but a configuration of institutional arrangements that different actors experience differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's structural position relative to the extraction flow. Short-term extractors benefit from the constraint (beneficiaries with mobile exit options) → low d → negative χ. Subsistence users depend on the commons and face extraction through overuse (victims with trapped exit) → high d → high χ. Sustainable users practice voluntary restraint and face free-rider exploitation (victims with constrained exit, but also benefit from functioning commons) → moderate-high d. Community managers can coordinate governance solutions (organized actors with mobile exit through reform) → moderate d. Regulatory authorities can enclose or allocate (institutional actors with arbitrage exit) → low d. The engine derives d automatically from beneficiary/victim declarations and exit options; the commentary reflects the structural rationale for each perspective's experienced extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    excludability_vs_rivalry,
    'Does the resource exhibit true excludability and rivalry, or can governance institutions create artificial excludability through property rights assignment?',
    'Analysis of whether property rights regimes prevent access or merely allocate extraction rights; comparison of resource depletion under open access vs managed regimes controlling for external demand pressures',
    'If artificial: tragedy is institutional failure, not natural law. If true property: some commons cannot escape tragedy regardless of governance. Shifts classification from false mountain toward robust tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(excludability_vs_rivalry, conceptual, 'Whether excludability is intrinsic to resource or constructed through property regime').

omega_variable(
    rational_actor_assumption,
    'Do actual users behave as rational profit-maximizers, or do social preferences, fairness norms, and reputation concerns change the payoff structure?',
    'Behavioral economics experiments (ultimatum game, commons dilemmas with reputation signaling); ethnographic study of actual commons-user decision-making; comparison of predicted vs observed extraction rates',
    'If norms dominate: tragedy mechanism is weaker than predicted; rope classification becomes more accurate. If rational extraction dominates: snare classification strengthened; community management fails without enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rational_actor_assumption, empirical, 'Extent to which actual users conform to rational actor model').

omega_variable(
    scalability_of_governance,
    'Can community-based governance mechanisms (social monitoring, reputation, collective enforcement) scale to large populations and geographically distributed users?',
    'Comparative study of commons governance success by scale; analysis of transaction costs of monitoring and enforcement as user population increases; case studies of scale-induced governance collapse',
    'If scales poorly: large commons are structurally snares/tangled_ropes (dependent on state enclosure or privatization). If scales well: scaffold perspective is less relevant; community management is genuine long-term solution (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_of_governance, empirical, 'Scalability limits of community-based commons governance').

omega_variable(
    resource_regeneration_rate,
    'Does the resource''s natural regeneration rate determine whether sustainable yield is possible, or can institutional design overcome biological limits?',
    'Bioeconomic modeling of sustainable yield thresholds; historical analysis of whether commons collapsed due to biological limits or institutional failure; comparison of managed commons sustainability at different regeneration rates',
    'If biology is binding: some commons are structurally mountains (unavoidable tragedy at certain extraction levels). If institutional design dominates: tragedy is preventable governance failure (snare/tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_regeneration_rate, empirical, 'Whether resource regeneration rate determines sustainability possibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tragedy_of_the_commons, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(totc_tr_t0, tragedy_of_the_commons, theater_ratio, 0, 0.25).
narrative_ontology:measurement(totc_tr_t5, tragedy_of_the_commons, theater_ratio, 5, 0.32).
narrative_ontology:measurement(totc_tr_t10, tragedy_of_the_commons, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(totc_be_t0, tragedy_of_the_commons, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(totc_be_t5, tragedy_of_the_commons, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(totc_be_t10, tragedy_of_the_commons, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tragedy_of_the_commons, resource_allocation).
narrative_ontology:affects_constraint(tragedy_of_the_commons, overfishing_north_atlantic).
narrative_ontology:affects_constraint(tragedy_of_the_commons, deforestation_commons).
narrative_ontology:affects_constraint(tragedy_of_the_commons, groundwater_depletion).
narrative_ontology:affects_constraint(tragedy_of_the_commons, atmospheric_carbon_accumulation).

% DUAL FORMULATION NOTE:
% The tragedy of the commons is a structural family with domain-specific instantiations (fisheries, forests, water, atmosphere). Each instantiation has its own extractiveness and institutional context. The generic constraint story applies the abstract mechanism; domain stories (overfishing, deforestation) apply it to specific resources. All family members share the tangled_rope/snare hybrid structure and the false mountain risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tragedy_of_the_commons, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
