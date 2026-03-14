% ============================================================================
% CONSTRAINT STORY: state_capacity_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_fragmentation, []).

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
 *   constraint_id: state_capacity_fragmentation
 *   human_readable: State Capacity Fragmentation Across Bureaucratic Domains
 *   domain: political/institutional/governance
 *
 * SUMMARY:
 *   State capacity fragmentation across bureaucratic domains represents a
 *   structural tension between the need for specialized expertise (which
 *   requires jurisdictional boundaries) and the need for integrated service
 *   delivery (which requires breaking those boundaries). Citizens requiring
 *   services across multiple agencies face duplication, contradictory
 *   requirements, and service gaps. Entrenched agencies benefit from
 *   fragmentation through jurisdictional autonomy and budget defense.
 *   Mid-level bureaucrats benefit from domain-specific expertise and job
 *   security. This constraint demonstrates all six DR types from different
 *   perspectives: an immutable law of federal governance (mountain), pure
 *   extraction on coordination-dependent populations (snare), mixed
 *   coordination-extraction for mid-level actors (tangled_rope), a temporary
 *   problem being solved by integration initiatives (scaffold), a degraded
 *   civil service system maintained by inertia (piton), and genuine
 *   coordination mechanisms within agency domains (rope). The theater_ratio
 *   (0.68) reflects that agencies engage in performative coordination
 *   (interagency committees, joint planning) without functional integration.
 *   Extractiveness has increased over the 20-year interval as technical
 *   barriers became easier to overcome but institutional resistance to
 *   data-sharing has hardened, suggesting the fragmentation is increasingly
 *   deliberate extraction rather than unavoidable technical constraint.
 *
 * KEY AGENTS:
 *   - Coordination-Dependent Populations: Primary victim (powerless/trapped) — citizens requiring services across multiple fragmented agencies; cannot exit bureaucratic fragmentation
 *   - Entrenched Agencies: Primary beneficiary (institutional/arbitrage) — capture budget autonomy, jurisdictional gatekeeping, and regulatory authority through fragmentation
 *   - Mid-Level Bureaucrats: Secondary beneficiary (moderate/constrained) — benefit from domain expertise and job security; also constrained by artificial jurisdictional boundaries
 *   - Reform Coalition: Organized agent (organized/mobile) — good-government organizations, public administration reformers building integration pathways with sunset logic
 *   - Legacy Civil Service System: Institutional actor (institutional/arbitrage) — maintains fragmented structure through inertia; sees integration as threat to established career paths and pension structures
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional design as necessary federalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_fragmentation, 0.58).
domain_priors:suppression_score(state_capacity_fragmentation, 0.62).
domain_priors:theater_ratio(state_capacity_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(state_capacity_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(state_capacity_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_fragmentation, tangled_rope).
narrative_ontology:human_readable(state_capacity_fragmentation, "State Capacity Fragmentation Across Bureaucratic Domains").
narrative_ontology:topic_domain(state_capacity_fragmentation, "political/institutional/governance").

domain_priors:requires_active_enforcement(state_capacity_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_fragmentation, entrenched_agencies).
narrative_ontology:constraint_beneficiary(state_capacity_fragmentation, regulatory_gatekeepers).
narrative_ontology:constraint_victim(state_capacity_fragmentation, public_service_recipients).
narrative_ontology:constraint_victim(state_capacity_fragmentation, coordination_dependent_populations).
narrative_ontology:constraint_victim(state_capacity_fragmentation, emerging_policy_domains).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COORDINATION-DEPENDENT POPULATION (SNARE) — Citizens requiring services across multiple fragmented agencies (health + housing + employment support) face insurmountable barriers. No exit option exists; cannot bypass bureaucratic fragmentation. Bears full cost of duplication, contradictory requirements, and service gaps. Maximum experienced extraction — abstract collective has no advocate and cannot organize.
constraint_indexing:constraint_classification(state_capacity_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL BUREAUCRAT (TANGLED ROPE) — Constrained by jurisdictional boundaries, resource siloing, and career risk of cross-agency initiatives. But also benefits from fragmentation: domain expertise provides local coordination value, and fragmentation creates job security and specialized authority. Genuine extraction exists (forced participation in dysfunctional system) alongside coordination benefit (expert niche protection).
constraint_indexing:constraint_classification(state_capacity_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ENTRENCHED AGENCY LEADERSHIP (ROPE) — Benefits from fragmentation through autonomy, budget defense, and jurisdictional gatekeeping. Experiences constraint as coordination of their own domain. Can arbitrage regulatory exceptions and budget allocations. Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(state_capacity_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Public administration reformers, good-government organizations, and policy entrepreneurs see fragmentation as a temporary coordination failure with a sunset: interagency task forces, unified digital platforms (e.g., government service portals), and shared data infrastructure are building integration pathways. Low effective extraction because organized coalition has agency and sees exit path toward unified service delivery.
constraint_indexing:constraint_classification(state_capacity_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CIVIL SERVICE SYSTEM (PITON) — Fragmentation persists through institutional inertia despite causing documented service failures. Civil service rules, pension structures, and career paths are built around agency silos. The system performs theater of coordination (interagency committees, joint planning) without functional integration. Maintained because alternatives haven't fully displaced it, not because it works. Piton classification derives from theater gate.
constraint_indexing:constraint_classification(state_capacity_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some jurisdictional fragmentation in large states is inherent to federalism and institutional differentiation: different domains require different expertise and accountability structures, and complete integration would destroy the separation of concerns that enables democratic accountability. This perspective sees fragmentation as an immutable structural property of complex governance. However, the structural data contradicts this mountain classification — the engine will compute this as a false summit, revealing that 'necessary federalism' naturalizes what is actually a contingent design choice.
constraint_indexing:constraint_classification(state_capacity_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_capacity_fragmentation, TR),
    TR >= 0.70.

:- end_tests(state_capacity_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Fragmentation creates genuine extraction through duplicated requirements, coordination failures, and service gaps that fall on powerless populations. The value reflects that extraction is real but not maximal — some fragmentation serves legitimate specialization purposes, and some populations can navigate multiple agencies. The increasing trajectory from 0.42 to 0.58 reflects that technical constraints have become easier to overcome, so remaining fragmentation is increasingly a choice rather than necessity, suggesting institutional extraction. Suppression (0.62): Moderate-high. Significant barriers to exit include legal requirements to provide individual information to multiple agencies, geographic access barriers to multiple physical offices, and cognitive burden of learning multiple bureaucratic systems. Suppression is not total — some populations can hire intermediaries or exploit informal networks. Theater ratio (0.68): High and increasing. Agencies engage in substantial performative coordination: interagency committees that produce reports but not integrated service delivery, joint planning sessions that respect jurisdictional boundaries, and pilot projects that don't scale beyond pilot populations. The trajectory from 0.45 to 0.68 reflects that theater has increased as technical integration became possible but institutional resistance hardened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. Entrenched agencies see coordination within their domain (rope) — they are solving legitimate specialization problems. Reform coalitions see a temporary problem with a sunset (scaffold) — unified digital platforms and data-sharing infrastructure are building integration pathways. The legacy civil service sees its own degraded ritual (piton) — performative coordination persists through inertia. Mid-level bureaucrats see mixed coordination and extraction (tangled_rope) — fragmentation both enables domain expertise and prevents cross-agency effectiveness. Coordination-dependent populations see pure extraction (snare) — duplicated requirements and service gaps with no self-correction mechanism. The civilizational analytical observer risks seeing immutable federalism (mountain) — different domains require different expertise — but the structural data reveals this as a false summit: the technical barriers to integration have become surmountable, and remaining fragmentation reflects institutional choice, not structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) emerges from the agent's structural position relative to fragmentation. Entrenched agencies with arbitrage exit options experience low d (they are beneficiaries) and see rope or coordination functions. Powerless populations with no exit experience high d (they are trapped) and see snare. Mid-level bureaucrats experience moderate d: they benefit from domain expertise but are constrained by jurisdictional boundaries. Organized reformers with mobile exit experience low d (they can build alternative systems) and see scaffold. The legacy civil service system experiences low d as an institutional actor with arbitrage options but high theater ratio (piton). The analytical observer experiences d ≈ 0.72 but risks naturalizing a contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by revealing that fragmentation is genuinely hybrid — it serves real coordination functions (specialization, accountability separation) while simultaneously extracting from powerless populations (service gaps, duplication). The resolution is not 'which type is correct?' but 'the constraint is correctly classified as tangled_rope because both functions are real and neither is subordinate to the other.' The analytical observer's mountain is a false summit: federalism is a legitimate principle, but current fragmentation exceeds what federalism requires. The entrenched agency's rope is their genuine experience within their domain. The coordination-dependent population's snare is their structural reality. The scaffold's sunset is technically real (integration platforms exist) but faces institutional capture risk (omega_reform_coalition_capture_risk).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_necessity_threshold,
    'What degree of jurisdictional separation is structurally necessary for democratic accountability versus unnecessary for service delivery?',
    'Comparative analysis of unitary vs federal systems; measurement of service quality and citizen satisfaction across jurisdictional integration levels; identification of critical separation points (legislature/executive/judiciary vs administrative domains)',
    'If threshold is high: most current fragmentation is necessary (mountain classification upheld). If threshold is low: current fragmentation is excessive and contingent (snare/tangled_rope is accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_necessity_threshold, conceptual, 'Threshold for necessary vs excessive jurisdictional fragmentation').

omega_variable(
    integration_cost_asymmetry,
    'Who bears the costs of integration vs fragmentation? Are costs symmetrically distributed or concentrated on powerless agents?',
    'Measurement of service access gaps by citizen income/location; tracking of bureaucratic load (forms/eligibility re-verification) by population segment; identification of who benefits from specialized agency expertise vs who is harmed by coordination failures',
    'If symmetric: fragmentation is a coordination problem (rope/scaffold). If asymmetric (costs on powerless, benefits on institutional): fragmentation is extraction (snare/tangled_rope confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(integration_cost_asymmetry, empirical, 'Whether fragmentation costs are symmetrically distributed or concentrated').

omega_variable(
    cross_agency_data_barrier_type,
    'Are information barriers between agencies technical/resolvable or institutional/deliberate? Do agencies lack data-sharing capability or choose not to share?',
    'Audit of technical systems and data architectures; examination of legal/policy barriers to information sharing; analysis of agency resistance to integration initiatives; comparison of data-sharing capability across countries with similar federalism structures',
    'If technical: fragmentation is a coordination problem solvable by infrastructure investment (scaffold confirms). If institutional/deliberate: fragmentation is maintenance of extractive jurisdictional control (snare/tangled_rope confirms).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_agency_data_barrier_type, empirical, 'Whether data barriers between agencies are technical or institutional').

omega_variable(
    reform_coalition_capture_risk,
    'As integration initiatives develop, do they capture benefits for organized participants (entrenched agencies, reformers with privileged access) while still excluding powerless populations?',
    'Tracking of who participates in integration task forces; measurement of whether unified platforms actually reduce citizen burden or merely consolidate gatekeeping into new form; monitoring of which populations remain unserved as systems modernize',
    'If capture occurs: reform coalition''s scaffold classification is false; true classification is tangled_rope with different beneficiaries. If genuine progress: scaffold sunset is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_capture_risk, empirical, 'Whether reform initiatives capture benefits while maintaining extraction').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) primarily structural (external barriers to coordination) or internalized (agencies have accepted fragmentation as legitimate institutional order)?',
    'Post-integration measurement: tracking of suppression levels after technical barriers are removed; qualitative analysis of agency resistance to actual data-sharing vs claims of technical constraints; measurement of whether agencies maintain artificial fragmentation even when unified systems are available',
    'If internalized: suppression persists after structural barriers removed; constraint''s effective suppression is higher than measured. If structural: removing technical barriers will reduce suppression proportionally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in agency culture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_fragmentation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scf_tr_t0, state_capacity_fragmentation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(scf_tr_t10, state_capacity_fragmentation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(scf_tr_t20, state_capacity_fragmentation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(scf_be_t0, state_capacity_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(scf_be_t10, state_capacity_fragmentation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(scf_be_t20, state_capacity_fragmentation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_fragmentation, resource_allocation).
narrative_ontology:affects_constraint(state_capacity_fragmentation, bureaucratic_regulatory_capture).
narrative_ontology:affects_constraint(state_capacity_fragmentation, public_service_access_inequality).
narrative_ontology:affects_constraint(state_capacity_fragmentation, interagency_coordination_overhead).

% DUAL FORMULATION NOTE:
% State capacity fragmentation decomposes into coordination-function fragmentation (legitimate specialization; ε≈0.20, rope) and extraction-function fragmentation (gatekeeping; ε≈0.55, snare). This story treats them as a single hybrid constraint (tangled_rope). Alternative formulation would separate into two stories, each with distinct ε values and perspectives. Current unified approach emphasizes that the same institutional structure serves both functions simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_capacity_fragmentation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
