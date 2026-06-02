% ============================================================================
% CONSTRAINT STORY: knowledge_silos_corporate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_silos_corporate, []).

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
 *   constraint_id: knowledge_silos_corporate
 *   human_readable: Knowledge Silos in Corporate Organizations
 *   domain: organizational_dynamics/information_management
 *
 * SUMMARY:
 *   Knowledge silos in corporate organizations create a structural constraint
 *   where departmental boundaries simultaneously serve coordination functions
 *   (clear accountability, focused expertise development) and extraction
 *   functions (information asymmetry, gatekeeping power, blocked career
 *   mobility). The constraint exhibits mixed properties: genuine coordination
 *   mechanisms exist alongside demonstrable extraction. Departmental
 *   leadership benefits from silo structure through resource control and
 *   autonomy; frontline employees bear costs through constrained access to
 *   information and limited cross-functional mobility. The theater ratio
 *   (0.68) reflects the performative overlay of knowledge management
 *   initiatives, matrix organizations, and cross-functional mandates that
 *   ostensibly break silos while leaving core gatekeeping mechanisms intact.
 *   The extractiveness trajectory shows increasing severity (0.38 → 0.54) as
 *   organizations accumulate complexity without corresponding information
 *   democratization. Theater ratio similarly increases as gap widens between
 *   stated collaboration values and functional information barriers.
 *
 * KEY AGENTS:
 *   - Departmental Leadership: Primary beneficiary (institutional/arbitrage) — consolidates power through information control, maintains resource autonomy, captures career advancement opportunities within protected domains
 *   - Frontline Employees: Primary victim (powerless/trapped) — trapped within functional silos; cannot access information needed for effective contribution; face career barriers when attempting cross-functional engagement
 *   - Organization Adaptive Capacity: Secondary victim (powerless/trapped) — abstract collective good that cannot exit; bears full cost of duplicated effort, slow decision cycles, and missed opportunities from siloed expertise
 *   - Cross-Functional Teams: Mixed victim (moderate/constrained) — benefit from coordination mechanisms and project-based knowledge sharing; also face extraction through information gatekeeping and departmental loyalty incentives
 *   - Knowledge Management Initiative: Organized coordinating actor (organized/constrained) — develops platforms and mandates for information sharing; captures resources and attention while performative theater substitutes for functional integration
 *   - Remote-First Restructuring: Organized structural actor (organized/mobile) — creates conditions where silos become cost-prohibitive; represents genuine structural alternative with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (specialization, bounded rationality) as immutable organizational law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_silos_corporate, 0.54).
domain_priors:suppression_score(knowledge_silos_corporate, 0.62).
domain_priors:theater_ratio(knowledge_silos_corporate, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_silos_corporate, extractiveness, 0.54).
narrative_ontology:constraint_metric(knowledge_silos_corporate, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(knowledge_silos_corporate, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_silos_corporate, tangled_rope).
narrative_ontology:human_readable(knowledge_silos_corporate, "Knowledge Silos in Corporate Organizations").
narrative_ontology:topic_domain(knowledge_silos_corporate, "organizational_dynamics/information_management").

domain_priors:requires_active_enforcement(knowledge_silos_corporate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_silos_corporate, departmental_leadership).
narrative_ontology:constraint_beneficiary(knowledge_silos_corporate, specialized_experts).
narrative_ontology:constraint_victim(knowledge_silos_corporate, organization_adaptive_capacity).
narrative_ontology:constraint_victim(knowledge_silos_corporate, frontline_employees).
narrative_ontology:constraint_victim(knowledge_silos_corporate, innovation_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE EMPLOYEE (SNARE) — Trapped within departmental boundaries; cannot access critical information held in other silos without navigating bureaucratic gatekeeping. Career advancement requires staying within functional domain. Maximum experienced extraction: constrained capability despite potential contributions, no realistic exit from the silo structure.
constraint_indexing:constraint_classification(knowledge_silos_corporate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CROSS-FUNCTIONAL TEAM (TANGLED ROPE) — Benefits from coordination mechanisms (shared projects, mandatory meetings) while bearing extraction costs (information gatekeeping, duplicated effort). Constrained by departmental loyalty incentives and career risk of challenging silo boundaries. Mixed extraction-coordination dynamic: some benefits, significant constraints.
constraint_indexing:constraint_classification(knowledge_silos_corporate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEPARTMENTAL LEADERSHIP (ROPE) — Benefits from silo structure through resource control, budget autonomy, and power consolidation. Experiences the constraint as coordination: maintaining departmental boundaries enables clear accountability and performance measurement. Net beneficiary with low friction — can arbitrage between competing organizational demands.
constraint_indexing:constraint_classification(knowledge_silos_corporate, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: KNOWLEDGE MANAGEMENT INITIATIVE (TANGLED ROPE) — Organized effort to break silos through enterprise platforms, wikis, and knowledge-sharing mandates. Genuine coordination function (enabling information flow) embedded in asymmetric extraction: the initiative captures resources and attention while departmental gatekeeping persists. High theater: knowledge platforms often become performative repositories rather than functional information bridges.
constraint_indexing:constraint_classification(knowledge_silos_corporate, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REMOTE-FIRST RESTRUCTURING (SCAFFOLD) — Organizations implementing distributed-first structures create conditions where silos become cost-prohibitive (no co-location to maintain them). This represents a temporary sunset mechanism: as communication infrastructure and async-first practices mature, the extractive silo structure loses force. Low effective extraction because the organized shift has real agency and produces genuine alternatives.
constraint_indexing:constraint_classification(knowledge_silos_corporate, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: MATRIX ORGANIZATION THEATER (PITON) — Matrix reporting structures, dotted-line relationships, and 'cross-functional collaboration' initiatives are largely performative overlays on persistent silos. The theater persists through organizational inertia despite low functional integration. Department heads maintain real power while matrix structures create appearance of coordination without eliminating underlying extraction mechanisms.
constraint_indexing:constraint_classification(knowledge_silos_corporate, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information compartmentalization is inherent to organizational complexity: complete transparency creates coordination costs that exceed silo costs. Specialization and bounded rationality necessarily create knowledge barriers. However, this perspective risks naturalizing contingent institutional arrangements as immutable structural features.
constraint_indexing:constraint_classification(knowledge_silos_corporate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_silos_corporate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_silos_corporate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_silos_corporate, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_silos_corporate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_silos_corporate, TR),
    TR >= 0.70.

:- end_tests(knowledge_silos_corporate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. Departmental leadership captures significant benefits through information asymmetry and resource autonomy, but the extraction is not maximal — some silo structure reflects legitimate coordination needs (specialization, accountability clarity, bounded rationality). The value reflects that a portion of observed barriers represents genuine organizational function, but extractive gatekeeping is substantial and measurable. Suppression (0.62): Moderate-high. Significant barriers to breaking silos include: formal reporting structures that make cross-silo communication difficult, career advancement tied to departmental loyalty, budget allocation that rewards departmental autonomy, and tacit knowledge embedded in specialized teams. These barriers are substantial but not absolute — some information flows cross silos, and determined agents can navigate them at cost. Theater ratio (0.68): High. Knowledge management initiatives, matrix organizations, and collaboration mandates are substantially performative. Enterprise platforms become compliance repositories rather than functional information bridges. Dotted-line relationships and cross-functional teams create appearance of integration without eliminating underlying gatekeeping. Theater has increased over the interval (0.52 → 0.68) as the gap widens between stated collaboration values and functional information barriers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: departmental leadership perceives tight coordination and clear accountability (Rope, low χ), while frontline employees perceive pure extraction and no exit (Snare, high χ). The analytical observer risks perceiving immutable natural law (Mountain) — organizational compartmentalization inherent to scale — but indexed perspectives reveal this as naturalization of contingent institutional design. The scaffold perspective (remote-first restructuring) demonstrates that silo constraints are not inevitable: organizational design choices create conditions where silos become cost-prohibitive, producing genuine alternatives with sunset logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from power consolidation. Departmental leadership (institutional/arbitrage) occupy beneficiary position with low d (0.15-0.20): they control information asymmetry and benefit from hierarchy. Frontline employees (powerless/trapped) occupy victim position with high d (0.90-0.95): they face maximum barriers and no exit. Cross-functional teams (moderate/constrained) occupy intermediate position (d ≈ 0.55): they have some agency to navigate silos but face significant costs. The knowledge management initiative (organized/constrained) occupies complex position: organizers benefit from initiative capture (low d) while organization bears cost (high d). Directionality-derived chi values amplify extraction for powerless agents and dampen it for institutional beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by clearly distinguishing coordination function from extraction mechanism. Departmental silos DO coordinate specialization and provide clear accountability (genuine coordination benefit). However, they ALSO enable gatekeeping, information asymmetry, and blocked career mobility (extraction overlay). The Tangled Rope classification resolves the ambiguity: the constraint provides genuine coordination while enabling asymmetric extraction simultaneously. The theater ratio (0.68) is diagnostic: performative knowledge initiatives, matrix organizations, and collaboration mandates create appearance of solving the coordination problem while leaving extraction mechanisms intact. This is precisely the signature of Tangled Rope — active enforcement of coordination that simultaneously preserves extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    silo_coordination_benefit_threshold,
    'What organizational scale threshold distinguishes legitimate specialization and bounded rationality from extractive gatekeeping?',
    'Comparative analysis of information flow rates and decision quality at different organizational scales; measurement of information access barriers vs operational complexity',
    'If threshold < 500 employees: many legitimate silos misclassified as extraction. If threshold > 5000 employees: extractive silos persist unchallenged under complexity cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silo_coordination_benefit_threshold, empirical, 'Scale threshold distinguishing functional specialization from extractive gatekeeping').

omega_variable(
    knowledge_platform_adoption_effectiveness,
    'Do enterprise knowledge platforms (wikis, intranets, shared repositories) actually reduce information asymmetry or become performative compliance mechanisms?',
    'Information access audit comparing stated policies vs actual usage patterns; measurement of cross-silo information flow before and after platform deployment',
    'If effective: theater ratio should decline over time and extraction should drop. If performative: theater ratio persists and extraction mechanisms simply migrate to new platforms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_platform_adoption_effectiveness, empirical, 'Whether knowledge platforms reduce information asymmetry or become performative').

omega_variable(
    remote_work_silo_elimination_causation,
    'Does distributed-first work structure cause silo reduction or do organizations selecting for remote-first already have lower silo orientation?',
    'Comparison of silo metrics in orgs before and after remote-first transition vs matched controls that remain co-located; longitudinal measurement of information flow accessibility',
    'If causal: scaffold sunset mechanism is real — structural shift produces real alternatives. If selection effect: remote-first attracts less hierarchical organizations but doesn''t eliminate silos in hierarchy-prone contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_silo_elimination_causation, empirical, 'Whether remote work structure causes silo reduction or reflects organizational selection').

omega_variable(
    departmental_accountability_necessity,
    'How much information compartmentalization is genuinely necessary for departmental accountability vs how much serves power consolidation?',
    'Comparative analysis of performance measurement in high-transparency vs high-silo organizations; measurement of accountability quality and extractive overhead',
    'If high necessity: silos represent legitimate coordination cost (higher floor for Boltzmann coupling). If low necessity: silos are primarily power mechanisms (lower floor justified).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(departmental_accountability_necessity, conceptual, 'Necessity of information compartmentalization for accountability vs power consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_silos_corporate, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ksilo_tr_t0, knowledge_silos_corporate, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ksilo_tr_t5, knowledge_silos_corporate, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ksilo_tr_t10, knowledge_silos_corporate, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ksilo_tr_t15, knowledge_silos_corporate, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ksilo_be_t0, knowledge_silos_corporate, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ksilo_be_t5, knowledge_silos_corporate, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ksilo_be_t10, knowledge_silos_corporate, base_extractiveness, 10, 0.54).
narrative_ontology:measurement(ksilo_be_t15, knowledge_silos_corporate, base_extractiveness, 15, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_silos_corporate, resource_allocation).
narrative_ontology:affects_constraint(knowledge_silos_corporate, innovation_velocity_corporate).
narrative_ontology:affects_constraint(knowledge_silos_corporate, employee_career_mobility).

% DUAL FORMULATION NOTE:
% Knowledge silos are downstream of organizational structure choices but represent distinct structural constraint. Upstream constraints (departmental autonomy, performance measurement by function) create conditions favoring silos. The silo constraint itself determines information accessibility patterns that affect downstream constraints (innovation velocity, career mobility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_silos_corporate, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
