% ============================================================================
% CONSTRAINT STORY: parkinsons_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_parkinsons_law, []).

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
 *   constraint_id: parkinsons_law
 *   human_readable: Parkinson's Law: Work Expansion and Bureaucratic Self-Perpetuation
 *   domain: organizational/economic
 *
 * SUMMARY:
 *   Parkinson's Law describes the self-perpetuating tendency of bureaucratic
 *   organizations to expand work and personnel beyond the minimum required to
 *   accomplish productive output. The constraint operates through structural
 *   incentives: managers are rewarded for headcount and budget control;
 *   workers generate work to justify their positions; administrative layers
 *   multiply to coordinate an expanding hierarchy. The law exhibits all six
 *   classification types from different structural positions. For productive
 *   workers, it appears as a snare: they cannot exit and bear the cost of
 *   artificial work proliferation. For middle management, it is a tangled
 *   rope: they benefit from career advancement while also imposing (and
 *   experiencing) the constraint's burdens. For administrative personnel, it
 *   is coordination: the work expansion justifies their employment and
 *   enables organizational structure. For organizational reformers, it is a
 *   temporary problem with a sunset: reorganizations and restructuring create
 *   windows where the constraint can be compressed. For legacy systems, it is
 *   a degraded ritual: efficiency audits and restructuring projects perform
 *   reform without achieving lasting change. For the analytical observer, it
 *   risks appearing as an immutable law of bureaucracy — but the requirement
 *   for active enforcement, the variance in suppression across organizational
 *   cultures, and the high theater ratio reveal it as a contingent
 *   institutional arrangement, not a law of nature.
 *
 * KEY AGENTS:
 *   - Productive Workers: Primary victims (powerless/trapped) — cannot exit institutional hierarchy; experience mandatory work expansion regardless of output value
 *   - Middle Management: Primary beneficiary (organized/constrained) — gains career advancement and authority through work generation; also enforces the constraint through delegation and reporting requirements
 *   - Administrative Personnel: Secondary beneficiary (institutional/arbitrage) — gains employment security and advancement opportunities; can relocate between organizations if conditions deteriorate
 *   - Organizational Reformers: Temporary actors (powerful/mobile) — initiates restructuring projects with sunset clauses; creates window for constraint compression before cultural inertia reasserts
 *   - Legacy Bureaucratic System: Institutional actor (institutional/arbitrage) — maintains work-expansion dynamics through performative efficiency measures; resistant to lasting change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional dynamics as immutable law of hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(parkinsons_law, 0.52).
domain_priors:suppression_score(parkinsons_law, 0.48).
domain_priors:theater_ratio(parkinsons_law, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(parkinsons_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(parkinsons_law, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(parkinsons_law, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(parkinsons_law, tangled_rope).
narrative_ontology:human_readable(parkinsons_law, "Parkinson's Law: Work Expansion and Bureaucratic Self-Perpetuation").
narrative_ontology:topic_domain(parkinsons_law, "organizational/economic").

domain_priors:requires_active_enforcement(parkinsons_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(parkinsons_law, administrative_personnel).
narrative_ontology:constraint_beneficiary(parkinsons_law, middle_management).
narrative_ontology:constraint_victim(parkinsons_law, organizational_efficiency).
narrative_ontology:constraint_victim(parkinsons_law, cost_control).
narrative_ontology:constraint_victim(parkinsons_law, productive_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRODUCTIVE WORKER (SNARE) — Individual workers cannot exit the constraint without abandoning their career. They experience mandatory work expansion: justified tasks multiply, reporting requirements accumulate, and the 'system' consumes their time regardless of actual output value. Trapped within institutional hierarchy with minimal exit options, bearing the full cost of artificial work proliferation.
constraint_indexing:constraint_classification(parkinsons_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE MANAGEMENT (TANGLED ROPE) — Experiences both genuine coordination benefits (delegating tasks, structuring workflows, supervising personnel) and asymmetric extraction (justified bloat of reporting, meeting proliferation, and team expansion that benefits their career progression). Their exit options are constrained — they depend on institutional advancement. The constraint is enforced through career incentives and resource allocation.
constraint_indexing:constraint_classification(parkinsons_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ADMINISTRATIVE PERSONNEL (ROPE) — Primary beneficiary with high exit options (can move between organizations, sectors, or roles). Experiences the constraint as coordination: work generation justifies headcount, budget allocation, and career stability. Gains employment security and advancement opportunities through the constraint's operation. Arbitrage exit means they can relocate if institutional context becomes unfavorable.
constraint_indexing:constraint_classification(parkinsons_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZATIONAL REFORMER (SCAFFOLD) — Sees Parkinson's Law as a temporary coordination failure solvable through restructuring, lean methodologies, and metric discipline. Experiences the constraint as high suppression (institutional resistance to layoffs) but with a sunset clause: reorganization projects, right-sizing initiatives, and efficiency mandates create temporary enforcement mechanisms that compress the work expansion. As reforms mature, the constraint relaxes — but cultural inertia often undermines lasting change.
constraint_indexing:constraint_classification(parkinsons_law, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BUREAUCRATIC SYSTEM (PITON) — The constraint persists through institutional inertia and theatrical compliance with efficiency metrics. Regular reorganizations, efficiency audits, and cost-control theaters are performed but rarely achieve lasting change. The constraint's primary function (coordination through hierarchical structure) has atrophied in modern flat organizations, yet the work-expansion dynamics persist. Theater ratio high because much organizational activity is performative: meetings about efficiency, audits that generate more paperwork, restructuring that preserves headcount.
constraint_indexing:constraint_classification(parkinsons_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Parkinson's Law might appear to describe an inherent feature of hierarchical organization: the tendency for information asymmetry between layers to generate work, or the inevitable entropy of bureaucratic systems. Work expansion could be seen as a law of institutional physics — axiomatically true for all bureaucracies. However, the structural data contradicts this: the constraint requires active enforcement (personnel policies, budget cycles, incentive structures), has measurable suppression (institutional resistance varies), and exhibits theater (efficiency rituals). The mountain framing naturalizes what is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(parkinsons_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(parkinsons_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(parkinsons_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(parkinsons_law, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(parkinsons_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(parkinsons_law, TR),
    TR >= 0.70.

:- end_tests(parkinsons_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts significant value from productive workers through artificial work generation, but it is not maximum because some administrative work provides genuine coordination value. The baseline extraction reflects that many organizations retain functional coordination benefits while experiencing inefficiency. Suppression (0.48): Moderate. Institutional resistance to eliminating positions and restructuring is substantial, but not absolute — organizations do attempt reforms, and some workers can negotiate lighter workloads. Exit is difficult but not impossible for specialized workers or those willing to leave organizational employment. Theater ratio (0.65): High. The constraint operates substantially through performative activity: meetings about efficiency, metrics tracking output-to-input ratios, reorganization projects that reshuffle rather than reduce, and justification narratives explaining why headcount must grow. The high theater reflects that much organizational activity around work expansion is rhetorical rather than functional. Over the interval (0-20 years), extractiveness increases as organizations mature: early organizations may have legitimate coordination needs; mature organizations develop parasitic layers without functional value. Theater increases as organizations develop increasingly sophisticated justification narratives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates high perspectival divergence across organizational hierarchy. The productive worker sees pure extraction (Snare): work expansion is imposed, unavoidable, and bears no benefit. Middle management sees coordination with asymmetric benefits (Tangled Rope): they must coordinate and expand work, but also benefit from the system. Administrative personnel see coordination (Rope): work expansion justifies their positions and enables career advancement. Organizational reformers see a solvable temporary problem (Scaffold): restructuring can compress the constraint through temporary enforcement of new metrics. Legacy systems see a degraded ritual (Piton): efficiency measures are performed but rarely achieve lasting change. The civilizational observer risks seeing immutable law (Mountain), but the structural data reveals this as false — the requirement for active enforcement, measurable suppression variance, and high theater indicate contingency, not necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from agents' structural positions. Productive workers (powerless/trapped) experience d ≈ 0.95 — they are full targets of extraction. Middle management with constrained exit derive d ≈ 0.50-0.60 — they are both beneficiaries (through advancement) and partial victims (through the constraint they help enforce). Administrative personnel (institutional/arbitrage) derive d ≈ 0.10-0.20 — they are beneficiaries with exit options, so they experience negative effective extraction (the constraint subsidizes their position). Organizational reformers (powerful/mobile) experience d ≈ 0.45 in their reform window but see the constraint relax as their projects succeed, reducing d over time. The analytical observer (analytical/analytical) derives d ≈ 0.72 — a neutral observer position that sees the full structure without benefiting or bearing costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing genuine coordination (middle management layer enabling larger organizations to function) from parasitic extraction (unjustified expansion beyond coordination needs). The constraint is a Tangled Rope precisely because it contains both: the classification gates require active enforcement (true), beneficiaries (administrative personnel and management, true), and victims (productive workers and organizational efficiency, true). The theater ratio distinguishes the constraint from a pure Snare: some of the work expansion is rhetorically justified even when functionally unnecessary, indicating that performative activity is a significant mechanism. The scaffold perspective acknowledges that some organizational contexts (reform windows, efficiency initiatives) can temporarily compress the constraint, suggesting it is not axiomatically permanent. The piton perspective captures the degradation of the coordination function over time: as organizations mature, the administrative overhead persists through inertia rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_coordination_vs_padding,
    'What percentage of work expansion serves genuine coordination functions versus pure bureaucratic padding?',
    'Task-level analysis tracking output-to-input ratios; comparative study of flat vs hierarchical organizations performing identical work; measurement of task completion rates across organizational layers',
    'If coordination dominates (>60%): constraint is primarily Rope, work expansion is justified. If padding dominates: constraint is primarily Snare, work expansion is extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_coordination_vs_padding, empirical, 'Ratio of genuine coordination to bureaucratic padding in work expansion').

omega_variable(
    incentive_structure_determinism,
    'Does Parkinson''s Law operate mechanistically from incentive structures, or do human choices and organizational culture provide genuine agency to resist it?',
    'Comparative case studies of organizations with identical budget rules but different outcomes; measurement of work expansion rates in post-restructuring periods; ethnographic analysis of management decision-making about headcount',
    'If mechanical: law-like constraint across all contexts. If culturally contingent: constraint is structurally variable and responsive to leadership choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_structure_determinism, conceptual, 'Whether Parkinson''s Law is mechanistic or culturally contingent').

omega_variable(
    digital_org_sustainability,
    'Do remote-first and distributed digital organizations exhibit different Parkinson''s Law dynamics than traditional hierarchical organizations?',
    'Longitudinal comparison of work-expansion rates in distributed vs co-located teams; measurement of meeting proliferation and administrative overhead in digital-native organizations; analysis of span-of-control distributions',
    'If digital organizations escape the constraint: Parkinson''s Law is contingent on physical-hierarchy communication patterns, not axiomatically true. If constraint persists: suggests deeper structural driver than coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_org_sustainability, empirical, 'Whether distributed digital organizations exhibit Parkinson''s Law dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(parkinsons_law, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(park_tr_t0, parkinsons_law, theater_ratio, 0, 0.45).
narrative_ontology:measurement(park_tr_t10, parkinsons_law, theater_ratio, 10, 0.58).
narrative_ontology:measurement(park_tr_t20, parkinsons_law, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(park_be_t0, parkinsons_law, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(park_be_t10, parkinsons_law, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(park_be_t20, parkinsons_law, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(parkinsons_law, resource_allocation).
narrative_ontology:affects_constraint(parkinsons_law, bureaucratic_rent_seeking).
narrative_ontology:affects_constraint(parkinsons_law, organizational_bloat_equilibrium).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(parkinsons_law, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
