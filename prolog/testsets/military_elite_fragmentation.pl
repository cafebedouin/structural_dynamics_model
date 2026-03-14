% ============================================================================
% CONSTRAINT STORY: military_elite_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_military_elite_fragmentation, []).

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
 *   constraint_id: military_elite_fragmentation
 *   human_readable: Military Elite Fragmentation and Institutional Coordination Failure
 *   domain: civil_military_relations/institutional_governance
 *
 * SUMMARY:
 *   Military elite fragmentation describes the structural inability of
 *   service branches (Army, Navy, Air Force, Space Force, Marines) to execute
 *   unified strategic command despite formal joint command structures. The
 *   constraint emerges from institutional autonomy: each service branch
 *   maintains independent acquisition authority, doctrine development, and
 *   personnel advancement pathways, creating incompatible incentives for
 *   mid-level and junior officers. The fragmentation exhibits all
 *   characteristics of a Tangled Rope: genuine coordination function (service
 *   specialization enables doctrinal depth and technological expertise),
 *   asymmetric extraction (coordination benefits flow to service branch
 *   leadership and defense contractors; operational costs fall on junior
 *   officers and enlisted personnel), and active enforcement (officers are
 *   formally required to maintain service-branch loyalty AND follow unified
 *   commands simultaneously). The constraint's extractiveness has increased
 *   over 40 years (0.42 to 0.58) as doctrinal specialization has deepened and
 *   technological complexity has amplified the cost of incompatible
 *   procedures. Theater ratio has also risen (0.35 to 0.55), indicating
 *   increasing performativity: formal joint command structures and
 *   interservice task forces create appearance of integration while real
 *   authority remains distributed. This is the classic Goodhart drift — the
 *   theater substitutes for function.
 *
 * KEY AGENTS:
 *   - Junior Officers: Primary victim (powerless/trapped) — caught between service-branch loyalty incentives and unified command directives; career advancement depends on satisfying mutually incompatible metrics
 *   - Enlisted Personnel: Primary victim (powerless/trapped) — execute contradictory tactical doctrines; bear physical and psychological cost of fragmentation
 *   - Midlevel Officer Network: Secondary victim/beneficiary (organized/constrained) — experience mixed costs and advancement opportunities; some officers benefit from being institutional bottlenecks
 *   - Service Branch Leadership: Primary beneficiary (institutional/arbitrage) — preserve autonomy, budgetary independence, and doctrine control; experience fragmentation as coordination mechanism
 *   - Defense Industrial Complex: Primary beneficiary (institutional/arbitrage) — fragmentation drives platform-specific procurement cycles; unified command would reduce total acquisition spending
 *   - Joint Chiefs of Staff System: Theater institution (institutional/arbitrage) — formal coordination mechanism that has become performative; maintains legitimacy without functional integration
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies hybrid coordination-extraction structure and risks mandatrophy misclassification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(military_elite_fragmentation, 0.58).
domain_priors:suppression_score(military_elite_fragmentation, 0.65).
domain_priors:theater_ratio(military_elite_fragmentation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(military_elite_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(military_elite_fragmentation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(military_elite_fragmentation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(military_elite_fragmentation, tangled_rope).
narrative_ontology:human_readable(military_elite_fragmentation, "Military Elite Fragmentation and Institutional Coordination Failure").
narrative_ontology:topic_domain(military_elite_fragmentation, "civil_military_relations/institutional_governance").

domain_priors:requires_active_enforcement(military_elite_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(military_elite_fragmentation, service_branch_leadership).
narrative_ontology:constraint_beneficiary(military_elite_fragmentation, defense_industrial_contractors).
narrative_ontology:constraint_victim(military_elite_fragmentation, operational_coherence).
narrative_ontology:constraint_victim(military_elite_fragmentation, junior_officer_corps).
narrative_ontology:constraint_victim(military_elite_fragmentation, enlisted_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JUNIOR OFFICER (SNARE) — Trapped between competing directives from Army vs Air Force vs Space Force leadership; career advancement depends on satisfying mutually incompatible performance metrics. No exit without abandoning military career entirely. Maximum extraction: must choose loyalty to service branch (risking national operational coherence) or national mission (risking career advancement). The fragmentation extracts obedience to branch-specific incentives at the cost of unified strategic execution.
constraint_indexing:constraint_classification(military_elite_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENLISTED PERSONNEL (SNARE) — Trapped in incompatible command hierarchies; tactical doctrine conflicts create physical danger and psychological whiplash. Cannot exit except through discharge with administrative consequences. Suppression is structural and psychological — fragmentation is naturalized as 'interservice coordination challenges' rather than understood as institutional extraction. Bears full cost of coordination failure.
constraint_indexing:constraint_classification(military_elite_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDLEVEL OFFICER NETWORK (TANGLED ROPE) — Organized agents with some agency (commanders can build unofficial coordination mechanisms, shared doctrine communities). Constrained by formal command structure but can create genuine coordination pathways (joint task forces, shared intelligence databases, informal networks). Experience mixed benefit and cost: fragmentation threatens mission success (they bear responsibility for tactical failures), but also creates advancement opportunities for officers who can 'bridge divides.' Asymmetric extraction: some officers benefit from being bottlenecks; others pay the cost of incompatibility.
constraint_indexing:constraint_classification(military_elite_fragmentation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SERVICE BRANCH LEADERSHIP (ROPE) — Institutional actors with arbitrage options. They experience fragmentation as a coordination mechanism that preserves branch autonomy, acquisition authority, and budgetary independence. Fragmentation enables Service-specific strategic narratives (Army: ground supremacy; Air Force: air dominance; Navy: sea control; Space Force: orbital superiority). Leadership perceives genuine coordination function: fragmented commands can develop specialized doctrines and maintain institutional legitimacy. Net beneficiary — extraction flows toward this level.
constraint_indexing:constraint_classification(military_elite_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEFENSE INDUSTRIAL CONTRACTORS (ROPE) — Institutional actors with arbitrage options. Fragmentation enables platform-specific acquisition cycles: each service branch drives independent procurement, creating multiple revenue streams. A unified command structure would rationalize procurement and reduce total spending. Contractors experience fragmentation as pure coordination: competing service demands create larger total acquisition budgets. Exit option is arbitrage — contractors can shift production emphasis across services as budget priorities shift. Net beneficiary.
constraint_indexing:constraint_classification(military_elite_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: JOINT CHIEFS OF STAFF SYSTEM (PITON) — Institution created to overcome fragmentation (1986 Goldwater-Nichols Act) has become performative. Formal joint command structures exist; real authority remains in service branches. Theater ratio is high: joint planning meetings, unified command briefs, interservice liaison offices create appearance of coordination while service branches retain actual authority. The institution persists through inertia and legitimacy maintenance rather than functional integration.
constraint_indexing:constraint_classification(military_elite_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, military elite fragmentation exhibits genuine coordination function (service specialization, doctrinal depth) AND asymmetric extraction (operational incoherence, personnel costs, strategic drift). The constraint is neither pure coordination nor pure extraction but a hybrid system where coordination benefits accrue to institutional leadership while extraction costs fall on operational personnel. Mandatrophy is genuine — the system could be mistaken for pure coordination if only service leadership perspectives are consulted.
constraint_indexing:constraint_classification(military_elite_fragmentation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(military_elite_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(military_elite_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(military_elite_fragmentation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(military_elite_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(military_elite_fragmentation, TR),
    TR >= 0.70.

:- end_tests(military_elite_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The constraint extracts operational coherence and personnel welfare by imposing incompatible performance metrics on mid-level and junior officers. Junior officers advance by maximizing service-branch-specific metrics (Army readiness, Air Force pilot hours, Navy deployments), which often conflict with unified command priorities. The extraction is not violent coercion but institutional: career penalties for disobedience to service-branch leadership. Suppression (0.65): Moderate-high and structural. Barriers to coordination include statutory service-branch autonomy (Title 10 USC), separate acquisition authority, incompatible technical standards, and deeply embedded doctrinal silos. Exit is nominally possible (officers can resign) but carries career destruction costs — security clearance suspension, transition trauma, lost retirement benefits. This is structured suppression. Theater ratio (0.55): Moderate and increasing. Formal joint command structures (CENTCOM, NORTHCOM, etc.) create appearance of integration. Joint staff briefs, interservice task forces, and unified command meetings occur regularly. But real authority remains in service branches — acquisition decisions, promotion authorities, and doctrinal development stay service-specific. The joint structure is increasingly theatrical as it fails to achieve functional integration despite growing formality. Claimed type (Tangled Rope): Confirmed by presence of both genuine coordination (service specialization enables expertise, doctrinal depth, technological focus) and asymmetric extraction (benefits accrue to service leadership and contractors; costs imposed on operational personnel). Requires active enforcement: officers are formally required to maintain both service loyalty and unified command obedience — the institutional structure actively enforces this contradiction.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between the institutional beneficiaries (service leadership, contractors) and the operational victims (junior officers, enlisted personnel). Both perceive the same structural constraint, but one group experiences coordination benefit and exit freedom; the other experiences extraction cost and entrapment. The Joint Chiefs theater disguises this gap — formal joint structures create false impression of alignment, which suppresses awareness of the perspectival divergence. The midlevel officer network occupies the middle ground, experiencing both benefits and costs, which is why they are the only perspective that produces Tangled Rope rather than pure types. The analytical observer's perspective is the only one that sees the structure clearly: hybrid coordination-extraction with asymmetric distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the fragmentation constraint. Service branch leadership (institutional/arbitrage) derives low d — they are net beneficiaries with exit options (can reallocate budgets, shift doctrinal emphasis, arbitrage between different military domains). This produces negative or near-zero χ for them. Junior officers (powerless/trapped) derive high d — they have no exit and bear full cost of incompatibility. This produces high χ approaching the snare boundary. Midlevel officers (organized/constrained) derive intermediate d — they have some agency (can build informal coordination networks) and mixed benefits (some advance through fragmentation-bridging, others pay costs). This produces moderate χ appropriate to Tangled Rope. Defense contractors (institutional/arbitrage) derive beneficiary d analogous to service leadership — they are net positive on fragmentation and have exit options through portfolio diversification. The formal command structure actively enforces the directionality asymmetry: officers are promoted for service-specific excellence but blamed collectively for operational failures.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY IS ACTIVE. The constraint risks misclassification as pure Rope (coordination mechanism) if only service leadership perspectives are consulted. Service branch autonomy is indeed a genuine coordination function — it enables specialization, expertise development, and doctrinal innovation that would be impossible in a unified structure. But this masks the asymmetric extraction imposed on junior officers and enlisted personnel. The Tangled Rope classification prevents this mislabeling by requiring explicit presence of both beneficiaries AND victims. Service branch leadership and contractors are beneficiaries (derive positive directional value). Junior officers and personnel are victims (derive negative directional value). The constraint cannot be classified as pure coordination because the extraction is structural and irreducible: it is not a side effect but a necessary component of the system. Conversely, it cannot be classified as pure extraction (Snare) because the coordination function is also genuine and irreducible. The mandatrophy is resolved by accepting that the constraint is legitimately both — the hybrid nature is the structural reality, not a classification ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specialization_vs_integration_threshold,
    'What level of service-branch specialization is necessary for doctrinal development, and at what point does specialization become extraction through institutional autonomy?',
    'Comparative analysis of military effectiveness in unified vs fragmented command structures; correlation between doctrinal specialization depth and operational outcome variability',
    'If specialization benefits outweigh integration costs: Rope from more perspectives. If integration costs dominate: Snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specialization_vs_integration_threshold, empirical, 'Threshold between necessary specialization and extractive fragmentation').

omega_variable(
    junior_officer_exit_constraint_mechanism,
    'Is the junior officer trapped by structural military law (2-year commitment bonds, security clearance penalties for discharge) or by internalized identity lock (self-concept as military professional making exit unthinkable)?',
    'Post-discharge trajectory analysis; comparison of career outcomes and identity coherence for officers who exit vs transfer to unified commands; identity interview analysis',
    'If structural: exit_options should remain ''trapped'' with mountain-like immutability. If internalized: exit_options could be reclassified as ''identity_locked'', revealing that exit is possible if identity frame breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(junior_officer_exit_constraint_mechanism, empirical, 'Whether junior officer entrapment is structural or identity-based').

omega_variable(
    unified_command_precedent_feasibility,
    'Do historical unified command episodes (Gulf War 1991, Desert Shield coordination) demonstrate that military elite fragmentation is surmountable, or were they temporary suspensions that reverted when external pressure subsided?',
    'Historical analysis of unified command effectiveness; post-unified-operation reversion timing and institutional memory retention',
    'If surmountable: Scaffold perspective gains credibility — fragmentation is institutional inertia with a possible sunset. If reversion inevitable: fragmentation is closer to Mountain (structural feature of military organization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unified_command_precedent_feasibility, empirical, 'Whether unified command is sustainable or temporary').

omega_variable(
    technological_integration_forcing,
    'Do emerging technologies (autonomous systems, AI-enabled command, space-based sensors) force integration of fragmented services, or do they enable further specialization through decoupled domain expertise?',
    'Technical feasibility analysis of multi-domain command architectures; comparative costs of unified vs fragmented technology stacks',
    'If forcing integration: Scaffold sunset mechanism is real (technology will overcome fragmentation). If enabling specialization: fragmentation calcifies into new technological silos.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_integration_forcing, empirical, 'Whether technological change forces integration or enables specialization').

omega_variable(
    personnel_cost_attribution,
    'How much of military personnel cost (healthcare, retirement, training) is legitimately attributable to operational fragmentation-induced inefficiency vs other factors?',
    'Accounting analysis isolating fragmentation-specific costs from baseline personnel costs; comparative analysis with unified military structures',
    'If fragmentation-attributable cost is high (>15% of total): extraction magnitude is greater than measured. If low (<5%): extraction metric should be downward-adjusted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personnel_cost_attribution, empirical, 'Attribution of personnel costs to fragmentation-induced inefficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(military_elite_fragmentation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(milef_tr_t0, military_elite_fragmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(milef_tr_t20, military_elite_fragmentation, theater_ratio, 20, 0.45).
narrative_ontology:measurement(milef_tr_t40, military_elite_fragmentation, theater_ratio, 40, 0.55).
narrative_ontology:measurement(milef_tr_t10, military_elite_fragmentation, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(milef_be_t0, military_elite_fragmentation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(milef_be_t20, military_elite_fragmentation, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(milef_be_t40, military_elite_fragmentation, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(milef_be_t10, military_elite_fragmentation, base_extractiveness, 10, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(military_elite_fragmentation, enforcement_mechanism).
narrative_ontology:affects_constraint(military_elite_fragmentation, civil_military_relations_subordination).
narrative_ontology:affects_constraint(military_elite_fragmentation, defense_procurement_fragmentation).
narrative_ontology:affects_constraint(military_elite_fragmentation, interservice_doctrine_incompatibility).

% DUAL FORMULATION NOTE:
% Military elite fragmentation is upstream of three downstream constraints: (1) civil-military relations subordination (civilian control authority is fragmented across service-specific defense secretaries), (2) defense procurement fragmentation (each service maintains independent acquisition cycles), (3) interservice doctrine incompatibility (no unified strategic doctrine, only service-specific doctrines). Each downstream constraint inherits fragmentation as a structural feature. The network indicates that resolving military elite fragmentation would propagate changes across all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(military_elite_fragmentation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
