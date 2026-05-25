% ============================================================================
% CONSTRAINT STORY: knowledge_silos_in_large_systems
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_silos_in_large_systems, []).

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
 *   constraint_id: knowledge_silos_in_large_systems
 *   human_readable: Knowledge Silos in Large Systems
 *   domain: organizational_structure/information_flow
 *
 * SUMMARY:
 *   Knowledge silos in large systems represent a structural constraint where
 *   the organization of specialized expertise creates boundaries that prevent
 *   cross-functional information flow. This constraint exhibits a core
 *   tension: genuine specialization requires some degree of knowledge
 *   compartmentalization and expertise preservation, but organizations often
 *   maintain silo depth and impermeability far beyond functional necessity,
 *   extracting value from the resulting information asymmetries. Department
 *   leadership maintains gatekeeping authority over specialized knowledge;
 *   mid-level coordinators capture value from bridging local silos;
 *   peripheral workers lack access to system-wide context despite needing it
 *   for effective contribution. The constraint operates through formal
 *   governance structures (access controls, approval hierarchies,
 *   classification schemes) that have acquired substantial theater over time
 *   — elaborate documentation and clearance procedures that perform
 *   coordination while actual knowledge flow operates through informal
 *   channels. Knowledge management initiatives represent an organized
 *   response with sunset logic: systematic codification and shared platforms
 *   are gradually building bridges that reduce silo extraction. At the
 *   civilizational scale, the analytical observer risks naturalizing silo
 *   fragmentation as an immutable consequence of complexity, missing the
 *   extractive institutional choices that maintain silos beyond functional
 *   necessity.
 *
 * KEY AGENTS:
 *   - Department Leadership: Primary beneficiary (institutional/arbitrage) — maintains specialized authority and information advantage; can choose to share knowledge or restrict it
 *   - Peripheral Knowledge Workers: Primary victim (powerless/trapped) — lack access to cross-functional context; cannot exit without leaving organization
 *   - Mid-Level Coordinators: Secondary actor (moderate/constrained) — benefit from information brokerage role while constrained by inability to access upper-level information; capture asymmetric value
 *   - Knowledge Management Initiative: Organized agents (organized/constrained) — treating silos as temporary coordination problem with clear sunset path through codification and systematic knowledge sharing
 *   - Formal Information Governance Structure: Institutional apparatus (institutional/arbitrage) — maintains elaborate access controls and approval procedures; now substantially performative (Piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing complexity-driven fragmentation as immutable law rather than contingent organizational choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_silos_in_large_systems, 0.52).
domain_priors:suppression_score(knowledge_silos_in_large_systems, 0.58).
domain_priors:theater_ratio(knowledge_silos_in_large_systems, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_silos_in_large_systems, extractiveness, 0.52).
narrative_ontology:constraint_metric(knowledge_silos_in_large_systems, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(knowledge_silos_in_large_systems, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_silos_in_large_systems, tangled_rope).
narrative_ontology:human_readable(knowledge_silos_in_large_systems, "Knowledge Silos in Large Systems").
narrative_ontology:topic_domain(knowledge_silos_in_large_systems, "organizational_structure/information_flow").

domain_priors:requires_active_enforcement(knowledge_silos_in_large_systems).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_silos_in_large_systems, departmental_leadership).
narrative_ontology:constraint_beneficiary(knowledge_silos_in_large_systems, specialized_unit_gatekeepers).
narrative_ontology:constraint_victim(knowledge_silos_in_large_systems, system_wide_coordination_capacity).
narrative_ontology:constraint_victim(knowledge_silos_in_large_systems, peripheral_knowledge_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL KNOWLEDGE WORKER (SNARE) — Lacks access to cross-functional information flows and organization-wide context. Cannot exit without leaving the organization entirely. Extraction of labor productivity through information asymmetry: worker invests effort without visibility into broader patterns they could optimize. Maximum suppression through compartmentalization and need-to-know restrictions.
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-LEVEL COORDINATOR (TANGLED ROPE) — Benefits from information brokerage role (status, leverage in local decisions) while constrained by inability to access or share information above departmental level. Real coordination function (bridging local silos) exists alongside asymmetric extraction (coordinator captures value of information asymmetry). Career risk and mobility constraints prevent exit.
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEPARTMENT LEADERSHIP (ROPE) — Primary beneficiary. Maintains specialized knowledge base and institutional authority through silo maintenance. Experiences silo as coordination function (preserves expertise, prevents dilution of specialized knowledge). Can arbitrage between internal specialization and external market value. Low suppression experienced — boundaries are maintained through preferred institutional structures, not force.
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: KNOWLEDGE MANAGEMENT INITIATIVE (SCAFFOLD) — Organized agents (knowledge managers, digital infrastructure teams) treating silos as a temporary coordination problem with sunset logic. KM systems, wiki platforms, and cross-functional working groups are building bridges. Constraints persist due to institutional inertia, but organized agents see clear exit path through systematic knowledge codification. Theater ratio high initially (elaborate documentation theater); declines as genuinely useful tools replace performative knowledge sharing.
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL INFORMATION GOVERNANCE STRUCTURE (PITON) — The institutional apparatus of information classification, clearance levels, and access controls. Originally functioned as a genuine coordination mechanism (managing sensitive data, organizing complex workflows). Now substantially theater: elaborate access-request procedures that perform governance while actual information flow operates through informal channels, back channels, and workarounds. Theater ratio 0.68 reflects significant performative documentation and approval rituals maintaining the appearance of structured information management.
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COMPLEXITY IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational scale, information silos appear as an immutable consequence of complexity: large systems necessarily subdivide into specialized domains, and perfect knowledge integration across all domains is computationally and cognitively infeasible. Knowledge loss across boundaries is inherent to scaling, not a contingent organizational choice. This perspective risks naturalizing what structural analysis reveals as extractive institutional choices. The engine's false summit detector will identify this as misclassification: genuine complexity constraints (specialization is necessary) are being conflated with organizational extraction mechanisms (silos are maintained *beyond* functional necessity).
constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_silos_in_large_systems_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_silos_in_large_systems, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_silos_in_large_systems, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_silos_in_large_systems, TR),
    TR >= 0.70.

:- end_tests(knowledge_silos_in_large_systems_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The organization captures value from information asymmetries maintained through silo structure. Peripheral workers invest effort without full system context; their productivity is constrained by lack of cross-functional awareness. Department leadership captures premium from specialized knowledge control. The extraction is not maximal (0.72+) because some silos reflect genuine specialization needs and some informal knowledge flow occurs. Over the interval, extractiveness increased from 0.38 to 0.52 as organizational growth made silo depth more severe. Suppression (0.58): Moderate-high. Significant barriers to cross-silo information flow include formal access restrictions, clearance procedures, cultural norms around knowledge hoarding, absence of platforms for cross-functional sharing, and career incentives aligned with departmental loyalty rather than organization-wide coordination. Suppression is not total because informal networks and back channels provide partial workarounds. Theater ratio (0.68): High and increasing. Formal information governance (classification schemes, access-request procedures, approval hierarchies) is substantially performative — elaborate documentation that creates the appearance of controlled information flow while actual knowledge transfer happens informally. Theater increased from 0.55 to 0.68 as governance apparatus expanded without proportional improvement in information accessibility.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence on what constitutes the problem. Department leadership sees Rope — they are coordinating specialized expertise and protecting knowledge commons from dilution. The organized KM initiative sees Scaffold — systematic knowledge codification and sharing platforms have clear sunset path and are already reducing silo depth. The formal governance apparatus sees Piton — it recognizes its own procedures as degraded theater while defending their necessity. Mid-level coordinators see Tangled Rope — they benefit from brokerage role but are constrained by information asymmetries they cannot transcend. Peripheral workers see Snare — they bear the cost of information poverty without exit or recourse. The analytical observer risks seeing Mountain — information fragmentation as inherent to complexity — but the structural data reveals this as false: the constraint persists and deepens through institutional choices (career incentive structures, deliberate gatekeeping, absence of investment in knowledge platforms) that could be otherwise.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position relative to information flow and silo boundaries. Department leadership (institutional/arbitrage) has high organizational flexibility and can arbitrage between internal specialization and external market value — low d (beneficiary position). Peripheral workers (powerless/trapped) have no exit options and lack structural alternatives — high d (victim position). Mid-level coordinators (moderate/constrained) occupy intermediate position: they benefit from brokerage role (moderate d) but face real constraints on upward information access and mobility. The knowledge management initiative (organized/constrained) has agency through systematic codification work — constrained exit (moderate d) because they can see and work toward sunset. Formal governance structure (institutional/arbitrage) maintains appearance of control through procedural theater — low d, beneficiary of the inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that genuine specialization benefits (Rope from leadership perspective) coexist with extractive silo depth maintenance (Snare from peripheral perspective). The constraint is legitimately Tangled Rope — it coordinates specialized expertise while asymmetrically extracting from those without silo-crossing authority. The theater ratio (0.68) indicates that the formal governance apparatus is substantially performative, but the performance serves the extraction by creating procedural barriers (access requests, clearances, approval chains) that feel legitimate but are not functionally necessary. The scaffold perspective is crucial: the constraint is not immutable because organized agents are systematically building alternatives (knowledge platforms, cross-functional working groups, codification initiatives) that reduce silo depth and extraction. The false summit in the mountain perspective lies in conflating necessary specialization (genuine coordination benefit) with extractive silo maintenance (institutional choice).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_specialization_threshold,
    'Where is the boundary between functionally necessary specialization and extractive silo maintenance?',
    'Comparative analysis of organizations with different silo permeability; measurement of coordination failure costs vs. specialization-depth benefits; cross-sectional data on information-sharing intensity and system performance',
    'If threshold is high (current silo structure is mostly necessary): classification shifts toward Rope and Mountain, reducing snare detection. If threshold is low (most silo depth is extractive): Snare and Tangled Rope percentages increase, revealing hidden extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_specialization_threshold, empirical, 'Boundary between necessary specialization and extractive silo depth').

omega_variable(
    informal_network_substitution,
    'Do informal information networks (hallway conversations, back channels, hidden groups) actually provide sufficient cross-silo knowledge flow to convert this to a lower-extraction constraint?',
    'Network analysis of informal information flows; comparison of coordination effectiveness via formal vs. informal channels; measurement of which decisions rely on informal knowledge brokerage',
    'If informal networks are effective: systemic extraction is lower than base_properties suggest (revise extractiveness downward). If ineffective: informal flows are tactical workarounds masking systemic failure, and extraction estimate is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_network_substitution, empirical, 'Whether informal networks substitute for formal silo-crossing mechanisms').

omega_variable(
    knowledge_loss_versus_redundancy_cost,
    'Is the measured extraction cost primarily knowledge loss (information decay across boundaries) or redundancy cost (duplicated effort when silos prevent reuse)?',
    'Audit of duplicate work across departments; measurement of knowledge loss metrics (citations of missed internal precedents, redundant problem-solving); comparison with organizations using different silo structures',
    'If primarily redundancy: constraint is coordination problem (Rope from organized perspective). If primarily knowledge loss: constraint is extraction mechanism (Snare from peripheral perspective) — organization is consuming knowledge worker capacity without delivering proportional system benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_loss_versus_redundancy_cost, empirical, 'Whether constraint cost is knowledge loss or redundant effort').

omega_variable(
    silo_maintenance_intentionality,
    'Are silos maintained through explicit institutional policy or through emergent path-dependence and cultural inertia?',
    'Historical analysis of institutional decisions: were silos formally created and are they formally defended, or did they emerge and persist through default? Interview data on leadership intent regarding silo boundaries.',
    'If intentional policy: Tangled Rope classification confirmed — requires active enforcement, asymmetric beneficiaries. If emergent inertia: classification may degrade toward Piton (theater-driven persistence without conscious extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silo_maintenance_intentionality, empirical, 'Whether silo maintenance reflects policy choice or institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_silos_in_large_systems, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ksilo_tr_t0, knowledge_silos_in_large_systems, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ksilo_tr_t3, knowledge_silos_in_large_systems, theater_ratio, 3, 0.62).
narrative_ontology:measurement(ksilo_tr_t6, knowledge_silos_in_large_systems, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(ksilo_be_t0, knowledge_silos_in_large_systems, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ksilo_be_t3, knowledge_silos_in_large_systems, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(ksilo_be_t6, knowledge_silos_in_large_systems, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_silos_in_large_systems, resource_allocation).
narrative_ontology:affects_constraint(knowledge_silos_in_large_systems, organizational_communication_bottleneck).
narrative_ontology:affects_constraint(knowledge_silos_in_large_systems, specialized_expertise_lock_in).
narrative_ontology:affects_constraint(knowledge_silos_in_large_systems, cross_functional_project_failure).

% DUAL FORMULATION NOTE:
% Knowledge silos form a constraint family with three related but distinct claims. The silo structure itself (this story) has extractiveness 0.52 — it coordinates specialization while maintaining information asymmetries. The communication bottleneck downstream (higher extractiveness) reflects how silo structure blocks inter-departmental coordination. The expertise lock-in constraint (separate story) has higher extractiveness because it examines how silo structure traps decision-making authority within specialized domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_silos_in_large_systems, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
