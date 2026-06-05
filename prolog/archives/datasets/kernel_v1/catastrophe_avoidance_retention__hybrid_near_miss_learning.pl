% ============================================================================
% CONSTRAINT STORY: catastrophe_avoidance_retention__hybrid_near_miss_learning
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_avoidance_retention__hybrid_near_miss_learning, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_avoidance_retention__hybrid_near_miss_learning
 *   human_readable: Catastrophe Avoidance Through Hybrid Near-Miss Learning
 *   domain: safety_engineering/organizational_learning/high_reliability_systems
 *
 * SUMMARY:
 *   High-reliability organizations face a structural tension: simulation and
 *   high-realism drills are necessary for competence maintenance, but neither
 *   is sufficient alone. Aviation has solved this through mandatory incident
 *   reporting systems (ASRS) that distribute learning from near-misses and
 *   foreign incidents across the industry; nuclear power uses INL databases;
 *   medicine has fragmented, voluntary incident reporting with structural
 *   barriers to cross-organization learning. This constraint is ONE READING
 *   of the contested kernel 'catastrophe avoidance retention' — specifically,
 *   the reading that distributed learning from near-misses and foreign
 *   incidents, supplementing but not replacing simulation, maintains
 *   competence reliably without requiring actual catastrophic failures. The
 *   alternative readings are: (1) simulation as proxy catastrophe (pure
 *   simulation can replace incident learning through sufficiently high
 *   fidelity), and (2) catastrophe as necessary selector (some proportion of
 *   actual failures is required for selection pressure, and incident networks
 *   are insufficient). This constraint story instantiates reading #1 and
 *   exhibits tangled rope structure: it coordinates learning across
 *   organizational boundaries (genuine coordination function) while
 *   asymmetrically extracting transparency burden and loss-of-face risk from
 *   participating organizations (asymmetric extraction requiring active
 *   enforcement).
 *
 * KEY AGENTS:
 *   - Safety culture maintainers (institutional/arbitrage): Primary beneficiaries — access to incident data that would be impossible to replicate through simulation alone. Can leverage network position for safety authority and regulatory influence.
 *   - Cross-organizational learning networks (institutional/mobile): Secondary beneficiary — aviation ASRS model, nuclear INL database, medical incident reporting boards all benefit from aggregation and standardization functions.
 *   - Incident learning commons (powerless/trapped): Primary victim — abstract collective knowledge good that cannot organize or exit. Owns value but cannot capture it; network authorities and safety authorities extract rents from aggregation.
 *   - Organizations without network access (moderate/constrained): Secondary victim — isolated organizations in unnetworked industries bear burden of reinventing safety knowledge locally or facing catastrophic failure risk. Constrained by geographic, linguistic, or regulatory isolation from dominant networks.
 *   - Regulatory bodies (organized/mobile): Organizers — push for mandatory transparency as temporary scaffold. See themselves as solving a collective action problem, not extracting value.
 *   - Simulation-centric institutions (institutional/arbitrage): Institutional actor maintaining performative function through inertia — training centers, certification bodies built on simulation paradigm persist despite evidence of insufficiency. Piton perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.52).
domain_priors:suppression_score(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.48).
domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope).
narrative_ontology:human_readable(catastrophe_avoidance_retention__hybrid_near_miss_learning, "Catastrophe Avoidance Through Hybrid Near-Miss Learning").
narrative_ontology:topic_domain(catastrophe_avoidance_retention__hybrid_near_miss_learning, "safety_engineering/organizational_learning/high_reliability_systems").

domain_priors:requires_active_enforcement(catastrophe_avoidance_retention__hybrid_near_miss_learning).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_avoidance_retention__hybrid_near_miss_learning, '6f5ce999-2782-4788-8364-f71dbba711dd').
narrative_ontology:cs_kernel_codification('6f5ce999-2782-4788-8364-f71dbba711dd', distributed).
narrative_ontology:cs_authority_grounding('6f5ce999-2782-4788-8364-f71dbba711dd', practice).
narrative_ontology:cs_interpretation_layer_present('6f5ce999-2782-4788-8364-f71dbba711dd').
narrative_ontology:cs_reading_relation('6f5ce999-2782-4788-8364-f71dbba711dd', catastrophe_avoidance_retention__simulation_as_proxy_catastrophe, coexists_with).
narrative_ontology:cs_reading_relation('6f5ce999-2782-4788-8364-f71dbba711dd', catastrophe_avoidance_retention__catastrophe_as_necessary_selector, coexists_with).
narrative_ontology:cs_axiom('6f5ce999-2782-4788-8364-f71dbba711dd', foundational, near_miss_data_qualitatively_sufficient).
narrative_ontology:cs_axiom_status(near_miss_data_qualitatively_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('6f5ce999-2782-4788-8364-f71dbba711dd', near_miss_data_qualitatively_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('6f5ce999-2782-4788-8364-f71dbba711dd', foundational, distributed_learning_networks_substitute_catastrophe).
narrative_ontology:cs_axiom_status(distributed_learning_networks_substitute_catastrophe, holdable).
narrative_ontology:cs_axiom_grounding('6f5ce999-2782-4788-8364-f71dbba711dd', distributed_learning_networks_substitute_catastrophe, empirically_contingent).
narrative_ontology:cs_reference_frame('6f5ce999-2782-4788-8364-f71dbba711dd', hybrid_learning_sufficiency).
narrative_ontology:cs_drift_state('6f5ce999-2782-4788-8364-f71dbba711dd', contemporary_regulatory_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6f5ce999-2782-4788-8364-f71dbba711dd', '').
narrative_ontology:cs_kernel_id(catastrophe_avoidance_retention__hybrid_near_miss_learning, catastrophe_avoidance_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, safety_culture_maintainers).
narrative_ontology:constraint_beneficiary(catastrophe_avoidance_retention__hybrid_near_miss_learning, cross_organizational_learning_networks).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, incident_learning_commons).
narrative_ontology:constraint_victim(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizations_without_network_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNNETWORKED ORGANIZATION (SNARE) — Isolated from incident-sharing networks, cannot access distributed learning from near-misses or foreign incidents. Trapped by inability to generate replicable competence via simulation alone. Bears full extraction cost: forced to reinvent safety knowledge locally or suffer catastrophic failure. Maximum extraction, minimal exit options.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NETWORKED PARTICIPANT (TANGLED ROPE) — Constrained by compliance reporting burden and loss-of-face risks from transparent incident disclosure. Also benefits from access to foreign incidents and near-miss data that would be impossible to generate through simulation alone. Mixed experience: genuine coordination function (knowledge sharing) + asymmetric extraction (burden of transparency falls unequally on high-safety-culture organizations).
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NETWORK AUTHORITY (ROPE) — Benefits from first-mover position in establishing standardized incident taxonomy and data governance. Experiences the constraint primarily as coordination: aggregating and distributing near-miss data solves the collective action problem of competence retention. Net beneficiary with arbitrage exit (can leverage monopoly on incident data or exit by licensing the platform).
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COORDINATION (SCAFFOLD) — Mandatory incident reporting with enforced transparency creates temporary scaffolding for learning networks. Low extraction from the regulatory perspective because the mechanism has a built-in sunset: as safety culture matures and voluntary reporting norms strengthen, the regulatory mandate becomes redundant. Organized agents (regulators, safety bodies) see this as a transitional coordination mechanism.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: SIMULATION-CENTRIC INSTITUTIONAL ACTORS (PITON) — Legacy institutional actors (training centers, certification bodies) built around simulation as THE primary learning mechanism now maintain that system through inertia despite clear evidence of its insufficiency. Theater ratio high because simulation drills persist as performative institutional ritual — certifying 'competence' through scenario completion — while competence actually derives from incident data. The simulation system persists because institutional infrastructure and career paths depend on it, not because it works alone.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED LIMITS (MOUNTAIN) — From a civilizational scope, distributed learning from near-misses and foreign incidents might appear to be a natural law of high-reliability systems: 'organizations with access to richer incident data have lower catastrophe rates; this is inherent to how complex systems learn.' However, this perspective naturalizes what are contingent institutional and network structures — the availability of incident data, the norms governing transparency, the architecture of knowledge-sharing networks. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_avoidance_retention__hybrid_near_miss_learning, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_avoidance_retention__hybrid_near_miss_learning, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_avoidance_retention__hybrid_near_miss_learning, TR),
    TR >= 0.70.

:- end_tests(catastrophe_avoidance_retention__hybrid_near_miss_learning_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts asymmetrically from organizations required to maintain transparent incident reporting systems. The extraction is real but not maximal: benefits also flow to participants (access to foreign incident data, reduced reinvention cost). Over time (t=0 to t=10), extractiveness increases as the burden of transparency reporting intensifies while willingness to participate voluntarily declines, necessitating more enforcement. Suppression (0.48): Moderate. Significant barriers exist to incident reporting: organizational liability concerns, reputational risk, regulatory gaming incentives (reporting undercuts licensing or insurance claims). But suppression is not total — transparent organizations do participate, and aviation's strong reporting culture shows suppression can be overcome. Suppression decreases over interval as reporting norms mature and legal protections (ASRS immunity, medical error reporting confidentiality) strengthen. Theater ratio (0.35): Low-moderate, declining. Simulation drills were initially performative (t=0: 0.55 theater ratio) — certifying 'competence' through scenario completion without incident data. As incident networks mature and real learning from near-misses replaces simulation as the primary mechanism, theater ratio declines (t=10: 0.35). The constraint increasingly drives real functional learning rather than performative certification.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The isolated organization (powerless/trapped) experiences pure snare: forced to generate competence without access to the learning that makes competence achievable. The networked organization (moderate/constrained) experiences tangled rope: genuine coordination benefit (access to foreign incidents) + asymmetric extraction burden (transparency requirements). The network authority (institutional/arbitrage) experiences rope: solving a coordination problem with beneficiary exit options. The regulatory body (organized/mobile) experiences scaffold: temporary enforcement mechanism with a sunset as voluntary reporting norms mature. The simulation institution (institutional/arbitrage) experiences piton: maintaining a degraded performative system through inertia. The civilizational analytical observer risks experiencing mountain: 'high-reliability systems inherently require distributed learning networks' could be naturalized as law, when actually it describes contingent institutional structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d for each perspective derives from the agent's structural position: power level, exit options, and beneficiary/victim relationship. Safety culture maintainers (institutional/arbitrage) have low d — they benefit from the constraint with easy exit, so effective extraction chi is low or negative. Organizations without network access (moderate/trapped) have high d — they bear costs without benefits, trapped by isolation. The incident learning commons has maximum d — it is a victim with no voice and no exit. The network authority (institutional/arbitrage) has low d — benefits from aggregation monopoly with exit via licensing. Suppression scaling (unscaled in this constraint, as per specification) interacts with d via chi = ε × f(d) × σ(S): a beneficiary experiencing low d feels low chi; a powerless victim at maximum d feels high chi; scope σ(S) at global amplifies extraction asymmetry (global=1.2). The piton perspective (simulation institutions) derives from high theater, not from directionality — institutional actors maintaining performative function experience low chi because the theater itself is the constraint, and their power level (institutional) dampens experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the choice between 'simulation alone' vs. 'incident learning alone' is a false dichotomy. Competence retention requires a hybrid: simulation for generating rare-event scenarios that won't occur naturally, incident data for testing real-world assumptions embedded in simulation models and for adapting to novel failure modes. The tangled rope classification captures this: (1) genuine coordination function — shared incident language and taxonomy enables organizations to understand each other's failures, (2) asymmetric extraction — transparency burden falls unequally on high-safety-culture organizations, (3) active enforcement required — regulatory mandate necessary because voluntary reporting is suppressed by liability and reputational risk. The mandatrophy is resolved not by choosing one type but by showing that all six readings are valid perspectival framings: the snare reading describes the unnetworked organization, the rope reading describes the network coordinator, the piton reading describes the degraded simulation system, the scaffold reading describes the regulatory transition, the mountain reading (false summit) describes the naturalization risk, and the tangled rope reading (this one) describes the working learning system itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_sufficiency_threshold,
    'At what threshold of simulation realism and frequency does simulation alone become sufficient for competence retention without incident learning?',
    'Longitudinal comparison of safety outcomes: organizations using only high-fidelity simulation vs. those with incident data access, controlling for incident severity and frequency exposure. Correlation analysis of simulation complexity vs. catastrophe prevention.',
    'If threshold is achievable with foreseeable technology: simulation perspective is viable alternative reading, not false choice. If threshold is unreachable: simulation alone is fundamentally insufficient, and incident learning is necessary structural element.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_sufficiency_threshold, empirical, 'Sufficiency threshold for simulation-only competence retention').

omega_variable(
    catastrophe_as_epistemic_selector,
    'Can competence be reliably maintained without actual catastrophic failures as learning inputs, or do near-miss + foreign incident proxies adequately substitute for the selective pressure of real failure?',
    'Analysis of organizations that have never experienced catastrophe but maintain high competence via incident networks (aviation, nuclear) vs. those that learn primarily from internal near-misses (medicine, construction). Test whether ''close call'' learning has same retention trajectory as incident-rich learning.',
    'If catastrophes are necessary selectors: the constraint forces a grim choice (allow some failures for system to learn) and reading shifts toward tragic acceptance. If near-misses + foreign incidents suffice: the reading''s hybrid logic holds and alternatives are truly available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophe_as_epistemic_selector, empirical, 'Whether actual catastrophes are necessary for competence selection').

omega_variable(
    network_architecture_dependency,
    'Does competence retention depend fundamentally on the specific network architecture (aviation''s ASRS model, nuclear''s INL database, medicine''s fragmented voluntary reporting) or is the mechanism portable across different governance structures?',
    'Comparative institutional analysis: identify which network features (anonymity, cross-sector access, regulator authority, voluntary vs. mandatory, real-time vs. aggregated reporting) correlate with competence retention. Test whether changing architecture degrades learning outcomes.',
    'If architecture-dependent: learning networks are contingent institutional arrangements and can be engineered differently. If universal: the network form itself is nearly optimal and captures structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_architecture_dependency, empirical, 'Portability of learning network architecture across governance forms').

omega_variable(
    reading_kernel_ambiguity,
    'Is ''catastrophe avoidance retention'' a natural requirement (catastrophes will happen, the question is only whether we learn from them) or a constructed institutional problem (catastrophes could be engineered away entirely with sufficient redundancy and would be, if we didn''t benefit from the learning they provide)?',
    'This is a conceptual omega routable to the kernel reading structure: the sibling reading ''catastrophe_as_necessary_selector'' assumes catastrophes are inevitable inputs; the reading you are instantiating (hybrid_near_miss_learning) assumes they can be replaced by proxies. The difference locates in whether catastrophes are facts of nature or structural features of how we design safety systems.',
    'Affects framing: if natural, the learning network reading is about optimizing response to inevitable failure. If constructed, the reading is about accepting a particular risk tolerance and institutional form that treats some failures as acceptable learning inputs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether catastrophes are natural selectors or constructed institutional features').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_avoidance_retention__hybrid_near_miss_learning, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carn_tr_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 0, 0.55).
narrative_ontology:measurement(carn_tr_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 5, 0.42).
narrative_ontology:measurement(carn_tr_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(carn_be_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(carn_be_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(carn_be_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(carn_su_t0, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(carn_su_t5, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(carn_su_t10, catastrophe_avoidance_retention__hybrid_near_miss_learning, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_avoidance_retention__hybrid_near_miss_learning, information_standard).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, simulation_sufficiency_learning_plateau).
narrative_ontology:affects_constraint(catastrophe_avoidance_retention__hybrid_near_miss_learning, organizational_transparency_liability_extraction).

% DUAL FORMULATION NOTE:
% This constraint is part of a kernel family with two sibling readings (simulation_as_proxy_catastrophe and catastrophe_as_necessary_selector). Each reading instantiates a different ε value: simulation-proxy reading would have lower ε (if simulation is sufficient, less extraction occurs), catastrophe-necessary reading would have higher ε (if catastrophes are needed, the system extracts more harshly from organizations that experience them). The shared kernel is the question: 'how does competence get retained in high-reliability systems?' Each reading answers with a different mechanism, producing different constraint structures. Downstream constraints (simulation_sufficiency_learning_plateau, organizational_transparency_liability_extraction) are affected by whichever reading is operant in a given industry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
