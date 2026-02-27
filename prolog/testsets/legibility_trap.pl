% ============================================================================
% CONSTRAINT STORY: legibility_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legibility_trap, []).

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
 *   constraint_id: legibility_trap
 *   human_readable: The Grid-Map Displacement
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   The grid-map displacement occurs when a state or institution imposes
 *   simplified, standardized metrics onto a complex, adaptive social system
 *   to make it administratively 'legible.' James C. Scott's framework
 *   identifies this as a core mechanism of state formation: the creation of
 *   standardized categories (surnames, land plots, census identities,
 *   occupational classifications) that transform organic, context-dependent
 *   social arrangements into legible, taxable, conscriptable units. This
 *   constraint exhibits the full range of DR classification because
 *   legibility genuinely solves a coordination problem (how does a central
 *   authority allocate resources and maintain order across scale?) while
 *   simultaneously destroying the informational and social infrastructure
 *   that permits distributed resilience. The same mechanism appears as
 *   coordination (Rope) from the state's perspective, extraction (Snare) from
 *   the trapped community's perspective, temporary misalignment (Scaffold)
 *   from the decentralization movement's perspective, and degraded ritual
 *   (Piton) from the post-colonial archive's perspective. The constraint's
 *   extractiveness (0.58) reflects that the state captures real benefits (tax
 *   base, conscription capacity, disease surveillance) while local
 *   communities lose access to informal safety nets, lose authority to
 *   external bureaucratic rule, and lose adaptive capacity as their knowledge
 *   becomes invisible to standardized metrics. The theater_ratio (0.64)
 *   reflects the gap between the claimed coordination function and actual
 *   local governance: much legibility activity is performative (census
 *   campaigns that don't affect local service delivery, identity
 *   documentation that exists for state records, district boundaries that
 *   ignore natural community divisions). The trap closes because neither the
 *   state can easily revert to less legible governance (it has built
 *   infrastructure and careers on standardized metrics), nor can communities
 *   escape (they are now juridically defined by the map, economically
 *   dependent on formal services that replaced informal networks, and legally
 *   prohibited from autonomous decision-making).
 *
 * KEY AGENTS:
 *   - Local Communities: Primary victims (powerless/trapped) — lose informal safety nets, local authority, and adaptive capacity as standardized metrics eliminate granularity they depend on
 *   - Informal Economy Practitioners: Primary victims (powerless/constrained) — rendered invisible by legibility standards; lose market access, credit networks, and regulatory flexibility
 *   - Tacit Knowledge Holders: Primary victims (moderate/constrained) — elders, healers, artisans whose authority derives from context-specific skill; delegitimized by external standardized expertise
 *   - State Bureaucracy: Primary beneficiary (institutional/arbitrage) — captures tax base, surveillance capacity, resource allocation control; can adjust legibility mechanisms
 *   - Centralized Administration: Primary beneficiary (institutional/arbitrage) — solves genuine coordination problem of governing heterogeneous populations; arbitrage exit option
 *   - Local Intermediaries: Secondary victims (moderate/constrained) — benefit from state access and resource availability; simultaneously lose authority and knowledge-base
 *   - Decentralization/Subsidiarity Movement: Organized agents (organized/constrained) — community organizations, participatory budgeting advocates building alternative legibility pathways with local input
 *   - Colonial Administrative Archive: Institutional actor (institutional/arbitrage) — post-colonial persistence of extractive legibility apparatus through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional design choices as inherent properties of state formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legibility_trap, 0.58).
domain_priors:suppression_score(legibility_trap, 0.68).
domain_priors:theater_ratio(legibility_trap, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legibility_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(legibility_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legibility_trap, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legibility_trap, tangled_rope).
narrative_ontology:human_readable(legibility_trap, "The Grid-Map Displacement").
narrative_ontology:topic_domain(legibility_trap, "political/social/economic").

domain_priors:requires_active_enforcement(legibility_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legibility_trap, state_bureaucracy).
narrative_ontology:constraint_beneficiary(legibility_trap, centralized_administration).
narrative_ontology:constraint_victim(legibility_trap, local_communities).
narrative_ontology:constraint_victim(legibility_trap, informal_economies).
narrative_ontology:constraint_victim(legibility_trap, tacit_knowledge_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL COMMUNITY (SNARE) — Embedded in informal networks, tacit knowledge systems, and adaptive local institutions. The grid-map imposes standardized metrics that eliminate the granularity their survival depends on. Cannot exit without abandoning home and community ties. Bears full extraction cost as informal safety nets dissolve, local authority is delegitimized, and adaptive capacity collapses.
constraint_indexing:constraint_classification(legibility_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL INTERMEDIARY (TANGLED ROPE) — Mediates between state and community. Benefits from coordination: legibility enables access to state resources, credit, and institutional recognition. Simultaneously victimized: loses authority and knowledge-base as standardized metrics replace judgment, faces career loss as role is absorbed into bureaucratic hierarchy. Constrained exit — can migrate but community obligations remain.
constraint_indexing:constraint_classification(legibility_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRALIZED STATE BUREAUCRACY (ROPE) — Experiences legibility as solution to coordination problem: standardized metrics enable central resource allocation, taxation, census, health/education delivery. Solves the genuine problem of governing large heterogeneous populations without detailed local knowledge. Benefits from arbitrage: can withdraw legibility mechanisms, can adjust metrics, can navigate complexity. Suppression is means to coordination, not its goal.
constraint_indexing:constraint_classification(legibility_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DECENTRALIZATION/SUBSIDIARITY MOVEMENT (SCAFFOLD) — Organized agents (community-based organizations, participatory budgeting advocates, local government reformers) see the legibility trap as a temporary coordination failure with structural sunset: devolution of authority, participatory metrics design, and recognition of informal institutions create alternative pathways that preserve local knowledge while enabling state-level coordination. Theater is declining as participatory and adaptive governance models mature. This perspective holds that the displacement is neither natural law nor permanent extraction, but a temporary institutional configuration being superseded.
constraint_indexing:constraint_classification(legibility_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COLONIAL/POST-COLONIAL ADMINISTRATIVE ARCHIVE (PITON) — The grid-map apparatus itself (district boundaries, census categories, land registries, identity documentation) is a vestigial inheritance from colonial administration designed for extractive governance. In post-colonial states, the same apparatus persists through institutional inertia despite degraded functionality. The theater is high (statistics rituals, census campaigns, identity verification ceremonies) while the actual coordination function has atrophied — the state continues legibility operations because alternatives haven't fully replaced them, not because the mechanism works. Piton classification derives from theater_ratio ≥ 0.70 and ε ≤ 0.25 for the administrative apparatus itself, though the legibility trap constraint has higher ε.
constraint_indexing:constraint_classification(legibility_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of legibility is inherent to state formation: any state that governs beyond face-to-face scale requires standardized metrics, categories, and documentation. This perspective sees the grid-map as an immutable property of stateness itself — unavoidable extraction that comes with the benefits of large-scale coordination. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the apparent 'inherence' naturalizes what are actually contingent institutional design choices (metric selection, discretion levels, feedback mechanisms) that could be configured differently.
constraint_indexing:constraint_classification(legibility_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legibility_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legibility_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legibility_trap, TR),
    TR >= 0.70.

:- end_tests(legibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. At T=0 (early legibility imposition), extraction is lower because informal systems still provide safety net redundancy and communities retain some adaptive capacity; the state's standardized metrics only partially displace local knowledge. By T=50 (mature legibility), extraction rises as informal networks are liquidated, formal service dependence increases, and local authority is fully externalized. The trajectory shows metric substitution (Goodhart drift): as the state optimizes for standardized categories, the actual locally-adaptive functions that metrics fail to capture are abandoned as 'inefficient.' Suppression (0.68): High. The grid-map enforcement requires suppression of alternative organizing principles. Barriers include: legal prohibition of autonomous local governance (communities cannot operate informal justice systems or resource allocation), bureaucratic de-legitimization of traditional authority (chiefs, elders lose official recognition), market barriers to informal economy (informal practitioners excluded from state credit, formal commerce networks), and information suppression (local knowledge is not collected in official statistics, rendering it administratively invisible). Theater_ratio (0.64): High and increasing. Census campaigns, identity documentation ceremonies, district boundary markers, administrative zoning rituals persist as performative activities with limited impact on actual local service delivery or community welfare. Legibility operations are carried out partly because they genuinely coordinate (taxation, disease tracking) and partly because the bureaucratic apparatus depends on them for its institutional justification and career structure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. The state bureaucracy sees Rope: legibility solves the genuine problem of governing large populations without face-to-face knowledge of local conditions. Taxation, census, health/education delivery, conscription all require standardization across heterogeneous units. For the state, this is coordination with acceptable suppression overhead. The trapped community sees Snare: they have no exit option (cannot opt out of taxation, conscription, or legal jurisdiction), face maximum suppression (their informal institutions are prohibited), and experience maximal extraction (loss of safety nets, loss of autonomy, loss of adaptive capacity). The local intermediary sees Tangled Rope: they benefit from new access to state resources and institutional recognition, but simultaneously lose their traditional authority and knowledge-base. The decentralization movement sees Scaffold: this is a temporary misalignment between legibility needs and implementation design; participatory metric design and devolution create alternative pathways with lower extraction and preserved local capacity. The post-colonial archive sees Piton: the apparatus persists through inertia, its primary function degraded, maintained by bureaucratic and institutional dependence. The analytical observer risks seeing Mountain: legibility seems inherent to any state that governs beyond face-to-face scale. But the structural data contradicts this: the extraction derives from specific institutional design choices (centralized metric selection, prohibition of local variance, formal service concentration, bureaucratic discretion) that could be configured to preserve both legibility and local adaptive capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position: who benefits, who bears costs, and what exit options exist. The state bureaucracy, as primary beneficiary with arbitrage exit options (they can adjust metrics, layer coordination mechanisms, exit enforcement if needed), experiences low d → low χ. The trapped community, as primary victim with no exit options, experiences high d → high χ via the sigmoid. Local intermediaries, as partial victims with some constrained exit options (can migrate, can work in formal sector), experience moderate d. The decentralization movement, as organized agents with constrained but visible exit pathways (participatory design, subsidiarity reform), experiences lower d than the trapped community but higher than the state because they lack full arbitrage. The post-colonial archive's d value is artificially constrained by the piton classification gate (theater_ratio ≥ 0.70): even though it might derive higher d from beneficiary status, the piton constraint requires treating institutional inertia as the dominant dynamic, not active extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that the same structural phenomenon legitimately classifies as both pure coordination (Rope) and pure extraction (Snare) depending on the observer's position. The state genuinely solves a coordination problem: without standardized categories, large-scale resource allocation is impossible. But from the trapped community's perspective, legibility is purely extractive: it destroys informal safety nets, eliminates local autonomy, and creates dependencies they did not consent to. The mandatrophy is resolved by recognizing that these are not competing evaluations of the same claim, but accurate descriptions of different structural realities. The state-level Rope classification is correct for the state's genuine coordination problem. The community-level Snare classification is correct for the trapped population's structural reality. The Tangled Rope from the analytical observer's perspective is also correct: the constraint simultaneously serves coordination (genuine state function) and extraction (genuine community cost) — this is not a contradiction but a description of hybrid constraint. The Scaffold perspective from the decentralization movement is correct: the extraction derives from specific institutional design choices (centralization, prohibition of variance, suppression of alternatives) that can be reformed without eliminating legibility itself. The mandatrophy violation would be claiming that ONE of these perspectives reveals 'the truth' and the others are mistaken. The framework recognizes all are correct within their structural context. The false summit (mountain perspective) is properly identified as naturalization: legibility is not inherent to all state formation, but contingent on specific institutional design choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_destruction_threshold,
    'What granularity of metric standardization is compatible with preservation of adaptive local institutions?',
    'Historical comparison of legibility systems by granularity level (village-level vs regional vs national categories) and correlation with local adaptive capacity, informal network survival, and community resilience',
    'If threshold exists and is identifiable: some legibility is compatible with Rope coordination (moderate ε). If threshold cannot be identified or enforced: all standardization trends toward Snare (high ε). If threshold is context-dependent: classification must be locally indexed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_destruction_threshold, empirical, 'Minimum metric granularity compatible with local adaptive capacity').

omega_variable(
    informal_network_substitutability,
    'Can state-provided formal safety nets and services fully substitute for the functions performed by informal community networks?',
    'Comparative study of communities with legibility-induced informal network collapse vs those retaining dual systems; measurement of resilience metrics (poverty reduction, crisis response speed, disease transmission) across both types',
    'If fully substitutable: displacement is coordination rebalancing (Tangled Rope). If partially substitutable: chronic welfare loss (Snare). If irreplaceable: legibility trap is structural entrapment with no compensation mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informal_network_substitutability, empirical, 'Whether formal services substitute for informal networks').

omega_variable(
    participatory_metric_viability,
    'Can participatory metric design (community input to legibility standards) reduce extraction while maintaining coordination function?',
    'Pilot programs comparing participatory metric design vs centrally-imposed metrics; measurement of community adoption, local adaptation, formal service take-up, and informal network retention',
    'If viable: decentralization movement''s scaffold perspective is structural (real sunset). If not viable: legibility necessarily requires top-down imposition (extraction is coordination cost). If partially viable: some domains permit participation, others require standardization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participatory_metric_viability, empirical, 'Whether participatory metric design enables coordination without extraction').

omega_variable(
    state_capacity_asymmetry,
    'Does legibility extraction derive from coordination difficulty or from the state''s institutional incentive to maximize control surface?',
    'Analysis of metric proliferation over time; comparison of states with similar populations but different legibility densities; examination of whether metric expansion correlates with coordination necessity or with bureaucratic expansion',
    'If coordination-driven: extraction is incidental to genuine need (Rope with costs). If control-driven: extraction is primary motive, coordination is justification (Snare). Affects whether state actor should be seen as solution or primary beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_asymmetry, conceptual, 'Whether legibility expansion serves coordination or institutional expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legibility_trap, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legib_tr_t0, legibility_trap, theater_ratio, 0, 0.38).
narrative_ontology:measurement(legib_tr_t25, legibility_trap, theater_ratio, 25, 0.51).
narrative_ontology:measurement(legib_tr_t50, legibility_trap, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(legib_be_t0, legibility_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(legib_be_t25, legibility_trap, base_extractiveness, 25, 0.42).
narrative_ontology:measurement(legib_be_t50, legibility_trap, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legibility_trap, resource_allocation).
narrative_ontology:affects_constraint(legibility_trap, informal_economy_liquidation).
narrative_ontology:affects_constraint(legibility_trap, bureaucratic_discretion_capture).
narrative_ontology:affects_constraint(legibility_trap, knowledge_visibility_collapse).

% DUAL FORMULATION NOTE:
% The legibility trap decomposes into three structurally distinct constraints: (1) informal_economy_liquidation (ε≈0.65, Snare) — the direct elimination of informal markets by formalization requirements; (2) bureaucratic_discretion_capture (ε≈0.52, Tangled Rope) — the way legibility systems concentrate decision-making power, enabling capture; (3) knowledge_visibility_collapse (ε≈0.38, Tangled Rope) — the epistemic loss when tacit knowledge is rendered invisible by standardized metrics. Each has different failure modes and different victim groups, though all three are downstream of the legibility apparatus. The primary constraint (legibility_trap, ε≈0.58) models the integrated effect; the three decomposed constraints model specific extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legibility_trap, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
