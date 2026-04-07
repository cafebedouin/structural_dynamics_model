% ============================================================================
% CONSTRAINT STORY: manager_role_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manager_role_necessity, []).

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
 *   constraint_id: manager_role_necessity
 *   human_readable: Manager Role Necessity in Organizational Hierarchies
 *   domain: organizational_structure/labor_relations
 *
 * SUMMARY:
 *   The manager role in contemporary organizational hierarchies exemplifies a
 *   constraint that simultaneously coordinates work and extracts labor value.
 *   From the manager's perspective, the role solves genuine coordination
 *   problems: task delegation, resource allocation, conflict resolution, and
 *   performance monitoring enable organizations to function beyond dyadic
 *   scale. From the individual worker's perspective, managerial oversight
 *   creates systematic extraction: asymmetric decision authority, time
 *   monitoring, behavioral control, and differential reward distribution. The
 *   constraint exhibits the full DR spectrum depending on observational
 *   position. Its theater_ratio (0.58) indicates that approximately 58% of
 *   managerial activity is performative (status maintenance, hierarchical
 *   legitimacy justification, behavioral surveillance rituals) rather than
 *   substantive coordination. The rising trajectory of both extractiveness
 *   and theater over the 40-year interval reflects decades of intensifying
 *   management infrastructure, surveillance capability, and justification
 *   ideology. Simultaneously, the emergence of documented self-managing
 *   alternatives (worker cooperatives, open-source communities, holacratic
 *   experiments) demonstrates that the constraint's necessity claim is
 *   empirically falsifiable, making it neither a mountain (immutable) nor a
 *   pure rope (unambiguous coordination). The canonical classification is
 *   tangled rope: genuine coordination function coupled with asymmetric
 *   extraction.
 *
 * KEY AGENTS:
 *   - Individual Worker: Primary victim (powerless/trapped) — bears behavioral monitoring, authority asymmetry, and differential reward extraction with no exit option
 *   - Organized Labor / Worker Collectives: Secondary victim (moderate/constrained) — collective bargaining provides partial agency but high coordination cost; benefits from work rule standardization
 *   - Manager: Primary beneficiary (institutional/arbitrage) — extracts authority asymmetry, status compensation, and decision-making power; experiences role as coordination function
 *   - Self-Management Movement: Organized agents (organized/constrained) — demonstrate alternative coordination mechanisms; build sunset pathway through proven viability at scale
 *   - Corporate Bureaucracy: Institutional actor (institutional/arbitrage) — maintains managerial infrastructure through inertia and legitimacy narrative; substantial theater ratio
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent organizational law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manager_role_necessity, 0.58).
domain_priors:suppression_score(manager_role_necessity, 0.65).
domain_priors:theater_ratio(manager_role_necessity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manager_role_necessity, extractiveness, 0.58).
narrative_ontology:constraint_metric(manager_role_necessity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(manager_role_necessity, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manager_role_necessity, tangled_rope).
narrative_ontology:human_readable(manager_role_necessity, "Manager Role Necessity in Organizational Hierarchies").
narrative_ontology:topic_domain(manager_role_necessity, "organizational_structure/labor_relations").

domain_priors:requires_active_enforcement(manager_role_necessity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manager_role_necessity, managers).
narrative_ontology:constraint_beneficiary(manager_role_necessity, organizational_consolidation_interests).
narrative_ontology:constraint_victim(manager_role_necessity, individual_workers).
narrative_ontology:constraint_victim(manager_role_necessity, collective_worker_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL WORKER (SNARE) — Structurally trapped by economic dependency. Cannot exit managerial oversight without losing livelihood. Bears full cost of managerial extraction: time monitoring, behavioral surveillance, decision authority asymmetry, and differential reward distribution. No meaningful alternatives in contemporary labor markets. Maximum experienced extraction.
constraint_indexing:constraint_classification(manager_role_necessity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ORGANIZED WORKER / LABOR COLLECTIVE (TANGLED ROPE) — Constrained by coordination costs and union negotiation complexity, but possesses collective agency through unionization and work rules. Management structure does coordinate task allocation and resource distribution (genuine coordination function), but simultaneously extracts labor value asymmetrically. Mixed experience: some extraction, some benefit through coordination.
constraint_indexing:constraint_classification(manager_role_necessity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MANAGER (ROPE) — Experiences managerial role as solving coordination problems: task delegation, performance monitoring, conflict resolution, resource allocation. Has exit options (arbitrage potential in labor market), benefits from authority asymmetry and compensation premiums. Experiences constraint primarily as coordination function. Low extracted cost from the manager's position.
constraint_indexing:constraint_classification(manager_role_necessity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SELF-MANAGEMENT MOVEMENT (SCAFFOLD) — Organized agents (worker cooperatives, flat hierarchies, open-source projects) demonstrate that managerial extraction is contingent, not necessary. Alternative coordination mechanisms (peer review, consensus decision-making, reputation systems, collective ownership) function without extraction. Sunset clause inherent: as alternative models scale and prove viable, traditional managerial hierarchy loses structural necessity.
constraint_indexing:constraint_classification(manager_role_necessity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE BUREAUCRACY (PITON) — Managerial hierarchies persist through institutional inertia despite evidence that coordination can function through alternative mechanisms. The managerial layer engages substantial theater: performance reviews, org charts, status rituals, and justification narratives maintain the appearance of necessity. Theater ratio (0.58) reflects that significant managerial time is devoted to performative function (maintaining legitimacy) rather than substantive coordination.
constraint_indexing:constraint_classification(manager_role_necessity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some coordination overhead is inherent to any organization above a certain scale: planning, conflict resolution, and resource allocation create irreducible complexity. This perspective naturalizes managerial hierarchy as an immutable law of organizational physics. However, the structural data contradicts the mountain classification — self-managing organizations of substantial scale (Wikipedia, Linux kernel, cooperatives with hundreds of workers) demonstrate that coordination is possible without traditional managerial extraction. The engine's false summit detector identifies this as naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(manager_role_necessity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manager_role_necessity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(manager_role_necessity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(manager_role_necessity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(manager_role_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(manager_role_necessity, TR),
    TR >= 0.70.

:- end_tests(manager_role_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric extraction of decision authority, time control, and behavioral surveillance coupled with genuine coordination function. The value is not as high as pure extraction (0.70+) because managerial structures do solve real coordination problems for above-dyadic organizations. The rising trajectory from 0.32 to 0.58 reflects increasing management infrastructure, surveillance technology, and complexity in extractive mechanisms over four decades. Suppression (0.65): Moderate-high. Workers face significant barriers to exit: economic dependency on wages, limited alternative employment structures in labor markets, geographic immobility, and skill specialization. Informal norms and identity investment in organizational belonging create internalized suppression. However, suppression is not total — labor mobility exists, unionization provides partial exit, and alternative employment models (freelance, cooperative) offer some escape routes. Theater ratio (0.58): Rising from 0.28 to 0.58 reflects the growth of performative managerial functions (performance reviews, org chart legitimacy rituals, leadership development, status ceremonies) relative to substantive coordination. The theater increase outpaces functional necessity, indicating degradation toward piton characteristics as the organizational field accumulates managerial overhead.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The manager sees coordination (rope) — their daily work solves task allocation problems. The individual worker sees extraction (snare) — their daily experience is monitoring and authority asymmetry. The organized labor collective sees mixed dynamics (tangled rope) — coordination benefits from standardization coupled with extraction from power asymmetry. The self-management movement sees contingency with sunset (scaffold) — proven alternatives demonstrate that extraction is not necessary. The corporate bureaucracy sees its own degradation (piton) — managerial infrastructure persists despite acknowledged excess through status rituals and justification narratives. The civilizational analytical observer risks seeing natural law (mountain) — asserting that all large organizations need managers — but the empirical contradiction is direct and measurable. The gap between beneficiary and victim experience is the widest among all six DR types in this constraint family.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position in the extraction flow. Individual workers with trapped exit options and victim status: d ≈ 0.92 → f(d) ≈ 1.38 → high experienced extractiveness. Managers with arbitrage options and beneficiary status: d ≈ 0.08 → f(d) ≈ -0.11 → negative/low experienced extractiveness (benefits from the constraint). Organized labor with constrained exit and mixed beneficiary/victim status: d ≈ 0.55 → f(d) ≈ 0.75 → moderate experienced extractiveness. The scope modifier σ(S) = 1.0 at national scale applies uniform scaling. The core perspectival asymmetry is that the agent bearing the cost (individual worker) has the lowest power atom (powerless) and most severe exit restriction (trapped), while the agent receiving benefit (manager) has institutional power and arbitrage exit. This structural arrangement ensures that chi = ε × f(d) × σ(S) systematically concentrates extraction on the most vulnerable agent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint escapes mandatrophy collapse through distinct empirical refutability. The mountain classification (implicit in 'managers are necessary') is contradicted by observable self-managing organizations at substantial scale (Wikipedia: 300k+ articles with no managerial hierarchy; Linux kernel: millions of lines with peer-review coordination; Mondragon cooperatives: 80k+ workers without executive hierarchy). The rope classification (implicit in 'this is just coordination') is contradicted by measured suppression (0.65), asymmetric authority, and rising theater ratio. The tangled rope classification holds because: (1) genuine coordination functions exist and are measurable (task allocation, resource conflict resolution, integration across specialties), (2) asymmetric extraction is documented and measurable (decision authority asymmetry, surveillance costs, differential reward distribution), and (3) active enforcement mechanisms persist (organizational hierarchy, authority structures, behavioral monitoring). The constraint satisfies the three gates for tangled rope: beneficiaries (managers, organizational consolidation), victims (workers, worker autonomy), and active enforcement (hierarchical authority structures). The empirical path to resolving mandatrophy is clear: measure coordination value in self-managing systems at equivalent scale and complexity. If coordination cost is similar with lower extraction cost, the mountain and rope alternatives are falsified. If coordination cost is substantially higher in self-managing systems, the tangled rope holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_threshold_ambiguity,
    'Is there a true organizational scale threshold above which managerial hierarchy becomes structurally necessary?',
    'Comparative analysis of large-scale self-managing organizations (worker cooperatives scaling to 500+ workers, Wikipedia with 300k+ articles, Linux kernel maintainers) against predictions of hierarchy necessity theory. Measurement of coordination cost and decision latency across different governance structures at comparable scales.',
    'If threshold exists and is exceeded: mountain classification (some hierarchical extraction is inherent). If threshold does not exist: mountain classification is false summit; managerial necessity is institutional artifact, not natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_threshold_ambiguity, empirical, 'Whether organizational scale creates inherent need for managerial hierarchy').

omega_variable(
    coordination_cost_attribution,
    'What portion of managerial overhead is genuine coordination cost vs. what portion is extractive authority maintenance?',
    'Time-motion studies of actual managerial work; comparison of substantive coordination time (task allocation, resource conflict resolution) vs. performative time (status meetings, hierarchical justification, behavioral monitoring). Contrast with coordination time in self-managing organizations.',
    'If coordination cost >> extraction cost: constraint approaches rope (pure coordination) rather than tangled rope. If extraction cost ≈ coordination cost: tangled rope classification confirmed, possibly shifted toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_cost_attribution, empirical, 'Ratio of genuine coordination work to extractive authority maintenance in managerial roles').

omega_variable(
    identity_lock_persistence,
    'Do workers'' identity investments in hierarchical structures (''I am a team member serving the organization,'' ''I need my manager''s validation'') persist after removing managerial oversight, or do they dissolve when the structure is removed?',
    'Longitudinal study of worker identity discourse before/after transition to self-management; analysis of whether workers trained in command-and-control structures adopt lateral decision-making naturally or resist it. Exit interviews from workers leaving hierarchical organizations for flat structures.',
    'If identity lock persists: suppression is partly internalized even after structural removal (higher effective suppression than structural measure suggests). If identity dissolves: suppression is purely structural; self-management adoption should accelerate post-transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether worker identity lock persists after removal of managerial structure').

omega_variable(
    coordination_mechanism_substitutability,
    'Are peer coordination, reputation systems, and collective decision-making true substitutes for managerial task allocation, or do they fail under different stressors (rapid change, crisis response, enforcement)?',
    'Case studies of self-managing organizations under stress: rapid scaling, market disruption, resource scarcity, serious disputes. Measurement of decision-making latency and quality under pressure. Comparison of crisis response times between hierarchical and non-hierarchical organizations.',
    'If fully substitutable: managerial hierarchy is pure extraction (snare from field perspective). If substitutable with trade-offs: tangled rope holds (genuine coordination + extraction). If limited substitutability: hierarchy has structural elements (partial mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_mechanism_substitutability, empirical, 'Whether non-hierarchical coordination mechanisms substitute for managerial functions under all conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manager_role_necessity, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mana_tr_t0, manager_role_necessity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(mana_tr_t20, manager_role_necessity, theater_ratio, 20, 0.42).
narrative_ontology:measurement(mana_tr_t40, manager_role_necessity, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(mana_be_t0, manager_role_necessity, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mana_be_t20, manager_role_necessity, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(mana_be_t40, manager_role_necessity, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manager_role_necessity, resource_allocation).
narrative_ontology:affects_constraint(manager_role_necessity, labor_market_dependency).
narrative_ontology:affects_constraint(manager_role_necessity, organizational_legitimacy_narrative).
narrative_ontology:affects_constraint(manager_role_necessity, peer_review_alternative_coordination).

% DUAL FORMULATION NOTE:
% The manager role necessity constraint decomposes into at least two structurally distinct claims: (1) coordination_function_necessity — managers solve genuine problems of multi-party task allocation and conflict resolution (ε ≈ 0.20, rope); (2) authority_asymmetry_extraction — the managerial role extracts behavioral control and decision authority asymmetrically (ε ≈ 0.65, snare). These separate stories linked via network show that the 'necessity' claim conflates coordination with extraction. The tangled rope classification at ε = 0.58 represents the composite constraint where both functions operate simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manager_role_necessity, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
