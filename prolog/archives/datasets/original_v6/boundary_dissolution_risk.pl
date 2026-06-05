% ============================================================================
% CONSTRAINT STORY: boundary_dissolution_risk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boundary_dissolution_risk, []).

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
 *   constraint_id: boundary_dissolution_risk
 *   human_readable: The Infinite Porosity Trap
 *   domain: technological/labor
 *
 * SUMMARY:
 *   The Infinite Porosity Trap describes the structural dissolution of
 *   boundaries between professional and personal domains through ubiquitous
 *   connectivity, digital surveillance, and algorithmic management. Once
 *   discrete domains (office vs. home, work time vs. personal time, digital
 *   vs. physical presence) have become permeable, with surveillance and
 *   coordination infrastructure flowing in one direction: inward. Workers
 *   experience constant availability expectations, monitoring of productivity
 *   metrics, location tracking, communication analysis, and behavioral
 *   prediction. The constraint exhibits a perspectival range from Snare
 *   (powerless workers) through Tangled Rope (workers benefiting from remote
 *   work but under surveillance) to Rope (platform operators seeing only
 *   coordination benefits) to Scaffold (regulatory movements toward
 *   alternatives) to Piton (degraded employment contract ritual). The
 *   extractiveness trajectory shows acceleration: from 0.25 (early remote
 *   work, relatively lightweight monitoring) to 0.58 (mature surveillance
 *   capitalism integrated with employment), while theater ratio rises from
 *   0.35 (monitoring partly functional) to 0.55 (increasingly performative
 *   compliance with anti-monitoring rhetoric while surveillance deepens). The
 *   constraint's defining feature is irreversibility within opt-in framing:
 *   once connectivity infrastructure exists, opting out requires exit from
 *   the labor market entirely.
 *
 * KEY AGENTS:
 *   - Knowledge Workers: Primary victims (powerless/trapped) — cannot exit surveillance without losing employment; experience one-way boundary dissolution
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture value from data asymmetry and monitoring capabilities; control enforcement infrastructure
 *   - Surveillance Vendors: Secondary beneficiary (institutional/arbitrage) — provide monitoring technology to platforms; benefit from increasing instrumentation
 *   - Privacy Rights Coalition: Organized agents (organized/constrained) — labor unions, regulatory bodies (EU GDPR), academic researchers building alternatives; constrained by political economy of platform capital
 *   - Employment Contract System: Institutional actor (institutional/arbitrage) — maintains performative boundary (contract says off-hours are personal) while infrastructure violates it; sees own legitimacy degrading (piton)
 *   - Autonomy Commons: Victim (powerless/trapped) — abstract collective good of worker agency and psychological separation; cannot organize or exit; bears full cost of porosity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boundary_dissolution_risk, 0.58).
domain_priors:suppression_score(boundary_dissolution_risk, 0.68).
domain_priors:theater_ratio(boundary_dissolution_risk, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boundary_dissolution_risk, extractiveness, 0.58).
narrative_ontology:constraint_metric(boundary_dissolution_risk, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(boundary_dissolution_risk, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boundary_dissolution_risk, tangled_rope).
narrative_ontology:human_readable(boundary_dissolution_risk, "The Infinite Porosity Trap").
narrative_ontology:topic_domain(boundary_dissolution_risk, "technological/labor").

domain_priors:requires_active_enforcement(boundary_dissolution_risk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, platform_operators).
narrative_ontology:constraint_beneficiary(boundary_dissolution_risk, surveillance_infrastructure_vendors).
narrative_ontology:constraint_victim(boundary_dissolution_risk, knowledge_workers).
narrative_ontology:constraint_victim(boundary_dissolution_risk, autonomy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MONITORED WORKER (SNARE) — Professional boundary collapse is structurally irreversible within current labor markets. Employees cannot exit surveillance without forfeiting employment. The work-life boundary, once discrete, is now permeable in one direction only: surveillance flows inward constantly. No technical, contractual, or social alternative exists. Maximum extraction — worker bears full cost of porosity while receiving no coordination benefit.
constraint_indexing:constraint_classification(boundary_dissolution_risk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE KNOWLEDGE WORKER COLLECTIVE (TANGLED ROPE) — Workers benefit from remote work access and asynchronous collaboration enabled by connectivity. They also bear extraction costs: boundary dissolution, surveillance penetration, always-on expectation. Constrained exit (cannot easily switch to offline-only work without career penalty). Mixed structure: genuine coordination function (distributed work) paired with asymmetric surveillance extraction. Active enforcement through productivity monitoring and algorithmic management.
constraint_indexing:constraint_classification(boundary_dissolution_risk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PLATFORM OPERATOR (ROPE) — Experiences connectivity infrastructure as pure coordination mechanism: enabling global labor matching, asynchronous work, and automated task distribution. From their perspective, monitoring is a coordination tool (verify task completion, match workers to tasks). Benefits from arbitrage (can move surveillance capabilities between markets, extract rents from data asymmetry). Low suppression experienced — they control the infrastructure and set the rules. Net beneficiary.
constraint_indexing:constraint_classification(boundary_dissolution_risk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PRIVACY RIGHTS COALITION (SCAFFOLD) — Organized agents (privacy advocates, labor unions, EU regulatory frameworks) see boundary dissolution as a temporary market failure with a sunset: data minimization laws (GDPR), right-to-disconnect regulations, portable work credentials, and decentralized identity systems are building alternative architectures. Sunset estimate: 15-25 years as regulatory frameworks mature and alternatives (federation, peer-to-peer) demonstrate viability. Low theater: these movements focus on structural alternatives rather than performative compliance. Constrained exit reflects regulatory capture resistance.
constraint_indexing:constraint_classification(boundary_dissolution_risk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE EMPLOYMENT CONTRACT RITUAL (PITON) — The formal employment contract once created a boundary: company property vs. personal time, workplace vs. home. Digital connectivity has eroded this boundary's practical function, but the contract ritual persists through institutional inertia. Companies still claim 'off-hours' are personal time while simultaneously monitoring email, Slack, location, and productivity metrics. Theater ratio high (0.55): the boundary is performatively maintained in contract language while being systematically violated in practice. Piton: the original function (clear separation) has atrophied; the form remains.
constraint_indexing:constraint_classification(boundary_dissolution_risk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / PHYSICAL LIMIT VIEW (MOUNTAIN) — From a universal perspective, boundary dissolution appears as an inevitable feature of ubiquitous connectivity: once information can flow costlessly across space, the distinction between 'here' and 'there,' 'work time' and 'personal time' becomes a matter of computational convention rather than physical reality. The porosity is inherent to the medium. However, this naturalizes a contingent institutional choice: we could design systems with hard boundaries (end-to-end encryption, data silos, temporal firewalls), but instead we choose architectures that maximize data flow. The mountain classification is a false summit — it mistakes a design choice for a law of nature.
constraint_indexing:constraint_classification(boundary_dissolution_risk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_dissolution_risk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_dissolution_risk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(boundary_dissolution_risk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(boundary_dissolution_risk, TR),
    TR >= 0.70.

:- end_tests(boundary_dissolution_risk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Base measurement reflects the magnitude of data and behavioral control extracted from workers. Unlike raw data theft, this extraction is nested within an employment relationship where workers receive wages in exchange for some degree of monitoring. However, the scope has expanded far beyond what employment legitimately requires — always-on availability expectations, real-time productivity metrics, location tracking, communication surveillance, and predictive behavioral monitoring constitute extraction beyond reasonable job performance verification. The trajectory from 0.25 to 0.58 over the interval reflects the deepening of surveillance integration as platforms mature and competitive pressure drives monitoring expansion. Suppression (0.68): High. Significant structural barriers limit workers' exit options: (1) labor market concentration (few alternative employers without surveillance), (2) skill specialization (switching careers is costly), (3) regulatory capture (employment law enables rather than restricts monitoring), (4) technological lock-in (accumulated data and credentials are platform-specific), (5) career risk (refusing monitoring signals unreliability), (6) normalized expectations (always-on is now industry standard). Suppression is not absolute — some workers can negotiate boundaries, some labor markets respect offline-only arrangements — but the path is narrow. Theater ratio (0.55): Moderate. Monitoring is partly functional (task verification, user matching) and partly performative (showing 'productivity' to justify salary, creating illusion of control). The rise from 0.35 to 0.55 reflects increasing theater as monitoring systems proliferate beyond functional necessity — heat maps, keystroke analysis, email sentiment analysis, meeting attendance tracking. Companies simultaneously use monitoring for real coordination decisions AND use the same data to justify power over workers. The performative content has grown as technical capabilities have exceeded coordination needs.
 *
 * PERSPECTIVAL GAP:
 *   The monitored worker (Snare) and platform operator (Rope) perceive opposite constraints from identical infrastructure. The worker experiences one-way surveillance extraction — boundary dissolution is enforced, irreversible, and bears maximum cost. The platform operator experiences bidirectional coordination — they see monitoring as a tool for matching workers to tasks efficiently. Both are describing the same infrastructure, but the directionality is inverted. The knowledge worker collective (Tangled Rope) occupies the middle: they genuinely benefit from remote work access (which requires connectivity and some monitoring) while being harmed by the surveillance expansion beyond what coordination requires. The scaffold perspective (privacy coalition) sees the current state as temporary — regulatory frameworks and alternative architectures (decentralized identity, data minimization, federation) are building paths toward lower-extraction coordination. The piton perspective reveals that the employment contract (institution maintaining the boundary performatively) is degrading — its legitimacy rests on a boundary it no longer enforces. The mountain perspective risks naturalizing the porosity as an inherent feature of digital connectivity, when it is actually a choice about infrastructure design.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply by structural position. Platform operators (institutional/arbitrage) have d ≈ 0.05-0.15 (net beneficiaries): they control infrastructure, set enforcement rules, and arbitrage data value. Their exit options are excellent — they can shift business models, move to new markets, offshore operations. Beneficiaries experience low or negative f(d). Knowledge workers (powerless/trapped) have d ≈ 0.90-0.95 (net targets): they cannot exit without career termination, their data is continuously extracted, and they have no control over enforcement. Trapped exit status combined with victim status produces maximum d and maximum f(d) ≈ 1.40, yielding the highest experienced extraction. Moderate workers (moderate/constrained) have d ≈ 0.65-0.75: they benefit from remote work coordination but face real career penalties for opting out. Their constrained exit produces moderate-high d. Organized agents (privacy coalition) have d ≈ 0.40-0.50: constrained by political economy but with real agency through regulatory change and alternative infrastructure. Analytical observer at civilizational scale treats porosity as universal feature (potential d ≈ 0.50 for symmetric view), but structural data reveals this as false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here resolves through decomposition by agent position. This is NOT a case of mislabeled pure extraction or pure coordination — both exist structurally. The Tangled Rope classification is correct: there IS genuine coordination (remote work, global labor matching, asynchronous collaboration) AND there IS asymmetric extraction (surveillance, data collection, always-on expectations). The trap is that the coordination justification is used to legitimize extraction beyond what coordination requires. The platform operator's Rope perspective is not wrong — they genuinely solve coordination problems. But it is partial: it observes only the coordination function while systematically underobserving the extraction. The worker's Snare perspective is also not wrong — they do experience maximal extraction. But it too is partial: it may underobserve the real benefits of remote work access. The Scaffold perspective resolves the mandatrophy by showing that lower-extraction alternatives exist: decentralized platforms, worker-owned cooperatives, federation architectures, and data minimization by design could deliver the same coordination benefits (remote work, task matching, asynchronous work) with far lower extraction and suppression. The existence of feasible lower-extraction designs proves that the current high extractiveness is not inherent to coordination but is a choice. Therefore, the constraint is correctly classified as Tangled Rope (0.58 extractiveness, 0.68 suppression, 0.55 theater) with a genuine sunset clause (alternative architectures are technically viable and legally possible in EU/progressive jurisdictions). This is not coordination masquerading as extraction, nor is it extraction masquerading as coordination — it is hybrid, with the key analytical point being that the extraction component is unnecessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    monitoring_technology_reversibility,
    'Can monitoring infrastructure be technically reversed or replaced once installed and normalized?',
    'Historical analysis of surveillance infrastructure retraction (when has monitoring been successfully removed or opt-in made genuinely frictionless?); technical assessment of decentralized alternatives'' deployment barriers',
    'If reversible: constraint is Scaffold (temporary, with exit path). If irreversible: constraint approaches Snare (structural lock-in). Current evidence suggests partial irreversibility due to data accumulation and algorithmic dependencies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monitoring_technology_reversibility, empirical, 'Whether monitoring infrastructure can be technically reversed or replaced').

omega_variable(
    boundary_cognitive_cost,
    'Is the cognitive cost of living in a porous boundary environment a structural feature of digital work or a transitional adaptation cost?',
    'Longitudinal cognitive load studies comparing workers with clear boundaries (offline-only) vs. porous boundaries (always-on); measurement of stress, focus capacity, creativity metrics over generational timescales',
    'If structural: workers will always experience extraction regardless of regulatory fixes — constraint is closer to Mountain (inherent cost of digital labor). If transitional: new cognitive norms and technologies can reduce cost — constraint remains Snare/Tangled Rope but with possibility of mitigation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_cognitive_cost, empirical, 'Whether boundary porosity cost is structural or transitional').

omega_variable(
    opt_out_career_viability,
    'Can workers actually maintain competitive careers while opting out of boundary dissolution (going fully offline, refusing monitoring, maintaining hard time boundaries)?',
    'Comparative analysis of career trajectories for boundary-respecting vs. boundary-porous workers; measurement of wage differential, advancement rates, opportunities; assessment of alternative labor market segments (cooperatives, offline firms) and their scale',
    'If viable opt-out exists: exit_options upgrade from trapped to constrained — classification shifts from Snare toward Tangled Rope. If opt-out is purely performative or career-terminal: exit remains trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opt_out_career_viability, empirical, 'Whether career viability is possible while opting out of boundary dissolution').

omega_variable(
    surveillance_data_asymmetry_necessity,
    'Is worker monitoring technically necessary for platform coordination, or is it primarily value extraction?',
    'Comparison of coordination outcomes in platforms with minimal monitoring vs. maximum monitoring; analysis of task-matching efficiency, user satisfaction, and completion rates; identification of which specific monitoring datapoints are actually used for coordination decisions',
    'If necessary for coordination: Tangled Rope classification solidified (genuine coordination + extraction). If unnecessary: classification shifts toward Snare (pure extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_data_asymmetry_necessity, empirical, 'Whether monitoring is technically necessary or primarily extractive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boundary_dissolution_risk, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(boundary_tr_t0, boundary_dissolution_risk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(boundary_tr_t5, boundary_dissolution_risk, theater_ratio, 5, 0.48).
narrative_ontology:measurement(boundary_tr_t10, boundary_dissolution_risk, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(boundary_be_t0, boundary_dissolution_risk, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(boundary_be_t5, boundary_dissolution_risk, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(boundary_be_t10, boundary_dissolution_risk, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boundary_dissolution_risk, resource_allocation).
narrative_ontology:affects_constraint(boundary_dissolution_risk, algorithmic_management_escalation).
narrative_ontology:affects_constraint(boundary_dissolution_risk, work_life_boundary_erosion).
narrative_ontology:affects_constraint(boundary_dissolution_risk, data_asymmetry_labor_markets).

% DUAL FORMULATION NOTE:
% Boundary dissolution is downstream of platform infrastructure design choices and upstream of specific labor exploitation mechanisms (wage suppression, algorithmic discipline). This constraint represents the structural condition that enables downstream extractive mechanisms. Coordinate with algorithmic_management_escalation (which models the enforcement layer) and data_asymmetry_labor_markets (which models the economic extraction layer). The family shares a common root: the choice to design systems maximizing data flow rather than maintaining hard boundaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boundary_dissolution_risk, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
