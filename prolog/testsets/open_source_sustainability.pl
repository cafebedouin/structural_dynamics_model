% ============================================================================
% CONSTRAINT STORY: open_source_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_sustainability, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: open_source_sustainability
 *   human_readable: Open Source Sustainability Constraint
 *   domain: technology_economics/governance
 *
 * SUMMARY:
 *   Open source software represents a structural tension between genuine
 *   coordination benefits (code reuse, standardization, security through
 *   transparency) and profound extraction of maintenance labor from core
 *   developers. The constraint manifests as a tangled rope: corporations and
 *   platforms benefit enormously from cost-free access to critical
 *   infrastructure, while individual maintainers bear unbounded maintenance
 *   costs with minimal financial compensation. The theater ratio has
 *   increased from 0.35 to 0.75 over thirty years as the 'open source gift
 *   economy' ideology has become increasingly performative — the narrative
 *   persists despite contradicting material reality. The extractiveness has
 *   increased from 0.32 to 0.58 as projects have become mission-critical to
 *   global infrastructure, concentrating maintenance burdens on smaller
 *   groups while expanding user bases and value capture elsewhere. This
 *   constraint exhibits all six DR types from different observational
 *   positions: core maintainers experience snare conditions (trapped, no
 *   viable exit); corporate integrators experience rope (beneficial
 *   coordination with high agency); dependent developers experience tangled
 *   rope (mixed benefits and extraction); sustainability initiatives see a
 *   scaffold with emerging sunset mechanisms; the open source ideology
 *   framework appears as a piton (degraded ritual persisting through
 *   inertia); and both the platform company and analytical observer face
 *   classification ambiguity that reveals false naturalization.
 *
 * KEY AGENTS:
 *   - Core Maintainers: Primary victims (powerless/trapped) — bear unbounded maintenance labor with minimal income; identity-locked or deeply constrained; experience maximum extraction.
 *   - Corporate Users: Primary beneficiaries (institutional/arbitrage) — access cost-free critical infrastructure; face minimal friction costs; experience pure coordination benefit.
 *   - Downstream Developers: Secondary beneficiaries (moderate/constrained) — depend on maintained projects; experience both benefit from stability and extraction through unmet maintenance demands.
 *   - Ecosystem Reliability: Victim (powerless/trapped) — abstract collective good that cannot organize; vulnerable to abandonment when maintainers burn out; bears cost of supply-chain risk.
 *   - Sustainability Coalition: Organized actor (organized/constrained) — sponsorship programs, foundations, funding platforms building alternative mechanisms with sunset trajectories.
 *   - Open Source Ideology: Institutional narrative (institutional/arbitrage) — performative framework that masks extraction; persists through cultural inertia despite contradicting material reality.
 *   - Platform Companies: Structural ambiguity (powerful/mobile) — simultaneously beneficiary and dependent; face supply-chain risk from projects they don't control; investing in sponsorship to secure pipelines.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_sustainability, 0.58).
domain_priors:suppression_score(open_source_sustainability, 0.62).
domain_priors:theater_ratio(open_source_sustainability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_sustainability, extractiveness, 0.58).
narrative_ontology:constraint_metric(open_source_sustainability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(open_source_sustainability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_sustainability, tangled_rope).
narrative_ontology:human_readable(open_source_sustainability, "Open Source Sustainability Constraint").
narrative_ontology:topic_domain(open_source_sustainability, "technology_economics/governance").

domain_priors:requires_active_enforcement(open_source_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_sustainability, corporate_users).
narrative_ontology:constraint_beneficiary(open_source_sustainability, platform_companies).
narrative_ontology:constraint_beneficiary(open_source_sustainability, downstream_integrators).
narrative_ontology:constraint_victim(open_source_sustainability, core_maintainers).
narrative_ontology:constraint_victim(open_source_sustainability, ecosystem_reliability).
narrative_ontology:constraint_victim(open_source_sustainability, software_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPAID MAINTAINER (SNARE) — Core maintainers are trapped: they cannot exit without abandoning projects they have invested years building. Exit costs include loss of professional identity, community standing, and inability to influence the software's future. Suppression is structural: no viable income stream from maintenance work, burnout-driven attrition, and lack of alternatives. Maximum extraction — volunteer labor subsidizes corporate dependency.
constraint_indexing:constraint_classification(open_source_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CORPORATE INTEGRATOR (ROPE) — Benefits from cost-free access to critical infrastructure. Experiences the constraint as pure coordination: using open source solves legitimate problems of standardization and code reuse. Can arbitrage: they can fork, maintain internally, or switch to proprietary alternatives. Net beneficiary with high structural agency.
constraint_indexing:constraint_classification(open_source_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DEPENDENT DEVELOPER (TANGLED ROPE) — Constrained by career incentives: contributing to open source builds professional reputation but does not generate income. Face meaningful costs to exit (lost community standing, career disruption) but retain some agency. Benefits from network effects and collaborative learning; also extracted from through unpaid labor expectations.
constraint_indexing:constraint_classification(open_source_sustainability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SUSTAINABILITY COALITION (SCAFFOLD) — Organized agents (sponsorship programs, GitHub funding, open-source foundations) see the unsustainability as a temporary coordination failure with visible exit pathways. Programs like GitHub Sponsors, OpenCollective, and foundation grants are creating alternative funding mechanisms. Suppression is declining as institutional recognition grows. Sunset clause: as formal funding mechanisms mature, the pure extraction mechanism (free labor) loses force.
constraint_indexing:constraint_classification(open_source_sustainability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE IDEOLOGY (PITON) — The cultural narrative of open source as 'collaborative gift economy' persists despite being substantially degraded. The ideology performed a real coordination function in the 1990s-2000s when software commodification was novel. Now it primarily masks extraction: maintainers continue unpaid work because they have internalized that 'real hackers code for love.' The performative element (the 'meritocratic community' story) has become decoupled from material reality. Theater ratio is high because maintaining the ideology requires constant ritual affirmation despite obvious structural contradictions.
constraint_indexing:constraint_classification(open_source_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PLATFORM COMPANY (TANGLED ROPE from structural view) — At larger time horizons and from a structural perspective recognizing platform dependency, major tech companies occupy an ambiguous position. They benefit enormously from open source infrastructure (genuine coordination function: standardization, code reuse, security through transparency). But they also face structural extraction: dependency on projects they don't control creates supply-chain risk, forcing them to invest in sponsorship and hiring to secure maintenance pipelines. This is a genuine tangled rope: real coordination gain bundled with real extraction risk. Mobile exit options (fork, maintain internally, switch) but substantial switching costs create structured mobility constraints.
constraint_indexing:constraint_classification(open_source_sustainability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some degree of asymmetry between users and maintainers appears inherent to software: every use has beneficiaries (users) and costs (maintenance). The constraint could be viewed as an immutable property of complex software systems. However, the structural data reveals this as false naturalization: the asymmetry is not inherent but contingent on institutional arrangements (copyright law, property norms, funding structures). This is a false summit.
constraint_indexing:constraint_classification(open_source_sustainability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_source_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_source_sustainability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_source_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_source_sustainability, TR),
    TR >= 0.70.

:- end_tests(open_source_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Core maintainers extract enormous value for users (billions of dollars in user value) while bearing unbounded maintenance costs compensated at near-zero rates. The value asymmetry is stark: a single critical project maintained by 3 people can serve millions of users generating trillions in derived value. The extractiveness is not at snare-level (0.66+) because some maintainers receive partial compensation, some projects have sustainable funding, and the structure is theoretically reformable. But the default condition is extraction. Theater ratio (0.68): High and increasing. The open source ideology ('meritocratic gift economy,' 'community-driven development,' 'hackers coding for love') performs a critical narrative function: it obscures the extraction by framing it as voluntary contribution rather than uncompensated labor. The performative element has increased because the ideology has become decoupled from reality — projects are now critical infrastructure, not hobby communities. Suppression (0.62): High. Multiple mechanisms suppress maintainer exit: (a) identity lock — many maintainers cannot imagine themselves outside their projects; (b) community shame — abandoning a project triggers guilt and reputation damage; (c) lack of alternatives — few funding models exist for open source maintenance; (d) FOSS licensing itself creates path dependency — forking requires duplicating maintenance burdens. The interval shows increasing theater and extractiveness, consistent with increasing maturity (larger user base, more mission-critical role) without corresponding increases in sustainable funding.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence: the same structural arrangement is rope (pure coordination benefit) from the corporate perspective, snare (pure extraction) from the maintainer perspective, and tangled rope (mixed) from the developer and platform perspectives. The gap reflects genuine structural asymmetry in exit options and value capture — not measurement ambiguity but real difference in lived experience. The maintainer sees themselves as trapped unpaid laborers subsidizing corporate profits. The corporate user sees standard vendor relationships where upstream handles maintenance and they integrate into their stack. Both are accurate descriptions of their own structural position; they are incompatible descriptions only if forced into a single perspective. The analytical observer's mountain classification is a false summit: it naturalizes what is contingent institutional arrangement (copyright law, funding norms, property allocation). The constraint is not immutable; it is contingently structured by policy choices about software ownership, funding mechanisms, and labor compensation.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) is computed from their structural relationship: who benefits from the extraction flow and with what costs to exit. Beneficiaries with high agency (corporate users, arbitrage exit) get low d values (0.05 for institutional arbitrage). Victims with high exit costs (core maintainers, trapped) get high d values (0.95). The sigmoid f(d) maps these to experienced extraction multipliers ranging from -0.12 (for beneficiaries) to 1.42 (for victims). Applied to global scope (σ(S) = 1.2), this produces effective extraction (χ) ranging from near-zero for corporate users to near-unity for maintainers, explaining the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint satisfies mandatrophy requirements by demonstrating genuine coordination function (code reuse, standardization, security benefits) bundled with asymmetric extraction (unpaid maintenance labor). The beneficiary/victim declarations are material: corporations and platforms genuinely benefit from cost-free access; core maintainers genuinely bear unbounded labor costs. The alternative analyses reveal: (1) the snare classification from the maintainer perspective is not an error but an accurate description of their structural position; (2) the rope classification from the corporate perspective is accurate for their position; (3) the tangled rope analytical classification encompasses both through the χ formula (different d values yield different effective extraction despite identical base properties); (4) the piton classification diagnoses performative ideology maintenance despite degraded function; (5) the scaffold classification identifies real alternative mechanisms with sunset properties. Mandatrophy is resolved by accepting perspectival legitimacy: the constraint truly exhibits multiple types because agents genuinely occupy different structural positions relative to the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintenance_burden_measurability,
    'Is the maintenance burden on core maintainers accurately captured by labor hours, or is there unmeasured psychic cost (identity burden, community accountability) that exceeds the extractiveness score?',
    'Longitudinal interviews with long-term maintainers; correlation between stated burnout levels and formal time commitments; analysis of maintenance work that is invisible (moderation, conflict resolution, institutional memory)',
    'If psychic cost is substantial: actual extractiveness > 0.58; snare classification strengthens. If primarily labor-measurable: current score is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_burden_measurability, empirical, 'Whether maintenance burden is fully captured by measurable labor metrics').

omega_variable(
    alternative_funding_mechanism_effectiveness,
    'Do emerging funding mechanisms (GitHub Sponsors, Open Collective, foundation grants) actually redirect material resources to maintainers at scale sufficient to reduce extraction, or do they primarily create visibility theater?',
    'Analysis of funding flow distribution: what percentage of active maintainers receive material support? Average annual support per maintainer? Correlation between funding receipt and reported burnout reduction.',
    'If >30% of maintainers receive >$10k annual: scaffold sunset is real. If <10% coverage or token amounts: funding mechanisms are piton-like theater, and snare classification persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_funding_mechanism_effectiveness, empirical, 'Whether formal funding mechanisms provide sufficient material support to reduce extraction').

omega_variable(
    corporate_dependency_asymmetry,
    'Is the extraction relationship genuinely symmetric between corporate users and individual maintainers, or do scale differences (millions of users vs. handful of maintainers) create structural asymmetry that the tangled_rope classification underestimates?',
    'Mapping of corporate revenue derived from open source infrastructure vs. total resources returned to maintainers; analysis of power asymmetry in setting maintenance agendas (do corporate users'' needs drive roadmaps?)',
    'If corporate extraction exceeds maintainer extraction: snare classification may be more accurate than tangled_rope for corporate perspective. If roughly balanced: tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_dependency_asymmetry, empirical, 'Whether corporate-maintainer extraction relationship is actually symmetric').

omega_variable(
    identity_lock_vs_constrained,
    'For core maintainers, is the binding mechanism primarily identity-locked (self-concept constituted through the project) or constrained (high-cost exit with material barriers)?',
    'Analysis of exit patterns: do maintainers leave when funded externally? When identity investment shifts? Post-exit behavior: do they rebuild similar projects? Can they articulate reasons for staying beyond ''this is who I am''?',
    'If identity-locked: perspective classification changes to identity_locked exit option; suggests cognitive capture mechanism. If constrained: trapped classification more accurate; suggests material dependency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained, empirical, 'Whether maintainer binding is identity-based or material-constraint-based').

omega_variable(
    institutional_capture_risk,
    'Are major open-source projects experiencing regulatory or institutional capture, where corporate funding creates de facto control over governance without formal authority?',
    'Comparative governance analysis: Linux Foundation, Apache Foundation, Rust Foundation governance structures; analysis of decision-making when funder interests diverge from community interests; historical cases of captured projects',
    'If capture is widespread: the corporate perspective shifts from rope to snare (they capture governance, extracting community labor through direction-setting). If governance remains distributed: rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Whether institutional capture is occurring in major open-source projects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_sustainability, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oss_tr_t0, open_source_sustainability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(oss_tr_t10, open_source_sustainability, theater_ratio, 10, 0.52).
narrative_ontology:measurement(oss_tr_t20, open_source_sustainability, theater_ratio, 20, 0.68).
narrative_ontology:measurement(oss_tr_t30, open_source_sustainability, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(oss_be_t0, open_source_sustainability, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(oss_be_t10, open_source_sustainability, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(oss_be_t20, open_source_sustainability, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(oss_be_t30, open_source_sustainability, base_extractiveness, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_sustainability, resource_allocation).
narrative_ontology:boltzmann_floor_override(open_source_sustainability, 0.12).
narrative_ontology:affects_constraint(open_source_sustainability, software_supply_chain_security).
narrative_ontology:affects_constraint(open_source_sustainability, digital_commons_sustainability).
narrative_ontology:affects_constraint(open_source_sustainability, tech_worker_extraction).

% DUAL FORMULATION NOTE:
% Open source sustainability is upstream of supply-chain security and digital commons stability. The unsustainability of maintainer compensation creates cascading risk for dependent projects. Decomposable into: (1) individual maintainer sustainability (labor economics story, ε≈0.72), (2) project governance sustainability (institutional capture story, ε≈0.55), (3) ecosystem diversity maintenance (commons stability story, ε≈0.48). This story captures the aggregate constraint; see linked stories for domain-specific decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_sustainability, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
