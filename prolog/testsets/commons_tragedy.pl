% ============================================================================
% CONSTRAINT STORY: commons_tragedy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commons_tragedy, []).

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
 *   constraint_id: commons_tragedy
 *   human_readable: Commons Tragedy: Resource Extraction from Shared Pools
 *   domain: political_economy/resource_management
 *
 * SUMMARY:
 *   The commons tragedy represents one of the most studied structural
 *   constraints in political economy: the misalignment between individual
 *   extraction incentives and collective resource sustainability. A shared
 *   resource pool — fishery, forest, aquifer, pasture, atmosphere — generates
 *   rational pressure for each user to extract beyond sustainable levels. The
 *   individual benefit of extraction is private and immediate; the cost of
 *   overuse is distributed across all users and deferred to the future. This
 *   constraint exhibits the full range of classification types from different
 *   perspectives: the current beneficiary sees coordination opportunity
 *   (Rope), the cooperative majority sees extraction by defectors
 *   superimposed on genuine coordination (Tangled Rope), future users see
 *   irreversible loss (Snare), regulators see temporary intervention
 *   (Scaffold), and traditional governance sees degraded ritual (Piton). The
 *   constraint has received intensive study since Hardin (1968), yet
 *   extractive collapse continues — fisheries depletion, forest loss, aquifer
 *   depletion, atmospheric carbon accumulation. This suggests the constraint
 *   is not inherent to commons as such but contingent on specific governance
 *   architectures and their vulnerability to scale, technology, and external
 *   market pressure.
 *
 * KEY AGENTS:
 *   - Individual Extractors/Defectors: Moderate power (organized/arbitrage) — capture private benefit of extraction with distributed cost. Experience constraint as pure coordination benefit.
 *   - Cooperative Restraint Practitioners: Moderate power (moderate/constrained) — genuinely benefit from coordination but bear enforcement burden and suffer from free-riders. Central to Tangled Rope classification.
 *   - Future Users/Posterity: Powerless (powerless/trapped) — inherit degraded resource base with no negotiation or exit option. Primary victims bearing full deferred extraction cost.
 *   - Regulatory Authorities: Organized power (organized/mobile) — impose quota systems and protected areas as temporary scaffolding. Experience constraint as solvable through design intervention.
 *   - Customary Governance Institutions: Institutional power (institutional/arbitrage) — maintain traditional commons rules (seasonal closures, ritual access) through inertia despite weakened function. Piton perspective.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent governance failures as inherent tragedy of collective action.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commons_tragedy, 0.58).
domain_priors:suppression_score(commons_tragedy, 0.48).
domain_priors:theater_ratio(commons_tragedy, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commons_tragedy, extractiveness, 0.58).
narrative_ontology:constraint_metric(commons_tragedy, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(commons_tragedy, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commons_tragedy, tangled_rope).
narrative_ontology:human_readable(commons_tragedy, "Commons Tragedy: Resource Extraction from Shared Pools").
narrative_ontology:topic_domain(commons_tragedy, "political_economy/resource_management").

domain_priors:requires_active_enforcement(commons_tragedy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commons_tragedy, individual_extractors).
narrative_ontology:constraint_beneficiary(commons_tragedy, short_term_users).
narrative_ontology:constraint_victim(commons_tragedy, future_users).
narrative_ontology:constraint_victim(commons_tragedy, systemic_resource_base).
narrative_ontology:constraint_victim(commons_tragedy, cooperative_restraint_practitioners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE USERS (SNARE) — Cannot exit the depleted resource base; faces irreversible loss of access and ecosystem services. The future bears full structural extraction with no ability to negotiate or withdraw. Maximum suppression: the current generation's extraction locks in their powerlessness.
constraint_indexing:constraint_classification(commons_tragedy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COOPERATIVE USERS (TANGLED ROPE) — Genuinely benefit from shared resource coordination (rules, seasonal closures, rotational harvesting) but face extraction through free-riders who circumvent restraint. The constraint enforces both coordination (enables sustainable use) and asymmetric extraction (rewards defection). Constrained by monitoring costs and community enforcement burden.
constraint_indexing:constraint_classification(commons_tragedy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE DEFECTORS (ROPE) — Experience the constraint as pure coordination benefit. Individual extraction is compatible with system function (at low usage levels). They benefit from restraint-enforcing infrastructure (monitoring, punishment, resource regeneration) without bearing its cost. Arbitrage exit via migration or market substitution available.
constraint_indexing:constraint_classification(commons_tragedy, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: REGULATORY SCAFFOLD (SCAFFOLD) — Quota systems, marine protected areas, and harvest caps are designed as temporary enforcement mechanisms with implicit sunset logic. As monitoring technology improves and market-based alternatives emerge (aquaculture, synthetic substitutes, restoration targets), the need for direct commons restriction declines. Theater modest because enforcement has genuine function (preventing collapse), but scaffolding explicitly envisions replacement.
constraint_indexing:constraint_classification(commons_tragedy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CUSTOMARY MANAGEMENT (PITON) — Traditional commons governance (seasonal closures, rotational harvesting, ritual access ceremonies) persists through institutional inertia despite weakened enforcement function. The rituals have high theater (ceremonial authority, elder legitimation) relative to their actual restraint on extraction. The function (coordination) has atrophied due to externally introduced technologies and market pressures, but institutional form remains.
constraint_indexing:constraint_classification(commons_tragedy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational view, the commons tragedy appears as an immutable feature of collective action: rational individual incentives are misaligned with collective outcomes, producing inevitable degradation. This perspective naturalizes the constraint as inherent to human motivation and resource scarcity. However, the structural data contradicts this reading — successful commons have operated for centuries with institutional design choices (monitoring, graduated sanctions, conflict resolution, polycentric governance). The mountain classification is a false summit: what appears natural is contingent on governance architecture.
constraint_indexing:constraint_classification(commons_tragedy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commons_tragedy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commons_tragedy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commons_tragedy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commons_tragedy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commons_tragedy, TR),
    TR >= 0.70.

:- end_tests(commons_tragedy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint shows significant asymmetric extraction from future users and from cooperators who bear enforcement costs, but the extractiveness is not total — much resource depletion reflects coordination failure (users genuinely cannot sustain cooperative restraint at scale) rather than pure exploitative extraction. The trajectory (0.35→0.58) reflects increasing defection as population grows and technology reduces monitoring capacity. Suppression (0.48): Moderate. Users face real barriers to exit (dependence on resource, limited substitutes) but not absolute barriers. Market alternatives (aquaculture, synthetic materials) are expanding, monitoring enforcement varies, and migration is possible though costly. Theater ratio (0.35): Low-moderate. Commons management is largely functional — enforcement mechanisms (fines, gear restrictions, seasonal closures) produce real resource effects rather than performative ritual. However, some theater appears in customary governance (ceremonial authority) that has weakened functional effect. Theater increases slightly (0.28→0.35) as traditional mechanisms lose salience relative to state regulation and market substitution.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival inversion: the analytical observer at civilizational scope risks seeing a Mountain (natural tragedy), while the powerless future user at generational scope clearly sees a Snare (extraction). The beneficiary (defector) sees Rope (pure coordination benefit from public restraint investment). The cooperative majority sees Tangled Rope (genuine coordination, genuine extraction by free-riders). The regulator sees Scaffold (temporary intervention sufficient until substitutes mature). The customary institution sees Piton (ritual form persisting despite degraded function). No single perspective is canonical — the perspectival spread itself is the diagnostic signal. The mountain perspective is a false summit: the 'tragedy' is not inherent to commons but contingent on governance design, monitoring capacity, and technology availability.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is unusual: the immediate beneficiary (current extractors) is not the agent with power to enforce the constraint. Extractors with arbitrage exit experience low d (0.15-0.25) and negative χ (they are net beneficiaries). Cooperative users with constrained exit experience moderate d (0.50-0.65), making them vulnerable to extraction despite genuine coordination benefit. Future users with absolute trapped exit experience high d (0.90+), meaning their extraction is maximum. The constraint's enforcement mechanism is exercised by organized middle powers (regulatory authorities, community coalitions) who have mobile exit but are structurally interested in sustainability — their d is intermediate (0.40-0.55). This creates a paradox: those who enforce restraint benefit less than those who enforce extraction, yet enforcement capacity is with the restraint side. Collapse occurs when defectors reach critical mass relative to enforcers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination failure from asymmetric extraction. The Tangled Rope classification correctly identifies that commons mechanisms both solve coordination problems (seasonal closures do prevent race-to-the-bottom) and enable extraction (users who bear enforcement costs are targets of extraction by free-riders). The classification prevents misreading of the constraint as pure Rope (mistaking coordination benefits for absence of extraction) or pure Snare (mistaking coordination machinery for pure coercion). The analytical observer's mountain classification is revealed as a false summit through the structural data: if the tragedy were natural, commons success would be rare; in fact, successful commons existed for centuries before scale and technology enabled defection. The constraint's primary function is coordination; extraction is secondary. The mandatrophy's resolution is indexical: from the beneficiary's view, the constraint is Rope. From the victim's view, it is Snare. From the cooperative majority's view, it is Tangled Rope. All three readings are structurally correct — the constraint IS all three simultaneously, observed from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_reversibility,
    'Is the resource base at or beyond an irreversible tipping point, or is degradation still reversible with policy intervention?',
    'Ecological assessment of regeneration rates vs extraction rates; historical case studies of recovery (e.g., Atlantic cod vs striped bass); computational models of collapse threshold crossing',
    'If reversible: classification shifts toward Scaffold (temporary intervention sufficient). If irreversible: classification collapses to Snare (extraction is permanent loss, future users have no recovery path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_reversibility, empirical, 'Whether resource degradation is reversible or has crossed tipping points').

omega_variable(
    enforcement_mechanism_endogeneity,
    'Are commons enforcement institutions internally generated by user communities or externally imposed by state/market actors?',
    'Historical analysis of community governance emergence; comparative study of user-managed vs state-managed vs privatized commons; ethnographic documentation of rule legitimacy sources',
    'If internally generated: cooperation is genuine Rope (users see enforcement as coordination benefit). If externally imposed: enforcement is Snare (extractive coercion masquerading as coordination). If hybrid: Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_endogeneity, empirical, 'Source of commons enforcement legitimacy (endogenous vs exogenous)').

omega_variable(
    substitution_technology_availability,
    'Are economically viable substitutes (aquaculture, synthetic alternatives, market-procured resources) available to bypass commons restraint?',
    'Cost-benefit analysis of alternatives; market penetration rates; willingness-to-pay studies; adoption trajectories across user groups',
    'If available: exit options expand (mobile → arbitrage), defector positions strengthen, Rope classification dominates. If unavailable: users are trapped in commons (constrained or trapped exit), Snare or Tangled Rope classifications dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technology_availability, empirical, 'Availability of economically viable substitutes for commons resources').

omega_variable(
    monitoring_capacity_inequality,
    'Do monitoring systems have capacity parity across all users, or do some users have systematic ability to hide extraction?',
    'Audits of monitoring technology distribution; detection rates by user type; hidden extraction analysis via DNA tracing, remote sensing, or catch reconstruction',
    'If parity: suppression is moderate (equal enforcement). If inequality: suppression is high (systematic evasion for privileged users), extraction becomes asymmetric, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monitoring_capacity_inequality, empirical, 'Monitoring capacity parity across user groups').

omega_variable(
    discount_rate_coordination,
    'Do users have aligned or misaligned time horizons? Are individual discount rates (preference for immediate extraction) endogenous to the constraint or exogenously determined by poverty, debt, or market pressure?',
    'Analysis of user financial stability; debt service burdens; subsistence dependency; temporal preference surveys; correlation between extraction rates and poverty/debt metrics',
    'If exogenously driven (poverty forces short-term extraction): the constraint is not failure of coordination but systemic inequality, classification shifts to Snare (asymmetric extraction). If endogenous (users choose short-term despite resource sustainability): Tangled Rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(discount_rate_coordination, conceptual, 'Whether time-horizon misalignment is endogenous preference or exogenous constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commons_tragedy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commons_tr_t0, commons_tragedy, theater_ratio, 0, 0.28).
narrative_ontology:measurement(commons_tr_t10, commons_tragedy, theater_ratio, 10, 0.32).
narrative_ontology:measurement(commons_tr_t20, commons_tragedy, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(commons_be_t0, commons_tragedy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(commons_be_t10, commons_tragedy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(commons_be_t20, commons_tragedy, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commons_tragedy, resource_allocation).
narrative_ontology:affects_constraint(commons_tragedy, climate_change_atmospheric_commons).
narrative_ontology:affects_constraint(commons_tragedy, fisheries_regulatory_capture).
narrative_ontology:affects_constraint(commons_tragedy, groundwater_depletion).

% DUAL FORMULATION NOTE:
% The commons tragedy is upstream of specific resource depletion constraints (fisheries, aquifers, forests). Each specific resource has its own ε based on regeneration capacity and extraction rates. The general commons tragedy represents the governance architecture that either prevents or enables depletion. Decompose into domain-specific stories if measuring specific resources; use this story for governance patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commons_tragedy, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
