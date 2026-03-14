% ============================================================================
% CONSTRAINT STORY: precarious_worker_volatility_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_precarious_worker_volatility_extraction, []).

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
 *   constraint_id: precarious_worker_volatility_extraction
 *   human_readable: Precarious Worker Volatility Extraction
 *   domain: labor_economics/employment_precarity
 *
 * SUMMARY:
 *   Precarious work — employment characterized by short-term contracts,
 *   on-demand scheduling, lack of benefits, and income volatility — has
 *   become structurally integral to labor markets in developed economies. The
 *   constraint operates by transferring demand volatility from employers and
 *   capital to individual workers. This transfer generates extraction:
 *   employers capture coordination benefits (efficient demand matching,
 *   reduced labor costs), while workers bear volatility costs (income
 *   instability, inability to plan, health/housing insecurity, stress). The
 *   constraint is enforced not primarily through explicit coercion but
 *   through material barriers (lack of savings, geographic immobility,
 *   credential requirements) and internalized acceptance that precarity is
 *   normal. The theater ratio (0.55) reflects that precarity is partially
 *   cloaked in narratives of worker flexibility and entrepreneurship ('gig
 *   economy,' 'independent contractor,' 'owner-operator') while the
 *   extraction mechanism (volatility transfer) remains structurally intact.
 *   The extractiveness trajectory shows increasing extraction over the
 *   interval (0.35 → 0.62) as platform economies have matured and normalized
 *   precarity as a primary employment model.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victims (powerless/trapped) — absorb demand volatility, face income instability, lack collective bargaining power; material barriers prevent exit to stable employment
 *   - Platform/On-Demand Employers: Primary beneficiaries (institutional/arbitrage) — capture coordination value and extraction rents; mobile exit via regulatory arbitrage or workforce restructuring
 *   - Labor Demand Aggregators: Secondary beneficiaries (powerful/mobile) — multinational platforms that engineered volatility-absorption models; actively extract through algorithmic task assignment and piece-rate structuring
 *   - Worker Organizing Collectives: Secondary victims (moderate/constrained) — coordinate precarious workers, benefit from organizing legitimacy, but face legal barriers and coordination costs
 *   - Regulatory Reform Coalition: Organized agents (organized/constrained) — labor unions, worker advocacy groups, progressive regulators building sunset mechanisms (sectoral bargaining, portable benefits, scheduling standards)
 *   - Traditional Employment System: Institutional actor (institutional/arbitrage) — maintains legal fiction of 'employment relationship' while allowing precarity to degrade it; piton: performative invocation without enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing precarity as inevitable volatility absorption, obscuring that volatility distribution is an institutional choice, not a law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(precarious_worker_volatility_extraction, 0.62).
domain_priors:suppression_score(precarious_worker_volatility_extraction, 0.68).
domain_priors:theater_ratio(precarious_worker_volatility_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(precarious_worker_volatility_extraction, extractiveness, 0.62).
narrative_ontology:constraint_metric(precarious_worker_volatility_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(precarious_worker_volatility_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(precarious_worker_volatility_extraction, tangled_rope).
narrative_ontology:human_readable(precarious_worker_volatility_extraction, "Precarious Worker Volatility Extraction").
narrative_ontology:topic_domain(precarious_worker_volatility_extraction, "labor_economics/employment_precarity").

domain_priors:requires_active_enforcement(precarious_worker_volatility_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(precarious_worker_volatility_extraction, labor_demand_aggregators).
narrative_ontology:constraint_beneficiary(precarious_worker_volatility_extraction, capital_owners).
narrative_ontology:constraint_victim(precarious_worker_volatility_extraction, precarious_workers).
narrative_ontology:constraint_victim(precarious_worker_volatility_extraction, labor_market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by economic necessity and lack of alternative income sources. Bears full cost of demand volatility: income instability, inability to plan, reduced access to credit and housing. Suppression is structural: material barriers to stable employment, lack of collective bargaining power, geographic immobility constraints. No meaningful exit option except withdrawal from labor market entirely, which is economically infeasible.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKER ORGANIZING COLLECTIVE (TANGLED ROPE) — Constrained by coordination costs and legal/political barriers to collective action, but benefits from the volatility constraint through increased organizing opportunity and legitimacy. Extraction is asymmetric: workers provide coordination value through collective identity and mutual aid, yet the constraint's structure prevents capturing this value at scale. Exit is costly but possible through legislative change or sustained organizing.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM AND ON-DEMAND EMPLOYER (ROPE) — Experiences the constraint as pure coordination benefit: demand volatility is managed by transferring timing risk to workers rather than maintaining costly inventory or workforce buffers. This is genuinely a coordination solution — it solves the employer's real problem of demand fluctuation. Beneficiary with arbitrage access: can shift to alternative workforce models or geographic markets if regulatory environment changes.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL LABOR DEMAND AGGREGATOR (TANGLED ROPE) — Large platforms (Uber, Amazon, DoorDash, TaskRabbit) coordinate labor supply across massive demand fluctuations while extracting volatility rents through algorithmic task assignment and piece-rate structuring. Benefits from coordination function (real value to customers) but extracts asymmetrically through wage suppression. Mobile exit: can shift operations, adjust geographic footprint, or modify worker classification. Powerful agent with agency.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized groups (labor unions, worker advocacy organizations, progressive regulators) perceive the volatility extraction as a temporary institutional arrangement with a sunset. Mechanisms like sectoral bargaining, minimum scheduling standards, portable benefits, and algorithmic transparency are nascent alternatives. Classification as scaffold reflects genuine sunset clause: regulatory frameworks establishing predictability are structurally available and being deployed. Extraction persists only because regulatory capture delays implementation.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL EMPLOYMENT CONTRACT SYSTEM (PITON) — The institutional expectation that employers should absorb demand volatility (full-time permanent employment with stable hours) is degraded but theatrically maintained in legal and social frameworks. Labor law still assumes full-time employment as the norm even as precarity has become structural. The old system persists through inertia — regulatory frameworks reference employment categories that no longer describe reality. Theater ratio reflects the performative invocation of 'employment relationship' standards that have no enforcement mechanism in platform economy.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From an extreme analytical distance, demand volatility is inherent to market economies: consumer preferences and production requirements fluctuate, and someone must absorb the timing risk. From this view, the question is not whether volatility exists but how its costs are distributed. The mountain perspective naturalizes this distribution as inevitable — 'someone has to bear the volatility, and precarious workers are the most efficient absorbers.' However, this naturalizes what is actually a contingent institutional choice: volatility could be absorbed by employers (capital), customers (prices), the state (insurance/stabilization), or workers. The mountain classification is a false summit.
constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(precarious_worker_volatility_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(precarious_worker_volatility_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(precarious_worker_volatility_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(precarious_worker_volatility_extraction, TR),
    TR >= 0.70.

:- end_tests(precarious_worker_volatility_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. Volatility extraction is significant but not total — workers retain some agency in task selection and timing (on platforms), and some segments have begun to organize collectively. The trajectory shows extractiveness increasing as platform models matured (0.35 → 0.62), indicating growing normalization of precarity and reduced alternative employment pathways. Suppression (0.68): High. Multiple reinforcing barriers: lack of savings (prevents refusal of work), geographic immobility (limited local employment options), credential requirements (barriers to higher-wage sectors), legal status (restricting worker protections), and information asymmetry (algorithm-controlled task assignment). Internalized suppression compounds structural suppression: workers accept precarity as 'how the economy works.' Theater ratio (0.55): Moderate-high. Precarity is theatrically framed as flexibility, independence, and entrepreneurship while extraction mechanisms (volatility transfer, wage suppression, lack of benefits) operate structurally. The constraint is partially visible (workers clearly experience income volatility) but partly obscured (attributed to market conditions or individual effort rather than institutional design). Theater increased over interval as platform rhetoric expanded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates acute perspectival divergence across structural positions. The platform employer genuinely experiences the constraint as solving a real coordination problem: demand fluctuates, and transferring volatility to workers is more efficient than maintaining constant excess workforce capacity. From their perspective, this is Rope — coordination with minimal coercion (workers 'choose' to participate). The precarious worker experiences the constraint as Snare: trapped by material necessity into absorbing uncompensated volatility, with suppression that prevents exit or collective action. The worker organizer experiences Tangled Rope: the constraint enables organizing (volatility-driven worker anger creates coalitions) while simultaneously constraining organizing (volatility limits workers' capacity to sustain strikes or collective action). The regulatory reform coalition perceives Scaffold: volatility extraction is a temporary institutional arrangement being systematically dismantled through sectoral bargaining, portable benefits, and scheduling standards. The traditional employment system perceives itself as degraded (Piton): legal employment frameworks still assume stable, full-time work, but these norms are theatrically invoked while systematically violated. The analytical observer risks perceiving Mountain: 'someone must absorb volatility, and precarious workers are the most efficient absorbers' naturalizes what is actually an institutional choice. This perspectival divergence is diagnostic of the constraint's true structure: it is Tangled Rope from the system-wide view (genuine coordination function plus asymmetric extraction), but appears as different types from each agent's local position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position: who benefits from volatility transfer, who bears costs, what exit options exist. Precarious workers (powerless/trapped) experience maximum directionality toward extraction (d ≈ 0.95): they are structurally targeted by volatility transfer with no exit option except withdrawal from labor market. Platform employers (institutional/arbitrage) experience low directionality (d ≈ 0.10): they are net beneficiaries of the coordination value and can exit via regulatory arbitrage. Organizing collectives (moderate/constrained) experience moderate directionality (d ≈ 0.60): they benefit from organizing opportunity but bear coordination costs and face legal barriers. The engine's sigmoid f(d) converts these directionalities into effective extractiveness values: maximum extraction for trapped agents, minimal extraction for beneficiaries with exit options. This produces perspectival divergence: the platform sees Rope (coordination), the precarious worker sees Snare (pure extraction), the organizer sees Tangled Rope (mixed coordination and extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandate trap through recognition that volatility extraction is both a coordination mechanism AND an extraction mechanism. The beneficiaries (platforms) honestly describe their systems as solving real coordination problems (demand matching). The victims (precarious workers) honestly describe their experience as extraction (income instability, lack of security). Both descriptions are accurate. The constraint is Tangled Rope: it genuinely coordinates labor supply to fluctuating demand (real coordination value) while systematically distributing volatility costs to the least-resourced actors (asymmetric extraction). The mandatrophy is resolved by: (1) declaring both beneficiaries and victims, making the extraction asymmetry explicit; (2) including perspectives from both structural positions, showing how the same constraint generates different classifications; (3) identifying sunset mechanisms (portable benefits, sectoral bargaining, scheduling standards) that could shift the constraint toward pure Rope or toward Scaffold as regulatory mechanisms mature. The constraint cannot be defended as 'just coordination' or dismissed as 'just extraction' — it is genuinely both, and the ethical question is how to weight and redistribute the coordination benefits and extraction costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    volatility_absorption_mechanism,
    'Is precarious work structurally necessary for efficient demand matching, or is it an institutional choice by employers to extract rents rather than invest in workforce stabilization?',
    'Comparative analysis of labor market outcomes in jurisdictions with different volatility-absorption regimes: sectoral bargaining (wage-hours stability), algorithmic scheduling transparency mandates, portable benefits systems, and temporary-to-permanent conversion requirements. Identify whether demand matching efficiency is materially better under precarity or whether stability mechanisms achieve comparable efficiency.',
    'If structural necessity: constraint reclassifies toward Rope (legitimate coordination). If institutional choice: constraint remains Tangled Rope or Snare (extractive). Resolves whether volatility extraction is a coordination solution or a rent-extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(volatility_absorption_mechanism, empirical, 'Whether demand volatility absorption is structurally necessary or institutionally chosen').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of precarious workers primarily structural (material barriers: lack of savings, geographic immobility, credential requirements) or internalized (cognitive framing: acceptance of precarity as normal, identity fusion with gig identity, epistemic closure about alternatives)?',
    'Post-exit trajectory analysis: workers who exit precarious employment and gain stable income — do they report that barriers were primarily material, or do they report cognitive shifts (changed self-concept, expanded sense of possibility, recognition of internalized constraints)? Qualitative analysis of organizing narratives: which barriers do workers themselves identify as most binding?',
    'If structural: suppression persists until material barriers are removed (requires policy intervention on savings, mobility, credentials). If internalized: suppression may persist even after barrier removal (requires consciousness-raising and identity shift). If both: requires combined intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    algorithmic_volatility_asymmetry,
    'Do algorithmic task-assignment systems (Uber, DoorDash, Amazon Flex) materially increase demand volatility for workers compared to traditional piece-work systems, or do they merely expose pre-existing volatility?',
    'Time-series analysis of task availability and earned income variance: compare daily/weekly income volatility for workers in platform systems vs traditional piece-work, gig labor, and temp employment. Identify whether platforms mechanically amplify volatility (through algorithm design choices) or whether they transparently reveal volatility that was always present but hidden in traditional wage structures.',
    'If amplified by design: constraint includes a technological extraction mechanism (algorithmic volatility engineering); supports higher extractiveness score. If merely exposed: constraint is redistribution of pre-existing volatility; supports lower extractiveness score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_volatility_asymmetry, empirical, 'Whether algorithms amplify volatility or expose pre-existing volatility').

omega_variable(
    portable_benefits_sufficiency,
    'Do portable benefits systems (stipends that follow workers across employers, sectoral healthcare, wage insurance) materially reduce the extraction experienced by precarious workers, or do they merely reduce its visibility?',
    'Policy simulation and comparative analysis: jurisdictions implementing portable benefits (Nordic sectoral bargaining models, some EU gig-work regulations, proposed US sectoral bargaining) vs. jurisdictions with no portable benefits. Measure: income stability post-benefits, access to healthcare, housing security, ability to absorb shocks. Identify whether extraction reduction is genuine or cosmetic.',
    'If genuine: portable benefits represent real sunset mechanism (scaffold classification confirmed). If cosmetic: benefits become new form of rent-extraction (platform companies provide visible benefits while capturing greater extraction through other means). Affects classification trajectory and timeline estimates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portable_benefits_sufficiency, empirical, 'Whether portable benefits genuinely reduce extraction or are cosmetic').

omega_variable(
    worker_coalition_critical_mass,
    'What threshold of organizing density is required for precarious worker coalitions to shift from Tangled Rope (constrained by coordination costs) to Rope (coordination benefit exceeds extraction)?',
    'Historical analysis of successful worker organizing campaigns: identify organizing density at inflection points (when campaigns shift from organizing toward negotiation, when extraction decreases). Cross-sector comparison: which sectors (logistics, rideshare, food delivery, healthcare) show highest organizing success, and do they have systematically different density thresholds?',
    'Below threshold: extraction persists, coalitions are trapped in losing-coalition dynamics. Above threshold: coalition power exceeds employer power to distribute volatility; constraint reclassifies toward Rope or Scaffold. Establishes whether coalition organizing is a realistic exit mechanism for precarious workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_coalition_critical_mass, empirical, 'Critical organizing density threshold for worker coalition power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(precarious_worker_volatility_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pwve_tr_t0, precarious_worker_volatility_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pwve_tr_t3, precarious_worker_volatility_extraction, theater_ratio, 3, 0.45).
narrative_ontology:measurement(pwve_tr_t6, precarious_worker_volatility_extraction, theater_ratio, 6, 0.52).
narrative_ontology:measurement(pwve_tr_t10, precarious_worker_volatility_extraction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pwve_be_t0, precarious_worker_volatility_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pwve_be_t3, precarious_worker_volatility_extraction, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pwve_be_t6, precarious_worker_volatility_extraction, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(pwve_be_t10, precarious_worker_volatility_extraction, base_extractiveness, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(precarious_worker_volatility_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(precarious_worker_volatility_extraction, 0.18).
narrative_ontology:affects_constraint(precarious_worker_volatility_extraction, income_volatility_insurance_systems).
narrative_ontology:affects_constraint(precarious_worker_volatility_extraction, algorithmic_wage_suppression).
narrative_ontology:affects_constraint(precarious_worker_volatility_extraction, worker_organizing_legal_barriers).
narrative_ontology:affects_constraint(precarious_worker_volatility_extraction, portable_benefits_architecture).

% DUAL FORMULATION NOTE:
% Precarious worker volatility extraction is an upstream constraint affecting multiple downstream constraints in labor market architecture. The constraint family includes: (1) algorithmic wage suppression (how platforms engineer piece-rate structures to extract from volatility transfer), (2) income volatility insurance systems (risk-pooling alternatives to individual absorption), (3) worker organizing legal barriers (how labor law restricts collective action on precarious workers), and (4) portable benefits architecture (how benefits portability would change extraction distribution). Each has its own ε; together they form the precarious-work constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(precarious_worker_volatility_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
