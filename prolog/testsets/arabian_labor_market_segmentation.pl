% ============================================================================
% CONSTRAINT STORY: arabian_labor_market_segmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arabian_labor_market_segmentation, []).

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
 *   constraint_id: arabian_labor_market_segmentation
 *   human_readable: Arabian Labor Market Segmentation: Kafala System and Migrant Worker Extraction
 *   domain: labor_economics/political_economy
 *
 * SUMMARY:
 *   The Arabian labor market, particularly in Gulf Cooperation Council
 *   states, is structured through the kafala (sponsorship) system—a labor
 *   governance regime that ties migrant workers' legal status, employment,
 *   and mobility rights to individual employers. This creates extreme power
 *   asymmetry, enabling systematic extraction of labor value through wage
 *   suppression, unsafe conditions, movement restrictions, and debt bondage.
 *   The constraint exhibits Snare characteristics for the trapped workers
 *   (powerless, biographical horizon, no exit), Tangled Rope for informal
 *   workers (moderate power, constrained exit), Rope for employers and
 *   receiving-state governments (institutional power, arbitrage options), and
 *   Piton for labor brokers maintaining the system through institutional
 *   inertia. The system has persisted and intensified over the past two
 *   decades despite formal international commitments to labor standards.
 *   Extractiveness has increased from 0.52 to 0.68 over the measurement
 *   interval, driven by growing remittance dependency in sending countries
 *   and capital intensity in receiving states. Theater has risen from 0.35 to
 *   0.58 as governments implement formal reforms (wage protection boards,
 *   labor inspectorates, mobility provisions) while enforcement capacity
 *   remains low and de facto extraction continues.
 *
 * KEY AGENTS:
 *   - Migrant Workers: Primary victims (powerless/trapped) — structurally dependent on employer sponsorship; no legal recourse; subject to wage theft, unsafe conditions, movement restrictions
 *   - Domestic Workers: Deep-structure victims (powerless/trapped) — excluded from formal labor protections; confined to private spaces; extreme vulnerability to abuse and exploitation
 *   - Gulf Employers: Primary beneficiaries (institutional/arbitrage) — access cheap, controllable labor; predictable workforce; arbitrage across labor-sending countries
 *   - Receiving State Governments: Secondary beneficiaries (institutional/arbitrage) — control labor inflows; stabilize construction and service sectors; reduce welfare obligations; maintain political stability through migration control
 *   - Labor Brokers: Tertiary beneficiaries (powerful/constrained) — extract fees from both workers and employers; maintain system through institutional embeddedness; face international pressure but maintain dominance
 *   - Sending Country Governments: Trapped beneficiaries (organized/identity_locked) — depend on remittances for development funding; cannot effectively regulate worker protection without risking income flows; locked into system through economic necessity
 *   - Informal Workers: Secondary victims (moderate/constrained) — outside formal kafala but experience similar extraction through wage suppression, no benefits, high accident rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arabian_labor_market_segmentation, 0.68).
domain_priors:suppression_score(arabian_labor_market_segmentation, 0.72).
domain_priors:theater_ratio(arabian_labor_market_segmentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arabian_labor_market_segmentation, extractiveness, 0.68).
narrative_ontology:constraint_metric(arabian_labor_market_segmentation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(arabian_labor_market_segmentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arabian_labor_market_segmentation, snare).
narrative_ontology:human_readable(arabian_labor_market_segmentation, "Arabian Labor Market Segmentation: Kafala System and Migrant Worker Extraction").
narrative_ontology:topic_domain(arabian_labor_market_segmentation, "labor_economics/political_economy").

domain_priors:requires_active_enforcement(arabian_labor_market_segmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arabian_labor_market_segmentation, gulf_employers).
narrative_ontology:constraint_beneficiary(arabian_labor_market_segmentation, receiving_state_governments).
narrative_ontology:constraint_beneficiary(arabian_labor_market_segmentation, labor_brokers).
narrative_ontology:constraint_victim(arabian_labor_market_segmentation, migrant_workers).
narrative_ontology:constraint_victim(arabian_labor_market_segmentation, domestic_workers).
narrative_ontology:constraint_victim(arabian_labor_market_segmentation, informal_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MIGRANT WORKER (SNARE) — Structurally trapped by employment sponsorship system (kafala). Exit requires employer consent; contract enforcement is asymmetric; wage theft, unsafe conditions, and movement restrictions are endemic. Suppression is total: legal status depends entirely on employer's sponsorship; exit would mean deportation, debt accumulation, and permanent blacklist.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC WORKER (SNARE) — Even more deeply trapped than formal migrant workers. Operate outside formal labor protections; confined to employer's private space; severe isolation from community; extreme vulnerability to abuse. Suppression approaches totality: no legal recourse, complete economic and spatial control by employer, cultural and legal exclusion from worker protections.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INFORMAL SECTOR PARTICIPANT (TANGLED ROPE) — Construction laborers, service workers outside kafala. Experience genuine coordination: work gets done, income flows reach workers, construction projects complete. But also experience significant extraction: wage suppression through undocumented status, no benefits, high accident rates, no insurance. Can exit at cost (relocation, forgone income) but not at zero cost.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYER (ROPE) — Experiences the kafala system as a coordination mechanism: labor supply is stable, workers are controllable, project timelines are predictable. Genuine coordination function exists alongside extraction. Employer has arbitrage options (can recruit from multiple labor-sending countries, can use formal vs informal channels) and can exit by relocating operations or changing labor strategies.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RECEIVING STATE GOVERNMENT (ROPE) — Benefits from kafala through labor supply control, remittance taxation (implicit through labor market suppression), political stability (migrant workers cannot organize), and reduced welfare obligations. Experiences the system as coordination: managing labor inflows, controlling urban growth, stabilizing construction sector. Has arbitrage options: can reform kafala, can shift to automation, can reduce migrant population through policy.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LABOR BROKER NETWORK (PITON) — Maintains kafala through institutional inertia despite formal reform attempts. Theater_ratio high: governments announce reforms while brokers circumvent them; regulatory bodies exist but lack enforcement capacity; labor trafficking is formally illegal yet remains endemic. Brokers benefit from system but face increasing international pressure and reputation costs. Primarily maintains dominance through institutional embeddedness rather than active value creation.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: SENDING COUNTRY GOVERNMENT (SNARE) — Depends on remittances (20-50% of GDP for some nations) flowing from Arabian labor markets. Trapped by economic necessity: workers send money home, governments collect taxes/fees on remittances, development projects depend on this flow. High extraction: workers bear the cost of unsafe conditions and wage suppression; sending countries' governments cannot effectively regulate or protect workers without risking remittance flows. Limited exit options due to economic dependency.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Risks naturalizing kafala as an inherent feature of labor market development or regional demographic needs. The framing 'Gulf states need migrant workers; workers need income; kafala is simply how this is organized' naturalizes a contingent extractive institutional choice as a necessity. This perspective will be flagged as a false summit by the engine: the structural data shows active enforcement, high suppression, concentrated beneficiaries, and trapped victims — not natural law.
constraint_indexing:constraint_classification(arabian_labor_market_segmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arabian_labor_market_segmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arabian_labor_market_segmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arabian_labor_market_segmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arabian_labor_market_segmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arabian_labor_market_segmentation, TR),
    TR >= 0.70.

:- end_tests(arabian_labor_market_segmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The kafala system produces sustained extraction through multiple mechanisms: (1) wage suppression via oversupply and legal status dependency (workers accept below-market wages to maintain sponsorship); (2) non-payment and debt bondage (brokers and employers use recruitment debt as control mechanism); (3) movement restrictions reduce exit options and worker bargaining power; (4) working condition degradation (unsafe work, excessive hours) is enabled by powerlessness. The 0.68 value reflects that extraction is severe and systematic but not absolute — some workers do accumulate savings, some wage increases occur, and some workers successfully exit (at high cost). Suppression (0.72): Very high. Legal status is controlled by employers; workers cannot organize (strikes are illegal); cannot change employers without sponsorship transfer (often denied); cannot access courts effectively; deportation threat is existential. Suppression is structural and enforced by both state apparatus (immigration law, labor ministry with limited capacity) and market actors (brokers, employers). Theater ratio (0.58): Moderate-high. Governments announce reforms (wage boards, labor inspectorates) but enforcement is weak. Brokers operate nominally under formal regulations while circumventing them. Trafficking is formally prohibited but endemic. The theater has increased as international pressure (UN conventions, NGO reporting, media scrutiny) has forced governments to adopt formal compliance structures while actual extraction patterns persist.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between victim and beneficiary perspectives is near-maximal. Workers classify the constraint as Snare (pure extraction, no coordination benefit, survival-level suppression). Employers classify as Rope (genuine coordination: labor supply stability, project completion, predictability). Receiving governments classify as Rope (labor management, economic growth, political stability). This perspectival gap is not a measurement error — it reflects that the kafala system's coordination function for capital and employers is built on the extraction of labor from workers. One agent's coordination benefit is another's extraction cost. The constraint is functionally extractive for workers (Snare) and coordinative for employers (Rope) simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range across the full spectrum reflecting the power asymmetry. Migrant workers: d ≈ 0.95 (trapped, powerless, full victims). Employers: d ≈ 0.05 (institutional power, arbitrage exit, beneficiaries). Receiving state governments: d ≈ 0.10 (institutional power, arbitrage options, net beneficiaries). Sending country governments: d ≈ 0.70 (organized power but constrained by remittance dependency; both beneficiaries and victims). The sigmoid f(d) scales these to f(d) values from 1.42 (workers) to -0.12 (employers). Effective extraction χ = ε × f(d) × σ(S) shows workers experiencing χ ≈ 0.97 (high extraction), employers experiencing χ ≈ -0.08 (subsidy), and national-scope regional actors experiencing intermediate values. The massive chi gap is diagnostic: when the same constraint produces near-opposite effective extractiveness for different agents, the distributional asymmetry is structural, not noise.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that a single structural phenomenon can be simultaneously Snare and Rope depending on perspective. This is not a classification error. The kafala system genuinely solves a coordination problem for employers and receiving-state governments (labor supply, project management, political control). It genuinely extracts from workers (wages below market value, working conditions degraded, mobility eliminated, legal status weaponized). Both truths are structural. The mandatrophy resolution: the constraint is a Snare-that-appears-as-Rope to those who benefit from it. The engine's perspectival classification captures this: the beneficiary sees Rope; the victim sees Snare. Neither is wrong. The system is a Snare because it systematically benefits some by extracting from others — that's the definition of a Snare with coordination function elements. It appears as Rope to beneficiaries because, from their position, the extraction is invisible (it looks like labor is simply cheaper in the global market). The analytical observer must see both truths simultaneously: the system works (for employers); the system extracts (from workers). Naturalizing either view alone produces false summits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kafala_reform_effectiveness,
    'Do recent kafala reforms (wage protection boards, labor mobility improvements) represent genuine structural change or are they theater masking continued extraction?',
    'Empirical tracking: wage compliance rates post-reform, worker exit/mobility rates, accident/injury rates, actual vs declared enforcement actions by labor ministry',
    'If reforms effective: classification shifts toward Tangled Rope or Rope across more perspectives, extraction coefficients drop. If theaters: classification remains Snare, theater_ratio increases over time as government spending on enforcement theater rises without outcome changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kafala_reform_effectiveness, empirical, 'Whether kafala reforms reduce extraction or are performative').

omega_variable(
    worker_organizing_capacity,
    'Can migrant workers in Gulf labor markets organize collectively despite legal restrictions, or is the suppression total?',
    'Documentation of strikes, informal collectives, mutual aid networks, cross-border organizing (via diaspora networks, sending-country unions); assessment of whether organizing has produced measurable outcome changes (wage increases, safety improvements, contract enforcement)',
    'If organizing produces outcomes: power_atom for migrant workers upgrades from powerless to organized, classification shifts from Snare to Tangled Rope or Rope. If organizing is suppressed: power remains powerless, Snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_organizing_capacity, empirical, 'Whether migrant workers can organize collectively despite restrictions').

omega_variable(
    alternative_labor_models_feasibility,
    'Can Gulf states shift to alternative labor coordination models (automation, higher-skilled temporary migration, expanded local workforce participation) that would reduce kafala dependency?',
    'Technical feasibility analysis (automation ROI in construction/service sectors), policy analysis (local hiring initiatives, visa diversification), economic modeling (labor cost impact of alternatives)',
    'If alternatives feasible: kafala represents institutional choice (Snare/Tangled Rope, not Mountain). Receiving states have genuine exit options. If alternatives infeasible: kafala may approach Natural Law (structural necessity of labor arbitrage system for Gulf development model).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_labor_models_feasibility, empirical, 'Whether alternative labor models are technically and economically feasible').

omega_variable(
    remittance_dependency_lock,
    'Are sending-country governments locked into accepting kafala extraction because of remittance economic dependency (Snare), or do they have genuine exit options?',
    'Counterfactual analysis: development indicators in sending countries with low vs high remittance dependency; tracking of whether sending countries that diversified economies reduced worker migration; assessment of actual regulatory capacity on kafala violations',
    'If locked: sending countries should be classified as Snare victims with identity_locked exit (trapped by economic necessity). If mobile: sending countries are Constrained, not trapped. Shifts the directionality calculus for who bears which costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_dependency_lock, empirical, 'Whether sending countries have exit options from remittance dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arabian_labor_market_segmentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alms_tr_t0, arabian_labor_market_segmentation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(alms_tr_t5, arabian_labor_market_segmentation, theater_ratio, 5, 0.48).
narrative_ontology:measurement(alms_tr_t10, arabian_labor_market_segmentation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(alms_be_t0, arabian_labor_market_segmentation, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(alms_be_t5, arabian_labor_market_segmentation, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(alms_be_t10, arabian_labor_market_segmentation, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arabian_labor_market_segmentation, resource_allocation).
narrative_ontology:affects_constraint(arabian_labor_market_segmentation, gulf_remittance_dependency).
narrative_ontology:affects_constraint(arabian_labor_market_segmentation, construction_sector_cost_structure).
narrative_ontology:affects_constraint(arabian_labor_market_segmentation, sending_country_development_model).

% DUAL FORMULATION NOTE:
% The kafala system decomposes into multiple constraint stories with different ε values: (1) kafala_legal_framework (ε=0.05, Mountain) — the formal sponsorship structure is written into law, immutable from within legal systems of receiving states; (2) kafala_enforcement_practice (ε=0.72, Snare) — the operational extraction mechanism dependent on low enforcement and worker desperation; (3) kafala_alternative_coordination (ε=0.15, Rope) — the coordination function (matching labor supply to employer demand) could theoretically be performed via alternative mechanisms. This story addresses the enforcement-practice level (highest ε, Snare from worker perspective). The upstream legal framework story would show Mountain from receiving-state governance perspective; downstream alternative-coordination story would show that technology and formal restructuring could replace kafala.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(arabian_labor_market_segmentation, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
