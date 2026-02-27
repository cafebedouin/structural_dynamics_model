% ============================================================================
% CONSTRAINT STORY: rotmigration_decision_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotmigration_decision_threshold, []).

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
 *   constraint_id: rotmigration_decision_threshold
 *   human_readable: The Migration Decision Threshold (Cost-Benefit Equilibrium)
 *   domain: economic/social
 *
 * SUMMARY:
 *   The migration decision threshold represents a critical structural
 *   constraint in global labor markets where the expected utility of moving
 *   across borders exceeds the costs and risks of remaining. This constraint
 *   exhibits tangled coordination-extraction architecture: genuine labor
 *   market matching (coordination) layered onto asymmetric information, debt
 *   bondage, and exploitation mechanisms (extraction). The threshold itself
 *   is not a fixed economic fact but an engineered equilibrium maintained
 *   through suppression mechanisms including origin-country exit barriers,
 *   destination-country legal precarity, debt systems, and migration
 *   intermediary control. The constraint has intensified over the measurement
 *   interval as extraction mechanisms have become more sophisticated (labor
 *   trafficking networks, micro-debt systems, document confiscation) while
 *   the underlying labor market coordination problem remains chronic. The
 *   theater ratio reflects that formal migration governance (visa systems,
 *   border enforcement, skills-matching rhetoric) performs regulatory
 *   legitimacy while informal migration systems function independently,
 *   suggesting institutional degradation (Piton characteristics). However,
 *   the constraint's core mechanics remain functional (extracting real value
 *   from migrant labor), preventing full Piton classification. The constraint
 *   simultaneously appears as pure extraction to vulnerable migrants (Snare),
 *   mixed coordination-extraction to intermediaries and communities (Tangled
 *   Rope), pure coordination to destination labor markets (Rope), a temporary
 *   development problem to governance coalitions (Scaffold), and
 *   institutional theater to origin states (Piton).
 *
 * KEY AGENTS:
 *   - Vulnerable Migrants: Primary victims (powerless/trapped) — bear maximum extraction through debt, precarity, exploitation; cannot exit without catastrophic loss
 *   - Origin Communities: Secondary victims/mixed (moderate/constrained) — lose working-age population and social cohesion but receive remittances; constrained by local economic structure
 *   - Destination Labor Markets: Primary beneficiaries (institutional/arbitrage) — capture wage suppression and labor supply elasticity; experience migration as solved coordination problem
 *   - Migration Intermediaries: Organized extractors (organized/constrained) — provide genuine service (information, transport) while capturing surplus through fees and debt; maintain network through barriers
 *   - Origin States: Institutional actors (institutional/arbitrage) — perform border sovereignty while extraction mechanisms function independently; maintain apparatus through inertia
 *   - International Governance Coalition: Powerful agents (powerful/mobile) — frame problem as temporary coordination failure solvable through development and legal pathways; have agency to shape sunset timeline
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — measures structural classification independent of normative framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotmigration_decision_threshold, 0.52).
domain_priors:suppression_score(rotmigration_decision_threshold, 0.68).
domain_priors:theater_ratio(rotmigration_decision_threshold, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotmigration_decision_threshold, extractiveness, 0.52).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotmigration_decision_threshold, tangled_rope).
narrative_ontology:human_readable(rotmigration_decision_threshold, "The Migration Decision Threshold (Cost-Benefit Equilibrium)").
narrative_ontology:topic_domain(rotmigration_decision_threshold, "economic/social").

domain_priors:requires_active_enforcement(rotmigration_decision_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, destination_labor_markets).
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, remittance_recipient_households).
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, migration_intermediaries).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, origin_labor_supply).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, origin_community_cohesion).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, vulnerable_migrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE MIGRANT (SNARE) — Trapped by limited local opportunity, high debt for passage, and legal precarity in destination. Cannot exit without catastrophic loss. d≈0.93, f(d)≈1.40, σ=0.8 → χ≈0.58. Extraction mechanism: debt bondage, labor exploitation, document confiscation.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORIGIN COMMUNITY (TANGLED ROPE) — Benefits from remittances but suffers loss of working-age population, social fragmentation, and dependency on external income. Constrained by inability to create sufficient local opportunity. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.43. Mixed: coordination (remittance pipeline) + extraction (demographic drain).
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DESTINATION LABOR MARKET (ROPE) — Benefits from labor supply elasticity and wage suppression in low-skill sectors. Experiences migration as coordination: labor shortages solved through international recruitment. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; extraction is coordinated gain.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MIGRATION INTERMEDIARY NETWORK (TANGLED ROPE) — Organized agents (smugglers, labor brokers, recruiters) provide genuine coordination service (information, transport, job placement) while extracting via fees, debt, and information asymmetry. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.17. Active enforcement required: networks maintain exclusivity through violence or legal barriers.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL MIGRATION GOVERNANCE COALITION (SCAFFOLD) — Powerful actors (UN agencies, development banks, labor standards bodies, destination governments) frame the migration threshold as temporary coordination problem solvable through legal pathways, skills matching, and remittance efficiency. d≈0.25, f(d)≈0.18, σ=1.2 → χ≈0.11. Low extraction because coalition has agency and sunset vision: once origin countries develop, migration pressure declines. Sunset rationale: development assistance, circular migration, and skills-based legal pathways will reduce undocumented migration within 15-25 years.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ORIGIN STATE BORDER APPARATUS (PITON) — Maintains exit restrictions, passport controls, and emigration taxes through institutional inertia. The apparatus performs sovereignty (border control theater) while extraction mechanisms function regardless. theater_ratio=0.58 reflects that formal legal barriers are partially performative — informal migration persists despite restrictions. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.06. Piton because the apparatus is degraded: controls are porous, legitimacy is contested, yet maintained through bureaucratic momentum.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the migration threshold represents a genuine coordination problem (labor supply matching) embedded in asymmetric extraction (debt, precarity, wage suppression). The structural data (ε=0.52, suppression=0.68, theater=0.58) confirms tangled rope: moderate extraction with real coordination function. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.70. This is the engine's computed constraint_claim — not a false summit.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotmigration_decision_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotmigration_decision_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotmigration_decision_threshold, TR),
    TR >= 0.70.

:- end_tests(rotmigration_decision_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value from migrant labor through multiple mechanisms: debt bondage (5-30% wage deduction typical), wage suppression in destination labor markets (10-25% below local workers), lost human capital in origin communities, and intermediary surplus capture. The extraction is real but not maximal — migrants do experience income gains (often 2-5x origin wages) and families receive substantial remittances. The measurement reflects net extraction after accounting for gross migrant benefit. Suppression (0.68): High. Multiple mechanisms enforce the constraint: origin-country exit barriers (passport controls, emigration taxes, skill restrictions), destination-country legal precarity (visa dependence, contract labor), debt systems (travel costs typically 3-12 months wages), document confiscation by employers, and migration intermediary violence or legal monopolies. Physical barriers exist but are less important than economic and legal ones. Theater ratio (0.58): Moderate. Formal migration governance (visa systems, skills-matching rhetoric, border enforcement narratives) performs regulatory legitimacy while informal migration systems function with different extraction logic. The formal system is partially degraded — many controls are porous and informal — yet maintained through bureaucratic momentum and political theater around border sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence based on structural position. The vulnerable migrant sees a Snare — forced by desperation into debt bondage with no exit option. The origin community sees Tangled Rope — receiving remittances (coordination benefit) while losing population and social capacity (extraction cost). The destination labor market sees Rope — genuine coordination solving chronic labor shortages without recognizing suppression of local wages. The intermediary network sees Tangled Rope — providing service (information, transport) while capturing extraction surplus. The governance coalition sees Scaffold — a temporary development problem with a sunset as origin countries develop. The origin state sees performative Piton — sovereignty theater with degraded functional control. The analytical observer sees Tangled Rope with high confidence — both coordination (labor matching) and extraction (debt, precarity, wage suppression) are structurally real. The gap between beneficiary (destination labor market) and victim (vulnerable migrant) perspectives is maximal: one experiences voluntary coordination; the other experiences forced extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable migrants: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction directionality. Cannot exit without catastrophic loss; bears full suppression burden. Origin communities: Mixed (victim of labor drain + beneficiary of remittances) + constrained → d≈0.62, f(d)≈0.82. Asymmetric: lose young adults, gain income stream. Destination labor markets: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with low cost. Can source labor from multiple origins; exit costs low. Migration intermediaries: Mixed (provide service + extract) + constrained → d≈0.35, f(d)≈0.32. Moderate extraction. Have some agency (operate networks) but constrained by law enforcement and competition. Origin states: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Beneficiary through remittance tax and reduced unemployment pressure; arbitrage option (can restrict migration). Governance coalition: Powerful + mobile → d≈0.25, f(d)≈0.18. Low extraction directionality; coalition has agency and alternative pathways. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Observer perspective shows both extraction and coordination; tangled rope is the correct classification.
 *
 * MANDATROPHY ANALYSIS:
 *   CORE MANDATROPHY QUESTION: Is the migration decision threshold a voluntary labor market coordination mechanism or a forced extraction mechanism? Structural data resolves this as Tangled Rope, not either pure type. The mandatrophy manifests as: (1) ORIGIN COMMUNITY LEVEL: Are remittances genuine development benefit or extraction lock-in? Early evidence (first 20 years) shows lock-in dominates — communities become dependent on external income while human capital in origin declines. After 20+ years, second-generation effects and reverse migration can enable local development, suggesting Scaffold sunset logic. (2) VULNERABLE MIGRANT LEVEL: Is migration a rational choice or desperation-driven trap? Survey data shows migrants have positive ex-ante expectations but experience ex-post precarity due to information asymmetry and debt. This is tangled rope signature: sold as coordination (job opportunity) but functions as extraction (debt bondage). (3) DESTINATION LABOR MARKET LEVEL: Does migration fill genuine labor gap or depress local wages? Evidence shows both: migrants fill jobs locals refuse (coordination) while simultaneously depressing wages for low-skill workers (extraction). Magnitude of wage depression (10-25% for competing workers) is significant enough to justify calling the constraint extractive despite coordination benefits. (4) INTERMEDIARY NETWORK LEVEL: Do brokers solve information asymmetry (rope) or exploit it (snare)? Answer: both simultaneously. Information provision is real; fees and debt systems are extraction mechanisms. The network cannot exist without both. RESOLUTION: The constraint is Tangled Rope from analytical view because it solves a genuine coordination problem (labor supply matching) while maintaining systemic extraction through multiple mechanisms (debt, precarity, information asymmetry, destination-country legal barriers). It is not a false summit — the coordination function is real. It is not a false Snare — the extraction mechanisms are systematic and high-magnitude. The tension between these is the constraint's core structure, not an artifact of perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_uncertainty,
    'What income differential and time-to-payback constitute the actual decision threshold for migration vs. remaining?',
    'Household survey data on expectations vs. outcomes; micro-econometric estimation of migration decisions conditional on perceived wage gaps, travel costs, and family separation costs',
    'If threshold is driven primarily by income gaps: constraint classifies as coordination problem with supplementary extraction (Rope+). If threshold is driven by debt obligation and fear of destitution: constraint is pure extraction (Snare from below). Current modeling suggests mixed, supporting Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_uncertainty, empirical, 'Calibration of the migration decision threshold').

omega_variable(
    remittance_dependency_lock_in,
    'Do remittance flows create permanent economic lock-in that prevents local development, or do they enable human capital accumulation that eventually facilitates local productivity?',
    'Longitudinal studies of remittance-recipient communities; analysis of second-generation outcomes; correlation between remittance intensity and subsequent local wage growth or stagnation',
    'If lock-in: origin community is victim in permanent extraction (Snare for communities). If enablement: origin community benefits from temporary coordination (Scaffold timing becomes crucial). Current data suggests lock-in dominates first 20 years.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_dependency_lock_in, empirical, 'Whether remittances enable or inhibit local development').

omega_variable(
    legal_pathway_substitution,
    'Do expanded legal migration pathways (work visas, guest worker programs) reduce debt-driven informal migration, or do they create parallel extraction systems with state enforcement?',
    'Comparative analysis of jurisdictions with generous vs. restrictive legal pathways; measurement of informal migration intensity; debt levels in legal vs. undocumented migrant populations',
    'If legal pathways reduce extraction: scaffold perspective is correct and development can follow. If legal pathways create state-enforced extraction (visa sponsorship debt, contract labor): extraction mechanism shifts from private to public, potentially increasing suppression. Early evidence suggests mixed outcomes by destination country.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_pathway_substitution, empirical, 'Whether legal pathways substitute for or supplement informal migration extraction').

omega_variable(
    origin_labor_market_counterfactual,
    'What would origin-country labor markets look like in the absence of migration option — would wages rise (elasticity effect) or would unemployment and precarity deepen (demand shock)?',
    'Quasi-experimental analysis of regions with sudden migration restrictions; synthetic control matching for changes in local wage and employment distributions; structural economic modeling of labor market feedback',
    'If wages rise: current migration is voluntary (Rope logic). If unemployment deepens: migration is escape from worse alternative, not choice (Snare logic). This is the core mandatrophy question for the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(origin_labor_market_counterfactual, empirical, 'Counterfactual labor market outcomes absent migration option').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotmigration_decision_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(migr_tr_t0, rotmigration_decision_threshold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(migr_tr_t5, rotmigration_decision_threshold, theater_ratio, 5, 0.48).
narrative_ontology:measurement(migr_tr_t10, rotmigration_decision_threshold, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(migr_be_t0, rotmigration_decision_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(migr_be_t5, rotmigration_decision_threshold, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(migr_be_t10, rotmigration_decision_threshold, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotmigration_decision_threshold, resource_allocation).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, origin_labor_market_stagnation).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, destination_wage_suppression).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, debt_dependency_lock_in).

% DUAL FORMULATION NOTE:
% The migration decision threshold can be decomposed into three structurally distinct constraints with different ε values: (1) origin labor market stagnation (ε≈0.35, Rope: coordination problem of job creation) — upstream cause; (2) migration decision threshold (ε≈0.52, Tangled Rope: mixed coordination-extraction) — this story; (3) destination wage suppression (ε≈0.62, Snare: extraction mechanism) — downstream effect. The three are linked by causal dependency: origin stagnation creates motivation for migration; migration decision threshold determines flow; destination suppression is the extraction consequence. Each has distinct structural properties and empirical status. This story is downstream of origin stagnation, upstream of wage suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotmigration_decision_threshold, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
