% ============================================================================
% CONSTRAINT STORY: kidney_exchange_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kidney_exchange_market, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kidney_exchange_market
 *   human_readable: Kidney Exchange Cycles and Chains
 *   domain: social/technological/biological
 *
 * SUMMARY:
 *   Kidney exchange cycles and chains solve the critical biological
 *   incompatibility problem: a patient with end-stage renal disease has a
 *   willing donor (spouse, family, friend) whose blood type or HLA crossmatch
 *   is incompatible with the recipient. In the absence of exchange, the
 *   willing donor cannot help their intended recipient, and both face the
 *   shortage of deceased donor organs on the national waitlist. Kidney
 *   exchange creates a coordination mechanism where incompatible pairs are
 *   matched with other incompatible pairs, enabling mutually beneficial
 *   swaps. An altruistic (non-directed) donor can initiate a chain that
 *   begins with an incompatible pair and continues through multiple
 *   exchanges, maximizing transplants from a single altruistic donation. This
 *   constraint demonstrates how low-extraction coordination mechanisms solve
 *   acute biological scarcity problems while managing fairness between paired
 *   and unpaired patients competing for organs.
 *
 * KEY AGENTS:
 *   - Incompatible Donor-Patient Pairs: Primary beneficiary (moderate/constrained) — face death without exchange; gain life-extending transplant through coordination
 *   - Waitlist Patients Without Donors: Secondary agent (powerless/trapped) — waiting for deceased donor kidney; may experience extraction if exchange pairs receive prioritized allocation
 *   - Transplant Centers: Institutional beneficiary (powerful/arbitrage) — gain surgical volume and reputation; operate exchange infrastructure
 *   - Organ Procurement Organizations: Institutional coordinator (institutional/arbitrage) — manage deceased and living donor allocation; coordinate multi-center exchanges
 *   - Altruistic Donors: Organized beneficiary (organized/constrained) — enable chain initiation; provide coordination catalyst through non-directed donation
 *   - Analytical Observer: Sees coordination mechanism with emerging allocation tensions (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kidney_exchange_market, 0.28).
domain_priors:suppression_score(kidney_exchange_market, 0.35).
domain_priors:theater_ratio(kidney_exchange_market, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kidney_exchange_market, extractiveness, 0.28).
narrative_ontology:constraint_metric(kidney_exchange_market, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kidney_exchange_market, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kidney_exchange_market, rope).
narrative_ontology:human_readable(kidney_exchange_market, "Kidney Exchange Cycles and Chains").
narrative_ontology:topic_domain(kidney_exchange_market, "social/technological/biological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kidney_exchange_market, incompatible_patient_donor_pairs).
narrative_ontology:constraint_beneficiary(kidney_exchange_market, list_waitlist_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOMPATIBLE DONOR-PATIENT PAIR (ROPE) — Faces life-threatening kidney failure with willing donor whose blood type or crossmatch is incompatible. No direct exit option (death without transplant is the alternative). Experiences kidney exchange as pure coordination mechanism solving a collective action problem: their incompatibility becomes solvable when linked to other incompatible pairs. No extraction—only enabling of mutual benefit.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSPLANT CENTER NETWORK (ROPE) — Institutional beneficiary with arbitrage options (can operate traditional kidney transplant programs, living donor programs, or deceased donor allocation). Experiences kidney exchange as coordination infrastructure that expands their surgical capacity and patient access without coercive overhead. Network effects create value: larger pools enable longer chains and better HLA matches. Benefits from algorithmic optimization and matching logistics, but extraction is minimal—centers gain reputation and case volume, not monopoly rents.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: WAITLIST PATIENT NOT IN EXCHANGE (TANGLED ROPE) — Waiting for deceased donor kidney; incompatible pairs in exchange systems may have better match probabilities and shorter waits than traditional waitlist. Experiences both coordination benefit (exchange system increases total transplants available) and extractive asymmetry (deceased donor kidneys may be preferentially allocated to exchange participants, creating two-tier system). Trapped—no exit option; constrained by organ scarcity and cannot join exchange without compatible donor. Moderate extraction due to structural prioritization of pairs over unpaired waitlist patients.
constraint_indexing:constraint_classification(kidney_exchange_market, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: ORGAN PROCUREMENT ORGANIZATION (OPO) — Manages allocation of deceased donor organs and coordinates living donor exchanges. Experiences kidney exchange as coordination mechanism that increases total transplant volume and donor utilization efficiency. Has arbitrage options (can prioritize deceased donors for exchange or traditional waitlist). Benefits from exchange participation without bearing suppression costs—no coercion required, only algorithmic matching and logistics.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTRUISTIC DONOR COALITION (SCAFFOLD) — Donors motivated by health equity and enabling access. Organized participation in chains and pools. Temporary coordination feature: altruistic donor participation creates 'chain starters' that unlock longer sequences. Sees exchange as temporary coordination scaffold because altruistic participation may decline if commodification concerns or matching inefficiencies emerge. Has sunset logic: if genetic typing and cross-platform matching improve sufficiently, or if directed altruistic donation becomes more formalized, this coalition's role may transition.
constraint_indexing:constraint_classification(kidney_exchange_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational and global scope, kidney exchange represents a coordination solution to the allocation problem of scarce incompatible organs. Base extraction is low (ε=0.28) because the mechanism is optionally entered and generates mutual gain with minimal coercive overhead. Suppression (0.35) reflects real barriers: incompatible pairs have limited alternatives (death without exchange), but the constraint solves rather than creates scarcity. Theater ratio (0.42) reflects moderate performative elements: matching algorithms are publicly reviewed, but some centers may emphasize exchange participation for reputation rather than patient benefit.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kidney_exchange_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kidney_exchange_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(kidney_exchange_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The base mechanism is optionally entered and creates mutual gain. Incompatible pairs gain access to transplantation they would not otherwise have. Transplant centers gain case volume and reputation without monopoly power—multiple centers can participate and benefit. The extractiveness is not zero because: (1) paired patients may gain prioritization over unpaired waitlist patients for deceased donor organs, creating two-tier access; (2) larger transplant centers with more resources may have algorithmic advantage in matching and chain optimization; (3) reputational benefits may concentrate among prestigious centers. Suppression (0.35): Moderate. Incompatible pairs have limited practical alternatives (death or extended waitlist). However, suppression is not high because: (1) exchange is genuinely voluntary—no coercion is used; (2) traditional transplant pathways still exist (paired exchange, altruistic donor directed donation, waitlist); (3) constraints are biological (incompatibility, organ scarcity) not imposed by the coordination mechanism itself. Theater ratio (0.42): Moderate. Matching algorithms are publicly reviewed and validated, but some centers may emphasize exchange participation for reputation or publicity rather than patient benefit. The performative element has increased over time as exchange programs have expanded nationally, with centers competing on exchange volume as a quality metric.
 *
 * PERSPECTIVAL GAP:
 *   Incompatible pairs and transplant centers both see kidney exchange as pure coordination (Rope) because it creates mutual benefit with low coercive overhead. Waitlist patients see tangled rope or incipient snare because exchange may extract resources (deceased donor priority) from their access. Altruistic donors see scaffold because their participation is temporary and sunset-dependent on sustainability of motivation. The analytical observer sees rope because the base mechanism is coordination-driven, but notes emerging tensions around allocation fairness and center-level gaming that could degrade the constraint toward tangled rope or snare if not managed.
 *
 * DIRECTIONALITY LOGIC:
 *   Incompatible pairs (moderate/constrained/immediate) derive d from their constraint status: they are victims of incompatibility but beneficiaries of exchange, placing them in the middle of the directionality spectrum. Transplant centers (powerful/arbitrage) derive low d because they have exit options and benefit from exchange participation without bearing suppression costs. Waitlist patients (powerless/trapped) derive high d because they have no exit option and may experience extraction through prioritization of paired patients. The analytical perspective (analytical/analytical) sees the constraint as genuine coordination with emerging extractive distortions—d reflects the balance between mutual benefit and fairness concerns.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination from extraction based on structural incentives. Kidney exchange would be pure Snare if it were coercive, monopolistic, or extracted rents from paired patients. It is Rope because: (1) participation is voluntary; (2) both parties to each swap benefit; (3) the mechanism solves a collective action problem (incompatibility) without creating the scarcity it addresses; (4) suppression is low—constraints are biological, not institutional. The emerging tension with waitlist patients creates the potential for tangled rope classification if deceased donor prioritization becomes systematically extractive. The scaffold perspective for altruistic donors correctly identifies the sunset logic: if altruistic participation declines or if matching efficiency improves with better typing technology, the chain-starting role becomes less critical and the coordination value shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exchange_vs_deceased_donor_prioritization,
    'Does prioritizing incompatible exchange pairs over unpaired waitlist patients for deceased donor organs constitute extractive allocation, or legitimate coordination incentive?',
    'Longitudinal outcome data: waitlist time and graft survival for paired patients in exchange vs unpaired patients on traditional waitlist; allocation policy analysis across OPO regions',
    'If exchange priority is justified by better outcomes: rope classification confirmed. If outcomes are equivalent but allocation follows politics: tangled_rope classification for waitlist patients strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exchange_vs_deceased_donor_prioritization, empirical, 'Whether exchange prioritization is medically justified or politically driven').

omega_variable(
    altruistic_donor_sustainability,
    'Can altruistic donor participation in chains be sustained without commodification risk or burnout dynamics?',
    'Longitudinal tracking of altruistic donor recruitment and participation rates; qualitative analysis of donor motivation; policy changes addressing commodification concerns',
    'If sustainable: scaffold sunset logic is real. If declining: exchange system becomes dependent on paired donors only, reducing flexibility and potentially increasing extracted rents from centers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(altruistic_donor_sustainability, empirical, 'Long-term viability of altruistic donor participation').

omega_variable(
    algorithmic_matching_manipulation,
    'Do transplant centers strategically withhold compatible pairs or manipulate donor information to optimize their exchange participation and chain length?',
    'Analysis of center-level compatibility disclosures; empirical comparison of stated vs inferred blood type/crossmatch distributions; audit of matching algorithm inputs',
    'If manipulation occurs: suppression increases and extraction emerges. If rare: rope classification reflects genuine coordination with minimal gaming.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_matching_manipulation, empirical, 'Whether centers manipulate matching to optimize exchange participation').

omega_variable(
    global_vs_local_pool_extraction,
    'Does expanding kidney exchange to national or international pools benefit all participants equally, or do resource-rich centers extract value from resource-poor regions?',
    'Comparative outcomes analysis by center size and resources; waitlist time differential across regions; cross-border flow patterns in international exchange programs',
    'If equal benefit: rope classification holds globally. If resource-rich centers gain disproportionately: snare or tangled_rope classification for smaller centers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_vs_local_pool_extraction, empirical, 'Whether larger pools create equitable or extractive outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_market, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kidney_exch_tr_t0, kidney_exchange_market, theater_ratio, 0, 0.28).
narrative_ontology:measurement(kidney_exch_tr_t5, kidney_exchange_market, theater_ratio, 5, 0.35).
narrative_ontology:measurement(kidney_exch_tr_t10, kidney_exchange_market, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(kidney_exch_be_t0, kidney_exchange_market, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(kidney_exch_be_t5, kidney_exchange_market, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(kidney_exch_be_t10, kidney_exchange_market, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kidney_exchange_market, resource_allocation).
narrative_ontology:affects_constraint(kidney_exchange_market, organ_scarcity_allocation).
narrative_ontology:affects_constraint(kidney_exchange_market, living_donor_coercion_risk).
narrative_ontology:affects_constraint(kidney_exchange_market, transplant_center_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
