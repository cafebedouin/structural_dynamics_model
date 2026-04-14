% ============================================================================
% CONSTRAINT STORY: veto_points_institutional_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_veto_points_institutional_capacity, []).

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
 *   constraint_id: veto_points_institutional_capacity
 *   human_readable: Veto Points and Institutional Capacity Mismatch
 *   domain: institutional_governance/political_economy
 *
 * SUMMARY:
 *   Veto points are institutional checkpoints designed to require consensus
 *   or supermajority support for policy change. A political system with
 *   multiple veto points (bicameral legislature, executive override power,
 *   judicial review, civil service implementation capacity) forces reformers
 *   to win agreement across all points while status quo defenders need only
 *   control one. This structural asymmetry creates a tangled-rope constraint:
 *   genuine coordination function (requiring deliberation across
 *   institutional actors) coexists with asymmetric extraction (reform
 *   constituencies bear disproportionate burden). The constraint's
 *   extractiveness increases over time (0.42→0.58) as jurisdictions
 *   accumulate new veto points or as existing points become politicized.
 *   Theater ratio increases (0.35→0.48) as institutions justify blockade
 *   through constitutional mythology rather than principled deliberation.
 *   Subnational governments provide temporary scaffolding—regional
 *   experimentation bypasses national veto points and generates political
 *   pressure for national reform—but this scaffold persists only if
 *   subnational success actually translates to national reform appetite
 *   rather than providing permanent exit for complainants.
 *
 * KEY AGENTS:
 *   - Reform Constituencies: Primary victims (powerless/trapped) — seek policy change but systematically subordinated to status quo defenders across all veto points
 *   - Status Quo Defenders: Primary beneficiaries (institutional/arbitrage) — control one or more veto points and leverage blocking capacity for policy concessions
 *   - Fragmented Reform Coalitions: Secondary victims (moderate/constrained) — must coordinate internally to achieve supermajority consensus; energy spent on internal coordination cannot be directed at advocacy
 *   - Reform Movement Leadership: Secondary victims (organized/constrained) — organize coalition while suppressing internal disagreement to maintain negotiating strength
 *   - Subnational Governments: Temporary scaffolders (organized/mobile) — implement policy locally, test innovations, build pressure for national reform
 *   - Constitutional Custodians: Institutional theater maintainers (institutional/arbitrage) — justify veto-point architecture through separation-of-powers mythology despite degraded deliberative function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(veto_points_institutional_capacity, 0.58).
domain_priors:suppression_score(veto_points_institutional_capacity, 0.62).
domain_priors:theater_ratio(veto_points_institutional_capacity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(veto_points_institutional_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(veto_points_institutional_capacity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(veto_points_institutional_capacity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(veto_points_institutional_capacity, tangled_rope).
narrative_ontology:human_readable(veto_points_institutional_capacity, "Veto Points and Institutional Capacity Mismatch").
narrative_ontology:topic_domain(veto_points_institutional_capacity, "institutional_governance/political_economy").

domain_priors:requires_active_enforcement(veto_points_institutional_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(veto_points_institutional_capacity, status_quo_defenders).
narrative_ontology:constraint_beneficiary(veto_points_institutional_capacity, veto_point_holders).
narrative_ontology:constraint_victim(veto_points_institutional_capacity, reform_constituencies).
narrative_ontology:constraint_victim(veto_points_institutional_capacity, policy_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM CONSTITUENCY (SNARE) — Trapped by institutional design. Multiple veto points (executive, legislative chamber, judiciary, bureaucracy) require supermajority consensus or unanimous agreement. The constituency seeking change faces an asymmetric burden: reform requires winning support at all veto points; blockade requires only control of one. No exit — the constituency is embedded in the jurisdiction and cannot opt out of national policy outcomes. Experiences maximum extraction: their preferences are systematically subordinated to those of status quo defenders.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRAGMENTED REFORM COALITION (TANGLED ROPE) — Multiple reform movements (labor, environmental, social justice) must coordinate with each other to muster the consensus required by veto-point architecture. This coordination function is genuine — the coalition solves collective action problems. But the veto-point structure extracts from the coalition: energy spent negotiating internal consensus cannot be spent on policy advocacy. Constrained exit — members can defect to smaller movements but lose coalition strength; movements can splinter but become irrelevant. Mixed experience: genuine coordination value but embedded extraction.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATUS QUO DEFENDER INSTITUTION (ROPE) — Single institution (e.g., upper chamber, judiciary, executive bureaucracy) that holds a veto point. Experiences the veto-point structure as pure coordination: communication of blocking intent is sufficient for policy stability. High exit optionality via arbitrage — the institution can shift positions or leverage across policy domains; can negotiate side payments or issue trades. Net beneficiary from veto architecture. The constraint appears as coordination mechanism, not extraction.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENT LEADERSHIP (TANGLED ROPE) — Organized coalition (party leadership, advocacy networks, civil society organizations) must maintain internal discipline to achieve consensus while negotiating across veto points. Experiences genuine coordination — disciplining the coalition, sequencing reform initiatives, building issue-linkage are real coordination functions. But also experiences extraction: leadership must suppress internal disagreement to maintain negotiating strength; the organization must subordinate member preferences to coalition-wide strategy. Constrained exit — leaders can defect but lose organizational platform; organization can fragment but become marginalized.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SUBNATIONAL EXIT COALITION (SCAFFOLD) — Subnational governments (states, provinces, municipalities) can implement policy innovation locally, bypassing national veto points. This represents a temporary scaffold: as subnational experimentation proves policy viability, national political pressure builds and veto points weaken. Regional governments have mobile exit options — they can implement locally without national consent. Theater ratio here is low (genuine policy implementation, not performative compliance). Has implicit sunset: as subnational innovation accumulates, the national veto-point structure's blocking capacity diminishes, and reform becomes possible nationally.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: CHECKS-AND-BALANCES INSTITUTIONAL THEATER (PITON) — Civilizational view of veto-point architecture as constitutional principle: 'separation of powers prevents tyranny.' The underlying coordination function (checking state power) has largely atrophied in many jurisdictions — veto points are now primarily used for partisan blockade rather than principled oversight. The constraint persists through institutional inertia and constitutional mythology rather than functional necessity. Theater ratio high (0.65+): performative constitutional reverence masks degraded deliberative capacity.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — From a universalist/analytical position, multiple veto points might appear as an immutable natural law: 'All political systems require checks and balances.' This naturalization risks masking the contingent institutional choice (number of veto points is deliberately designed, not inherent). Base extraction (0.58) and required enforcement (true) contradict mountain classification. Engine will flag as false summit.
constraint_indexing:constraint_classification(veto_points_institutional_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(veto_points_institutional_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(veto_points_institutional_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(veto_points_institutional_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(veto_points_institutional_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(veto_points_institutional_capacity, TR),
    TR >= 0.70.

:- end_tests(veto_points_institutional_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Multiple veto points impose coordination costs (supermajority assembly, cross-institutional negotiation, accommodation of diverse preferences) that exceed transaction costs of single-authority decision-making. For reform constituencies, these coordination costs manifest as extraction: energy spent assembling consensus across veto points is energy not available for substantive policy advocacy. The 0.42→0.58 trajectory reflects accumulation of veto points or politicization of existing points. Suppression (0.62): Moderate-high. Structural barriers prevent unilateral reform: exit options are limited (exit jurisdiction entirely, or exit national politics by focusing on subnational implementation). Policy preferences cannot be satisfied outside the national institutional framework. Suppression includes both structural factors (multiple veto points requiring consensus) and behavioral factors (internalized expectations that reform is impossible). Theater ratio (0.48): Moderate. Veto-point exercises include both genuine deliberative content (substantive disagreements on policy) and performative content (constitutional justifications, partisan positioning). The ratio increases over time as institutions invest more in constitutional mythology to justify blockade.
 *
 * PERSPECTIVAL GAP:
 *   The major perspectival gap is between status quo defenders (Rope perspective: veto points enable coordination and deliberation) and reform constituencies (Snare perspective: veto points impose impossible consensus requirements). Status quo defenders experience the constraint as enabling stable governance; reform constituencies experience it as blocking capacity. The piton perspective reveals that constitutional justifications for veto-point architecture (separation of powers, checks and balances) persist despite atrophied deliberative function—veto points are now primarily partisan blockade tools rather than principled oversight mechanisms. The scaffold perspective reveals a temporary exit: subnational governments bypass national veto points through regional policy innovation, but this scaffold only persists if subnational success accumulates to create national reform pressure. The false summit (analytical/mountain) risks naturalizing a contingent institutional choice as immutable constitutional principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from beneficiary/victim status and exit options. Status quo defenders with arbitrage options experience low d (they benefit from veto-point architecture and can shift positions or negotiate side payments). Reform constituencies with trapped exit experience high d (they cannot exit the national framework and are systematically subordinated). Organized reform coalitions with constrained exit experience moderate d (they can form alternative organizations but lose coalition strength). Subnational governments with mobile exit experience lower d (they can implement locally without national consent). The beneficiary/victim asymmetry is structural: veto points systematically advantage status quo over reform. This produces high d for victims and low d for beneficiaries, driving the tangled-rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing the genuine coordination function of veto points (deliberation across institutional actors) from their extractive use (partisan blockade). The tangled-rope classification captures both: veto points do coordinate (deliberative exchanges, issue-linkage, multi-institutional accommodation) AND extract (asymmetric burden on reform constituencies). The piton perspective reveals that the coordination function has partially degraded—veto-point exercises are increasingly performative. The scaffold perspective suggests time-limited structure: subnational policy innovation could eventually weaken national veto-point blocking through demonstrated feasibility and accumulated political pressure. Mandatrophy is resolved by treating veto-point architecture as a design choice (contingent, potentially reformable) rather than a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_count_threshold,
    'What is the empirical threshold for ''too many veto points''? Does institutional capacity degrade continuously with each additional veto point or is there a discrete threshold beyond which reform becomes structurally impossible?',
    'Comparative analysis of polities with 2, 3, 4, 5+ veto points; measurement of policy innovation rate, reform success rate, and time-to-enactment; longitudinal tracking of whether additional veto points correlate with paralysis or simply slower consensus-building',
    'If continuous: extraction accumulates with each veto point; suppressiveness scales with veto count. If threshold exists: some veto-point designs are functional rope (low extraction) while others are snare (high extraction). Current design may be on wrong side of threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_count_threshold, empirical, 'Empirical threshold for veto-point induced institutional paralysis').

omega_variable(
    subnational_escape_valve_adequacy,
    'Does subnational policy experimentation actually reduce national veto-point blocking, or does it provide a permanent escape valve that prevents national reform by allowing complainants to exit the national political arena?',
    'Historical tracking of policy innovations first tested locally, then adopted nationally; measurement of whether subnational success increases national reform likelihood or decreases national reform pressure; analysis of whether subnational exit makes national reform coalitions smaller and weaker',
    'If escape valve: scaffold perspective is correct; veto points will eventually weaken as subnational success accumulates. If permanent exit: veto points are reinforced; subnational experimentation removes the constituency that would push for national reform, perpetuating national blockade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subnational_escape_valve_adequacy, empirical, 'Whether subnational policy allows escape from or reinforces national veto points').

omega_variable(
    coordination_function_decay,
    'Has the original coordination function of veto points (preventing tyranny through distributed power) been replaced by partisan blockade logic, or do veto points still serve a meaningful deliberative function in contemporary institutional contexts?',
    'Qualitative analysis of veto-point exercise patterns: are blocking actions justified by principled constitutional concerns or by partisan advantage? Measurement of deliberative quality in veto-point negotiations; analysis of whether veto points generate policy compromise or merely stalemate',
    'If function decayed: piton classification is correct; veto-point architecture persists through inertia and should be reformed. If function intact: tangled rope or rope classification may be more accurate; extraction is compensation for genuine coordination service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_decay, conceptual, 'Whether veto points maintain genuine coordination function or have become purely extractive blockade mechanism').

omega_variable(
    institutional_capacity_measurement,
    'How is ''institutional capacity to implement reform'' measured? Is it a function of coordination costs alone, or does it include the substantive complexity of the policy being reformed?',
    'Decompose policy adoption timelines by policy complexity; distinguish between coordination delays (veto-point induced) and substantive delays (legitimate deliberation on complex issues); measure whether veto-point delays exceed substantive deliberation time',
    'If coordination dominates: extractiveness estimate (0.58) is accurate. If substantive complexity is conflated with veto-point blockade: extractiveness may be overstated; some delay may be legitimate rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capacity_measurement, empirical, 'Attribution of policy delays to veto-point coordination vs. substantive complexity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(veto_points_institutional_capacity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(veto_tr_t0, veto_points_institutional_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(veto_tr_t10, veto_points_institutional_capacity, theater_ratio, 10, 0.42).
narrative_ontology:measurement(veto_tr_t20, veto_points_institutional_capacity, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(veto_be_t0, veto_points_institutional_capacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(veto_be_t10, veto_points_institutional_capacity, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(veto_be_t20, veto_points_institutional_capacity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(veto_points_institutional_capacity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(veto_points_institutional_capacity, 0.12).
narrative_ontology:affects_constraint(veto_points_institutional_capacity, legislative_bicameralism_partisan_alignment).
narrative_ontology:affects_constraint(veto_points_institutional_capacity, executive_judicial_power_distribution).

% DUAL FORMULATION NOTE:
% Veto-point architecture itself (number and distribution of veto points) is a structural parameter. This constraint story treats veto points as a fixed institutional design. Separate constraint stories address specific veto-point instantiations (bicameral legislatures, executive/judicial checks) with their own extractiveness values. This story models the general coordination/extraction tension inherent to multi-veto-point systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(veto_points_institutional_capacity, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
